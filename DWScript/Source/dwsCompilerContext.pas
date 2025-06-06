{ ********************************************************************** }
{ }
{ "The contents of this file are subject to the Mozilla Public }
{ License Version 1.1 (the "License"); you may not use this }
{ file except in compliance with the License. You may obtain }
{ a copy of the License at http://www.mozilla.org/MPL/ }
{ }
{ Software distributed under the License is distributed on an }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express }
{ or implied. See the License for the specific language }
{ governing rights and limitations under the License. }
{ }
{ Copyright Creative IT. }
{ Current maintainer: Eric Grange }
{ }
{ ********************************************************************** }
unit dwsCompilerContext;

interface

uses
  dwsUtils, dwsSymbols, dwsErrors, dwsScriptSource, dwsXPlatform,
  dwsUnitSymbols, dwsStrings, dwsTokenizer, dwsCustomData, dwsSpecialKeywords,
  dwsSymbolDictionary;

type
  TCompilerOption = (coOptimize, // enable compiler optimizations
    coSymbolDictionary, // fillup symbol dictionary
    coContextMap, // fillup context map
    coAssertions, // compile assertions (if absent, ignores assertions)
    coHintsDisabled, // don't generate hints messages
    coWarningsDisabled, // don't generate warnings messages
    coExplicitUnitUses,
    // unit dependencies must be explicit via a "uses" clause
    coVariablesAsVarOnly, // only variable can be passed as "var" parameters
    // (for CodeGen that does not support passing record fields or array elements)
    coAllowClosures,
    // allow closures, ie. capture of local procedures as function pointers
    // (not suppported yet by script engine, may be supported by CodeGen)
    coAllowAsyncAwait,
    // allow for assync/await keywords (only supported by JS-codegen)
    coDelphiDialect, // do not warn or hint about Delphi language idioms
    coHintKeywordCaseMismatch,
    // when set, if pedantic hints are active, hints will be
    // created for all case-mismatching keywords
    coMissingOverloadedAsErrors
    // when set missing "overloaded" are treated as errors
    // when not set, they are just reported as hints

    );
  TCompilerOptions = set of TCompilerOption;

  TdwsCompilerContext = class(TdwsBaseSymbolsContext)
  private
    FMsgs: TdwsCompileMessageList;
    FSystemTable: TSystemSymbolTable;
    FProg: TObject;
    FUnifiedConstants: TObject;
    FOrphanedObjects: TSimpleStack<TRefCountedObject>;
    FUnitList: TIdwsUnitList;
    FHelperMemberNames: TSimpleStringHash;
    FSpecialSymbols: array [TSpecialKeywordKind] of TSymbol;
    FFuncSymbolListPool: TFuncSymbolList;

    FTypDefaultConstructor: TMethodSymbol;
    FTypDefaultDestructor: TMethodSymbol;

    FStringsUnifier: TStringUnifier;

    FExecution: TdwsExecution;
    FOptions: TCompilerOptions;

    FCustomStates: TdwsCustomStates;
    FCustomStatesMRSW: TMultiReadSingleWrite;

  protected
    procedure SetSystemTable(const val: TSystemSymbolTable);
    procedure SetProg(aProg: TObject);
    function GetSymbolDictionary: TdwsSymbolDictionary;

  public
    constructor Create;
    destructor Destroy; override;

    procedure OrphanObject(obj: TRefCountedObject);

    function GetTempAddr(DataSize: Integer = -1): Integer;
    function Level: Integer;
    function Table: TSymbolTable;

    function CreateConstExpr(typ: TTypeSymbol; const value: Variant): TExprBase;
    function CreateInteger(value: Int64): TExprBase;

    function WrapWithImplicitCast(toTyp: TTypeSymbol;
      const scriptPos: TScriptPos; var expr): Boolean;
    function FindType(const typName: String): TTypeSymbol; override;

    function SpecialSymbol(sk: TSpecialKeywordKind): TSymbol;

    function Optimize: Boolean;

    function AllocateFuncSymbolList: TFuncSymbolList;
    procedure ReleaseFuncSymbolList(list: TFuncSymbolList);

    property StringsUnifier: TStringUnifier read FStringsUnifier;

    property Msgs: TdwsCompileMessageList read FMsgs write FMsgs;
    property SystemTable: TSystemSymbolTable read FSystemTable
      write SetSystemTable;
    property Prog: TObject read FProg write SetProg;
    property UnifiedConstants: TObject read FUnifiedConstants
      write FUnifiedConstants;
    property UnitList: TIdwsUnitList read FUnitList write FUnitList;
    property HelperMemberNames: TSimpleStringHash read FHelperMemberNames;
    property SymbolDictionary: TdwsSymbolDictionary read GetSymbolDictionary;

    property Execution: TdwsExecution read FExecution write FExecution;
    property Options: TCompilerOptions read FOptions write FOptions;

    property TypDefaultConstructor: TMethodSymbol read FTypDefaultConstructor;
    property TypDefaultDestructor: TMethodSymbol read FTypDefaultDestructor;

    procedure CustomStateGet(const index: TGUID; var result: Variant);
    procedure CustomStateSet(const index: TGUID; const value: Variant);
    procedure CustomStateCompareExchange(const index: TGUID;
      const exchange, comparand: Variant; var result: Variant);
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  System.Variants,
  dwsExprs, dwsUnifiedConstants, dwsConstExprs, dwsOperators, dwsCompilerUtils,
  dwsConvExprs, dwsDynamicArrays, dwsAssociativeArrays;

type
  TdwsCompilerContextHelper = class helper for TdwsCompilerContext
    function GetProgram: TdwsProgram; inline;
  end;

  // GetProgram
  //
function TdwsCompilerContextHelper.GetProgram: TdwsProgram;
begin
  result := TdwsProgram(FProg);
end;

// ------------------
// ------------------ TdwsCompilerContext ------------------
// ------------------

// Create
//
constructor TdwsCompilerContext.Create;
begin
  inherited;
  FOrphanedObjects := TSimpleStack<TRefCountedObject>.Create;
  FStringsUnifier := TStringUnifier.Create;
  FHelperMemberNames := TSimpleStringHash.Create;
  FCustomStatesMRSW := TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TdwsCompilerContext.Destroy;
var
  obj: TRefCountedObject;
  sk: TSpecialKeywordKind;
begin
  // stack behavior is required to allow objects to orphan others while being cleaned up
  while FOrphanedObjects.Count > 0 do
  begin
    obj := FOrphanedObjects.Peek;
    obj.DecRefCount;
    FOrphanedObjects.Pop;
  end;
  FOrphanedObjects.Free;

  FStringsUnifier.Free;

  FHelperMemberNames.Free;

  FCustomStatesMRSW.Free;
  FCustomStates.Free;

  for sk := Low(FSpecialSymbols) to High(FSpecialSymbols) do
    FSpecialSymbols[sk].Free;

  FFuncSymbolListPool.Free;

  inherited;
end;

// OrphanObject
//
procedure TdwsCompilerContext.OrphanObject(obj: TRefCountedObject);
begin
  if obj <> nil then
    FOrphanedObjects.Push(obj);
end;

// GetTempAddr
//
function TdwsCompilerContext.GetTempAddr(DataSize: Integer = -1): Integer;
begin
  result := GetProgram.GetTempAddr(DataSize);
end;

// Level
//
function TdwsCompilerContext.Level: Integer;
begin
  result := GetProgram.Level;
end;

// Table
//
function TdwsCompilerContext.Table: TSymbolTable;
begin
  result := GetProgram.Table;
end;

// CreateConstExpr
//
function TdwsCompilerContext.CreateConstExpr(typ: TTypeSymbol;
  const value: Variant): TExprBase;

  function CreateDynamicArrayValue(typ: TTypeSymbol): TConstExpr;
  var
    val: IScriptDynArray;
  begin
    val := IUnknown(value) as IScriptDynArray;
    if val = nil then
      CreateNewDynamicArray(typ.typ, val);
    result := TConstExpr.CreateValue(cNullPos, typ, val);
  end;

  function CreateAssociativeArrayValue(typ: TAssociativeArraySymbol)
    : TConstExpr;
  var
    val: IScriptAssociativeArray;
  begin
    val := IUnknown(value) as IScriptAssociativeArray;
    if val = nil then
      CreateNewAssociativeArray(typ.KeyType, typ.typ, val);
    result := TConstExpr.CreateValue(cNullPos, typ, val);
  end;

begin
  if typ = TypString then
    result := TUnifiedConstants(FUnifiedConstants).CreateString(value)
  else if typ.ClassType = TDynamicArraySymbol then
    result := CreateDynamicArrayValue(typ)
  else if typ.ClassType = TAssociativeArraySymbol then
    result := CreateAssociativeArrayValue(TAssociativeArraySymbol(typ))
  else if typ = TypBoolean then
    result := TUnifiedConstants(FUnifiedConstants).CreateBoolean(value)
  else if typ = TypFloat then
    result := TUnifiedConstants(FUnifiedConstants)
      .CreateFloat(VariantToFloat(value))
  else if typ = TypInteger then
    result := TUnifiedConstants(FUnifiedConstants).CreateInteger(value)
  else if typ.typ = TypInteger then
    result := TConstIntExpr.Create(cNullPos, typ, value)
  else
    result := TConstExpr.CreateValue(cNullPos, typ, value);
end;

// CreateInteger
//
function TdwsCompilerContext.CreateInteger(value: Int64): TExprBase;
begin
  result := TUnifiedConstants(FUnifiedConstants).CreateInteger(value);
end;

// WrapWithImplicitCast
//
function TdwsCompilerContext.WrapWithImplicitCast(toTyp: TTypeSymbol;
  const scriptPos: TScriptPos; var expr): Boolean;
var
  casterClass: TTypedExprClass;
  typedExpr: TTypedExpr;
  Prog: TdwsProgram;
  opSym: TOperatorSymbol;
  funcExpr: TFuncExprBase;
  posArray: TScriptPosArray;
begin
  typedExpr := TObject(expr) as TTypedExpr;
  if typedExpr.typ = nil then
    Exit(False);

  Prog := GetProgram;
  opSym := Prog.Table.FindImplicitCastOperatorFor(typedExpr.typ, toTyp);
  if opSym <> nil then
  begin
    funcExpr := CreateSimpleFuncExpr(Self, scriptPos, opSym.UsesSym);
    funcExpr.AddArg(typedExpr);
    TObject(expr) := funcExpr;
    if opSym.UsesSym.typ.Size > 1 then
      funcExpr.InitializeResultAddr(Prog);
    SetLength(posArray, 1);
    posArray[0] := scriptPos;
    TypeCheckArguments(Self, funcExpr, posArray);
    if Optimize then
      TObject(expr) := funcExpr.OptimizeToTypedExpr(Self, scriptPos);
    result := True;
  end
  else
  begin
    casterClass := (Prog.Root.Operators as TOperators).FindImplicitCaster(toTyp,
      typedExpr.typ);
    result := (casterClass <> nil);
    if result then
    begin
      if casterClass.InheritsFrom(TUnaryOpExpr) then
        typedExpr := TUnaryOpExprClass(casterClass).Create(Self, scriptPos,
          typedExpr)
      else
      begin
        Assert(casterClass.InheritsFrom(TUnaryOpDataExpr));
        typedExpr := TUnaryOpDataExprClass(casterClass).Create(Self, scriptPos,
          typedExpr);
      end;
      TObject(expr) := typedExpr;
      if Optimize then
        TObject(expr) := typedExpr.OptimizeToTypedExpr(Self, scriptPos);
    end
    else if toTyp.UnAliasedTypeIs(TDynamicArraySymbol) and
      (typedExpr is TArrayConstantExpr) and toTyp.UnAliasedType.typ.IsCompatible
      (typedExpr.typ.UnAliasedType.typ) then
    begin
      result := True;
      typedExpr := TConvArrayConstantToDynamicExpr.Create(Self, scriptPos,
        TArrayConstantExpr(typedExpr),
        TDynamicArraySymbol(toTyp.UnAliasedType));
      TObject(expr) := typedExpr;
    end
    else if (toTyp.UnAliasedType = TypFloat) and
      (typedExpr.typ.UnAliasedType = TypInteger) then
    begin
      result := True;
      typedExpr := TConvIntToFloatExpr.Create(Self, scriptPos, typedExpr);
      typedExpr.typ := toTyp;
      TObject(expr) := typedExpr;
    end;
  end;
end;

// FindType
//
function TdwsCompilerContext.FindType(const typName: String): TTypeSymbol;
begin
  result := SystemTable.FindTypeLocal(typName)
end;

// SpecialSymbol
//
function TdwsCompilerContext.SpecialSymbol(sk: TSpecialKeywordKind): TSymbol;

  function InitializeSymbol(sk: TSpecialKeywordKind): TSymbol;
  begin
    result := TPseudoMethodSymbol.Create(nil, cSpecialKeywords[sk],
      fkFunction, 0);
    case sk of
      skAssigned, skDefined, skDeclared, skConditionalDefined:
        result.typ := TypBoolean;
      skHigh, skLength, skLow, skOrd, skSizeOf, skInc, skDec:
        result.typ := TypInteger;
    end;
  end;

begin
  result := FSpecialSymbols[sk];
  if result = nil then
  begin
    result := InitializeSymbol(sk);
    FSpecialSymbols[sk] := result;
  end;
end;

// Optimize
//
function TdwsCompilerContext.Optimize: Boolean;
begin
  result := (coOptimize in FOptions) and (not FMsgs.HasErrors);
end;

// AllocateFuncSymbolList
//
function TdwsCompilerContext.AllocateFuncSymbolList: TFuncSymbolList;
begin
  if FFuncSymbolListPool <> nil then
  begin
    result := FFuncSymbolListPool;
    FFuncSymbolListPool := nil;
  end
  else
    result := TFuncSymbolList.Create;
end;

// ReleaseFuncSymbolList
//
procedure TdwsCompilerContext.ReleaseFuncSymbolList(list: TFuncSymbolList);
begin
  if list <> nil then
  begin
    if FFuncSymbolListPool = nil then
    begin
      list.Clear;
      FFuncSymbolListPool := list;
    end
    else
      list.Free;
  end;
end;

// SetSystemTable
//
procedure TdwsCompilerContext.SetSystemTable(const val: TSystemSymbolTable);
begin
  FSystemTable := val;
  SetBaseTypes(FSystemTable.BaseSymbolTypes);

  FTypDefaultConstructor := TypTObject.Members.FindSymbol(SYS_TOBJECT_CREATE,
    cvPublic) as TMethodSymbol;
  FTypDefaultDestructor := TypTObject.Members.FindSymbol(SYS_TOBJECT_DESTROY,
    cvPublic) as TMethodSymbol;
end;

// SetProg
//
procedure TdwsCompilerContext.SetProg(aProg: TObject);
begin
  FProg := aProg as TdwsProgram;
end;

// GetSymbolDictionary
//
function TdwsCompilerContext.GetSymbolDictionary: TdwsSymbolDictionary;
begin
  result := TdwsProgram(FProg).Root.SymbolDictionary;
end;

// CustomStateGet
//
procedure TdwsCompilerContext.CustomStateGet(const index: TGUID;
  var result: Variant);
begin
  if FCustomStates = nil then
    VarClearSafe(result)
  else
  begin
    FCustomStatesMRSW.BeginRead;
    try
      VarCopySafe(result, FCustomStates.States[index]);
    finally
      FCustomStatesMRSW.EndRead;
    end;
  end;
end;

// CustomStateSet
//
procedure TdwsCompilerContext.CustomStateSet(const index: TGUID;
  const value: Variant);
begin
  FCustomStatesMRSW.BeginWrite;
  try
    if FCustomStates = nil then
      FCustomStates := TdwsCustomStates.Create;
    FCustomStates.States[index] := value;
  finally
    FCustomStatesMRSW.EndWrite;
  end;
end;

// CustomStateCompareExchange
//
procedure TdwsCompilerContext.CustomStateCompareExchange(const index: TGUID;
  const exchange, comparand: Variant; var result: Variant);
begin
  FCustomStatesMRSW.BeginWrite;
  try
    if FCustomStates = nil then
      FCustomStates := TdwsCustomStates.Create;
    VarCopySafe(result, FCustomStates[index]);
    if VarCompareSafe(result, comparand) = vrEqual then
      FCustomStates[index] := exchange;
  finally
    FCustomStatesMRSW.EndWrite;
  end;
end;

end.
