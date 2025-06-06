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
{ The Initial Developer of the Original Code is Matthias }
{ Ackermann. For other initial contributors, see contributors.txt }
{ Subsequent portions Copyright Creative IT. }
{ }
{ Current maintainer: Eric Grange }
{ }
{ ********************************************************************** }
unit dwsConvExprs;

{$I dws.inc}

interface

uses
  System.SysUtils,
  dwsUtils, dwsDataContext, dwsStack, dwsExprs, dwsExprList,
  dwsConstExprs, dwsSymbols, dwsScriptSource, dwsCompilerContext;

type

  // newType(x)
  TConvExpr = class(TUnaryOpExpr)
  public
    class function WrapWithConvCast(context: TdwsCompilerContext;
      const scriptPos: TScriptPos; toTyp: TTypeSymbol; expr: TTypedExpr;
      const reportError: String): TTypedExpr; static;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // Just wraps with a typ for an invalid conversion expr
  // this is used only to keep compiling, the resulting program is not executable
  TConvInvalidExpr = class sealed(TConvExpr)
  protected
    function GetIsConstant: Boolean; override;

  public
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr; toTyp: TTypeSymbol);
      reintroduce;
  end;

  // Float(int x)
  TConvIntToFloatExpr = class(TUnaryOpFloatExpr)
    function EvalAsFloat(exec: TdwsExecution): Double; override;
  end;

  // Float(variant x)
  TConvVarToFloatExpr = class(TUnaryOpFloatExpr)
    function EvalAsFloat(exec: TdwsExecution): Double; override;
  end;

  // Integer(variant x)
  TConvVarToIntegerExpr = class(TUnaryOpIntExpr)
    function EvalAsInteger(exec: TdwsExecution): Int64; override;
  end;

  // Integer(ordinal x)
  TConvOrdToIntegerExpr = class(TUnaryOpIntExpr)
    function EvalAsInteger(exec: TdwsExecution): Int64; override;
    function Optimize(context: TdwsCompilerContext): TProgramExpr; override;
  end;

  // String(variant x)
  TConvVarToStringExpr = class sealed(TUnaryOpStringExpr)
    procedure EvalAsString(exec: TdwsExecution; var result: String); override;
    function SpecializeTypedExpr(const context: ISpecializationContext)
      : TTypedExpr; override;
  end;

  // Boolean(int x)
  TConvIntToBoolExpr = class(TUnaryOpBoolExpr)
    function EvalAsBoolean(exec: TdwsExecution): Boolean; override;
  end;

  // Boolean(float x)
  TConvFloatToBoolExpr = class(TUnaryOpBoolExpr)
    function EvalAsBoolean(exec: TdwsExecution): Boolean; override;
  end;

  // Boolean(variant x)
  TConvVarToBoolExpr = class(TUnaryOpBoolExpr)
    function EvalAsBoolean(exec: TdwsExecution): Boolean; override;
  end;

  // Variant(simple)
  TConvVariantExpr = class sealed(TUnaryOpVariantExpr)
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    function SpecializeTypedExpr(const context: ISpecializationContext)
      : TTypedExpr; override;
  end;

  // Array Constant (inline) to Dynamic Array
  TConvArrayConstantToDynamicExpr = class sealed(TUnaryOpExpr)
  public
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; expr: TArrayConstantExpr;
      toTyp: TDynamicArraySymbol); reintroduce;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    procedure EvalAsScriptDynArray(exec: TdwsExecution;
      var result: IScriptDynArray); override;
    function GetIsConstant: Boolean; override;
  end;

  // Static Array (value) to Dynamic Array
  TConvStaticArrayToDynamicExpr = class sealed(TUnaryOpExpr)
  public
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr;
      toTyp: TDynamicArraySymbol); reintroduce;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    procedure EvalAsScriptDynArray(exec: TdwsExecution;
      var result: IScriptDynArray); override;
    function GetIsConstant: Boolean; override;
  end;

  // ExternalClass(x)
  TConvExternalExpr = class(TUnaryOpVariantExpr)
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // convert to data expr
  TConvDataExpr = class(TDataExpr)
  private
    FExpr: TTypedExpr;

  protected
    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

    function GetIsConstant: Boolean; override;

  public
    constructor Create(const scriptPos: TScriptPos; expr: TTypedExpr;
      aTyp: TTypeSymbol);
    destructor Destroy; override;

    function IsWritable: Boolean; override;

    property expr: TTypedExpr read FExpr write FExpr;
  end;

  // Static Array to set of
  TConvStaticArrayToSetOfExpr = class(TConvDataExpr)
  public
    constructor Create(const scriptPos: TScriptPos; expr: TArrayConstantExpr;
      toTyp: TSetOfSymbol);

    function ToConstExpr(exec: TdwsExecution): TConstExpr;

    procedure GetDataPtr(exec: TdwsExecution;
      var result: IDataContext); override;
  end;

  // Integer(x)
  TConvSetOfToIntegerExpr = class(TUnaryOpIntExpr)
    function EvalAsInteger(exec: TdwsExecution): Int64; override;
  end;

  // set of (Integer)
  TConvIntegerToSetOfExpr = class(TConvDataExpr)
  public
    constructor Create(const scriptPos: TScriptPos; expr: TTypedExpr;
      toTyp: TSetOfSymbol);

    procedure GetDataPtr(exec: TdwsExecution;
      var result: IDataContext); override;
  end;

  // cast something as Typ
  TAsCastExpr = class(TUnaryOpExpr)
  private
    FPos: TScriptPos;

  public
    constructor Create(context: TdwsCompilerContext; const aPos: TScriptPos;
      expr: TTypedExpr; toTyp: TTypeSymbol); reintroduce;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // class as TMyClass
  TClassAsClassExpr = class(TAsCastExpr)
  protected
    procedure RaiseMetaClassCastFailed(exec: TdwsExecution;
      classSym: TClassSymbol);

  public
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // obj as TMyClass
  TObjAsClassExpr = class(TAsCastExpr)
  protected
    procedure RaiseInstanceClassCastFailed(exec: TdwsExecution;
      classSym: TClassSymbol);

  public
    procedure EvalAsScriptObj(exec: TdwsExecution;
      var result: IScriptObj); override;
  end;

  // obj.ClassType
  TObjToClassTypeExpr = class(TUnaryOpExpr)
  public
    constructor Create(context: TdwsBaseSymbolsContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr); override;

    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // obj as Interface
  TObjAsIntfExpr = class(TAsCastExpr)
  public
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // interface as Interface
  TIntfAsIntfExpr = class(TAsCastExpr)
  public
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // interface as class
  TIntfAsClassExpr = class(TAsCastExpr)
  public
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsCoreExprs, dwsConnectorSymbols, dwsDynamicArrays, dwsErrors, dwsStrings;

// ------------------
// ------------------ TConvExpr ------------------
// ------------------

// WrapWithConvCast
//
class function TConvExpr.WrapWithConvCast(context: TdwsCompilerContext;
  const scriptPos: TScriptPos; toTyp: TTypeSymbol; expr: TTypedExpr;
  const reportError: String): TTypedExpr;

  procedure ReportIncompatibleTypes;
  var
    cleft, cright: String;
  begin
    if reportError = '' then
      Exit;
    if toTyp = nil then
      cleft := SYS_VOID
    else
    begin
      if toTyp is TAnyTypeSymbol then
        Exit;
      cleft := toTyp.Caption;
    end;
    if expr.Typ = nil then
      cright := SYS_VOID
    else
    begin
      cright := expr.Typ.Caption;
      if expr.Typ is TAnyTypeSymbol then
        Exit;
    end;
    context.Msgs.AddCompilerErrorFmt(scriptPos, reportError, [cright, cleft]);
  end;

var
  arrayConst: TArrayConstantExpr;
  staticArrayToSetOf: TConvStaticArrayToSetOfExpr;
begin
  result := expr;
  if (toTyp = nil) or (expr.Typ = nil) then
  begin
    ReportIncompatibleTypes;
    Exit;
  end;

  if expr.Typ = toTyp then
    Exit;

  if context.WrapWithImplicitCast(toTyp, scriptPos, expr) then
  begin
    result := expr;
    Exit;
  end;

  if expr.ClassType = TArrayConstantExpr then
  begin

    arrayConst := TArrayConstantExpr(expr);
    var
    toTypClass := toTyp.ClassType;
    if toTypClass = TDynamicArraySymbol then
    begin
      if (toTyp.Typ.IsOfType(expr.Typ.Typ)) or
        ((arrayConst.ElementCount = 0) and
        (arrayConst.Typ.Typ.IsOfType(context.TypVariant))) then
        result := TConvArrayConstantToDynamicExpr.Create(context, scriptPos,
          arrayConst, TDynamicArraySymbol(toTyp))
    end
    else if toTypClass = TSetOfSymbol then
    begin
      if arrayConst.ElementCount = 0 then
      begin
        result := TConstExpr.Create(cNullPos, toTyp);
        expr.Free;
      end
      else if arrayConst.Typ.Typ.IsOfType(toTyp.Typ) then
      begin
        staticArrayToSetOf := TConvStaticArrayToSetOfExpr.Create(scriptPos,
          arrayConst, TSetOfSymbol(toTyp));
        Assert(staticArrayToSetOf.IsConstant);
        result := staticArrayToSetOf.ToConstExpr(context.Execution);
        staticArrayToSetOf.Free;
      end;
    end;

  end
  else if expr.Typ.UnAliasedTypeIs(TBaseVariantSymbol) then
  begin

    if toTyp.IsOfType(context.TypInteger) then
      result := TConvVarToIntegerExpr.Create(context, scriptPos, expr)
    else if toTyp.IsOfType(context.TypFloat) then
      result := TConvVarToFloatExpr.Create(context, scriptPos, expr)
    else if toTyp.IsOfType(context.TypString) then
      result := TConvVarToStringExpr.Create(context, scriptPos, expr)
    else if toTyp.IsOfType(context.TypBoolean) then
      result := TConvVarToBoolExpr.Create(context, scriptPos, expr);

  end
  else if (toTyp is TStructuredTypeMetaSymbol) and (expr.Typ.IsOfType(toTyp.Typ))
  then
  begin

    if toTyp.ClassType = TClassOfSymbol then
    begin
      result := TObjToClassTypeExpr.Create(context, scriptPos, expr);
      if toTyp.Typ <> expr.Typ then
        result := TClassAsClassExpr.Create(context, scriptPos, result, toTyp);
    end;

  end
  else if (toTyp is TDynamicArraySymbol) and (expr.Typ is TStaticArraySymbol)
    and expr.Typ.Typ.SameType(toTyp.Typ) then
  begin

    result := TConvStaticArrayToDynamicExpr.Create(context, scriptPos, expr,
      TDynamicArraySymbol(toTyp));

  end
  else if toTyp.UnAliasedTypeIs(TConnectorSymbol) then
  begin

    if expr.Typ.UnAliasedType <> toTyp.UnAliasedType then
    begin
      result := TConnectorSymbol(toTyp.UnAliasedType).CreateConvExpr(context,
        scriptPos, expr);
    end;

  end
  else
  begin

    if toTyp.IsOfType(context.TypFloat) and expr.IsOfType(context.TypInteger)
    then
    begin
      if expr is TConstIntExpr then
      begin
        result := TConstFloatExpr.Create(scriptPos, context.TypFloat,
          TConstIntExpr(expr).Value);
        expr.Free;
      end
      else
        result := TConvIntToFloatExpr.Create(context, scriptPos, expr);
    end;

  end;
  // Look if Types are compatible
  if not toTyp.IsCompatible(result.Typ) then
    if not(toTyp.IsGeneric or result.Typ.IsGeneric) then
      ReportIncompatibleTypes;
end;

// EvalAsVariant
//
procedure TConvExpr.EvalAsVariant(exec: TdwsExecution; var result: Variant);
begin
  Assert(False);
end;

// ------------------
// ------------------ TConvInvalidExpr ------------------
// ------------------

// Create
//
constructor TConvInvalidExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr; toTyp: TTypeSymbol);
begin
  inherited Create(context, aScriptPos, expr);
  Typ := toTyp;
end;

// GetIsConstant
//
function TConvInvalidExpr.GetIsConstant: Boolean;
begin
  result := False;
end;

// ------------------
// ------------------ TConvIntToFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TConvIntToFloatExpr.EvalAsFloat(exec: TdwsExecution): Double;
begin
  result := FExpr.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TConvVarToFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TConvVarToFloatExpr.EvalAsFloat(exec: TdwsExecution): Double;
begin
  result := FExpr.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TConvVarToIntegerExpr ------------------
// ------------------

// EvalAsInteger
//
function TConvVarToIntegerExpr.EvalAsInteger(exec: TdwsExecution): Int64;
begin
  result := FExpr.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TConvOrdToIntegerExpr ------------------
// ------------------

// EvalAsInteger
//
function TConvOrdToIntegerExpr.EvalAsInteger(exec: TdwsExecution): Int64;
begin
  result := FExpr.EvalAsInteger(exec);
end;

// Optimize
//
function TConvOrdToIntegerExpr.Optimize(context: TdwsCompilerContext)
  : TProgramExpr;
begin
  // this can happen when an integer was qualifed as a type
  if expr.ClassType = TConstIntExpr then
  begin
    if expr.Typ = Typ then
    begin
      result := expr;
      expr := nil;
    end
    else
    begin
      // requalify constant type
      result := TConstIntExpr.Create(scriptPos, Typ, TConstIntExpr(expr).Value);
    end;
    Free;
  end
  else
    result := Self;
end;

// ------------------
// ------------------ TConvVarToStringExpr ------------------
// ------------------

// EvalAsString
//
procedure TConvVarToStringExpr.EvalAsString(exec: TdwsExecution;
  var result: String);
begin
  FExpr.EvalAsString(exec, result);
end;

// SpecializeTypedExpr
//
function TConvVarToStringExpr.SpecializeTypedExpr(const context
  : ISpecializationContext): TTypedExpr;
begin
  result := TConvVarToStringExpr.Create(context.BaseSymbols, scriptPos,
    expr.SpecializeTypedExpr(context));
end;

// ------------------
// ------------------ TConvIntToBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TConvIntToBoolExpr.EvalAsBoolean(exec: TdwsExecution): Boolean;
begin
  result := (FExpr.EvalAsInteger(exec) <> 0);
end;

// ------------------
// ------------------ TConvFloatToBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TConvFloatToBoolExpr.EvalAsBoolean(exec: TdwsExecution): Boolean;
begin
  result := (FExpr.EvalAsFloat(exec) <> 0);
end;

// ------------------
// ------------------ TConvVarToBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TConvVarToBoolExpr.EvalAsBoolean(exec: TdwsExecution): Boolean;
var
  v: Variant;
begin
  FExpr.EvalAsVariant(exec, v);
  result := VariantToBool(v);
end;

// ------------------
// ------------------ TConvVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TConvVariantExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  FExpr.EvalAsVariant(exec, result);
end;

// SpecializeTypedExpr
//
function TConvVariantExpr.SpecializeTypedExpr(const context
  : ISpecializationContext): TTypedExpr;
begin
  result := TConvVariantExpr.Create(context.BaseSymbols, scriptPos,
    expr.SpecializeTypedExpr(context));
end;

// ------------------
// ------------------ TConvArrayConstantToDynamicExpr ------------------
// ------------------

// Create
//
constructor TConvArrayConstantToDynamicExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; expr: TArrayConstantExpr;
  toTyp: TDynamicArraySymbol);
begin
  inherited Create(context, aScriptPos, expr);
  Typ := toTyp;
end;

// EvalAsVariant
//
procedure TConvArrayConstantToDynamicExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  dynArray: IScriptDynArray;
begin
  EvalAsScriptDynArray(exec, dynArray);
  VarCopySafe(result, dynArray);
end;

// EvalAsScriptDynArray
//
procedure TConvArrayConstantToDynamicExpr.EvalAsScriptDynArray
  (exec: TdwsExecution; var result: IScriptDynArray);
var
  arr: TArrayConstantExpr;

  procedure ConvThroughDataContext;
  var
    data: IDataContext;
  begin
    exec.DataContext_CreateEmpty(result.ElementSize * arr.ElementCount, data);
    arr.EvalToDataContext(exec, data, 0);
    result.WriteData(0, data, 0, data.DataLength);
  end;

  procedure ConvDirectly;
  begin
    for var i := 0 to arr.ElementCount - 1 do
      result.SetFromExpr(i, exec, arr.Elements[i]);
  end;

var
  elementTyp: TTypeSymbol;
begin
  arr := TArrayConstantExpr(expr);

  elementTyp := TDynamicArraySymbol(Typ).Typ;
  CreateNewDynamicArray(elementTyp, result);
  if arr.ElementCount > 0 then
  begin
    result.ArrayLength := arr.ElementCount;
    if elementTyp.Size = 1 then
      ConvDirectly
    else
      ConvThroughDataContext;
  end;
end;

// GetIsConstant
//
function TConvArrayConstantToDynamicExpr.GetIsConstant: Boolean;
begin
  result := False;
end;

// ------------------
// ------------------ TConvStaticArrayToDynamicExpr ------------------
// ------------------

// Create
//
constructor TConvStaticArrayToDynamicExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr; toTyp: TDynamicArraySymbol);
begin
  inherited Create(context, aScriptPos, expr);
  Typ := toTyp;
end;

// EvalAsVariant
//
procedure TConvStaticArrayToDynamicExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  dynArray: IScriptDynArray;
begin
  EvalAsScriptDynArray(exec, dynArray);
  VarCopySafe(result, dynArray);
end;

// EvalAsScriptDynArray
//
procedure TConvStaticArrayToDynamicExpr.EvalAsScriptDynArray
  (exec: TdwsExecution; var result: IScriptDynArray);
var
  elementTyp: TTypeSymbol;
  staticTyp: TStaticArraySymbol;
  dc: IDataContext;
begin
  elementTyp := TDynamicArraySymbol(Typ).Typ;
  CreateNewDynamicArray(elementTyp, result);

  staticTyp := expr.Typ.UnAliasedType as TStaticArraySymbol;

  (expr as TDataExpr).GetDataPtr(exec, dc);
  result.ArrayLength := staticTyp.ElementCount;
  result.WriteData(0, dc, 0, staticTyp.ElementCount * staticTyp.Typ.Size);
end;

// GetIsConstant
//
function TConvStaticArrayToDynamicExpr.GetIsConstant: Boolean;
begin
  result := False;
end;

// ------------------
// ------------------ TConvExternalExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TConvExternalExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  expr.EvalAsVariant(exec, result);
end;

// ------------------
// ------------------ TConvSetOfToIntegerExpr ------------------
// ------------------

// EvalAsInteger
//
function TConvSetOfToIntegerExpr.EvalAsInteger(exec: TdwsExecution): Int64;
begin
  result := TDataExpr(expr).DataPtr[exec].AsInteger[0];
end;

// ------------------
// ------------------ TAsCastExpr ------------------
// ------------------

// Create
//
constructor TAsCastExpr.Create(context: TdwsCompilerContext;
  const aPos: TScriptPos; expr: TTypedExpr; toTyp: TTypeSymbol);
begin
  inherited Create(context, aPos, expr);
  FPos := aPos;
  FTyp := toTyp;
end;

// EvalAsVariant
//
procedure TAsCastExpr.EvalAsVariant(exec: TdwsExecution; var result: Variant);
var
  scriptObj: IScriptObj;
begin
  EvalAsScriptObj(exec, scriptObj);
  VarCopySafe(result, scriptObj);
end;

// ------------------
// ------------------ TClassAsClassExpr ------------------
// ------------------

// RaiseMetaClassCastFailed
//
procedure TClassAsClassExpr.RaiseMetaClassCastFailed(exec: TdwsExecution;
  classSym: TClassSymbol);
begin
  RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos, RTE_MetaClassCastFailed,
    [classSym.Caption, FTyp.Name]))
end;

// EvalAsVariant
//
procedure TClassAsClassExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  ref: TClassSymbol;
begin
  ref := TClassSymbol(expr.EvalAsInteger(exec));
  VarCopySafe(result, Int64(ref));

  if ref <> nil then
  begin
    if not FTyp.IsCompatible(ref.MetaSymbol) then
      RaiseMetaClassCastFailed(exec, ref);
  end;
end;

// ------------------
// ------------------ TObjAsClassExpr ------------------
// ------------------

// RaiseInstanceClassCastFailed
//
procedure TObjAsClassExpr.RaiseInstanceClassCastFailed(exec: TdwsExecution;
  classSym: TClassSymbol);
begin
  RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos,
    RTE_ClassInstanceCastFailed, [classSym.Caption, FTyp.Caption]))
end;

// EvalAsScriptObj
//
procedure TObjAsClassExpr.EvalAsScriptObj(exec: TdwsExecution;
  var result: IScriptObj);
begin
  expr.EvalAsScriptObj(exec, result);

  if Assigned(result) and not(FTyp.IsCompatible(result.classSym)) then
    RaiseInstanceClassCastFailed(exec, result.classSym);
end;

// ------------------
// ------------------ TObjToClassTypeExpr ------------------
// ------------------

// Create
//
constructor TObjToClassTypeExpr.Create(context: TdwsBaseSymbolsContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr);
begin
  inherited Create(context, aScriptPos, expr);
  Typ := (expr.Typ as TStructuredTypeSymbol).MetaSymbol;
end;

// EvalAsVariant
//
procedure TObjToClassTypeExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  obj: IScriptObj;
begin
  expr.EvalAsScriptObj(exec, obj);
  if obj = nil then
    VarCopySafe(result, Int64(0))
  else
    VarCopySafe(result, Int64(obj.classSym));
end;

// ------------------
// ------------------ TObjAsIntfExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TObjAsIntfExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);

  procedure RaiseIntfCastFailed(const obj: IScriptObj);
  begin
    RaiseScriptError(exec, EClassCast.CreatePosFmt(scriptPos,
      RTE_ObjCastToIntfFailed, [obj.classSym.Caption, FTyp.Caption]))
  end;

var
  intf: TScriptInterface;
  resolved: TResolvedInterface;
  obj: IScriptObj;
begin
  expr.EvalAsScriptObj(exec, obj);

  if Assigned(obj) then
  begin
    if not obj.classSym.ResolveInterface(TInterfaceSymbol(Typ), resolved) then
      RaiseIntfCastFailed(obj);
    intf := TScriptInterface.Create(obj, resolved);
  end
  else
    intf := nil;
  VarCopySafe(result, IScriptObjInterface(intf));
end;

// ------------------
// ------------------ TIntfAsIntfExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TIntfAsIntfExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);

  procedure RaiseIntfCastFailed(const obj: IScriptObj);
  begin
    RaiseScriptError(exec, EClassCast.CreatePosFmt(scriptPos,
      RTE_IntfCastToIntfFailed, [obj.classSym.Caption, FTyp.Caption]))
  end;

var
  scriptIntf: IScriptObjInterface;
  intf: TScriptInterface;
  instance: IScriptObj;
  resolved: TResolvedInterface;
begin
  expr.EvalAsScriptObjInterface(exec, scriptIntf);

  if Assigned(scriptIntf) then
  begin
    instance := TScriptInterface(scriptIntf.GetSelf).instance;
    if not instance.classSym.ResolveInterface(TInterfaceSymbol(Typ), resolved)
    then
      RaiseIntfCastFailed(instance);
    intf := TScriptInterface.Create(instance, resolved);
  end
  else
    intf := nil;
  VarCopySafe(result, IUnknown(intf));
end;

// ------------------
// ------------------ TIntfAsClassExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TIntfAsClassExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);

  procedure RaiseIntfCastFailed(const obj: IScriptObj);
  begin
    RaiseScriptError(exec, EClassCast.CreatePosFmt(scriptPos,
      RTE_IntfCastToObjFailed, [obj.classSym.Caption, FTyp.Caption]))
  end;

var
  scriptIntf: IScriptObjInterface;
  obj: IScriptObj;
  intf: TScriptInterface;
begin
  expr.EvalAsScriptObjInterface(exec, scriptIntf);

  if Assigned(scriptIntf) then
  begin
    intf := TScriptInterface(scriptIntf.GetSelf);
    obj := intf.instance;
    if not obj.classSym.IsCompatible(FTyp) then
      RaiseIntfCastFailed(obj);
  end
  else
    VarCopySafe(result, IUnknown(nil));
end;

// ------------------
// ------------------ TConvDataExpr ------------------
// ------------------

// Create
//
constructor TConvDataExpr.Create(const scriptPos: TScriptPos; expr: TTypedExpr;
  aTyp: TTypeSymbol);
begin
  inherited Create(scriptPos, aTyp);
  FExpr := expr;
end;

// Destroy
//
destructor TConvDataExpr.Destroy;
begin
  inherited;
  FExpr.Free;
end;

// IsWritable
//
function TConvDataExpr.IsWritable: Boolean;
begin
  result := False;
end;

// GetSubExpr
//
function TConvDataExpr.GetSubExpr(i: Integer): TExprBase;
begin
  result := FExpr;
end;

// GetSubExprCount
//
function TConvDataExpr.GetSubExprCount: Integer;
begin
  result := 1;
end;

// GetIsConstant
//
function TConvDataExpr.GetIsConstant: Boolean;
begin
  result := FExpr.IsConstant;
end;

// ------------------
// ------------------ TConvStaticArrayToSetOfExpr ------------------
// ------------------

// Create
//
constructor TConvStaticArrayToSetOfExpr.Create(const scriptPos: TScriptPos;
  expr: TArrayConstantExpr; toTyp: TSetOfSymbol);
begin
  inherited Create(scriptPos, expr, toTyp);
end;

// ToConstExpr
//
function TConvStaticArrayToSetOfExpr.ToConstExpr(exec: TdwsExecution)
  : TConstExpr;
var
  data: IDataContext;
begin
  GetDataPtr(exec, data);
  result := TConstExpr.CreateData(scriptPos, Typ, data);
end;

// GetDataPtr
//
procedure TConvStaticArrayToSetOfExpr.GetDataPtr(exec: TdwsExecution;
  var result: IDataContext);
var
  i, v: Integer;
  i64: Int64;
  setOfSym: TSetOfSymbol;
  arrayExpr: TArrayConstantExpr;
begin
  setOfSym := TSetOfSymbol(Typ);

  if exec <> nil then
    result := exec.Stack.CreateEmpty(setOfSym.Size)
  else
    result := TDataContext.CreateStandalone(setOfSym.Size);
  setOfSym.InitDataContext(result, 0);

  arrayExpr := TArrayConstantExpr(expr);
  for i := 0 to arrayExpr.ElementCount - 1 do
  begin
    v := arrayExpr.Elements[i].EvalAsInteger(exec) - setOfSym.MinValue;
    if Cardinal(v) < Cardinal(setOfSym.CountValue) then
    begin
      i64 := result.AsInteger[v shr 6];
      result.AsInteger[v shr 6] := i64 or (Int64(1) shl (v and 63));
    end;
  end;
end;

// ------------------
// ------------------ TConvIntegerToSetOfExpr ------------------
// ------------------

// Create
//
constructor TConvIntegerToSetOfExpr.Create(const scriptPos: TScriptPos;
  expr: TTypedExpr; toTyp: TSetOfSymbol);
begin
  inherited Create(scriptPos, expr, toTyp);
  Assert(toTyp.Size = 1);
end;

// GetDataPtr
//
procedure TConvIntegerToSetOfExpr.GetDataPtr(exec: TdwsExecution;
  var result: IDataContext);
begin
  exec.DataContext_CreateEmpty(1, result);
  result.AsInteger[0] := expr.EvalAsInteger(exec);
end;

end.
