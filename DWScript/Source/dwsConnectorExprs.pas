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
unit dwsConnectorExprs;

{$I dws.inc}

interface

uses
  SysUtils, Variants,
  dwsUtils, dwsDataContext, dwsStack, dwsXPlatform,
  dwsExprs, dwsExprList, dwsSymbols, dwsUnitSymbols, dwsConnectorSymbols,
  dwsCoreExprs, dwsScriptSource, dwsCompilerContext;

type
  TConnectorCallFlag = (ccfIsInstruction, ccfIsIndex, ccfHasVarParams,
    ccfComplexArgs);
  TConnectorCallFlags = set of TConnectorCallFlag;

  TBaseConnectorCallExpr = class(TDataExpr)
  private
    FArguments: TExprBaseListRec;
    FName: String;

  protected
    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;
    function GetBaseExpr: TTypedExpr;
    procedure SetBaseExpr(expr: TTypedExpr);

  public
    constructor Create(const aScriptPos: TScriptPos; const aName: String;
      aBaseExpr: TTypedExpr);
    destructor Destroy; override;

    procedure AddArg(expr: TTypedExpr);

    property BaseExpr: TTypedExpr read GetBaseExpr write SetBaseExpr;
  end;

  // TODO : split between Complex & Fast classes
  // (has structual implication because of the late binding)
  TConnectorCallExpr = class sealed(TBaseConnectorCallExpr)
  private
    FConnectorArgsCall: IConnectorArgsCall;
    FConnectorFastCall: IConnectorFastCall;
    FConnectorParams: TConnectorParamArray;
    FIsWritable: Boolean;
    FFlags: TConnectorCallFlags;

  protected
    function GetIsIndex: Boolean; inline;

    procedure ComplexEvalAsVariant(exec: TdwsExecution; var result: Variant);
    procedure FastEvalAsVariant(exec: TdwsExecution; var result: Variant);
    function FastEvalAsFloat(exec: TdwsExecution): Double;
    function FastEvalAsInteger(exec: TdwsExecution): Int64;

  public
    constructor Create(const aScriptPos: TScriptPos; const aName: String;
      aBaseExpr: TTypedExpr; isWrite: Boolean = True; isIndex: Boolean = False);

    function AssignConnectorSym(prog: TdwsProgram;
      const connectorType: IConnectorType): Boolean;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    function EvalAsInteger(exec: TdwsExecution): Int64; override;
    function EvalAsFloat(exec: TdwsExecution): Double; override;
    function IsWritable: Boolean; override;

    procedure GetDataPtr(exec: TdwsExecution;
      var result: IDataContext); override;

    function ConnectorCall: IConnectorCall;

    property isWrite: Boolean read FIsWritable write FIsWritable;
    property isIndex: Boolean read GetIsIndex;
  end;

  // TConnectorReadMemberExpr
  //
  TConnectorReadMemberExpr = class(TDataExpr)
  private
    FBaseExpr: TTypedExpr;
    FName: String;
    FWritable: Boolean;

  protected
    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

  public
    class function CreateNew(const aScriptPos: TScriptPos; const aName: String;
      aBaseExpr: TTypedExpr; const aConnectorType: IConnectorType)
      : TConnectorReadMemberExpr; static;
    destructor Destroy; override;

    function IsWritable: Boolean; override;

    property BaseExpr: TTypedExpr read FBaseExpr write FBaseExpr;
    property Name: String read FName write FName;
  end;

  // TConnectorReadExpr
  //
  TConnectorReadExpr = class sealed(TConnectorReadMemberExpr)
  private
    FConnectorMember: IConnectorDataMember;

  public
    procedure GetDataPtr(exec: TdwsExecution;
      var result: IDataContext); override;

    property ConnectorMember: IConnectorDataMember read FConnectorMember
      write FConnectorMember;
  end;

  // TConnectorFastReadExpr
  //
  TConnectorFastReadExpr = class sealed(TConnectorReadMemberExpr)
  private
    FConnectorMember: IConnectorFastMember;

  public
    procedure GetDataPtr(exec: TdwsExecution;
      var result: IDataContext); override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    function EvalAsBoolean(exec: TdwsExecution): Boolean; override;
    function EvalAsFloat(exec: TdwsExecution): Double; override;

    property ConnectorMember: IConnectorFastMember read FConnectorMember
      write FConnectorMember;
  end;

  // TConnectorWriteMemberExpr
  //
  TConnectorWriteMemberExpr = class(TTypedExpr)
  private
    FBaseExpr: TTypedExpr;
    FValueExpr: TTypedExpr;
    FName: String;
    FScriptPos: TScriptPos;

  protected
    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

  public
    class function CreateNew(context: TdwsCompilerContext;
      const scriptPos: TScriptPos; const aName: String;
      aBaseExpr, aValueExpr: TTypedExpr; const connectorType: IConnectorType)
      : TConnectorWriteMemberExpr;
    destructor Destroy; override;

    function scriptPos: TScriptPos; override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;

    property Name: String read FName write FName;
    property BaseExpr: TTypedExpr read FBaseExpr write FBaseExpr;
    property ValueExpr: TTypedExpr read FValueExpr write FValueExpr;
  end;

  TConnectorWriteExpr = class sealed(TConnectorWriteMemberExpr)
  private
    FConnectorMember: IConnectorDataMember;

  public
    procedure EvalNoResult(exec: TdwsExecution); override;

    property ConnectorMember: IConnectorDataMember read FConnectorMember
      write FConnectorMember;
  end;

  TConnectorFastWriteExpr = class sealed(TConnectorWriteMemberExpr)
  private
    FConnectorMember: IConnectorFastMember;

  public
    procedure EvalNoResult(exec: TdwsExecution); override;

    property ConnectorMember: IConnectorFastMember read FConnectorMember
      write FConnectorMember;
  end;

  TConnectorForInExpr = class sealed(TNoResultExpr)
  private
    FInExpr: TTypedExpr;
    FLoopVarExpr: TTypedExpr;
    FDoExpr: TProgramExpr;
    FConnectorEnumerator: IConnectorEnumerator;

  protected
    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

  public
    constructor Create(const scriptPos: TScriptPos;
      const enumerator: IConnectorEnumerator; loopVarExpr, inExpr: TTypedExpr);
    destructor Destroy; override;

    procedure EvalNoResult(exec: TdwsExecution); override;

    procedure EnumerateSteppableExprs(const callback: TExprBaseProc); override;

    property ConnectorEnumerator: IConnectorEnumerator read FConnectorEnumerator
      write FConnectorEnumerator;
    property inExpr: TTypedExpr read FInExpr write FInExpr;
    property loopVarExpr: TTypedExpr read FLoopVarExpr write FLoopVarExpr;
    property DoExpr: TProgramExpr read FDoExpr write FDoExpr;
  end;

  TConnectorCastExpr = class sealed(TUnaryOpExpr)
  private
    FConnectorCast: IConnectorCast;

  public
    constructor CreateCast(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr;
      const cast: IConnectorCast);

    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;

    property ConnectorCast: IConnectorCast read FConnectorCast
      write FConnectorCast;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsStrings, dwsErrors;

// ------------------
// ------------------ TBaseConnectorCallExpr ------------------
// ------------------

// Create
//
constructor TBaseConnectorCallExpr.Create(const aScriptPos: TScriptPos;
  const aName: String; aBaseExpr: TTypedExpr);
begin
  inherited Create(aScriptPos, nil);
  FName := aName;
  FArguments.Add(aBaseExpr);
end;

// Destroy
//
destructor TBaseConnectorCallExpr.Destroy;
begin
  FArguments.Clean;
  inherited;
end;

// GetSubExpr
//
function TBaseConnectorCallExpr.GetSubExpr(i: Integer): TExprBase;
begin
  result := FArguments.ExprBase[i];
end;

// GetSubExprCount
//
function TBaseConnectorCallExpr.GetSubExprCount: Integer;
begin
  result := FArguments.Count;
end;

// GetBaseExpr
//
function TBaseConnectorCallExpr.GetBaseExpr: TTypedExpr;
begin
  result := TTypedExpr(FArguments.ExprBase[0]);
end;

// SetBaseExpr
//
procedure TBaseConnectorCallExpr.SetBaseExpr(expr: TTypedExpr);
begin
  FArguments.ExprBase[0] := expr;
end;

// AddArg
//
procedure TBaseConnectorCallExpr.AddArg(expr: TTypedExpr);
begin
  FArguments.Add(expr);
end;

// ------------------
// ------------------ TConnectorExpr ------------------
// ------------------

// Create
//
constructor TConnectorCallExpr.Create(const aScriptPos: TScriptPos;
  const aName: String; aBaseExpr: TTypedExpr; isWrite: Boolean;
  isIndex: Boolean);
begin
  inherited Create(aScriptPos, aName, aBaseExpr);
  if isWrite then
    Include(FFlags, ccfIsInstruction);
  FIsWritable := isWrite;
  if isIndex then
    Include(FFlags, ccfIsIndex);
end;

// AssignConnectorSym
//
function TConnectorCallExpr.AssignConnectorSym(prog: TdwsProgram;
  const connectorType: IConnectorType): Boolean;
var
  i: Integer;
  typSym: TTypeSymbol;
  arg: TTypedExpr;
  autoVarParams, hasVarParams: Boolean;
  call: IConnectorCall;
begin
  // Prepare the parameter information array to query the connector symbol
  if FArguments.Count > 63 then
    prog.CompileMsgs.AddCompilerErrorFmt(scriptPos,
      CPE_ConnectorTooManyArguments, [FArguments.Count - 1]);

  autoVarParams := connectorType.autoVarParams;
  hasVarParams := False;
  SetLength(FConnectorParams, FArguments.Count - 1);
  for i := 0 to FArguments.Count - 2 do
  begin
    arg := TTypedExpr(FArguments.ExprBase[i + 1]);
    FConnectorParams[i].IsVarParam := autoVarParams and (arg is TDataExpr) and
      TDataExpr(arg).IsWritable and not(arg.Typ is TArraySymbol);
    FConnectorParams[i].typSym := arg.Typ;
    hasVarParams := hasVarParams or FConnectorParams[i].IsVarParam;
    if (arg.Typ = nil) or (arg.Typ.Size > 1) or
      (arg.Typ.ClassType = TDynamicArraySymbol) then
      Include(FFlags, ccfComplexArgs);
  end;
  if hasVarParams then
    Include(FFlags, ccfHasVarParams);

  if not connectorType.AcceptsParams(FConnectorParams) then
  begin
    if FName <> '' then
    begin
      prog.CompileMsgs.AddCompilerErrorFmt(scriptPos, CPE_MethodConnectorParams,
        [FName, connectorType.ConnectorCaption])
    end
    else
    begin
      prog.CompileMsgs.AddCompilerErrorFmt(scriptPos, CPE_ConnectorParams,
        [connectorType.ConnectorCaption]);
    end;
  end;

  // Ask the connector symbol if such a method exists
  try
    if ccfIsIndex in FFlags then
      call := connectorType.HasIndex(FName, FConnectorParams, typSym,
        FIsWritable)
    else
    begin
      FIsWritable := False;
      call := connectorType.HasMethod(FName, FConnectorParams, typSym);
    end;
  except
    on E: ECompileException do
    begin
      prog.CompileMsgs.AddCompilerError(scriptPos, E.Message);
      Exit(False);
    end
    else
      raise;
  end;

  result := Assigned(call);
  if result then
  begin
    FTyp := typSym;
    call.QueryInterface(IConnectorFastCall, FConnectorFastCall);
    if FConnectorFastCall = nil then
      call.QueryInterface(IConnectorArgsCall, FConnectorArgsCall)
  end;
  if (FConnectorArgsCall = nil) and (FConnectorFastCall = nil) then
  begin
    prog.CompileMsgs.AddCompilerErrorFmt(scriptPos, CPE_ConnectorCall,
      [FName, connectorType.ConnectorCaption])
  end;
end;

// EvalAsVariant
//
procedure TConnectorCallExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  if FConnectorFastCall <> nil then
    FastEvalAsVariant(exec, result)
  else
    ComplexEvalAsVariant(exec, result);
end;

// EvalAsInteger
//
function TConnectorCallExpr.EvalAsInteger(exec: TdwsExecution): Int64;

  function Fallback: Int64;
  var
    v: Variant;
  begin
    ComplexEvalAsVariant(exec, v);
    result := VariantToInt64(v);
  end;

begin
  if FConnectorFastCall <> nil then
    result := FastEvalAsInteger(exec)
  else
    result := Fallback;
end;

// EvalAsFloat
//
function TConnectorCallExpr.EvalAsFloat(exec: TdwsExecution): Double;

  function Fallback: Double;
  var
    v: Variant;
  begin
    ComplexEvalAsVariant(exec, v);
    result := VariantToFloat(v);
  end;

begin
  if FConnectorFastCall <> nil then
    result := FastEvalAsFloat(exec)
  else
    result := Fallback;
end;

// ComplexEvalAsVariant
//
procedure TConnectorCallExpr.ComplexEvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  callArgs: TConnectorArgs;

  function DynamicArrayToVariantArray(const dyn: IScriptDynArray): Variant;
  var
    i: Integer;
    len: Integer;
    v: Variant;
  begin
    len := dyn.ArrayLength;
    result := VarArrayCreate([0, len - 1], varVariant);
    for i := 0 to len - 1 do
    begin
      dyn.EvalAsVariant(i, v);
      result[i] := v;
    end;
  end;

  procedure EvalComplexArgs;
  var
    i: Integer;
    arg: TTypedExpr;
    argTyp: TTypeSymbol;
    dyn: IScriptDynArray;
    sourcePtr: IDataContext;
  begin
    for i := 0 to FArguments.Count - 2 do
    begin
      arg := TTypedExpr(FArguments.ExprBase[i + 1]);
      argTyp := FConnectorParams[i].typSym;
      SetLength(callArgs[i], argTyp.Size);
      if argTyp.Size = 1 then
      begin
        if argTyp.ClassType = TDynamicArraySymbol then
        begin
          arg.EvalAsScriptDynArray(exec, dyn);
          callArgs[i][0] := DynamicArrayToVariantArray(dyn);
        end
        else
          arg.EvalAsVariant(exec, callArgs[i][0]);
      end
      else
      begin
        sourcePtr := TDataExpr(arg).DataPtr[exec];
        sourcePtr.CopyData(callArgs[i], 0, argTyp.Size);
      end;
    end;
  end;

  procedure AssignVarParams;
  var
    i: Integer;
    locData: IDataContext;
  begin
    for i := 0 to High(FConnectorParams) do
    begin
      if FConnectorParams[i].IsVarParam then
      begin
        exec.DataContext_Create(callArgs[i], 0, locData);
        TDataExpr(FArguments.ExprBase[i + 1]).AssignData(exec, locData);
      end;
    end;
  end;

  procedure DirectReferenceCall(var resultData: TData);
  var
    buf: Variant;
    dc: IDataContext;
  begin
    dc := TDataExpr(BaseExpr).DataPtr[exec];
    dc.EvalAsVariant(0, buf);
    try
      resultData := FConnectorArgsCall.call(buf, callArgs);
    finally
      dc.AsVariant[0] := buf;
    end;
  end;

var
  i: Integer;
  arg: TExprBase;
  buf: Variant;
  resultData: TData;
begin
  if exec.IsDebugging then
    exec.Debugger.EnterFunc(exec, Self);

  // Call function
  try
    SetLength(callArgs, FArguments.Count - 1);
    if ccfComplexArgs in FFlags then
      EvalComplexArgs
    else
    begin
      for i := 0 to FArguments.Count - 2 do
      begin
        SetLength(callArgs[i], 1);
        arg := FArguments.ExprBase[i + 1];
        arg.EvalAsVariant(exec, callArgs[i][0]);
      end;
    end;

    try
      // The call itself
      if FConnectorArgsCall.NeedDirectReference then
      begin
        DirectReferenceCall(resultData);
      end
      else
      begin
        BaseExpr.EvalAsVariant(exec, buf);
        resultData := FConnectorArgsCall.call(buf, callArgs);
      end;
    except
      on E: EScriptError do
      begin
        EScriptError(E).scriptPos := scriptPos;
        raise;
      end
      else
        exec.SetScriptError(Self);
      raise;
    end;

    if ccfHasVarParams in FFlags then
      AssignVarParams;
  finally
    if exec.IsDebugging then
      exec.Debugger.LeaveFunc(exec, Self);
  end;

  if Length(resultData) > 0 then
    result := resultData[0]
  else
    VarClearSafe(result);
end;

// FastEvalAsVariant
//
procedure TConnectorCallExpr.FastEvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  callArgs: TExprBaseListExec;
begin
  if exec.IsDebugging then
    exec.Debugger.EnterFunc(exec, Self);
  try
    callArgs.ListRec := FArguments;
    callArgs.exec := exec;
    FConnectorFastCall.FastCall(callArgs, result);
  finally
    if exec.IsDebugging then
      exec.Debugger.LeaveFunc(exec, Self);
  end;
end;

// FastEvalAsFloat
//
function TConnectorCallExpr.FastEvalAsFloat(exec: TdwsExecution): Double;
var
  callArgs: TExprBaseListExec;
begin
  if exec.IsDebugging then
    exec.Debugger.EnterFunc(exec, Self);
  try
    callArgs.ListRec := FArguments;
    callArgs.exec := exec;
    result := FConnectorFastCall.FastCallFloat(callArgs);
  finally
    if exec.IsDebugging then
      exec.Debugger.LeaveFunc(exec, Self);
  end;
end;

// FastEvalAsInteger
//
function TConnectorCallExpr.FastEvalAsInteger(exec: TdwsExecution): Int64;
var
  callArgs: TExprBaseListExec;
begin
  if exec.IsDebugging then
    exec.Debugger.EnterFunc(exec, Self);
  try
    callArgs.ListRec := FArguments;
    callArgs.exec := exec;
    result := FConnectorFastCall.FastCallInteger(callArgs);
  finally
    if exec.IsDebugging then
      exec.Debugger.LeaveFunc(exec, Self);
  end;

end;

// GetDataPtr
//
procedure TConnectorCallExpr.GetDataPtr(exec: TdwsExecution;
  var result: IDataContext);
var
  data: TData;
begin
  SetLength(data, 1);
  EvalAsVariant(exec, data[0]);
  result := exec.Stack.CreateDataContext(data, 0);
end;

// ConnectorCall
//
function TConnectorCallExpr.ConnectorCall: IConnectorCall;
begin
  if FConnectorFastCall <> nil then
    result := FConnectorFastCall
  else
    result := FConnectorArgsCall;
end;

// IsWritable
//
function TConnectorCallExpr.IsWritable: Boolean;
begin
  result := FIsWritable;
end;

// GetIsIndex
//
function TConnectorCallExpr.GetIsIndex: Boolean;
begin
  result := (ccfIsIndex in FFlags);
end;

// ------------------
// ------------------ TConnectorReadMemberExpr ------------------
// ------------------

// Create
//
class function TConnectorReadMemberExpr.CreateNew(const aScriptPos: TScriptPos;
  const aName: String; aBaseExpr: TTypedExpr;
  const aConnectorType: IConnectorType): TConnectorReadMemberExpr;
var
  connMember: IConnectorMember;
  connFastMember: IConnectorFastMember;
  connDataMember: IConnectorDataMember;
  typSym: TTypeSymbol;
begin
  typSym := nil;
  connMember := aConnectorType.HasMember(aName, typSym, False);
  if connMember = nil then
    Exit(nil);

  connMember.QueryInterface(IConnectorFastMember, connFastMember);

  if Assigned(connFastMember) then
  begin

    result := TConnectorFastReadExpr.Create(aScriptPos, typSym);
    TConnectorFastReadExpr(result).ConnectorMember := connFastMember;

  end
  else
  begin

    connMember.QueryInterface(IConnectorDataMember, connDataMember);

    result := TConnectorReadExpr.Create(aScriptPos, typSym);
    TConnectorReadExpr(result).ConnectorMember := connDataMember;

  end;

  result.Name := aName;
  result.BaseExpr := aBaseExpr;
  result.FWritable := aConnectorType.WritableReads(aName);
end;

// Destroy
//
destructor TConnectorReadMemberExpr.Destroy;
begin
  FBaseExpr.Free;
  inherited;
end;

// IsWritable
//
function TConnectorReadMemberExpr.IsWritable: Boolean;
begin
  result := FWritable;
end;

// GetSubExpr
//
function TConnectorReadMemberExpr.GetSubExpr(i: Integer): TExprBase;
begin
  result := FBaseExpr
end;

// GetSubExprCount
//
function TConnectorReadMemberExpr.GetSubExprCount: Integer;
begin
  result := 1;
end;

// ------------------
// ------------------ TConnectorReadExpr ------------------
// ------------------

// GetDataPtr
//
procedure TConnectorReadExpr.GetDataPtr(exec: TdwsExecution;
  var result: IDataContext);
var
  base: Variant;
  resultData: TData;
begin
  try
    FBaseExpr.EvalAsVariant(exec, base);
    resultData := FConnectorMember.Read(base);
  except
    on E: EScriptError do
    begin
      EScriptError(E).scriptPos := scriptPos;
      raise;
    end
    else
      exec.SetScriptError(Self);
    raise;
  end;

  exec.DataContext_Create(resultData, 0, result);
end;

// ------------------
// ------------------ TConnectorFastReadExpr ------------------
// ------------------

// GetDataPtr
//
procedure TConnectorFastReadExpr.GetDataPtr(exec: TdwsExecution;
  var result: IDataContext);
var
  resultData: TData;
begin
  SetLength(resultData, 1);
  EvalAsVariant(exec, resultData[0]);
  exec.DataContext_Create(resultData, 0, result);
end;

// EvalAsVariant
//
procedure TConnectorFastReadExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  try
    FConnectorMember.FastRead(exec, BaseExpr, result);
  except
    on E: EScriptError do
    begin
      EScriptError(E).scriptPos := scriptPos;
      raise;
    end
    else
      exec.SetScriptError(Self);
    raise;
  end;
end;

// EvalAsBoolean
//
function TConnectorFastReadExpr.EvalAsBoolean(exec: TdwsExecution): Boolean;
begin
  try
    result := FConnectorMember.FastReadBoolean(exec, BaseExpr);
  except
    on E: EScriptError do
    begin
      EScriptError(E).scriptPos := scriptPos;
      raise;
    end
    else
      exec.SetScriptError(Self);
    raise;
  end;
end;

// EvalAsFloat
//
function TConnectorFastReadExpr.EvalAsFloat(exec: TdwsExecution): Double;
begin
  try
    result := FConnectorMember.FastReadFloat(exec, BaseExpr);
  except
    on E: EScriptError do
    begin
      EScriptError(E).scriptPos := scriptPos;
      raise;
    end
    else
      exec.SetScriptError(Self);
    raise;
  end;
end;

// ------------------
// ------------------ TConnectorWriteMemberExpr ------------------
// ------------------

// CreateNew
//
class function TConnectorWriteMemberExpr.CreateNew(context: TdwsCompilerContext;
  const scriptPos: TScriptPos; const aName: String;
  aBaseExpr, aValueExpr: TTypedExpr; const connectorType: IConnectorType)
  : TConnectorWriteMemberExpr;
var
  connMember: IConnectorMember;
  connFastMember: IConnectorFastMember;
  connDataMember: IConnectorDataMember;
  typSym: TTypeSymbol;
begin
  connMember := connectorType.HasMember(aName, typSym, True);
  if connMember <> nil then
  begin

    if not(Assigned(typSym) and Assigned(aValueExpr.Typ) and
      typSym.IsCompatible(aValueExpr.Typ)) then
      context.Msgs.AddCompilerError(scriptPos, CPE_ConnectorTypeMismatch);

    connMember.QueryInterface(IConnectorFastMember, connFastMember);

    if connFastMember <> nil then
    begin

      result := TConnectorFastWriteExpr.Create;
      TConnectorFastWriteExpr(result).ConnectorMember := connFastMember;

    end
    else
    begin

      connMember.QueryInterface(IConnectorDataMember, connDataMember);

      result := TConnectorWriteExpr.Create;
      TConnectorWriteExpr(result).ConnectorMember := connDataMember;
      if connDataMember = nil then
        context.Msgs.AddCompilerErrorFmt(scriptPos, CPE_ConnectorMember,
          [aName, connectorType.ConnectorCaption]);

    end;

    result.FScriptPos := scriptPos;
    result.FName := aName;
    result.FBaseExpr := aBaseExpr;
    result.FValueExpr := aValueExpr;

  end
  else
  begin

    aBaseExpr.Free;
    aValueExpr.Free;
    result := nil;

  end;
end;

// Destroy
//
destructor TConnectorWriteMemberExpr.Destroy;
begin
  FBaseExpr.Free;
  FValueExpr.Free;
  inherited;
end;

// ScriptPos
//
function TConnectorWriteMemberExpr.scriptPos: TScriptPos;
begin
  result := FScriptPos;
end;

// EvalAsVariant
//
procedure TConnectorWriteMemberExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  EvalNoResult(exec);
end;

// GetSubExpr
//
function TConnectorWriteMemberExpr.GetSubExpr(i: Integer): TExprBase;
begin
  if i = 0 then
    result := FBaseExpr
  else
    result := FValueExpr;
end;

// GetSubExprCount
//
function TConnectorWriteMemberExpr.GetSubExprCount: Integer;
begin
  result := 2;
end;

// ------------------
// ------------------ TConnectorWriteExpr ------------------
// ------------------

// EvalNoResult
//
procedure TConnectorWriteExpr.EvalNoResult(exec: TdwsExecution);
var
  data: TData;
  tmp: Variant;
begin
  FBaseExpr.EvalAsVariant(exec, tmp);

  SetLength(data, 1);
  FValueExpr.EvalAsVariant(exec, data[0]);
  try
    FConnectorMember.Write(tmp, data);
    if FBaseExpr.IsWritable then
      FBaseExpr.AssignValue(exec, tmp);
  except
    on E: EScriptError do
    begin
      EScriptError(E).scriptPos := FScriptPos;
      raise;
    end
    else
      exec.SetScriptError(Self);
    raise;
  end;
end;

// ------------------
// ------------------ TConnectorFastWriteExpr ------------------
// ------------------

// EvalNoResult
//
procedure TConnectorFastWriteExpr.EvalNoResult(exec: TdwsExecution);
begin
  try
    FConnectorMember.FastWrite(exec, BaseExpr, ValueExpr);
  except
    on E: EScriptError do
    begin
      EScriptError(E).scriptPos := FScriptPos;
      raise;
    end
    else
      exec.SetScriptError(Self);
    raise;
  end;
end;

// ------------------
// ------------------ TConnectorForInExpr ------------------
// ------------------

// Create
//
constructor TConnectorForInExpr.Create(const scriptPos: TScriptPos;
  const enumerator: IConnectorEnumerator; loopVarExpr, inExpr: TTypedExpr);
begin
  inherited Create(scriptPos);
  Assert(loopVarExpr is TVarExpr);
  FConnectorEnumerator := enumerator;
  FLoopVarExpr := loopVarExpr;
  FInExpr := inExpr;
end;

// Destroy
//
destructor TConnectorForInExpr.Destroy;
begin
  inherited;
  FLoopVarExpr.Free;
  FInExpr.Free;
  FDoExpr.Free;
end;

// GetSubExpr
//
function TConnectorForInExpr.GetSubExpr(i: Integer): TExprBase;
begin
  case i of
    0:
      result := loopVarExpr;
    1:
      result := inExpr;
  else
    result := DoExpr;
  end;
end;

// GetSubExprCount
//
function TConnectorForInExpr.GetSubExprCount: Integer;
begin
  result := 3;
end;

// EvalNoResult
//
procedure TConnectorForInExpr.EvalNoResult(exec: TdwsExecution);
var
  base: Variant;
  item: TData;
  enumData: IUnknown;
begin
  FInExpr.EvalAsVariant(exec, base);
  if VarIsClear(base) then
    Exit;
  enumData := ConnectorEnumerator.NewEnumerator(base, nil);
  if enumData <> nil then
  begin
    SetLength(item, 1);
    while ConnectorEnumerator.Step(enumData, item) do
    begin
      TVarExpr(FLoopVarExpr).AssignValue(exec, item[0]);
      exec.DoStep(DoExpr);
      DoExpr.EvalNoResult(exec);
      if exec.Status <> esrNone then
      begin
        case exec.Status of
          esrBreak:
            begin
              exec.Status := esrNone;
              break;
            end;
          esrContinue:
            exec.Status := esrNone;
          esrExit:
            Exit;
        end;
      end;
      exec.DoStep(Self);
    end;
  end;
end;

// EnumerateSteppableExprs
//
procedure TConnectorForInExpr.EnumerateSteppableExprs(const callback
  : TExprBaseProc);
begin
  callback(DoExpr);
end;

// ------------------
// ------------------ TConnectorCastExpr ------------------
// ------------------

// CreateCast
//
constructor TConnectorCastExpr.CreateCast(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr; const cast: IConnectorCast);
begin
  inherited Create(context, aScriptPos, expr);
  FConnectorCast := cast;
end;

// EvalAsVariant
//
procedure TConnectorCastExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  buf: Variant;
begin
  expr.EvalAsVariant(exec, buf);
  VarCopySafe(result, FConnectorCast.CastVariant(buf));
end;

end.
