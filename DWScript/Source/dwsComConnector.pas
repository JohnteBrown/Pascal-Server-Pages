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
unit dwsComConnector;

{$I dws.inc}

interface

uses
  System.Classes,
  dwsUtils, dwsDataContext, dwsExprList, dwsConnectorSymbols,
  dwsMagicExprs, dwsExprs, dwsComp, dwsSymbols, dwsOperators, dwsUnitSymbols,
  dwsScriptSource;

const
  COM_ConnectorCaption = 'COM Connector 2.0';
  COM_UnitName = 'COM';

type
  TdwsComConnector = class(TdwsAbstractStaticUnit, IUnknown, IConnector)
  private
    function ConnectorCaption: UnicodeString;
    function ConnectorName: UnicodeString;
    function GetUnit(const UnitName: UnicodeString): IConnectorType;

  protected
    procedure AddUnitSymbols(systemTable: TSystemSymbolTable;
      Table: TSymbolTable; operators: TOperators); override;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property StaticSymbols;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  System.Win.ComObj, Winapi.ActiveX, System.SysUtils, System.Variants,
  dwsStrings, dwsXPlatform, dwsFunctions, dwsCompilerUtils,
  dwsArrayMethodKinds, dwsConvExprs;

const
  MaxDispArgs = 64;
  DISP_E_PARAMNOTFOUND = HRESULT($80020004);
  LOCALE_SYSTEM_DEFAULT = $0800;

var
  vUnassigned: Variant;

  // DwsOleCheck
  //
procedure DwsOleCheck(Result: HRESULT);

  procedure RaiseOleError;
  begin
    raise EScriptError.CreateFmt('OLE Error %.8x (%s)',
      [Cardinal(Result), SysErrorMessage(Cardinal(Result))]);
  end;

begin
  if not Succeeded(Result) then
    RaiseOleError;
end;

// RaiseOleError
//
procedure RaiseOleError(err: HRESULT; const excepInfo: TExcepInfo);
var
  msg: UnicodeString;
begin
  msg := excepInfo.bstrDescription;
  if excepInfo.bstrSource <> '' then
  begin
    if msg <> '' then
      msg := excepInfo.bstrSource + ', ' + msg
    else
      msg := excepInfo.bstrSource;
  end;
  if msg <> '' then
    msg := ' from ' + msg;
  raise EScriptError.CreateFmt('OLE Error %.8x (%s)%s',
    [err, SysErrorMessage(Cardinal(err)), msg]);
end;

type
  POleParams = ^TOleParams;
  TOleParams = array [0 .. MaxDispArgs - 1] of PVarData;

  TStringDesc = record
    BStr: PWideChar;
    PStr: PString;
  end;

  PStringDesc = ^TStringDesc;

  // DispatchInvoke
  //
function DispatchInvoke(const dispatch: IDispatch;
  invKind, namedArgCount: Integer; dispIDs: PDispIDList;
  const connArgs: TConnectorArgs; PResult: PVariant): HRESULT;
var
  i, argType, strCount: Integer;
  dispParams: TDispParams;
  strings: array [0 .. MaxDispArgs - 1] of TStringDesc;
  argPtr: PVariantArg;
  args: array [0 .. MaxDispArgs - 1] of TVariantArg;
  param: PVarData;
  dispID: Integer;
  excepInfo: TExcepInfo;
begin
  strCount := 0;
{$IFNDEF DELPHI_TOKYO_PLUS}
  Result := S_OK;
{$ENDIF}
  // Fill in the dispParams struct
  FillChar(strings, MaxDispArgs * SizeOf(TStringDesc), 0);
  FillChar(args, MaxDispArgs * SizeOf(TVariantArg), 0);
  try
    argPtr := @args[0];
    for i := High(connArgs) downto 0 do
    begin
      param := @connArgs[i][0];
      argType := param.VType and varTypeMask;

      if (param.VType and varArray) <> 0 then
      begin

        argPtr.vt := VT_ARRAY Or argType;
        argPtr.parray := PSafeArray(param.VArray);

      end
      else
      begin

        case argType of
          varSmallint:
            begin
              argPtr.vt := VT_I2 or VT_BYREF;
              argPtr.plVal := @param.VSmallInt;
            end;
          varInteger:
            begin
              argPtr.vt := VT_I4 or VT_BYREF;
              argPtr.plVal := @param.VInteger;
            end;
          varInt64:
            begin
              argPtr.vt := VT_I8 or VT_BYREF;
              argPtr.plVal := @param.VInt64;
            end;
          varSingle:
            begin
              argPtr.vt := VT_R4 or VT_BYREF;
              argPtr.pdblVal := @param.VSingle;
            end;
          varDouble:
            begin
              argPtr.vt := VT_R8 or VT_BYREF;
              argPtr.pdblVal := @param.VDouble;
            end;
          varBoolean:
            begin
              argPtr.vt := VT_BOOL or VT_BYREF;
              argPtr.pbool := @param.VBoolean;
            end;
          varDate:
            begin
              argPtr.vt := VT_DATE or VT_BYREF;
              argPtr.pdate := @param.VDate;
            end;
          varString:
            begin
              // Transform Delphi-strings to OLE-strings
              strings[strCount].BStr :=
                StringToOleStr(AnsiString(param.VString));
              strings[strCount].PStr := @param.VString;
              argPtr.vt := VT_BSTR or VT_BYREF;
              argPtr.pbstrVal := @strings[strCount].BStr;
              Inc(strCount);
            end;
          varUString:
            begin
              // Transform Delphi-strings to OLE-strings
              strings[strCount].BStr :=
                StringToOleStr(UnicodeString(param.VUString));
              strings[strCount].PStr := @param.VUString;
              argPtr.vt := VT_BSTR or VT_BYREF;
              argPtr.pbstrVal := @strings[strCount].BStr;
              Inc(strCount);
            end;
          varOleStr:
            begin
              argPtr.vt := VT_BSTR or VT_BYREF;
              argPtr.pbstrVal := @param.VOleStr;
            end;
          varDispatch:
            begin
              argPtr.vt := VT_DISPATCH or VT_BYREF;
              argPtr.pdispVal := @param.VDispatch;
            end;
          varError:
            begin
              argPtr.vt := VT_ERROR;
              argPtr.scode := DISP_E_PARAMNOTFOUND;
            end;
          varVariant, varEmpty, varNull:
            begin
              argPtr.vt := varVariant or VT_BYREF;
              argPtr.pvarVal := PVariant(param);
            end;
        else
          raise Exception.CreateFmt
            ('Unsupported data type (%d) for DWScript COM Connector!',
            [argType]);
        end;
      end;
      Inc(argPtr);
    end;
    dispParams.rgvarg := @args;
    dispParams.cArgs := Length(connArgs);

    dispID := dispIDs[0];

    if invKind = DISPATCH_PROPERTYPUT then
    begin

      if (args[0].vt and varTypeMask) = varDispatch then
        invKind := DISPATCH_PROPERTYPUTREF;
      dispParams.rgdispidNamedArgs := dispIDs;
      dispParams.cNamedArgs := namedArgCount + 1;
      dispIDs[0] := DISPID_PROPERTYPUT;

    end
    else
    begin

      dispParams.rgdispidNamedArgs := @dispIDs[1];
      dispParams.cNamedArgs := namedArgCount;

    end;

    FillChar(excepInfo, SizeOf(excepInfo), 0);

    PVarData(PResult).VType := varSingle;

    // Invoke COM Method
    Result := dispatch.Invoke(dispID, GUID_NULL, 0, invKind, dispParams,
      PResult, @excepInfo, nil);

    if Result = S_OK then
    begin
      for i := strCount - 1 downto 0 do
      begin
        if strings[i].PStr <> nil then
          OleStrToStrVar(strings[i].BStr, strings[i].PStr^);
      end;
    end
    else
    begin
      RaiseOleError(Result, excepInfo);
    end;

  finally
    for i := strCount - 1 downto 0 do
      SysFreeString(strings[i].BStr);
  end;
end;

// DispatchGetPropOrCall
//
function DispatchGetPropOrCall(const disp: IDispatch; dispID: Integer)
  : OleVariant;
var
  excepInfo: TExcepInfo;
  dispParams: TDispParams;
  err: HRESULT;
begin
  FillChar(dispParams, SizeOf(dispParams), 0);
  err := disp.Invoke(dispID, GUID_NULL, 0, DISPATCH_PROPERTYGET or
    DISPATCH_METHOD, dispParams, @Result, @excepInfo, nil);
  if err <> S_OK then
    RaiseOleError(err, excepInfo);
end;

// DispatchSetProp
//
procedure DispatchSetProp(const disp: IDispatch; dispID: Integer;
  const value: OleVariant);
const
  dispIDNamedArgs: Longint = DISPID_PROPERTYPUT;
var
  excepInfo: TExcepInfo;
  dispParams: TDispParams;
  err: HRESULT;
  flags: Word;
begin
  dispParams.rgvarg := @value;
  dispParams.rgdispidNamedArgs := @dispIDNamedArgs;
  dispParams.cArgs := 1;
  dispParams.cNamedArgs := 1;
  if VarType(value) = varDispatch then
    flags := DISPATCH_PROPERTYPUT or DISPATCH_PROPERTYPUTREF
  else
    flags := DISPATCH_PROPERTYPUT;
  err := disp.Invoke(dispID, GUID_NULL, 0, flags, dispParams, nil,
    @excepInfo, nil);
  if err <> S_OK then
    RaiseOleError(err, excepInfo);
end;

// Connection to
//
var
  vWbemLocator: OleVariant;
  vWbemLocatorMRSW: TMultiReadSingleWrite;

function WbemLocatorConnect(const path: UnicodeString;
  const user: UnicodeString = ''; const pass: UnicodeString = ''): OleVariant;
var
  p: Integer;
  server, namespace: UnicodeString;
begin
  p := Pos(':\\', path);
  server := Copy(path, p + 3, MaxInt);
  p := Pos('\', server);
  namespace := Copy(server, p + 1, MaxInt);
  SetLength(server, p - 1);

  vWbemLocatorMRSW.BeginWrite;
  try
    if VarIsClear(vWbemLocator) then
      vWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
    Result := vWbemLocator.ConnectServer(server, namespace, user, pass);
  finally
    vWbemLocatorMRSW.EndWrite;
  end;
end;

type
  TCreateOleObjectFunc = class(TInternalMagicVariantFunction)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TCreateComObjectFunc = class(TInternalMagicVariantFunction)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TGetActiveOleObjectFunc = class(TInternalMagicVariantFunction)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TClassIDToProgIDFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args: TExprBaseListExec;
      var Result: UnicodeString); override;
  end;

  TOleConversionFunc = class(TInternalMagicVariantFunction);

  TOleInt16Func = class(TOleConversionFunc)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TOleInt32Func = class(TOleConversionFunc)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TOleInt64Func = class(TOleConversionFunc)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TOleCurrencyFunc = class(TOleConversionFunc)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TOleDateFunc = class(TOleConversionFunc)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TOleSingleFunc = class(TOleConversionFunc)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TOleDoubleFunc = class(TOleConversionFunc)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TOleStringFunc = class(TOleConversionFunc)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var Result: Variant); override;
  end;

  TComVarClearFunc = class(TInternalMagicProcedure)
    procedure DoEvalProc(const args: TExprBaseListExec); override;
  end;

  TBinaryToGUIDFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args: TExprBaseListExec;
      var Result: UnicodeString); override;
  end;

  TGUIDToBinaryFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args: TExprBaseListExec;
      var Result: UnicodeString); override;
  end;

  TComConnectorType = class(TInterfacedSelfObject, IUnknown, IConnectorType,
    IConnectorEnumerator)
  private
    FTable: TSymbolTable;

  protected
    { IConnectorType }
    function ComVariantSymbol: TTypeSymbol;

    function ConnectorCaption: UnicodeString;
    function AutoVarParams: Boolean;
    function AcceptsParams(const params: TConnectorParamArray): Boolean;
    function WritableReads(const memberName: UnicodeString): Boolean;

    function HasMethod(const aMethodName: UnicodeString;
      const aParams: TConnectorParamArray; var TypSym: TTypeSymbol)
      : IConnectorCall;
    function HasMember(const aMemberName: UnicodeString;
      var TypSym: TTypeSymbol; isWrite: Boolean): IConnectorMember;
    function HasIndex(const aPropName: UnicodeString;
      const aParams: TConnectorParamArray; var TypSym: TTypeSymbol;
      isWrite: Boolean): IConnectorCall;
    function HasEnumerator(var TypSym: TTypeSymbol): IConnectorEnumerator;
    function HasCast(TypSym: TTypeSymbol): IConnectorCast;

    function NewEnumerator(const base: Variant; const args: TConnectorArgs)
      : IUnknown;
    function Step(const enumerator: IInterface; var data: TData): Boolean;

  public
    constructor Create(Table: TSymbolTable);
  end;

  TComConnectorCall = class(TInterfacedSelfObject, IUnknown, IConnectorCall,
    IConnectorArgsCall)
  private
    FMethodName: WideString;
    FPMethodName: PWideString;
    FMethodType: Cardinal;

  protected
    function Call(const base: Variant; const args: TConnectorArgs): TData;
    function NeedDirectReference: Boolean;

  public
    constructor Create(const aMethodName: UnicodeString; aMethodType: Cardinal);
  end;

  TComConnectorMember = class(TInterfacedSelfObject, IUnknown, IConnectorMember,
    IConnectorDataMember)
  private
    FMemberName: WideString;
    FPMemberName: PWideString;

  protected
    function GetDispID(const disp: IDispatch): Integer;
    function Read(const base: Variant): TData;
    procedure Write(const base: Variant; const data: TData);

  public
    constructor Create(const memberName: UnicodeString);
  end;

  TComVariantArraySymbol = class(TConnectorSymbol)
  public
    constructor Create(const name: UnicodeString;
      const connectorType: IConnectorType; Typ: TTypeSymbol);

    function IsCompatible(TypSym: TTypeSymbol): Boolean; override;
    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
  end;

  TComVariantArrayMember = class(TInterfacedSelfObject, IConnectorMember)
    procedure Write(const base: Variant; const data: TData);
  end;

  TComVariantArrayLengthMember = class(TComVariantArrayMember,
    IConnectorFastMember)
    procedure FastRead(const exec: TdwsExecution; const base: TExprBase;
      var Result: Variant);
    procedure FastWrite(const exec: TdwsExecution;
      const base, value: TExprBase);
    function FastReadBoolean(const exec: TdwsExecution;
      const base: TExprBase): Boolean;
    function FastReadInteger(const exec: TdwsExecution;
      const base: TExprBase): Int64;
    function FastReadFloat(const exec: TdwsExecution;
      const base: TExprBase): Double;
  end;

  TComVariantArrayHighBoundMember = class(TComVariantArrayMember,
    IConnectorDataMember)
    function Read(const base: Variant): TData;
    procedure Write(const base: Variant; const data: TData);
  end;

  TComVariantArrayLowBoundMember = class(TComVariantArrayMember,
    IConnectorDataMember)
    function Read(const base: Variant): TData;
  end;

  TComVariantArrayDimCountMember = class(TComVariantArrayMember,
    IConnectorDataMember)
    function Read(const base: Variant): TData;
  end;

  TComVariantArrayCall = class(TInterfacedSelfObject, IConnectorCall)
    function NeedDirectReference: Boolean;
  end;

  TComVariantArrayReadIndex = class(TComVariantArrayCall, IConnectorArgsCall)
    function Call(const base: Variant; const args: TConnectorArgs): TData;
  end;

  TComVariantArrayWriteIndex = class(TComVariantArrayCall, IConnectorArgsCall)
    function Call(const base: Variant; const args: TConnectorArgs): TData;
  end;

  TComVariantArrayLengthCall = class(TComVariantArrayCall, IConnectorFastCall)
    procedure FastCall(const args: TExprBaseListExec; var Result: Variant);
    function FastCallInteger(const args: TExprBaseListExec): Int64;
    function FastCallFloat(const args: TExprBaseListExec): Double;
  end;

  TComVariantArrayHighBoundCall = class(TComVariantArrayCall,
    IConnectorFastCall)
    procedure FastCall(const args: TExprBaseListExec; var Result: Variant);
    function FastCallInteger(const args: TExprBaseListExec): Int64;
    function FastCallFloat(const args: TExprBaseListExec): Double;
  end;

  TComVariantArrayLowBoundCall = class(TComVariantArrayCall, IConnectorFastCall)
    procedure FastCall(const args: TExprBaseListExec; var Result: Variant);
    function FastCallInteger(const args: TExprBaseListExec): Int64;
    function FastCallFloat(const args: TExprBaseListExec): Double;
  end;

  TComVariantArrayEnumerator = class(TInterfacedSelfObject)
  private
    FArray: Variant;
    FIndex: Integer;

  public
    constructor Create(const a: Variant);

    function Next(var v: Variant): Boolean;
  end;

  TComVariantArrayType = class(TInterfacedSelfObject, IUnknown, IConnectorType,
    IConnectorEnumerator)
  private
    FTable: TSystemSymbolTable;
    FLengthMember: IConnectorMember;
    FLengthCall: IConnectorCall;
    FLowBoundMember, FHighBoundMember: IConnectorMember;
    FLowBoundCall, FHighBoundCall: IConnectorCall;
    FDimCountMember: IConnectorMember;
    FArrayWriteIndex, FArrayReadIndex: IConnectorCall;

  protected
    function NewEnumerator(const base: Variant; const args: TConnectorArgs)
      : IUnknown;
    function Step(const enumerator: IInterface; var data: TData): Boolean;

    { IConnectorType }
    function ConnectorCaption: UnicodeString;
    function AutoVarParams: Boolean;
    function AcceptsParams(const params: TConnectorParamArray): Boolean;
    function WritableReads(const memberName: UnicodeString): Boolean;

    function HasMethod(const methodName: UnicodeString;
      const params: TConnectorParamArray; var TypSym: TTypeSymbol)
      : IConnectorCall;
    function HasMember(const memberName: UnicodeString; var TypSym: TTypeSymbol;
      isWrite: Boolean): IConnectorMember;
    function HasIndex(const PropName: UnicodeString;
      const params: TConnectorParamArray; var TypSym: TTypeSymbol;
      isWrite: Boolean): IConnectorCall;
    function HasEnumerator(var TypSym: TTypeSymbol): IConnectorEnumerator;
    function HasCast(TypSym: TTypeSymbol): IConnectorCast;

    { IConnectorEnumerator }
    function IConnectorEnumerator.NewEnumerator = NewEnumerator;
    function IConnectorEnumerator.Step = Step;

  public
    constructor Create(Table: TSystemSymbolTable);
  end;

  // ------------------
  // ------------------ TdwsComConnector ------------------
  // ------------------

  // Create
  //
constructor TdwsComConnector.Create(AOwner: TComponent);
begin
  inherited;
  UnitName := COM_UnitName;
end;

function TdwsComConnector.ConnectorCaption: UnicodeString;
begin
  Result := COM_ConnectorCaption;
end;

function TdwsComConnector.ConnectorName: UnicodeString;
begin
  Result := COM_UnitName;
end;

function TdwsComConnector.GetUnit(const UnitName: UnicodeString)
  : IConnectorType;
begin
  raise Exception.Create('Not supported');
end;

procedure TdwsComConnector.AddUnitSymbols(systemTable: TSystemSymbolTable;
  Table: TSymbolTable; operators: TOperators);
var
  v: Variant;
  comVariantSym: TTypeSymbol;
begin
  // Datatype of com-objects
  comVariantSym := TConnectorSymbol.Create('ComVariant',
    TComConnectorType.Create(Table));
  Table.AddSymbol(comVariantSym);
  Table.AddSymbol(TAliasSymbol.Create('OleVariant', comVariantSym));

  // Optional parameter for dispatch interfaces with unnamed arguments
  v := 0;
  PVarData(@v).VType := varError;
  Table.AddSymbol(TConstSymbol.CreateValue('ComOpt',
    systemTable.TypVariant, v));

  // Function to create a new COM-Object
  TCreateOleObjectFunc.Create(Table, 'CreateOleObject',
    ['ClassName', SYS_STRING], 'ComVariant');
  TCreateComObjectFunc.Create(Table, 'CreateComObject', ['GUID', SYS_STRING],
    'ComVariant');

  TClassIDToProgIDFunc.Create(Table, 'ClassIDToProgID', ['ClassID', SYS_STRING],
    SYS_STRING);
  TGetActiveOleObjectFunc.Create(Table, 'GetActiveOleObject',
    ['ClassName', SYS_STRING], 'ComVariant');

  TOleInt16Func.Create(Table, 'OleInt16', ['v', SYS_INTEGER], 'ComVariant',
    [iffStateLess]);
  TOleInt32Func.Create(Table, 'OleInt32', ['v', SYS_INTEGER], 'ComVariant',
    [iffStateLess]);
  TOleInt64Func.Create(Table, 'OleInt64', ['v', SYS_INTEGER], 'ComVariant',
    [iffStateLess]);
  TOleCurrencyFunc.Create(Table, 'OleCurrency', ['v', SYS_VARIANT],
    'ComVariant', [iffStateLess]);
  TOleDateFunc.Create(Table, 'OleDate', ['v', SYS_FLOAT], 'ComVariant',
    [iffStateLess]);
  TOleSingleFunc.Create(Table, 'OleSingle', ['v', SYS_FLOAT], 'ComVariant',
    [iffStateLess]);
  TOleDoubleFunc.Create(Table, 'OleDouble', ['v', SYS_FLOAT], 'ComVariant',
    [iffStateLess]);
  TOleStringFunc.Create(Table, 'OleString', ['v', SYS_STRING], 'ComVariant',
    [iffStateLess]);
  TComVarClearFunc.Create(Table, 'VarClear', ['@v', 'ComVariant'], '',
    [iffOverloaded]);

  TBinaryToGUIDFunc.Create(Table, 'BinaryToGUID', ['b', SYS_STRING], SYS_STRING,
    [iffStateLess]);
  TGUIDToBinaryFunc.Create(Table, 'GUIDToBinary', ['guid', SYS_STRING],
    SYS_STRING, [iffStateLess]);

  Table.AddSymbol(TComVariantArraySymbol.Create('ComVariantArray',
    TComVariantArrayType.Create(systemTable), systemTable.TypVariant));

  operators.RegisterCaster(systemTable.TypInteger, comVariantSym,
    TConvVarToIntegerExpr, True);
  operators.RegisterCaster(systemTable.TypFloat, comVariantSym,
    TConvVarToFloatExpr, True);
  operators.RegisterCaster(systemTable.TypBoolean, comVariantSym,
    TConvVarToBoolExpr, True);
  operators.RegisterCaster(systemTable.TypString, comVariantSym,
    TConvVarToStringExpr, True);
end;

// ------------------
// ------------------ TCreateOleObjectFunc ------------------
// ------------------

procedure TCreateOleObjectFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  VarCopySafe(Result, CreateOleObject(args.AsString[0]));
end;

// ------------------
// ------------------ TCreateComObjectFunc ------------------
// ------------------

procedure TCreateComObjectFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  VarCopySafe(Result, CreateComObject(StringToGUID(args.AsString[0])));
end;

// ------------------
// ------------------ TClassIDToProgIDFunc ------------------
// ------------------

procedure TClassIDToProgIDFunc.DoEvalAsString(const args: TExprBaseListExec;
  var Result: UnicodeString);
var
  guid: TGUID;
begin
  guid := StringToGUID(args.AsString[0]);
  Result := ClassIDToProgID(guid);
end;

// ------------------
// ------------------ TGetActiveOleObjectFunc ------------------
// ------------------

procedure TGetActiveOleObjectFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
var
  n: UnicodeString;
begin
  n := args.AsString[0];
  if StrIBeginsWith(n, 'winmgmts:') then
    VarCopySafe(Result, WbemLocatorConnect(n))
  else
    VarCopySafe(Result, GetActiveOleObject(n));
end;

// ------------------
// ------------------ TOleInt16Func ------------------
// ------------------

// DoEvalAsVariant
//
procedure TOleInt16Func.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  Result := SmallInt(args.AsInteger[0]);
end;

// ------------------
// ------------------ TOleInt32Func ------------------
// ------------------

// DoEvalAsVariant
//
procedure TOleInt32Func.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  Result := Int32(args.AsInteger[0]);
end;

// ------------------
// ------------------ TOleInt64Func ------------------
// ------------------

// DoEvalAsVariant
//
procedure TOleInt64Func.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  Result := args.AsInteger[0];
end;

// ------------------
// ------------------ TOleCurrencyFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TOleCurrencyFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  args.ExprBase[0].EvalAsVariant(args.exec, Result);
  if VarType(Result) <> varCurrency then
    Result := Currency(Result);
end;

// ------------------
// ------------------ TOleDateFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TOleDateFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  Result := VarFromDateTime(args.AsFloat[0]);
end;

// ------------------
// ------------------ TOleSingleFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TOleSingleFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  VarClearSafe(Result);
  // Needed so compiler won't generate a double precision variant
  PVarData(@Result)^.VType := varSingle;
  PVarData(@Result)^.VSingle := args.AsFloat[0];
end;

// ------------------
// ------------------ TOleDoubleFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TOleDoubleFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  VarCopySafe(Result, args.AsFloat[0]);
end;

// ------------------
// ------------------ TOleStringFunc ------------------
// ------------------

// DoEvalAsVariant
//
procedure TOleStringFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var Result: Variant);
begin
  VarClearSafe(Result);
  Result := OleVariant(args.AsString[0]);
end;

// ------------------
// ------------------ TComVarClearFunc ------------------
// ------------------

procedure TComVarClearFunc.DoEvalProc(const args: TExprBaseListExec);
begin
  args.ExprBase[0].AssignValue(args.exec, vUnassigned);
end;

// ------------------
// ------------------ TBinaryToGUIDFunc ------------------
// ------------------

procedure TBinaryToGUIDFunc.DoEvalAsString(const args: TExprBaseListExec;
  var Result: UnicodeString);
var
  guid: TGUID;
  buf: RawByteString;
begin
  buf := args.AsDataString[0];
  if Length(buf) <> SizeOf(guid) then
    raise Exception.CreateFmt('Invalid GUID length (expected %d, got %d)',
      [SizeOf(guid), Length(buf)]);
  System.Move(Pointer(buf)^, guid, SizeOf(guid));
  Result := GUIDToString(guid);
end;

// ------------------
// ------------------ TGUIDToBinaryFunc ------------------
// ------------------

procedure TGUIDToBinaryFunc.DoEvalAsString(const args: TExprBaseListExec;
  var Result: UnicodeString);
var
  guid: TGUID;
  buf: RawByteString;
begin
  guid := StringToGUID(args.AsString[0]);
  SetLength(buf, SizeOf(guid));
  System.Move(guid, Pointer(buf)^, SizeOf(guid));
  RawByteStringToScriptString(buf, Result);
end;

// ------------------
// ------------------ TComConnectorType ------------------
// ------------------

// ComVariantSymbol
//
function TComConnectorType.ComVariantSymbol: TTypeSymbol;
begin
  Result := FTable.FindTypeSymbol('ComVariant', cvMagic);
end;

function TComConnectorType.ConnectorCaption: UnicodeString;
begin
  Result := COM_ConnectorCaption;
end;

// AutoVarParams
//
function TComConnectorType.AutoVarParams: Boolean;
begin
  Result := True;
end;

constructor TComConnectorType.Create(Table: TSymbolTable);
begin
  inherited Create;
  FTable := Table;
end;

function TComConnectorType.HasIndex(const aPropName: UnicodeString;
  const aParams: TConnectorParamArray; var TypSym: TTypeSymbol;
  isWrite: Boolean): IConnectorCall;
var
  methType: Cardinal;
begin
  TypSym := ComVariantSymbol;
  if isWrite then
    methType := DISPATCH_PROPERTYPUT
  else
    methType := DISPATCH_PROPERTYGET or DISPATCH_METHOD;
  Result := TComConnectorCall.Create(aPropName, methType);
end;

// HasEnumerator
//
function TComConnectorType.HasEnumerator(var TypSym: TTypeSymbol)
  : IConnectorEnumerator;
begin
  TypSym := ComVariantSymbol;
  Result := IConnectorEnumerator(Self);
end;

// HasCast
//
function TComConnectorType.HasCast(TypSym: TTypeSymbol): IConnectorCast;
begin
  Result := nil;
end;

// NewEnumerator
//
function TComConnectorType.NewEnumerator(const base: Variant;
  const args: TConnectorArgs): IUnknown;
var
  disp: IDispatch;
  dispIDs: Integer;
  outValue: Variant;
begin
  disp := base;
  dispIDs := DISPID_NEWENUM;
  DispatchInvoke(disp, DISPATCH_METHOD or DISPATCH_PROPERTYGET, 0, @dispIDs,
    nil, @outValue);
  Result := IUnknown(outValue) as IEnumVARIANT;
end;

// Step
//
function TComConnectorType.Step(const enumerator: IInterface;
  var data: TData): Boolean;
var
  fetched: LongWord;
  ov: OleVariant;
begin
  if IEnumVARIANT(enumerator).Next(1, ov, fetched) = 0 then
  begin
    Result := (fetched = 1);
    if Result then
      data[0] := ov;
  end
  else
    Result := False;
end;

function TComConnectorType.HasMember(const aMemberName: UnicodeString;
  var TypSym: TTypeSymbol; isWrite: Boolean): IConnectorMember;
begin
  TypSym := ComVariantSymbol;
  Result := TComConnectorMember.Create(aMemberName);
end;

// AcceptsParams
//
function TComConnectorType.AcceptsParams(const params
  : TConnectorParamArray): Boolean;
var
  x: Integer;
  Typ: TTypeSymbol;
begin
  for x := 0 to Length(params) - 1 do
  begin
    Typ := params[x].TypSym;
    if (Typ = nil) or (Typ.Size > 1) then
      Exit(False);
    if Typ is TArraySymbol then
      if not(Typ is TDynamicArraySymbol) then
        Exit(False);
    if Typ.AsFuncSymbol <> nil then
      Exit(False);
  end;
  Result := True;
end;

// WritableReads
//
function TComConnectorType.WritableReads(const memberName
  : UnicodeString): Boolean;
begin
  Result := False;
end;

function TComConnectorType.HasMethod(const aMethodName: UnicodeString;
  const aParams: TConnectorParamArray; var TypSym: TTypeSymbol): IConnectorCall;
begin
  TypSym := ComVariantSymbol;
  Result := TComConnectorCall.Create(aMethodName, DISPATCH_METHOD or
    DISPATCH_PROPERTYGET);
end;

// ------------------
// ------------------ TComConnectorCall ------------------
// ------------------

// Create
//
constructor TComConnectorCall.Create(const aMethodName: UnicodeString;
  aMethodType: Cardinal);
begin
  inherited Create;
  FMethodName := aMethodName;
  FPMethodName := PWideString(FMethodName);
  FMethodType := aMethodType;
end;

// Call
//
function TComConnectorCall.Call(const base: Variant;
  const args: TConnectorArgs): TData;
var
  disp: IDispatch;
  dispID: Integer;
begin
  disp := base;
  if disp = nil then
    raise EScriptError.Create(CPE_NilConnectorCall);

  if FMethodName = '' then
    dispID := 0 // default method or property
  else
    DwsOleCheck(disp.GetIDsOfNames(GUID_NULL, @FPMethodName, 1,
      LOCALE_SYSTEM_DEFAULT, @dispID));

  SetLength(Result, 1);
  DwsOleCheck(DispatchInvoke(disp, FMethodType, 0, @dispID, args, @Result[0]));
end;

// NeedDirectReference
//
function TComConnectorCall.NeedDirectReference: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TComConnectorMember ------------------
// ------------------

// Create
//
constructor TComConnectorMember.Create(const memberName: UnicodeString);
begin
  inherited Create;
  FMemberName := memberName;
  FPMemberName := PWideString(FMemberName);
end;

// GetDispID
//
function TComConnectorMember.GetDispID(const disp: IDispatch): Integer;
begin
  Result := 0;
  DwsOleCheck(disp.GetIDsOfNames(GUID_NULL, @FPMemberName, 1,
    LOCALE_SYSTEM_DEFAULT, @Result));
end;

// Read
//
function TComConnectorMember.Read(const base: Variant): TData;
var
  disp: IDispatch;
begin
  disp := base;
  if disp = nil then
    raise EScriptError.Create(CPE_NilConnectorRead);

  SetLength(Result, 1);
  Result[0] := DispatchGetPropOrCall(disp, GetDispID(disp));
end;

// Write
//
procedure TComConnectorMember.Write(const base: Variant; const data: TData);
var
  disp: IDispatch;
begin
  disp := base;
  if disp = nil then
    raise EScriptError.Create(CPE_NilConnectorWrite);

  DispatchSetProp(disp, GetDispID(disp), data[0]);
end;

// ------------------
// ------------------ TComVariantArrayType ------------------
// ------------------

constructor TComVariantArrayType.Create(Table: TSystemSymbolTable);
begin
  inherited Create;
  FTable := Table;

  FLengthMember := TComVariantArrayLengthMember.Create;
  FLengthCall := TComVariantArrayLengthCall.Create;

  FLowBoundMember := TComVariantArrayLowBoundMember.Create;
  FLowBoundCall := TComVariantArrayLowBoundCall.Create;

  FHighBoundMember := TComVariantArrayHighBoundMember.Create;
  FHighBoundCall := TComVariantArrayHighBoundCall.Create;

  FDimCountMember := TComVariantArrayDimCountMember.Create;

  FArrayWriteIndex := TComVariantArrayWriteIndex.Create;
  FArrayReadIndex := TComVariantArrayReadIndex.Create;

end;

function TComVariantArrayReadIndex.Call(const base: Variant;
  const args: TConnectorArgs): TData;
var
  Indices: array of Integer;
  x, ArgCount: Integer;
begin
  ArgCount := Length(args);
  SetLength(Result, 1);
  SetLength(Indices, ArgCount);
  for x := 0 to ArgCount - 1 do
    Indices[x] := args[x][0];
  VarCopy(Result[0], VarArrayGet(base, Indices));
end;

function TComVariantArrayWriteIndex.Call(const base: Variant;
  const args: TConnectorArgs): TData;
var
  BaseRef: PVariant;
  x, ArgCount: Integer;
  Indices: array of Integer;
begin
  ArgCount := Length(args) - 1;
  SetLength(Indices, ArgCount);
  for x := 0 to ArgCount - 1 do
    Indices[x] := args[x][0];
  BaseRef := @base; // VarArrayRef(Base); // need var-ref
  VarArrayPut(BaseRef^, args[ArgCount][0], Indices);
end;

function TComVariantArrayType.ConnectorCaption: UnicodeString;
begin
  Result := 'ComVariantArray';
end;

// AutoVarParams
//
function TComVariantArrayType.AutoVarParams: Boolean;
begin
  Result := True;
end;

function TComVariantArrayType.HasIndex(Const PropName: UnicodeString;
  const params: TConnectorParamArray; var TypSym: TTypeSymbol; isWrite: Boolean)
  : IConnectorCall;
var
  SymInteger: TTypeSymbol;
  SymVariant: TTypeSymbol;
  x, l: Integer;
begin
  Result := nil;

  SymVariant := FTable.FindTypeSymbol(SYS_VARIANT, cvMagic);
  SymInteger := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);

  l := Length(params);
  if isWrite then
  begin
    Dec(l); // Last Parameter is Put-Value
    if not SymVariant.IsCompatible(params[l].TypSym) then
      Exit;
  end;

  // Check Integer Indices
  x := 0;
  while (x < l) and SymInteger.IsCompatible(params[x].TypSym) do
    Inc(x);

  if x < l then
    Exit;

  if isWrite then
  begin
    TypSym := nil;
    Result := FArrayWriteIndex;
  end
  else
  begin
    TypSym := SymVariant;
    Result := FArrayReadIndex;
  end;
end;

// HasEnumerator
//
function TComVariantArrayType.HasEnumerator(var TypSym: TTypeSymbol)
  : IConnectorEnumerator;
begin
  TypSym := FTable.TypVariant;
  Result := IConnectorEnumerator(Self);
end;

// HasCast
//
function TComVariantArrayType.HasCast(TypSym: TTypeSymbol): IConnectorCast;
begin
  Result := nil;
end;

// HasMember
//
function TComVariantArrayType.HasMember(const memberName: UnicodeString;
  var TypSym: TTypeSymbol; isWrite: Boolean): IConnectorMember;
var
  methodKind: TArrayMethodKind;
begin
  methodKind := NameToArrayMethod(memberName, nil, cNullPos);
  if methodKind = amkHigh then
  begin
    Result := FHighBoundMember;
    TypSym := FTable.TypInteger;
  end
  else if isWrite then
    Result := nil
  else
  begin
    case methodKind of
      amkLength, amkCount:
        begin
          Result := FLengthMember;
          TypSym := FTable.TypInteger;
        end;
      amkLow:
        begin
          Result := FLowBoundMember;
          TypSym := FTable.TypInteger;
        end;
      amkDimCount:
        begin
          Result := FDimCountMember;
          TypSym := FTable.TypInteger;
        end;
    else
      Result := nil;
    end;
  end;
end;

// AcceptsParams
//
function TComVariantArrayType.AcceptsParams(const params
  : TConnectorParamArray): Boolean;
begin
  Result := (Length(params) in [1, 2]) and FTable.FindTypeSymbol(SYS_INTEGER,
    cvMagic).IsCompatible(params[0].TypSym);
end;

// WritableReads
//
function TComVariantArrayType.WritableReads(const memberName
  : UnicodeString): Boolean;
begin
  Result := False;
end;

// HasMethod
//
function TComVariantArrayType.HasMethod(const methodName: UnicodeString;
  const params: TConnectorParamArray; var TypSym: TTypeSymbol): IConnectorCall;
var
  methodKind: TArrayMethodKind;
begin
  methodKind := NameToArrayMethod(methodName, nil, cNullPos);
  case methodKind of
    amkLength, amkCount:
      begin
        Result := FLengthCall;
        TypSym := FTable.TypInteger;
      end;
    amkLow:
      begin
        Result := FLowBoundCall;
        TypSym := FTable.TypInteger;
      end;
    amkHigh:
      begin
        Result := FHighBoundCall;
        TypSym := FTable.TypInteger;
      end;
  else
    Result := nil;
  end;
end;

// NewEnumerator
//
function TComVariantArrayType.NewEnumerator(const base: Variant;
  const args: TConnectorArgs): IUnknown;
begin
  Result := TComVariantArrayEnumerator.Create(base);
end;

// Step
//
function TComVariantArrayType.Step(const enumerator: IInterface;
  var data: TData): Boolean;
begin
  Result := ((enumerator as IGetSelf).GetSelf as TComVariantArrayEnumerator)
    .Next(data[0]);
end;

// NeedDirectReference
//
function TComVariantArrayCall.NeedDirectReference: Boolean;
begin
  Result := True;
end;

// Write
//
procedure TComVariantArrayMember.Write(const base: Variant; const data: TData);
begin
  Assert(False); // we should never come here
end;

function TComVariantArrayDimCountMember.Read(const base: Variant): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayDimCount(base);
end;

function TComVariantArrayLowBoundMember.Read(const base: Variant): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayLowBound(base, 1);
end;

procedure TComVariantArrayLowBoundCall.FastCall(const args: TExprBaseListExec;
  var Result: Variant);
begin
  VarCopySafe(Result, FastCallInteger(args));
end;

// FastCallInteger
//
function TComVariantArrayLowBoundCall.FastCallInteger
  (const args: TExprBaseListExec): Int64;
var
  base: Variant;
begin
  args.EvalAsVariant(0, base);
  Result := VarArrayLowBound(base, args.AsInteger[1]);
end;

// FastCallFloat
//
function TComVariantArrayLowBoundCall.FastCallFloat
  (const args: TExprBaseListExec): Double;
begin
  Result := FastCallInteger(args);
end;

procedure TComVariantArrayHighBoundMember.Write(const base: Variant;
  const data: TData);
var
  BaseRef: Variant;
  x: Integer;
begin
  x := data[0];
  BaseRef := VarArrayRef(base);
  VarArrayRedim(BaseRef, x);
end;

procedure TComVariantArrayHighBoundCall.FastCall(const args: TExprBaseListExec;
  var Result: Variant);
begin
  VarCopySafe(Result, FastCallInteger(args));
end;

// FastCallInteger
//
function TComVariantArrayHighBoundCall.FastCallInteger
  (const args: TExprBaseListExec): Int64;
var
  base: Variant;
begin
  args.EvalAsVariant(0, base);
  Result := VarArrayHighBound(base, args.AsInteger[1]);
end;

// FastCallFloat
//
function TComVariantArrayHighBoundCall.FastCallFloat
  (const args: TExprBaseListExec): Double;
begin
  Result := FastCallInteger(args);
end;

function TComVariantArrayHighBoundMember.Read(const base: Variant): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayHighBound(base, 1);
end;

procedure TComVariantArrayLengthMember.FastRead(const exec: TdwsExecution;
  const base: TExprBase; var Result: Variant);
begin
  VarCopySafe(Result, FastReadInteger(exec, base));
end;

// FastWrite
//
procedure TComVariantArrayLengthMember.FastWrite(const exec: TdwsExecution;
  const base, value: TExprBase);
begin
  raise Exception.Create('FastWrite not supported by ' + ClassName);
end;

// FastReadBoolean
//
function TComVariantArrayLengthMember.FastReadBoolean(const exec: TdwsExecution;
  const base: TExprBase): Boolean;
begin
  Result := FastReadInteger(exec, base) <> 0;
end;

// FastReadInteger
//
function TComVariantArrayLengthMember.FastReadInteger(const exec: TdwsExecution;
  const base: TExprBase): Int64;
var
  bv: Variant;
begin
  base.EvalAsVariant(exec, bv);
  Result := VarArrayHighBound(bv, 1) - VarArrayLowBound(bv, 1) + 1;
end;

// FastReadFloat
//
function TComVariantArrayLengthMember.FastReadFloat(const exec: TdwsExecution;
  const base: TExprBase): Double;
begin
  Result := FastReadInteger(exec, base);
end;

procedure TComVariantArrayLengthCall.FastCall(const args: TExprBaseListExec;
  var Result: Variant);
begin
  VarCopySafe(Result, FastCallInteger(args));
end;

// FastCallInteger
//
function TComVariantArrayLengthCall.FastCallInteger
  (const args: TExprBaseListExec): Int64;
var
  base: Variant;
  x: Integer;
begin
  args.EvalAsVariant(0, base);
  x := args.AsInteger[1];
  Result := VarArrayHighBound(base, x) - VarArrayLowBound(base, x) + 1;
end;

// FastCallFloat
//
function TComVariantArrayLengthCall.FastCallFloat
  (const args: TExprBaseListExec): Double;
begin
  Result := FastCallInteger(args);
end;

// ------------------
// ------------------ TComVariantArraySymbol ------------------
// ------------------                                                          fedituser

function TComVariantArraySymbol.IsCompatible(TypSym: TTypeSymbol): Boolean;
begin
  // only accept comvariantarray or std-arrays or variants
  Result := (Self = TypSym) or
    (TypSym.IsBaseType and (TypSym.BaseType is TBaseVariantSymbol)) or
    (TypSym.IsBaseType and Typ.IsCompatible(TypSym.Typ));
end;

constructor TComVariantArraySymbol.Create(const name: UnicodeString;
  const connectorType: IConnectorType; Typ: TTypeSymbol);
begin
  inherited Create(Name, connectorType);
  Self.Typ := Typ;
end;

// InitDataContext
//
procedure TComVariantArraySymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.AsVariant[offset] := VarArrayCreate([0, -1], varVariant); // empty array
end;

// ------------------
// ------------------ TComVariantArrayEnumerator ------------------
// ------------------

// Create
//
constructor TComVariantArrayEnumerator.Create(const a: Variant);
begin
  inherited Create;
  FArray := a;
  FIndex := 0;
end;

// Next
//
function TComVariantArrayEnumerator.Next(var v: Variant): Boolean;
begin
  if FIndex <= VarArrayHighBound(FArray, 1) then
  begin

    v := FArray[FIndex];
    Inc(FIndex);
    Result := True;

  end
  else
    Result := False;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

vWbemLocatorMRSW := TMultiReadSingleWrite.Create;

finalization

FreeAndNil(vWbemLocatorMRSW);

end.
