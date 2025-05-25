unit dwsExternalFunctions;
deprecated;

{$I dws.inc}

interface

uses
  SysUtils,
  dwsXPlatform, dwsExprList, dwsUtils,
  dwsCompiler, dwsExprs, dwsMagicExprs, dwsSymbols, dwsFunctions,
  dwsExternalFunctionJIT;

type
  TBoxedTypeConverter = class
  private
    FValue: TTypeLookupData;
  public
    constructor Create(value: TTypeLookupData);
    property value: TTypeLookupData read FValue;
  end;

  TExternalFunctionManager = class(TInterfacedObject,
    IdwsExternalFunctionsManager)
  private
    FCompiler: IdwsCompiler;
    FRoutines: TSimpleNameObjectHash<TInternalFunction>;
    class var FTypeMap: TSimpleNameObjectHash<TBoxedTypeConverter>;
    class constructor Create;
    class destructor Destroy;
    class function LookupType(const name: UnicodeString): TTypeLookupData;
  protected
    procedure BeginCompilation(const compiler: IdwsCompiler);
    procedure EndCompilation(const compiler: IdwsCompiler);

    function ConvertToMagicSymbol(value: TFuncSymbol): TFuncSymbol;
    function CreateExternalFunction(funcSymbol: TFuncSymbol): IExternalRoutine;
    procedure RegisterTypeMapping(const name: UnicodeString;
      const typ: TTypeLookupData);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterExternalFunction(const name: UnicodeString;
      address: Pointer; ignoreIfMissing: Boolean = False);

    property compiler: IdwsCompiler read FCompiler;

  end;

  TExternalProcedure = class(TInternalMagicProcedure, IExternalRoutine)
  private type
    TProcedureStub = procedure(const args: TExprBaseListExec);

  private
    FBuffer: TBytes;
    FStub: TProcedureStub;
    FCalls: TFunctionCallArray;
    FTryFrame: TTryFrame;

    procedure SetExternalPointer(value: Pointer);

  public
    constructor Create(aFuncSymbol: TFuncSymbol; prog: TdwsProgram);
    destructor Destroy; override;
    procedure DoEvalProc(const args: TExprBaseListExec); override;
  end;

  TExternalFunction = class(TInternalMagicVariantFunction, IExternalRoutine)
  private type
    TVariantFunctionStub = function(const args: TExprBaseListExec): Variant;

  private
    FBuffer: TBytes;
    FStub: TVariantFunctionStub;
    FCalls: TFunctionCallArray;
    FTryFrame: TTryFrame;

    procedure SetExternalPointer(value: Pointer);

  public
    constructor Create(aFuncSymbol: TFuncSymbol; prog: TdwsProgram);
    destructor Destroy; override;

    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var result: Variant); override;
  end;

implementation

uses
  Windows,
  dwsStrings,
  dwsTokenTypes{$IFDEF CPU386}, dwsExternalFunctionJitx86{$ENDIF};

type
  TdwsExternalStubJit = class
  private
    FBuffer: TBytes;
    FInternalJit: IExternalFunctionJit;

    procedure Clear;
  public
    destructor Destroy; override;
    procedure Eval(funcSymbol: TFuncSymbol; prog: TdwsProgram);
  end;

procedure RaiseUnHandledExternalCall(exec: TdwsExecution; func: TFuncSymbol);
begin
  raise EdwsExternalFuncHandler.CreateFmt(RTE_UnHandledExternalCall,
    [func.name, '']);
end;

function MakeExecutable(const value: TBytes; const calls: TFunctionCallArray;
  call: Pointer; const tryFrame: TTryFrame): Pointer;
var
  oldprotect: cardinal;
  lCall, lOffset: nativeInt;
  ptr: Pointer;
  fixup: TFunctionCall;
  info: _MEMORY_BASIC_INFORMATION;
begin
  result := VirtualAlloc(nil, length(value), MEM_RESERVE or MEM_COMMIT,
    PAGE_READWRITE);
  if result = nil then
    OutOfMemoryError;
  system.Move(value[0], result^, length(value));
  for fixup in calls do
  begin
    ptr := @PByte(result)[fixup.offset];
    if fixup.call = 0 then
      lCall := nativeInt(call)
    else
      lCall := fixup.call;
    lOffset := (lCall - nativeInt(ptr)) - sizeof(Pointer);
    PNativeInt(ptr)^ := lOffset;
  end;
  if tryFrame[0] <> 0 then
  begin
    ptr := @PByte(result)[tryFrame[0]];
    if PPointer(ptr)^ <> nil then
      asm int 3
      end;
    PPointer(ptr)^ := @PByte(result)[tryFrame[2] - 1];

    ptr := @PByte(result)[tryFrame[1]];
    if PPointer(ptr)^ <> nil then
      asm int 3
      end;
    PPointer(ptr)^ := @PByte(result)[tryFrame[3]];
  end;

  if not VirtualProtect(result, length(value), PAGE_EXECUTE, oldprotect) then
    RaiseLastOSError;
  VirtualQuery(result, info, sizeof(info));
  if info.Protect <> PAGE_EXECUTE then
    raise Exception.Create('VirtualProtect failed');
end;

procedure MakeNotExecutable(value: Pointer);
begin
  if assigned(value) then
    if not VirtualFree(value, 0, MEM_RELEASE) then
      RaiseLastOSError;
end;

{ TExternalProcedure }

constructor TExternalProcedure.Create(aFuncSymbol: TFuncSymbol;
  prog: TdwsProgram);
var
  jit: TdwsExternalStubJit;
begin
  funcSymbol := aFuncSymbol;

  assert(assigned(aFuncSymbol));
  assert(aFuncSymbol.result = nil);
  assert(aFuncSymbol.Executable = nil);
  assert(aFuncSymbol.ExternalConvention in [ttREGISTER .. ttSTDCALL]);
  jit := TdwsExternalStubJit.Create;
  try
    jit.Eval(aFuncSymbol, prog);
    FBuffer := jit.FBuffer;
    FCalls := jit.FInternalJit.GetCalls;
    if jit.FInternalJit.HasTryFrame then
      FTryFrame := jit.FInternalJit.GetTryFrame;
  finally
    jit.Free;
  end;
end;

destructor TExternalProcedure.Destroy;
begin
  MakeNotExecutable(@FStub);
  inherited Destroy;
end;

procedure TExternalProcedure.DoEvalProc(const args: TExprBaseListExec);
begin
  if not assigned(FStub) then
    RaiseUnHandledExternalCall(args.exec, funcSymbol);
  FStub(args);
end;

procedure TExternalProcedure.SetExternalPointer(value: Pointer);
begin
  if assigned(FStub) then
    raise Exception.Create('External function cannot be assigned twice');
  FStub := MakeExecutable(FBuffer, FCalls, value, FTryFrame);
end;

{ TExternalFunction }

constructor TExternalFunction.Create(aFuncSymbol: TFuncSymbol;
  prog: TdwsProgram);
var
  jit: TdwsExternalStubJit;
begin
  funcSymbol := aFuncSymbol;

  assert(assigned(aFuncSymbol));
  assert(assigned(aFuncSymbol.result));
  assert(aFuncSymbol.Executable = nil);
  assert(aFuncSymbol.ExternalConvention in [ttREGISTER .. ttSTDCALL]);
  jit := TdwsExternalStubJit.Create;
  try
    jit.Eval(aFuncSymbol, prog);
    FBuffer := jit.FBuffer;
    FCalls := jit.FInternalJit.GetCalls;
    if jit.FInternalJit.HasTryFrame then
      FTryFrame := jit.FInternalJit.GetTryFrame;
  finally
    jit.Free;
  end;
end;

destructor TExternalFunction.Destroy;
begin
  MakeNotExecutable(@FStub);
  inherited Destroy;
end;

procedure TExternalFunction.DoEvalAsVariant(const args: TExprBaseListExec;
  var result: Variant);
begin
  if not assigned(FStub) then
    RaiseUnHandledExternalCall(args.exec, funcSymbol);
  result := FStub(args);
end;

procedure TExternalFunction.SetExternalPointer(value: Pointer);
begin
  if assigned(FStub) then
    raise Exception.Create('External function cannot be assigned twice');
  FStub := MakeExecutable(FBuffer, FCalls, value, FTryFrame);
end;

{ TdwsExternalStubJit }

destructor TdwsExternalStubJit.Destroy;
begin
  Clear;
  inherited;
end;

procedure TdwsExternalStubJit.Clear;
begin
  FBuffer := nil;
  FInternalJit := nil;
end;

procedure TdwsExternalStubJit.Eval(funcSymbol: TFuncSymbol; prog: TdwsProgram);
var
  i: integer;
begin
  Clear;
  FInternalJit := JitFactory(funcSymbol.ExternalConvention, prog,
    TExternalFunctionManager.LookupType);
  if assigned(funcSymbol.result) then
    FInternalJit.BeginFunction(funcSymbol.result.typ, funcSymbol.Params)
  else
    FInternalJit.BeginProcedure(funcSymbol.Params);
  for i := 0 to funcSymbol.Params.Count - 1 do
    FInternalJit.PassParam(funcSymbol.Params[i]);
  FInternalJit.call;
  FInternalJit.PostCall;
  FBuffer := FInternalJit.GetBytes;
end;

// ------------------
// ------------------ TExternalFunctionManager ------------------
// ------------------

class constructor TExternalFunctionManager.Create;
begin
  FTypeMap := TSimpleNameObjectHash<TBoxedTypeConverter>.Create;
end;

class destructor TExternalFunctionManager.Destroy;
begin
  FTypeMap.Clean;
  FTypeMap.Free;
end;

// Create
//
constructor TExternalFunctionManager.Create;
begin
  inherited;
  FRoutines := TSimpleNameObjectHash<TInternalFunction>.Create;
end;

// Destroy
//
destructor TExternalFunctionManager.Destroy;
begin
  inherited;
  FRoutines.Free;
end;

// BeginCompilation
//
procedure TExternalFunctionManager.BeginCompilation(const compiler
  : IdwsCompiler);
begin
  assert(FCompiler = nil, 'Only one session supported right now');
  FCompiler := compiler;
  FRoutines.Clear;
end;

// EndCompilation
//
procedure TExternalFunctionManager.EndCompilation(const compiler: IdwsCompiler);
begin
  assert(FCompiler = compiler);
  FCompiler := nil;
end;

class function TExternalFunctionManager.LookupType(const name: UnicodeString)
  : TTypeLookupData;
var
  conv: TBoxedTypeConverter;
begin
  conv := FTypeMap.Objects[name];
  if conv = nil then
    raise Exception.CreateFmt('No type info is registered for %s.', [name]);
  result := conv.value;
end;

// ConvertToMagicSymbol
//
function TExternalFunctionManager.ConvertToMagicSymbol(value: TFuncSymbol)
  : TFuncSymbol;
var
  i: integer;
begin
  // TODO: add check that value really is supported as an external symbol
  // (parameter types, etc.)
  result := TMagicFuncSymbol.Create(value.name, value.Kind, value.Level);
  result.typ := value.typ;
  for i := 0 to value.Params.Count - 1 do
    result.AddParam(value.Params[i].Clone);
  value.Free;
end;

// CreateExternalFunction
//
function TExternalFunctionManager.CreateExternalFunction
  (funcSymbol: TFuncSymbol): IExternalRoutine;
begin
  if assigned(funcSymbol.result) then
    result := TExternalFunction.Create(funcSymbol, compiler.CurrentProg.Root)
  else
    result := TExternalProcedure.Create(funcSymbol, compiler.CurrentProg.Root);
  if not FRoutines.AddObject(funcSymbol.name,
    result.GetSelf as TInternalFunction) then
    compiler.Msgs.AddCompilerErrorFmt(compiler.Tokenizer.HotPos,
      CPE_DuplicateExternal, [funcSymbol.name]);
end;

// RegisterExternalFunction
//
procedure TExternalFunctionManager.RegisterExternalFunction
  (const name: UnicodeString; address: Pointer;
  ignoreIfMissing: Boolean = False);
var
  func: TInternalFunction;
  ext: IExternalRoutine;
begin
  func := FRoutines.Objects[name];
  if func = nil then
  begin
    if ignoreIfMissing then
      Exit;
    raise Exception.CreateFmt
      ('No external function named "%s" is registered', [name]);
  end;
  assert(supports(func, IExternalRoutine, ext));
  ext.SetExternalPointer(address);
end;

procedure TExternalFunctionManager.RegisterTypeMapping
  (const name: UnicodeString; const typ: TTypeLookupData);
begin
  FTypeMap.AddObject(name, TBoxedTypeConverter.Create(typ));
end;

{ TBoxedTypeConverter }

constructor TBoxedTypeConverter.Create(value: TTypeLookupData);
begin
  FValue := value;
end;

end.
