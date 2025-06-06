unit UExternalFunctionTests;

interface

{$IF Defined(WIN32)}

uses
  Classes,
  dwsXPlatformTests, dwsComp, dwsErrors, dwsExprList, dwsCompiler, dwsExprs,
  dwsExternalFunctions;

type
  TExternalFunctionTests = class(TTestCase)
  private
    FTests: TStringList;
    FCompiler: TDelphiWebScript;
    FUnit: TdwsUnit;
    procedure RegisterExternalRoutines(const manager
      : IdwsExternalFunctionsManager);
    procedure FreeBoxedString(ExternalObject: TObject);

  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure Execution;
    procedure Compilation;
  published
    procedure Test;
  end;

implementation

uses
  SysUtils,
  dwsXPlatform,
  dwsSymbols, dwsUtils, dwsDataContext;

{ TExternalFunctionTests }

procedure TExternalFunctionTests.Compilation;
var
  source: TStringList;
  i: Integer;
  prog: IdwsProgram;
begin
  source := TStringList.Create;
  try

    for i := 0 to FTests.Count - 1 do
    begin

      source.LoadFromFile(FTests[i]);

      prog := FCompiler.Compile(source.Text);
      CheckEquals('', prog.Msgs.AsInfo, FTests[i]);

    end;

  finally
    source.Free;
  end;
end;

type
  TBoxedString = class
    value: string;
  end;

procedure Blank;
begin
end;

procedure Ints3(a, b, c: Integer);
begin
  assert(a = 1);
  assert(b = 5);
  assert(c = 6);
end;

procedure TestString(a: Integer; b: string);
begin
  assert(a = 5);
  assert(b = 'Testing');
end;

procedure TestStringExc(a: Integer; b: string);
begin
  TestString(a, b);
  Abort;
end;

procedure TestBool(a: Integer; b: boolean);
begin
  assert(a = 5);
  assert(b = true);
end;

procedure TestStack(a, b, c, d: Integer);
begin
  assert(a = 1);
  assert(b = 5);
  assert(c = 12);
  assert(d = -57);
end;

procedure TestFloat(a: Integer; b: double);
begin
  assert(a = 1);
  assert(b = 0.5);
end;

procedure TestObject(a: Integer; b: TBoxedString);
begin
  assert(a = 1);
  assert(b.value = 'Boxed String');
end;

procedure TestObjectExc(a: Integer; b: TBoxedString);
begin
  TestObject(a, b);
  Abort;
end;

function TestReturnInt(a, b: Integer): Integer;
begin
  result := a + b;
end;

function TestReturnObject: TBoxedString;
begin
  result := TBoxedString.Create;
  result.value := 'Boxed String';
end;

procedure TestArray(a: Integer; b: TStringDynArray);
begin
  assert(a = 1);
  assert(length(b) = 3);
  assert(b[0] = 'Testing');
  assert(b[1] = 'testing');
  assert(b[2] = '123');
end;

procedure TExternalFunctionTests.RegisterExternalRoutines
  (const manager: IdwsExternalFunctionsManager);
begin
  manager.RegisterExternalFunction('Blank', @Blank, true);
  manager.RegisterExternalFunction('Ints3', @Ints3, true);
  manager.RegisterExternalFunction('TestString', @TestString, true);
  manager.RegisterExternalFunction('TestStringExc', @TestStringExc, true);
  manager.RegisterExternalFunction('TestBool', @TestBool, true);
  manager.RegisterExternalFunction('TestStack', @TestStack, true);
  manager.RegisterExternalFunction('TestFloat', @TestFloat, true);
  manager.RegisterExternalFunction('TestObject', @TestObject, true);
  manager.RegisterExternalFunction('TestObjectExc', @TestObjectExc, true);
  manager.RegisterExternalFunction('TestReturnInt', @TestReturnInt, true);
  manager.RegisterExternalFunction('TestReturnObject', @TestReturnObject, true);
  manager.RegisterExternalFunction('TestArray', @TestArray, true);
end;

procedure ExtractStringArray(const source: IDataContext;
  var output { TStringDynArray } );
var
  i: Integer;
  result: TStringDynArray absolute output;
begin
  SetLength(result, source.DataLength);
  for i := 0 to source.DataLength - 1 do
    result[i] := source.AsString[i];
end;

procedure TExternalFunctionTests.Execution;
var
  source, expectedResult: TStringList;
  i: Integer;
  prog: IdwsProgram;
  exec: IdwsProgramExecution;
  manager: IdwsExternalFunctionsManager;
  resultText, resultsFileName: String;
begin
  source := TStringList.Create;
  expectedResult := TStringList.Create;
  try

    for i := 0 to FTests.Count - 1 do
    begin

      source.LoadFromFile(FTests[i]);

      manager := TExternalFunctionManager.Create;

      // TODO: IdwsExternalFunctionsManager being low-level
      // it shouldn't be exposed at the TDelphiWebScript level
      // (need to have a TComponent property be exposed there)
      FCompiler.Compiler.ExternalFunctionsManager := manager;
      manager.RegisterTypeMapping('TStringDynArray',
        TTypeLookupData.Create(@ExtractStringArray, TypeInfo(TStringDynArray)));

      prog := FCompiler.Compile(source.Text);
      CheckEquals('', prog.Msgs.AsInfo, FTests[i]);

      // TODO: ideally should happen before compilation
      // and registration should be able to be program-independent
      RegisterExternalRoutines(manager);

      exec := prog.Execute;

      // TODO: make compiler program independent from manager
      FCompiler.Compiler.ExternalFunctionsManager := nil;

      resultText := exec.result.ToString;
      if exec.Msgs.Count > 0 then
        resultText := resultText + #13#10'>>>> Error(s): '#13#10 +
          exec.Msgs.AsInfo;

      resultsFileName := ChangeFileExt(FTests[i], '.txt');
      if FileExists(resultsFileName) then
      begin
        expectedResult.LoadFromFile(resultsFileName);
        CheckEquals(expectedResult.Text, resultText, FTests[i]);
      end
      else
        CheckEquals('', resultText, FTests[i]);
      CheckEquals('', exec.Msgs.AsInfo, FTests[i]);

    end;

  finally
    expectedResult.Free;
    source.Free;
  end;
end;

procedure TExternalFunctionTests.SetUp;
var
  tbs: TdwsClass;
  arr: TdwsArray;
begin
  FTests := TStringList.Create;

  CollectFiles(ExtractFilePath(ParamStr(0)) + 'External' + PathDelim,
    '*.pas', FTests);

  FCompiler := TDelphiWebScript.Create(nil);
  FUnit := TdwsUnit.Create(FCompiler);
  FUnit.ParseName := pnAlways;
  FUnit.UnitName := 'Helper';
  FUnit.script := FCompiler;

  tbs := FUnit.Classes.Add;
  tbs.Name := 'TBoxedString';
  tbs.OnCleanUp := self.FreeBoxedString;

  arr := FUnit.Arrays.Add;
  arr.Name := 'TStringDynArray';
  arr.IsDynamic := true;
  arr.DataType := 'string';

  FUnit.ImplicitUse := true;
end;

procedure TExternalFunctionTests.TearDown;
begin
  FCompiler.Free;

  FTests.Free;
end;

procedure TExternalFunctionTests.FreeBoxedString(ExternalObject: TObject);
begin
  ExternalObject.Free;
end;

procedure TExternalFunctionTests.Test;
begin
  Compilation;
  Execution;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterTest('dwsExternalFunctionTests', TExternalFunctionTests);

{$ELSE}

implementation

{$IFEND}

end.
