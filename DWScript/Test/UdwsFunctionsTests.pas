unit UdwsFunctionsTests;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs,
  dwsTokenizer, dwsSymbols, dwsXPlatform, dwsUtils, dwsErrors,
  dwsMathFunctions, dwsTimeFunctions, dwsGlobalVarsFunctions,
  dwsVariantFunctions,
  dwsMathComplexFunctions, dwsMath3DFunctions, dwsCompilerContext,
  dwsByteBufferFunctions, dwsUnitSymbols, dwsJSONConnector;

type

  TdwsFunctionsTestsBase = class(TTestCase)
  private
    FFolder, FFolderPath: String;
    FTests: TStringList;
    FCompiler: TDelphiWebScript;
    FJSON: TdwsJSONLibModule;

  public
    procedure SetUp; override;
    procedure TearDown; override;

    function DoNeedUnit(const unitName: String; var unitSource: String)
      : IdwsUnit;

    procedure Compilation;
    procedure Execution;

  published

    procedure CompilationNormal;
    procedure CompilationWithMapAndSymbols;
    procedure ExecutionNonOptimized;
    procedure ExecutionOptimized;
  end;

  TdwsFuncFunctionsTestsMath = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  TdwsFuncFunctionsTestsMathComplex = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  TdwsFuncFunctionsTestsMath3D = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  TdwsFuncFunctionsTestsTime = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  TdwsFuncFunctionsTestsString = class(TdwsFunctionsTestsBase)
  private
    FLocalizer: TdwsCustomLocalizer;

  protected
    procedure DoOnLocalize(Sender: TObject; const aString: String;
      var result: String);

  public
    procedure SetUp; override;

  published
    procedure LocalizeTest;

  end;

  TdwsFuncFunctionsTestsVariant = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  TdwsFuncFunctionsTestsGlobalVars = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  TdwsFuncFunctionsTestsRTTI = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  TdwsFuncFunctionsTestsDebug = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  TdwsFuncFunctionsTestsByteBuffer = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  TdwsFuncFunctionsTestsFile = class(TdwsFunctionsTestsBase)
  public
    procedure SetUp; override;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsFunctionsTestsBase ------------------
// ------------------

// SetUp
//
procedure TdwsFunctionsTestsBase.SetUp;
begin
  SetDecimalSeparator('.');

  FCompiler := TDelphiWebScript.Create(nil);
  FCompiler.Config.OnNeedUnit := DoNeedUnit;

  FFolderPath := ExtractFilePath(ParamStr(0)) + FFolder + PathDelim;

  FTests := TStringList.Create;
  CollectFiles(FFolderPath, '*.pas', FTests);
end;

// TearDown
//
procedure TdwsFunctionsTestsBase.TearDown;
begin
  FTests.Free;

  FreeAndNil(FJSON);
  FCompiler.Free;
end;

// DoNeedUnit
//
function TdwsFunctionsTestsBase.DoNeedUnit(const unitName: String;
  var unitSource: String): IdwsUnit;
var
  tempPath: String;
begin
  if unitName = 'TestTempPath' then
  begin
    tempPath := FFolderPath + 'Temp' + PathDelim;
    FastStringReplace(tempPath, '"', '""');
    unitSource := 'unit TestTempPath; const TempPath = "' + tempPath + '";';
  end
  else
    unitSource := LoadTextFromFile(FFolderPath + unitName + '.pas');
end;

// Compilation
//
procedure TdwsFunctionsTestsBase.Compilation;
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
      CheckEquals(False, prog.Msgs.HasErrors, FTests[i] + #13#10 +
        prog.Msgs.AsInfo);

    end;

  finally
    source.Free;
  end;
end;

// Execution
//
procedure TdwsFunctionsTestsBase.Execution;
var
  source, expectedResult: TStringList;
  i: Integer;
  prog: IdwsProgram;
  exec: IdwsProgramExecution;
  resultsFileName, output: String;
begin
  source := TStringList.Create;
  expectedResult := TStringList.Create;
  try

    for i := 0 to FTests.Count - 1 do
    begin

      source.LoadFromFile(FTests[i]);

      prog := FCompiler.Compile(source.Text);
      CheckEquals(False, prog.Msgs.HasErrors, FTests[i]);
      exec := prog.Execute;

      if prog.Msgs.Count + exec.Msgs.Count = 0 then
        output := exec.result.ToString
      else
      begin
        output := 'Errors >>>>'#13#10 + prog.Msgs.AsInfo + exec.Msgs.AsInfo +
          'Result >>>>'#13#10 + exec.result.ToString;
      end;

      resultsFileName := ChangeFileExt(FTests[i], '.txt');
      if FileExists(resultsFileName) then
      begin
        expectedResult.LoadFromFile(resultsFileName);
        CheckEquals(expectedResult.Text, output, FTests[i]);
      end
      else
        CheckEquals('', exec.result.ToString, FTests[i]);

    end;

  finally
    expectedResult.Free;
    source.Free;
  end;
end;

// CompilationNormal
//
procedure TdwsFunctionsTestsBase.CompilationNormal;
begin
  FCompiler.Config.CompilerOptions := [coOptimize];
  Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsFunctionsTestsBase.CompilationWithMapAndSymbols;
begin
  FCompiler.Config.CompilerOptions := [coSymbolDictionary, coContextMap,
    coAssertions];
  Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsFunctionsTestsBase.ExecutionNonOptimized;
begin
  FCompiler.Config.CompilerOptions := [coSymbolDictionary, coContextMap,
    coAssertions];
  Execution;
end;

// ExecutionOptimized
//
procedure TdwsFunctionsTestsBase.ExecutionOptimized;
begin
  FCompiler.Config.CompilerOptions := [coOptimize, coAssertions];
  Execution;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsMath ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsMath.SetUp;
begin
  FFolder := 'FunctionsMath';
  inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsMathComplex ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsMathComplex.SetUp;
begin
  FFolder := 'FunctionsMathComplex';
  inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsMath3D ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsMath3D.SetUp;
begin
  FFolder := 'FunctionsMath3D';
  inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsTime ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsTime.SetUp;
begin
  FFolder := 'FunctionsTime';
  inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsString ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsString.SetUp;
begin
  FFolder := 'FunctionsString';
  inherited;

  FLocalizer := TdwsCustomLocalizer.Create(FCompiler);
  FCompiler.Config.Localizer := FLocalizer;
  FLocalizer.OnLocalizeString := DoOnLocalize;
end;

// DoOnLocalize
//
procedure TdwsFuncFunctionsTestsString.DoOnLocalize(Sender: TObject;
  const aString: String; var result: String);
begin
  result := '[' + aString + ']';
end;

// LocalizeTest
//
procedure TdwsFuncFunctionsTestsString.LocalizeTest;
var
  prog: IdwsProgram;
  exec: IdwsProgramExecution;
begin
  prog := FCompiler.Compile('Print(_("Test"));');
  exec := prog.CreateNewExecution;
  exec.Execute;
  CheckEquals('[Test]', exec.result.ToString);
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsVariant ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsVariant.SetUp;
begin
  FFolder := 'FunctionsVariant';
  inherited;
  FJSON := TdwsJSONLibModule.Create(nil);
  FJSON.Script := FCompiler;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsGlobalVars ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsGlobalVars.SetUp;
begin
  FFolder := 'FunctionsGlobalVars';
  inherited;
  FJSON := TdwsJSONLibModule.Create(nil);
  FJSON.Script := FCompiler;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsRTTI ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsRTTI.SetUp;
begin
  FFolder := 'FunctionsRTTI';
  inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsDebug ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsDebug.SetUp;
begin
  FFolder := 'FunctionsDebug';
  inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsByteBuffer ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsByteBuffer.SetUp;
begin
  FFolder := 'FunctionsByteBuffer';
  inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsFile ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsFile.SetUp;
begin
  FFolder := 'FunctionsFile';
  inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterTest('Functions', TdwsFuncFunctionsTestsMath);
RegisterTest('Functions', TdwsFuncFunctionsTestsMathComplex);
RegisterTest('Functions', TdwsFuncFunctionsTestsMath3D);
RegisterTest('Functions', TdwsFuncFunctionsTestsTime);
RegisterTest('Functions', TdwsFuncFunctionsTestsString);
RegisterTest('Functions', TdwsFuncFunctionsTestsVariant);
RegisterTest('Functions', TdwsFuncFunctionsTestsGlobalVars);
RegisterTest('Functions', TdwsFuncFunctionsTestsRTTI);
RegisterTest('Functions', TdwsFuncFunctionsTestsDebug);
RegisterTest('Functions', TdwsFuncFunctionsTestsByteBuffer);
RegisterTest('Functions', TdwsFuncFunctionsTestsFile);

end.
