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
unit dwsCompiler;

{$I dws.inc}

interface

uses
  System.Classes, System.SysUtils, System.TypInfo,
  dwsFileSystem, dwsUtils, dwsArrayMethodKinds,
  dwsExprs, dwsSymbols, dwsTokenizer, dwsTokenTypes, dwsErrors, dwsDataContext,
  dwsExprList, dwsFunctions, dwsStack, dwsConnectorSymbols, dwsFilter,
  dwsCoreExprs, dwsMagicExprs, dwsMethodExprs, dwsConstExprs, dwsConnectorExprs,
  dwsUnifiedConstants, dwsOperators, dwsPascalTokenizer,
  dwsSystemOperators, dwsContextMap, dwsUnitSymbols, dwsScriptSource,
  dwsSymbolDictionary, dwsCompilerContext, dwsGenericSymbols,
  dwsSpecializationContext, dwsSpecialKeywords, dwsArrayExprs;

const
  cDefaultCompilerOptions = [coOptimize, coAssertions];
  cDefaultMaxRecursionDepth = 1024;
  cDefaultMaxExceptionDepth = 10;
  cDefaultStackChunkSize = 4096;
  // 64 kB in 32bit Delphi, each stack entry is a Variant

  // compiler version is date in YYYYMMDD format, dot subversion number
  cCompilerVersion = 20231031.0;

type
  TdwsCompiler = class;

  TIncludeEvent = procedure(const scriptName: String; var scriptSource: String)
    of object;
  TdwsIncludeEventEx = procedure(const scriptName: String;
    var scriptSource, scriptLocation: String) of object;
  TdwsOnNeedUnitEvent = function(const unitName: String; var unitSource: String)
    : IdwsUnit of object;
  TdwsOnNeedUnitEventEx = function(const unitName: String;
    var unitSource, unitLocation: String): IdwsUnit of object;
  TdwsResourceEvent = procedure(compiler: TdwsCompiler;
    const resourceName: String) of object;
  TdwsCodeGenEvent = procedure(compiler: TdwsCompiler;
    const switchPos: TScriptPos; const code: String) of object;
  TdwsFilterEvent = procedure(compiler: TdwsCompiler; const sourceName: String;
    var sourceCode: String; var filter: TdwsFilter) of object;

  TCompilerCreateBaseVariantSymbolEvent = function(table: TSystemSymbolTable)
    : TBaseVariantSymbol of object;
  TCompilerCreateSystemSymbolsEvent = procedure(table: TSystemSymbolTable)
    of object;
  TCompilerRegisterSystemOperatorsEvent = procedure(table: TSystemSymbolTable;
    operators: TOperators) of object;
  TCompilerReadInstrEvent = function(compiler: TdwsCompiler)
    : TNoResultExpr of object;
  TCompilerReadInstrSwitchEvent = function(compiler: TdwsCompiler)
    : Boolean of object;
  TCompilerFindUnknownNameEvent = function(compiler: TdwsCompiler;
    const name: String): TSymbol of object;
  TCompilerReadUnknownNameEvent = function(compiler: TdwsCompiler)
    : TTypedExpr of object;
  TCompilerSectionChangedEvent = procedure(compiler: TdwsCompiler) of object;
  TCompilerReadScriptEvent = procedure(compiler: TdwsCompiler;
    sourceFile: TSourceFile; scriptType: TScriptSourceType) of object;
  TCompilerGetDefaultEnvironmentEvent = function: IdwsEnvironment of object;
  TCompilerGetDefaultLocalizerEvent = function: IdwsLocalizer of object;
  TCompilerOnRootExternalClassEvent = function(compiler: TdwsCompiler;
    const externalName: String): TClassSymbol of object;
  TCompilerApplyConditionalDefines = procedure(defines: TStrings) of object;

  TdwsNameListOption = (nloAllowDots, nloNoCheckSpecials, nloAllowStrings);
  TdwsNameListOptions = set of TdwsNameListOption;

  // TdwsLocalizerComponent
  //
  TdwsLocalizerComponent = class(TComponent)
  public
    function GetLocalizer: IdwsLocalizer; virtual; abstract;
  end;

  ISystemSymbols = interface(ISystemSymbolTable)
    function operators: TSystemOperators;
  end;

  TdwsConfiguration = class(TPersistent)
  private
    FCompilerOptions: TCompilerOptions;
    FHintsLevel: TdwsHintsLevel;
    FConnectors: TStrings;
    FDefaultResultType: TdwsResultType;
    FFilter: TdwsFilter;
    FMaxDataSize: Integer;
    FMaxRecursionDepth: Integer;
    FMaxExceptionDepth: Integer;
    FOnInclude: TIncludeEvent;
    FOnIncludeEx: TdwsIncludeEventEx;
    FOnNeedUnit: TdwsOnNeedUnitEvent;
    FOnNeedUnitEx: TdwsOnNeedUnitEventEx;
    FOnResource: TdwsResourceEvent;
    FOnCodeGen: TdwsCodeGenEvent;
    FOnFilter: TdwsFilterEvent;
    FOnCreateBaseVariantSymbol: TCompilerCreateBaseVariantSymbolEvent;
    FOnCreateSystemSymbols: TCompilerCreateSystemSymbolsEvent;
    FOnRegisterSystemOperators: TCompilerRegisterSystemOperatorsEvent;
    FOnExecutionStarted: TdwsExecutionEvent;
    FOnExecutionEnded: TdwsExecutionEvent;
    FOwner: TComponent;
    FResultType: TdwsResultType;
    FScriptPaths: TStrings;
    FConditionals: TStringList;
    FStackChunkSize: Integer;
    FSystemSymbols: ISystemSymbols;
    FTimeoutMilliseconds: Integer;
    FUnits: TIdwsUnitList;
    FCompileFileSystem: TdwsCustomFileSystem;
    FRuntimeFileSystem: TdwsCustomFileSystem;
    FLocalizer: TdwsLocalizerComponent;

  protected
    procedure InitSystemTable;
    procedure SetResultType(const value: TdwsResultType);
    procedure SetFilter(const value: TdwsFilter);
    procedure SetTimeOut(const val: Integer);
    procedure SetCompileFileSystem(const val: TdwsCustomFileSystem);
    procedure SetRuntimeFileSystem(const val: TdwsCustomFileSystem);
    procedure SetScriptPaths(const values: TStrings);
    procedure SetConditionals(const val: TStringList);
    function GetSystemSymbols: ISystemSymbols;
    procedure SetLocalizer(const val: TdwsLocalizerComponent);

    function DoGetLocalizer: IdwsLocalizer;

    procedure DoIncludeEx(const scriptName: String;
      var scriptSource, scriptLocation: String);
    function DoNeedUnitEx(const unitName: String;
      var unitSource, unitLocation: String): IdwsUnit;

  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);

    procedure DetachSystemTable;

    property Connectors: TStrings read FConnectors write FConnectors;
    property SystemSymbols: ISystemSymbols read GetSystemSymbols;
    property Units: TIdwsUnitList read FUnits;

  published
    property filter: TdwsFilter read FFilter write SetFilter;
    property ResultType: TdwsResultType read FResultType write SetResultType;
    property CompilerOptions: TCompilerOptions read FCompilerOptions
      write FCompilerOptions default cDefaultCompilerOptions;
    property HintsLevel: TdwsHintsLevel read FHintsLevel write FHintsLevel
      default hlStrict;
    property MaxDataSize: Integer read FMaxDataSize write FMaxDataSize
      default 0;
    property MaxRecursionDepth: Integer read FMaxRecursionDepth
      write FMaxRecursionDepth default cDefaultMaxRecursionDepth;
    property MaxExceptionDepth: Integer read FMaxExceptionDepth
      write FMaxExceptionDepth default cDefaultMaxExceptionDepth;
    property Conditionals: TStringList read FConditionals write SetConditionals;
    property ScriptPaths: TStrings read FScriptPaths write SetScriptPaths;
    property CompileFileSystem: TdwsCustomFileSystem read FCompileFileSystem
      write SetCompileFileSystem;
    property RuntimeFileSystem: TdwsCustomFileSystem read FRuntimeFileSystem
      write SetRuntimeFileSystem;
    property Localizer: TdwsLocalizerComponent read FLocalizer
      write SetLocalizer;
    property TimeoutMilliseconds: Integer read FTimeoutMilliseconds
      write FTimeoutMilliseconds default 0;
    property TimeOut: Integer write SetTimeOut;
    property StackChunkSize: Integer read FStackChunkSize write FStackChunkSize
      default cDefaultStackChunkSize;

    property OnInclude: TIncludeEvent read FOnInclude write FOnInclude;
    property OnIncludeEx: TdwsIncludeEventEx read FOnIncludeEx
      write FOnIncludeEx;
    property OnNeedUnit: TdwsOnNeedUnitEvent read FOnNeedUnit write FOnNeedUnit;
    property OnNeedUnitEx: TdwsOnNeedUnitEventEx read FOnNeedUnitEx
      write FOnNeedUnitEx;
    property OnResource: TdwsResourceEvent read FOnResource write FOnResource;
    property OnCodeGen: TdwsCodeGenEvent read FOnCodeGen write FOnCodeGen;
    property OnFilter: TdwsFilterEvent read FOnFilter write FOnFilter;
    property OnExecutionStarted: TdwsExecutionEvent read FOnExecutionStarted
      write FOnExecutionStarted;
    property OnExecutionEnded: TdwsExecutionEvent read FOnExecutionEnded
      write FOnExecutionEnded;
  end;

  TAddArgProcedure = procedure(argExpr: TTypedExpr) of object;
  TExpectedArgFunction = function: TParamSymbol of object;

  TSwitchInstruction = (siNone, siIncludeLong, siIncludeShort, siIncludeOnce,
    siFilterLong, siFilterShort, siResourceLong, siResourceShort, siDefine,
    siUndef, siIfDef, siIfNDef, siIf, siEndIf, siElse, siHint, siHints,
    siWarning, siWarnings, siError, siFatal, siRegion, siEndRegion, siCodeGen);

  TLoopExitable = (leNotExitable, leBreak, leExit);

  TdwsOptimizationMessageList = class(TdwsRuntimeMessageList)
  private
    FCompileMsgs: TdwsCompileMessageList;

  public
    procedure AddMessage(aMessage: TdwsMessage); override;

  end;

  // holds execution context for optimizations during compilation
  TdwsCompilerExecution = class(TdwsExecution)
  private
    FCompiler: TdwsCompiler;
    FOptimMsgs: TdwsOptimizationMessageList;

  protected
    function GetMsgs: TdwsRuntimeMessageList; override;

  public
    constructor Create(const stackParams: TStackParameters;
      compiler: TdwsCompiler);
    destructor Destroy; override;

    function GetCallStack: TdwsExprLocationArray; override;
    function CallStackLastExpr: TExprBase; override;
    function CallStackLastProg: TObject; override;
    function CallStackDepth: Integer; override;
    procedure DebuggerNotifyException(const exceptObj: IScriptObj); override;
  end;

  TdwsReadTypeContext = (tcDeclaration, tcVariable, tcConstant, tcMember,
    tcParameter, tcResult, tcOperand, tcExceptionClass, tcProperty, tcHelper,
    tcGeneric);

  TdwsReadProcDeclOption = (pdoClassMethod, pdoType, pdoAnonymous,
    pdoReferenceTo, pdoHelperMethod);
  TdwsReadProcDeclOptions = set of TdwsReadProcDeclOption;

  TdwsUnitSection = (secMixed, secHeader, secProgram, secInterface,
    secImplementation, secInitialization, secFinalization, secEnd);
  TdwsStatementAction = (saNone, saNoSemiColon, saInterface,
    saImplementation, saEnd);

  TdwsCompilerUnitContext = record
    SourceUnit: TSourceUnit;
    Tokenizer: TTokenizer;
    UnitSymbol: TUnitMainSymbol;
    Context: TdwsSourceContext;
  end;

  IdwsDataSymbolFactory = interface
    procedure CheckName(const name: String; const namePos: TScriptPos);
    function CreateDataSymbol(const name, externalName: String;
      const namePos: TScriptPos; typ: TTypeSymbol): TDataSymbol;
    function CreateConstSymbol(const name: String; const namePos: TScriptPos;
      typ: TTypeSymbol; const data: IDataContext): TConstSymbol;
    function ReadExpr(expecting: TTypeSymbol = nil): TTypedExpr;
    function ReadArrayConstantExpr(closingToken: TTokenType;
      expecting: TTypeSymbol): TArrayConstantExpr;
    function ReadInitExpr(expecting: TTypeSymbol = nil): TTypedExpr;
  end;

  ISymbolAttributesBag = interface
    function Attributes: TdwsSymbolAttributes;
  end;

  TdwsCompilerUnitContextStack = class(TSimpleStack<TdwsCompilerUnitContext>)
  public
    destructor Destroy; override;
    procedure Clean;

    procedure PushContext(compiler: TdwsCompiler);
    procedure PopContext(compiler: TdwsCompiler;
      var oldSourceUnit: TSourceUnit);
  end;

  IdwsCompiler = interface;

  TTypeConvertEvent = procedure(const Source: IDataContext; var output);

  TTypeLookupData = record
    event: TTypeConvertEvent;
    info: PTypeInfo;
    constructor Create(event: TTypeConvertEvent; info: PTypeInfo);
  end;

  IdwsExternalFunctionsManager = interface
    ['{6365B0E4-4BEA-4AD4-8DDE-C37758A63FEF}']
    procedure BeginCompilation(const compiler: IdwsCompiler);
    procedure EndCompilation(const compiler: IdwsCompiler);

    function ConvertToMagicSymbol(value: TFuncSymbol): TFuncSymbol;
    function CreateExternalFunction(funcSymbol: TFuncSymbol): IExternalRoutine;

    procedure RegisterExternalFunction(const name: String; address: pointer;
      ignoreIfMissing: Boolean = False);
    procedure RegisterTypeMapping(const name: String;
      const typ: TTypeLookupData);
  end;

  IdwsCompiler = interface
    // direct access to the underlying instance, use with caution!!!
    function compiler: TdwsCompiler;

    function Compile(const aCodeText: String; aConf: TdwsConfiguration;
      const mainFileName: String = ''): IdwsProgram;
    procedure RecompileInContext(const Context: IdwsProgram;
      const aCodeText: String; aConf: TdwsConfiguration);

    procedure AbortCompilation;

    function GetCurrentProg: TdwsProgram;
    function GetMsgs: TdwsCompileMessageList;
    function GetTokenizer: TTokenizer;
    function CompileTimeExecution: TdwsExecution;
    function GetCompilerContext: TdwsCompilerContext;

    function ActiveProgramCount: Integer;

    procedure SetExternalFunctionsManager(const value
      : IdwsExternalFunctionsManager);
    function GetExternalFunctionsManager: IdwsExternalFunctionsManager;

    property CurrentProg: TdwsProgram read GetCurrentProg;
    property CompilerContext: TdwsCompilerContext read GetCompilerContext;
    property Msgs: TdwsCompileMessageList read GetMsgs;
    property Tokenizer: TTokenizer read GetTokenizer;
    property ExternalFunctionsManager: IdwsExternalFunctionsManager
      read GetExternalFunctionsManager write SetExternalFunctionsManager;

    function ReadExpr(expecting: TTypeSymbol = nil): TTypedExpr;
  end;

  // TdwsCompiler
  //
  TSimpleObjectObjectHash_TDataSymbol_TVarExpr =
    TSimpleObjectObjectHash<TDataSymbol, TVarExpr>;

  TdwsCompiler = class(TInterfacedObject, IdwsCompiler)
  private
    FOptions: TCompilerOptions;
    FTokRules: TTokenizerRules;
    FTok: TTokenizer;
    FMainProg: TdwsMainProgram;
    FCurrentProg: TdwsProgram;
    FCompilerContext: TdwsCompilerContext;
    FUnifiedConstants: TUnifiedConstants;
    FSourceContextMap: TdwsSourceContextMap;
    FSymbolDictionary: TdwsSymbolDictionary;
    FOperators: TOperators;
    FLoopExprs: TSimpleStack<TProgramExpr>;
    FLoopExitable: TSimpleStack<TLoopExitable>;
    FFinallyExprs: TSimpleStack<Boolean>;
    FMsgs: TdwsCompileMessageList;
    FDefaultHintsLevel: TdwsHintsLevel;

    FExec: TdwsCompilerExecution;
    FConnectors: TStrings;
    FCompileFileSystem: IdwsFileSystem;
    FOnInclude: TdwsIncludeEventEx;
    FOnNeedUnit: TdwsOnNeedUnitEventEx;
    FOnResource: TdwsResourceEvent;
    FOnCodeGen: TdwsCodeGenEvent;
    FOnFilter: TdwsFilterEvent;
    FSystemTable: TSystemSymbolTable;
    FScriptPaths: TStrings;
    FFilter: TdwsFilter;
    FIsExcept: Boolean;
    FIsSwitch: Boolean;
    FLineCount: Integer;
    FSourcePostConditionsIndex: Integer;
    FUnitSection: TdwsUnitSection;
    FUnitContextStack: TdwsCompilerUnitContextStack;
    FUnitsFromStack: TSimpleStack<String>;
    FCurrentSourceUnit: TSourceUnit;
    FCurrentUnitSymbol: TUnitMainSymbol;
    FCurrentStructure: TCompositeTypeSymbol;
    FStandardDataSymbolFactory: IdwsDataSymbolFactory;
    FPendingAttributes: TdwsSymbolAttributes;
    FStringListPool: TSimpleStringListPool;
    FDefaultConditionals: IAutoStrings;
    FGenericSymbol: TSimpleStack<TGenericSymbol>;

    // if set we're in a setter write expression or statement
    FPendingSetterValueExpr: TVarExpr;

    FDataSymbolExprReuse: TSimpleObjectObjectHash_TDataSymbol_TVarExpr;

    FExternalRoutinesManager: IdwsExternalFunctionsManager;

    FStaticExtensionSymbols: Boolean;
    FOnCreateBaseVariantSymbol: TCompilerCreateBaseVariantSymbolEvent;
    FOnCreateSystemSymbols: TCompilerCreateSystemSymbolsEvent;
    FOnRegisterSystemOperators: TCompilerRegisterSystemOperatorsEvent;
    FOnReadInstr: TCompilerReadInstrEvent;
    FOnReadInstrSwitch: TCompilerReadInstrSwitchEvent;
    FOnFindUnknownName: TCompilerFindUnknownNameEvent;
    FOnReadUnknownName: TCompilerReadUnknownNameEvent;
    FOnSectionChanged: TCompilerSectionChangedEvent;
    FOnReadScript: TCompilerReadScriptEvent;
    FOnGetDefaultEnvironment: TCompilerGetDefaultEnvironmentEvent;
    FOnGetDefaultLocalizer: TCompilerGetDefaultLocalizerEvent;
    FOnRootExternalClass: TCompilerOnRootExternalClassEvent;
    FOnApplyConditionalDefines: TCompilerApplyConditionalDefines;
    FOnExecutionStarted: TdwsExecutionEvent;
    FOnExecutionEnded: TdwsExecutionEvent;

    FActiveProgramCount: Integer;

    F8087CW: Cardinal;
    FCompilerAbort: Boolean;

  protected
    function Optimize: Boolean;

    function CheckPropertyFuncParams(paramsA: TParamsSymbolTable;
      methSym: TMethodSymbol; indexSym: TSymbol = nil;
      typSym: TTypeSymbol = nil): Boolean;
    procedure CheckName(const name: String; const namePos: TScriptPos);
    procedure CheckUnitName(const name: String; const namePos: TScriptPos;
      const locationName: String);
    procedure CheckSpecialName(const name: String);
    procedure CheckSpecialNameCase(const name: String; sk: TSpecialKeywordKind;
      const namePos: TScriptPos);
    function CheckParams(tableA, tableB: TParamsSymbolTable;
      checkNames: Boolean; skipB: Integer = 0): Boolean;
    procedure CompareFuncKinds(a, b: TFuncKind);
    procedure CompareFuncSymbolParams(a, b: TFuncSymbol);
    function CurrentStruct: TCompositeTypeSymbol;
    function FindStructMember(typ: TStructuredTypeSymbol;
      const name: String): TSymbol;

    procedure HintUnusedSymbols;
    procedure HintUnusedPrivateSymbols;
    procedure HintUnusedResult(resultSymbol: TDataSymbol);
    procedure HintReferenceConstVarParams(funcSym: TFuncSymbol);

    procedure ReportNoMemberForType(const name: String;
      const namePos: TScriptPos; typ: TTypeSymbol;
      fatal: Boolean = True); overload;
    procedure ReportNoMemberForType(const name: String;
      const namePos: TScriptPos; base: TTypedExpr); overload;

    function GetVarExpr(const aScriptPos: TScriptPos; dataSym: TDataSymbol)
      : TVarExpr;

    function GetLazyParamExpr(dataSym: TLazyParamSymbol): TLazyParamExpr;
    function GetVarParamExpr(dataSym: TVarParamSymbol): TByRefParamExpr;
    function GetConstByRefParamExpr(dataSym: TConstByRefParamSymbol)
      : TByRefParamExpr;
    function GetConstParamExpr(const aScriptPos: TScriptPos;
      dataSym: TParamSymbol): TVarExpr;
    function GetSelfParamExpr(const aScriptPos: TScriptPos;
      selfSym: TDataSymbol): TVarExpr;

    function ReadAssign(token: TTokenType; var left: TDataExpr): TProgramExpr;

    function ReadSetOfType(const typeName: String;
      typeContext: TdwsReadTypeContext): TSetOfSymbol;

    function ReadArrayType(const typeName: String;
      typeContext: TdwsReadTypeContext): TTypeSymbol;
    function ReadAssociativeArrayType(const typeName: String;
      keyType: TTypeSymbol; typeContext: TdwsReadTypeContext)
      : TAssociativeArraySymbol;
    function ReadArrayConstant(closingToken: TTokenType; expecting: TTypeSymbol)
      : TArrayConstantExpr;
    function ReadArrayMethod(const name: String; const namePos: TScriptPos;
      baseExpr: TTypedExpr): TProgramExpr;
    function ReadAssociativeArrayMethod(const name: String;
      const namePos: TScriptPos; baseExpr: TTypedExpr): TProgramExpr;
    function ReadStringMethod(const name: String; const namePos: TScriptPos;
      baseExpr: TTypedExpr): TProgramExpr;
    function ReadSetOfMethod(const name: String; const namePos: TScriptPos;
      baseExpr: TTypedExpr): TProgramExpr;
    function ReadElementMethod(const name: String; const namePos: TScriptPos;
      baseExpr: TTypedExpr): TProgramExpr;

    function ReadCase: TCaseExpr;
    function ReadCaseConditions(var condList: TTightList;
      valueExpr: TTypedExpr): Integer;
    function ReadAliasedNameSymbol(var namePos: TScriptPos): TSymbol;
    function ReadNameSymbol(var namePos: TScriptPos): TSymbol;
    function ReadClassName: TClassSymbol;
    function ReadClassOf(const typeName: String): TClassOfSymbol;
    function ReadClassDecl(const typeName: String; flags: TClassSymbolFlags;
      allowNonConstExpressions: Boolean): TClassSymbol;
    procedure ReadClassVars(const ownerSymbol: TCompositeTypeSymbol;
      aVisibility: TdwsVisibility);
    procedure ReadClassConst(const ownerSymbol: TCompositeTypeSymbol;
      aVisibility: TdwsVisibility);
    function ReadClassConstSymbol(const constPos: TScriptPos; typ: TTypeSymbol;
      const ownerSymbol: TCompositeTypeSymbol; aVisibility: TdwsVisibility)
      : TConstSymbol;
    function ReadInterface(const typeName: String): TInterfaceSymbol;
    function ReadConnectorSym(const name: String; baseExpr: TTypedExpr;
      const connectorType: IConnectorType; isWrite: Boolean): TProgramExpr;
    function ReadConnectorArray(const name: String; var baseExpr: TTypedExpr;
      const connectorType: IConnectorType; isWrite: Boolean)
      : TConnectorCallExpr;

    function ReadConstSymbol(const name: String; const constPos: TScriptPos;
      typ: TTypeSymbol; const factory: IdwsDataSymbolFactory): TConstSymbol;
    procedure ReadConstDecl(const factory: IdwsDataSymbolFactory);
    procedure ReadConstDeclBlock(var action: TdwsStatementAction);
    function ReadConstImmediateValue: TConstExpr;
    function ReadConstRecord(symbol: TRecordSymbol): IDataContext;

    function ReadBlock: TProgramExpr;
    function ReadBlocks(const endTokens: TTokenTypes;
      var finalToken: TTokenType): TProgramExpr;
    function ReadRootStatement(var action: TdwsStatementAction;
      initVarBlockExpr: TBlockExpr): TProgramExpr;
    function ReadRootBlock(const endTokens: TTokenTypes;
      var finalToken: TTokenType): TBlockExpr;
    procedure UnexpectedBlockTokenError(const endTokens: TTokenTypes);

    function ReadEnumeration(const typeName: String;
      aStyle: TEnumerationSymbolStyle): TEnumerationSymbol;
    function ReadExit: TNoResultExpr;
    function ReadClassExpr(ownerSymbol: TCompositeTypeSymbol;
      expecting: TTypeSymbol = nil): TTypedExpr;
    function ReadExpr(expecting: TTypeSymbol = nil): TTypedExpr;
    function ReadExprAdd(expecting: TTypeSymbol = nil;
      leftExpr: TTypedExpr = nil): TTypedExpr;
    function ReadExprMult(expecting: TTypeSymbol = nil;
      leftExpr: TTypedExpr = nil): TTypedExpr;
    function ReadBooleanExpr: TTypedExpr;
    function ReadExprIn(var left: TTypedExpr): TTypedExpr;
    function ReadExprInConditions(var left: TTypedExpr): TInOpExpr;
    function ReadExternalVar(sym: TExternalVarSymbol; isWrite: Boolean)
      : TFuncExpr;

    function ReadField(const scriptPos: TScriptPos; selfSym: TDataSymbol;
      fieldSym: TFieldSymbol): TDataExpr; overload;
    function ReadField(const scriptPos: TScriptPos; selfSym: TDataSymbol;
      fieldSym: TFieldSymbol; var varExpr: TTypedExpr): TDataExpr; overload;

    function ReadFor: TProgramExpr;
    function ReadForTo(const forPos: TScriptPos; loopVarExpr: TVarExpr;
      const loopVarName: String; const loopVarNamePos: TScriptPos)
      : TNoResultExpr;
    function ReadForStep(const forPos: TScriptPos; forExprClass: TForExprClass;
      iterVarExpr: TIntVarExpr; var fromExpr, toExpr: TTypedExpr;
      loopFirstStatement: TProgramExpr): TForExpr;
    function ReadForIn(const forPos: TScriptPos; loopVarExpr: TVarExpr;
      const loopVarName: String; const loopVarNamePos: TScriptPos)
      : TProgramExpr;
    function ReadForInString(const forPos: TScriptPos; inExpr: TProgramExpr;
      loopVarExpr: TVarExpr; const loopVarName: String;
      const loopVarNamePos: TScriptPos): TProgramExpr;
    function ReadForInSetOf(const forPos: TScriptPos; inExpr: TDataExpr;
      loopVarExpr: TVarExpr; const loopVarName: String;
      const loopVarNamePos: TScriptPos): TProgramExpr;
    function ReadForInConnector(const forPos: TScriptPos; inExpr: TTypedExpr;
      const inPos: TScriptPos; loopVarExpr: TVarExpr; const loopVarName: String;
      const loopVarNamePos: TScriptPos): TProgramExpr;

    function ReadFuncOverloaded(funcSym: TFuncSymbol; fromTable: TSymbolTable;
      codeExpr: TDataExpr = nil; expecting: TTypeSymbol = nil): TTypedExpr;
    procedure CollectMethodOverloads(methSym: TMethodSymbol;
      overloads: TFuncSymbolList);
    function ReadSelfMethOverloaded(methSym: TMethodSymbol; isWrite: Boolean;
      expecting: TTypeSymbol; options: TCreateFunctionOptions): TTypedExpr;
    function ReadMethOverloaded(methSym: TMethodSymbol;
      instanceExpr: TTypedExpr; const scriptPos: TScriptPos;
      expecting: TTypeSymbol = nil): TTypedExpr;
    function ReadStaticMethOverloaded(methSym: TMethodSymbol;
      metaExpr: TTypedExpr; const scriptPos: TScriptPos;
      expecting: TTypeSymbol = nil): TTypedExpr;

    function ResolveOverload(var funcExpr: TFuncExprBase;
      overloads: TFuncSymbolList; const argPosArray: TScriptPosArray;
      expecting: TFuncSymbol; cfOptions: TCreateFunctionOptions): Boolean;

    function FuncHasConflictingOverload(funcSym, forwardedSym
      : TFuncSymbol): Boolean;
    function CheckMethodOverloads(newMethSym: TMethodSymbol): Boolean;

    function FuncPerfectMatchOverload(funcSym: TFuncSymbol): TFuncSymbol;
    function MethPerfectMatchOverload(methSym: TMethodSymbol; recurse: Boolean)
      : TMethodSymbol;

    function ReadFunc(funcSym: TFuncSymbol; codeExpr: TTypedExpr = nil;
      expecting: TTypeSymbol = nil; overloads: TFuncSymbolList = nil)
      : TTypedExpr;
    function ReadSelfMethod(methodSym: TMethodSymbol; isWrite: Boolean;
      expecting: TTypeSymbol; overloads: TFuncSymbolList;
      options: TCreateFunctionOptions): TTypedExpr;
    function ReadMethod(methodSym: TMethodSymbol; instanceExpr: TTypedExpr;
      const scriptPos: TScriptPos; expecting: TTypeSymbol = nil;
      overloads: TFuncSymbolList = nil): TTypedExpr;
    function ReadStaticMethod(methodSym: TMethodSymbol; metaExpr: TTypedExpr;
      const scriptPos: TScriptPos; expecting: TTypeSymbol = nil;
      overloads: TFuncSymbolList = nil): TTypedExpr;

    function WrapUpFunctionRead(funcExpr: TFuncExprBase; expecting: TTypeSymbol;
      overloads: TFuncSymbolList; cfOptions: TCreateFunctionOptions)
      : TTypedExpr;

    procedure ReadFuncArgs(funcExpr: TFuncExprBase;
      var argPosArray: TScriptPosArray; overloads: TFuncSymbolList);
    procedure ReadArguments(const addArgProc: TAddArgProcedure;
      leftDelim, rightDelim: TTokenType; var argPosArray: TScriptPosArray;
      const expectedProc: TExpectedArgFunction = nil);
    function ReadFuncResultType(funcKind: TFuncKind): TTypeSymbol;

    function ReadIf: TProgramExpr;
    function ReadInherited(isWrite: Boolean): TProgramExpr;
    function ReadInstr: TProgramExpr;
    function ReadUntilEndOrElseSwitch(allowElse: Boolean): Boolean;
    function ReadIntfMethodDecl(intfSym: TInterfaceSymbol; funcKind: TFuncKind)
      : TSourceMethodSymbol;
    function ReadMethodDecl(const hotPos: TScriptPos;
      ownerSym: TCompositeTypeSymbol; funcKind: TFuncKind;
      aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
      : TMethodSymbol;
    function ReadMethodImpl(ownerSym: TCompositeTypeSymbol; funcKind: TFuncKind;
      aCreateOptions: TMethodCreateOptions): TMethodSymbol;

    function ReadDeprecatedMessage(withSemiColon: Boolean = True): String;
    procedure WarnDeprecatedFunc(funcExpr: TFuncExprBase);
    procedure WarnDeprecatedType(const scriptPos: TScriptPos;
      typeSymbol: TTypeSymbol);
    procedure WarnDeprecatedSymbol(const scriptPos: TScriptPos; sym: TSymbol;
      const deprecatedMessage: String);

    function ResolveUnitNameSpace(const prefixPos: TScriptPos;
      unitPrefix: TUnitSymbol): TUnitSymbol;
    function ReadName(isWrite: Boolean = False; expecting: TTypeSymbol = nil)
      : TProgramExpr;
    function ReadEnumerationSymbolName(const enumPos: TScriptPos;
      enumSym: TEnumerationSymbol; acceptTypeRef: Boolean): TProgramExpr;
    function ReadClassSymbolName(baseType: TClassSymbol; isWrite: Boolean;
      expecting: TTypeSymbol): TProgramExpr;
    function ReadInterfaceSymbolName(baseType: TInterfaceSymbol;
      isWrite: Boolean; expecting: TTypeSymbol): TProgramExpr;
    function ReadRecordSymbolName(baseType: TRecordSymbol; isWrite: Boolean;
      expecting: TTypeSymbol): TProgramExpr;
    function ReadConstName(const constPos: TScriptPos; constSym: TConstSymbol;
      isWrite: Boolean): TProgramExpr;
    function ReadDataSymbolName(const aScriptPos: TScriptPos;
      dataSym: TDataSymbol; fromTable: TSymbolTable; isWrite: Boolean;
      expecting: TTypeSymbol): TProgramExpr;
    function ReadImplicitCall(codeExpr: TTypedExpr; isWrite: Boolean;
      expecting: TTypeSymbol): TProgramExpr;
    function ReadResourceStringName(resSym: TResourceStringSymbol;
      const namePos: TScriptPos): TResourceStringExpr;
    function ReadNameOld(isWrite: Boolean): TTypedExpr;
    function ReadNameInherited(isWrite: Boolean): TProgramExpr;
    procedure ReadNameList(names: TSimpleStringList;
      var posArray: TScriptPosArray; const options: TdwsNameListOptions = [];
      externalNames: TSimpleStringList = nil);
    procedure ReadExternalName(funcSym: TFuncSymbol);
    function ReadNew(restrictTo: TClassSymbol; asAttribute: Boolean)
      : TProgramExpr;
    function ReadNewArray(elementTyp: TTypeSymbol): TNewArrayExpr;
    function ReadNewOperator(const aScriptPos: TScriptPos;
      opSym: TOperatorSymbol; elementTyp: TTypeSymbol): TTypedExpr;
    procedure ReadArrayParams(ArrayIndices: TSymbolTable);
    // Don't want to add param symbols to dictionary when a method implementation (they get thrown away)
    procedure ReadParams(const hasParamMeth: THasParamSymbolMethod;
      const addParamMeth: TAddParamSymbolMethod;
      forwardedParams: TParamsSymbolTable;
      expectedLambdaParams: TParamsSymbolTable; var posArray: TScriptPosArray);
    procedure ReadProcCallQualifiers(funcSymbol: TFuncSymbol);
    procedure AdaptParametersSymPos(guess, actual: TFuncSymbol;
      const useTypes: TSymbolUsages; var posArray: TScriptPosArray);
    function ReadProcDeclAsync(const hotPos: TScriptPos): TFuncSymbol;
    procedure ReadProcDeclProperty(funcSymbol: TFuncSymbol;
      const hotPos: TScriptPos);
    function ReadProcDecl(funcToken: TTokenType; const hotPos: TScriptPos;
      declOptions: TdwsReadProcDeclOptions = [];
      expectedLambdaParams: TParamsSymbolTable = nil): TFuncSymbol;
    procedure ReadProcBody(funcSymbol: TFuncSymbol);
    procedure ReadProcEmpty(funcSymbol: TFuncSymbol);
    procedure ReadConditions(funcSymbol: TFuncSymbol;
      conditions: TSourceConditions; condsSymClass: TConditionSymbolClass);
    procedure ReadPostConditions(funcSymbol: TFuncSymbol;
      conditions: TSourcePostConditions; condsSymClass: TConditionSymbolClass);
    function ReadOperatorDecl: TOperatorSymbol;
    function ReadClassOperatorDecl(ClassSym: TClassSymbol)
      : TClassOperatorSymbol;
    procedure ReadPropertyDecl(ownerSym: TCompositeTypeSymbol;
      aVisibility: TdwsVisibility; classProperty: Boolean);
    procedure ReadPropertyDeclAutoField(propSym: TPropertySymbol;
      classProperty: Boolean);
    function ReadPropertyExternalField(propSym: TPropertySymbol): TFieldSymbol;
    function ReadPropertyDeclGetter(propSym: TPropertySymbol;
      var scriptPos: TScriptPos; classProperty: Boolean): TSymbol;
    function ReadPropertyDeclSetter(propSym: TPropertySymbol;
      var scriptPos: TScriptPos; classProperty: Boolean): TSymbol;
    function ReadPropertyProgExpr(var expr: TProgramExpr;
      propertySym: TPropertySymbol; isWrite: Boolean): TProgramExpr;
    function ReadPropertyExpr(var expr: TTypedExpr;
      propertySym: TPropertySymbol; isWrite: Boolean): TProgramExpr;
    function ReadPropertyReadExpr(var expr: TTypedExpr;
      propertySym: TPropertySymbol): TTypedExpr;
    function ReadPropertyWriteExpr(var expr: TTypedExpr;
      propertySym: TPropertySymbol): TProgramExpr;
    function ReadPropertyArrayAccessor(var expr: TTypedExpr;
      propertySym: TPropertySymbol; typedExprList: TTypedExprList;
      const scriptPos: TScriptPos; isWrite: Boolean): TFuncExprBase;

    function ReadRecordDecl(const typeName: String;
      allowNonConstExpressions: Boolean): TRecordSymbol;
    procedure ReadFieldsDecl(struct: TStructuredTypeSymbol;
      visibility: TdwsVisibility; allowNonConstExpressions: Boolean;
      forceExternal: Boolean);

    function ReadHelperDecl(const typeName: String; qualifierToken: TTokenType;
      isStrict: Boolean): THelperSymbol;

    function ReadRaise: TRaiseBaseExpr;
    function ReadRepeat: TProgramExpr;
    function ReadImplementationBlock: TTokenType;
    procedure ReadSemiColon(fatal: Boolean = False);
    function ReadScript(sourceFile: TSourceFile; scriptType: TScriptSourceType)
      : TProgramExpr;
    procedure ReadScriptImplementations;
    function ReadSpecialFunction(const namePos: TScriptPos;
      specialKind: TSpecialKeywordKind): TProgramExpr;
    function ReadIncludeExclude(const namePos: TScriptPos;
      methodKind: TArrayMethodKind; var argExpr: TTypedExpr;
      const argPos: TScriptPos): TProgramExpr;

    function ReadStatement(var action: TdwsStatementAction;
      initVarBlockExpr: TBlockExpr): TProgramExpr;
    function ReadResourceStringDecl: TResourceStringSymbol;
    procedure ReadResourceStringDeclBlock(var action: TdwsStatementAction);
    function ReadStringArray(expr: TDataExpr; isWrite: Boolean): TProgramExpr;

    function ReadSwitch(const switchName: String): Boolean;
    function ReadInstrSwitch(const switchName: String): Boolean;
    function ReadExprSwitch(const switchPos: TScriptPos): Boolean;
    procedure SkipUntilToken(tt: TTokenType);

    function ReadSymbol(expr: TProgramExpr; isWrite: Boolean = False;
      expecting: TTypeSymbol = nil): TProgramExpr;
    function ReadSymbolArrayExpr(var baseExpr: TTypedExpr): TProgramExpr;
    function ReadSymbolAssociativeArrayExpr(var baseExpr: TDataExpr)
      : TProgramExpr;
    function ReadSymbolMemberExpr(var expr: TProgramExpr; isWrite: Boolean;
      expecting: TTypeSymbol): TProgramExpr;

    function ReadTerm(isWrite: Boolean = False; expecting: TTypeSymbol = nil)
      : TTypedExpr;
    function ReadBracket(expecting: TTypeSymbol = nil): TTypedExpr;
    function ReadNegation: TTypedExpr;
    function ReadIfExpr(expecting: TTypeSymbol = nil): TTypedExpr;

    function ReadTry: TExceptionExpr;
    procedure ReadFinally(finallyExpr: TFinallyExpr);
    procedure ReadExcept(exceptExpr: TExceptExpr; var finalToken: TTokenType);

    function ReadGenericParametersDecl: IGenericParameters;
    function ReadSpecializedType(genericType: TGenericSymbol): TTypeSymbol;
    procedure CheckGenericParameters(genericType: TGenericSymbol);

    function ReadType(const typeName: String; typeContext: TdwsReadTypeContext)
      : TTypeSymbol;
    function ReadTypeGenericDecl(const typeName: String;
      typeContext: TdwsReadTypeContext;
      const genericParameters: IGenericParameters = nil): TTypeSymbol;
    function ReadTypeCast(const namePos: TScriptPos; typeSym: TTypeSymbol)
      : TTypedExpr;
    function ReadTypeExpr(const namePos: TScriptPos; typeSym: TTypeSymbol;
      isWrite: Boolean; expecting: TTypeSymbol = nil): TProgramExpr;

    procedure EnterGeneric(generic: TGenericSymbol);
    procedure LeaveGeneric;

    procedure ReadAttributes(tokenALEFTAlreadyDeleted: Boolean);
    function BagPendingAttributes: ISymbolAttributesBag;
    procedure AttachBaggedAttributes(symbol: TSymbol;
      const bag: ISymbolAttributesBag);
    procedure CheckNoPendingAttributes;

    procedure AddProcHelper(func: TFuncSymbol);
    function EnumerateHelpers(typeSym: TTypeSymbol): THelperSymbols;
    function ReadTypeHelper(expr: TTypedExpr; const name: String;
      const namePos: TScriptPos; expecting: TTypeSymbol; isWrite: Boolean;
      killNameToken: Boolean): TProgramExpr;
    function ReadSelfTypeHelper(const name: TToken; const namePos: TScriptPos;
      expecting: TTypeSymbol): TProgramExpr;

    procedure ReadTypeDeclBlock;
    function ReadTypeDecl(firstInBlock: Boolean): Boolean;

    procedure ReadUses;
    function ReadUnitHeader: TScriptSourceType;

    procedure ReadVarDeclBlock(var action: TdwsStatementAction;
      initVarBlockExpr: TBlockExprBase);
    procedure ReadVarDecl(const dataSymbolFactory: IdwsDataSymbolFactory;
      initVarBlockExpr: TBlockExprBase);
    procedure ReadNamedVarsDecl(names, externalNames: TSimpleStringList;
      const posArray: TScriptPosArray;
      const dataSymbolFactory: IdwsDataSymbolFactory;
      initVarBlockExpr: TBlockExprBase);
    function CreateNamedVarDeclExpr(const dataSymbolFactory
      : IdwsDataSymbolFactory; const name, externalName: String;
      const scriptPos: TScriptPos; typ: TTypeSymbol; var initExpr: TTypedExpr;
      var sym: TDataSymbol): TProgramExpr;
    function ReadWhile: TProgramExpr;
    function ReadWith: TProgramExpr;
    function ResolveUnitReferences(scriptType: TScriptSourceType)
      : TIdwsUnitList;

    function CreateTypedDefault(typ: TTypeSymbol): TConstExpr;

    procedure SetCurrentProg(aProg: TdwsProgram);

    function GetCompilerContext: TdwsCompilerContext;

    function GetFieldReadOnlyState(fieldSym: TFieldSymbol)
      : TFieldExprReadOnlyState;

    procedure DoProgramDestroyed(Sender: TObject);

  protected
    procedure EnterLoop(loopExpr: TProgramExpr);
    procedure MarkLoopExitable(level: TLoopExitable);
    procedure LeaveLoop;

    function GetFuncExpr(funcSym: TFuncSymbol; codeExpr: TTypedExpr = nil)
      : TFuncExprBase;
    function GetMethodExpr(meth: TMethodSymbol; expr: TTypedExpr;
      RefKind: TRefKind; const scriptPos: TScriptPos;
      options: TCreateFunctionOptions): TFuncExprBase;

    procedure MemberSymbolWithNameAlreadyExists(sym: TSymbol;
      const hotPos: TScriptPos);
    procedure IncompatibleTypes(const scriptPos: TScriptPos; const fmt: String;
      typ1, typ2: TTypeSymbol); overload;
    procedure IncompatibleTypes(const scriptPos: TScriptPos; const fmt: String;
      typ1: TTypeSymbol; expr2: TTypedExpr); overload;
    procedure IncompatibleTypesWarn(const scriptPos: TScriptPos;
      const fmt: String; typ1, typ2: TTypeSymbol);
    procedure ReportImplicitMethodOverload(const scriptPos: TScriptPos;
      const name: String; const aLevel: TdwsHintsLevel = hlNormal);

    function CreateProgram(const systemTable: ISystemSymbolTable;
      ResultType: TdwsResultType; const stackParams: TStackParameters;
      const mainFileName: String): TdwsMainProgram;
    function CreateAssign(const scriptPos: TScriptPos; token: TTokenType;
      left: TDataExpr; right: TTypedExpr): TProgramExpr;

    function CreateArrayLow(const aScriptPos: TScriptPos;
      baseExpr: TProgramExpr; typ: TArraySymbol; captureBase: Boolean)
      : TTypedExpr;
    function CreateArrayHigh(const aScriptPos: TScriptPos;
      baseExpr: TProgramExpr; typ: TArraySymbol; captureBase: Boolean)
      : TTypedExpr;
    function CreateArrayLength(const aScriptPos: TScriptPos;
      baseExpr: TTypedExpr; typ: TArraySymbol): TTypedExpr;
    function CreateArrayExpr(const scriptPos: TScriptPos; baseExpr: TDataExpr;
      indexExpr: TTypedExpr): TArrayExpr;
    function CreateDynamicArrayExpr(const scriptPos: TScriptPos;
      baseExpr: TTypedExpr; indexExpr: TTypedExpr): TDynamicArrayExpr;

    function EnsureLoopVarExpr(const loopPos: TScriptPos;
      const loopVarName: String; const loopVarNamePos: TScriptPos;
      var loopVarExpr: TVarExpr; loopVarTyp: TTypeSymbol): TBlockExpr;

    function CreateTypedOperatorExpr(token: TTokenType;
      const scriptPos: TScriptPos; aLeft, aRight: TTypedExpr): TTypedExpr;
    function CreateAssignOperatorExpr(token: TTokenType;
      const scriptPos: TScriptPos; exec: TdwsExecution; aLeft: TDataExpr;
      aRight: TTypedExpr): TProgramExpr;
    function CreateSetOperatorExpr(token: TTokenType;
      const scriptPos: TScriptPos; aLeft, aRight: TTypedExpr): TTypedExpr;

    procedure DoSectionChanged;
    procedure DoTokenizerEndSourceFile(sourceFile: TSourceFile);

    property CurrentSourceUnit: TSourceUnit read FCurrentSourceUnit;
    property CurrentUnitSymbol: TUnitMainSymbol read FCurrentUnitSymbol;
    procedure EnterUnit(srcUnit: TSourceUnit; var oldSrcUnit: TSourceUnit);
    procedure LeaveUnit(oldSrcUnit: TSourceUnit);
    procedure SwitchTokenizerToUnit(srcUnit: TSourceUnit;
      const sourceCode, sourceLocation: String);

    procedure SetupCompileOptions(conf: TdwsConfiguration);
    procedure SetupMsgsOptions(conf: TdwsConfiguration);
    procedure CleanupAfterCompile;

    procedure CheckFilterDependencies(confUnits: TIdwsUnitList);
    procedure HandleUnitDependencies(scriptType: TScriptSourceType);
    function HandleExplicitDependency(const scriptPos: TScriptPos;
      const unitName: String): TUnitSymbol;

    procedure SetupInitializationFinalization;

    procedure OrphanObject(obj: TRefCountedObject); overload;

    procedure OrphanAndNil(var expr: TTypedExpr); overload;
    procedure OrphanAndNil(var expr: TProgramExpr); overload;
    procedure OrphanAndNil(var expr: TDataExpr); overload;
    procedure OrphanAndNil(var expr: TBlockExpr); overload;
    procedure OrphanAndNil(var expr: TVarExpr); overload;

    function compiler: TdwsCompiler;
    function GetCurrentProg: TdwsProgram;
    function GetMsgs: TdwsCompileMessageList;
    function GetTokenizer: TTokenizer;
    function GetExternalFunctionsManager: IdwsExternalFunctionsManager;
    procedure SetExternalFunctionsManager(const value
      : IdwsExternalFunctionsManager);
    function CompileTimeExecution: TdwsExecution;

    procedure AttachContextProgram(contextProgram: TdwsProgram);
    procedure DetachContextProgram;
    procedure AttachTokenizer(tok: TTokenizer);
    function DetachTokenizer: TTokenizer;
    property SourceContextMap: TdwsSourceContextMap read FSourceContextMap;

  public
    constructor Create;
    destructor Destroy; override;

    function Compile(const aCodeText: String; aConf: TdwsConfiguration;
      const mainFileName: String = ''): IdwsProgram;
    procedure RecompileInContext(const Context: IdwsProgram;
      const aCodeText: String; aConf: TdwsConfiguration);

    class procedure Evaluate; static;
      deprecated 'Moved to TdwsEvaluateExpr.Evaluate';

    procedure AbortCompilation;

    function ActiveProgramCount: Integer;

    procedure WarnForVarUsage(varExpr: TVarExpr; const scriptPos: TScriptPos);

    procedure CheckMatchingDeclarationCase(const nameString: String;
      sym: TSymbol; const scriptPos: TScriptPos);

    procedure RecordSymbolUse(sym: TSymbol; const scriptPos: TScriptPos;
      const useTypes: TSymbolUsages);
    procedure RecordSymbolUseReference(sym: TSymbol;
      const scriptPos: TScriptPos; isWrite: Boolean);
    procedure RecordSymbolUseImplicitReference(sym: TSymbol;
      const scriptPos: TScriptPos; isWrite: Boolean);
    procedure ReplaceSymbolUse(oldSym, newSym: TSymbol;
      const scriptPos: TScriptPos);

    function OpenStreamForFile(const fileName: String): TStream;
    function GetScriptSource(const scriptName: String): String;
    function GetIncludeScriptSource(const scriptName: String;
      var scriptLocation: String): String;

    property CurrentProg: TdwsProgram read FCurrentProg write SetCurrentProg;
    property CompilerContext: TdwsCompilerContext read FCompilerContext;

    property Msgs: TdwsCompileMessageList read FMsgs;
    property options: TCompilerOptions read FOptions write FOptions;
    property ScriptPaths: TStrings read FScriptPaths;
    property filter: TdwsFilter read FFilter;
    property SymbolDictionary: TdwsSymbolDictionary read FSymbolDictionary;
    property UnitSection: TdwsUnitSection read FUnitSection write FUnitSection;
    property TokenizerRules: TTokenizerRules read FTokRules;
    property Tokenizer: TTokenizer read FTok write FTok;

    property StaticExtensionSymbols: Boolean read FStaticExtensionSymbols
      write FStaticExtensionSymbols;
    property OnCreateBaseVariantSymbol: TCompilerCreateBaseVariantSymbolEvent
      read FOnCreateBaseVariantSymbol write FOnCreateBaseVariantSymbol;
    property OnCreateSystemSymbols: TCompilerCreateSystemSymbolsEvent
      read FOnCreateSystemSymbols write FOnCreateSystemSymbols;
    property OnRegisterSystemOperators: TCompilerRegisterSystemOperatorsEvent
      read FOnRegisterSystemOperators write FOnRegisterSystemOperators;
    property OnReadInstr: TCompilerReadInstrEvent read FOnReadInstr
      write FOnReadInstr;
    property OnReadInstrSwitch: TCompilerReadInstrSwitchEvent
      read FOnReadInstrSwitch write FOnReadInstrSwitch;
    property OnFindUnknownName: TCompilerFindUnknownNameEvent
      read FOnFindUnknownName write FOnFindUnknownName;
    property OnReadUnknownName: TCompilerReadUnknownNameEvent
      read FOnReadUnknownName write FOnReadUnknownName;
    property OnSectionChanged: TCompilerSectionChangedEvent
      read FOnSectionChanged write FOnSectionChanged;
    property OnReadScript: TCompilerReadScriptEvent read FOnReadScript
      write FOnReadScript;
    property OnGetDefaultEnvironment: TCompilerGetDefaultEnvironmentEvent
      read FOnGetDefaultEnvironment write FOnGetDefaultEnvironment;
    property OnGetDefaultLocalizer: TCompilerGetDefaultLocalizerEvent
      read FOnGetDefaultLocalizer write FOnGetDefaultLocalizer;
    property OnRootExternalClass: TCompilerOnRootExternalClassEvent
      read FOnRootExternalClass write FOnRootExternalClass;
    property OnApplyConditionalDefines: TCompilerApplyConditionalDefines
      read FOnApplyConditionalDefines write FOnApplyConditionalDefines;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  System.Variants, System.Math,
  dwsArrayIndexOfExprs, dwsStrings, dwsSetOfExprs, dwsConvExprs, dwsRelExprs,
  dwsGenericExprs, dwsCompilerUtils, dwsXPlatform;

const
  cSwitchInstructions: array [TSwitchInstruction] of String = ('',
    SWI_INCLUDE_LONG, SWI_INCLUDE_SHORT, SWI_INCLUDE_ONCE, SWI_FILTER_LONG,
    SWI_FILTER_SHORT, SWI_RESOURCE_LONG, SWI_RESOURCE_SHORT, SWI_DEFINE,
    SWI_UNDEF, SWI_IFDEF, SWI_IFNDEF, SWI_IF, SWI_ENDIF, SWI_ELSE, SWI_HINT,
    SWI_HINTS, SWI_WARNING, SWI_WARNINGS, SWI_ERROR, SWI_FATAL, SWI_REGION,
    SWI_ENDREGION, SWI_CODEGEN);

  cAssignmentTokens: TTokenTypes = [ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN,
    ttTIMES_ASSIGN, ttDIVIDE_ASSIGN];

  cTokenToVisibility: array [ttPRIVATE .. ttPUBLISHED] of TdwsVisibility =
    (cvPrivate, cvProtected, cvPublic, cvPublished);
  cTokenToFuncKind: array [ttFUNCTION .. ttLAMBDA] of TFuncKind = (fkFunction,
    fkProcedure, fkConstructor, fkDestructor, fkMethod, fkLambda);

  cMaxArrayItemRange = 1024;

type
  TReachStatus = (rsReachable, rsUnReachable, rsUnReachableWarned);

  TExceptObjFunc = class(TInternalMagicVariantFunction)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var result: Variant); override;
  end;

  TParamFunc = class(TInternalMagicVariantFunction)
    procedure DoEvalAsVariant(const args: TExprBaseListExec;
      var result: Variant); override;
  end;

  TParamStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(const args: TExprBaseListExec;
      var result: String); override;
  end;

  TParamCountFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(const args: TExprBaseListExec): Int64; override;
  end;

  TStandardSymbolFactory = class(TInterfacedObject, IdwsDataSymbolFactory)
  private
    FCompiler: TdwsCompiler;

  public
    constructor Create(aCompiler: TdwsCompiler);
    procedure CheckName(const name: String; const namePos: TScriptPos); virtual;
    function CreateDataSymbol(const name, externalName: String;
      const namePos: TScriptPos; typ: TTypeSymbol): TDataSymbol; virtual;
    function CreateConstSymbol(const name: String; const namePos: TScriptPos;
      typ: TTypeSymbol; const data: IDataContext): TConstSymbol; virtual;
    function ReadExpr(expecting: TTypeSymbol = nil): TTypedExpr; virtual;
    function ReadArrayConstantExpr(closingToken: TTokenType;
      expecting: TTypeSymbol): TArrayConstantExpr;
    function ReadInitExpr(expecting: TTypeSymbol = nil): TTypedExpr;
  end;

  TCompositeTypeSymbolFactory = class(TStandardSymbolFactory)
  private
    FOwnerType: TCompositeTypeSymbol;
    FVisibility: TdwsVisibility;

  public
    constructor Create(aCompiler: TdwsCompiler; ownerType: TCompositeTypeSymbol;
      aVisibility: TdwsVisibility);
    procedure CheckName(const name: String; const namePos: TScriptPos);
      override;
    function CreateDataSymbol(const name, externalName: String;
      const namePos: TScriptPos; typ: TTypeSymbol): TDataSymbol; override;
    function CreateConstSymbol(const name: String; const namePos: TScriptPos;
      typ: TTypeSymbol; const data: IDataContext): TConstSymbol; override;
    function ReadExpr(expecting: TTypeSymbol = nil): TTypedExpr; override;
  end;

  // const expr created to "keep compiling"
  TBogusConstExpr = class sealed(TConstExpr);

    TSymbolAttributesBag = class(TInterfacedSelfObject, ISymbolAttributesBag)
      FAttributes: TdwsSymbolAttributes;
    destructor Destroy; override;
    function Attributes: TdwsSymbolAttributes;
  end;

  TSystemSymbols = class(TSystemSymbolTable, ISystemSymbols)
    FOperators: TSystemOperators;
    destructor Destroy; override;
    function operators: TSystemOperators;
  end;

  // StringToSwitchInstruction
  //
function StringToSwitchInstruction(const str: String): TSwitchInstruction;
begin
  // This procedure is called by the tokenizer if it finds {$xx in the String
  for result := Low(TSwitchInstruction) to High(TSwitchInstruction) do
  begin
    if str = cSwitchInstructions[result] then
      Exit;
  end;
  result := siNone;
end;

// ------------------
// ------------------ TSymbolAttributesBag ------------------
// ------------------

// Destroy
//
destructor TSymbolAttributesBag.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

// Attributes
//
function TSymbolAttributesBag.Attributes: TdwsSymbolAttributes;
begin
  result := FAttributes;
end;

// ------------------
// ------------------ TSystemSymbols ------------------
// ------------------

// Destroy
//
destructor TSystemSymbols.Destroy;
begin
  FOperators.Free;
  inherited;
end;

// Operators
//
function TSystemSymbols.operators: TSystemOperators;
begin
  result := FOperators;
end;

// ------------------
// ------------------ TStandardSymbolFactory ------------------
// ------------------

// Create
//
constructor TStandardSymbolFactory.Create(aCompiler: TdwsCompiler);
begin
  inherited Create;
  FCompiler := aCompiler;
end;

// CheckName
//
procedure TStandardSymbolFactory.CheckName(const name: String;
  const namePos: TScriptPos);
begin
  FCompiler.CheckName(name, namePos);
end;

// CreateDataSymbol
//
function TStandardSymbolFactory.CreateDataSymbol(const name,
  externalName: String; const namePos: TScriptPos; typ: TTypeSymbol)
  : TDataSymbol;
begin
  CheckName(name, namePos);
  result := TVarDataSymbol.Create(name, typ);
  if externalName <> '' then
    result.externalName := externalName;
  FCompiler.CurrentProg.table.AddSymbol(result);
end;

// CreateConstSymbol
//
function TStandardSymbolFactory.CreateConstSymbol(const name: String;
  const namePos: TScriptPos; typ: TTypeSymbol; const data: IDataContext)
  : TConstSymbol;
begin
  if data <> nil then
    result := TConstSymbol.CreateData(name, typ, data)
  else
    result := TConstSymbol.Create(name, typ);
  FCompiler.CurrentProg.table.AddSymbol(result);
end;

// ReadExpr
//
function TStandardSymbolFactory.ReadExpr(expecting: TTypeSymbol = nil)
  : TTypedExpr;
begin
  result := FCompiler.ReadExpr(expecting);
end;

// ReadArrayConstantExpr
//
function TStandardSymbolFactory.ReadArrayConstantExpr(closingToken: TTokenType;
  expecting: TTypeSymbol): TArrayConstantExpr;
begin
  if coContextMap in FCompiler.options then
    FCompiler.FSourceContextMap.OpenContext(FCompiler.FTok.CurrentPos,
      expecting.typ, ttARRAY);
  try
    result := FCompiler.ReadArrayConstant(closingToken, expecting);
  finally
    if coContextMap in FCompiler.options then
      FCompiler.FSourceContextMap.CloseContext
        (FCompiler.FTok.CurrentPos, ttARRAY);
  end;
end;

// ReadInitExpr
//
function TStandardSymbolFactory.ReadInitExpr(expecting: TTypeSymbol = nil)
  : TTypedExpr;

  function ReadConstRecordInitExpr(recSym: TRecordSymbol): TTypedExpr;
  begin
    // isolate because of the temporary dynamic array
    result := TConstExpr.CreateData(cNullPos, expecting,
      FCompiler.ReadConstRecord(recSym));
  end;

begin
  if expecting <> nil then
  begin
    case FCompiler.Tokenizer.TestAny([ttBLEFT, ttALEFT]) of
      ttBLEFT:
        begin
          var
          expectingClassType := expecting.ClassType;
          if expectingClassType = TRecordSymbol then
          begin
            result := ReadConstRecordInitExpr(TRecordSymbol(expecting));
            Exit;
          end
          else if expectingClassType.InheritsFrom(TArraySymbol) then
          begin
            FCompiler.Tokenizer.KillToken;
            result := ReadArrayConstantExpr(ttBRIGHT, expecting);
            Exit;
          end;
        end;
      ttALEFT:
        begin
          var
          expectingClassType := expecting.ClassType;
          if expectingClassType.InheritsFrom(TArraySymbol) then
          begin
            FCompiler.Tokenizer.KillToken;
            result := ReadArrayConstantExpr(ttARIGHT, expecting);
            Exit;
          end
          else if expectingClassType = TSetOfSymbol then
          begin
            FCompiler.Tokenizer.KillToken;
            result := ReadArrayConstantExpr(ttARIGHT, expecting);
            result := TConvExpr.WrapWithConvCast(FCompiler.FCompilerContext,
              FCompiler.Tokenizer.hotPos, expecting, result,
              CPE_IncompatibleTypes);
            Exit;
          end;
        end;
    end;
  end;
  result := ReadExpr(expecting)
end;

// ------------------
// ------------------ TCompositeTypeSymbolFactory ------------------
// ------------------

// Create
//
constructor TCompositeTypeSymbolFactory.Create(aCompiler: TdwsCompiler;
  ownerType: TCompositeTypeSymbol; aVisibility: TdwsVisibility);
begin
  inherited Create(aCompiler);
  FOwnerType := ownerType;
  FVisibility := aVisibility;
end;

// CheckName
//
procedure TCompositeTypeSymbolFactory.CheckName(const name: String;
  const namePos: TScriptPos);
var
  sym: TSymbol;
begin
  if name = '' then
    Exit;

  sym := FOwnerType.Members.FindLocal(name);
  if Assigned(sym) then
    FCompiler.FMsgs.AddCompilerErrorFmt(namePos, CPE_NameAlreadyExists, [name]);
end;

// CreateDataSymbol
//
function TCompositeTypeSymbolFactory.CreateDataSymbol(const name,
  externalName: String; const namePos: TScriptPos; typ: TTypeSymbol)
  : TDataSymbol;
var
  cvs: TClassVarSymbol;
begin
  CheckName(name, namePos);

  cvs := TClassVarSymbol.Create(name, typ, FVisibility);
  cvs.level := 0;
  cvs.StackAddr := FCompiler.FMainProg.GetGlobalAddr(typ.Size);
  if externalName <> '' then
    cvs.externalName := externalName;
  FOwnerType.AddClassVar(cvs);
  result := cvs;
end;

// CreateConstSymbol
//
function TCompositeTypeSymbolFactory.CreateConstSymbol(const name: String;
  const namePos: TScriptPos; typ: TTypeSymbol; const data: IDataContext)
  : TConstSymbol;
var
  classConstSym: TClassConstSymbol;
begin
  if data <> nil then
    classConstSym := TClassConstSymbol.CreateData(name, typ, data)
  else
    classConstSym := TClassConstSymbol.Create(name, typ);
  classConstSym.visibility := FVisibility;
  FOwnerType.AddConst(classConstSym);
  result := classConstSym;
end;

// ReadExpr
//
function TCompositeTypeSymbolFactory.ReadExpr(expecting: TTypeSymbol = nil)
  : TTypedExpr;
begin
  result := FCompiler.ReadClassExpr(FOwnerType, expecting);
end;

// ------------------
// ------------------ TdwsCompiler ------------------
// ------------------

// Create
//
constructor TdwsCompiler.Create;
var
  stackParams: TStackParameters;
begin
  inherited;

  FStringListPool.Initialize;

  FStandardDataSymbolFactory := TStandardSymbolFactory.Create(Self);

  FTokRules := TPascalTokenizerStateRules.Create;

  FLoopExprs := TSimpleStack<TProgramExpr>.Create;
  FLoopExitable := TSimpleStack<TLoopExitable>.Create;
  FFinallyExprs := TSimpleStack<Boolean>.Create;
  FUnitsFromStack := TSimpleStack<String>.Create;
  FUnitContextStack := TdwsCompilerUnitContextStack.Create;

  FPendingAttributes := TdwsSymbolAttributes.Create;

  stackParams.MaxLevel := 1;
  stackParams.ChunkSize := 512;
  stackParams.MaxByteSize := MaxInt;
  stackParams.MaxRecursionDepth := cDefaultMaxRecursionDepth;
  stackParams.MaxExceptionDepth := cDefaultMaxExceptionDepth;

  FExec := TdwsCompilerExecution.Create(stackParams, Self);

  FGenericSymbol := TSimpleStack<TGenericSymbol>.Create;
end;

// Destroy
//
destructor TdwsCompiler.Destroy;
begin
  FGenericSymbol.Free;

  FPendingAttributes.Free;

  FUnitsFromStack.Free;
  FUnitContextStack.Free;
  FExec.Free;
  FFinallyExprs.Free;
  FLoopExitable.Free;
  FLoopExprs.Free;
  FTokRules.Free;

  FStringListPool.Finalize;

  inherited;
end;

function TdwsCompiler.ResolveUnitReferences(scriptType: TScriptSourceType)
  : TIdwsUnitList;
var
  expectedUnitCount: Integer;
  deps: TStringList;
  refCount: array of Integer;
  changed: Boolean;
  unitName: String;
  curUnit: IdwsUnit;
begin
  // Check for duplicate unit names
  unitName := FCompilerContext.UnitList.FindDuplicateUnitName;
  if unitName <> '' then
    FMsgs.AddCompilerStopFmt(cNullPos, CPH_UnitAlreadyReferred, [unitName]);

  // initialize reference count vector
  expectedUnitCount := FCompilerContext.UnitList.Count;
  SetLength(refCount, expectedUnitCount);

  // Calculate number of outgoing references
  for var i := 0 to FCompilerContext.UnitList.Count - 1 do
  begin
    curUnit := FCompilerContext.UnitList[i];
    if (ufImplicitUse in curUnit.GetUnitFlags) or
      ((scriptType <> stUnit) and not(coExplicitUnitUses in FOptions)) then
    begin
      deps := curUnit.GetDependencies;
      for var j := 0 to deps.Count - 1 do
      begin
        if FCompilerContext.UnitList.IndexOfName(deps[j]) < 0 then
          FMsgs.AddCompilerStopFmt(cNullPos, CPE_UnitNotFound,
            [deps[j], curUnit.GetUnitName]);
      end;
      refCount[i] := deps.Count;
    end
    else
    begin
      refCount[i] := -1;
      Dec(expectedUnitCount);
    end;
  end;

  result := TIdwsUnitList.Create;

  // Resolve references
  repeat
    changed := False;
    for var i := 0 to FCompilerContext.UnitList.Count - 1 do
    begin
      // Find unit that is not referencing other units
      if refCount[i] = 0 then
      begin
        curUnit := FCompilerContext.UnitList[i];
        result.Add(curUnit);

        // Remove the references to this unit from all other units
        unitName := curUnit.GetUnitName;
        for var j := 0 to FCompilerContext.UnitList.Count - 1 do
        begin
          deps := FCompilerContext.UnitList[j].GetDependencies;
          for var k := 0 to deps.Count - 1 do
          begin
            if UnicodeSameText(deps[k], unitName) then
              Dec(refCount[j]);
          end;
        end;

        refCount[i] := -1;
        changed := True;
      end;
    end;
  until not changed;

  if result.Count <> expectedUnitCount then
  begin
    result.Free;
    result := nil;
    FMsgs.AddCompilerStop(cNullPos, CPE_UnitCircularReference);
  end;
end;

// CreateTypedDefault
//
function TdwsCompiler.CreateTypedDefault(typ: TTypeSymbol): TConstExpr;

  function CreateGenericTypedDefault: TConstExpr;
  var
    data: IDataContext;
  begin
    data := TDataContext.CreateStandalone(typ.Size);
    typ.InitDataContext(data, 0);
    result := TConstExpr.CreateData(cNullPos, typ, data);
  end;

begin
  if typ = FCompilerContext.TypInteger then
    result := FUnifiedConstants.CreateInteger(0)
  else if typ = FCompilerContext.TypString then
    result := FUnifiedConstants.CreateEmptyString
  else
    result := CreateGenericTypedDefault;
end;

// SetCurrentProg
//
procedure TdwsCompiler.SetCurrentProg(aProg: TdwsProgram);
begin
  FCurrentProg := aProg;
  if Assigned(aProg) then
  begin
    FCompilerContext := aProg.Root.CompilerContext;
    FCompilerContext.Prog := aProg;
    FCompilerContext.Execution := FExec;
    FCompilerContext.options := FOptions;
  end
  else
  begin
    FCompilerContext := nil;
  end;
end;

// GetCompilerContext
//
function TdwsCompiler.GetCompilerContext: TdwsCompilerContext;
begin
  result := FCompilerContext;
end;

// GetFieldReadOnlyState
//
function TdwsCompiler.GetFieldReadOnlyState(fieldSym: TFieldSymbol)
  : TFieldExprReadOnlyState;
begin
  result := feroDefault;
  if (CurrentProg = nil) or not fieldSym.ReadOnly then
    Exit;

  if CurrentProg is TdwsProcedure then
  begin
    // readonly fields are writeable in constructors of their structure
    var
    func := TdwsProcedure(CurrentProg).func;
    if (func.Kind = fkConstructor) and
      (TMethodSymbol(func).StructSymbol = fieldSym.StructSymbol) then
      result := feroWriteable;
  end;
end;

// DoProgramDestroyed
//
procedure TdwsCompiler.DoProgramDestroyed(Sender: TObject);
begin
  FastInterlockedDecrement(FActiveProgramCount);
end;

// EnterLoop
//
procedure TdwsCompiler.EnterLoop(loopExpr: TProgramExpr);
begin
  FLoopExprs.Push(loopExpr);
  FLoopExitable.Push(leNotExitable);
  if FFinallyExprs.Count > 0 then
    FFinallyExprs.Push(False);
end;

// MarkLoopExitable
//
procedure TdwsCompiler.MarkLoopExitable(level: TLoopExitable);
var
  i: Integer;
begin
  if FLoopExprs.Count = 0 then
    Exit;
  case level of
    leBreak:
      begin
        if FLoopExitable.Peek = leNotExitable then
          FLoopExitable.Peek := level;
      end;
    leExit:
      begin
        for i := 0 to FLoopExitable.Count - 1 do
        begin
          if FLoopExitable.Items[i] = level then
            Break;
          FLoopExitable.Items[i] := level;
        end;
      end;
  end;
end;

// LeaveLoop
//
procedure TdwsCompiler.LeaveLoop;
begin
  if FLoopExitable.Peek = leNotExitable then
    FCompilerContext.Msgs.AddCompilerWarning(FLoopExprs.Peek.scriptPos,
      CPW_InfiniteLoop);

  if (FFinallyExprs.Count > 0) and (not FFinallyExprs.Peek) then
    FFinallyExprs.Pop;

  FLoopExprs.Pop;
  FLoopExitable.Pop;
end;

// GetFuncExpr
//
function TdwsCompiler.GetFuncExpr(funcSym: TFuncSymbol;
  codeExpr: TTypedExpr = nil): TFuncExprBase;
begin
  if codeExpr = nil then
  begin

    result := CreateSimpleFuncExpr(FCompilerContext, FTok.hotPos, funcSym);

  end
  else
  begin

    result := TFuncPtrExpr.Create(FCompilerContext, FTok.hotPos, codeExpr);

  end;

  if (funcSym.typ <> nil) and (funcSym.typ.Size > 1) and
    result.InheritsFrom(TFuncExpr) then
    TFuncExpr(result).InitializeResultAddr(CurrentProg);
end;

// GetMethodExpr
//
function TdwsCompiler.GetMethodExpr(meth: TMethodSymbol; expr: TTypedExpr;
  RefKind: TRefKind; const scriptPos: TScriptPos;
  options: TCreateFunctionOptions): TFuncExprBase;
begin
  result := CreateMethodExpr(FCompilerContext, meth, expr, RefKind,
    scriptPos, options);
end;

// MemberSymbolWithNameAlreadyExists
//
procedure TdwsCompiler.MemberSymbolWithNameAlreadyExists(sym: TSymbol;
  const hotPos: TScriptPos);
var
  msgFmt: String;
begin
  if sym is TFieldSymbol then
    msgFmt := CPE_FieldRedefined
  else if sym is TPropertySymbol then
    msgFmt := CPE_PropertyRedefined
  else if sym is TClassVarSymbol then
    msgFmt := CPE_ClassVarRedefined
  else if sym is TConstSymbol then
    msgFmt := CPE_ClassConstRedefined
  else if sym is TMethodSymbol then
  begin
    msgFmt := CPE_MethodRedefined;
    TMethodSymbol(sym).IgnoreMissingImplementation := True;
  end
  else
  begin
    msgFmt := CPE_NameAlreadyExists
  end;
  FMsgs.AddCompilerErrorFmt(hotPos, msgFmt, [sym.name])
end;

// IncompatibleTypes
//
procedure TdwsCompiler.IncompatibleTypes(const scriptPos: TScriptPos;
  const fmt: String; typ1, typ2: TTypeSymbol);
begin
  FMsgs.AddCompilerErrorFmt(scriptPos, fmt, [typ1.Caption, typ2.Caption]);
end;

// IncompatibleTypes
//
procedure TdwsCompiler.IncompatibleTypes(const scriptPos: TScriptPos;
  const fmt: String; typ1: TTypeSymbol; expr2: TTypedExpr);
begin
  if expr2 is TFuncSimpleExpr then
    FMsgs.AddCompilerErrorFmt(scriptPos, fmt,
      [typ1.Caption, TFuncSimpleExpr(expr2).funcSym.Caption])
  else
    IncompatibleTypes(scriptPos, fmt, typ1, expr2.typ);
end;

// IncompatibleTypesWarn
//
procedure TdwsCompiler.IncompatibleTypesWarn(const scriptPos: TScriptPos;
  const fmt: String; typ1, typ2: TTypeSymbol);
begin
  FMsgs.AddCompilerWarningFmt(scriptPos, fmt, [typ1.Caption, typ2.Caption]);
end;

// ReportImplicitMethodOverload
//
procedure TdwsCompiler.ReportImplicitMethodOverload(const scriptPos: TScriptPos;
  const name: String; const aLevel: TdwsHintsLevel = hlNormal);
begin
  if coMissingOverloadedAsErrors in FOptions then
    FMsgs.AddCompilerErrorFmt(scriptPos, CPH_MustExplicitMethodOverload, [name])
  else
    FMsgs.AddCompilerHintFmt(scriptPos, CPH_ShouldExplicitMethodOverload,
      [name], aLevel);
end;

// SetupCompileOptions
//
procedure TdwsCompiler.SetupCompileOptions(conf: TdwsConfiguration);
begin
  FCompilerAbort := False;

  FFilter := conf.filter;
  FConnectors := conf.Connectors;
  FOptions := conf.CompilerOptions;
  FOnInclude := conf.DoIncludeEx;
  FOnNeedUnit := conf.DoNeedUnitEx;
  FOnResource := conf.OnResource;
  FOnCodeGen := conf.OnCodeGen;
  FOnFilter := conf.OnFilter;
  FScriptPaths := conf.ScriptPaths;

  FOnExecutionStarted := conf.OnExecutionStarted;
  FOnExecutionEnded := conf.OnExecutionEnded;

  conf.FOnCreateBaseVariantSymbol := FOnCreateBaseVariantSymbol;
  conf.FOnCreateSystemSymbols := FOnCreateSystemSymbols;
  conf.FOnRegisterSystemOperators := FOnRegisterSystemOperators;
  if not StaticExtensionSymbols then
    conf.DetachSystemTable;

  if conf.CompileFileSystem <> nil then
    FCompileFileSystem := conf.CompileFileSystem.AllocateFileSystem
  else
    FCompileFileSystem := TdwsOSFileSystem.Create;

  FOnGetDefaultLocalizer := conf.DoGetLocalizer;

  FDataSymbolExprReuse := nil;
  /// disabled for now TSimpleObjectObjectHash_TDataSymbol_TVarExpr.Create;

  FDefaultConditionals := TAutoStrings.CreateClone(conf.Conditionals);
  FDefaultConditionals.value.Duplicates := dupIgnore;
  FDefaultConditionals.value.Sorted := True;
  FDefaultConditionals.value.Add('DWSCRIPT');
  if Assigned(FOnApplyConditionalDefines) then
    FOnApplyConditionalDefines(FDefaultConditionals.value);

  F8087CW := DirectSet8087CW($133F);
end;

// SetupMsgsOptions
//
procedure TdwsCompiler.SetupMsgsOptions(conf: TdwsConfiguration);
begin
  FDefaultHintsLevel := conf.HintsLevel;
  if coHintsDisabled in conf.CompilerOptions then
    FMsgs.HintsLevel := hlDisabled
  else
    FMsgs.HintsLevel := conf.HintsLevel;
  FMsgs.WarningsDisabled := (coWarningsDisabled in conf.CompilerOptions);
  if (coHintKeywordCaseMismatch in conf.CompilerOptions) and
    (FMsgs.HintsLevel >= hlPedantic) then
    FTokRules.CaseSensitive := tcsHintCaseMismatch
  else
    FTokRules.CaseSensitive := tcsCaseInsensitive;
end;

// CleanupAfterCompile
//
var
  vCompileTidy: Integer = 8;

procedure TdwsCompiler.CleanupAfterCompile;
begin
  DirectSet8087CW(F8087CW);

  if AtomicDecrement(vCompileTidy) = 0 then
  begin
    TidyStringsUnifier;
    vCompileTidy := 8;
  end;

  FPendingAttributes.Clear;

  if FDataSymbolExprReuse <> nil then
  begin
    FDataSymbolExprReuse.CleanValues;
    FDataSymbolExprReuse.Free;
    FDataSymbolExprReuse := nil;
  end;

  FIsExcept := False;

  FOperators := nil;

  FMsgs := nil;

  FCompileFileSystem := nil;
  FOnInclude := nil;
  FOnNeedUnit := nil;
  FSystemTable := nil;
  FUnitContextStack.Clean;
  FUnitsFromStack.Clear;
  FCurrentUnitSymbol := nil;
  FCurrentSourceUnit := nil;

  FCurrentProg := nil;
  if FCompilerContext <> nil then
    FCompilerContext.StringsUnifier.Clear;
  FCompilerContext := nil;
  FMainProg := nil;
  FUnifiedConstants := nil;
  FSourceContextMap := nil;
  FSymbolDictionary := nil;

  FOnExecutionStarted := nil;
  FOnExecutionEnded := nil;

  FLoopExprs.Clear;
  FLoopExitable.Clear;
  FFinallyExprs.Clear;
end;

// Compile
//
function TdwsCompiler.Compile(const aCodeText: String; aConf: TdwsConfiguration;
  const mainFileName: String = ''): IdwsProgram;
var
  stackParams: TStackParameters;
  codeText: String;
  sourceFile: TSourceFile;
  compileStartTicks: Int64;
begin
  compileStartTicks := GetSystemMilliseconds;

  SetupCompileOptions(aConf);

  FGenericSymbol.Clear;

  stackParams.MaxByteSize := aConf.MaxDataSize;
  if stackParams.MaxByteSize <= 0 then
    stackParams.MaxByteSize := MaxInt;

  stackParams.ChunkSize := aConf.StackChunkSize;
  Assert(stackParams.ChunkSize > 0);

  stackParams.MaxRecursionDepth := aConf.MaxRecursionDepth;
  stackParams.MaxExceptionDepth := aConf.MaxExceptionDepth;

  FLineCount := 0;

  // Create the TdwsProgram
  FMainProg := CreateProgram(aConf.SystemSymbols, aConf.ResultType, stackParams,
    mainFileName);
  FSystemTable := FMainProg.systemTable.SymbolTable;
  FUnifiedConstants := TUnifiedConstants(FMainProg.UnifiedConstants);

  FMsgs := FMainProg.CompileMsgs;
  SetupMsgsOptions(aConf);

  FMainProg.compiler := Self;
  FMainProg.TimeoutMilliseconds := aConf.TimeoutMilliseconds;
  FMainProg.RuntimeFileSystem := aConf.RuntimeFileSystem;
  FMainProg.ConditionalDefines.value.Assign(FDefaultConditionals.value);
  FMainProg.OnExecutionStarted := FOnExecutionStarted;
  FMainProg.OnExecutionEnded := FOnExecutionEnded;

  FastInterlockedIncrement(FActiveProgramCount);
  FMainProg.OnDestroy := DoProgramDestroyed;

  FSourceContextMap := FMainProg.SourceContextMap;
  FSymbolDictionary := FMainProg.SymbolDictionary;
  FUnitSection := secMixed;

  CurrentProg := FMainProg;

  FCompilerContext.UnitList.AddUnits(aConf.Units);

  FOperators := aConf.SystemSymbols.operators;
  FMainProg.operators := FOperators;

  if Assigned(FExternalRoutinesManager) then
    FExternalRoutinesManager.BeginCompilation(Self);

  FCompilerContext.HelperMemberNames.Clear;
  FCompilerContext.HelperMemberNames.PreallocateCapacity(256);
  dwsInternalUnit.EnumerateHelperMemberNames
    (FCompilerContext.HelperMemberNames);

  try
    CheckFilterDependencies(aConf.Units);

    // Filter stuff
    if Assigned(FFilter) then
      codeText := FFilter.Process(aCodeText, FMsgs)
    else
      codeText := aCodeText;

    sourceFile := FMainProg.SourceList.Add(MSG_MainModule, codeText, stMain,
      mainFileName);

    // Start compilation
    CurrentProg.expr := ReadScript(sourceFile, stMain);
    ReadScriptImplementations;

    if CurrentProg.expr = nil then
      CurrentProg.expr := TBlockExpr.Create(FCompilerContext, cNullPos);

    HintUnusedPrivateSymbols;

    // Initialize symbol table
    CurrentProg.table.Initialize(FMsgs);
    CurrentProg.UnitMains.Initialize(FMsgs);

    SetupInitializationFinalization;

    // setup environment
    if Assigned(FOnGetDefaultEnvironment) then
      FMainProg.DefaultEnvironment := FOnGetDefaultEnvironment();

    // setup localizer
    if Assigned(FOnGetDefaultLocalizer) then
      FMainProg.DefaultLocalizer := FOnGetDefaultLocalizer();

  except
    on e: ECompileError do;
    on e: ECompileException do
      FMsgs.AddCompilerError(e.scriptPos, e.Message);
    on e: Exception do
      FMsgs.AddCompilerError(cNullPos, e.Message);
  end;

  if Assigned(FExternalRoutinesManager) then
    FExternalRoutinesManager.EndCompilation(Self);

  if FMsgs.State = mlsInProgress then
    FMsgs.State := mlsCompleted;
  FMsgs.RemoveInvalidDeferred;

  FMainProg.TimeStamp := Now;
  FMainProg.CompileDurationMSec := GetSystemMilliseconds - compileStartTicks;
  FMainProg.LineCount := FLineCount;
  FMainProg.compiler := nil;

  result := FMainProg;

  CleanupAfterCompile;
end;

// CheckFilterDependencies
//
procedure TdwsCompiler.CheckFilterDependencies(confUnits: TIdwsUnitList);
var
  f: TdwsFilter;
  dep: String;
begin
  // Check for missing units
  f := FFilter;
  while Assigned(f) do
  begin
    for dep in f.Dependencies do
    begin
      if confUnits.IndexOfName(dep) < 0 then
        FMsgs.AddCompilerErrorFmt(cNullPos, CPE_FilterDependsOnUnit,
          [f.ClassName, dep]);
    end;
    f := f.SubFilter;
  end;
end;

// HandleUnitDependencies
//
procedure TdwsCompiler.HandleUnitDependencies(scriptType: TScriptSourceType);
var
  unitsResolved: TIdwsUnitList;
  unitTable: TUnitSymbolTable;
  UnitSymbol: TUnitMainSymbol;
  u: IdwsUnit;
begin
  unitsResolved := ResolveUnitReferences(scriptType);
  try
    // Get the symboltables of the units
    for var i := 0 to unitsResolved.Count - 1 do
    begin
      u := unitsResolved[i];
      UnitSymbol := FMainProg.UnitMains.Find(u.GetUnitName);
      if not Assigned(UnitSymbol) then
      begin
        unitTable := (u as IdwsUnitTableFactory).GetUnitTable(FSystemTable,
          FMainProg.UnitMains, FOperators, FMainProg.RootTable);
        UnitSymbol := TUnitMainSymbol.Create(u.GetUnitName, unitTable,
          FMainProg.UnitMains);
        UnitSymbol.deprecatedMessage := u.GetDeprecatedMessage;
      end;
      UnitSymbol.ReferenceInSymbolTable(CurrentProg.table, True);
    end;
  finally
    unitsResolved.Free;
  end;
end;

// HandleExplicitDependency
//
function TdwsCompiler.HandleExplicitDependency(const scriptPos: TScriptPos;
  const unitName: String): TUnitSymbol;
var
  i: Integer;
  unitResolved: IdwsUnit;
  unitTable: TUnitSymbolTable;
  unitMain: TUnitMainSymbol;
  Dependencies: TStringList;
  unitSource, unitLocation: String;
  resolvedUnitName: String;
  srcUnit: TSourceUnit;
  oldContext: TdwsSourceContext;
begin
  for i := 0 to FUnitsFromStack.Count - 1 do
    if UnicodeSameText(FUnitsFromStack.Items[i], unitName) then
      FMsgs.AddCompilerStop(scriptPos, CPE_UnitCircularReference);

  result := TUnitSymbol(CurrentProg.table.FindLocalOfClass(unitName,
    TUnitSymbol));
  if (result <> nil) and (result.Main <> nil) then
  begin
    // ignore multiple requests (for now)
    Exit;
  end;

  i := FCompilerContext.UnitList.IndexOfName(unitName);
  if i < 0 then
  begin
    if Assigned(FOnNeedUnit) then
    begin
      unitResolved := FOnNeedUnit(unitName, unitSource, unitLocation);
      if (unitResolved = nil) and (unitLocation <> '') then
      begin
        resolvedUnitName := ChangeFileExt(ExtractFileName(unitLocation), '');
        if not UnicodeSameText(resolvedUnitName, unitName) then
          resolvedUnitName := unitName
        else if resolvedUnitName <> unitName then
          FMsgs.AddCompilerHint(scriptPos, CPH_UnitNameCaseDoesntMatch)
      end;
    end;
    if unitResolved <> nil then
      FCompilerContext.UnitList.Add(unitResolved)
    else
    begin
      if unitSource = '' then
        unitSource := GetScriptSource(unitName + '.pas');
      if unitSource <> '' then
      begin
        if resolvedUnitName = '' then
          resolvedUnitName := unitName;
        srcUnit := TSourceUnit.Create(resolvedUnitName,
          CurrentProg.Root.RootTable, CurrentProg.UnitMains);
        unitResolved := srcUnit;
        FCompilerContext.UnitList.Add(unitResolved);
        oldContext := FSourceContextMap.SuspendContext;
        SwitchTokenizerToUnit(srcUnit, unitSource, unitLocation);
        FSourceContextMap.ResumeContext(oldContext);
      end;
    end;
    if unitResolved = nil then
    begin
      if FUnitsFromStack.Count = 0 then
        FMsgs.AddCompilerErrorFmt(scriptPos, CPE_UnknownUnit, [unitName])
      else
        FMsgs.AddCompilerErrorFmt(scriptPos, CPE_UnitNotFound,
          [unitName, FUnitsFromStack.Peek]);
      Exit;
    end;
  end
  else
    unitResolved := FCompilerContext.UnitList[i];
  resolvedUnitName := unitResolved.GetUnitName;

  Dependencies := unitResolved.GetDependencies;
  for i := 0 to Dependencies.Count - 1 do
  begin
    FUnitsFromStack.Push(unitName);
    try
      HandleExplicitDependency(scriptPos, Dependencies[i]);
    finally
      FUnitsFromStack.Pop;
    end;
  end;

  unitMain := CurrentProg.UnitMains.Find(resolvedUnitName);
  if unitMain = nil then
  begin
    unitTable := nil;
    try
      unitTable := (unitResolved as IdwsUnitTableFactory)
        .GetUnitTable(FSystemTable, CurrentProg.UnitMains, FOperators,
        CurrentProg.RootTable);
      unitMain := TUnitMainSymbol.Create(resolvedUnitName, unitTable,
        CurrentProg.UnitMains);
      unitMain.deprecatedMessage := unitResolved.GetDeprecatedMessage;
    except
      unitTable.Free;
      raise;
    end;
  end;

  result := unitMain.ReferenceInSymbolTable(CurrentProg.table, False);
end;

type
  TUnitMainSymbolArray = array of TUnitMainSymbol;

  TRankedUnits = class
    Ranked: TUnitMainSymbolArray;
    function Compare(index1, index2: NativeInt): Integer;
    procedure Swap(index1, index2: NativeInt);
  end;

function TRankedUnits.Compare(index1, index2: NativeInt): Integer;
begin
  result := Ranked[index1].InitializationRank - Ranked[index2]
    .InitializationRank;
end;

// Swap
//
procedure TRankedUnits.Swap(index1, index2: NativeInt);
var
  t: TUnitMainSymbol;
begin
  t := Ranked[index1];
  Ranked[index1] := Ranked[index2];
  Ranked[index2] := t;
end;

// SetupInitializationFinalization
//
procedure TdwsCompiler.SetupInitializationFinalization;
var
  rankedUnits: TUnitMainSymbolArray;

  procedure RankUnits(rankLow: Integer);
  var
    r: Integer;
    ums, dep: TUnitMainSymbol;
    change: Boolean;
  begin
    change := False;
    for ums in rankedUnits do
    begin
      r := ums.InitializationRank + 1;
      if r >= rankLow then
      begin
        for dep in ums.Dependencies do
        begin
          if dep.InitializationRank < r then
          begin
            dep.InitializationRank := r;
            change := True;
          end;
        end;
      end;
    end;
    if change and (rankLow < High(rankedUnits)) then
      RankUnits(rankLow + 1);
  end;

  procedure SortRankedUnits;
  var
    Ranked: TRankedUnits;
    sorter: TQuickSort;
  begin
    Ranked := TRankedUnits.Create;
    try
      Ranked.Ranked := rankedUnits;
      sorter.CompareMethod := Ranked.Compare;
      sorter.SwapMethod := Ranked.Swap;
      sorter.Sort(0, High(rankedUnits));
      rankedUnits := Ranked.Ranked;
    finally
      Ranked.Free;
    end;
  end;

var
  i, k: Integer;
  ums: TUnitMainSymbol;
  unitInitExpr: TBlockExprBase;
begin
  // collect and rank all units with an initialization or finalization sections
  // NOTE: UnitMains order may change arbitrarily in the future, hence the need to reorder
  SetLength(rankedUnits, CurrentProg.UnitMains.Count);
  k := 0;
  for i := 0 to CurrentProg.UnitMains.Count - 1 do
  begin
    ums := CurrentProg.UnitMains[i];
    if (ums.InitializationExpr <> nil) or (ums.FinalizationExpr <> nil) then
    begin
      rankedUnits[k] := ums;
      Inc(k);
      ums.InitializationRank := 0;
    end;
  end;
  if k = 0 then
    Exit;

  // compute dependency graph rank & sort accordingly
  SetLength(rankedUnits, k);
  RankUnits(0);
  SortRankedUnits;

  // append initializations to InitExpr of the main prog
  for i := High(rankedUnits) downto 0 do
  begin
    ums := rankedUnits[i];
    if (ums <> nil) and (ums.InitializationExpr <> nil) then
    begin
      unitInitExpr := ums.InitializationExpr as TBlockExprBase;
      if coOptimize in options then
      begin
        if unitInitExpr.StatementCount = 0 then
          continue;
      end;
      FMainProg.initExpr.AddStatement(unitInitExpr);
      ums.InitializationExpr.IncRefCount;
    end;
  end;
  // append initializations to FinalExpr of the main prog in reverse order
  for i := 0 to High(rankedUnits) do
  begin
    ums := rankedUnits[i];
    if (ums <> nil) and (ums.FinalizationExpr <> nil) then
    begin
      FMainProg.AddFinalExpr(ums.FinalizationExpr as TBlockExprBase);
      ums.FinalizationExpr.IncRefCount;
    end;
  end;
end;

// OrphanObject
//
procedure TdwsCompiler.OrphanObject(obj: TRefCountedObject);
begin
  if obj <> nil then
    FCompilerContext.OrphanObject(obj);
end;

// OrphanAndNil (TTypedExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr: TTypedExpr);
begin
  if expr <> nil then
  begin
    expr.Orphan(FCompilerContext);
    expr := nil;
  end;
end;

// OrphanAndNil (TProgramExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr: TProgramExpr);
begin
  if expr <> nil then
  begin
    expr.Orphan(FCompilerContext);
    expr := nil;
  end;
end;

// OrphanAndNil (TDataExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr: TDataExpr);
begin
  if expr <> nil then
  begin
    expr.Orphan(FCompilerContext);
    expr := nil;
  end;
end;

// OrphanAndNil (TBlockExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr: TBlockExpr);
begin
  if expr <> nil then
  begin
    expr.Orphan(FCompilerContext);
    expr := nil;
  end;
end;

// OrphanAndNil (TVarExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr: TVarExpr);
begin
  if expr <> nil then
  begin
    expr.Orphan(FCompilerContext);
    expr := nil;
  end;
end;

// Compiler
//
function TdwsCompiler.compiler: TdwsCompiler;
begin
  result := Self;
end;

// GetCurrentProg
//
function TdwsCompiler.GetCurrentProg: TdwsProgram;
begin
  result := CurrentProg;
end;

// GetMsgs
//
function TdwsCompiler.GetMsgs: TdwsCompileMessageList;
begin
  result := Msgs;
end;

// GetTokenizer
//
function TdwsCompiler.GetTokenizer: TTokenizer;
begin
  result := Tokenizer;
end;

// SetExternalFunctionsManager
//
function TdwsCompiler.GetExternalFunctionsManager: IdwsExternalFunctionsManager;
begin
  result := FExternalRoutinesManager;
end;

// SetExternalFunctionsManager
//
procedure TdwsCompiler.SetExternalFunctionsManager
  (const value: IdwsExternalFunctionsManager);
begin
  FExternalRoutinesManager := value;
end;

// CompileTimeExecution
//
function TdwsCompiler.CompileTimeExecution: TdwsExecution;
begin
  result := FExec;
end;

// AttachContextProgram
//
procedure TdwsCompiler.AttachContextProgram(contextProgram: TdwsProgram);
begin
  CurrentProg := contextProgram;
  FMainProg := contextProgram.Root;
  FUnifiedConstants := TUnifiedConstants(FMainProg.UnifiedConstants);
  FSourceContextMap := FMainProg.SourceContextMap;
  FSymbolDictionary := FMainProg.SymbolDictionary;
  FOperators := (FMainProg.operators as TOperators);
  FMsgs := FMainProg.CompileMsgs;
end;

// DetachContextProgram
//
procedure TdwsCompiler.DetachContextProgram;
begin
  FMsgs := nil;
  FOperators := nil;
  CurrentProg := nil;
  FMainProg := nil;
  FUnifiedConstants := nil;
  FSourceContextMap := nil;
  FSymbolDictionary := nil;
end;

// AttachTokenizer
//
procedure TdwsCompiler.AttachTokenizer(tok: TTokenizer);
begin
  FTok := tok;
end;

// DetachTokenizer
//
function TdwsCompiler.DetachTokenizer: TTokenizer;
begin
  result := FTok;
  FTok := nil;
end;

// CheckMatchingDeclarationCase
//
procedure TdwsCompiler.CheckMatchingDeclarationCase(const nameString: String;
  sym: TSymbol; const scriptPos: TScriptPos);
begin
  if (nameString <> sym.name) and UnicodeSameText(nameString, sym.name) then
    FMsgs.AddCompilerHintFmt(scriptPos, CPH_CaseDoesNotMatchDeclaration,
      [nameString, sym.name], hlPedantic);
end;

// RecordSymbolUse
//
procedure TdwsCompiler.RecordSymbolUse(sym: TSymbol;
  const scriptPos: TScriptPos; const useTypes: TSymbolUsages);
begin
  if coSymbolDictionary in options then
    FSymbolDictionary.AddSymbol(sym, scriptPos, useTypes);
end;

// RecordSymbolUseReference
//
procedure TdwsCompiler.RecordSymbolUseReference(sym: TSymbol;
  const scriptPos: TScriptPos; isWrite: Boolean);
begin
  if isWrite then
    RecordSymbolUse(sym, scriptPos, [suReference, suWrite])
  else
    RecordSymbolUse(sym, scriptPos, [suReference, suRead]);
end;

// RecordSymbolUseImplicitReference
//
procedure TdwsCompiler.RecordSymbolUseImplicitReference(sym: TSymbol;
  const scriptPos: TScriptPos; isWrite: Boolean);
begin
  if isWrite then
    RecordSymbolUse(sym, scriptPos, [suReference, suWrite, suImplicit])
  else
    RecordSymbolUse(sym, scriptPos, [suReference, suRead, suImplicit]);
end;

// ReplaceSymbolUse
//
procedure TdwsCompiler.ReplaceSymbolUse(oldSym, newSym: TSymbol;
  const scriptPos: TScriptPos);
begin
  if coSymbolDictionary in options then
    FSymbolDictionary.ReplaceSymbolAt(oldSym, newSym, scriptPos);
end;

// RecompileInContext
//
procedure TdwsCompiler.RecompileInContext(const Context: IdwsProgram;
  const aCodeText: String; aConf: TdwsConfiguration);
var
  codeText: String;
  sourceFile: TSourceFile;
begin
  SetupCompileOptions(aConf);

  FMainProg := Context.ProgramObject as TdwsMainProgram;
  FMainProg.compiler := Self;
  FSystemTable := FMainProg.systemTable.SymbolTable;
  FUnifiedConstants := TUnifiedConstants(FMainProg.UnifiedConstants);
  FSourceContextMap := FMainProg.SourceContextMap;
  FSymbolDictionary := FMainProg.SymbolDictionary;

  FMsgs := FMainProg.CompileMsgs;
  SetupMsgsOptions(aConf);

  CurrentProg := FMainProg;

  FOperators := FMainProg.operators as TOperators;

  FLineCount := 0;
  try
    // Filter stuff
    if Assigned(FFilter) then
      codeText := FFilter.Process(aCodeText, FMsgs)
    else
      codeText := aCodeText;

    sourceFile := FMainProg.SourceList.FindScriptSourceItem(MSG_MainModule)
      .sourceFile;
    sourceFile.code := codeText;

    FMainProg.ResetExprs;
    FCurrentUnitSymbol := nil;
    FCurrentSourceUnit := nil;
    FUnitSection := secMixed;

    // Start compilation
    CurrentProg.expr := ReadScript(sourceFile, stRecompile);
    ReadScriptImplementations;

    if CurrentProg.expr = nil then
      CurrentProg.expr := TNullExpr.Create(cNullPos);

    // Initialize symbol table
    CurrentProg.table.Initialize(FMsgs);
  except
    on e: ECompileError do;
    on e: Exception do
      FMsgs.AddCompilerError(cNullPos, e.Message);
  end;

  FMainProg.LineCount := FLineCount;
  FMainProg.compiler := nil;

  CleanupAfterCompile;
end;

// Evaluate
//
class procedure TdwsCompiler.Evaluate;
begin
  // dummy
end;

// AbortCompilation
//
procedure TdwsCompiler.AbortCompilation;
begin
  FCompilerAbort := True;
end;

// ActiveProgramCount
//
function TdwsCompiler.ActiveProgramCount: Integer;
begin
  result := FActiveProgramCount;
end;

// Optimize
//
function TdwsCompiler.Optimize: Boolean;
begin
  result := FCompilerContext.Optimize;
end;

// ReadRootBlock
//
function TdwsCompiler.ReadRootBlock(const endTokens: TTokenTypes;
  var finalToken: TTokenType): TBlockExpr;
var
  reach: TReachStatus;
  stmt: TProgramExpr;
  action: TdwsStatementAction;
begin
  reach := rsReachable;
  result := TBlockExpr.Create(FCompilerContext, FTok.hotPos);
  try
    while FTok.HasTokens do
    begin
      finalToken := FTok.TestDeleteAny(endTokens);
      if finalToken <> ttNone then
        Break;

      if reach = rsUnReachable then
      begin
        reach := rsUnReachableWarned;
        FMsgs.AddCompilerWarning(FTok.hotPos, CPW_UnReachableCode);
      end;
      if FCompilerAbort then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_CompilationAborted);

      stmt := ReadRootStatement(action, result);
      if Assigned(stmt) then
      begin
        result.AddStatement(stmt);
        if (reach = rsReachable) and (stmt.InterruptsFlow) then
          reach := rsUnReachable;
      end;

      case action of
        saNone:
          begin
            if not FTok.TestDelete(ttSEMI) then
            begin
              if endTokens <> [] then
              begin
                finalToken := FTok.TestDeleteAny(endTokens);
                if finalToken = ttNone then
                  UnexpectedBlockTokenError(endTokens);
                Break;
              end
              else
              begin
                if FTok.HasTokens then
                  UnexpectedBlockTokenError(endTokens);
              end;
            end;
          end;
        saImplementation:
          begin
            finalToken := ttIMPLEMENTATION;
            Exit;
          end;
        saEnd:
          begin
            finalToken := ttEND;
            Exit;
          end;
      end;
    end;
  except
    OrphanAndNil(result);
    raise;
  end;
end;

// ReadSemiColon
//
procedure TdwsCompiler.ReadSemiColon(fatal: Boolean = False);
begin
  if not FTok.TestDelete(ttSEMI) then
  begin
    if fatal then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_SemiExpected)
    else
      FMsgs.AddCompilerError(FTok.hotPos, CPE_SemiExpected);
  end;
end;

// ReadScript
//
function TdwsCompiler.ReadScript(sourceFile: TSourceFile;
  scriptType: TScriptSourceType): TProgramExpr;
var
  oldTok: TTokenizer;
  oldSection: TdwsUnitSection;
  initialToken, finalToken: TTokenType;
  unitBlock: TBlockExpr;
  readingMain: Boolean;
  contextFix: TdwsSourceContext;
  i: Integer;
  UnitSymbol: TUnitSymbol;
begin
  oldTok := FTok;
  oldSection := FUnitSection;
  FTok := FTokRules.CreateTokenizer(FCompilerContext.Msgs,
    FCompilerContext.StringsUnifier);
  try
    FTok.BeginSourceFile(sourceFile);
    if coContextMap in options then
    begin
      case scriptType of
        stMain:
          FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttPROGRAM);
        stUnit:
          FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttUNIT);
      else
        FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttNone);
      end;
    end;
    FTok.SwitchHandler := ReadSwitch;
    FTok.SwitchProcessor := ReadInstrSwitch;
    if scriptType in [stMain, stRecompile] then
    begin
      FTok.ConditionalDefines := FMainProg.ConditionalDefines;
      readingMain := True;
    end
    else
    begin
      FTok.ConditionalDefines := FDefaultConditionals.Clone;
      readingMain := False;
    end;

    if readingMain then
    begin
      if FTok.Test(ttUNIT) then
      begin
        if scriptType = stRecompile then
          FMsgs.AddCompilerError(FTok.hotPos,
            CPE_RecompileInContextDoesntSupportUnits);

        if coContextMap in options then
        begin
          // need to fix the context map
          // the convoluted code below is required in case the first code content encountered
          // was an include switch, worst case is a 'UNIT' keyword in a bunch of nested includes
          contextFix := FSourceContextMap.Current;
          while (contextFix <> nil) and (contextFix.token <> ttPROGRAM) and
            (contextFix.StartPos.sourceFile <> sourceFile) do
            contextFix := contextFix.Parent;
          if contextFix <> nil then
            contextFix.token := ttUNIT;
        end;
        scriptType := stUnit;
        HandleUnitDependencies(scriptType);
      end
      else
      begin
        initialToken := FTok.TestDeleteAny([ttPROGRAM, ttLIBRARY]);
        if initialToken <> ttNone then
        begin
          if initialToken = ttPROGRAM then
            FMainProg.ProgramType := ptProgram
          else
            FMainProg.ProgramType := ptLibrary;
          UnitSection := secProgram;
          if not FTok.TestName then
            FMsgs.AddCompilerError(FTok.hotPos, CPE_NameExpected)
          else
            FTok.KillToken;
          ReadSemiColon;
        end
        else
        begin
          FMainProg.ProgramType := ptScript;
        end;
      end;
    end;

    if Assigned(FOnReadScript) then
      FOnReadScript(Self, sourceFile, scriptType);

    case scriptType of
      stMain:
        begin
          HandleUnitDependencies(scriptType);
        end;
      stUnit:
        begin
          FUnitSection := secHeader;
          scriptType := ReadUnitHeader;
        end;
    end;

    result := ReadRootBlock([], finalToken);

    case scriptType of
      stUnit:
        begin
          if finalToken in [ttNone, ttEND, ttINITIALIZATION, ttFINALIZATION]
          then
          begin
            if coContextMap in options then
              FSourceContextMap.CloseAllContexts(FTok.CurrentPos);
          end;
          CurrentUnitSymbol.AddInitializationExpr(result);
          result := nil;
        end;
      stMain, stRecompile:
        begin
          if coContextMap in options then
          begin
            FSourceContextMap.CloseAllContexts(FTok.CurrentPos);
          end;
        end;
      stUnitNamespace:
        begin
          if (finalToken = ttNone) or (finalToken = ttEND) then
          begin
            if coContextMap in options then
              FSourceContextMap.CloseAllContexts(FTok.CurrentPos);
          end;
          for i := 0 to CurrentUnitSymbol.table.Count - 1 do
          begin
            if CurrentUnitSymbol.table[i] is TUnitSymbol then
            begin
              UnitSymbol := TUnitSymbol(CurrentUnitSymbol.table[i]);
              if UnitSymbol.Main <> nil then
                CurrentSourceUnit.GetDependencies.Add(UnitSymbol.name);
            end;
          end;
          OrphanAndNil(result);
        end;
    end;

    if FTok.ConditionalDepth.Count > 0 then
      FMsgs.AddCompilerError(FTok.ConditionalDepth.Peek.scriptPos,
        CPE_UnbalancedConditionalDirective);

    if finalToken = ttIMPLEMENTATION then
    begin
      RecordSymbolUse(CurrentUnitSymbol, FTok.hotPos,
        [suImplementation, suImplicit]);
      if readingMain then
      begin
        if ReadImplementationBlock <> ttEND then
        begin
          unitBlock := ReadRootBlock([], finalToken);
          CurrentProg.initExpr.AddStatement(unitBlock);
        end;
        FLineCount := FLineCount + FTok.CurrentPos.Line - 2;
        FTok.Free;
      end
      else
      begin
        FUnitContextStack.PushContext(Self);
      end;
    end
    else
    begin
      FLineCount := FLineCount + FTok.CurrentPos.Line - 2;
      FTok.Free;
    end;
    FTok := nil;

    if (result <> nil) and Optimize then
      result := result.Optimize(FCompilerContext);
  finally
    FTok.Free;
    FTok := oldTok;
    FUnitSection := oldSection;
  end;
end;

// ReadImplementationBlock
//
function TdwsCompiler.ReadImplementationBlock: TTokenType;
var
  unitBlock: TBlockExpr;
  initializationBlock, finalizationBlock: TBlockExpr;
begin
  initializationBlock := nil;
  finalizationBlock := nil;
  unitBlock := ReadRootBlock([ttINITIALIZATION, ttFINALIZATION], result);
  try
    if result = ttINITIALIZATION then
    begin
      FUnitSection := secInitialization;
      if coContextMap in options then
        FSourceContextMap.OpenContext(FTok.hotPos, CurrentUnitSymbol,
          ttINITIALIZATION);
      initializationBlock := ReadRootBlock([ttFINALIZATION, ttEND], result);
      if coContextMap in options then
        FSourceContextMap.CloseContext(FTok.hotPos);
    end;
    if result = ttFINALIZATION then
    begin
      FUnitSection := secFinalization;
      if coContextMap in options then
        FSourceContextMap.OpenContext(FTok.hotPos, CurrentUnitSymbol,
          ttFINALIZATION);
      finalizationBlock := ReadRootBlock([ttEND], result);
      if coContextMap in options then
        FSourceContextMap.CloseContext(FTok.hotPos);
    end;
    if (result = ttEND) and (FUnitSection <> secEnd) then
    begin
      FUnitSection := secEnd;
      if not FTok.TestDelete(ttDOT) then
        FMsgs.AddCompilerWarning(FTok.hotPos, CPE_DotExpected);
    end;

    if coContextMap in options then
      FSourceContextMap.CloseAllContexts(FTok.CurrentPos);

    if unitBlock.StatementCount > 0 then
    begin
      CurrentProg.initExpr.AddStatement(unitBlock);
      unitBlock := nil;
    end;

    if coOptimize in options then
    begin
      if (initializationBlock <> nil) and
        (initializationBlock.StatementCount = 0) then
        OrphanAndNil(initializationBlock);
      if (finalizationBlock <> nil) and (finalizationBlock.StatementCount = 0)
      then
        OrphanAndNil(finalizationBlock);
    end;

    Assert(CurrentUnitSymbol <> nil);

    // this is a normal unit
    if initializationBlock <> nil then
    begin
      CurrentUnitSymbol.AddInitializationExpr(initializationBlock);
      initializationBlock := nil;
    end;
    if finalizationBlock <> nil then
    begin
      CurrentUnitSymbol.FinalizationExpr := finalizationBlock;
      finalizationBlock := nil;
    end;
  finally
    OrphanAndNil(unitBlock);
    OrphanAndNil(initializationBlock);
    OrphanAndNil(finalizationBlock);
  end;
end;

// ReadScriptImplementations
//
procedure TdwsCompiler.ReadScriptImplementations;
var
  implemTable: TUnitImplementationTable;
  oldUnit: TSourceUnit;
begin
  while FUnitContextStack.Count > 0 do
  begin
    FUnitContextStack.PopContext(Self, oldUnit);
    FUnitSection := secImplementation;
    try
      implemTable := TUnitImplementationTable.Create(CurrentUnitSymbol);
      CurrentProg.EnterSubTable(implemTable);
      try
        ReadImplementationBlock;
      finally
        CurrentProg.LeaveSubTable;
      end;
      FLineCount := FLineCount + FTok.CurrentPos.Line - 2;
    finally
      FTok.Free;
      FTok := nil;
      LeaveUnit(oldUnit);
    end;
  end;
end;

// ReadRootStatement
//
function TdwsCompiler.ReadRootStatement(var action: TdwsStatementAction;
  initVarBlockExpr: TBlockExpr): TProgramExpr;
var
  hotPos: TScriptPos;
  token: TTokenType;
  rootBlock: Boolean;
begin
  action := saNone;
  result := nil;

  FTok.TestName;
  hotPos := FTok.hotPos;

  token := FTok.TestDeleteAny([ttTYPE, ttPROCEDURE, ttFUNCTION, ttCONSTRUCTOR,
    ttDESTRUCTOR, ttMETHOD, ttCLASS, ttUSES, ttIMPLEMENTATION, ttEND, ttASYNC]);
  case token of
    ttTYPE:
      begin
        if UnitSection in [secInterface, secImplementation] then
          ReadTypeDeclBlock
        else
          ReadTypeDecl(True);
        action := saNoSemiColon
      end;
    ttASYNC:
      ReadProcBody(ReadProcDeclAsync(hotPos));
    ttPROCEDURE, ttFUNCTION, ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD:
      ReadProcBody(ReadProcDecl(token, hotPos));
    ttCLASS:
      begin
        token := FTok.TestDeleteAny([ttPROCEDURE, ttFUNCTION, ttMETHOD]);
        case token of
          ttPROCEDURE, ttFUNCTION, ttMETHOD:
            ReadProcBody(ReadProcDecl(token, hotPos, [pdoClassMethod]));
        else
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_ProcOrFuncExpected);
        end;
      end;
    ttUSES:
      ReadUses;
    ttIMPLEMENTATION:
      begin
        if (CurrentProg.table <> CurrentProg.Root.table) or
          (UnitSection <> secInterface) then
        begin
          FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_UnexpectedSection,
            [cTokenStrings[token]]);
          action := saNoSemiColon;
        end
        else
        begin
          if coContextMap in FOptions then
          begin
            if coContextMap in options then
              FSourceContextMap.CloseContext(FTok.hotPos, ttINTERFACE);
            FSourceContextMap.OpenContext(FTok.hotPos, CurrentUnitSymbol,
              ttIMPLEMENTATION);
          end;
          FUnitSection := secImplementation;
          DoSectionChanged;
          action := saImplementation;
        end;
      end;
    ttEND:
      begin
        if (CurrentProg.table <> CurrentProg.Root.table) or
          (UnitSection <> secImplementation) then
          FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_UnexpectedEnd,
            [cTokenStrings[token]])
        else
        begin
          if FTok.TestDelete(ttDOT) then
          begin
            FUnitSection := secEnd;
            action := saEnd;
          end
          else
            FMsgs.AddCompilerError(FTok.hotPos, CPE_DotExpected);
        end;
      end;
  else
    rootBlock := FTok.Test(ttBEGIN);
    result := ReadStatement(action, initVarBlockExpr);
    if rootBlock and (CurrentProg.level = 0) and FTok.TestDelete(ttDOT) then
      action := saEnd;
  end;
end;

// ReadStatement
//
function TdwsCompiler.ReadStatement(var action: TdwsStatementAction;
  initVarBlockExpr: TBlockExpr): TProgramExpr;
var
  token: TTokenType;
begin
  result := nil;
  token := FTok.TestDeleteAny([ttVAR, ttCONST, ttOPERATOR, ttRESOURCESTRING]);
  case token of
    ttVAR:
      ReadVarDeclBlock(action, initVarBlockExpr);
    ttCONST:
      ReadConstDeclBlock(action);
    ttRESOURCESTRING:
      ReadResourceStringDeclBlock(action);
    ttOPERATOR:
      ReadOperatorDecl;
  else
    if CurrentProg.level = 0 then
    begin
      case UnitSection of
        secMixed, secInitialization, secFinalization:
          ;
        secProgram:
          UnitSection := secMixed;
      else
        FMsgs.AddCompilerError(FTok.hotPos, CPE_UnexpectedStatement);
      end;
    end;
    result := ReadBlock
  end;
end;

// ReadResourceStringDecl
//
function TdwsCompiler.ReadResourceStringDecl: TResourceStringSymbol;
var
  name, buf: String;
  namePos: TScriptPos;
  expr: TTypedExpr;
begin
  if not FTok.TestDeleteNamePos(name, namePos) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

  CheckName(name, namePos);
  CheckSpecialName(name);

  if not FTok.TestDelete(ttEQ) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_EqualityExpected);

  expr := ReadExpr;
  try
    if not expr.IsConstant then
    begin
      FMsgs.AddCompilerError(FTok.hotPos, CPE_ConstantExpressionExpected);
      OrphanAndNil(expr);
    end
    else if (expr.typ = nil) or not expr.typ.IsOfType(FCompilerContext.TypString)
    then
    begin
      FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected);
      OrphanAndNil(expr);
    end;
    // keep compiling
    if expr = nil then
      result := TResourceStringSymbol.Create(name, '')
    else
    begin
      expr.EvalAsString(FExec, buf);
      result := TResourceStringSymbol.Create(name, buf);
    end;
    FMainProg.ResourceStringList.Add(result);
    RecordSymbolUse(result, namePos, [suDeclaration]);
  finally
    OrphanAndNil(expr);
  end;
end;

// ReadResourceStringDeclBlock
//
procedure TdwsCompiler.ReadResourceStringDeclBlock
  (var action: TdwsStatementAction);
var
  resStringSym: TResourceStringSymbol;
begin
  action := saNoSemiColon;
  if coContextMap in FOptions then
    FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttRESOURCESTRING);
  try
    repeat
      resStringSym := ReadResourceStringDecl;
      CurrentProg.table.AddSymbol(resStringSym);
      ReadSemiColon;
    until not((UnitSection in [secInterface, secImplementation]) and
      (CurrentProg.level = 0) and FTok.TestName);
  finally
    if coContextMap in FOptions then
      FSourceContextMap.CloseContext(FTok.CurrentPos, ttRESOURCESTRING);
  end;
end;

// ReadVarDeclBlock
//
procedure TdwsCompiler.ReadVarDeclBlock(var action: TdwsStatementAction;
  initVarBlockExpr: TBlockExprBase);
begin
  action := saNoSemiColon;
  repeat
    ReadVarDecl(FStandardDataSymbolFactory, initVarBlockExpr);
    ReadSemiColon;
  until not((UnitSection in [secProgram, secInterface, secImplementation]) and
    (CurrentProg.level = 0) and FTok.TestName);
end;

// ReadVarDecl
//
procedure TdwsCompiler.ReadVarDecl(const dataSymbolFactory
  : IdwsDataSymbolFactory; initVarBlockExpr: TBlockExprBase);
var
  names, externalNames: TSimpleStringList;
  posArray: TScriptPosArray;
begin
  names := FStringListPool.Acquire;
  externalNames := FStringListPool.Acquire;
  try
    ReadNameList(names, posArray, [], externalNames);
    ReadNamedVarsDecl(names, externalNames, posArray, dataSymbolFactory,
      initVarBlockExpr);
  finally
    FStringListPool.Release(externalNames);
    FStringListPool.Release(names);
  end;
end;

// ReadNamedVarsDecl
//
procedure TdwsCompiler.ReadNamedVarsDecl(names, externalNames
  : TSimpleStringList; const posArray: TScriptPosArray;
  const dataSymbolFactory: IdwsDataSymbolFactory;
  initVarBlockExpr: TBlockExprBase);
var
  x: Integer;
  sym: TDataSymbol;
  typ: TTypeSymbol;
  hotPos: TScriptPos;
  initExpr: TTypedExpr;
  assignExpr: TProgramExpr;
  externalName: String;
begin
  initExpr := nil;
  try
    hotPos := FTok.hotPos;

    if FTok.TestDelete(ttCOLON) then
    begin

      // explicit typing
      // var myVar : type
      // var myVar : type = expr
      // var myVar : type := expr
      typ := ReadType('', tcVariable);
      if names.Count = 1 then
      begin
        if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then
          initExpr := dataSymbolFactory.ReadInitExpr(typ);
      end;

    end
    else if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then
    begin

      // inferred typing
      // var myVar = expr
      // var myVar := expr
      if names.Count <> 1 then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_ColonExpected);
      initExpr := dataSymbolFactory.ReadExpr(nil);
      if initExpr <> nil then
      begin
        typ := initExpr.typ;
        RecordSymbolUseImplicitReference(typ, hotPos, False);
      end
      else
        typ := nil;

      if typ = nil then
      begin
        FMsgs.AddCompilerError(hotPos, CPE_RightSideNeedsReturnType);
        OrphanAndNil(initExpr);
      end
      else if typ = FCompilerContext.TypNil then
        if not(initExpr is TBogusConstExpr) then
          FMsgs.AddCompilerError(hotPos, CPE_TypeCouldNotBeInferenced);

    end
    else
    begin

      // keep going
      typ := FCompilerContext.TypVariant;
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ColonExpected);

    end;

    // keep going in case of error
    if typ = nil then
      typ := FCompilerContext.TypVariant
    else if (typ.ClassType = TClassSymbol) and TClassSymbol(typ).IsStatic then
      FMsgs.AddCompilerErrorFmt(hotPos, CPE_ClassIsStaticNoInstances,
        [typ.name]);

    for x := 0 to names.Count - 1 do
    begin
      if externalNames <> nil then
        externalName := externalNames[x];
      assignExpr := CreateNamedVarDeclExpr(dataSymbolFactory, names[x],
        externalName, posArray[x], typ, initExpr, sym);
      if assignExpr <> nil then
        initVarBlockExpr.AddStatement(assignExpr);
    end;
  finally
    OrphanAndNil(initExpr);
  end;
end;

// CreateNamedVarDeclExpr
//
function TdwsCompiler.CreateNamedVarDeclExpr(const dataSymbolFactory
  : IdwsDataSymbolFactory; const name, externalName: String;
  const scriptPos: TScriptPos; typ: TTypeSymbol; var initExpr: TTypedExpr;
  var sym: TDataSymbol): TProgramExpr;
var
  initData: IDataContext;
  assignExpr: TAssignExpr;
  constExpr: TConstExpr;
  varExpr: TVarExpr;
begin
  result := nil;

  sym := dataSymbolFactory.CreateDataSymbol(name, externalName, scriptPos, typ);

  varExpr := GetVarExpr(scriptPos, sym);
  if Assigned(initExpr) then
  begin

    // Initialize with an expression
    RecordSymbolUse(sym, scriptPos, [suDeclaration, suReference, suWrite]);

    if (externalName <> '') and (sym is TClassVarSymbol) then
      FMsgs.AddCompilerError(scriptPos,
        CPE_ExternalClassVariablesInitializationIsNotSupported);

{$IFNDEF COALESCE_VAR_INITIALIZATION}
    CurrentProg.initExpr.AddStatement(TInitDataExpr.Create(FCompilerContext,
      scriptPos, varExpr));
    varExpr.IncRefCount;
{$ENDIF}
    result := CreateAssign(scriptPos, ttASSIGN, varExpr, initExpr);
    initExpr := nil;

  end
  else
  begin

    RecordSymbolUse(sym, scriptPos, [suDeclaration]);

    if sym.typ.DynamicInitialization then
    begin

      CurrentProg.initExpr.AddStatement(TInitDataExpr.Create(FCompilerContext,
        scriptPos, varExpr));

    end
    else
    begin

      // Initialize with default value
      if (varExpr.typ = FCompilerContext.TypInteger) or
        (varExpr.typ is TEnumerationSymbol) then
        assignExpr := TAssignConstToIntegerVarExpr.CreateVal(FCompilerContext,
          scriptPos, varExpr, 0)
      else if varExpr.typ = FCompilerContext.TypFloat then
        assignExpr := TAssignConstToFloatVarExpr.CreateVal(FCompilerContext,
          scriptPos, varExpr, 0)
      else if varExpr.typ = FCompilerContext.TypBoolean then
        assignExpr := TAssignConstToBoolVarExpr.CreateVal(FCompilerContext,
          scriptPos, varExpr, False)
      else if varExpr.typ = FCompilerContext.TypString then
        assignExpr := TAssignConstToStringVarExpr.CreateVal(FCompilerContext,
          scriptPos, varExpr, '')
      else if varExpr.typ = FCompilerContext.TypVariant then
        assignExpr := TAssignConstToVariantVarExpr.CreateVal(FCompilerContext,
          scriptPos, varExpr, Unassigned)
      else if varExpr.typ.ClassType = TClassSymbol then
        assignExpr := TAssignNilToVarExpr.CreateVal(FCompilerContext,
          scriptPos, varExpr)
      else if varExpr.typ.ClassType = TClassOfSymbol then
        assignExpr := TAssignNilClassToVarExpr.CreateVal(FCompilerContext,
          scriptPos, varExpr)
      else if varExpr.typ.AsFuncSymbol <> nil then
        assignExpr := TAssignNilToVarExpr.CreateVal(FCompilerContext,
          scriptPos, varExpr)
      else
      begin
        initData := TDataContext.CreateStandalone(sym.typ.Size);
        sym.typ.InitDataContext(initData, 0);

        constExpr := TConstExpr.CreateData(scriptPos, sym.typ, initData);
        assignExpr := TAssignConstDataToVarExpr.Create(FCompilerContext,
          scriptPos, varExpr, constExpr);
      end;
      CurrentProg.initExpr.AddStatement(assignExpr);

    end;

  end;
end;

// ReadConstSymbol
//
function TdwsCompiler.ReadConstSymbol(const name: String;
  const constPos: TScriptPos; typ: TTypeSymbol;
  const factory: IdwsDataSymbolFactory): TConstSymbol;
var
  expr: TTypedExpr;
  dataExpr: TDataExpr;
  sas: TStaticArraySymbol;
  recordData: IDataContext;
  exprPos: TScriptPos;
begin
  if typ is TRecordSymbol then
  begin

    recordData := ReadConstRecord(TRecordSymbol(typ));
    result := factory.CreateConstSymbol(name, constPos, typ, recordData);

  end
  else
  begin

    exprPos := FTok.hotPos;
    if typ is TArraySymbol then
    begin
      case FTok.TestDeleteAny([ttALEFT, ttBLEFT]) of
        ttALEFT:
          expr := factory.ReadArrayConstantExpr(ttARIGHT, typ);
        ttBLEFT:
          expr := factory.ReadArrayConstantExpr(ttBRIGHT, typ);
      else
        expr := factory.ReadExpr(nil);
      end;
    end
    else
      expr := factory.ReadExpr(nil);
    if (expr <> nil) and expr.scriptPos.Defined then
      exprPos := expr.scriptPos;
    try
      if Assigned(typ) then
      begin
        if expr = nil then
        begin
          // keep compiling
          expr := TConvInvalidExpr.Create(FCompilerContext, constPos, nil, typ);
        end
        else if not typ.IsCompatible(expr.typ) then
          expr := CompilerUtils.WrapWithImplicitConversion(FCompilerContext,
            expr, typ, exprPos);
      end
      else if expr <> nil then
      begin
        typ := expr.typ;
      end;

      if not expr.IsConstant then
      begin

        if not(expr is TConvInvalidExpr) then
          FMsgs.AddCompilerError(exprPos, CPE_ConstantExpressionExpected);
        // keep compiling
        if typ = nil then
          typ := FCompilerContext.TypVariant;
        result := factory.CreateConstSymbol(name, constPos, typ, nil);

      end
      else
      begin

        if typ is TArraySymbol then
        begin

          if typ is TStaticArraySymbol then
            sas := TStaticArraySymbol(typ)
          else
          begin
            sas := TStaticArraySymbol.Create('', typ.typ,
              FCompilerContext.TypInteger, 0, TArraySymbol(typ).typ.Size - 1);
            CurrentProg.table.AddSymbol(sas);
          end;
          if expr is TConstExpr then
          begin
            result := factory.CreateConstSymbol(name, constPos, sas,
              TConstExpr(expr).DataContext);
          end
          else
          begin
            recordData := TDataContext.CreateStandalone(expr.typ.Size);
            (expr as TArrayConstantExpr).EvalToDataContext(FExec,
              recordData, 0);
            result := factory.CreateConstSymbol(name, constPos, sas,
              recordData);
          end;

        end
        else
        begin

          recordData := TDataContext.CreateStandalone(typ.Size);
          if typ.Size = 1 then
          begin
            expr.EvalAsVariantToDataContext(FExec, recordData, 0);
            result := factory.CreateConstSymbol(name, constPos, typ,
              recordData);
          end
          else
          begin
            dataExpr := (expr as TDataExpr);
            FExec.Stack.Push(CurrentProg.DataSize);
            try
              recordData.WriteData(dataExpr.DataPtr[FExec], typ.Size);
              result := factory.CreateConstSymbol(name, constPos, typ,
                recordData);
            finally
              FExec.Stack.Pop(CurrentProg.DataSize);
            end;
          end;

        end;

      end;

    finally
      OrphanAndNil(expr);
    end;
  end;
  if FTok.Test(ttDEPRECATED) then
    result.deprecatedMessage := ReadDeprecatedMessage(False);
end;

// ReadConstDecl
//
procedure TdwsCompiler.ReadConstDecl(const factory: IdwsDataSymbolFactory);
var
  name: String;
  typ: TTypeSymbol;
  constPos: TScriptPos;
  constSym: TConstSymbol;
begin
  if not FTok.TestDeleteNamePos(name, constPos) then
  begin

    FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

  end
  else
  begin

    factory.CheckName(name, constPos);
    CheckSpecialName(name);

    if FTok.TestDelete(ttCOLON) then
    begin

      typ := ReadType('', tcConstant);
      if typ.AsFuncSymbol <> nil then
        FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_InvalidConstType,
          [typ.Caption]);

    end
    else
      typ := nil;

    if not FTok.TestDelete(ttEQ) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_EqualityExpected);

    constSym := ReadConstSymbol(name, constPos, typ, factory);

    RecordSymbolUse(constSym, constPos, [suDeclaration]);
  end;
end;

// ReadConstDeclBlock
//
procedure TdwsCompiler.ReadConstDeclBlock(var action: TdwsStatementAction);
begin
  action := saNoSemiColon;
  repeat
    ReadConstDecl(FStandardDataSymbolFactory);
    ReadSemiColon;
  until not((UnitSection in [secProgram, secInterface, secImplementation]) and
    (CurrentProg.level = 0) and FTok.TestName);
end;

// ReadTypeDeclBlock
//
procedure TdwsCompiler.ReadTypeDeclBlock;
var
  token: TTokenType;
begin
  token := ttTYPE;
  repeat
    if not ReadTypeDecl(token = ttTYPE) then
      Break;
    token := FTok.TestAny([ttINTERFACE, ttIMPLEMENTATION, ttINITIALIZATION,
      ttFINALIZATION, ttTYPE, ttVAR, ttCONST, ttEND, ttCLASS, ttFUNCTION,
      ttPROCEDURE, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR, ttOPERATOR]);
  until (not FTok.HasTokens) or (token <> ttNone);
end;

// ReadTypeDecl
//
function TdwsCompiler.ReadTypeDecl(firstInBlock: Boolean): Boolean;
var
  name: String;
  typNew, typOld: TTypeSymbol;
  typePos: TScriptPos;
  oldSymPos: TSymbolPosition; // Mark *where* the old declaration was
  typContext: TdwsSourceContext;
  attributesBag: ISymbolAttributesBag;
  genericParameters: IGenericParameters;
begin
  result := True;

  ReadAttributes(False);

  attributesBag := BagPendingAttributes;

  if not FTok.TestDeleteNamePos(name, typePos) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

  if FTok.TestDelete(ttLESS) then
  begin
    genericParameters := ReadGenericParametersDecl;
    if not FTok.TestDelete(ttGTR) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_GreaterExpected)
  end;

  if not FTok.TestDelete(ttEQ) then
  begin
    if firstInBlock then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_EqualityExpected)
    else
    begin
      FTok.SimulateNameToken(typePos, name);
      Exit(False);
    end;
  end;

  if FSystemTable.FindLocal(name) <> nil then
    FMsgs.AddCompilerErrorFmt(typePos, CPE_NameIsReserved, [name]);

  typOld := CurrentProg.table.FindTypeSymbol(name, cvMagic);
  if Assigned(typOld) and (coSymbolDictionary in FOptions) then
  begin
    oldSymPos := FSymbolDictionary.FindSymbolUsage(typOld, suDeclaration);
    // may be nil
  end
  else
  begin
    oldSymPos := nil;
  end;

  // Wrap whole type declarations in a context.
  if coContextMap in FOptions then
  begin
    FSourceContextMap.OpenContext(typePos, nil, ttNAME);
    typContext := FSourceContextMap.Current;
  end
  else
    typContext := nil;
  try

    if genericParameters <> nil then
      typNew := ReadTypeGenericDecl(name, tcDeclaration, genericParameters)
    else
      typNew := ReadType(name, tcDeclaration);

    if typContext <> nil then
      typContext.ParentSym := typNew;

    AttachBaggedAttributes(typNew, attributesBag);

    if typNew.name <> '' then
    begin
      // typOld = typNew if a forwarded class declaration was overwritten
      if typOld <> typNew then
      begin
        CheckName(name, typePos);
        CheckSpecialName(name);
        if typNew.name <> '' then
          CurrentProg.table.AddSymbol(typNew);
      end
      else
      begin
        // Handle overwriting forwards in Dictionary
        // Original symbol was a forward. Update symbol entry
        // If the type is in the SymbolDictionary (disabled dictionary would leave pointer nil),
        if Assigned(oldSymPos) and not(typOld.InheritsFrom(TClassSymbol) and
          TClassSymbol(typOld).IsPartial) then
        begin
          if FSymbolDictionary.FindSymbolUsage(typOld, suForward) = nil then
            oldSymPos.SymbolUsages := [suForward];
          // update old position to reflect that the type was forwarded
        end;
      end;

      // Add symbol position as being the type being declared (works for forwards too)
      if typNew.IsForwarded then
        RecordSymbolUse(typNew, typePos, [suForward])
      else
        RecordSymbolUse(typNew, typePos, [suDeclaration]);
    end;

    ReadSemiColon;

    typNew.deprecatedMessage := ReadDeprecatedMessage;

  finally
    if coContextMap in FOptions then
      FSourceContextMap.CloseContext(FTok.CurrentPos, ttNAME);
  end;
end;

// ReadProcDecl
//
function TdwsCompiler.ReadProcDecl(funcToken: TTokenType;
  const hotPos: TScriptPos; declOptions: TdwsReadProcDeclOptions = [];
  expectedLambdaParams: TParamsSymbolTable = nil): TFuncSymbol;
var
  funcKind: TFuncKind;
  name: String;
  sym: TSymbol;
  funcPos: TScriptPos;
  compositeSym: TCompositeTypeSymbol;
  overloadFuncSym, existingFuncSym, forwardedSym: TFuncSymbol;
  forwardedSymForParams: TFuncSymbol;
  forwardedSymPos: TSymbolPosition;
  genericSymbol: TGenericSymbol;
  sourceContext: TdwsSourceContext;
  posArray: TScriptPosArray;
  isForward: Boolean;
begin
  result := nil;
  sym := nil;
  isForward := False;

  funcKind := cTokenToFuncKind[funcToken];
  funcPos := hotPos;

  if not(pdoType in declOptions) then
  begin
    // Find existing symbol for function name (if any)
    if pdoAnonymous in declOptions then
    begin
      name := '';
    end
    else
    begin
      if not FTok.TestDeleteNamePos(name, funcPos) then
      begin
        FMsgs.AddCompilerError(FTok.hotPos, CPE_NameExpected);
        name := '';
      end
      else
      begin
        CheckSpecialName(name);
        sym := CurrentProg.table.FindSymbol(name, cvMagic);
      end;
    end;
  end
  else
  begin
    name := '';
  end;

  // Open context. Closed in ReadProcBody.
  if coContextMap in options then
  begin
    FSourceContextMap.OpenContext(hotPos, nil, funcToken);
    sourceContext := FSourceContextMap.Current;
  end
  else
    sourceContext := nil;

  if sym is TGenericSymbol then
  begin
    genericSymbol := TGenericSymbol(sym);
    CheckGenericParameters(genericSymbol);
    sym := genericSymbol.genericType;
  end
  else
    genericSymbol := nil;

  // name is the name of composite type -> this is a method implementation
  if sym is TCompositeTypeSymbol then
  begin

    compositeSym := TCompositeTypeSymbol(sym);
    if compositeSym.IsPartial or (compositeSym.UnitSymbol = CurrentUnitSymbol)
    then
    begin

      if CurrentProg.level <> 0 then
        FMsgs.AddCompilerStop(hotPos, CPE_UnexpectedMethodImplementation);

      // Store reference to class in dictionary
      RecordSymbolUse(sym, funcPos, [suReference]);

      if genericSymbol <> nil then
        EnterGeneric(genericSymbol);
      try
        var
          createOptions: TMethodCreateOptions := [];
        if pdoClassMethod in declOptions then
          Include(createOptions, mcoClassMethod);
        if pdoHelperMethod in declOptions then
          Include(createOptions, mcoHelperMethod);
        result := ReadMethodImpl(compositeSym, funcKind, createOptions);
      finally
        if genericSymbol <> nil then
          LeaveGeneric;
      end;

    end;

  end;

  if result = nil then
  begin

    // Read normal procedure/function declaration
    if (pdoClassMethod in declOptions) or
      (funcKind in [fkConstructor, fkDestructor, fkMethod]) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ImplClassNameExpected);

    forwardedSym := nil;
    overloadFuncSym := nil;
    existingFuncSym := sym.AsFuncSymbol;
    if existingFuncSym <> nil then
    begin
      if existingFuncSym.IsOverloaded then
        overloadFuncSym := existingFuncSym;
      if existingFuncSym.IsForwarded and
        (CurrentUnitSymbol.HasSymbol(sym) or CurrentProg.table.HasSymbol(sym))
      then
      begin
        // There was already a (forward) declaration
        forwardedSym := existingFuncSym;
      end;
    end;

    if (forwardedSym = nil) and (overloadFuncSym = nil) then
      CheckName(name, funcPos);

    if pdoType in declOptions then
      result := TSourceFuncSymbol.Create('', funcKind, -1)
    else
      result := TSourceFuncSymbol.Create(name, funcKind,
        FMainProg.NextStackLevel(CurrentProg.level));
    try
      if funcToken = ttLAMBDA then
        result.IsLambda := True;

      // Don't add params to dictionary when function is forwarded. It is already declared.
      forwardedSymForParams := forwardedSym;
      if forwardedSym <> nil then
        ReadParams(result.HasParam, result.AddParam, forwardedSym.Params,
          nil, posArray)
      else
        ReadParams(result.HasParam, result.AddParam, nil, expectedLambdaParams,
          posArray);

      if (funcToken <> ttLAMBDA) or FTok.Test(ttCOLON) then
        result.typ := ReadFuncResultType(funcKind);

      if not(pdoAnonymous in declOptions) then
      begin

        if pdoType in declOptions then
        begin

          if FTok.TestDelete(ttOF) then
          begin
            if FTok.TestDelete(ttOBJECT) then
            begin
              if not(coDelphiDialect in CompilerContext.options) then
                FMsgs.AddCompilerHint(FTok.hotPos, CPH_OfObjectIsLegacy,
                  hlPedantic);
              result.IsOfObject := True;
            end
            else
              FMsgs.AddCompilerError(FTok.hotPos, CPE_OfObjectExpected);
          end;
          if pdoReferenceTo in declOptions then
            result.IsReferenceTo := True;
          ReadProcCallQualifiers(result);

        end
        else
        begin

          if not FTok.TestDelete(ttSEMI) then
            FMsgs.AddCompilerWarning(FTok.hotPos, CPE_SemiExpected);

          if overloadFuncSym <> nil then
            forwardedSym := FuncPerfectMatchOverload(result);

          // handle function overloading
          if FTok.TestDelete(ttOVERLOAD) then
          begin

            if FuncHasConflictingOverload(result, forwardedSym) then
              FMsgs.AddCompilerErrorFmt(hotPos, CPE_MatchingOverload, [name]);

            result.IsOverloaded := True;
            ReadSemiColon;

          end
          else if overloadFuncSym <> nil then
          begin

            // nested funcs are allowed to overwrite overloads without errors
            if result.level <= existingFuncSym.level then
            begin
              forwardedSym := FuncPerfectMatchOverload(result);
              if forwardedSym = nil then
              begin
                // no match, possible name conflict or fogotten overload keyword
                FMsgs.AddCompilerErrorFmt(hotPos,
                  CPE_MustExplicitOverloads, [name]);
                // keep compiling, mark overloaded
                result.IsOverloaded := True;
              end;
            end;

          end;

          if Assigned(forwardedSym) then
          begin

            // check forward symbol match

            CompareFuncKinds(forwardedSym.Kind, result.Kind);
            CompareFuncSymbolParams(forwardedSym, result);

          end
          else
          begin

            // forward, external, export & helper declarations

            if FTok.TestDelete(ttEXTERNAL) then
            begin
              ReadExternalName(result);
              if Assigned(FExternalRoutinesManager) then
                result := FExternalRoutinesManager.ConvertToMagicSymbol(result);
              result.IsExternal := True;
              if FTok.TestDelete(ttPROPERTY) then
                ReadProcDeclProperty(result, FTok.hotPos);
              ReadSemiColon;
            end;

            if UnitSection = secInterface then
            begin
              // default to forward in interface section, except for external funcs
              isForward := True;
              if not result.IsExternal then
                result.SetForwardedPos(funcPos);
              if FTok.TestDelete(ttFORWARD) then
              begin
                FMsgs.AddCompilerHint(FTok.hotPos, CPW_ForwardIsImplicit);
                ReadSemiColon;
              end;
            end
            else
            begin
              if FTok.TestDelete(ttFORWARD) then
              begin
                isForward := True;
                if result.IsExternal then
                  FMsgs.AddCompilerHint(FTok.hotPos, CPW_ForwardIsMeaningless);
                result.SetForwardedPos(funcPos);
                ReadSemiColon;
              end;
            end;

            if FTok.TestDelete(ttEXPORT) then
            begin
              result.IsExport := True;
              if FTok.Test(ttStrVal) then
              begin
                result.externalName := FTok.GetToken.AsString;
                FTok.KillToken;
              end;
              ReadSemiColon;
            end;

            // helper anonymous declaration

            if FTok.TestDelete(ttHELPER) then
              AddProcHelper(result);

          end;

          ReadProcCallQualifiers(result);

          if FTok.TestDelete(ttINLINE) then
          begin
            result.SetInline;
            ReadSemiColon;
          end;

          if FTok.Test(ttDEPRECATED) then
            result.deprecatedMessage := ReadDeprecatedMessage;

          if Assigned(forwardedSym) then
          begin
            // Get forwarded position in script. If compiled without symbols it will just return a nil
            forwardedSymPos := FSymbolDictionary.FindSymbolUsage(forwardedSym,
              suDeclaration); // may be nil

            // Adapt dictionary entry to reflect that it was a forward
            // If the record is in the SymbolDictionary (disabled dictionary would leave pointer nil)
            if Assigned(forwardedSymPos) then
              forwardedSymPos.SymbolUsages := [suDeclaration, suForward];
            // update old position to reflect that the type was forwarded

            if forwardedSymForParams <> nil then
              AdaptParametersSymPos(forwardedSymForParams, forwardedSym,
                [suReference], posArray)
            else
              AdaptParametersSymPos(result, forwardedSym, [suReference],
                posArray);

            SymbolDictionary.Remove(result);

            OrphanObject(result);
            result := forwardedSym;
            result.ClearIsForwarded;

          end
          else
          begin

            if forwardedSymForParams <> nil then
              AdaptParametersSymPos(forwardedSymForParams, result,
                [suDeclaration], posArray);

            CurrentProg.table.AddSymbol(result);

          end;

          if result.IsForwarded or result.IsExternal then
            FTok.SimulateToken(ttSEMI, FTok.hotPos);
        end;

      end;

      if result.IsLambda then
        RecordSymbolUse(result, funcPos, [suDeclaration, suImplementation,
          suReference])
      else if isForward then
        RecordSymbolUse(result, funcPos, [suDeclaration, suForward])
      else if forwardedSym = nil then
      begin
        if result.name = '' then
          RecordSymbolUse(result, funcPos, [suDeclaration, suImplementation,
            suReference, suImplicit])
        else
          RecordSymbolUse(result, funcPos, [suDeclaration, suImplementation]);
      end
      else
        RecordSymbolUse(result, funcPos, [suImplementation]);
    except
      OrphanObject(result);
      raise;
    end;
  end;

  if sourceContext <> nil then
    sourceContext.ParentSym := result;
end;

// AdaptParametersSymPos
//
procedure TdwsCompiler.AdaptParametersSymPos(guess, actual: TFuncSymbol;
  const useTypes: TSymbolUsages; var posArray: TScriptPosArray);
var
  i, d: Integer;
  guessSymPosList: TSymbolPositionList;
  guessParam: TSymbol;
  symPos: TSymbolPosition;
begin
  if not(coSymbolDictionary in options) then
    Exit;

  d := actual.Params.Count - Length(posArray);
  // params can have been mislocated in guess and must be reassigned to actual
  for i := 0 to actual.Params.Count - 1 do
  begin
    // note: d can be negative in case of syntax errors
    // f.i. when more parameters were specified than were declared
    // so we can't use d as lower bound for the loop
    if i < d then
      continue;
    RecordSymbolUse(actual.Params[i], posArray[i - d], useTypes);
    guessParam := guess.Params.FindLocal(actual.Params[i].name);
    if guessParam <> nil then
    begin
      guessSymPosList := SymbolDictionary.FindSymbolPosList(guessParam);
      if guessSymPosList <> nil then
      begin
        symPos := guessSymPosList.Items[guessSymPosList.Count - 1];
        if symPos.scriptPos.SamePosAs(posArray[i - d]) then
          guessSymPosList.Delete(guessSymPosList.Count - 1);
      end;
    end;
  end;
  d := guess.Params.Count;
  if d > Length(posArray) then
    d := Length(posArray);
  for i := actual.Params.Count to d - 1 do
  begin
    guessParam := guess.Params.FindLocal(guess.Params[i].name);
    if guessParam <> nil then
    begin
      guessSymPosList := SymbolDictionary.FindSymbolPosList(guessParam);
      if guessSymPosList <> nil then
      begin
        symPos := guessSymPosList.Items[guessSymPosList.Count - 1];
        if symPos.scriptPos.SamePosAs(posArray[i]) then
          guessSymPosList.Delete(guessSymPosList.Count - 1);
      end;
    end;
  end;
end;

// ReadProcDeclAsync
//
function TdwsCompiler.ReadProcDeclAsync(const hotPos: TScriptPos): TFuncSymbol;
var
  token: TTokenType;
begin
  if not(coAllowAsyncAwait in FCompilerContext.options) then
    FMsgs.AddCompilerError(hotPos, CPE_AsyncNotSupported);
  token := FTok.TestDeleteAny([ttPROCEDURE, ttFUNCTION]);
  case token of
    ttPROCEDURE, ttFUNCTION:
      begin
        result := ReadProcDecl(token, hotPos);
        result.IsAsync := True;
      end;
  else
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_ProcOrFuncExpected);
    result := nil;
  end;
end;

// ReadProcDeclProperty
//
procedure TdwsCompiler.ReadProcDeclProperty(funcSymbol: TFuncSymbol;
  const hotPos: TScriptPos);
begin
  funcSymbol.IsProperty := True;
  if funcSymbol.typ = nil then
    FMsgs.AddCompilerError(hotPos, CPE_ExternalPropertyNoType);
  if funcSymbol.Params.Count > 1 then
    FMsgs.AddCompilerError(hotPos, CPE_ExternalPropertyNoArguments);
end;

// ReadIntfMethodDecl
//
function TdwsCompiler.ReadIntfMethodDecl(intfSym: TInterfaceSymbol;
  funcKind: TFuncKind): TSourceMethodSymbol;
var
  name: String;
  sym: TSymbol;
  methPos: TScriptPos;
  posArray: TScriptPosArray;
begin
  // Find Symbol for Functionname
  if not FTok.TestDeleteNamePos(name, methPos) then
  begin
    FMsgs.AddCompilerError(FTok.hotPos, CPE_NameExpected);
    name := '';
  end;

  // Check if name is already used
  sym := intfSym.Members.FindSymbolFromScope(name, intfSym);
  if sym <> nil then
    MemberSymbolWithNameAlreadyExists(sym, methPos);

  // Read declaration of method
  result := TSourceMethodSymbol.Create(name, funcKind, intfSym,
    cvPublished, []);
  result.DeclarationPosition := methPos;

  try
    ReadParams(result.HasParam, result.AddParam, nil, nil, posArray);

    result.typ := ReadFuncResultType(funcKind);
    ReadSemiColon;

    // Added as last step. OnExcept, won't need to be freed.
    RecordSymbolUse(result, methPos, [suDeclaration]);
  except
    OrphanObject(result);
    raise;
  end;
end;

// ReadMethodDecl
//
function TdwsCompiler.ReadMethodDecl(const hotPos: TScriptPos;
  ownerSym: TCompositeTypeSymbol; funcKind: TFuncKind;
  aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
  : TMethodSymbol;

  function OverrideParamsCheck(newMeth, oldMeth: TMethodSymbol): Boolean;
  var
    i: Integer;
    oldParam, newParam: TSymbol;
  begin
    if newMeth.Params.Count <> oldMeth.Params.Count then
      Exit(False);
    for i := 0 to newMeth.Params.Count - 1 do
    begin
      newParam := newMeth.Params[i];
      oldParam := oldMeth.Params[i];
      if (not newParam.typ.IsOfType(oldParam.typ)) or
        (not UnicodeSameText(newParam.name, oldParam.name)) then
        Exit(False);
    end;
    result := True;
  end;

var
  name: String;
  sym: TSymbol;
  meth, defaultConstructor: TMethodSymbol;
  isReintroduced: Boolean;
  methPos: TScriptPos;
  qualifier: TTokenType;
  funcResult: TSourceMethodSymbol;
  bodyToken: TTokenType;
  posArray: TScriptPosArray;
begin
  // Find Symbol for Functionname
  if not FTok.TestDeleteNamePos(name, methPos) then
  begin
    methPos := FTok.hotPos;
    FMsgs.AddCompilerError(methPos, CPE_NameExpected);
    name := '';
  end;

  // Check if name is already used
  sym := ownerSym.Members.FindSymbolFromScope(name, ownerSym);
  if (sym <> nil) then
  begin
    if sym is TMethodSymbol then
    begin
      meth := TMethodSymbol(sym);
    end
    else
    begin
      meth := nil;
      if ownerSym.Members.HasSymbol(sym) then
        MemberSymbolWithNameAlreadyExists(sym, methPos);
    end;
  end
  else
    meth := nil;

  if ownerSym.IsStatic and not(mcoClassMethod in aCreateOptions) then
    FMsgs.AddCompilerErrorFmt(methPos, CPE_ClassIsStaticNoInstances,
      [ownerSym.name]);

  // Read declaration of method implementation
  funcResult := TSourceMethodSymbol.Create(name, funcKind, ownerSym,
    aVisibility, aCreateOptions);
  funcResult.DeclarationPosition := methPos;
  try
    if FGenericSymbol.Count > 0 then
      funcResult.InternalParams.InsertParent(0,
        FGenericSymbol.Peek.Parameters.List);

    ReadParams(funcResult.HasParam, funcResult.AddParam, nil, nil, posArray);

    funcResult.typ := ReadFuncResultType(funcKind);
    ReadSemiColon;

    if meth <> nil then
      isReintroduced := meth.IsVirtual
    else
      isReintroduced := False;

    if FTok.TestDelete(ttREINTRODUCE) then
    begin
      if not isReintroduced then
        FMsgs.AddCompilerErrorFmt(methPos, CPE_CantReintroduce, [name]);
      isReintroduced := False;
      ReadSemiColon;
    end;

    // handle method overloading
    if FTok.TestDelete(ttOVERLOAD) then
    begin

      if not ownerSym.AllowOverloads then
        FMsgs.AddCompilerError(hotPos, CPE_OverloadNotAllowed);
      if CheckMethodOverloads(funcResult) then
        FMsgs.AddCompilerErrorFmt(hotPos, CPE_MatchingOverload, [name]);

      funcResult.IsOverloaded := True;
      ReadSemiColon;

    end
    else
    begin

      funcResult.SetOverlap(meth);
      if meth <> nil then
      begin
        if FTok.Test(ttOVERRIDE) then
        begin
          // this could actually be an override of an inherited method
          // and not just an overload of a local method
          // in that case 'overload' is optional (but recommand it)
          if meth.IsOverloaded then
          begin
            ReportImplicitMethodOverload(hotPos, name, hlStrict);
            funcResult.IsOverloaded := True;
          end
          else if meth.StructSymbol = ownerSym then
          begin
            // name conflict or fogotten overload keyword
            ReportImplicitMethodOverload(methPos, name);
            // keep compiling, mark overloaded
            funcResult.IsOverloaded := True;
          end;
        end
        else if meth.StructSymbol = ownerSym then
        begin
          // name conflict or fogotten overload keyword
          ReportImplicitMethodOverload(methPos, name);
          // keep compiling, mark overloaded
          funcResult.IsOverloaded := True;
        end
        else if MethPerfectMatchOverload(funcResult, False) <> nil then
          MemberSymbolWithNameAlreadyExists(sym, FTok.hotPos)
      end;

    end;

    if funcResult.IsOverloaded and (meth <> nil) and meth.IsOverloaded then
      isReintroduced := False;

    if ownerSym.AllowVirtualMembers then
    begin
      qualifier := FTok.TestDeleteAny([ttVIRTUAL, ttOVERRIDE, ttABSTRACT]);
      if qualifier <> ttNone then
      begin
        case qualifier of
          ttVIRTUAL:
            begin
              if aVisibility = cvPrivate then
                FMsgs.AddCompilerHint(FTok.hotPos,
                  CPH_PrivateVirtualMethodCantBeOverridden);
              funcResult.IsVirtual := True;
              ReadSemiColon;
              if FTok.TestDelete(ttABSTRACT) then
              begin
                funcResult.IsAbstract := True;
                ReadSemiColon;
              end;
            end;
          ttOVERRIDE:
            begin
              if funcResult.IsOverloaded then
                meth := MethPerfectMatchOverload(funcResult, True);
              if meth = nil then
                FMsgs.AddCompilerErrorFmt(methPos,
                  CPE_CantOverrideNotInherited, [name])
              else
              begin
                RecordSymbolUse(meth, methPos, [suImplicit]);
                if not meth.IsVirtual then
                  FMsgs.AddCompilerErrorFmt(methPos,
                    CPE_CantOverrideNotVirtual, [name])
                else
                begin
                  if funcResult.Kind <> meth.Kind then
                    FMsgs.AddCompilerErrorFmt(FTok.hotPos,
                      CPE_CantOverrideWrongFuncKind,
                      [cFuncKindToString[meth.Kind],
                      cFuncKindToString[funcResult.Kind]])
                  else if funcResult.IsClassMethod <> meth.IsClassMethod then
                    FMsgs.AddCompilerError(FTok.hotPos,
                      CPE_CantOverrideWrongMethodType)
                  else if not funcResult.typ.IsOfType(meth.typ) then
                    FMsgs.AddCompilerError(FTok.hotPos,
                      CPE_CantOverrideWrongResultType)
                  else if not OverrideParamsCheck(funcResult, meth) then
                    FMsgs.AddCompilerError(FTok.hotPos,
                      CPE_CantOverrideWrongParameterList)
                  else if meth.IsFinal then
                    FMsgs.AddCompilerErrorFmt(FTok.hotPos,
                      CPE_CantOverrideFinal, [name]);
                  funcResult.SetOverride(meth);
                  isReintroduced := False;
                end;
              end;
              ReadSemiColon;
            end;
          ttABSTRACT:
            begin
              FMsgs.AddCompilerError(FTok.hotPos, CPE_NonVirtualAbstract);
              ReadSemiColon;
            end;
        end;
      end;
    end;

    if FTok.TestDelete(ttDEFAULT) then
    begin
      if funcKind <> fkConstructor then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_NonConstructorDefault)
      else
      begin
        defaultConstructor := ownerSym.FindDefaultConstructor(cvMagic);
        if (defaultConstructor <> nil) and
          (defaultConstructor.StructSymbol = ownerSym) then
          FMsgs.AddCompilerErrorFmt(FTok.hotPos,
            CPE_DefaultConstructorAlreadyDefined,
            [ownerSym.name, defaultConstructor.name])
        else
          funcResult.IsDefault := True;
      end;
      ReadSemiColon;
    end;

    if ownerSym.AllowVirtualMembers then
    begin
      if FTok.TestDelete(ttFINAL) then
      begin
        if not funcResult.IsOverride then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_CantFinalWithoutOverride)
        else
          funcResult.SetIsFinal;
        ReadSemiColon;
      end;
    end;

    if FTok.TestDelete(ttSTATIC) then
    begin
      if funcResult.IsVirtual or (not funcResult.IsClassMethod) or
        (not(funcKind in [fkFunction, fkProcedure, fkMethod])) then
        FMsgs.AddCompilerError(FTok.hotPos,
          CPE_OnlyNonVirtualClassMethodsAsStatic)
      else
        funcResult.SetIsStatic;
      ReadSemiColon;
    end;

    if FTok.TestDelete(ttEXTERNAL) then
    begin
      if not ownerSym.IsExternal then
        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_StructureIsNotExternal,
          [funcResult.QualifiedName]);
      ReadExternalName(funcResult);
      if FTok.TestDelete(ttPROPERTY) then
        ReadProcDeclProperty(funcResult, FTok.hotPos);
      ReadSemiColon;
    end;

    ReadProcCallQualifiers(funcResult);

    if FTok.Test(ttDEPRECATED) then
      funcResult.deprecatedMessage := ReadDeprecatedMessage;

    if isReintroduced then
      FMsgs.AddCompilerWarningFmt(methPos, CPE_ReintroduceWarning, [name]);

    RecordSymbolUse(funcResult, methPos, [suDeclaration]);

  finally
    ownerSym.AddMethod(funcResult);
  end;

  bodyToken := FTok.TestAny([ttBEGIN, ttREQUIRE, ttEMPTY]);
  if bodyToken <> ttNone then
  begin
    if funcResult.IsAbstract then
      FMsgs.AddCompilerErrorFmt(methPos, CPE_ImplAbstract,
        [ownerSym.name, funcResult.name]);
    case bodyToken of
      ttBEGIN, ttREQUIRE:
        begin
          // inline declaration
          if coContextMap in FOptions then
            FSourceContextMap.OpenContext(hotPos, funcResult, bodyToken);
          ReadProcBody(funcResult);
          ReadSemiColon;
        end;
      ttEMPTY:
        begin
          // empty body
          ReadProcEmpty(funcResult);
        end;
    end;
  end;

  result := funcResult;
end;

// ReadMethodImpl
//
function TdwsCompiler.ReadMethodImpl(ownerSym: TCompositeTypeSymbol;
  funcKind: TFuncKind; aCreateOptions: TMethodCreateOptions): TMethodSymbol;
var
  methName: String;
  sym: TSymbol;
  tmpMeth, overloadedMeth: TMethodSymbol;
  methPos: TScriptPos;
  declaredMethod, explicitParams: Boolean;
  posArray: TScriptPosArray;
begin
  if not FTok.TestDelete(ttDOT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_DotExpected);
  if not FTok.TestDeleteNamePos(methName, methPos) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

  FTok.Test(ttBLEFT);

  sym := ownerSym.Members.FindSymbol(methName, cvPrivate);

  declaredMethod := (sym is TMethodSymbol) and
    (TMethodSymbol(sym).StructSymbol = ownerSym);
  if declaredMethod then
    result := TMethodSymbol(sym)
  else if sym is TCompositeTypeSymbol then
  begin
    // nested type
    result := ReadMethodImpl(TCompositeTypeSymbol(sym), funcKind,
      aCreateOptions);
    Exit;
  end
  else
  begin
    // keep compiling
    FMsgs.AddCompilerErrorFmt(methPos, CPE_ImplInvalidClass,
      [methName, ownerSym.name]);
    result := TSourceMethodSymbol.Create(methName, funcKind, ownerSym, cvPublic,
      aCreateOptions);
    ownerSym.AddMethod(result);
  end;

  explicitParams := not FTok.Test(ttSEMI);
  if declaredMethod then
  begin
    tmpMeth := TSourceMethodSymbol.Create(methName, funcKind, ownerSym,
      TMethodSymbol(result).visibility, aCreateOptions);
    try
      // Don't store these params to Dictionary. They will become invalid when the method is freed.
      if not FTok.TestDelete(ttSEMI) then
      begin
        if ownerSym.HasNestedTypes then
        begin
          // composite has nested types, enable their scope
          // since it is rather costly and nested types are infrequent,
          // only use this code path when necessary
          CurrentProg.EnterSubTable(ownerSym.Members);
          try
            ReadParams(tmpMeth.HasParam, tmpMeth.AddParam, result.Params, nil,
              posArray);
            tmpMeth.typ := ReadFuncResultType(funcKind);
          finally
            CurrentProg.LeaveSubTable;
          end;
        end
        else
        begin
          ReadParams(tmpMeth.HasParam, tmpMeth.AddParam, result.Params, nil,
            posArray);
          tmpMeth.typ := ReadFuncResultType(funcKind);
        end;
        if not FTok.TestDelete(ttSEMI) then
          FMsgs.AddCompilerWarning(FTok.hotPos, CPE_SemiExpected);
      end;

      if result.IsOverloaded then
      begin
        overloadedMeth := MethPerfectMatchOverload(tmpMeth, False);
        if overloadedMeth = nil then
          FMsgs.AddCompilerErrorFmt(methPos, CPE_NoMatchingOverloadDeclaration,
            [tmpMeth.name])
        else
        begin
          AdaptParametersSymPos(result, overloadedMeth, [suReference],
            posArray);
          result := overloadedMeth;
        end;
      end
      else if explicitParams then
      begin
        if result.IsStatic then
          tmpMeth.SetIsStatic;
        CompareFuncSymbolParams(result, tmpMeth);
      end;

    finally
      OrphanObject(tmpMeth);
    end;
  end
  else
  begin
    // keep compiling a method that wasn't declared in class
    if not FTok.TestDelete(ttSEMI) then
    begin
      ReadParams(result.HasParam, result.AddParam, nil, nil, posArray);
      result.typ := ReadFuncResultType(funcKind);
      ReadSemiColon;
    end;
  end;

  if result.IsClassMethod <> (mcoClassMethod in aCreateOptions) then
  begin
    if result.IsClassMethod then
      FMsgs.AddCompilerError(methPos, CPE_ImplClassExpected)
    else
      FMsgs.AddCompilerError(methPos, CPE_ImplNotClassExpected);
  end;

  CompareFuncKinds(result.Kind, funcKind);

  if result.IsAbstract then
    FMsgs.AddCompilerErrorFmt(methPos, CPE_ImplAbstract,
      [ownerSym.name, methName]);

  RecordSymbolUse(result, methPos, [suImplementation]);
end;

// ReadDeprecatedMessage
//
function TdwsCompiler.ReadDeprecatedMessage(withSemiColon
  : Boolean = True): String;
begin
  if FTok.TestDelete(ttDEPRECATED) then
  begin
    if FTok.Test(ttStrVal) then
    begin
      result := FTok.GetToken.AsString;
      FTok.KillToken;
    end;
    if result = '' then
      result := MSG_DeprecatedEmptyMsg;
    if withSemiColon then
      ReadSemiColon;
  end
  else
    result := '';
end;

// WarnDeprecatedFunc
//
procedure TdwsCompiler.WarnDeprecatedFunc(funcExpr: TFuncExprBase);
var
  funcSym: TFuncSymbol;
begin
  funcSym := funcExpr.funcSym;
  if funcSym.IsDeprecated then
    WarnDeprecatedSymbol(funcExpr.scriptPos, funcSym,
      funcSym.deprecatedMessage);
end;

// WarnDeprecatedType
//
procedure TdwsCompiler.WarnDeprecatedType(const scriptPos: TScriptPos;
  typeSymbol: TTypeSymbol);
begin
  if (typeSymbol <> nil) and (typeSymbol.deprecatedMessage <> '') then
    WarnDeprecatedSymbol(scriptPos, typeSymbol, typeSymbol.deprecatedMessage);
end;

// WarnDeprecatedSymbol
//
procedure TdwsCompiler.WarnDeprecatedSymbol(const scriptPos: TScriptPos;
  sym: TSymbol; const deprecatedMessage: String);
begin
  if deprecatedMessage <> MSG_DeprecatedEmptyMsg then
    FMsgs.AddCompilerWarningFmt(scriptPos, CPW_DeprecatedWithMessage,
      [sym.name, deprecatedMessage])
  else
    FMsgs.AddCompilerWarningFmt(scriptPos, CPW_Deprecated, [sym.name]);
end;

// ReadProcBody
//
procedure TdwsCompiler.ReadProcBody(funcSymbol: TFuncSymbol);
var
  oldprog: TdwsProgram;
  proc: TdwsProcedure;
  tt, sectionType, finalToken: TTokenType;
  hotPos: TScriptPos;
  progExpr: TBlockExpr;
begin
  // Stop if declaration was forwarded or external
  if funcSymbol.IsForwarded or funcSymbol.IsExternal then
  begin
    // Closed context of procedure (was only a forward)
    if coContextMap in FOptions then
      FSourceContextMap.CloseContext(FTok.hotPos);
    Exit;
  end;

  if funcSymbol.Executable <> nil then
    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_MethodRedefined,
      [funcSymbol.name]);

  if UnitSection = secInterface then
    FMsgs.AddCompilerError(FTok.hotPos,
      CPE_UnexpectedImplementationInInterface);

  // Open context of full procedure body (may include a 'var' section)
  if coContextMap in FOptions then
    FSourceContextMap.OpenContext(FTok.CurrentPos, funcSymbol, ttBEGIN);
  // attach to symbol that it belongs to (perhaps a class)

  funcSymbol.ImplementationPosition := FTok.hotPos;

  try
    // Function Body
    oldprog := CurrentProg;
    proc := TdwsProcedure.Create(CurrentProg);
    CurrentProg := proc;
    try
      FMainProg.compiler := Self;
      proc.AssignTo(funcSymbol);

      if FTok.TestDelete(ttREQUIRE) then
      begin
        if funcSymbol is TMethodSymbol then
        begin
          proc.PreConditions := TSourceMethodPreConditions.Create(proc);
          if TMethodSymbol(funcSymbol).IsOverride then
            FMsgs.AddCompilerError(FTok.hotPos,
              CPE_PreconditionsMustBeInRootMethod);
        end
        else
          proc.PreConditions := TSourcePreConditions.Create(proc);
        ReadConditions(funcSymbol, proc.PreConditions, TPreConditionSymbol);
      end
      else if funcSymbol is TMethodSymbol then
      begin
        if TMethodSymbol(funcSymbol).HasConditions then
          proc.PreConditions := TSourceMethodPreConditions.Create(proc);
      end;

      if not funcSymbol.IsLambda then
      begin
        // Read local variable, constant & proc declarations
        tt := FTok.TestAny([ttVAR, ttCONST, ttPROCEDURE, ttFUNCTION]);
        if (tt in [ttVAR, ttCONST]) or ((UnitSection = secImplementation) and
          (tt in [ttPROCEDURE, ttFUNCTION])) then
        begin
          // Read names of local variable and constants
          sectionType := ttNone;
          repeat

            tt := FTok.TestAny([ttVAR, ttCONST, ttPROCEDURE, ttFUNCTION]);
            case tt of
              ttVAR, ttCONST:
                begin
                  sectionType := tt;
                  FTok.KillToken;
                end;
              ttPROCEDURE, ttFUNCTION:
                begin
                  if UnitSection = secImplementation then
                  begin
                    hotPos := FTok.hotPos;
                    FTok.KillToken;
                    ReadProcBody(ReadProcDecl(tt, hotPos));
                    sectionType := ttNone;
                    ReadSemiColon;
                    continue;
                  end
                  else
                    Break;
                end;
              ttBEGIN:
                Break;
            end;

            case sectionType of
              ttVAR:
                ReadVarDecl(FStandardDataSymbolFactory, CurrentProg.initExpr);
              ttCONST:
                begin
                  ReadConstDecl(FStandardDataSymbolFactory);
                end;
            else
              Break;
            end;

            ReadSemiColon;

          until FTok.Test(ttBEGIN);
        end;
      end;

      if coContextMap in FOptions then
        FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttBEGIN);
      try
        // Set the current context's LocalTable to be the table of the new procedure
        if coContextMap in FOptions then
          FSourceContextMap.Current.LocalTable := CurrentProg.table;

        // Read procedure body
        if funcSymbol.Kind <> fkLambda then
        begin
          if not FTok.TestDelete(ttBEGIN) then
          begin
            if FTok.Test(ttFORWARD) then
              FMsgs.AddCompilerStop(FTok.hotPos, CPE_FuncForwardAlreadyExists)
            else
              FMsgs.AddCompilerStop(FTok.hotPos, CPE_BeginExpected);
          end;
        end;

        proc.SetBeginPos(FTok.hotPos);

        progExpr := ReadRootBlock([ttEND, ttENSURE], finalToken);
        if Optimize then
        begin
          proc.OptimizeConstAssignments(progExpr);
          CurrentProg.expr := progExpr.Optimize(FCompilerContext);
        end
        else
          CurrentProg.expr := progExpr;

        if finalToken = ttENSURE then
        begin
          if funcSymbol is TMethodSymbol then
            proc.PostConditions := TSourceMethodPostConditions.Create(proc)
          else
            proc.PostConditions := TSourcePostConditions.Create(proc);
          ReadPostConditions(funcSymbol, proc.PostConditions,
            TPostConditionSymbol);
          if FTok.TestDelete(ttEND) then
            finalToken := ttEND;
        end
        else if funcSymbol is TMethodSymbol then
        begin
          if TMethodSymbol(funcSymbol).HasConditions then
            proc.PostConditions := TSourceMethodPostConditions.Create(proc);
        end;

        if finalToken <> ttEND then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_EndOfBlockExpected);

        HintUnusedSymbols;
        HintUnusedResult(proc.func.result);
        HintReferenceConstVarParams(proc.func);

      finally
        if coContextMap in FOptions then
          FSourceContextMap.CloseContext(FTok.CurrentPos);
        // close with inside procedure end
      end;
    finally
      FMainProg.compiler := nil;
      CurrentProg := oldprog;
    end;
  finally
    // Closed procedure body and procedure implementation (from declaration to body)
    if coContextMap in FOptions then
    begin
      FSourceContextMap.CloseContext(FTok.CurrentPos);
      // closed begin..end body (may include 'var' section)
      FSourceContextMap.CloseContext(FTok.CurrentPos);
      // closed from declaration through implementation
    end;
  end;
end;

// ReadProcEmpty
//
procedure TdwsCompiler.ReadProcEmpty(funcSymbol: TFuncSymbol);
var
  proc: TdwsProcedure;
begin
  FTok.KillToken;

  funcSymbol.ImplementationPosition := FTok.hotPos;

  proc := TdwsProcedure.Create(CurrentProg);
  proc.SetBeginPos(FTok.hotPos);
  proc.AssignTo(funcSymbol);
  proc.expr := TNullExpr.Create(FTok.hotPos);

  ReadSemiColon;
end;

// ReadConditions
//
procedure TdwsCompiler.ReadConditions(funcSymbol: TFuncSymbol;
  conditions: TSourceConditions; condsSymClass: TConditionSymbolClass);
var
  hotPos: TScriptPos;
  testExpr, msgExpr: TTypedExpr;
  testStart: PChar;
  testLength: Integer;
  msg: String;
  srcCond: TSourceCondition;
  endToken: TTokenType;
begin
  testStart := FTok.PosPtr;
  repeat

    FTok.Test(ttNone);
    hotPos := FTok.hotPos;

    msgExpr := nil;
    testExpr := ReadExpr(FCompilerContext.TypBoolean);
    try
      if not testExpr.IsOfType(FCompilerContext.TypBoolean) then
        FMsgs.AddCompilerError(hotPos, CPE_BooleanExpected);
      if Optimize then
        testExpr := testExpr.OptimizeToTypedExpr(FCompilerContext, hotPos);
      if testExpr.IsConstant then
        FMsgs.AddCompilerWarning(hotPos, CPW_ConstantCondition);

      testLength := (NativeUInt(FTok.PosPtr) - NativeUInt(testStart))
        div SizeOf(Char);
      if FTok.TestDelete(ttCOLON) then
      begin
        msgExpr := ReadExpr;
        if not msgExpr.IsOfType(FCompilerContext.TypString) then
          FMsgs.AddCompilerError(hotPos, CPE_StringExpected);
        if Optimize then
          msgExpr := msgExpr.OptimizeToTypedExpr(FCompilerContext, hotPos);
      end
      else
      begin
        SetString(msg, testStart, testLength);
        msg := Trim(msg);
        if (msg <> '') and (msg[Length(msg)] = ';') then
          SetLength(msg, Length(msg) - 1);
        msgExpr := FUnifiedConstants.CreateString(msg);
      end;

      ReadSemiColon(True);
      testStart := FTok.PosPtr;

      srcCond := TSourceCondition.Create(hotPos, testExpr, msgExpr);
      conditions.AddCondition(srcCond);
      funcSymbol.AddCondition(condsSymClass.Create(hotPos, srcCond, srcCond));
    except
      OrphanAndNil(testExpr);
      OrphanAndNil(msgExpr);
      raise;
    end;

    endToken := FTok.TestAny([ttVAR, ttCONST, ttBEGIN, ttEND, ttENSURE,
      ttREQUIRE, ttFUNCTION, ttPROCEDURE, ttTYPE])
  until endToken <> ttNone;
end;

// ReadPostConditions
//
procedure TdwsCompiler.ReadPostConditions(funcSymbol: TFuncSymbol;
  conditions: TSourcePostConditions; condsSymClass: TConditionSymbolClass);
begin
  if conditions is TSourcePostConditions then
    FSourcePostConditionsIndex := 1;
  try
    ReadConditions(funcSymbol, conditions, condsSymClass);
  finally
    FSourcePostConditionsIndex := 0;
  end;
end;

type
  TFindOverloadedFunc = class
    OpSymbol: TOperatorSymbol;
    CapturableUsesSym: TFuncSymbol;
    function Callback1(symbol: TSymbol): Boolean;
    function Callback2(symbol: TSymbol): Boolean;
  end;

function TFindOverloadedFunc.Callback1(symbol: TSymbol): Boolean;
var
  funcSym: TFuncSymbol;
begin
  result := False;
  funcSym := symbol.AsFuncSymbol;
  if (funcSym <> nil) and (not symbol.IsType) then
  begin
    if (funcSym.Params.Count = 1) and (funcSym.typ <> nil) and
      (Length(OpSymbol.Params) = 1) and funcSym.typ.IsOfType(OpSymbol.typ) and
      funcSym.Params[0].typ.IsOfType(OpSymbol.Params[0]) then
    begin
      CapturableUsesSym := funcSym;
      result := True;
    end;
  end;
end;

function TFindOverloadedFunc.Callback2(symbol: TSymbol): Boolean;
var
  funcSym: TFuncSymbol;
begin
  result := False;
  funcSym := symbol.AsFuncSymbol;
  if (funcSym <> nil) and (not symbol.IsType) then
  begin
    if (funcSym.Params.Count = 2) and (funcSym.typ <> nil) and
      (Length(OpSymbol.Params) = 2) and funcSym.typ.IsOfType(OpSymbol.typ) and
      funcSym.Params[0].typ.IsOfType(OpSymbol.Params[0]) and
      funcSym.Params[1].typ.IsOfType(OpSymbol.Params[1]) then
    begin
      CapturableUsesSym := funcSym;
      result := True;
    end;
  end;
end;

// ReadOperatorDecl
//
function TdwsCompiler.ReadOperatorDecl: TOperatorSymbol;

  procedure FindOverloadedFunc(var usesSym: TFuncSymbol; const usesName: String;
    fromTable: TSymbolTable; OpSymbol: TOperatorSymbol; nbParams: Integer);
  var
    finder: TFindOverloadedFunc;
  begin
    finder := TFindOverloadedFunc.Create;
    try
      finder.CapturableUsesSym := usesSym;
      finder.OpSymbol := OpSymbol;
      case nbParams of
        1:
          fromTable.EnumerateSymbolsOfNameInScope(usesName, finder.Callback1);
        2:
          fromTable.EnumerateSymbolsOfNameInScope(usesName, finder.Callback2);
      else
        Assert(False);
      end;
      usesSym := finder.CapturableUsesSym;
    finally
      finder.Free;
    end;
  end;

var
  tt: TTokenType;
  usesName: String;
  opPos, usesPos: TScriptPos;
  sym: TTypeSymbol;
  usesSym: TFuncSymbol;
  fromTable: TSymbolTable;
  typ: TTypeSymbol;
  expectedNbParams: Integer;
begin
  opPos := FTok.hotPos;
  tt := FTok.TestDeleteAny([ttPLUS, ttMINUS, ttTIMES, ttDIVIDE, ttMOD, ttDIV,
    ttOR, ttAND, ttXOR, ttIN, ttIMPLIES, ttIMPLICIT, ttSHL, ttSHR, ttSAR, ttEQ,
    ttNOT_EQ, ttGTR, ttGTR_EQ, ttLESS, ttLESS_EQ, ttLESS_LESS, ttGTR_GTR,
    ttCARET, ttEQ_EQ, ttEXCL_EQ]);
  if tt = ttNone then
    FMsgs.AddCompilerError(FTok.hotPos, CPE_OverloadableOperatorExpected);

  result := TOperatorSymbol.Create(tt);
  try
    if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackLeftExpected);

    repeat
      typ := ReadType('', tcOperand);
      result.AddParam(typ);
    until not FTok.TestDelete(ttCOMMA);

    if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);

    case tt of
      ttIMPLICIT:
        expectedNbParams := 1;
    else
      expectedNbParams := 2;
    end;
    if Length(result.Params) <> expectedNbParams then
    begin
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_BadNumberOfParameters,
        [expectedNbParams, Length(result.Params)]);
      result.token := ttNone; // nerf the operator
    end;

    if not FTok.TestDelete(ttCOLON) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ColonExpected);

    result.typ := ReadType('', tcOperand);

    if not FTok.TestDelete(ttUSES) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_UsesExpected);

    usesSym := nil;
    fromTable := CurrentProg.table;
    if not FTok.TestDeleteNamePos(usesName, usesPos) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_NameExpected)
    else
    begin
      sym := fromTable.FindTypeSymbol(usesName, cvPublic);
      if sym is THelperSymbol then
      begin
        RecordSymbolUse(sym, usesPos, [suReference]);
        if not FTok.TestDelete(ttDOT) then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_DotExpected)
        else if not FTok.TestDeleteNamePos(usesName, usesPos) then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_NameExpected)
        else
        begin
          fromTable := THelperSymbol(sym).Members;
          sym := fromTable.FindTypeSymbol(usesName, cvPublic);
        end;
      end;
      usesSym := sym.AsFuncSymbol;
      if (usesSym = nil) or sym.IsType then
        FMsgs.AddCompilerError(usesPos, CPE_FunctionMethodExpected);
    end;

    if usesSym <> nil then
    begin

      if usesSym.IsOverloaded then
        FindOverloadedFunc(usesSym, usesName, fromTable, result,
          expectedNbParams);

      RecordSymbolUse(usesSym, usesPos, [suReference]);

      if (usesSym.typ = nil) or not usesSym.typ.IsOfType(result.typ) then
        FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadResultType,
          [result.typ.Caption])
      else if usesSym.Params.Count <> expectedNbParams then
        FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadNumberOfParameters,
          [expectedNbParams, usesSym.Params.Count])
      else if not usesSym.Params[0].typ.IsOfType(result.Params[0]) then
        FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadParameterType,
          [0, result.Params[0].Caption, usesSym.Params[0].typ.Caption])
      else if usesSym.Params[0].ClassType = TVarParamSymbol then
        FMsgs.AddCompilerErrorFmt(usesPos, CPE_VarParameterForbidden, [0])
      else if (expectedNbParams > 1) and not usesSym.Params[1].typ.IsOfType
        (result.Params[1]) then
        FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadParameterType,
          [1, result.Params[1].Caption, usesSym.Params[1].typ.Caption])
      else if (expectedNbParams > 1) and
        (usesSym.Params[1].ClassType = TVarParamSymbol) then
        FMsgs.AddCompilerErrorFmt(usesPos, CPE_VarParameterForbidden, [1])
      else
        result.usesSym := usesSym;
    end;
  except
    OrphanObject(result);
    raise;
  end;

  CurrentProg.table.AddSymbol(result);
  if (result.token <> ttNone) and (result.usesSym <> nil) then
  begin
    if CurrentProg.table.HasSameLocalOperator(result) then
      FMsgs.AddCompilerError(opPos, CPE_OverloadAlreadyExists);
  end;
end;

// UnexpectedBlockTokenError
//
procedure TdwsCompiler.UnexpectedBlockTokenError(const endTokens: TTokenTypes);
var
  msg, found: String;
  foundTyp: TTokenType;
begin
  msg := TokenTypesToString(endTokens);
  if FTok.HasTokens then
  begin
    foundTyp := FTok.GetToken.FTyp;
    found := cTokenStrings[foundTyp];
  end
  else
    foundTyp := ttNone;
  if msg = '' then
    if found = '' then
      msg := CPE_SemiExpected
    else
      msg := Format(CPE_Unexpected_X, [found])
  else
    case foundTyp of
      ttNAME:
        msg := Format(CPE_X_ExpectedBut_Y_Found, [msg, 'identifier']);
      ttNone:
        msg := msg + CPE_XxxExpected;
    else
      msg := Format(CPE_X_ExpectedBut_Y_Found, [msg, '"' + found + '"']);
    end;
  FMsgs.AddCompilerStop(FTok.hotPos, msg);
end;

// ReadBlocks
//
function TdwsCompiler.ReadBlocks(const endTokens: TTokenTypes;
  var finalToken: TTokenType): TProgramExpr;
var
  stmt: TProgramExpr;
  token: TToken;
  closePos: TScriptPos;
  // Position at which the ending token was found (for context)
  blockExpr: TBlockExpr;
  reach: TReachStatus;
  action: TdwsStatementAction;
begin
  // Read a block of instructions enclosed in "begin" and "end"
  result := nil;
  reach := rsReachable;
  blockExpr := TBlockExpr.Create(FCompilerContext, FTok.hotPos);
  try
    if coContextMap in FOptions then
    begin
      FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttBEGIN);
      closePos.Clear;
    end;

    CurrentProg.EnterSubTable(blockExpr.table);
    try

      repeat

        if not FTok.HasTokens then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_EndOfBlockExpected);

        if FTok.GetToken.FTyp in endTokens then
        begin
          finalToken := FTok.GetToken.FTyp;
          closePos := FTok.GetToken.FScriptPos;
          // get start position of ending token
          FTok.KillToken;
          Break;
        end;

        if reach = rsUnReachable then
        begin
          reach := rsUnReachableWarned;
          FMsgs.AddCompilerWarning(FTok.CurrentPos, CPW_UnReachableCode);
        end;

        action := saNone;
        stmt := ReadStatement(action, blockExpr);

        if Assigned(stmt) then
        begin
          blockExpr.AddStatement(stmt);
          if (reach = rsReachable) and stmt.InterruptsFlow then
            reach := rsUnReachable;
        end;

        case action of
          saNoSemiColon:
            ;
          saNone:
            begin
              if not FTok.TestDelete(ttSEMI) then
              begin
                token := FTok.GetToken;
                if (token = nil) or (not(token.FTyp in endTokens)) then
                  UnexpectedBlockTokenError(endTokens);
              end;
            end;
        else
          Assert(False);
        end;

      until False;

      HintUnusedSymbols;

      if Optimize then
      begin
        result := blockExpr.Optimize(FCompilerContext);
        blockExpr := nil;
      end
      else
        result := blockExpr;
    finally
      CurrentProg.LeaveSubTable;

      if coContextMap in FOptions then
      begin
        if blockExpr <> nil then
          FSourceContextMap.Current.LocalTable := blockExpr.table
        else if result is TBlockExpr then
          FSourceContextMap.Current.LocalTable := TBlockExpr(result).table;
        if not closePos.Defined then
          closePos := FTok.CurrentPos; // means an error occured
        FSourceContextMap.CloseContext(closePos);
      end;
    end;
  except
    OrphanAndNil(blockExpr);
    raise;
  end;
end;

// ReadBlock
//
function TdwsCompiler.ReadBlock: TProgramExpr;
var
  tt: TTokenType;
begin
  result := nil;
  if FTok.TestDelete(ttBEGIN) then
  begin
    result := ReadBlocks([ttEND], tt);
  end
  else if FTok.HasTokens then
  begin
    // Read a single instruction
    result := ReadInstr;
  end
  else
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_EndOfBlockExpected);
end;

// ReadInstr
//
function TdwsCompiler.ReadInstr: TProgramExpr;
var
  token: TTokenType;
  locExpr: TProgramExpr;
  locExprClass: TClass;
  hotPos: TScriptPos;
  msgsCount: Integer;
  funcSym: TFuncSymbol;
begin
  if Assigned(FOnReadInstr) then
  begin
    result := FOnReadInstr(Self);
    if result <> nil then
      Exit;
  end;

  // Decide which instruction to read
  case FTok.TestDeleteAny([ttIF, ttCASE, ttFOR, ttWHILE, ttREPEAT, ttBREAK,
    ttEXIT, ttTRY, ttRAISE, ttCONTINUE, ttWITH]) of
    ttIF:
      result := ReadIf;
    ttCASE:
      begin
        result := ReadCase;
        if Optimize then
          result := result.Optimize(FCompilerContext);
      end;
    ttFOR:
      result := ReadFor;
    ttWHILE:
      result := ReadWhile;
    ttREPEAT:
      result := ReadRepeat;
    ttBREAK:
      begin
        if FLoopExprs.Count = 0 then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_BreakOutsideOfLoop)
        else if (FFinallyExprs.Count > 0) and FFinallyExprs.Peek then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_BreakContinueInFinally);
        result := TBreakExpr.Create(FTok.hotPos);
        MarkLoopExitable(leBreak);
      end;
    ttEXIT:
      begin
        if FFinallyExprs.Count > 0 then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_ExitInFinally);
        result := ReadExit;
        MarkLoopExitable(leExit);
      end;
    ttTRY:
      result := ReadTry;
    ttCONTINUE:
      begin
        if FLoopExprs.Count = 0 then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_ContinueOutsideOfLoop)
        else if (FFinallyExprs.Count > 0) and FFinallyExprs.Peek then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_BreakContinueInFinally);
        result := TContinueExpr.Create(FTok.hotPos);
      end;
    ttRAISE:
      result := ReadRaise;
    ttWITH:
      result := ReadWith;
  else
    // Try to read a function call, method call or an assignment
    if (FTok.TestAny([ttBLEFT, ttINHERITED, ttNEW]) <> ttNone) or FTok.TestName
    then
    begin // !! TestName must be the last !!
      hotPos := FTok.hotPos;
      msgsCount := FMsgs.Count;
      if FTok.TestDelete(ttBLEFT) then // (X as TY)
        locExpr := ReadSymbol(ReadBracket, True)
      else
        locExpr := ReadName(True);
      if locExpr = nil then
      begin
        FMsgs.AddCompilerStop(hotPos, CPE_InvalidInstruction);
        Exit(nil);
      end;
      locExprClass := locExpr.ClassType;
      if locExprClass.InheritsFrom(TTypedExpr) then
      begin
        if FTok.TestAny([ttLESS_LESS, ttGTR_GTR]) <> ttNone then
        begin
          locExpr := ReadExprMult(nil, TTypedExpr(locExpr));
          locExprClass := locExpr.ClassType;
        end;
      end;
      try
        token := FTok.TestDeleteAny(cAssignmentTokens);
        if token <> ttNone then
        begin

          if not locExprClass.InheritsFrom(TDataExpr) then
          begin
            FMsgs.AddCompilerError(hotPos, CPE_CantWriteToLeftSide);
            OrphanAndNil(locExpr);
            result := ReadExpr; // keep compiling
            OrphanAndNil(result);
          end
          else
          begin
            if not TDataExpr(locExpr).IsWritable then
              FMsgs.AddCompilerError(FTok.hotPos, CPE_CantWriteToLeftSide);
            if locExprClass.InheritsFrom(TVarExpr) then
              WarnForVarUsage(TVarExpr(locExpr), hotPos);
            result := ReadAssign(token, TDataExpr(locExpr));
          end;

        end
        else
        begin

          if locExprClass.InheritsFrom(TDataExpr) then
          begin
            funcSym := locExpr.typ.AsFuncSymbol;
            if funcSym <> nil then
            begin
              locExpr := ReadFunc(funcSym, locExpr as TDataExpr);
              locExprClass := locExpr.ClassType;
            end;
          end;

          if locExprClass.InheritsFrom(TAssignExpr) then

            result := locExpr

          else if locExprClass.InheritsFrom(TFuncExprBase) or
            (locExprClass = TConnectorCallExpr) then
          begin
            result := locExpr;
            if locExpr.IsConstant then
            begin
              if FMsgs.Count = msgsCount then // avoid hint on calls with issues
                FMsgs.AddCompilerHint(hotPos, CPE_ConstantInstruction);
            end;
          end
          else if (locExprClass = TConnectorWriteExpr) or
            locExprClass.InheritsFrom(TDynamicArraySetExpr) or
            locExprClass.InheritsFrom(TStringArraySetExpr) or
            locExprClass.InheritsFrom(TArrayPseudoMethodExpr) then
            result := locExpr
          else if locExprClass.InheritsFrom(TConstExpr) then
          begin
            OrphanAndNil(locExpr);
            result := TNullExpr.Create(hotPos);
            if FMsgs.Count = msgsCount then
            // avoid hint on expression with issues
              FMsgs.AddCompilerHint(hotPos, CPE_ConstantInstruction);
          end
          else if locExprClass.InheritsFrom(TNullExpr) then
          begin
            result := locExpr;
          end
          else if (FPendingSetterValueExpr <> nil) and
            locExpr.InheritsFrom(TTypedExpr) then
          begin
            result := TNoResultWrapperExpr.Create(hotPos, TTypedExpr(locExpr));
          end
          else
          begin
            result := locExpr;
          end;

        end;
      except
        OrphanAndNil(locExpr);
        raise;
      end;
    end
    else
    begin
      result := TNullExpr.Create(FTok.hotPos);
    end;
  end;
end;

// ReadInherited
//
function TdwsCompiler.ReadInherited(isWrite: Boolean): TProgramExpr;
var
  name: String;
  namePos: TScriptPos;
  sym: TSymbol;
  methSym: TMethodSymbol;
  compositeSym, ParentSym: TCompositeTypeSymbol;
  varExpr: TTypedExpr;
begin
  result := nil;
  if not((CurrentProg is TdwsProcedure) and
    (TdwsProcedure(CurrentProg).func is TMethodSymbol)) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_InheritedOnlyAllowedInMethods);

  methSym := TMethodSymbol(TdwsProcedure(CurrentProg).func);

  if not FTok.TestDeleteNamePos(name, namePos) then
  begin

    sym := methSym.ParentMeth;
    if not methSym.IsOverride then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_InheritedWithoutName);

  end
  else
  begin

    compositeSym := methSym.StructSymbol;
    if compositeSym.ClassType = THelperSymbol then
    begin
      sym := THelperSymbol(compositeSym).ForType.UnAliasedType;
      if sym is TArraySymbol then
      begin
        if sym is TDynamicArraySymbol then
          varExpr := GetVarExpr(namePos, methSym.selfSym)
        else
          varExpr := GetConstByRefParamExpr
            (methSym.selfSym as TConstByRefParamSymbol);
        result := ReadArrayMethod(name, namePos, varExpr);
        Exit;
      end;
      if sym is TCompositeTypeSymbol then
        ParentSym := TCompositeTypeSymbol(sym)
      else
        ParentSym := nil;
    end
    else
      ParentSym := compositeSym.Parent;

    if ParentSym <> nil then
      sym := ParentSym.Members.FindSymbol(name, cvPrivate)
    else
      sym := nil;

  end;

  if Assigned(sym) then
  begin

    if sym is TMethodSymbol then
    begin

      methSym := TMethodSymbol(sym);

      if name = '' then
        RecordSymbolUse(methSym, FTok.hotPos, [suReference, suImplicit])
      else
        RecordSymbolUse(methSym, FTok.hotPos, [suReference]);

      if methSym.IsAbstract then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_AbstractMethodUsage);

      if methSym.IsOverloaded then
        result := ReadSelfMethOverloaded(methSym, isWrite, nil,
          [cfoForceStatic, cfoInheritedCall])
      else
        result := ReadSelfMethod(methSym, isWrite, nil, nil,
          [cfoForceStatic, cfoInheritedCall]);

    end
    else if sym is TPropertySymbol then
    begin

      RecordSymbolUseReference(sym, FTok.hotPos, isWrite);

      varExpr := TVarExpr.CreateTyped(FCompilerContext, FTok.hotPos,
        methSym.selfSym);
      try
        result := ReadPropertyExpr(varExpr, TPropertySymbol(sym), isWrite);
      except
        OrphanAndNil(varExpr);
        raise;
      end;

    end
    else
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_InheritedWithoutName);
  end
  else
    FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_InheritedMethodNotFound, [Name]);
end;

// ResolveUnitNameSpace
//
function TdwsCompiler.ResolveUnitNameSpace(const prefixPos: TScriptPos;
  unitPrefix: TUnitSymbol): TUnitSymbol;
var
  dottedName, nextDottedName: String;
begin
  if not FTok.Test(ttDOT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_DotExpected);

  dottedName := unitPrefix.name;
  while FTok.TestDelete(ttDOT) do
  begin
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);
    nextDottedName := dottedName + '.' + FTok.GetToken.AsString;
    if not unitPrefix.PossibleNameSpace(nextDottedName) then
      Break;
    dottedName := nextDottedName;
    FTok.KillToken;
  end;

  result := unitPrefix.FindNameSpaceUnit(dottedName);
  if result = nil then
    FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_UnknownName, [dottedName])
  else
  begin
    if coSymbolDictionary in options then
      SymbolDictionary.ReplaceSymbolAt(unitPrefix, result, prefixPos);
  end;
end;

// ReadName
//
function TdwsCompiler.ReadName(isWrite: Boolean = False;
  expecting: TTypeSymbol = nil): TProgramExpr;

  function ReadSpecialName(const name: String; const namePos: TScriptPos;
    sk: TSpecialKeywordKind; var exprResult: TProgramExpr): Boolean;
  begin
    FTok.KillToken;
    if (sk = skDefault) and not FTok.Test(ttBLEFT) then
    begin
      FTok.SimulateNameToken(namePos, name);
      FTok.TestName;
      result := False;
    end
    else
    begin
      if not(coHintsDisabled in FOptions) then
        CheckSpecialNameCase(name, sk, namePos);
      exprResult := ReadSymbol(ReadSpecialFunction(namePos, sk), isWrite,
        expecting);
      result := True;
    end;
  end;

var
  sym: TSymbol;
  nameToken: TToken;
  namePos: TScriptPos;
  varExpr: TTypedExpr;
  fieldExpr: TTypedExpr;
  propExpr, castExpr: TProgramExpr;
  funcExpr: TTypedExpr;
  progMeth: TMethodSymbol;
  selfSym: TDataSymbol;
  baseType: TTypeSymbol;
  baseTypeClass: TClass;
  sk: TSpecialKeywordKind;
  symClassType: TClass;
  symTaxonomy: TdwsSymbolTaxonomy;
begin
  if (FSourcePostConditionsIndex <> 0) and FTok.TestDelete(ttOLD) then
    Exit(ReadNameOld(isWrite));

  if FTok.TestDelete(ttNEW) then
  begin
    result := ReadNew(nil, False);
    result := ReadSymbol(result, isWrite);
    Exit;
  end;

  if FTok.TestDelete(ttINHERITED) then
    Exit(ReadNameInherited(isWrite));

  // Get name
  if not FTok.TestName then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

  nameToken := FTok.GetToken;
  namePos := FTok.hotPos;

  // Test for special functions
  sk := IdentifySpecialKeyword(nameToken.AsString);
  if sk <> skNone then
  begin
    if ReadSpecialName(nameToken.AsString, namePos, sk, result) then
      Exit;
    nameToken := FTok.GetToken;
  end;

  // Find name in symboltable
  sym := CurrentProg.table.FindSymbol(nameToken.AsString, cvPrivate);
  if not Assigned(sym) then
  begin
    result := ReadSelfTypeHelper(nameToken, FTok.hotPos, expecting);
    if result <> nil then
      Exit;
    if Assigned(FOnFindUnknownName) then
      sym := FOnFindUnknownName(Self, nameToken.AsString);
    if Assigned(FOnReadUnknownName) then
    begin
      result := FOnReadUnknownName(Self);
      if result <> nil then
        Exit;
    end;
    if sym = nil then
    begin
      sym := CurrentProg.table.FindSymbol(nameToken.AsString, cvMagic);
      if sym = nil then
        FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownName, [nameToken.AsString])
      else
        FMsgs.AddCompilerErrorFmt(namePos, CPE_MemberSymbolNotVisible,
          [nameToken.AsString]);
    end;
  end;

  if (sym <> nil) and not(coHintsDisabled in FOptions) then
    CheckMatchingDeclarationCase(nameToken.AsString, sym, namePos);

  FTok.KillToken;

  // Add the symbol usage to Dictionary
  RecordSymbolUseReference(sym, namePos, isWrite);

  if sym.ClassType = TGenericSymbol then
    sym := ReadSpecializedType(TGenericSymbol(sym));

  result := nil;
  try
    baseType := sym.baseType;

    if baseType <> nil then
    begin

      // Namespace prefix found
      if baseType.ClassType = TUnitSymbol then
      begin

        baseType := ResolveUnitNameSpace(namePos, TUnitSymbol(baseType));

        namePos := FTok.hotPos;
        sym := TUnitSymbol(baseType).table.FindLocal(FTok.GetToken.AsString);

        if not Assigned(sym) then
          FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_UnknownNameDotName,
            [baseType.name, FTok.GetToken.AsString]);

        baseType := sym.baseType;

        FTok.KillToken;

        // Already added symbol usage of the unit. Now add for the unit's specified symbol.
        RecordSymbolUseReference(sym, namePos, isWrite);

      end;

    end;

    symClassType := sym.ClassType;
    symTaxonomy := sym.Taxonomy;

    if stValueSymbol in symTaxonomy then
    begin // symClassType.InheritsFrom(TValueSymbol) then begin

      // value symbols

      if stConstSymbol in symTaxonomy then
      begin // symClassType.InheritsFrom(TConstSymbol)

        result := ReadConstName(namePos, TConstSymbol(sym), isWrite);

      end
      else if stDataSymbol in symTaxonomy then
      begin // symClassType.InheritsFrom(TDataSymbol) then begin

        if symClassType = TLazyParamSymbol then
        begin

          result := ReadSymbol(GetLazyParamExpr(TLazyParamSymbol(sym)), isWrite,
            expecting);

        end
        else if symClassType = TVarParamSymbol then
        begin

          result := ReadSymbol(GetVarParamExpr(TVarParamSymbol(sym)), isWrite,
            expecting);

        end
        else if symClassType = TConstByRefParamSymbol then
        begin

          result := ReadSymbol
            (GetConstByRefParamExpr(TConstByRefParamSymbol(sym)), isWrite,
            expecting);

        end
        else
        begin

          result := ReadDataSymbolName(namePos, TDataSymbol(sym),
            CurrentProg.table, isWrite, expecting);

        end;

      end
      else if symClassType = TFieldSymbol then
      begin

        if TdwsProcedure(CurrentProg).func is TMethodSymbol then
        begin
          progMeth := CurrentProg.ContextMethodSymbol;
          selfSym := progMeth.selfSym;
        end
        else
        begin
          selfSym := TDataSymbol(CurrentProg.table.FindSymbol(SYS_SELF, cvMagic,
            TDataSymbol));
        end;

        if (selfSym = nil) or (selfSym.typ is TStructuredTypeMetaSymbol) then
        begin

          FMsgs.AddCompilerError(FTok.hotPos, CPE_ObjectReferenceExpected);
          fieldExpr := TFieldExpr.Create(namePos, TFieldSymbol(sym), nil,
            feroDefault);

        end
        else
        begin

          fieldExpr := ReadField(namePos, selfSym, TFieldSymbol(sym));

        end;
        result := ReadImplicitCall(fieldExpr, isWrite, expecting);

      end
      else if symClassType = TPropertySymbol then
      begin

        progMeth := CurrentProg.ContextMethodSymbol;
        selfSym := progMeth.selfSym;

        if selfSym = nil then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_ObjectReferenceExpected);

        if selfSym.ClassType = TVarParamSymbol then
          varExpr := GetVarParamExpr(progMeth.selfSym as TVarParamSymbol)
        else
          varExpr := GetVarExpr(namePos, progMeth.selfSym);
        try
          propExpr := ReadPropertyExpr(varExpr, TPropertySymbol(sym), isWrite);
        except
          OrphanAndNil(varExpr);
          raise;
        end;

        result := ReadSymbol(propExpr, isWrite, expecting);

      end
      else if symClassType = TExternalVarSymbol then
      begin

        result := ReadSymbol(ReadExternalVar(TExternalVarSymbol(sym), isWrite),
          isWrite, expecting);
        result := ReadSymbol(result, isWrite, expecting);

      end
      else
      begin

        FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_Unexpected_X, [sym.name]);

      end;

    end
    else if stTypeSymbol in symTaxonomy then
    begin

      // type symbols

      if stAliasSymbol in symTaxonomy then
      begin
        sym := TAliasSymbol(sym).UnAliasedType;
        symClassType := sym.ClassType;
      end;

      if baseType is TStructuredTypeSymbol then
      begin

        WarnDeprecatedType(namePos, baseType);
        baseTypeClass := baseType.ClassType;

        if baseTypeClass = TClassSymbol then
        begin

          result := ReadClassSymbolName(TClassSymbol(baseType), isWrite,
            expecting);

        end
        else if baseTypeClass = TInterfaceSymbol then
        begin

          result := ReadInterfaceSymbolName(TInterfaceSymbol(baseType), isWrite,
            expecting);

        end
        else
        begin

          Assert(baseTypeClass = TRecordSymbol);
          if FTok.TestDelete(ttBLEFT) then
            result := ReadTypeCast(namePos, TTypeSymbol(sym))
          else
            result := ReadRecordSymbolName(TRecordSymbol(baseType), isWrite,
              expecting);

        end;

      end
      else if symClassType.InheritsFrom(TFuncSymbol) then
      begin

        if symClassType.InheritsFrom(TMethodSymbol) then
        begin

          if TMethodSymbol(sym).IsOverloaded then
            funcExpr := ReadSelfMethOverloaded(TMethodSymbol(sym), isWrite,
              expecting, [])
          else
            funcExpr := ReadSelfMethod(TMethodSymbol(sym), isWrite,
              expecting, nil, []);
          result := ReadSymbol(funcExpr, isWrite, expecting);

        end
        else
        begin

          if TFuncSymbol(sym).IsOverloaded then
            funcExpr := ReadFuncOverloaded(TFuncSymbol(sym), CurrentProg.table,
              nil, expecting)
          else
            funcExpr := ReadFunc(TFuncSymbol(sym), nil, expecting);
          result := ReadSymbol(funcExpr, isWrite, expecting);

        end;

      end
      else if symClassType.InheritsFrom(TEnumerationSymbol) then
      begin

        result := ReadEnumerationSymbolName(namePos, TEnumerationSymbol(sym),
          expecting = FCompilerContext.TypAnyType)

      end
      else
      begin

        WarnDeprecatedType(namePos, TTypeSymbol(baseType));

        if FTok.TestDelete(ttBLEFT) then
          castExpr := ReadTypeCast(namePos, TTypeSymbol(sym))
        else
          castExpr := ReadTypeExpr(namePos, TTypeSymbol(sym), isWrite,
            expecting);
        result := ReadSymbol(castExpr, isWrite, expecting);

      end;

    end
    else if symClassType = TResourceStringSymbol then
    begin

      result := ReadResourceStringName(TResourceStringSymbol(sym), namePos);

    end
    else
    begin

      FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_UnknownType, [sym.name]);

    end;

  except
    OrphanAndNil(result);
    raise;
  end;
end;

// ReadEnumerationSymbolName
//
function TdwsCompiler.ReadEnumerationSymbolName(const enumPos: TScriptPos;
  enumSym: TEnumerationSymbol; acceptTypeRef: Boolean): TProgramExpr;

  function ReadByName(const elemPos: TScriptPos): TTypedExpr;
  var
    nameExpr: TTypedExpr;
    namePos: TScriptPos;
  begin
    namePos := FTok.CurrentPos;
    nameExpr := ReadBracket(FCompilerContext.TypString);
    if (nameExpr = nil) or (nameExpr.typ = nil) or
      not nameExpr.typ.IsOfType(FCompilerContext.TypString) then
    begin
      FMsgs.AddCompilerError(namePos, CPE_StringExpected);
      // keep compiling
      OrphanObject(nameExpr);
      nameExpr := TConstStringExpr.Create(namePos,
        FCompilerContext.TypString, '');
    end;
    result := TEnumerationElementByNameExpr.Create(FCompilerContext, elemPos,
      enumSym, nameExpr);
  end;

var
  name: String;
  elemPos: TScriptPos;
  elem: TSymbol;
begin
  if FTok.TestDelete(ttBLEFT) then
  begin

    result := ReadTypeCast(enumPos, enumSym);
    result := ReadSymbol(result, False, nil);

  end
  else if FTok.TestDelete(ttDOT) then
  begin

    if not FTok.TestDeleteNamePos(name, elemPos) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);
    if FTok.TestDelete(ttBLEFT) then
    begin
      if UnicodeSameText(name, 'ByName') then
      begin
        Exit(ReadByName(elemPos));
      end
      else if not FTok.TestDelete(ttBRIGHT) then
        FMsgs.AddCompilerStopFmt(elemPos, CPE_UnknownMethodForType,
          [name, enumSym.name]);
      elem := nil;
    end
    else
    begin
      elem := enumSym.Elements.FindLocal(name);
    end;
    if elem = nil then
    begin
      if UnicodeSameText(name, 'Low') then
        result := TConstIntExpr.Create(elemPos, enumSym, enumSym.LowBound)
      else if UnicodeSameText(name, 'High') then
        result := TConstIntExpr.Create(elemPos, enumSym, enumSym.HighBound)
      else
      begin
        FMsgs.AddCompilerErrorFmt(elemPos, CPE_UnknownNameDotName,
          [enumSym.name, name]);
        result := TConstIntExpr.Create(elemPos, enumSym, 0);
      end;
    end
    else
    begin
      RecordSymbolUseReference(elem, elemPos, False);
      result := ReadConstName(elemPos, elem as TElementSymbol, False);
    end;

  end
  else
  begin

    result := TTypeReferenceExpr.Create(enumSym, enumPos);
    if not acceptTypeRef then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_BrackLeftExpected);

  end;
end;

// ReadClassSymbolName
//
function TdwsCompiler.ReadClassSymbolName(baseType: TClassSymbol;
  isWrite: Boolean; expecting: TTypeSymbol): TProgramExpr;
var
  namePos: TScriptPos;
  constExpr: TTypedExpr;
  operandExpr, convExpr: TTypedExpr;
  castedExprTyp: TTypeSymbol;
begin
  if baseType.IsForwarded then
    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ClassNotCompletelyDefined,
      [baseType.name]);

  if FTok.TestDelete(ttBLEFT) then
  begin
    // Cast
    FTok.TestName;
    namePos := FTok.hotPos;
    operandExpr := ReadExpr(expecting);
    if baseType.IsExternalRooted then
    begin
      convExpr := TConvExternalExpr.Create(FCompilerContext, namePos,
        operandExpr);
      convExpr.typ := baseType;
    end
    else
    begin
      convExpr := TObjAsClassExpr.Create(FCompilerContext, namePos, operandExpr,
        baseType);
      if operandExpr <> nil then
      begin
        castedExprTyp := operandExpr.typ;
        if castedExprTyp <> FCompilerContext.TypNil then
        begin
          if castedExprTyp <> nil then
            castedExprTyp := castedExprTyp.UnAliasedType;
          if (not castedExprTyp.IsClassSymbol) or
            ((not TClassSymbol(castedExprTyp).IsOfType(baseType)) and
            (not baseType.IsOfType(castedExprTyp))) then
          begin
            IncompatibleTypes(namePos, CPE_IncompatibleTypes, castedExprTyp,
              baseType);
          end;
        end;
      end;
    end;
    if not(FTok.TestDelete(ttBRIGHT)) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_BrackRightExpected);
    result := ReadSymbol(convExpr, isWrite, expecting);

  end
  else
  begin

    constExpr := TConstIntExpr.Create(FTok.hotPos, baseType.MetaSymbol,
      Int64(baseType));
    result := ReadSymbol(constExpr, isWrite, expecting);

  end;
end;

// ReadInterfaceSymbolName
//
function TdwsCompiler.ReadInterfaceSymbolName(baseType: TInterfaceSymbol;
  isWrite: Boolean; expecting: TTypeSymbol): TProgramExpr;
var
  constExpr: TTypedExpr;
begin
  constExpr := TConstIntExpr.Create(cNullPos, baseType, Int64(baseType));
  result := ReadSymbol(constExpr, isWrite, expecting);
end;

// ReadRecordSymbolName
//
function TdwsCompiler.ReadRecordSymbolName(baseType: TRecordSymbol;
  isWrite: Boolean; expecting: TTypeSymbol): TProgramExpr;
var
  constExpr: TTypedExpr;
begin
  constExpr := TConstIntExpr.Create(cNullPos, baseType.MetaSymbol,
    Int64(baseType));
  result := ReadSymbol(constExpr, isWrite, expecting);
end;

// ReadConstName
//
function TdwsCompiler.ReadConstName(const constPos: TScriptPos;
  constSym: TConstSymbol; isWrite: Boolean): TProgramExpr;
var
  typ: TTypeSymbol;
begin
  if constSym.IsDeprecated then
    WarnDeprecatedSymbol(constPos, constSym, constSym.deprecatedMessage);
  typ := constSym.typ;
  result := ReadSymbol(TConstExpr.CreateTyped(FCompilerContext, typ,
    constSym), isWrite)
end;

// ReadDataSymbolName
//
function TdwsCompiler.ReadDataSymbolName(const aScriptPos: TScriptPos;
  dataSym: TDataSymbol; fromTable: TSymbolTable; isWrite: Boolean;
  expecting: TTypeSymbol): TProgramExpr;
var
  varExpr: TVarExpr;
begin
  varExpr := GetVarExpr(aScriptPos, dataSym);
  result := ReadImplicitCall(varExpr, isWrite, expecting);
end;

// ReadImplicitCall
//
function TdwsCompiler.ReadImplicitCall(codeExpr: TTypedExpr; isWrite: Boolean;
  expecting: TTypeSymbol): TProgramExpr;
var
  codeExprTyp: TTypeSymbol;
  funcSym: TFuncSymbol;
begin
  if codeExpr = nil then
    Exit(nil);
  codeExprTyp := codeExpr.typ;
  if codeExprTyp = nil then
  begin
    OrphanObject(codeExpr);
    Exit(nil);
  end;

  funcSym := codeExprTyp.AsFuncSymbol;
  if funcSym <> nil then
  begin
    if FTok.Test(ttASSIGN) or
      ((expecting <> nil) and codeExprTyp.IsOfType(expecting) and
      not FTok.Test(ttBLEFT)) then
      result := codeExpr
    else if not funcSym.IsOverloaded then
      result := ReadFunc(funcSym, codeExpr, expecting)
    else
    begin
      OrphanObject(codeExpr);
      FMsgs.AddCompilerStopFmt(codeExpr.scriptPos,
        CPH_AmbiguousMatchingOverloadsForCall, [funcSym.name]);
      result := nil;
    end;
  end
  else
    result := codeExpr;

  result := ReadSymbol(result, isWrite, expecting);
end;

// ReadResourceStringName
//
function TdwsCompiler.ReadResourceStringName(resSym: TResourceStringSymbol;
  const namePos: TScriptPos): TResourceStringExpr;
begin
  RecordSymbolUse(resSym, namePos, [suReference, suRead]);
  result := TResourceStringExpr.Create(FCompilerContext, namePos, resSym);
end;

// ReadNameOld
//
function TdwsCompiler.ReadNameOld(isWrite: Boolean): TTypedExpr;
var
  sym: TDataSymbol;
  oldExpr: TProgramExpr;
  expr: TTypedExpr;
  initExpr: TProgramExpr;
  varExpr: TVarExpr;
begin
  oldExpr := ReadName(isWrite);
  if (not(oldExpr is TTypedExpr)) or (TTypedExpr(oldExpr).typ = nil) then
  begin
    FMsgs.AddCompilerError(FTok.hotPos, CPE_FunctionOrValueExpected);
    // keep going
    OrphanAndNil(oldExpr);
    expr := TConstExpr.CreateNull(cNullPos, FCompilerContext.TypVariant);
  end
  else
    expr := TTypedExpr(oldExpr);

  sym := TScriptDataSymbol.Create('old$' + IntToStr(FSourcePostConditionsIndex),
    expr.typ);
  Inc(FSourcePostConditionsIndex);
  CurrentProg.table.AddSymbol(sym);
  varExpr := GetVarExpr(FTok.hotPos, sym);
  initExpr := CreateAssign(FTok.hotPos, ttASSIGN, varExpr, expr);
  CurrentProg.initExpr.AddStatement(initExpr);

  result := GetVarExpr(FTok.hotPos, sym);
end;

// ReadNameInherited
//
function TdwsCompiler.ReadNameInherited(isWrite: Boolean): TProgramExpr;
begin
  // Name with inherited
  result := ReadInherited(isWrite);
  result := ReadSymbol(result, isWrite);
end;

// ReadField
//
function TdwsCompiler.ReadField(const scriptPos: TScriptPos;
  selfSym: TDataSymbol; fieldSym: TFieldSymbol): TDataExpr;
var
  varExpr: TTypedExpr;
begin
  varExpr := nil;
  result := ReadField(scriptPos, selfSym, fieldSym, varExpr);
end;

// ReadField
//
function TdwsCompiler.ReadField(const scriptPos: TScriptPos;
  selfSym: TDataSymbol; fieldSym: TFieldSymbol; var varExpr: TTypedExpr)
  : TDataExpr;
begin
  if varExpr = nil then
    varExpr := GetSelfParamExpr(scriptPos, selfSym);
  if fieldSym.StructSymbol.ClassType = TRecordSymbol then
  begin
    if varExpr.ClassType = TVarExpr then
      result := TRecordVarExpr.Create(scriptPos, TVarExpr(varExpr), fieldSym)
    else
      result := TRecordExpr.Create(scriptPos, (varExpr as TDataExpr), fieldSym)
  end
  else
  begin
    var
    readOnlyState := GetFieldReadOnlyState(fieldSym);
    if varExpr is TObjectVarExpr then
      result := TFieldVarExpr.Create(FTok.hotPos, fieldSym, varExpr,
        readOnlyState)
    else
      result := TFieldExpr.Create(FTok.hotPos, fieldSym, varExpr,
        readOnlyState);
  end;
  varExpr := nil;
end;

// ReadPropertyProgExpr
//
function TdwsCompiler.ReadPropertyProgExpr(var expr: TProgramExpr;
  propertySym: TPropertySymbol; isWrite: Boolean): TProgramExpr;
var
  typedExpr: TTypedExpr;
begin
  try
    typedExpr := (expr as TTypedExpr);
    result := ReadPropertyExpr(typedExpr, propertySym, isWrite);
  finally
    expr := typedExpr;
  end;
end;

// ReadPropertyExpr
//
function TdwsCompiler.ReadPropertyExpr(var expr: TTypedExpr;
  propertySym: TPropertySymbol; isWrite: Boolean): TProgramExpr;
begin
  if propertySym.IsDeprecated then
    WarnDeprecatedSymbol(FTok.hotPos, propertySym,
      propertySym.deprecatedMessage);
  if propertySym.IsReintroduce and FTok.TestDelete(ttBLEFT) then
  begin
    FMsgs.AddCompilerHintFmt(FTok.hotPos, CPH_ReintroducedPropertyBrackets,
      [propertySym.name]);
    if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_BrackRightExpected);
  end;

  if isWrite then
    result := ReadPropertyWriteExpr(expr, propertySym)
  else
    result := ReadPropertyReadExpr(expr, propertySym);
end;

// ReadPropertyReadExpr
//
function TdwsCompiler.ReadPropertyReadExpr(var expr: TTypedExpr;
  propertySym: TPropertySymbol): TTypedExpr;
var
  sym: TSymbol;
  aPos: TScriptPos;
  typedExprList: TTypedExprList;
  argPosArray: TScriptPosArray;
begin
  aPos := FTok.hotPos;

  sym := propertySym.ReadSym;

  // No ReadSym
  if sym = nil then
  begin

    if propertySym.WriteSym = nil then
      FMsgs.AddCompilerError(aPos, CPE_CantReadProperty)
    else
      FMsgs.AddCompilerError(aPos, CPE_WriteOnlyProperty);
    OrphanAndNil(expr);
    Exit(CreateTypedDefault(propertySym.typ));

  end;

  RecordSymbolUseImplicitReference(sym, aPos, False);

  if sym is TFieldSymbol then
  begin

    // ReadSym is a field
    if expr.typ is TStructuredTypeMetaSymbol then
      FMsgs.AddCompilerError(aPos, CPE_ObjectReferenceExpected);
    result := ReadField(aPos, nil, TFieldSymbol(sym), expr);

  end
  else if sym is TMethodSymbol then
  begin

    typedExprList := TTypedExprList.Create;
    try
      if propertySym.HasArrayIndices then
      begin
        typedExprList.table := propertySym.ArrayIndices;
        ReadArguments(typedExprList.AddExpr, ttALEFT, ttARIGHT, argPosArray,
          typedExprList.ExpectedArg);
      end;

      result := ReadPropertyArrayAccessor(expr, propertySym, typedExprList,
        aPos, False);

    finally
      typedExprList.Orphan(FCompilerContext);
    end;

  end
  else if sym is TClassVarSymbol then
  begin

    OrphanAndNil(expr);
    result := GetVarExpr(aPos, TClassVarSymbol(sym));

  end
  else
  begin

    Assert(sym is TConstSymbol);

    OrphanAndNil(expr);
    result := TConstExpr.CreateTyped(FCompilerContext, sym.typ,
      TConstSymbol(sym));

  end;
end;

// ReadPropertyWriteExpr
//
function TdwsCompiler.ReadPropertyWriteExpr(var expr: TTypedExpr;
  propertySym: TPropertySymbol): TProgramExpr;
var
  sym: TSymbol;
  aPos: TScriptPos;
  tokenType: TTokenType;
  typedExprList: TTypedExprList;
  argPosArray: TScriptPosArray;
begin
  aPos := FTok.hotPos;

  typedExprList := TTypedExprList.Create;
  try
    if propertySym.HasArrayIndices then
    begin
      typedExprList.table := propertySym.ArrayIndices;
      ReadArguments(typedExprList.AddExpr, ttALEFT, ttARIGHT, argPosArray,
        typedExprList.ExpectedArg);
    end;

    tokenType := FTok.TestDeleteAny(cAssignmentTokens);

    // implicit assign for setter write expressions
    if (tokenType = ttNone) and (FPendingSetterValueExpr <> nil) and
      FTok.Test(ttBRIGHT) then
      tokenType := ttASSIGN;

    if tokenType <> ttNone then
    begin

      if tokenType <> ttASSIGN then
        FMsgs.AddCompilerError(FTok.hotPos,
          CPE_CantUseCombinedAssignmentOnProperty);

      sym := propertySym.WriteSym;

      if sym is TFieldSymbol then
      begin

        // WriteSym is a Field
        RecordSymbolUseImplicitReference(sym, aPos, True);
        if expr.typ is TClassOfSymbol then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_ObjectReferenceExpected);
        var
        fieldExpr := ReadField(aPos, nil, TFieldSymbol(sym), expr);
        result := ReadAssign(ttASSIGN, fieldExpr);

      end
      else if sym is TClassVarSymbol then
      begin

        // WriteSym is a class var
        RecordSymbolUseImplicitReference(sym, aPos, True);
        OrphanAndNil(expr);
        var
          fieldExpr: TDataExpr := GetVarExpr(aPos, TClassVarSymbol(sym));
        result := ReadAssign(ttASSIGN, fieldExpr);

      end
      else if sym is TMethodSymbol then
      begin

        // WriteSym is a Method
        // Convert an assignment to a function call f := x  -->  f(x)
        RecordSymbolUseImplicitReference(sym, aPos, False);
        result := ReadPropertyArrayAccessor(expr, propertySym, typedExprList,
          aPos, True);

      end
      else
      begin

        OrphanAndNil(expr);
        result := TErrorExpr.Create(aPos);
        FMsgs.AddCompilerError(aPos, CPE_ReadOnlyProperty)

      end;

    end
    else
    begin

      if (FTok.TestAny([ttDOT, ttALEFT]) <> ttNone) or
        (propertySym.typ.AsFuncSymbol <> nil) then
      begin

        sym := propertySym.ReadSym;

        if sym is TMethodSymbol then
        begin

          result := ReadPropertyArrayAccessor(expr, propertySym, typedExprList,
            aPos, False);

        end
        else if sym is TFieldSymbol then
        begin

          if expr.typ is TClassOfSymbol then
          begin
            FMsgs.AddCompilerError(FTok.hotPos, CPE_ObjectReferenceExpected);
            OrphanAndNil(expr);
          end;
          if propertySym.ownerSymbol is TRecordSymbol then
            result := TRecordExpr.Create(FTok.hotPos, expr as TDataExpr,
              TFieldSymbol(sym))
          else
          begin
            var
            fieldExpr := TFieldExpr.Create(FTok.hotPos, TFieldSymbol(sym), expr,
              feroReadOnly);
            fieldExpr.typ := propertySym.typ;
            result := fieldExpr;
          end;
          expr := nil;

        end
        else if sym is TClassVarSymbol then
        begin

          OrphanAndNil(expr);
          result := GetVarExpr(aPos, TClassVarSymbol(sym));

        end
        else
        begin

          if not(sym is TConstSymbol) then
            FMsgs.AddCompilerStop(FTok.hotPos, CPE_ConstantExpressionExpected);

          OrphanAndNil(expr);
          result := TConstExpr.CreateTyped(FCompilerContext, sym.typ,
            TConstSymbol(sym));

        end;

      end
      else
      begin

        FMsgs.AddCompilerError(aPos, CPE_InvalidInstruction);
        // fake to keep going
        OrphanAndNil(expr);
        result := TConstExpr.Create(aPos, propertySym.typ);

      end;

    end;

  finally
    typedExprList.Orphan(FCompilerContext);
  end;
end;

// ReadPropertyArrayAccessor
//
function TdwsCompiler.ReadPropertyArrayAccessor(var expr: TTypedExpr;
  propertySym: TPropertySymbol; typedExprList: TTypedExprList;
  const scriptPos: TScriptPos; isWrite: Boolean): TFuncExprBase;
var
  i: Integer;
  sym: TMethodSymbol;
begin
  if isWrite then
    sym := propertySym.WriteSym as TMethodSymbol
  else
    sym := propertySym.ReadSym as TMethodSymbol;

  if expr.typ is TStructuredTypeMetaSymbol then
  begin
    // Class properties
    if not sym.IsClassMethod then
    begin
      if isWrite then
        FMsgs.AddCompilerError(scriptPos, CPE_StaticPropertyWriteExpected)
      else
        FMsgs.AddCompilerError(scriptPos, CPE_StaticPropertyReadExpected);
    end;
    result := GetMethodExpr(sym, expr, rkClassOfRef, scriptPos, []);
  end
  else
    result := GetMethodExpr(sym, expr, rkObjRef, scriptPos, []);

  expr := nil;
  try
    // Add array indices (if any)
    for i := 0 to typedExprList.Count - 1 do
      result.AddArg(typedExprList.expr[i]);
    typedExprList.Clear;

    if Assigned(propertySym.indexSym) then
      result.AddArg(TConstExpr.CreateTyped(FCompilerContext,
        propertySym.indexSym, propertySym.IndexValue));

    // Add right side of assignment
    if isWrite then
      result.AddArg(ReadExpr(propertySym.typ));

    TypeCheckArguments(FCompilerContext, result, nil);
  except
    if result <> nil then
      result.Orphan(FCompilerContext);
    raise;
  end;
end;

// ReadSymbol
//
function TdwsCompiler.ReadSymbol(expr: TProgramExpr; isWrite: Boolean = False;
  expecting: TTypeSymbol = nil): TProgramExpr;

  function GetDefaultProperty(struct: TCompositeTypeSymbol): TPropertySymbol;
  begin
    while Assigned(struct) and not Assigned(struct.DefaultProperty) do
      struct := struct.Parent;

    if Assigned(struct) then
      result := struct.DefaultProperty
    else
      result := nil;
  end;

begin
  result := expr;
  try
    repeat
      expr := result;

      // Member
      case FTok.TestAny([ttDOT, ttALEFT, ttBLEFT]) of

        ttDOT:
          begin

            FTok.KillToken;
            result := nil;
            result := ReadSymbolMemberExpr(expr, isWrite, expecting);

          end;
        ttALEFT:
          begin
            // Arrays

            if Assigned(result) then
            begin

              var
              baseType := result.baseType;
              if (baseType is TStructuredTypeSymbol) or
                (baseType is TStructuredTypeMetaSymbol) then
              begin

                // class array property
                var
                  DefaultProperty: TPropertySymbol;
                if baseType is TStructuredTypeSymbol then
                  DefaultProperty :=
                    GetDefaultProperty(TStructuredTypeSymbol(baseType))
                else
                  DefaultProperty :=
                    GetDefaultProperty(TStructuredTypeMetaSymbol(baseType)
                    .StructSymbol);

                if Assigned(DefaultProperty) then
                begin
                  RecordSymbolUseImplicitReference(DefaultProperty,
                    FTok.hotPos, isWrite);
                  result := ReadPropertyProgExpr(result,
                    DefaultProperty, isWrite)
                end
                else
                begin
                  FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_NoDefaultProperty,
                    [TDataExpr(result).typ.name]);
                end;

              end
              else
              begin

                // Type "array"
                if not(result is TTypedExpr) then
                begin
                  OrphanAndNil(result);
                end;
                if (baseType is TArraySymbol) and (result is TTypedExpr) then
                begin
                  result := ReadSymbolArrayExpr(TTypedExpr(result))
                end
                else if TTypedExpr(result).IsOfType(FCompilerContext.TypString)
                  and (result is TDataExpr) then
                begin
                  FTok.KillToken;
                  result := ReadStringArray(TDataExpr(result), isWrite);
                end
                else if baseType is TAssociativeArraySymbol then
                begin
                  result := ReadSymbolAssociativeArrayExpr(TDataExpr(result))
                end
                else if baseType is TConnectorSymbol then
                begin
                  result := ReadConnectorArray('', TTypedExpr(result),
                    TConnectorSymbol(baseType).connectorType, isWrite)
                end
                else
                begin
                  FMsgs.AddCompilerError(FTok.hotPos, CPE_ArrayExpected);
                  SkipUntilToken(ttARIGHT);
                  FTok.TestDelete(ttARIGHT);
                end;
                if Optimize then
                  result := result.Optimize(FCompilerContext);

              end;

            end;
          end;

        ttBLEFT:
          begin

            var
              funcSym: TFuncSymbol := nil;
            var
            baseType := result.baseType;
            if baseType <> nil then
            begin
              funcSym := baseType.AsFuncSymbol;
              if (funcSym = nil) and baseType.CanExpectAnyFuncSymbol then
                funcSym := FCompilerContext.TypAnyFunc;
            end;
            if funcSym <> nil then
            begin
              var
              codeExpr := result as TTypedExpr;
              result := nil;
              result := ReadFunc(funcSym, codeExpr);
            end
            else
              FMsgs.AddCompilerStop(FTok.hotPos, CPE_NoMethodExpected);

          end;

      end;

    until (expr = result);
  except
    OrphanAndNil(result);
    raise;
  end;
end;

// ReadSymbolArrayExpr
//
function TdwsCompiler.ReadSymbolArrayExpr(var baseExpr: TTypedExpr)
  : TProgramExpr;
var
  idx: Int64;
  indexExpr, valueExpr: TTypedExpr;
  newBaseExpr: TTypedExpr;
  baseType: TArraySymbol;
  arraySymbol: TStaticArraySymbol;
  errCount: Integer;
  hotPos: TScriptPos;
begin
  FTok.KillToken;

  newBaseExpr := nil;
  indexExpr := nil;

  if FTok.TestDelete(ttARIGHT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_ExpressionExpected);

  errCount := FMsgs.Count;

  // There is at least one index expression
  try
    repeat
      hotPos := FTok.hotPos;
      indexExpr := ReadExpr;
      if not(baseExpr.baseType is TArraySymbol) then
      begin
        FMsgs.AddCompilerError(hotPos, RTE_TooManyIndices);
        OrphanAndNil(indexExpr);
        continue;
      end
      else if indexExpr = nil then
      begin
        continue;
      end;

      baseType := TArraySymbol(baseExpr.baseType);

      if (indexExpr.typ = nil) or
        not((indexExpr.typ.UnAliasedType = baseType.IndexType.UnAliasedType) or
        indexExpr.typ.IsOfType(FCompilerContext.TypVariant)) then
        IncompatibleTypes(hotPos, CPE_ArrayIndexMismatch, baseType.IndexType,
          indexExpr.typ);

      if baseType is TStaticArraySymbol then
      begin

        arraySymbol := TStaticArraySymbol(baseType);
        if arraySymbol is TOpenArraySymbol then
        begin

          newBaseExpr := TOpenArrayExpr.Create(FTok.hotPos,
            baseExpr as TDataExpr, indexExpr, arraySymbol);
          indexExpr := nil;

        end
        else
        begin

          if arraySymbol.IndexType.IsOfType(FCompilerContext.TypBoolean) then
          begin

            newBaseExpr := TStaticArrayBoolExpr.Create(FTok.hotPos,
              baseExpr as TDataExpr, indexExpr, arraySymbol);
            indexExpr := nil;

          end
          else
          begin

            newBaseExpr := CreateArrayExpr(FTok.hotPos, baseExpr as TDataExpr,
              indexExpr);
            if indexExpr.IsConstant and (FMsgs.Count = errCount) then
            begin
              idx := indexExpr.EvalAsInteger(FExec);
              if idx < arraySymbol.LowBound then
                FMsgs.AddCompilerErrorFmt(FTok.hotPos,
                  RTE_ArrayLowerBoundExceeded, [idx])
              else if idx > arraySymbol.HighBound then
                FMsgs.AddCompilerErrorFmt(FTok.hotPos,
                  RTE_ArrayUpperBoundExceeded, [idx]);
            end;
            indexExpr := nil;

          end;
        end;

      end
      else
      begin

        Assert(baseType is TDynamicArraySymbol);

        if FTok.Test(ttCOMMA) then
        begin

          newBaseExpr := CreateDynamicArrayExpr(FTok.hotPos, baseExpr,
            indexExpr);
          indexExpr := nil;

        end
        else if FTok.TestDelete(ttARIGHT) then
        begin

          if FTok.TestDelete(ttASSIGN) then
          begin
            hotPos := FTok.hotPos;
            valueExpr := ReadExpr(baseType.typ);
            if valueExpr.typ <> baseType.typ then
              FCompilerContext.WrapWithImplicitCast(baseType.typ, hotPos,
                valueExpr);
            if not baseType.typ.IsCompatible(valueExpr.typ) then
            begin
              IncompatibleTypes(hotPos, CPE_AssignIncompatibleTypes,
                valueExpr.typ, baseType.typ);
              OrphanAndNil(valueExpr);
              OrphanAndNil(indexExpr);
              OrphanAndNil(baseExpr);
              result := TNullExpr.Create(FTok.hotPos);
            end
            else
            begin
              result := CreateDynamicArraySetExpr(FCompilerContext, FTok.hotPos,
                baseExpr, indexExpr, valueExpr);
            end;
            indexExpr := nil;
          end
          else
          begin
            result := CreateDynamicArrayExpr(FTok.hotPos, baseExpr, indexExpr);
            indexExpr := nil;
          end;
          Exit;

        end
        else
        begin

          OrphanAndNil(indexExpr);
          Break;

        end;

      end;

      baseExpr := newBaseExpr;
      newBaseExpr := nil;
    until not FTok.TestDelete(ttCOMMA);
  except
    OrphanAndNil(indexExpr);
    OrphanAndNil(newBaseExpr);
    raise;
  end;

  result := baseExpr;

  if not FTok.TestDelete(ttARIGHT) then
    FMsgs.AddCompilerError(FTok.hotPos, CPE_ArrayBracketRightExpected);
end;

// ReadSymbolAssociativeArrayExpr
//
function TdwsCompiler.ReadSymbolAssociativeArrayExpr(var baseExpr: TDataExpr)
  : TProgramExpr;
var
  baseType: TAssociativeArraySymbol;
  keyExpr, valueExpr: TTypedExpr;
  hotPos: TScriptPos;
begin
  FTok.KillToken;

  if FTok.TestDelete(ttARIGHT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_ExpressionExpected);

  baseType := (baseExpr.baseType as TAssociativeArraySymbol);

  keyExpr := nil;
  try
    hotPos := FTok.hotPos;
    keyExpr := ReadExpr;

    if not keyExpr.typ.IsOfType(baseType.keyType) then
      keyExpr := CompilerUtils.WrapWithImplicitConversion(FCompilerContext,
        keyExpr, baseType.keyType, hotPos, CPE_ArrayIndexMismatch);

    if (keyExpr.typ = nil) or not((keyExpr.typ.IsCompatible(baseType.keyType))
      or keyExpr.typ.IsOfType(FCompilerContext.TypVariant)) then
      IncompatibleTypes(hotPos, CPE_ArrayIndexMismatch, baseType.keyType,
        keyExpr.typ);

    if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ArrayBracketRightExpected);

    if FTok.TestDelete(ttASSIGN) then
    begin

      hotPos := FTok.hotPos;
      valueExpr := ReadExpr(baseType.typ);
      if valueExpr.typ <> baseType.typ then
        FCompilerContext.WrapWithImplicitCast(baseType.typ, hotPos, valueExpr);

      if not baseType.typ.IsCompatible(valueExpr.typ) then
      begin
        IncompatibleTypes(hotPos, CPE_AssignIncompatibleTypes, valueExpr.typ,
          baseType.typ);
      end;

      if baseType.KeyAndElementSizeAreBaseTypesOfSizeOne then
        result := TAssociativeArrayValueSetExpr.Create(FTok.hotPos, baseExpr,
          keyExpr, valueExpr)
      else
        result := TAssociativeArraySetExpr.Create(FTok.hotPos, baseExpr,
          keyExpr, valueExpr);
      keyExpr := nil;
      Exit;

    end
    else
    begin

      if (keyExpr.typ.Size = 1) and keyExpr.typ.IsBaseType then
        baseExpr := TAssociativeArrayValueKeyGetExpr.Create(FTok.hotPos,
          baseExpr, keyExpr, baseType)
      else
        baseExpr := TAssociativeArrayGetExpr.Create(FTok.hotPos, baseExpr,
          keyExpr, baseType);
      keyExpr := nil;

    end;

  except
    OrphanAndNil(keyExpr);
    raise;
  end;

  result := baseExpr;
end;

// ReadSymbolMemberExpr
//
function TdwsCompiler.ReadSymbolMemberExpr(var expr: TProgramExpr;
  isWrite: Boolean; expecting: TTypeSymbol): TProgramExpr;
var
  name: String;
  namePos: TScriptPos;
  helperExpr: TProgramExpr;
  member: TSymbol;
  memberClassType: TClass;
  baseExpr: TTypedExpr;
  meth: TMethodSymbol;
  baseType: TTypeSymbol;
begin
  result := nil;
  try

    if FTok.TestDeleteAnyNamePos(name, namePos) then
    begin

      if expr <> nil then
        baseType := expr.baseType
      else
        baseType := nil;

      if baseType = nil then
      begin
        OrphanAndNil(expr);
        ReportNoMemberForType(name, namePos, baseType);
        Exit;
      end;

      if (baseType <> nil) and FCompilerContext.HelperMemberNames.Contains(name)
      then
      begin
        helperExpr := ReadTypeHelper(expr as TTypedExpr, name, namePos,
          expecting, isWrite, False);
        if helperExpr <> nil then
        begin

          expr := nil;
          result := helperExpr;
          Exit;

        end;
      end;
      result := expr;

      baseType := baseType.UnAliasedType;

      // Class, record, intf
      if baseType.InheritsFrom(TStructuredTypeSymbol) then
      begin

        if (baseType is TRecordSymbol) and (result is TFuncExpr) then
          TFuncExpr(result).InitializeResultAddr(CurrentProg);

        member := FindStructMember(TStructuredTypeSymbol(baseType), name);
        if member = nil then
        begin

          ReportNoMemberForType(name, namePos, baseType);

        end
        else
        begin

          if not member.IsOverloaded then
            RecordSymbolUseReference(member, namePos, isWrite);
          if not(coHintsDisabled in FOptions) then
            CheckMatchingDeclarationCase(name, member, namePos);

          memberClassType := member.ClassType;

          if memberClassType.InheritsFrom(TMethodSymbol) then
          begin

            baseExpr := (result as TTypedExpr);
            result := nil;
            meth := TMethodSymbol(member);
            if meth.IsOverloaded then
              result := ReadMethOverloaded(meth, baseExpr, namePos, expecting)
            else
              result := ReadMethod(meth, baseExpr, namePos, expecting);

          end
          else if memberClassType.InheritsFrom(TFieldSymbol) then
          begin

            Assert(result is TTypedExpr);
            result := ReadField(FTok.hotPos, nil, TFieldSymbol(member),
              TTypedExpr(result));

          end
          else if memberClassType.InheritsFrom(TPropertySymbol) then
          begin

            Assert(result is TTypedExpr);
            result := ReadPropertyExpr(TTypedExpr(result),
              TPropertySymbol(member), isWrite)

          end
          else if memberClassType = TClassVarSymbol then
          begin

            OrphanAndNil(result);
            result := ReadDataSymbolName(namePos, TDataSymbol(member),
              TStructuredTypeSymbol(member).Members, isWrite, expecting);

          end
          else if memberClassType = TClassConstSymbol then
          begin

            OrphanAndNil(result);
            result := ReadConstName(namePos, TConstSymbol(member), isWrite);

          end
          else
          begin

            ReportNoMemberForType(name, namePos, baseType);

          end;

        end;

        // Meta (Class Of, Record Of)
      end
      else if baseType.InheritsFrom(TStructuredTypeMetaSymbol) then
      begin

        member := TStructuredTypeSymbol(baseType.typ)
          .Members.FindSymbolFromScope(Name, CurrentStruct);
        if member <> nil then
        begin
          memberClassType := member.ClassType;
          if not(coHintsDisabled in FOptions) then
            CheckMatchingDeclarationCase(name, member, namePos);
        end
        else
          memberClassType := nil;

        RecordSymbolUseReference(member, namePos, isWrite);

        // Class method
        if member is TMethodSymbol then
        begin

          baseExpr := (result as TTypedExpr);
          result := nil;
          meth := TMethodSymbol(member);
          if meth.IsOverloaded then
            result := ReadStaticMethOverloaded(meth, baseExpr, namePos,
              expecting)
          else
            result := ReadStaticMethod(meth, baseExpr, namePos, expecting);

          // Static property
        end
        else if member is TPropertySymbol then
        begin

          expr := result;
          result := nil;
          result := ReadPropertyProgExpr(expr, TPropertySymbol(member),
            isWrite);

        end
        else if memberClassType = TClassVarSymbol then
        begin

          OrphanAndNil(result);
          result := ReadDataSymbolName(namePos, TDataSymbol(member),
            TStructuredTypeSymbol(baseType.typ).Members, isWrite, expecting);

        end
        else if memberClassType = TClassConstSymbol then
        begin

          OrphanAndNil(result);
          result := ReadConstName(namePos, TConstSymbol(member), isWrite);

        end
        else if member <> nil then
        begin

          FMsgs.AddCompilerStop(namePos, CPE_StaticMethodExpected);

        end
        else
        begin

          ReportNoMemberForType(name, namePos, baseType);

        end;

        // Array symbol
      end
      else if baseType.InheritsFrom(TArraySymbol) then
      begin

        result := ReadArrayMethod(name, namePos, result as TTypedExpr);

        // String symbol
      end
      else if baseType.InheritsFrom(TBaseStringSymbol) then
      begin

        result := nil;
        result := ReadStringMethod(name, namePos, expr as TTypedExpr);

        // Associative Array symbol
      end
      else if baseType.InheritsFrom(TAssociativeArraySymbol) then
      begin

        result := ReadAssociativeArrayMethod(name, namePos,
          result as TTypedExpr);

        // "set of" symbol
      end
      else if baseType.InheritsFrom(TSetOfSymbol) then
      begin

        result := nil;
        result := ReadSetOfMethod(name, namePos, expr as TTypedExpr);

        // enumeration element symbol
      end
      else if (expr <> nil) and expr.typ.UnAliasedTypeIs(TEnumerationSymbol)
      then
      begin

        result := nil;
        result := ReadElementMethod(name, namePos, expr as TTypedExpr);

        // Connector symbol
      end
      else if baseType.InheritsFrom(TConnectorSymbol) and
        not(result is TTypeReferenceExpr) then
      begin

        try
          result := ReadConnectorSym(Name, result as TTypedExpr,
            TConnectorSymbol(baseType).connectorType, isWrite)
        except
          result := nil;
          raise;
        end;

      end
      else
        ReportNoMemberForType(name, namePos, baseType);

    end
    else
    begin

      OrphanAndNil(expr);
      expr := nil;
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

    end;
  except
    OrphanAndNil(result);
    raise;
  end;
end;

// ReadExternalVar
//
function TdwsCompiler.ReadExternalVar(sym: TExternalVarSymbol; isWrite: Boolean)
  : TFuncExpr;
begin
  result := nil;
  try
    if isWrite and not FTok.Test(ttDOT) then
    begin
      if FTok.TestDelete(ttASSIGN) then
      begin
        if not Assigned(sym.WriteFunc) then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_CantWriteToLeftSide);
        // Transform a := b into a(b)
        result := TFuncSimpleExpr.Create(FCompilerContext, FTok.hotPos,
          sym.WriteFunc);
        result.AddArg(ReadExpr);
      end
      else if sym.typ.IsClassSymbol or (sym.typ is TClassOfSymbol) or
        (sym.typ is TConnectorSymbol) then
      begin
        if not Assigned(sym.ReadFunc) then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_RightSideNeedsReturnType);
        result := TFuncSimpleExpr.Create(FCompilerContext, FTok.hotPos,
          sym.ReadFunc)
      end
      else
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_AssignExpected);
    end
    else if Assigned(sym.ReadFunc) then
      result := TFuncSimpleExpr.Create(FCompilerContext, FTok.hotPos,
        sym.ReadFunc)
    else
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_WriteOnlyProperty); // ??
    TypeCheckArguments(FCompilerContext, result, nil);
  except
    if result <> nil then
      result.Orphan(FCompilerContext);
    raise;
  end;
end;

// ReadFor
//
function TdwsCompiler.ReadFor: TProgramExpr;
var
  forPos: TScriptPos;
  expr: TProgramExpr;
  loopVarExpr: TVarExpr;
  loopVarName: String;
  loopVarNamePos: TScriptPos;
begin
  forPos := FTok.hotPos;

  if FTok.TestDelete(ttVAR) then
  begin

    expr := nil;
    loopVarExpr := nil;
    if not FTok.TestDeleteNamePos(loopVarName, loopVarNamePos) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);
    CheckName(loopVarName, loopVarNamePos);

  end
  else
  begin

    expr := ReadName(True);

    if expr is TFuncPtrExpr then
      expr := TFuncPtrExpr(expr).Extract;
    if not(expr is TVarExpr) then
    begin
      OrphanAndNil(expr);
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_VariableExpected);
    end;

    loopVarExpr := TVarExpr(expr);
    loopVarNamePos := FTok.hotPos;

    WarnForVarUsage(loopVarExpr, loopVarNamePos);

  end;

  case FTok.TestDeleteAny([ttASSIGN, ttIN]) of
    ttASSIGN:
      result := ReadForTo(forPos, loopVarExpr, loopVarName, loopVarNamePos);
    ttIN:
      result := ReadForIn(forPos, loopVarExpr, loopVarName, loopVarNamePos);
  else
    OrphanAndNil(expr);
    result := nil;
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_AssignExpected);
  end;
end;

// ReadForTo
//
function TdwsCompiler.ReadForTo(const forPos: TScriptPos; loopVarExpr: TVarExpr;
  const loopVarName: String; const loopVarNamePos: TScriptPos): TNoResultExpr;
var
  iterVarExpr: TIntVarExpr;
  fromExpr, toExpr: TTypedExpr;
  forExprClass: TForExprClass;
  loopVarSymbol: TDataSymbol;
  loopBlockExpr: TBlockExpr;
begin
  fromExpr := nil;
  toExpr := nil;
  loopBlockExpr := nil;
  try
    if loopVarExpr <> nil then
    begin
      if not loopVarExpr.IsOfType(FCompilerContext.TypInteger) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_IntegerExpected);
      if not(loopVarExpr is TIntVarExpr) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_FORLoopMustBeLocalVariable);
    end;

    fromExpr := ReadExpr;
    if not(fromExpr.IsOfType(FCompilerContext.TypInteger) or
      fromExpr.IsOfType(FCompilerContext.TypVariant)) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_IntegerExpected);

    if loopVarExpr = nil then
    begin
      loopBlockExpr := TBlockExpr.Create(FCompilerContext, forPos);
      loopVarSymbol := TScriptDataSymbol.Create(loopVarName, fromExpr.typ,
        sdspLoopIterator);
      loopBlockExpr.table.AddSymbol(loopVarSymbol);
      RecordSymbolUse(loopVarSymbol, loopVarNamePos,
        [suDeclaration, suReference, suWrite]);
      loopVarExpr := GetVarExpr(loopVarNamePos, loopVarSymbol);
      CurrentProg.initExpr.AddStatement(TAssignConstToIntegerVarExpr.CreateVal
        (FCompilerContext, loopVarNamePos, loopVarExpr, 0));
      loopVarExpr.IncRefCount;
    end;

    iterVarExpr := TIntVarExpr(loopVarExpr);

    if FTok.TestDelete(ttTO) then
      forExprClass := TForUpwardExpr
    else if FTok.TestDelete(ttDOWNTO) then
      forExprClass := TForDownwardExpr
    else
    begin
      forExprClass := TForUpwardExpr;
      FMsgs.AddCompilerError(FTok.hotPos, CPE_ToOrDowntoExpected);
    end;

    if loopBlockExpr <> nil then
    begin
      if coContextMap in FOptions then
        FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttFOR);
      CurrentProg.EnterSubTable(loopBlockExpr.table);
    end;
    try
      toExpr := ReadExpr;
      if not(toExpr.IsOfType(FCompilerContext.TypInteger) or
        toExpr.IsOfType(FCompilerContext.TypVariant)) then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_IntegerExpected);

      loopVarExpr := nil;
      result := ReadForStep(forPos, forExprClass, iterVarExpr, fromExpr,
        toExpr, nil);
    finally
      if loopBlockExpr <> nil then
      begin
        CurrentProg.LeaveSubTable;
        if coContextMap in options then
        begin
          FSourceContextMap.Current.LocalTable := loopBlockExpr.table;
          FSourceContextMap.CloseContext(FTok.CurrentPos);
        end;
      end;
    end;

  except
    OrphanAndNil(loopBlockExpr);
    OrphanAndNil(fromExpr);
    OrphanAndNil(toExpr);
    OrphanAndNil(loopVarExpr);
    raise;
  end;

  if loopBlockExpr <> nil then
  begin
    loopBlockExpr.AddStatement(result);
    result := loopBlockExpr;
  end;
end;

// ReadForStep
//
function TdwsCompiler.ReadForStep(const forPos: TScriptPos;
  forExprClass: TForExprClass; iterVarExpr: TIntVarExpr;
  var fromExpr, toExpr: TTypedExpr; loopFirstStatement: TProgramExpr): TForExpr;
var
  stepExpr: TTypedExpr;
  stepPos: TScriptPos;
  iterBlockExpr: TBlockExpr;
begin
  try
    if FTok.Test(ttNAME) and ASCIISameText(FTok.GetToken.AsString, 'step') then
    begin
      FTok.KillToken;
      FTok.Test(ttNone);
      stepPos := FTok.hotPos;
      stepExpr := ReadExpr;
      if not stepExpr.IsOfType(FCompilerContext.TypInteger) then
        FMsgs.AddCompilerError(stepPos, CPE_IntegerExpected);
      if stepExpr.InheritsFrom(TConstIntExpr) and
        (TConstIntExpr(stepExpr).value <= 0) then
        FMsgs.AddCompilerErrorFmt(stepPos,
          RTE_ForLoopStepShouldBeStrictlyPositive,
          [TConstIntExpr(stepExpr).value]);
      if forExprClass = TForUpwardExpr then
        forExprClass := TForUpwardStepExpr
      else
        forExprClass := TForDownwardStepExpr;
    end
    else
    begin
      stepExpr := nil;
    end;

    iterBlockExpr := nil;
    result := forExprClass.Create(forPos);
    EnterLoop(result);
    try
      MarkLoopExitable(leBreak);
      result.varExpr := iterVarExpr;
      iterVarExpr := nil;

      result.fromExpr := fromExpr;
      fromExpr := nil;

      result.toExpr := toExpr;
      toExpr := nil;

      if stepExpr <> nil then
      begin
        TForStepExpr(result).stepExpr := stepExpr;
        stepExpr := nil;
      end;

      if not FTok.TestDelete(ttDO) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_DoExpected);

      if loopFirstStatement <> nil then
      begin
        iterBlockExpr := TBlockExpr.Create(FCompilerContext, FTok.hotPos);
        iterBlockExpr.AddStatement(loopFirstStatement);
        loopFirstStatement := nil;
        iterBlockExpr.AddStatement(ReadBlock);
        result.DoExpr := iterBlockExpr;
        iterBlockExpr := nil;
      end
      else
      begin
        result.DoExpr := ReadBlock;
      end;

    except
      OrphanAndNil(iterBlockExpr);
      OrphanAndNil(stepExpr);
      if result <> nil then
        result.Orphan(FCompilerContext);
      raise;
    end;
    LeaveLoop;
  except
    if iterVarExpr <> nil then
      iterVarExpr.Orphan(FCompilerContext);
    OrphanAndNil(fromExpr);
    OrphanAndNil(toExpr);
    OrphanAndNil(loopFirstStatement);
    raise;
  end;
end;

// ReadForIn
//
function TdwsCompiler.ReadForIn(const forPos: TScriptPos; loopVarExpr: TVarExpr;
  const loopVarName: String; const loopVarNamePos: TScriptPos): TProgramExpr;
var
  iterVarExpr: TIntVarExpr;
  iterVarSym: TDataSymbol;
  initIterVarExpr: TAssignConstToIntegerVarExpr;
  inExpr: TProgramExpr;
  inExprVarSym: TDataSymbol;
  fromExpr, toExpr: TTypedExpr;
  forExprClass: TForExprClass;
  arraySymbol: TArraySymbol;
  enumSymbol: TTypeSymbol;
  inPos, inExprPos: TScriptPos;
  inExprAssignExpr: TProgramExpr;
  readArrayItemExpr: TProgramExpr;
  inExprVarExpr: TVarExpr;
  blockExpr: TBlockExpr;
begin
  forExprClass := TForUpwardExpr;

  inPos := FTok.hotPos;

  inExpr := ReadExpr(FCompilerContext.TypAnyType);

  readArrayItemExpr := nil;
  inExprAssignExpr := nil;
  blockExpr := nil;
  iterVarExpr := nil;

  if (inExpr is TTypedExpr) and (inExpr.ClassType <> TTypeReferenceExpr) then
  begin

    if inExpr.typ.IsOfType(FCompilerContext.TypString) and
      ((loopVarExpr = nil) or loopVarExpr.typ.IsOfType
      (FCompilerContext.TypInteger) or loopVarExpr.typ.IsOfType
      (FCompilerContext.TypString)) then
    begin

      result := ReadForInString(forPos, inExpr, loopVarExpr, loopVarName,
        loopVarNamePos);
      Exit;

    end
    else if inExpr.typ is TConnectorSymbol then
    begin

      result := ReadForInConnector(forPos, inExpr as TTypedExpr, inPos,
        loopVarExpr, loopVarName, loopVarNamePos);
      Exit;

    end
    else
    begin

      // if inExpr is an expression, create a temporary variable
      // so it is evaluated only once
      if (inExpr.typ <> nil) and
        not((inExpr is TVarExpr) or (inExpr is TConstExpr)) then
      begin
        inExprVarSym := TScriptDataSymbol.Create('', inExpr.typ);
        CurrentProg.table.AddSymbol(inExprVarSym);
        inExprVarExpr := GetVarExpr(inPos, inExprVarSym);
        inExprAssignExpr := CreateAssignExpr(FCompilerContext, FTok.hotPos,
          ttASSIGN, inExprVarExpr, TTypedExpr(inExpr));
        inExpr := inExprVarExpr;
        inExpr.IncRefCount;
      end;

      if inExpr.typ is TArraySymbol then
      begin

        arraySymbol := TArraySymbol(inExpr.typ);

        // create anonymous iter variables & its initialization expression
        iterVarSym := TScriptDataSymbol.Create('', arraySymbol.IndexType);
        CurrentProg.table.AddSymbol(iterVarSym);
        iterVarExpr := GetVarExpr(inPos, iterVarSym) as TIntVarExpr;
        initIterVarExpr := TAssignConstToIntegerVarExpr.CreateVal
          (FCompilerContext, inPos, iterVarExpr, 0);
        CurrentProg.initExpr.AddStatement(initIterVarExpr);

        fromExpr := CreateArrayLow(inPos, inExpr, arraySymbol, False);
        toExpr := CreateArrayHigh(inPos, inExpr, arraySymbol, False);

        blockExpr := EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos,
          loopVarExpr, arraySymbol.typ);

        iterVarExpr := GetVarExpr(inPos, iterVarSym) as TIntVarExpr;
        readArrayItemExpr := CreateAssign(FTok.hotPos, ttASSIGN, loopVarExpr,
          CreateArrayExpr(FTok.hotPos, (inExpr as TDataExpr), iterVarExpr));

        iterVarExpr := GetVarExpr(inPos, iterVarSym) as TIntVarExpr;

      end
      else if inExpr.typ is TSetOfSymbol then
      begin

        result := ReadForInSetOf(forPos, inExpr as TDataExpr, loopVarExpr,
          loopVarName, loopVarNamePos);
        if inExprAssignExpr <> nil then
        begin
          blockExpr := TBlockExpr.Create(FCompilerContext, forPos);
          blockExpr.AddStatement(inExprAssignExpr);
          blockExpr.AddStatement(result);
          result := blockExpr;
        end;
        Exit;

      end
      else
      begin

        if inExprAssignExpr <> nil then
        begin
          inExprAssignExpr.Orphan(FCompilerContext);
          inExprAssignExpr := nil;
        end;
        OrphanAndNil(loopVarExpr);
        if iterVarExpr <> nil then
        begin
          iterVarExpr.Orphan(FCompilerContext);
          iterVarExpr := nil;
        end;
        fromExpr := nil;
        toExpr := nil;
        OrphanAndNil(inExpr);
        FMsgs.AddCompilerStop(inPos, CPE_ArrayExpected);

      end;

    end;

  end
  else
  begin

    inExprPos := inExpr.scriptPos;
    enumSymbol := nil;
    if inExpr is TTypeReferenceExpr then
    begin
      if inExpr.typ.InheritsFrom(TEnumerationSymbol) then
      begin
        enumSymbol := TEnumerationSymbol(inExpr.typ);
      end;
    end;
    OrphanAndNil(inExpr);

    if enumSymbol = nil then
    begin
      FMsgs.AddCompilerError(inPos, CPE_EnumerationExpected);
      enumSymbol := FCompilerContext.TypInteger;
    end
    else
    begin
      RecordSymbolUse(enumSymbol, inExprPos, [suReference]);
    end;
    blockExpr := EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos,
      loopVarExpr, enumSymbol);

    if enumSymbol is TEnumerationSymbol then
    begin
      fromExpr := TConstIntExpr.Create(loopVarNamePos, loopVarExpr.typ,
        TEnumerationSymbol(enumSymbol).LowBound);
      toExpr := TConstIntExpr.Create(loopVarNamePos, loopVarExpr.typ,
        TEnumerationSymbol(enumSymbol).HighBound);
    end
    else
    begin
      fromExpr := TConstIntExpr.Create(loopVarNamePos, loopVarExpr.typ, 0);
      toExpr := TConstIntExpr.Create(loopVarNamePos, loopVarExpr.typ, 1);
    end;

    iterVarExpr := (loopVarExpr as TIntVarExpr);

  end;

  if inExprAssignExpr <> nil then
  begin
    if blockExpr = nil then
      blockExpr := TBlockExpr.Create(FCompilerContext, forPos);
    blockExpr.AddStatement(inExprAssignExpr);
  end;
  if blockExpr <> nil then
  begin
    if coContextMap in FOptions then
      FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttFOR);
    CurrentProg.EnterSubTable(blockExpr.table);
  end;
  result := blockExpr;
  try
    try
      result := ReadForStep(forPos, forExprClass, iterVarExpr, fromExpr, toExpr,
        readArrayItemExpr);
    except
      result := nil;
      OrphanObject(blockExpr);
      raise;
    end;
    if Optimize then
      result := result.Optimize(FCompilerContext);
  finally
    if blockExpr <> nil then
    begin
      CurrentProg.LeaveSubTable;
      if result <> nil then
      begin
        blockExpr.AddStatement(result);
        if Optimize then
          result := blockExpr.Optimize(FCompilerContext)
        else
          result := blockExpr;
      end
      else
        result := blockExpr;
      if coContextMap in FOptions then
      begin
        if blockExpr is TBlockExpr then
          FSourceContextMap.Current.LocalTable := TBlockExpr(blockExpr).table;
        FSourceContextMap.CloseContext(FTok.CurrentPos);
      end;
    end;
  end;
end;

// ReadForInString
//
function TdwsCompiler.ReadForInString(const forPos: TScriptPos;
  inExpr: TProgramExpr; loopVarExpr: TVarExpr; const loopVarName: String;
  const loopVarNamePos: TScriptPos): TProgramExpr;
var
  blockExpr: TBlockExpr;
  forInExpr: TForInStrExpr;
begin
  if not FTok.TestDelete(ttDO) then
  begin
    OrphanAndNil(inExpr);
    OrphanAndNil(loopVarExpr);
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_DoExpected);
  end;

  if loopVarExpr = nil then
  begin

    blockExpr := EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos,
      loopVarExpr, FCompilerContext.TypString);

    forInExpr := TForCharInStrExpr.Create(FCompilerContext, forPos,
      loopVarExpr as TStrVarExpr, TTypedExpr(inExpr));
    if coContextMap in options then
      FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttFOR);
    CurrentProg.EnterSubTable(blockExpr.table);
    EnterLoop(forInExpr);
    try
      MarkLoopExitable(leBreak);
      forInExpr.DoExpr := ReadBlock;
    finally
      LeaveLoop;
      CurrentProg.LeaveSubTable;
      if coContextMap in options then
      begin
        FSourceContextMap.Current.LocalTable := blockExpr.table;
        FSourceContextMap.CloseContext(FTok.CurrentPos);
      end;
      blockExpr.AddStatement(forInExpr);
      result := blockExpr;
    end;

  end
  else
  begin

    if loopVarExpr.typ.IsOfType(FCompilerContext.TypInteger) then
      forInExpr := TForCharCodeInStrExpr.Create(FCompilerContext, forPos,
        loopVarExpr as TIntVarExpr, TTypedExpr(inExpr))
    else
      forInExpr := TForCharInStrExpr.Create(FCompilerContext, forPos,
        loopVarExpr as TStrVarExpr, TTypedExpr(inExpr));
    EnterLoop(forInExpr);
    try
      MarkLoopExitable(leBreak);
      forInExpr.DoExpr := ReadBlock;
    finally
      LeaveLoop;
    end;
    result := forInExpr;

  end;
end;

// ReadForInSetOf
//
function TdwsCompiler.ReadForInSetOf(const forPos: TScriptPos;
  inExpr: TDataExpr; loopVarExpr: TVarExpr; const loopVarName: String;
  const loopVarNamePos: TScriptPos): TProgramExpr;
var
  setOfSymbol: TSetOfSymbol;
  elementTyp: TTypeSymbol;
  blockExpr: TBlockExpr;
  doBlock: TProgramExpr;
  loopVarInSetExpr: TSetOfInExpr;
  forExpr: TForUpwardExpr;
begin
  setOfSymbol := (inExpr.typ as TSetOfSymbol);
  elementTyp := setOfSymbol.typ;

  blockExpr := EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos,
    loopVarExpr, elementTyp);

  if not FTok.TestDelete(ttDO) then
    FMsgs.AddCompilerError(FTok.hotPos, CPE_DoExpected);

  if blockExpr <> nil then
    CurrentProg.EnterSubTable(blockExpr.table);

  loopVarInSetExpr := TSetOfInExpr.CreateOptimal(FCompilerContext, forPos,
    loopVarExpr, inExpr);
  loopVarExpr.IncRefCount;

  forExpr := TForUpwardExpr.Create(forPos);
  forExpr.fromExpr := TConstIntExpr.Create(inExpr.scriptPos, elementTyp,
    setOfSymbol.MinValue);
  forExpr.toExpr := TConstIntExpr.Create(inExpr.scriptPos, elementTyp,
    setOfSymbol.MaxValue);
  forExpr.varExpr := (loopVarExpr as TIntVarExpr);

  EnterLoop(forExpr);
  try
    MarkLoopExitable(leBreak);
    try
      doBlock := ReadBlock;
      var
      ifThenExpr := TIfThenExpr.Create(FCompilerContext, forPos,
        loopVarInSetExpr, doBlock);
      forExpr.DoExpr := ifThenExpr;
    except
      OrphanAndNil(blockExpr);
      OrphanAndNil(loopVarExpr);
      OrphanAndNil(inExpr);
      raise;
    end;
  finally
    LeaveLoop;
  end;

  if blockExpr <> nil then
  begin
    CurrentProg.LeaveSubTable;
    blockExpr.AddStatement(forExpr);
    if Optimize then
      result := blockExpr.Optimize(FCompilerContext)
    else
      result := blockExpr;
  end
  else
    result := forExpr;
end;

// ReadForInConnector
//
function TdwsCompiler.ReadForInConnector(const forPos: TScriptPos;
  inExpr: TTypedExpr; const inPos: TScriptPos; loopVarExpr: TVarExpr;
  const loopVarName: String; const loopVarNamePos: TScriptPos): TProgramExpr;
var
  connectorSymbol: TConnectorSymbol;
  enumerator: IConnectorEnumerator;
  itemType: TTypeSymbol;
  blockExpr: TBlockExpr;
  forInExpr: TConnectorForInExpr;
begin
  connectorSymbol := (inExpr.typ as TConnectorSymbol);

  enumerator := connectorSymbol.connectorType.HasEnumerator(itemType);
  if enumerator = nil then
  begin
    FMsgs.AddCompilerError(inPos, CPE_ArrayExpected);
    itemType := FCompilerContext.TypVariant;
  end;

  blockExpr := EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos,
    loopVarExpr, itemType);

  if not FTok.TestDelete(ttDO) then
    FMsgs.AddCompilerError(FTok.hotPos, CPE_DoExpected);

  forInExpr := TConnectorForInExpr.Create(forPos, enumerator,
    loopVarExpr, inExpr);
  result := forInExpr;

  if blockExpr <> nil then
    CurrentProg.EnterSubTable(blockExpr.table);
  EnterLoop(forInExpr);
  try
    MarkLoopExitable(leBreak);
    try
      forInExpr.DoExpr := ReadBlock;
    except
      forInExpr.Orphan(FCompilerContext);
      raise;
    end;
  finally
    LeaveLoop;

    if blockExpr <> nil then
    begin
      CurrentProg.LeaveSubTable;
      blockExpr.AddStatement(result);
      if Optimize then
        result := blockExpr.Optimize(FCompilerContext)
      else
        result := blockExpr;
    end;
  end;
end;

// WarnForVarUsage
//
procedure TdwsCompiler.WarnForVarUsage(varExpr: TVarExpr;
  const scriptPos: TScriptPos);
var
  i: Integer;
  loopExpr: TProgramExpr;
  currVarExpr: TVarExpr;
  varSymbol: TDataSymbol;
begin
  varSymbol := varExpr.dataSym;
  for i := 0 to FLoopExprs.Count - 1 do
  begin
    loopExpr := FLoopExprs.Items[i];
    if loopExpr.InheritsFrom(TForExpr) then
    begin
      currVarExpr := TForExpr(loopExpr).varExpr;
      if currVarExpr.ReferencesVariable(varSymbol) then
      begin
        FMsgs.AddCompilerWarning(scriptPos, CPE_AssignementToFORLoopVariable);
        Break;
      end;
    end;
  end;
end;

// ReadIf
//
function TdwsCompiler.ReadIf: TProgramExpr;
var
  hotPos: TScriptPos;
  condExpr: TTypedExpr;
  thenExpr: TProgramExpr;
  elseExpr: TProgramExpr;
begin
  hotPos := FTok.hotPos;

  condExpr := nil;
  thenExpr := nil;
  elseExpr := nil;
  try
    condExpr := ReadBooleanExpr;

    if not FTok.TestDelete(ttTHEN) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ThenExpected);

    if FTok.Test(ttSEMI) then
      FMsgs.AddCompilerHint(FTok.hotPos, CPH_EmptyThenBlock);

    if FTok.TestDelete(ttELSE) then
    begin // if () then else;

      FMsgs.AddCompilerHint(FTok.hotPos, CPH_EmptyThenBlock);
      condExpr := TNotBoolExpr.Create(FCompilerContext, FTok.hotPos, condExpr);
      thenExpr := ReadBlock;

    end
    else
    begin

      thenExpr := ReadBlock;
      if FTok.TestDelete(ttELSE) then
      begin
        if FTok.Test(ttSEMI) then
          FMsgs.AddCompilerHint(FTok.hotPos, CPH_EmptyElseBlock);
        elseExpr := ReadBlock;
        if Optimize and (elseExpr is TNullExpr) then
          FreeAndNil(elseExpr);
      end;

    end;

    if elseExpr = nil then
      result := TIfThenExpr.Create(FCompilerContext, hotPos, condExpr, thenExpr)
    else
      result := TIfThenElseExpr.Create(FCompilerContext, hotPos, condExpr,
        thenExpr, elseExpr);
  except
    OrphanAndNil(condExpr);
    OrphanAndNil(thenExpr);
    OrphanAndNil(elseExpr);
    raise;
  end;

  if Optimize then
    result := result.Optimize(FCompilerContext);
end;

// ReadCase
//
function TdwsCompiler.ReadCase: TCaseExpr;
var
  expr: TProgramExpr;
  condList: TTightList;
  condition: TRefCountedObject;
  tt: TTokenType;
begin
  condList.Initialize;
  try
    result := TCaseExpr.Create(FTok.hotPos);
    try
      result.valueExpr := ReadExpr;
      if result.valueExpr.typ = nil then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_ExpressionExpected);

      if not FTok.TestDelete(ttOF) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_OfExpected);

      while not FTok.TestDelete(ttEND) do
      begin
        if FTok.TestDelete(ttELSE) then
        begin
          if FTok.Test(ttBEGIN) then
            FMsgs.AddCompilerHint(FTok.hotPos, CPH_BeginInCaseOfElseClause,
              hlPedantic);
          result.elseExpr := ReadBlocks([ttEND], tt);
          Break;
        end
        else
        begin
          ReadCaseConditions(condList, result.valueExpr);

          if not FTok.TestDelete(ttCOLON) then
            FMsgs.AddCompilerStop(FTok.hotPos, CPE_ColonExpected);

          expr := ReadBlock;

          // Add case conditions to TCaseExpr
          for condition in condList do
          begin
            TCaseCondition(condition).TrueExpr := expr;
            expr.IncRefCount;
            result.AddCaseCondition(TCaseCondition(condition));
          end;
          expr.DecRefCount;
          condList.Clear;

          if not(FTok.Test(ttELSE) or FTok.Test(ttEND) or
            FTok.TestDelete(ttSEMI)) then
            FMsgs.AddCompilerStop(FTok.hotPos, CPE_SemiExpected);
        end;
      end;
    except
      result.Orphan(FCompilerContext);
      raise;
    end;
  finally
    condList.Clean;
  end;
end;

// ReadCaseConditions
//
function TdwsCompiler.ReadCaseConditions(var condList: TTightList;
  valueExpr: TTypedExpr): Integer;
var
  hotPos: TScriptPos;
  exprFrom, exprTo: TTypedExpr;
  condition: TCaseCondition;

  procedure CheckConstantRange;
  var
    vFrom, vTo: Variant;
  begin
    exprFrom.EvalAsVariant(FExec, vFrom);
    exprTo.EvalAsVariant(FExec, vTo);
    if VarCompareSafe(vFrom, vTo) = vrGreaterThan then
      FMsgs.AddCompilerHint(hotPos,
        CPH_CaseRangeLowerBoundIsGreaterThanHigherBound);
  end;

begin
  // Find a comma sparated list of case conditions  0, 1, 2..4: ;
  repeat

    hotPos := FTok.hotPos;
    exprFrom := ReadExpr;

    try
      if FTok.TestDelete(ttDOTDOT) then
      begin
        // range condition e. g. 0..12
        exprTo := ReadExpr;
        condition := TRangeCaseCondition.Create(hotPos, exprFrom, exprTo);
        if exprFrom.IsConstant and exprTo.IsConstant then
          CheckConstantRange;
      end
      else
      begin
        // compare condition e. g. 123:
        if exprFrom is TConstStringExpr then
        begin
          condition := TCompareConstStringCaseCondition.Create(hotPos,
            TConstStringExpr(exprFrom).value);
          OrphanAndNil(exprFrom);
        end
        else
        begin
          condition := TCompareCaseCondition.Create(hotPos, exprFrom);
        end;
      end;
      exprFrom := nil;
      condList.Add(condition);
      if valueExpr.typ <> nil then
        condition.TypeCheck(FCompilerContext, valueExpr.typ);
    except
      OrphanAndNil(exprFrom);
      raise;
    end;

  until not FTok.TestDelete(ttCOMMA);

  result := condList.Count;
end;

// ReadWhile
//
function TdwsCompiler.ReadWhile: TProgramExpr;
var
  condExpr: TTypedExpr;
begin
  result := TWhileExpr.Create(FTok.hotPos);
  EnterLoop(result);
  try
    condExpr := ReadBooleanExpr;
    TWhileExpr(result).condExpr := condExpr;

    if not FTok.TestDelete(ttDO) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_DoExpected);

    if (not condExpr.IsConstant) or
      (not condExpr.IsOfType(FCompilerContext.TypBoolean)) or
      (not condExpr.EvalAsBoolean(FExec)) then
      MarkLoopExitable(leBreak);

    TWhileExpr(result).loopExpr := ReadBlock;
  except
    OrphanAndNil(result);
    raise;
  end;
  LeaveLoop;

  if Optimize then
    result := result.Optimize(FCompilerContext);
end;

// ReadWith
//
function TdwsCompiler.ReadWith: TProgramExpr;
var
  DoExpr: TProgramExpr;
  closePos: TScriptPos;
  // Position at which the ending token was found (for context)
  blockExpr: TBlockExpr;
begin
  // Read a block of instructions enclosed in "begin" and "end"
  result := nil;
  blockExpr := TBlockExpr.Create(FCompilerContext, FTok.hotPos);
  try
    if coContextMap in FOptions then
    begin
      FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttWRITE);
      closePos.Clear;
    end;

    CurrentProg.EnterSubTable(blockExpr.table);
    try

      repeat
        ReadVarDecl(FStandardDataSymbolFactory, blockExpr);
        if not FTok.TestDelete(ttCOMMA) then
          Break;
      until False;

      if not FTok.TestDelete(ttDO) then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_DoExpected);

      DoExpr := ReadBlock;
      blockExpr.AddStatement(DoExpr);

      if (coContextMap in FOptions) and (DoExpr is TBlockExpr) then
        closePos := FSourceContextMap.Current.SubContext
          [FSourceContextMap.Current.SubContexts.Count - 1].EndPos;

      HintUnusedSymbols;

      if Optimize then
      begin
        result := blockExpr.Optimize(FCompilerContext);
        blockExpr := nil;
      end
      else
        result := blockExpr;
    finally
      CurrentProg.LeaveSubTable;
      if coContextMap in FOptions then
      begin
        if blockExpr <> nil then
          FSourceContextMap.Current.LocalTable := blockExpr.table
        else if result is TBlockExpr then
          FSourceContextMap.Current.LocalTable := TBlockExpr(result).table;
        if not closePos.Defined then
          closePos := FTok.CurrentPos; // means an error occured
        FSourceContextMap.CloseContext(closePos);
      end;
    end;

  except
    OrphanAndNil(blockExpr);
    raise;
  end;
end;

// ReadRepeat
//
function TdwsCompiler.ReadRepeat: TProgramExpr;
var
  tt: TTokenType;
  condExpr: TTypedExpr;
begin
  result := TRepeatExpr.Create(FTok.hotPos);
  EnterLoop(result);
  try
    TRepeatExpr(result).loopExpr := ReadBlocks([ttUNTIL], tt);
    TRepeatExpr(result).SetScriptPos(FTok.hotPos);
    condExpr := ReadBooleanExpr;
    TRepeatExpr(result).condExpr := condExpr;
    if (not condExpr.IsConstant) or condExpr.EvalAsBoolean(FExec) or FMsgs.HasErrors
    then
      MarkLoopExitable(leBreak);
  except
    OrphanAndNil(result);
    raise;
  end;
  LeaveLoop;

  if Optimize then
    result := result.Optimize(FCompilerContext);
end;

// ReadAssign
//
function TdwsCompiler.ReadAssign(token: TTokenType; var left: TDataExpr)
  : TProgramExpr;
var
  hotPos: TScriptPos;
  right: TTypedExpr;

  procedure CheckAssigningToSelf;
  var
    leftSymbol: TSymbol;
  begin
    if left.ClassType = right.ClassType then
    begin
      if left is TVarExpr then
      begin
        leftSymbol := TDataExpr(left).DataSymbol;
        if (leftSymbol <> nil) and (leftSymbol = TDataExpr(right).DataSymbol)
        then
          FMsgs.AddCompilerHintFmt(hotPos, CPH_AssigningToItself,
            [leftSymbol.QualifiedName])
      end
      else if left is TFieldExpr then
      begin
        leftSymbol := TFieldExpr(left).fieldSym;
        if (leftSymbol <> nil) and (leftSymbol = TFieldExpr(right).fieldSym) and
          TFieldExpr(left).ObjectExpr.SameDataExpr(TFieldExpr(right).ObjectExpr)
        then
        begin
          FMsgs.AddCompilerHintFmt(hotPos, CPH_AssigningToItself,
            [leftSymbol.QualifiedName]);
        end;
      end;
    end;
  end;

begin
  hotPos := FTok.hotPos;
  right := nil;
  try
    right := ReadExpr(left.typ);
    // only check self assignment when no error have occurred, it is too sensitive otherwise
    if (token = ttASSIGN) and not FMsgs.HasErrors then
      CheckAssigningToSelf;

    result := CreateAssign(hotPos, token, left, right);
    left := nil;
  except
    OrphanAndNil(left);
    OrphanAndNil(right);
    raise;
  end;
end;

// ReadSelfMethod
//
function TdwsCompiler.ReadSelfMethod(methodSym: TMethodSymbol; isWrite: Boolean;
  expecting: TTypeSymbol; overloads: TFuncSymbolList;
  options: TCreateFunctionOptions): TTypedExpr;
var
  progMeth: TMethodSymbol;
  structSym: TCompositeTypeSymbol;
  selfExpr: TTypedExpr;
  RefKind: TRefKind;
begin
  progMeth := CurrentProg.ContextMethodSymbol;

  if progMeth <> nil then
  begin
    if progMeth.IsStatic then
    begin
      structSym := progMeth.StructSymbol;
      selfExpr := TConstIntExpr.Create(FTok.hotPos, structSym.MetaSymbol,
        Int64(structSym));
      RefKind := rkClassOfRef;
    end
    else if progMeth.selfSym is TConstByRefParamSymbol then
    begin
      selfExpr := GetConstByRefParamExpr
        (TConstByRefParamSymbol(progMeth.selfSym));
      RefKind := rkObjRef;
    end
    else if progMeth.selfSym = nil then
    begin
      selfExpr := nil;
      RefKind := rkClassOfRef;
    end
    else
    begin
      selfExpr := GetSelfParamExpr(FTok.hotPos, progMeth.selfSym);
      RefKind := rkObjRef;
    end;
  end
  else
  begin
    structSym := methodSym.StructSymbol;
    selfExpr := TConstIntExpr.Create(FTok.hotPos, structSym.MetaSymbol,
      Int64(structSym));
    RefKind := rkClassOfRef;
  end;

  if overloads <> nil then
    result := TOverloadedExpr.Create(CompilerContext, FTok.hotPos,
      methodSym, selfExpr)
  else
    result := GetMethodExpr(methodSym, selfExpr, RefKind, FTok.hotPos, options);

  result := WrapUpFunctionRead(TFuncExpr(result), expecting, overloads,
    options);
end;

// ReadMethod
//
function TdwsCompiler.ReadMethod(methodSym: TMethodSymbol;
  instanceExpr: TTypedExpr; const scriptPos: TScriptPos;
  expecting: TTypeSymbol = nil; overloads: TFuncSymbolList = nil): TTypedExpr;
var
  funcExpr: TFuncExprBase;
begin
  if overloads <> nil then
    funcExpr := TOverloadedExpr.Create(CompilerContext, scriptPos, methodSym,
      instanceExpr)
  else if methodSym.IsClassMethod then
    funcExpr := GetMethodExpr(methodSym, instanceExpr, rkClassOfRef,
      scriptPos, [])
  else
  begin
    funcExpr := GetMethodExpr(methodSym, instanceExpr, rkObjRef, scriptPos, []);
  end;
  result := WrapUpFunctionRead(funcExpr, expecting, overloads, []);
end;

// ReadStaticMethod
//
function TdwsCompiler.ReadStaticMethod(methodSym: TMethodSymbol;
  metaExpr: TTypedExpr; const scriptPos: TScriptPos;
  expecting: TTypeSymbol = nil; overloads: TFuncSymbolList = nil): TTypedExpr;
var
  funcExpr: TFuncExprBase;
  compoSym: TCompositeTypeSymbol;
begin
  if methodSym.Kind = fkConstructor then
  begin
    compoSym := (metaExpr.typ as TStructuredTypeMetaSymbol).StructSymbol;
    if compoSym.IsStatic then
      FMsgs.AddCompilerErrorFmt(scriptPos, CPE_ClassIsStaticNoInstantiation,
        [compoSym.name]);
  end;
  if overloads <> nil then
    funcExpr := TOverloadedExpr.Create(CompilerContext, scriptPos,
      methodSym, metaExpr)
  else
    funcExpr := GetMethodExpr(methodSym, metaExpr, rkClassOfRef, scriptPos, []);
  result := WrapUpFunctionRead(funcExpr, expecting, overloads, []);
end;

type
  TFuncAtLevelSymbolList = class(TFuncSymbolList)
  public
    level: Integer;
    function Callback(sym: TSymbol): Boolean;
  end;

function TFuncAtLevelSymbolList.Callback(sym: TSymbol): Boolean;
var
  locFuncSym: TFuncSymbol;
begin
  locFuncSym := sym.AsFuncSymbol;
  if locFuncSym <> nil then
  begin
    if locFuncSym.level = level then
      Add(locFuncSym);
  end;
  result := False;
end;

// ReadFuncOverloaded
//
function TdwsCompiler.ReadFuncOverloaded(funcSym: TFuncSymbol;
  fromTable: TSymbolTable; codeExpr: TDataExpr = nil;
  expecting: TTypeSymbol = nil): TTypedExpr;
var
  overloads: TFuncAtLevelSymbolList;
begin
  overloads := TFuncAtLevelSymbolList.Create;
  try
    overloads.level := funcSym.level;
    fromTable.EnumerateSymbolsOfNameInScope(funcSym.name, overloads.Callback);
    result := ReadFunc(funcSym, codeExpr, expecting, overloads);
  finally
    overloads.Free;
  end;
end;

// CollectMethodOverloads
//
procedure TdwsCompiler.CollectMethodOverloads(methSym: TMethodSymbol;
  overloads: TFuncSymbolList);
var
  member: TSymbol;
  struct: TCompositeTypeSymbol;
  lastOverloaded: TMethodSymbol;
  visibility: TdwsVisibility;
begin
  lastOverloaded := methSym;
  struct := methSym.StructSymbol;
  visibility := struct.Members.VisibilityFromScope(CurrentStruct);
  repeat
    for member in struct.Members do
    begin
      if not UnicodeSameText(member.name, methSym.name) then
        continue;
      if not member.InheritsFrom(TMethodSymbol) then
        continue;
      if not member.IsVisibleFor(visibility) then
        continue;
      lastOverloaded := TMethodSymbol(member);
      if not overloads.ContainsChildMethodOf(lastOverloaded) then
        overloads.Add(lastOverloaded);
    end;
    struct := struct.Parent;
  until (struct = nil) or not lastOverloaded.IsOverloaded;
end;

// ReadSelfMethOverloaded
//
function TdwsCompiler.ReadSelfMethOverloaded(methSym: TMethodSymbol;
  isWrite: Boolean; expecting: TTypeSymbol; options: TCreateFunctionOptions)
  : TTypedExpr;
var
  overloads: TFuncSymbolList;
begin
  overloads := CompilerContext.AllocateFuncSymbolList;
  try
    CollectMethodOverloads(methSym, overloads);
    result := ReadSelfMethod(methSym, isWrite, expecting, overloads, options);
  finally
    CompilerContext.ReleaseFuncSymbolList(overloads);
  end;
end;

// ReadMethOverloaded
//
function TdwsCompiler.ReadMethOverloaded(methSym: TMethodSymbol;
  instanceExpr: TTypedExpr; const scriptPos: TScriptPos;
  expecting: TTypeSymbol = nil): TTypedExpr;
var
  overloads: TFuncSymbolList;
begin
  overloads := CompilerContext.AllocateFuncSymbolList;
  try
    CollectMethodOverloads(methSym, overloads);
    result := ReadMethod(methSym, instanceExpr, scriptPos, expecting,
      overloads);
  finally
    CompilerContext.ReleaseFuncSymbolList(overloads);
  end;
end;

// ReadStaticMethOverloaded
//
function TdwsCompiler.ReadStaticMethOverloaded(methSym: TMethodSymbol;
  metaExpr: TTypedExpr; const scriptPos: TScriptPos;
  expecting: TTypeSymbol = nil): TTypedExpr;
var
  i: Integer;
  overloads: TFuncSymbolList;
  meth: TMethodSymbol;
begin
  overloads := CompilerContext.AllocateFuncSymbolList;
  try
    CollectMethodOverloads(methSym, overloads);
    for i := overloads.Count - 1 downto 0 do
    begin
      meth := (overloads[i] as TMethodSymbol);
      case meth.Kind of
        fkFunction, fkProcedure, fkMethod:
          if not meth.IsClassMethod then
            overloads.Extract(i);
        fkDestructor:
          overloads.Extract(i);
      end;
    end;
    if overloads.Count = 0 then
      FMsgs.AddCompilerStop(scriptPos, CPE_StaticMethodExpected);
    methSym := TMethodSymbol(overloads[0]);
    result := ReadStaticMethod(methSym, metaExpr, scriptPos, expecting,
      overloads);
  finally
    CompilerContext.ReleaseFuncSymbolList(overloads);
  end;
end;

// ResolveOverload
//
function TdwsCompiler.ResolveOverload(var funcExpr: TFuncExprBase;
  overloads: TFuncSymbolList; const argPosArray: TScriptPosArray;
  expecting: TFuncSymbol; cfOptions: TCreateFunctionOptions): Boolean;
var
  i, delta: Integer;
  j: Integer;
  funcExprArgCount: Integer;
  match, bestMatch: TFuncSymbol;
  struct: TCompositeTypeSymbol;
  matchDistance, bestMatchDistance, bestCount: Integer;
  matchParamType, funcExprParamType: TTypeSymbol;
  wasVarParam, nowVarParam: Boolean;
  funcExprArg: TExprBase;
begin
  bestMatch := nil;
  bestCount := 0;
  bestMatchDistance := MaxInt;
  if expecting <> nil then
    funcExprArgCount := expecting.Params.Count
  else
    funcExprArgCount := funcExpr.args.Count;
  for i := 0 to overloads.Count - 1 do
  begin
    match := overloads[i];
    if funcExprArgCount > match.Params.Count then
      continue;
    matchDistance := 0;
    for j := 0 to funcExprArgCount - 1 do
    begin
      matchParamType := match.GetParamType(j);
      if expecting <> nil then
        funcExprParamType := expecting.Params[j].typ
      else
        funcExprParamType := funcExpr.GetArgType(j);
      if not matchParamType.IsOfType(funcExprParamType) then
      begin

        if match.ParamTypeForbidsImplicitCasts(j) then
        begin

          match := nil;
          Break;

        end
        else if funcExprParamType.IsOfType(FCompilerContext.TypVariant) then
        begin

          if not funcExprParamType.IsCompatible(matchParamType) then
          begin
            match := nil;
            Break;
          end
          else
          begin
            // disfavor variant promotion to integer
            if matchParamType.IsOfType(FCompilerContext.TypInteger) then
              Inc(matchDistance, 256)
            else
              Inc(matchDistance, 1);
          end;
          (*
            end else if matchParamType.IsOfType(FCompilerContext.TypVariant) then begin

            if not matchParamType.IsCompatible(funcExprParamType) then begin
            match := nil;
            break;
            end else begin
            // disfavor casts to variants
            Inc(matchDistance, 256);
            end; *)

        end
        else if (funcExprParamType is TStaticArraySymbol) and
          (matchParamType is TDynamicArraySymbol) then
        begin

          if funcExprParamType.typ.IsOfType(matchParamType.typ) then
          begin

            if funcExprParamType.typ.IsClassSymbol or
              (funcExprParamType.typ is TInterfaceSymbol) then
            begin

              Inc(matchDistance, (matchParamType.typ as TStructuredTypeSymbol)
                .NthParentOf(TStructuredTypeSymbol(funcExprParamType.typ)));

            end
            else
            begin

              Inc(matchDistance, 1);

            end;

          end
          else if matchParamType.typ.IsOfType(FCompilerContext.TypVariant) and
            matchParamType.typ.IsCompatible(funcExprParamType.typ) then
          begin

            // disfavor promotion to variant
            Inc(matchDistance, 128);

          end
          else if (TStaticArraySymbol(funcExprParamType).ElementCount = 0) or
            ((funcExprParamType.typ = FCompilerContext.TypNil) and
            (matchParamType.typ.IsClassSymbol or
            (matchParamType.typ is TInterfaceSymbol))) then
          begin

            Inc(matchDistance, 1);

          end
          else
          begin

            match := nil;
            Break;

          end;

        end
        else if matchParamType.IsOfType(FCompilerContext.TypFloat) and
          funcExprParamType.IsOfType(FCompilerContext.TypInteger) then
        begin

          Inc(matchDistance, 1);

        end
        else if not matchParamType.IsCompatible(funcExprParamType) then
        begin

          match := nil;
          Break;

        end
        else
        begin

          if funcExprParamType.IsClassSymbol or
            (funcExprParamType is TInterfaceSymbol) then
          begin

            if matchParamType is TStructuredTypeSymbol then
              Inc(matchDistance, (matchParamType as TStructuredTypeSymbol)
                .NthParentOf(TStructuredTypeSymbol(funcExprParamType)))
            else
              Inc(matchDistance, 256);

          end
          else if matchParamType.IsOfType(FCompilerContext.TypVariant) then
          begin

            // disfavor promotion to variant
            Inc(matchDistance, 128);

          end
          else
          begin

            Inc(matchDistance, 1);

          end;

        end;
      end;
    end;
    if match = nil then
      continue;
    for j := funcExprArgCount to match.Params.Count - 1 do
    begin
      if match.Params[j].ClassType <> TParamSymbolWithDefaultValue then
      begin
        match := nil;
        Break;
      end;
    end;
    if match = nil then
      continue;

    if match is TMethodSymbol then
    begin
      // for method symbols give precedence to the deepest subclass
      // this will only differentiate matches that rated the same on parameters
      matchDistance := (matchDistance + 1) shl 16;
      struct := TMethodSymbol(match).StructSymbol;
      while struct <> nil do
      begin
        Dec(matchDistance);
        struct := struct.Parent;
      end;
    end;

    if matchDistance <= bestMatchDistance then
    begin
      if matchDistance < bestMatchDistance then
      begin
        bestMatch := match;
        bestMatchDistance := matchDistance;
        bestCount := 1;
      end
      else
        Inc(bestCount);
    end;
  end;
  if bestMatch <> nil then
  begin
    if bestCount > 1 then
    begin
      FMsgs.AddCompilerHintFmt(funcExpr.scriptPos,
        CPH_AmbiguousMatchingOverloadsForCall, [funcExpr.funcSym.name]);
    end;

    if coSymbolDictionary in options then
      RecordSymbolUseReference(bestMatch, funcExpr.scriptPos, False);

    if bestMatch.ClassType = TAliasMethodSymbol then
    begin
      bestMatch := TAliasMethodSymbol(bestMatch).Alias;
      if coSymbolDictionary in options then
        RecordSymbolUseImplicitReference(bestMatch, funcExpr.scriptPos, False);
    end;

    if coSymbolDictionary in options then
    begin
      RecordSymbolUseReference(bestMatch, funcExpr.scriptPos, False);
      delta := funcExpr.args.Count - funcExpr.funcSym.Params.Count;
      if delta >= 0 then
      begin
        for i := 0 to Min(bestMatch.Params.Count, funcExpr.args.Count -
          delta) - 1 do
        begin
          nowVarParam := (bestMatch.Params[i].ClassType = TVarParamSymbol);
          funcExprArg := funcExpr.args[i + delta];
          wasVarParam := (funcExprArg is TByRefParamExpr) and
            TByRefParamExpr(funcExprArg).IsWritable;
          if wasVarParam <> nowVarParam then
          begin
            if wasVarParam then
              FSymbolDictionary.ChangeUsageAt(argPosArray[i], [], [suWrite])
            else
              FSymbolDictionary.ChangeUsageAt(argPosArray[i], [suWrite], []);
          end;
        end;
      end;
    end;
    funcExpr := funcExpr.ChangeFuncSymbol(FCompilerContext, bestMatch,
      cfOptions);
    result := True;
  end
  else
  begin
    FMsgs.AddCompilerErrorFmt(funcExpr.scriptPos, CPE_NoMatchingOverloadForCall,
      [funcExpr.funcSym.name]);
    result := False;
  end;
end;

type
  TFuncConflictEnumerator = class
    forwardedSym, funcSym: TFuncSymbol;
    function Callback(sym: TSymbol): Boolean;
  end;

function TFuncConflictEnumerator.Callback(sym: TSymbol): Boolean;
var
  locSym: TFuncSymbol;
begin
  if sym <> forwardedSym then
  begin
    locSym := sym.AsFuncSymbol;
    if locSym <> nil then
      if locSym.level = funcSym.level then
        if not funcSym.IsValidOverloadOf(locSym) then
          Exit(True);
  end;
  result := False;
end;

// FuncHasConflictingOverload
//
function TdwsCompiler.FuncHasConflictingOverload(funcSym,
  forwardedSym: TFuncSymbol): Boolean;
var
  enumerator: TFuncConflictEnumerator;
begin
  enumerator := TFuncConflictEnumerator.Create;
  try
    enumerator.forwardedSym := forwardedSym;
    enumerator.funcSym := funcSym;
    result := CurrentProg.table.EnumerateSymbolsOfNameInScope(funcSym.name,
      enumerator.Callback);
  finally
    enumerator.Free;
  end;
end;

// CheckMethodOverloads
//
function TdwsCompiler.CheckMethodOverloads(newMethSym: TMethodSymbol): Boolean;
var
  struct: TCompositeTypeSymbol;
  member: TSymbol;
  memberMeth: TMethodSymbol;
begin
  struct := newMethSym.StructSymbol;
  for member in struct.Members do
  begin
    if not UnicodeSameText(member.name, newMethSym.name) then
      continue;
    if not(member is TMethodSymbol) then
      continue;
    memberMeth := TMethodSymbol(member);
    if not newMethSym.IsValidOverloadOf(memberMeth) then
      Exit(True);
    if not memberMeth.IsOverloaded then
    begin
      ReportImplicitMethodOverload(memberMeth.DeclarationPosition,
        memberMeth.name);
      memberMeth.IsOverloaded := True;
    end;
  end;
  result := False;
end;

// FuncPerfectMatchOverload
//
function TdwsCompiler.FuncPerfectMatchOverload(funcSym: TFuncSymbol)
  : TFuncSymbol;
var
  enumerator: TPerfectMatchEnumerator;
begin
  enumerator := TPerfectMatchEnumerator.Create;
  try
    enumerator.funcSym := funcSym;
    CurrentProg.table.EnumerateSymbolsOfNameInScope(funcSym.name,
      enumerator.Callback);
    result := enumerator.match;
  finally
    enumerator.Free;
  end;
end;

// MethPerfectMatchOverload
//
function TdwsCompiler.MethPerfectMatchOverload(methSym: TMethodSymbol;
  recurse: Boolean): TMethodSymbol;
var
  struct: TCompositeTypeSymbol;
  member: TSymbol;
  locSym: TMethodSymbol;
begin
  locSym := methSym;
  struct := methSym.StructSymbol;
  repeat
    for member in struct.Members do
    begin
      if not UnicodeSameText(member.name, methSym.name) then
        continue;
      if not(member is TMethodSymbol) then
        continue;
      locSym := TMethodSymbol(member);
      if methSym.IsSameOverloadOf(locSym) then
        Exit(locSym);
    end;
    struct := struct.Parent;
  until (not recurse) or (struct = nil) or (not locSym.IsOverloaded);
  result := nil;
end;

// ReadFunc
//
function TdwsCompiler.ReadFunc(funcSym: TFuncSymbol; codeExpr: TTypedExpr = nil;
  expecting: TTypeSymbol = nil; overloads: TFuncSymbolList = nil): TTypedExpr;
var
  funcExpr: TFuncExprBase;
begin
  if funcSym.IsExternal then
  begin
    if funcSym is TMagicFuncSymbol then
    begin
      // jitted external call
      Assert(Assigned(FExternalRoutinesManager));
      funcSym.Executable :=
        FExternalRoutinesManager.CreateExternalFunction(funcSym);
      TMagicFuncSymbol(funcSym).InternalFunction :=
        funcSym.Executable.GetSelf as TInternalMagicFunction;
      TMagicFuncSymbol(funcSym).InternalFunction.IncRefCount;
    end
    else
    begin
      // abstract external call
      funcSym.Executable := TExternalFuncHandler.Create;
    end;
  end;

  if overloads = nil then
    funcExpr := GetFuncExpr(funcSym, codeExpr)
  else
    funcExpr := TOverloadedExpr.Create(CompilerContext, FTok.hotPos,
      funcSym, nil);
  result := WrapUpFunctionRead(funcExpr, expecting, overloads, []);

  if Optimize then
    result := result.OptimizeToTypedExpr(FCompilerContext, result.scriptPos);
end;

// WrapUpFunctionRead
//
function TdwsCompiler.WrapUpFunctionRead(funcExpr: TFuncExprBase;
  expecting: TTypeSymbol; overloads: TFuncSymbolList;
  cfOptions: TCreateFunctionOptions): TTypedExpr;
var
  argPosArray: TScriptPosArray;
begin
  result := funcExpr;
  try
    if FTok.Test(ttBLEFT) then
    begin
      ReadFuncArgs(funcExpr, argPosArray, overloads);
      if overloads <> nil then
      begin
        if not ResolveOverload(funcExpr, overloads, argPosArray, nil, cfOptions)
        then
        begin
          result := TErrorValueExpr.Create(FSystemTable.TypAnyType);
          funcExpr.Orphan(FCompilerContext);
          Exit;
        end;
        result := funcExpr;
      end;
      TypeCheckArguments(FCompilerContext, funcExpr, argPosArray);
    end
    else
    begin
      if ((expecting is TNilSymbol) and (funcExpr is TFuncPtrExpr) and
        not FTok.Test(ttDOT)) or ((expecting.AsFuncSymbol <> nil) and
        funcExpr.funcSym.IsCompatible(expecting)) then
      begin
        if (funcExpr.funcSym.level > 1) and not(coAllowClosures in options) then
          FMsgs.AddCompilerError(funcExpr.scriptPos,
            CPE_LocalFunctionAsDelegate);
        result := TFuncRefExpr.Create(FCompilerContext, funcExpr);
      end
      else
      begin
        if overloads <> nil then
        begin
          if (funcExpr.args.Count = 0) and (expecting.AsFuncSymbol <> nil) then
          begin
            if not ResolveOverload(funcExpr, overloads, argPosArray,
              expecting.AsFuncSymbol, cfOptions) then
              Exit;
            if (funcExpr.funcSym.level > 1) and not(coAllowClosures in options)
            then
              FMsgs.AddCompilerError(funcExpr.scriptPos,
                CPE_LocalFunctionAsDelegate);
            result := TFuncRefExpr.Create(FCompilerContext, funcExpr);
            Exit;
          end
          else
          begin
            if not ResolveOverload(funcExpr, overloads, argPosArray, nil,
              cfOptions) then
              Exit;
          end;
          result := funcExpr;
        end;
        TypeCheckArguments(FCompilerContext, funcExpr, nil);
      end;
    end;
    WarnDeprecatedFunc(funcExpr);
  except
    OrphanAndNil(result);
    raise;
  end;
end;

// ReadFuncResultType
//
function TdwsCompiler.ReadFuncResultType(funcKind: TFuncKind): TTypeSymbol;
begin
  result := nil;
  if FTok.TestDelete(ttCOLON) then
  begin
    if not(funcKind in [fkFunction, fkMethod, fkLambda]) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_NoResultTypeExpected);
    result := ReadType('', tcResult);
  end
  else if funcKind = fkFunction then
  begin
    FMsgs.AddCompilerError(FTok.hotPos, CPE_FunctionTypeExpected);
    result := FCompilerContext.TypVariant;
  end;
end;

// ReadFuncArgs
//
procedure TdwsCompiler.ReadFuncArgs(funcExpr: TFuncExprBase;
  var argPosArray: TScriptPosArray; overloads: TFuncSymbolList);

  procedure ReadOverloaded;
  var
    helper: TFuncExprOverloadsHelper;
  begin
    helper := TFuncExprOverloadsHelper.Create(funcExpr, overloads);
    try
      ReadArguments(funcExpr.AddArg, ttBLEFT, ttBRIGHT, argPosArray,
        helper.ExpectedArg)
    finally
      helper.Free;
    end;
  end;

begin
  if (overloads <> nil) and (overloads.Count > 1) then
    ReadOverloaded
  else
    ReadArguments(funcExpr.AddArg, ttBLEFT, ttBRIGHT, argPosArray,
      funcExpr.ExpectedArg);
end;

// ReadArguments
//
procedure TdwsCompiler.ReadArguments(const addArgProc: TAddArgProcedure;
  leftDelim, rightDelim: TTokenType; var argPosArray: TScriptPosArray;
  const expectedProc: TExpectedArgFunction = nil);
var
  arg: TTypedExpr;
  argSym: TParamSymbol;
  argPos: TScriptPos;
  expectedType: TTypeSymbol;
  funcSym: TFuncSymbol;
  n: Integer;
begin
  if FTok.TestDelete(leftDelim) then
  begin
    if not FTok.TestDelete(rightDelim) then
    begin
      // At least one argument was found
      repeat
        argPos := FTok.hotPos;

        if Assigned(expectedProc) then
          argSym := expectedProc()
        else
          argSym := nil;
        if argSym <> nil then
          expectedType := argSym.typ
        else
          expectedType := nil;

        if (argSym <> nil) and (argSym.ClassType = TVarParamSymbol) then
          arg := ReadTerm(True, expectedType)
        else
          arg := ReadExpr(expectedType);

        if arg <> nil then
        begin
          if Optimize then
            arg := arg.OptimizeToTypedExpr(FCompilerContext, argPos);

          if expectedType <> nil then
          begin
            funcSym := arg.typ.AsFuncSymbol;
            if (funcSym <> nil) and (expectedType.AsFuncSymbol = nil) then
            begin
              arg := ReadFunc(funcSym, arg as TDataExpr, nil);
            end;
          end;

          addArgProc(arg);
          n := Length(argPosArray);
          SetLength(argPosArray, n + 1);
          argPosArray[n] := argPos;

          if (argSym <> nil) and (argSym.ClassType = TVarParamSymbol) and
            (arg is TVarExpr) then
            WarnForVarUsage(TVarExpr(arg), argPos);
        end;
      until not(FTok.TestDelete(ttCOMMA) and FTok.HasTokens);
      if not FTok.TestDelete(rightDelim) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);
    end;
  end;
end;

// ReadSetOfType
//
function TdwsCompiler.ReadSetOfType(const typeName: String;
  typeContext: TdwsReadTypeContext): TSetOfSymbol;
var
  elementType: TTypeSymbol;
  aMin, aMax: Integer;
  typePos: TScriptPos;
begin
  if not FTok.TestDelete(ttOF) then
    FMsgs.AddCompilerError(FTok.hotPos, CPE_OfExpected);

  typePos := FTok.hotPos;
  elementType := ReadType('', typeContext);
  aMin := 0;
  aMax := 0;

  if elementType.UnAliasedTypeIs(TEnumerationSymbol) then
  begin

    aMax := TEnumerationSymbol(elementType.UnAliasedType).HighBound;

  end
  else
    FMsgs.AddCompilerError(typePos, CPE_EnumerationExpected);

  result := TSetOfSymbol.Create(typeName, elementType, aMin, aMax);
end;

// ReadArrayType
//
function TdwsCompiler.ReadArrayType(const typeName: String;
  typeContext: TdwsReadTypeContext): TTypeSymbol;
var
  hotPos: TScriptPos;

  function CheckBound(bound: TTypedExpr): Boolean;
  begin
    if bound = nil then
      result := False // error message already signaled
    else if (bound.typ = nil) or
      not(bound.typ.IsOfType(FCompilerContext.TypInteger) or
      (bound.typ is TEnumerationSymbol) or
      bound.typ.IsOfType(FCompilerContext.TypBoolean)) then
    begin
      FMsgs.AddCompilerError(hotPos, CPE_ArrayBoundNotOrdinal);
      result := False;
    end
    else if not bound.IsConstant then
    begin
      FMsgs.AddCompilerError(hotPos, CPE_ArrayBoundNotAConstant);
      result := False;
    end
    else
      result := True;
  end;

var
  x: Integer;
  Min, max: TTypedExprList;
  typ: TTypeSymbol;
  boundsOk: Boolean;
  LowBound: TTypedExpr;
  enumSymbol: TEnumerationSymbol;
begin
  boundsOk := True;
  Min := TTypedExprList.Create;
  max := TTypedExprList.Create;
  try

    if FTok.TestDelete(ttALEFT) then
    begin

      repeat
        // Lower bound
        hotPos := FTok.hotPos;
        LowBound := ReadExpr(FCompilerContext.TypAnyType);

        if LowBound is TTypeReferenceExpr then
        begin

          // handle "array [someType] of" special cases

          if TTypeReferenceExpr(LowBound).typ is TBaseBooleanSymbol then
          begin

            Min.Insert0(FUnifiedConstants.CreateBoolean(False));
            max.Insert0(FUnifiedConstants.CreateBoolean(True));

          end
          else if TTypeReferenceExpr(LowBound).typ is TEnumerationSymbol then
          begin

            enumSymbol := TEnumerationSymbol(TTypeReferenceExpr(LowBound).typ);

            Min.Insert0(TConstIntExpr.Create(hotPos, enumSymbol,
              enumSymbol.LowBound));
            max.Insert0(TConstIntExpr.Create(hotPos, enumSymbol,
              enumSymbol.HighBound));

          end
          else if (Min.Count = 0) and FTok.TestDelete(ttARIGHT) then
          begin

            try
              result := ReadAssociativeArrayType(typeName,
                TTypeReferenceExpr(LowBound).typ, typeContext);
            finally
              OrphanAndNil(LowBound);
            end;
            Exit;

          end
          else
          begin

            FMsgs.AddCompilerError(FTok.hotPos, CPE_ArrayBoundNotOrdinal)

          end;
          OrphanAndNil(LowBound);

        end
        else if (Min.Count = 0) and (LowBound.typ is TStructuredTypeMetaSymbol)
          and FTok.TestDelete(ttARIGHT) then
        begin
          try
            result := ReadAssociativeArrayType(typeName, LowBound.typ.typ,
              typeContext);
          finally
            OrphanAndNil(LowBound);
          end;
          Exit;

        end
        else
        begin

          // handle "array [low..high] of" normal case

          Min.Insert0(LowBound);

          boundsOk := boundsOk and CheckBound(Min[0]);

          if not FTok.TestDelete(ttDOTDOT) then
            FMsgs.AddCompilerStop(FTok.hotPos, CPE_DotDotExpected);

          // Upper bound
          hotPos := FTok.hotPos;
          max.Insert0(ReadExpr);

          boundsOk := boundsOk and CheckBound(max[0]);

          if boundsOk and (max[0].typ <> Min[0].typ) then
            FMsgs.AddCompilerError(hotPos, CPE_ArrayBoundsOfDifferentTypes);

          if boundsOk and
            (max[0].EvalAsInteger(FExec) < Min[0].EvalAsInteger(FExec)) then
          begin
            FMsgs.AddCompilerError(hotPos, CPE_LowerBoundGreaterThanUpperBound);
            // keep compiling
            max[0].Orphan(FCompilerContext);
            max[0] := FUnifiedConstants.CreateInteger
              (Min[0].EvalAsInteger(FExec));
          end;

        end;

        if FTok.Test(ttARIGHT) then
          Break;
      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttARIGHT) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_ArrayBracketRightExpected);
    end;

    if not FTok.TestDelete(ttOF) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_OfExpected);

    if FTok.TestDelete(ttCONST) then
    begin

      if not(typeContext in [tcDeclaration, tcParameter, tcOperand]) then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_TypeExpected);
      if Min.Count > 0 then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_NoIndicesExpectedForOpenArray);

      result := TOpenArraySymbol.Create(typeName, FCompilerContext.TypVariant,
        FCompilerContext.TypInteger);

    end
    else
    begin

      typ := ReadType('', typeContext);

      if boundsOk and (Min.Count > 0) then
      begin

        if typ.ClassType = TRecordSymbol then
          if not TRecordSymbol(typ).IsFullyDefined then
            FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_RecordTypeNotFullyDefined,
              [typ.name]);

        // initialize innermost array
        result := TStaticArraySymbol.Create('', typ, Min[0].typ,
          Min[0].EvalAsInteger(FExec), max[0].EvalAsInteger(FExec));
        // add outer arrays
        for x := 1 to Min.Count - 1 do
        begin
          CurrentProg.RootTable.AddToDestructionList(result);
          result := TStaticArraySymbol.Create('', result, Min[x].typ,
            Min[x].EvalAsInteger(FExec), max[x].EvalAsInteger(FExec));
        end;

        // only outermost array is named
        result.SetName(typeName);

      end
      else
      begin

        result := TDynamicArraySymbol.Create(typeName, typ,
          FCompilerContext.TypInteger);

      end;

    end;

  finally
    Min.Orphan(FCompilerContext);
    max.Orphan(FCompilerContext);
  end;
end;

// ReadAssociativeArrayType
//
function TdwsCompiler.ReadAssociativeArrayType(const typeName: String;
  keyType: TTypeSymbol; typeContext: TdwsReadTypeContext)
  : TAssociativeArraySymbol;
var
  elementType: TTypeSymbol;
begin
  if not FTok.TestDelete(ttOF) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_OfExpected);

  elementType := ReadType('', typeContext);
  result := TAssociativeArraySymbol.Create(typeName, elementType, keyType);
end;

// ReadArrayConstant
//
function TdwsCompiler.ReadArrayConstant(closingToken: TTokenType;
  expecting: TTypeSymbol): TArrayConstantExpr;
var
  factory: IdwsDataSymbolFactory;
  itemExpecting: TTypeSymbol;

  procedure ReadArrayConstantRange(result: TArrayConstantExpr;
    expr1: TTypedExpr);
  var
    expr2: TTypedExpr;
    range1, range2: Int64;
    boundsOk: Integer;
  begin
    boundsOk := 0;
    expr2 := nil;
    try
      if not expr1.IsOfType(FCompilerContext.TypInteger) then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_OrdinalExpressionExpected)
      else if not expr1.IsConstant then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_ConstantExpressionExpected)
      else
        boundsOk := 1;

      expr2 := factory.ReadInitExpr(itemExpecting);

      if (expr2 = nil) or not expr2.IsOfType(FCompilerContext.TypInteger) then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_OrdinalExpressionExpected)
      else if not expr2.IsConstant then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_ConstantExpressionExpected)
      else
        Inc(boundsOk);

      if boundsOk = 2 then
      begin
        if expr1.typ.SameType(expr2.typ) then
        begin
          range1 := expr1.EvalAsInteger(FExec);
          range2 := expr2.EvalAsInteger(FExec);
          if Abs(range2 - range1) > cMaxArrayItemRange then
            FMsgs.AddCompilerError(FTok.hotPos, CPE_RangeTooLarge)
          else
            result.AddElementRange(FCompilerContext, range1, range2, expr1.typ);
        end
        else
        begin
          IncompatibleTypes(FTok.hotPos, CPE_RangeIncompatibleTypes, expr1.typ,
            expr2.typ);
        end;
      end;
    finally
      OrphanAndNil(expr1);
      OrphanAndNil(expr2);
    end;
  end;

var
  expr: TTypedExpr;
  hotPos: TScriptPos;
begin
  factory := TStandardSymbolFactory.Create(Self);
  result := TArrayConstantExpr.Create(FCompilerContext, FTok.hotPos);
  try
    if expecting <> nil then
      itemExpecting := expecting.typ
    else
      itemExpecting := nil;
    if not FTok.TestDelete(closingToken) then
    begin
      // At least one argument was found
      repeat
        hotPos := FTok.CurrentPos;
        expr := factory.ReadInitExpr(itemExpecting);
        if expr <> nil then
        begin
          if FTok.TestDelete(ttDOTDOT) then
            ReadArrayConstantRange(TArrayConstantExpr(result), expr)
          else
          begin
            TArrayConstantExpr(result).AddElementExpr(hotPos,
              FCompilerContext, expr);
          end;
        end;
      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(closingToken) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_ArrayBracketRightExpected);
    end
    else
    begin
      // empty array
      (result.typ as TStaticArraySymbol).typ := FCompilerContext.TypVariant;
    end;

    if expecting is TOpenArraySymbol then
      (result.typ as TStaticArraySymbol).typ := FCompilerContext.TypVariant
    else
      result.TypeCheckElements(FCompilerContext);
    if Optimize then
      result := result.Optimize(FCompilerContext) as TArrayConstantExpr;
  except
    result.Orphan(FCompilerContext);
    raise;
  end;
end;

// ReadArrayMethod
//
function TdwsCompiler.ReadArrayMethod(const name: String;
  const namePos: TScriptPos; baseExpr: TTypedExpr): TProgramExpr;
var
  arraySym: TArraySymbol;
  argList: TTypedExprList;
  argPosArray: TScriptPosArray;
  argSymTable: TUnSortedSymbolTable;
  i: Integer;
  mapFunctionType: TFuncSymbol;
  filterFunctionType: TFuncSymbol;
  forEachFunctionType: TFuncSymbol;
  methodKind: TArrayMethodKind;
  indexOfClass: TArrayIndexOfExprClass;
  pseudoMethod: TPseudoMethodSymbol;

  procedure CheckNotTypeReference;
  begin
    if baseExpr.ClassType = TTypeReferenceExpr then
      FMsgs.AddCompilerError(namePos, RTE_ArrayInstanceExpected);
  end;

  procedure CheckRestricted;
  begin
    if arraySym.ClassType <> TDynamicArraySymbol then
      FMsgs.AddCompilerErrorFmt(namePos,
        CPE_ArrayMethodRestrictedToDynamicArrays, [name])
    else
      CheckNotTypeReference;
  end;

  procedure CheckDynamicOrStatic;
  begin
    if (arraySym.ClassType <> TDynamicArraySymbol) and
      (arraySym.ClassType <> TStaticArraySymbol) then
      FMsgs.AddCompilerErrorFmt(namePos,
        CPE_ArrayMethodNotAvailableOnOpenArrays, [name])
    else
      CheckNotTypeReference;
  end;

  function CheckArguments(expectedMin, expectedMax: Integer): Boolean;
  begin
    result := argList.Count in [expectedMin .. expectedMax];
    if not result then
    begin
      if expectedMax = 0 then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_NoArgumentsExpected)
      else if argList.Count > expectedMax then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_TooManyArguments)
      else
        FMsgs.AddCompilerError(FTok.hotPos, CPE_TooFewArguments);
    end;
  end;

begin
  result := nil;
  argSymTable := nil;
  argList := TTypedExprList.Create;
  try
    arraySym := baseExpr.typ.UnAliasedType as TArraySymbol;

    methodKind := NameToArrayMethod(name, FMsgs, namePos);
    if coSymbolDictionary in options then
    begin
      pseudoMethod := arraySym.PseudoMethodSymbol(methodKind, FCompilerContext);
      if pseudoMethod <> nil then
        SymbolDictionary.AddSymbol(pseudoMethod, namePos, [suReference]);
    end;

    case methodKind of

      amkAdd, amkPush:
        argList.DefaultExpected := TParamSymbol.Create('', arraySym.typ);

      amkIndexOf, amkRemove, amkContains:
        begin
          argSymTable := TUnSortedSymbolTable.Create;
          argSymTable.AddSymbol(TParamSymbol.Create('', arraySym.typ));
          argList.table := argSymTable;
        end;

      amkSort:
        argList.DefaultExpected := TParamSymbol.Create('',
          arraySym.SortFunctionType(FCompilerContext));

      amkMap:
        argList.DefaultExpected := TParamSymbol.Create('',
          arraySym.mapFunctionType(FCompilerContext));

      amkFilter:
        argList.DefaultExpected := TParamSymbol.Create('',
          arraySym.filterFunctionType(FCompilerContext));

      amkForEach:
        argList.DefaultExpected := TParamSymbol.Create('',
          arraySym.forEachFunctionType(FCompilerContext));

    end;

    ReadArguments(argList.AddExpr, ttBLEFT, ttBRIGHT, argPosArray,
      argList.ExpectedArg);

    try
      case methodKind of

        amkLow:
          begin
            CheckArguments(0, 0);
            result := CreateArrayLow(namePos, baseExpr, arraySym, True);
          end;

        amkHigh:
          begin
            CheckArguments(0, 0);
            if not(arraySym is TStaticArraySymbol) then
              CheckNotTypeReference;
            result := CreateArrayHigh(namePos, baseExpr, arraySym, True);
          end;

        amkLength, amkCount:
          begin
            CheckArguments(0, 0);
            CheckNotTypeReference;
            result := CreateArrayLength(namePos, baseExpr, arraySym);
          end;

        amkAdd, amkPush:
          begin
            CheckRestricted;
            if CheckArguments(1, 99) then
            begin
              for i := 0 to argList.Count - 1 do
              begin
                if (argList[i].typ = nil) or
                  not(arraySym.typ.IsCompatible(argList[i].typ) or
                  arraySym.IsCompatible(argList[i].typ) or
                  ((argList[i].typ is TStaticArraySymbol) and
                  (arraySym.typ.IsCompatible(argList[i].typ.typ) or
                  (argList[i].typ.Size = 0)))) then
                begin
                  argList[i] := CompilerUtils.WrapWithImplicitConversion
                    (FCompilerContext, argList[i], arraySym.typ, argPosArray[i],
                    CPE_IncompatibleParameterTypes);
                end
                else if argList[i].ClassType = TArrayConstantExpr then
                begin
                  TArrayConstantExpr(argList[i]).Prepare(FCompilerContext,
                    arraySym.typ);
                end;
              end;
            end;
            result := CompilerUtils.DynamicArrayAdd(CompilerContext, baseExpr,
              namePos, argList, argPosArray);
          end;

        amkPop:
          begin
            CheckRestricted;
            CheckArguments(0, 0);
            result := TArrayPopExpr.Create(FCompilerContext, namePos, baseExpr);
          end;

        amkPeek:
          begin
            CheckRestricted;
            CheckArguments(0, 0);
            result := TArrayPeekExpr.Create(FCompilerContext, namePos,
              baseExpr);
          end;

        amkDelete:
          begin
            CheckRestricted;
            if CheckArguments(1, 2) then
            begin
              if (argList[0].typ = nil) or not argList[0].typ.IsOfType
                (FCompilerContext.TypInteger) then
                FMsgs.AddCompilerError(argPosArray[0],
                  CPE_IntegerExpressionExpected);
              if argList.Count > 1 then
              begin
                if (argList[1].typ = nil) or not argList[1].typ.IsOfType
                  (FCompilerContext.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[1],
                    CPE_IntegerExpressionExpected);
                result := TArrayDeleteExpr.Create(namePos, baseExpr, argList[0],
                  argList[1]);
              end
              else
                result := TArrayDeleteExpr.Create(namePos, baseExpr,
                  argList[0], nil);
              argList.Clear;
            end
            else
              result := TArrayDeleteExpr.Create(namePos, baseExpr, nil, nil);
          end;

        amkIndexOf:
          begin
            CheckDynamicOrStatic;
            indexOfClass := TArrayIndexOfExpr.ArrayIndexOfExprClass(arraySym);
            if CheckArguments(1, 2) then
            begin
              if (argList[0].typ = nil) or not arraySym.typ.IsCompatible
                (argList[0].typ) then
              begin
                // IncompatibleTypes(argPosArray[0], CPE_IncompatibleParameterTypes,
                // arraySym.Typ, argList[0].Typ);
                argList[0] := CompilerUtils.WrapWithImplicitConversion
                  (FCompilerContext, argList[0], arraySym.typ, argPosArray[0],
                  CPE_IncompatibleParameterTypes);
              end;
              if argList.Count > 1 then
              begin
                if (argList[1].typ = nil) or not argList[1].typ.IsOfType
                  (FCompilerContext.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0],
                    CPE_IntegerExpressionExpected);
                result := indexOfClass.Create(FCompilerContext, namePos,
                  baseExpr, argList[0], argList[1]);
              end
              else
              begin
                result := indexOfClass.Create(FCompilerContext, namePos,
                  baseExpr, argList[0], nil);
              end;
              argList.Clear;
            end
            else
            begin
              result := indexOfClass.Create(FCompilerContext, namePos, baseExpr,
                nil, nil);
            end;
          end;

        amkContains:
          begin
            CheckDynamicOrStatic;
            if CheckArguments(1, 1) then
            begin
              if (argList[0].typ = nil) or not arraySym.typ.IsCompatible
                (argList[0].typ) then
              begin
                argList[0] := CompilerUtils.WrapWithImplicitConversion
                  (FCompilerContext, argList[0], arraySym.typ, argPosArray[0],
                  CPE_IncompatibleParameterTypes);
              end;
              result := CompilerUtils.ArrayContains(FCompilerContext, namePos,
                baseExpr, argList[0]);
              argList.Clear;
            end;
          end;

        amkRemove:
          begin
            CheckRestricted;
            if CheckArguments(1, 2) then
            begin
              if (argList[0].typ = nil) or not arraySym.typ.IsCompatible
                (argList[0].typ) then
                IncompatibleTypes(argPosArray[0],
                  CPE_IncompatibleParameterTypes, arraySym.typ, argList[0].typ);
              if argList.Count > 1 then
              begin
                if (argList[1].typ = nil) or not argList[1].typ.IsOfType
                  (FCompilerContext.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0],
                    CPE_IntegerExpressionExpected);
                result := TArrayRemoveExpr.Create(FCompilerContext, namePos,
                  baseExpr, argList[0], argList[1]);
              end
              else
                result := TArrayRemoveExpr.Create(FCompilerContext, namePos,
                  baseExpr, argList[0], nil);
              argList.Clear;
            end
            else
              result := TArrayRemoveExpr.Create(FCompilerContext, namePos,
                baseExpr, nil, nil);
          end;

        amkInsert:
          begin
            CheckRestricted;
            if CheckArguments(2, 2) then
            begin
              if (argList[0].typ = nil) or not argList[0].typ.IsOfType
                (FCompilerContext.TypInteger) then
                FMsgs.AddCompilerError(argPosArray[0],
                  CPE_IntegerExpressionExpected);
              if (argList[1].typ = nil) or not arraySym.typ.IsCompatible
                (argList[1].typ) then
                argList[1] := CompilerUtils.WrapWithImplicitConversion
                  (FCompilerContext, argList[1], arraySym.typ, argPosArray[1],
                  CPE_IncompatibleParameterTypes);
              result := TArrayInsertExpr.Create(namePos, baseExpr, argList[0],
                argList[1]);
              argList.Clear;
            end
            else
              result := TArrayInsertExpr.Create(namePos, baseExpr, nil, nil);
          end;

        amkMove:
          begin
            CheckRestricted;
            if CheckArguments(2, 2) then
            begin
              if (argList[0].typ = nil) or not argList[0].typ.IsOfType
                (FCompilerContext.TypInteger) then
                FMsgs.AddCompilerError(argPosArray[0],
                  CPE_IntegerExpressionExpected);
              if (argList[1].typ = nil) or not argList[1].typ.IsOfType
                (FCompilerContext.TypInteger) then
                FMsgs.AddCompilerError(argPosArray[1],
                  CPE_IntegerExpressionExpected);
              result := TArrayMoveExpr.Create(namePos, baseExpr, argList[0],
                argList[1]);
              argList.Clear;
            end
            else
              result := TArrayMoveExpr.Create(namePos, baseExpr, nil, nil);
          end;

        amkSetLength:
          begin
            CheckRestricted;
            if CheckArguments(1, 1) then
            begin
              if (argList[0].typ = nil) or not argList[0].typ.IsOfType
                (FCompilerContext.TypInteger) then
                FMsgs.AddCompilerError(argPosArray[0],
                  CPE_IntegerExpressionExpected);
              result := TArraySetLengthExpr.Create(namePos, baseExpr,
                argList[0]);
              argList.Clear;
            end
            else
              result := TArraySetLengthExpr.Create(namePos, baseExpr, nil);
          end;

        amkClear:
          begin
            CheckRestricted;
            CheckArguments(0, 0);
            result := TArraySetLengthExpr.Create(namePos, baseExpr,
              FUnifiedConstants.CreateInteger(0));
          end;

        amkSwap:
          begin
            CheckRestricted;
            if CheckArguments(2, 2) then
            begin
              if (argList[0].typ = nil) or not argList[0].typ.IsOfType
                (FCompilerContext.TypInteger) then
                FMsgs.AddCompilerError(argPosArray[0],
                  CPE_IntegerExpressionExpected);
              if (argList[1].typ = nil) or not argList[1].typ.IsOfType
                (FCompilerContext.TypInteger) then
                FMsgs.AddCompilerError(argPosArray[1],
                  CPE_IntegerExpressionExpected);
              result := TArraySwapExpr.Create(FCompilerContext, namePos,
                baseExpr, argList[0], argList[1]);
              argList.Clear;
            end
            else
              result := TArraySwapExpr.Create(FCompilerContext, namePos,
                baseExpr, nil, nil);
          end;

        amkCopy:
          begin
            CheckRestricted;
            if CheckArguments(0, 2) then
            begin
              if argList.Count > 0 then
              begin
                if (argList[0].typ = nil) or not argList[0].typ.IsOfType
                  (FCompilerContext.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0],
                    CPE_IntegerExpressionExpected);
                if argList.Count > 1 then
                begin
                  if (argList[1].typ = nil) or not argList[1].typ.IsOfType
                    (FCompilerContext.TypInteger) then
                    FMsgs.AddCompilerError(argPosArray[1],
                      CPE_IntegerExpressionExpected);
                  result := TArrayCopyExpr.Create(FCompilerContext, namePos,
                    baseExpr, argList[0], argList[1]);
                end
                else
                  result := TArrayCopyExpr.Create(FCompilerContext, namePos,
                    baseExpr, argList[0], nil);
              end
              else
                result := TArrayCopyExpr.Create(FCompilerContext, namePos,
                  baseExpr, nil, nil);
              argList.Clear;
            end
            else
              result := TArrayCopyExpr.Create(FCompilerContext, namePos,
                baseExpr, nil, nil);
          end;

        amkSort:
          begin
            CheckRestricted;
            if CheckArguments(0, 1) then
            begin
              if argList.Count = 0 then
              begin
                if arraySym.typ.IsOfType(FCompilerContext.TypString) then
                  result := TArraySortNaturalStringExpr.Create(FCompilerContext,
                    namePos, baseExpr)
                else if arraySym.typ.IsOfType(FCompilerContext.TypInteger) then
                  result := TArraySortNaturalIntegerExpr.Create
                    (FCompilerContext, namePos, baseExpr)
                else if arraySym.typ.IsOfType(FCompilerContext.TypFloat) then
                  result := TArraySortNaturalFloatExpr.Create(FCompilerContext,
                    namePos, baseExpr)
                else if arraySym.typ.IsOfType(FCompilerContext.TypBoolean) then
                  result := TArraySortNaturalExpr.Create(FCompilerContext,
                    namePos, baseExpr)
                else
                begin
                  FMsgs.AddCompilerError(namePos,
                    CPE_ArrayDoesNotHaveNaturalSortOrder);
                  result := TArraySortNaturalExpr.Create(FCompilerContext,
                    namePos, baseExpr);
                end;
              end
              else
              begin
                if not argList[0].typ.IsCompatible
                  (arraySym.SortFunctionType(FCompilerContext)) then
                begin
                  IncompatibleTypes(argPosArray[0],
                    CPE_IncompatibleParameterTypes,
                    arraySym.SortFunctionType(FCompilerContext),
                    argList[0].typ);
                  argList.OrphanItems(FCompilerContext);
                  argList.Clear;
                end;
                if argList.Count > 0 then
                begin
                  result := TArraySortExpr.Create(FCompilerContext, namePos,
                    baseExpr, TFuncPtrExpr.Create(FCompilerContext,
                    argPosArray[0], argList[0]));
                  argList.Clear;
                end
                else
                  result := TArraySortExpr.Create(FCompilerContext, namePos,
                    baseExpr, nil);
              end;
            end
            else
              result := TArraySortExpr.Create(FCompilerContext, namePos,
                baseExpr, nil);
          end;

        amkMap:
          begin
            CheckRestricted;
            if CheckArguments(1, 1) then
            begin
              mapFunctionType := arraySym.mapFunctionType(FCompilerContext);
              if argList[0].typ.IsCompatible(mapFunctionType) and
                (argList[0].typ.typ <> nil) then
              begin
                result := TArrayMapExpr.Create(FCompilerContext, namePos,
                  baseExpr, TFuncPtrExpr.Create(FCompilerContext,
                  argPosArray[0], argList[0]));
                argList.Clear;
              end
              else
              begin
                IncompatibleTypes(argPosArray[0],
                  CPE_IncompatibleParameterTypes, mapFunctionType, argList[0]);
                argList.OrphanItems(FCompilerContext)
              end;
            end;
            if result = nil then
              result := TArrayMapExpr.Create(FCompilerContext, namePos,
                baseExpr, nil);
          end;

        amkFilter:
          begin
            CheckRestricted;
            if CheckArguments(1, 1) then
            begin
              filterFunctionType := arraySym.filterFunctionType
                (FCompilerContext);
              if argList[0].typ.IsCompatible(filterFunctionType) and
                (argList[0].typ.typ <> nil) then
              begin
                result := TArrayFilterExpr.Create(FCompilerContext, namePos,
                  baseExpr, TFuncPtrExpr.Create(FCompilerContext,
                  argPosArray[0], argList[0]));
                argList.Clear;
              end
              else
              begin
                IncompatibleTypes(argPosArray[0],
                  CPE_IncompatibleParameterTypes, filterFunctionType,
                  argList[0].typ);
                argList.OrphanItems(FCompilerContext)
              end;
            end;
            if result = nil then
              result := TArrayFilterExpr.Create(FCompilerContext, namePos,
                baseExpr, nil);
          end;

        amkForEach:
          begin
            CheckRestricted;
            if CheckArguments(1, 1) then
            begin
              forEachFunctionType := arraySym.forEachFunctionType
                (FCompilerContext);
              if argList[0].typ.IsCompatible(forEachFunctionType) then
              begin
                result := TArrayForEachExpr.Create(FCompilerContext, namePos,
                  baseExpr, TFuncPtrExpr.Create(FCompilerContext,
                  argPosArray[0], argList[0]));
                argList.Clear;
              end
              else
              begin
                IncompatibleTypes(argPosArray[0],
                  CPE_IncompatibleParameterTypes, forEachFunctionType,
                  argList[0].typ);
                argList.OrphanItems(FCompilerContext)
              end;
            end;
            if result = nil then
              result := TArrayForEachExpr.Create(FCompilerContext, namePos,
                baseExpr, nil);
          end;

        amkReverse:
          begin
            CheckRestricted;
            CheckArguments(0, 0);
            result := TArrayReverseExpr.Create(FCompilerContext, namePos,
              baseExpr);
          end;

      else
        ReportNoMemberForType(name, namePos, baseExpr);
      end;
    except
      OrphanAndNil(result);
      raise;
    end;
  finally
    OrphanObject(argSymTable);
    argList.Orphan(FCompilerContext);
  end;
end;

// ReadAssociativeArrayMethod
//
function TdwsCompiler.ReadAssociativeArrayMethod(const name: String;
  const namePos: TScriptPos; baseExpr: TTypedExpr): TProgramExpr;
var
  argList: TTypedExprList;
  argPosArray: TScriptPosArray;
  argSymTable: TUnSortedSymbolTable;
  arraySym: TAssociativeArraySymbol;
  methodKind: TArrayMethodKind;
  pseudoMethod: TPseudoMethodSymbol;

  function CheckArguments(expectedMin, expectedMax: Integer): Boolean;
  begin
    result := argList.Count in [expectedMin .. expectedMax];
    if not result then
    begin
      if expectedMax = 0 then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_NoArgumentsExpected)
      else if argList.Count > expectedMax then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_TooManyArguments)
      else
        FMsgs.AddCompilerError(FTok.hotPos, CPE_TooFewArguments);
    end;
  end;

begin
  result := nil;
  argSymTable := nil;
  argList := TTypedExprList.Create;
  try
    arraySym := baseExpr.typ.UnAliasedType as TAssociativeArraySymbol;

    methodKind := NameToArrayMethod(name, FMsgs, namePos);
    if coSymbolDictionary in options then
    begin
      pseudoMethod := arraySym.PseudoMethodSymbol(methodKind, FCompilerContext);
      if pseudoMethod <> nil then
        SymbolDictionary.AddSymbol(pseudoMethod, namePos, [suReference]);
    end;

    ReadArguments(argList.AddExpr, ttBLEFT, ttBRIGHT, argPosArray,
      argList.ExpectedArg);

    try
      case methodKind of

        amkLength, amkCount:
          begin
            CheckArguments(0, 0);
            result := TAssociativeArrayLengthExpr.Create(FCompilerContext,
              namePos, baseExpr);
          end;

        amkClear:
          begin
            CheckArguments(0, 0);
            result := TAssociativeArrayClearExpr.Create(namePos, baseExpr);
          end;

        amkDelete:
          begin
            if CheckArguments(1, 1) then
            begin
              if (argList[0].typ = nil) or not arraySym.keyType.IsCompatible
                (argList[0].typ) then
                IncompatibleTypes(argPosArray[0],
                  CPE_IncompatibleParameterTypes, arraySym.typ, argList[0].typ);
              result := TAssociativeArrayDeleteExpr.Create(FCompilerContext,
                baseExpr, argList[0]);
              argList.Clear;
            end
            else
              result := TAssociativeArrayDeleteExpr.Create(FCompilerContext,
                baseExpr, nil);
          end;

        amkKeys:
          begin
            CheckArguments(0, 0);
            result := TAssociativeArrayKeysExpr.Create(FCompilerContext,
              namePos, baseExpr);
          end;

      else
        ReportNoMemberForType(name, namePos, baseExpr);
      end;
    except
      OrphanAndNil(result);
      raise;
    end;
  finally
    OrphanObject(argSymTable);
    argList.Orphan(FCompilerContext);
  end;
end;

// ReadStringMethod
//
function TdwsCompiler.ReadStringMethod(const name: String;
  const namePos: TScriptPos; baseExpr: TTypedExpr): TProgramExpr;
var
  sk: TSpecialKeywordKind;
begin
  try
    if FTok.TestDelete(ttBLEFT) and not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NoParamsExpected);

    sk := IdentifySpecialKeyword(name);

    case sk of
      skLength, skHigh:
        begin
          if coSymbolDictionary in FCompilerContext.options then
          begin
            if sk = skLength then
              RecordSymbolUseReference
                (FCompilerContext.TypString.LengthPseudoSymbol
                (FCompilerContext), namePos, False)
            else
              RecordSymbolUseReference
                (FCompilerContext.TypString.HighPseudoSymbol(FCompilerContext),
                namePos, False)
          end;
          result := TStringLengthExpr.Create(FCompilerContext,
            namePos, baseExpr)
        end;
      skLow:
        begin
          if coSymbolDictionary in FCompilerContext.options then
          begin
            RecordSymbolUseReference(FCompilerContext.TypString.LowPseudoSymbol
              (FCompilerContext), namePos, False)
          end;
          OrphanAndNil(baseExpr);
          result := FUnifiedConstants.CreateInteger(1);
        end;
    else
      result := nil;
      ReportNoMemberForType(name, namePos, baseExpr);
    end;

    if not(coHintsDisabled in FOptions) then
      CheckSpecialNameCase(name, sk, namePos);
  except
    OrphanAndNil(baseExpr);
    raise;
  end;
end;

// ReadSetOfMethod
//
function TdwsCompiler.ReadSetOfMethod(const name: String;
  const namePos: TScriptPos; baseExpr: TTypedExpr): TProgramExpr;
var
  amk: TArrayMethodKind;
begin
  try
    if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackLeftExpected);

    amk := NameToArrayMethod(name, FMsgs, namePos);

    case amk of
      amkInclude, amkExclude:
        result := ReadIncludeExclude(namePos, amk, baseExpr, namePos);
    else
      result := nil;
      ReportNoMemberForType(name, namePos, baseExpr);
    end;

    if not FTok.TestDelete(ttBRIGHT) then
    begin
      OrphanAndNil(result);
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);
    end;
  except
    OrphanAndNil(baseExpr);
    raise;
  end;
end;

// ReadElementMethod
//
function TdwsCompiler.ReadElementMethod(const name: String;
  const namePos: TScriptPos; baseExpr: TTypedExpr): TProgramExpr;
type
  TElementMethod = (emInvalid, emName, emQualifiedName, emValue);
var
  enumeration: TEnumerationSymbol;
  element: TElementSymbol;
  meth: TElementMethod;
  baseTyp: TTypeSymbol;
begin
  enumeration := (baseExpr.typ.UnAliasedType as TEnumerationSymbol);

  if SameText(name, 'name') then
    meth := emName
  else if SameText(name, 'qualifiedname') then
    meth := emQualifiedName
  else if SameText(name, 'value') then
    meth := emValue
  else
    meth := emInvalid;

  case meth of
    emName, emQualifiedName:
      begin

        if baseExpr.ClassType = TConstIntExpr then
        begin

          element := enumeration.ElementByValue(TConstIntExpr(baseExpr).value);
          if element = nil then
          begin
            FMsgs.AddCompilerHint(namePos, CPH_UnnamedEnumerationElement);
            result := FUnifiedConstants.CreateEmptyString;
          end
          else if meth = emName then
            result := TConstStringExpr.Create(namePos,
              FCompilerContext.TypString, element.name)
          else
            result := TConstStringExpr.Create(namePos,
              FCompilerContext.TypString, element.QualifiedName);
          OrphanAndNil(baseExpr);

        end
        else
        begin

          if meth = emName then
            result := TEnumerationElementNameExpr.Create(FCompilerContext,
              namePos, baseExpr)
          else
            result := TEnumerationElementQualifiedNameExpr.Create
              (FCompilerContext, namePos, baseExpr);
          RecordSymbolUse(baseExpr.typ, namePos, [suRTTI, suImplicit]);

        end;

      end;
    emValue:
      begin

        if baseExpr.ClassType = TConstIntExpr then
        begin

          result := FUnifiedConstants.CreateInteger
            (TConstIntExpr(baseExpr).value);
          OrphanAndNil(baseExpr);

        end
        else
        begin

          result := TOrdIntExpr.Create(FCompilerContext, namePos, baseExpr);

        end;

      end;
  else

    result := nil;
    if baseExpr <> nil then
      baseTyp := baseExpr.typ
    else
      baseTyp := nil;
    OrphanAndNil(baseExpr);
    ReportNoMemberForType(name, namePos, baseTyp);

  end;
end;

// ReadNameList
//
procedure TdwsCompiler.ReadNameList(names: TSimpleStringList;
  var posArray: TScriptPosArray; const options: TdwsNameListOptions = [];
  externalNames: TSimpleStringList = nil);
var
  n: Integer;
begin
  n := 0;
  names.Clear;
  if externalNames <> nil then
    externalNames.Clear;
  repeat
    if not FTok.TestName then
    begin
      if not((nloAllowStrings in options) and FTok.Test(ttStrVal)) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);
    end;

    if n = Length(posArray) then
      SetLength(posArray, n + 1);
    posArray[n] := FTok.hotPos;
    Inc(n);

    names.Add(FTok.GetToken.AsString);
    if not(nloNoCheckSpecials in options) then
      CheckSpecialName(FTok.GetToken.AsString);
    FTok.KillToken;

    while (nloAllowDots in options) and FTok.TestDelete(ttDOT) do
    begin
      if not FTok.TestName then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);
      names[names.Count - 1] := names[names.Count - 1] + '.' +
        FTok.GetToken.AsString;
      FTok.KillToken;
    end;

    if externalNames <> nil then
    begin
      if FTok.TestDelete(ttEXTERNAL) then
      begin
        if CurrentProg.level <> 0 then
          FMsgs.AddCompilerError(FTok.hotPos,
            CPE_ExternalVariablesMustBeGlobal);
        if FTok.Test(ttStrVal) then
        begin
          externalNames.Add(FTok.GetToken.AsString);
          FTok.KillToken;
        end
        else
        begin
          FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected);
          externalNames.Add('');
        end;
      end
      else
        externalNames.Add('');
    end;

  until not FTok.TestDelete(ttCOMMA);
end;

// ReadExternalName
//
procedure TdwsCompiler.ReadExternalName(funcSym: TFuncSymbol);
begin
  FTok.KillToken;
  if FTok.TestAny([ttSEMI, ttPROPERTY]) = ttNone then
  begin
    if FTok.TestDelete(ttARRAY) then
    begin
      if (funcSym is TMethodSymbol) and (not TMethodSymbol(funcSym).IsVirtual)
        and (not TMethodSymbol(funcSym).IsClassMethod) then
      else
        FMsgs.AddCompilerError(FTok.hotPos,
          CPE_ExternalArrayForStaticMethodsOnly);
      funcSym.externalName := SYS_EXTERNAL_ARRAY
    end
    else
    begin
      if not FTok.Test(ttStrVal) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_StringExpected)
      else
      begin
        funcSym.externalName := FTok.GetToken.AsString;
        FTok.KillToken;
      end;
    end;
  end;
end;

// ReadNew
//
function TdwsCompiler.ReadNew(restrictTo: TClassSymbol; asAttribute: Boolean)
  : TProgramExpr;

  function FindAsAttribute(table: TSymbolTable; const name: String): TSymbol;
  begin
    result := table.FindSymbol(name + 'Attribute', cvPrivate);
    if result <> nil then
    begin
      if result.IsClassSymbol and TClassSymbol(result).IsAttribute then
        Exit
      else
        result := nil;
    end;
  end;

var
  sym: TSymbol;
  typSym: TTypeSymbol;
  ClassSym: TClassSymbol;
  methSym: TMethodSymbol;
  nameToken: TToken;
  hotPos: TScriptPos;
  typedExpr: TTypedExpr;
  baseExpr: TDataExpr;
  argPosArray: TScriptPosArray;
  overloads: TFuncSymbolList;
  i: Integer;
  funcExpr: TFuncExprBase;
  opSym: TOperatorSymbol;
begin
  baseExpr := nil;
  ClassSym := nil;

  if FTok.TestDelete(ttBLEFT) then
  begin

    hotPos := FTok.hotPos;
    typedExpr := ReadExpr;
    if typedExpr <> nil then
    begin
      try
        if typedExpr.typ.UnAliasedTypeIs(TClassOfSymbol) and
          (typedExpr is TDataExpr) then
        begin
          baseExpr := TDataExpr(typedExpr);
          ClassSym := TClassOfSymbol(typedExpr.typ.UnAliasedType)
            .TypClassSymbol;
        end
        else
          FMsgs.AddCompilerStop(hotPos, CPE_ClassRefExpected);
      except
        OrphanAndNil(typedExpr);
        raise;
      end;
    end
    else
    begin
      // error was already registered, attempt to keep compiling
      ClassSym := FCompilerContext.TypTObject;
      baseExpr := TBogusConstExpr.CreateNull(hotPos, ClassSym);
    end;
    if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerError(hotPos, CPE_BrackRightExpected);

  end
  else
  begin

    // Get name
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ClassRefExpected);

    nameToken := FTok.GetToken;
    hotPos := FTok.hotPos;
    if asAttribute then
      sym := FindAsAttribute(CurrentProg.table, nameToken.AsString)
    else
      sym := nil;
    if sym = nil then
      sym := CurrentProg.table.FindSymbol(nameToken.AsString, cvPrivate);

    if sym = nil then
      FMsgs.AddCompilerErrorFmt(hotPos, CPE_UnknownName, [nameToken.AsString])
    else
      CheckMatchingDeclarationCase(nameToken.AsString, sym, hotPos);

    FTok.KillToken;

    if (sym <> nil) and (sym.ClassType = TGenericSymbol) then
      sym := ReadSpecializedType(TGenericSymbol(sym));

    if FTok.TestDelete(ttALEFT) then
    begin

      if Assigned(sym) and sym.IsType then
      begin

        typSym := TTypeSymbol(sym);
        RecordSymbolUse(typSym, hotPos, [suReference]);
        result := ReadNewArray(typSym);

      end
      else
      begin

        if sym <> nil then
          FMsgs.AddCompilerError(hotPos, CPE_TypeExpected);
        result := ReadNewArray(FCompilerContext.TypVariant);

      end;
      Exit;

    end
    else
    begin

      if sym is TAliasSymbol then
        sym := TAliasSymbol(sym).UnAliasedType
      else if sym = nil then
      begin
        // keep compiling
        sym := FCompilerContext.TypTObject;
      end;

      if sym.IsClassSymbol then
      begin

        ClassSym := TClassSymbol(sym);
        RecordSymbolUse(ClassSym, hotPos, [suReference]);

      end
      else if (sym is TDataSymbol) and (sym.typ is TClassOfSymbol) then
      begin

        ClassSym := TClassOfSymbol(sym.typ).TypClassSymbol;
        RecordSymbolUseReference(sym, hotPos, False);

      end
      else if sym is TTypeSymbol then
      begin

        opSym := FOperators.FindUnaryOperatorFor(ttNEW, TTypeSymbol(sym));
        if opSym <> nil then
        begin

          result := ReadNewOperator(hotPos, opSym, TTypeSymbol(sym));
          Exit;

        end
        else
          FMsgs.AddCompilerStopFmt(hotPos, CPE_NotSupportedFor,
            [cTokenStrings[ttNEW], sym.name]);

      end
      else if sym <> nil then
        FMsgs.AddCompilerStop(hotPos, CPE_ClassRefExpected);

    end;

    if sym.IsClassSymbol then
      baseExpr := TConstIntExpr.Create(hotPos, ClassSym.MetaSymbol,
        Int64(ClassSym))
    else
      baseExpr := TVarExpr.CreateTyped(FCompilerContext, hotPos,
        TDataSymbol(sym));
  end;

  WarnDeprecatedType(hotPos, ClassSym);

  if ClassSym.IsStatic then
  begin
    OrphanAndNil(baseExpr);
    FMsgs.AddCompilerErrorFmt(hotPos, CPE_ClassIsStaticNoInstantiation,
      [ClassSym.name]);
    result := TConstIntExpr.Create(cNullPos, ClassSym, 0);
    Exit;
  end;
  if (restrictTo <> nil) and not ClassSym.IsOfType(restrictTo) then
    FMsgs.AddCompilerErrorFmt(hotPos, CPE_MustBeSubClassOf, [restrictTo.name]);

  overloads := nil;
  methSym := ClassSym.FindDefaultConstructor(cvPrivate);
  if methSym = nil then
  begin
    OrphanAndNil(baseExpr);
    FMsgs.AddCompilerStopFmt(hotPos, CPE_NoDefaultConstructor, [ClassSym.name])
  end
  else if methSym.IsOverloaded then
    overloads := CompilerContext.AllocateFuncSymbolList;
  try
    if overloads <> nil then
    begin
      CollectMethodOverloads(methSym, overloads);
      for i := overloads.Count - 1 downto 0 do
        if overloads[i].Kind <> fkConstructor then
          overloads.Extract(i);
    end;

    RecordSymbolUseImplicitReference(methSym, hotPos, False);

    result := nil;
    try
      result := GetMethodExpr(methSym, baseExpr, rkClassOfRef, hotPos, []);
    except
      OrphanAndNil(baseExpr);
      raise;
    end;
    try
      ReadFuncArgs(TFuncExpr(result), argPosArray, overloads);
      (result as TMethodExpr).typ := ClassSym;
      if overloads <> nil then
      begin
        funcExpr := (result as TFuncExpr);
        if not ResolveOverload(funcExpr, overloads, argPosArray, nil, []) then
          Exit;
        result := funcExpr;
      end;
      TypeCheckArguments(FCompilerContext, TFuncExpr(result), argPosArray);
    except
      OrphanAndNil(result);
      raise;
    end;
  finally
    CompilerContext.ReleaseFuncSymbolList(overloads);
  end;
end;

// ReadNewArray
//
function TdwsCompiler.ReadNewArray(elementTyp: TTypeSymbol): TNewArrayExpr;
var
  lengthExpr: TTypedExpr;
  hotPos: TScriptPos;
  newExpr: TNewArrayExpr;
begin
  newExpr := TNewArrayExpr.Create(FCompilerContext, FTok.hotPos, elementTyp);
  try
    repeat
      FTok.HasTokens;
      hotPos := FTok.hotPos;
      lengthExpr := ReadExpr;
      newExpr.AddLengthExpr(lengthExpr, FCompilerContext.TypInteger);
      if not(lengthExpr.IsOfType(FCompilerContext.TypInteger)) then
        FMsgs.AddCompilerError(hotPos, CPE_IntegerExpressionExpected);
    until not FTok.TestDelete(ttCOMMA);

    if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ArrayBracketRightExpected);
  except
    newExpr.Orphan(FCompilerContext);
    raise;
  end;
  result := newExpr;
end;

// ReadNewOperator
//
function TdwsCompiler.ReadNewOperator(const aScriptPos: TScriptPos;
  opSym: TOperatorSymbol; elementTyp: TTypeSymbol): TTypedExpr;
begin
  result := TUnaryOpExprClass(opSym.OperatorExprClass).Create(FCompilerContext,
    aScriptPos, nil);
end;

// ReadAliasedNameSymbol
//
function TdwsCompiler.ReadAliasedNameSymbol(var namePos: TScriptPos): TSymbol;
var
  name: String;
  unitSym: TUnitSymbol;
begin
  // Declaration of a class reference
  if not FTok.TestDeleteNamePos(name, namePos) then
  begin
    namePos := FTok.hotPos;
    FMsgs.AddCompilerError(namePos, CPE_NameExpected);
    FTok.KillToken;
    result := nil;
    Exit;
  end;

  result := CurrentProg.table.FindTypeSymbol(name, cvMagic);

  if result = nil then
  begin

    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_UnknownName, [name]);

  end
  else if (result.baseType <> nil) and (result.baseType.ClassType = TUnitSymbol)
  then
  begin

    RecordSymbolUse(result, namePos, [suReference]);

    unitSym := TUnitSymbol(result.baseType);
    unitSym := ResolveUnitNameSpace(namePos, unitSym);

    namePos := FTok.hotPos;
    result := unitSym.table.FindLocal(FTok.GetToken.AsString);

    if not Assigned(result) then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_UnknownNameDotName,
        [unitSym.name, FTok.GetToken.AsString]);

    FTok.KillToken;
  end;
end;

// ReadNameSymbol
//
function TdwsCompiler.ReadNameSymbol(var namePos: TScriptPos): TSymbol;
begin
  result := ReadAliasedNameSymbol(namePos);
  if (result <> nil) and (result.ClassType = TAliasSymbol) then
    result := TAliasSymbol(result).UnAliasedType;
end;

// ReadClassName
//
function TdwsCompiler.ReadClassName: TClassSymbol;
var
  namePos: TScriptPos;
  sym: TSymbol;
begin
  sym := ReadNameSymbol(namePos);

  if not sym.IsClassSymbol then
  begin
    if Assigned(sym) then
      FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAClass, [sym.name]);
    result := FCompilerContext.TypTObject; // keep compiling
  end
  else
  begin
    result := TClassSymbol(sym);
    RecordSymbolUse(result, namePos, [suReference]);
  end;
end;

// ReadClassOf
//
function TdwsCompiler.ReadClassOf(const typeName: String): TClassOfSymbol;
var
  classTyp: TClassSymbol;
begin
  // Declaration of a class reference
  classTyp := ReadClassName;

  if typeName <> '' then
    result := TClassOfSymbol.Create(typeName, classTyp)
  else
    result := (classTyp.MetaSymbol as TClassOfSymbol);
end;

// ReadClassDeck
//
function TdwsCompiler.ReadClassDecl(const typeName: String;
  flags: TClassSymbolFlags; allowNonConstExpressions: Boolean): TClassSymbol;

  procedure CheckAndSetForwardDecl;
  begin
    if typeName = '' then
      FMsgs.AddCompilerError(FTok.hotPos,
        CPE_AnonymousClassesMustBeFullyDefined);
    if result.IsForwarded then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ClassForwardAlreadyExists,
        [result.name])
    else if csfPartial in flags then
      result.SetIsPartial;
    result.SetForwardedPos(FTok.hotPos);
  end;

var
  namePos, hotPos: TScriptPos;
  sym, typ: TSymbol;
  ancestorTyp: TClassSymbol;
  intfTyp: TInterfaceSymbol;
  interfaces: TList;
  missingMethod: TMethodSymbol;
  isInSymbolTable, firstVisibilityToken, hasSubTypes: Boolean;
  previousClassFlags: TClassSymbolFlags;
  visibility: TdwsVisibility;
  tt: TTokenType;
  i: Integer;
begin
  result := nil;

  // Check for a forward declaration of this class
  if typeName <> '' then
  begin
    if csfPartial in flags then
      sym := CurrentProg.table.FindTypeSymbol(typeName, cvMagic)
    else
      sym := CurrentProg.table.FindTypeLocal(typeName);

    if Assigned(sym) then
    begin
      if sym.IsClassSymbol then
      begin
        result := TClassSymbol(sym);
        if not(result.IsForwarded or result.IsPartial) then
        begin
          FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ClassAlreadyDefined,
            [sym.name]);
          result := nil;
        end;
      end
      else
      begin
        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_NameAlreadyExists,
          [sym.name]);
      end;
      if result = nil then // make anonymous to keep compiling
        result := TClassSymbol.Create('', CurrentUnitSymbol);
    end;
  end;

  isInSymbolTable := Assigned(result);

  if isInSymbolTable then
  begin
    previousClassFlags := result.flags;
    if (csfPartial in flags) and not CurrentProg.table.HasSymbol(result) then
    begin
      CurrentProg.table.AddSymbolDirect(result);
      result.IncRefCount;
    end;
  end
  else
  begin
    result := TClassSymbol.Create(typeName, CurrentUnitSymbol);
    previousClassFlags := [];
  end;

  // forwarded declaration
  if FTok.Test(ttSEMI) then
  begin
    CheckAndSetForwardDecl;
    Exit;
  end
  else
    result.ClearIsForwarded;

  hasSubTypes := False;
  if not isInSymbolTable then
    CurrentProg.table.AddSymbol(result); // auto-forward
  interfaces := TList.Create;
  try
    try
      if FTok.TestDelete(ttSTATIC) or (csfStatic in flags) then
      begin
        result.IsStatic := True;
      end;
      tt := FTok.TestDeleteAny([ttABSTRACT, ttSEALED]);
      case tt of
        ttABSTRACT:
          result.IsExplicitAbstract := True;
        ttSEALED:
          result.IsSealed := True;
      end;
      if FTok.TestDelete(ttEXTERNAL) then
      begin
        result.IsExternal := True;
        if FTok.Test(ttStrVal) then
        begin
          result.externalName := FTok.GetToken.AsString;
          FTok.KillToken;
        end;
      end;
      if FTok.TestDelete(ttPARTIAL) or (csfPartial in flags) then
      begin
        Include(flags, csfPartial);
        result.SetIsPartial;
        if isInSymbolTable then
        begin
          if not(csfPartial in previousClassFlags) then
            FMsgs.AddCompilerError(FTok.hotPos, CPE_ClassWasNotPartial)
          else if previousClassFlags <> result.flags then
            FMsgs.AddCompilerError(FTok.hotPos,
              CPE_ClassPartialModifiersNotMatched);
        end;
      end;
      if FTok.Test(ttSEMI) then
      begin
        CheckAndSetForwardDecl;
        Exit;
      end;

      if result.IsPartial and not(csfPartial in flags) then
        FMsgs.AddCompilerHint(FTok.hotPos, CPH_ClassWasPartial);

      // inheritance
      if FTok.TestDelete(ttBLEFT) then
      begin

        typ := ReadNameSymbol(namePos);
        if typ is TGenericSymbol then
          typ := ReadSpecializedType(TGenericSymbol(typ));

        if not typ.IsClassSymbol then
        begin
          if typ is TInterfaceSymbol then
            interfaces.Add(typ)
          else if typ <> nil then
            FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAClass, [typ.name]);
          typ := FCompilerContext.TypTObject;
        end;
        RecordSymbolUse(typ, namePos, [suReference]);

        ancestorTyp := TClassSymbol(typ);
        WarnDeprecatedType(namePos, ancestorTyp);

        if ancestorTyp.IsForwarded or (ancestorTyp = result) then
        begin
          FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ClassNotCompletelyDefined,
            [ancestorTyp.name]);
          ancestorTyp := FCompilerContext.TypTObject;
        end;

        if ancestorTyp.IsSealed then
          FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ClassIsSealed,
            [ancestorTyp.name]);
        if ancestorTyp.IsInternal then
          FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ClassIsInternal,
            [ancestorTyp.name]);

        while FTok.TestDelete(ttCOMMA) do
        begin

          typ := ReadNameSymbol(namePos);
          if not(typ is TInterfaceSymbol) then
          begin

            if typ <> nil then
              FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAnInterface,
                [typ.name]);

          end
          else
          begin

            intfTyp := TInterfaceSymbol(typ);
            if intfTyp.IsForwarded then
              FMsgs.AddCompilerErrorFmt(namePos,
                CPE_InterfaceNotCompletelyDefined, [typ.name]);
            if interfaces.IndexOf(intfTyp) >= 0 then
              FMsgs.AddCompilerErrorFmt(namePos,
                CPE_InterfaceAlreadyImplemented, [typ.name])
            else
              interfaces.Add(intfTyp);

          end;
        end;

        if not FTok.TestDelete(ttBRIGHT) then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);

      end
      else
      begin

        if (csfPartial in previousClassFlags) and (result.Parent <> nil) then
          ancestorTyp := result.Parent
        else if result.IsExternal or (typeName = '') then
        begin
          if Assigned(FOnRootExternalClass) then
            ancestorTyp := FOnRootExternalClass(Self, result.externalName)
          else
            ancestorTyp := FCompilerContext.TypObject;
        end
        else
          ancestorTyp := FCompilerContext.TypTObject;

      end;

      if result.IsExternal and (ancestorTyp <> FCompilerContext.TypObject) and
        (ancestorTyp.ExternalRoot = nil) then
        FMsgs.AddCompilerError(FTok.hotPos,
          CPE_ClassExternalAncestorMustBeExternalOrObject);

      if result.IsStatic and (ancestorTyp <> FCompilerContext.TypTObject) and
        (not ancestorTyp.IsStatic) then
      begin
        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ClassAncestorNotStatic,
          [ancestorTyp.name]);
      end;

      if result.Parent <> nil then
      begin
        if ancestorTyp <> result.Parent then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_ClassAncestorDoesNotMatch);
      end
      else if result.Parent <> ancestorTyp then
      begin
        if (not result.IsExternal) or (ancestorTyp.IsExternal) then
          result.InheritFrom(ancestorTyp);
      end;

      visibility := cvPublished;
      firstVisibilityToken := True;

      // standard class definition
      if not FTok.Test(ttSEMI) then
      begin

        while not FTok.Test(ttEND) do
        begin

          // Read methods and properties
          hotPos := FTok.hotPos;
          tt := FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD,
            ttCONSTRUCTOR, ttDESTRUCTOR, ttOPERATOR, ttCLASS, ttPROPERTY,
            ttCONST, ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
            ttALEFT, ttTYPE]);
          case tt of

            ttFUNCTION, ttPROCEDURE, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR:
              ReadMethodDecl(hotPos, result, cTokenToFuncKind[tt],
                visibility, []);

            ttCLASS:
              begin

                tt := FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD,
                  ttOPERATOR, ttVAR, ttCONST, ttPROPERTY]);
                case tt of
                  ttPROCEDURE, ttFUNCTION, ttMETHOD:
                    ReadMethodDecl(hotPos, result, cTokenToFuncKind[tt],
                      visibility, [mcoClassMethod]);
                  ttOPERATOR:
                    result.AddOperator(ReadClassOperatorDecl(result));
                  ttVAR:
                    ReadClassVars(result, visibility);
                  ttCONST:
                    ReadClassConst(result, visibility);
                  ttPROPERTY:
                    ReadPropertyDecl(result, visibility, True);
                else
                  FMsgs.AddCompilerStop(FTok.hotPos, CPE_ProcOrFuncExpected);
                end;

              end;
            ttPROPERTY:
              begin

                ReadPropertyDecl(result, visibility, False);

              end;
            ttOPERATOR:
              begin

                result.AddOperator(ReadClassOperatorDecl(result));

              end;
            ttCONST:
              begin

                ReadClassConst(result, visibility);

              end;
            ttPRIVATE .. ttPUBLISHED:
              begin

                if visibility = cTokenToVisibility[tt] then
                begin
                  if not firstVisibilityToken then
                    FMsgs.AddCompilerHintFmt(FTok.hotPos,
                      CPH_RedundantVisibilitySpecifier,
                      [cTokenStrings[tt]], hlStrict)
                end
                else
                  visibility := cTokenToVisibility[tt];
                firstVisibilityToken := False;

              end;
            ttALEFT:
              begin
                ReadAttributes(True);
              end;
            ttTYPE:
              begin
                if not hasSubTypes then
                begin
                  // only do this here when a subtype is explicitly requested, as full implications are still not clear
                  hasSubTypes := True;
                  TSymbolTable(result.Members).AddParent(FCurrentProg.table);
                  FCurrentProg.EnterSubTable(result.Members);
                end;
                ReadTypeDecl(True);
              end;

          else

            ReadFieldsDecl(result, visibility, allowNonConstExpressions, False);
            // if not (FTok.TestDelete(ttSEMI) or FTok.Test(ttEND)) then
            // Break;

          end;

        end; // while

        if not FTok.TestDelete(ttEND) then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_EndExpected);

        CheckNoPendingAttributes;

      end;

      // resolve interface tables
      for i := 0 to interfaces.Count - 1 do
      begin
        intfTyp := interfaces[i];
        if not result.AddInterface(intfTyp, cvPrivate, missingMethod) then
          FMsgs.AddCompilerErrorFmt(namePos, CPE_MissingMethodForInterface,
            [missingMethod.name, intfTyp.name]);
      end;

    except
      // Set Result to nil to prevent auto-forward removal then re-reraise
      if not isInSymbolTable then
        result := nil;
      raise;
    end;
  finally
    if hasSubTypes then
    begin
      FCurrentProg.table.RemoveParent(FCurrentProg.table);
      FCurrentProg.LeaveSubTable;
    end;
    interfaces.Free;
    if not isInSymbolTable then
      CurrentProg.table.Remove(result); // auto-forward
  end;
end;

// ReadClassVars
//
procedure TdwsCompiler.ReadClassVars(const ownerSymbol: TCompositeTypeSymbol;
  aVisibility: TdwsVisibility);
var
  factory: IdwsDataSymbolFactory;
begin
  factory := TCompositeTypeSymbolFactory.Create(Self, ownerSymbol, aVisibility);
  ReadVarDecl(factory, CurrentProg.initExpr);
  ReadSemiColon;
end;

// ReadClassConst
//
procedure TdwsCompiler.ReadClassConst(const ownerSymbol: TCompositeTypeSymbol;
  aVisibility: TdwsVisibility);
var
  factory: IdwsDataSymbolFactory;
begin
  factory := TCompositeTypeSymbolFactory.Create(Self, ownerSymbol, aVisibility);
  ReadConstDecl(factory);
  ReadSemiColon;
end;

// ReadClassConstSymbol
//
function TdwsCompiler.ReadClassConstSymbol(const constPos: TScriptPos;
  typ: TTypeSymbol; const ownerSymbol: TCompositeTypeSymbol;
  aVisibility: TdwsVisibility): TConstSymbol;
var
  factory: IdwsDataSymbolFactory;
begin
  factory := TCompositeTypeSymbolFactory.Create(Self, ownerSymbol, aVisibility);
  result := ReadConstSymbol('', constPos, typ, factory);
end;

// ReadInterface
//
function TdwsCompiler.ReadInterface(const typeName: String): TInterfaceSymbol;
var
  sym: TSymbol;
  ancestor: TInterfaceSymbol;
  namePos, hotPos: TScriptPos;
  tt: TTokenType;
  wasForwarded: Boolean;
begin
  hotPos := FTok.hotPos;
  sym := CurrentProg.table.FindSymbol(typeName, cvMagic);

  wasForwarded := False;
  if Assigned(sym) then
  begin
    if sym is TInterfaceSymbol then
    begin
      result := TInterfaceSymbol(sym);
      if not result.IsForwarded then
      begin
        result := nil;
        FMsgs.AddCompilerErrorFmt(hotPos, CPE_InterfaceAlreadyDefined,
          [sym.name]);
      end;
    end
    else
    begin
      result := nil;
      FMsgs.AddCompilerErrorFmt(hotPos, CPE_NameAlreadyExists, [sym.name]);
    end;
    if result = nil then
    begin
      // keep compiling, make it anonymous
      result := TInterfaceSymbol.Create('', CurrentUnitSymbol);
    end;
  end
  else
    result := TInterfaceSymbol.Create(typeName, CurrentUnitSymbol);

  if FTok.Test(ttSEMI) then
  begin
    // forward declaration
    if result.IsForwarded then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_InterfaceForwardAlreadyExists,
        [sym.name])
    else
      result.SetForwardedPos(hotPos);
    Exit;
  end
  else
    result.ClearIsForwarded;

  try
    // auto-forward
    if not wasForwarded then
      CurrentProg.table.AddSymbol(result);
    try

      if FTok.TestDelete(ttBLEFT) then
      begin

        sym := ReadNameSymbol(namePos);

        if not(sym is TInterfaceSymbol) then
        begin

          if sym <> nil then
            FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAnInterface, [sym.name])

        end
        else
        begin

          ancestor := TInterfaceSymbol(sym);
          if ancestor.IsForwarded then
            Msgs.AddCompilerErrorFmt(namePos, CPE_InterfaceNotCompletelyDefined,
              [ancestor.name]);
          result.InheritFrom(ancestor);

        end;
        if not FTok.TestDelete(ttBRIGHT) then
          FMsgs.AddCompilerStop(namePos, CPE_BrackRightExpected);
      end;
      if result.Parent = nil then
        result.InheritFrom(FCompilerContext.TypInterface);

      if FTok.TestDelete(ttALEFT) then
      begin
        // accept but ignore GUID
        if not FTok.TestDelete(ttStrVal) then
        begin
          FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected);
          FTok.SkipTo(ttARIGHT);
        end;
        if not FTok.TestDelete(ttARIGHT) then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_ArrayBracketRightExpected);
      end;

      while not FTok.Test(ttEND) do
      begin

        // Read methods and properties
        tt := FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD,
          ttPROPERTY]);
        case tt of
          ttFUNCTION, ttPROCEDURE, ttMETHOD:
            result.AddMethod(ReadIntfMethodDecl(result, cTokenToFuncKind[tt]));
          ttPROPERTY:
            ReadPropertyDecl(result, cvPublished, False);
        else
          Break;
        end;

      end;

      if not FTok.TestDelete(ttEND) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_EndExpected);
      CheckNoPendingAttributes;

    finally
      // remove auto-forward
      if not wasForwarded then
        CurrentProg.table.Remove(result);
    end;
  except
    OrphanObject(result);
    raise;
  end;
end;

// CheckPropertyFuncParams
//
function TdwsCompiler.CheckPropertyFuncParams(paramsA: TParamsSymbolTable;
  methSym: TMethodSymbol; indexSym: TSymbol = nil;
  typSym: TTypeSymbol = nil): Boolean;
var
  paramsB: TParamsSymbolTable;
  skipB: Integer;
begin
  result := False;
  paramsB := methSym.Params;

  if (paramsB.Count > 0) and (paramsB[0] = methSym.selfSym) then
    skipB := 1
  else
    skipB := 0;

  if Assigned(indexSym) then
  begin
    if Assigned(typSym) then
    begin
      if paramsB.Count <> paramsA.Count + skipB + 2 then
        Exit;
      if not paramsB[paramsA.Count + skipB + 1].typ.SameType(typSym) then
        Exit;
      if paramsB[paramsA.Count + skipB].typ <> indexSym then
        Exit;
    end
    else
    begin
      if paramsB.Count <> paramsA.Count + skipB + 1 then
        Exit
      else if paramsB[paramsA.Count + skipB].typ <> indexSym then
        Exit;
    end;
  end
  else
  begin
    if Assigned(typSym) then
    begin
      if paramsB.Count <> paramsA.Count + skipB + 1 then
        Exit;
      if not paramsB[paramsA.Count + skipB].typ.SameType(typSym) then
        Exit;
    end
    else
    begin
      if paramsA.Count + skipB <> paramsB.Count then
        Exit;
    end;
  end;

  result := CheckParams(paramsA, paramsB, False, skipB);
end;

// ReadClassOperatorDecl
//
function TdwsCompiler.ReadClassOperatorDecl(ClassSym: TClassSymbol)
  : TClassOperatorSymbol;
var
  tt: TTokenType;
  usesName: String;
  usesPos: TScriptPos;
  sym: TTypeSymbol;
begin
  tt := FTok.TestDeleteAny([ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN,
    ttDIVIDE_ASSIGN, ttIN, ttCARET_ASSIGN]);
  if tt = ttNone then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_OverloadableOperatorExpected);

  result := TClassOperatorSymbol.Create(tt);
  try
    result.typ := ReadType('', tcOperand);

    if ClassSym.FindClassOperatorStrict(tt, result.typ, False) <> nil then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ClassOperatorRedefined,
        [result.typ.name]);

    if not FTok.TestDelete(ttUSES) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_UsesExpected);

    if not FTok.TestDeleteNamePos(usesName, usesPos) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

    sym := ClassSym.Members.FindTypeSymbol(usesName, cvPrivate);

    if (not Assigned(sym)) or (not(sym is TMethodSymbol)) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ProcedureMethodExpected);

    result.usesSym := TMethodSymbol(sym);
    RecordSymbolUse(sym, usesPos, [suReference]);

    if result.usesSym.Params.Count <> 1 then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_SingleParameterExpected);
    if tt = ttIN then
    begin
      if not result.usesSym.Params[0].typ.IsOfType(result.typ) then
        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_InvalidParameterType,
          [result.usesSym.name]);
      if not result.usesSym.result.typ.IsOfType(FCompilerContext.TypBoolean)
      then
        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_InvalidResultType,
          [result.usesSym.result.typ.name]);
    end
    else
    begin
      if result.usesSym.Params[0].typ <> result.typ then
        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_InvalidParameterType,
          [result.usesSym.name]);
    end;

    ReadSemiColon;
  except
    OrphanObject(result);
    raise;
  end;
end;

// ReadPropertyDecl
//
procedure TdwsCompiler.ReadPropertyDecl(ownerSym: TCompositeTypeSymbol;
  aVisibility: TdwsVisibility; classProperty: Boolean);
var
  gotReadOrWrite: Boolean;
  name: String;
  propSym, promotedPropSym: TPropertySymbol;
  sym: TSymbol;
  typ: TTypeSymbol;
  tempArrayIndices: TParamsSymbolTable;
  propStartPos, propNamePos: TScriptPos;
  accessPos: TScriptPos;
  // Position where either a Read or Write symbol is found
  indexExpr: TTypedExpr;
  indexTyp: TTypeSymbol;
begin
  propStartPos := FTok.hotPos;

  // Read property name
  if not FTok.TestDeleteNamePos(name, propNamePos) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

  // Check if property name is available
  sym := ownerSym.Members.FindSymbolFromScope(name, ownerSym);
  if Assigned(sym) then
  begin
    if sym is TPropertySymbol then
    begin
      if TPropertySymbol(sym).ownerSymbol = ownerSym then
      begin
        MemberSymbolWithNameAlreadyExists(sym, propNamePos);
        name := ''; // make anonymous to keep compiling
      end;
    end
    else if ownerSym.Members.HasSymbol(sym) then
    begin
      MemberSymbolWithNameAlreadyExists(sym, propNamePos);
      name := ''; // make anonymous to keep compiling
    end;
  end;

  if (not classProperty) and FTok.TestDelete(ttSEMI) then
  begin
    // property visibility promotion
    if sym = nil then
      ReportNoMemberForType(name, propNamePos, ownerSym, False)
    else if not(sym is TPropertySymbol) then
      FMsgs.AddCompilerErrorFmt(propNamePos, CPE_NotAProperty, [sym.name])
    else
    begin
      propSym := TPropertySymbol(sym);
      if propSym.visibility > aVisibility then
        FMsgs.AddCompilerError(propNamePos, CPE_CannotDemotePropertyVisibility)
      else
      begin
        promotedPropSym := TPropertySymbol.Create(propSym.name, propSym.typ,
          aVisibility, propSym.ArrayIndices);
        propSym.ArrayIndices.IncRefCount;
        promotedPropSym.ReadSym := propSym.ReadSym;
        promotedPropSym.WriteSym := propSym.WriteSym;
        ownerSym.AddProperty(promotedPropSym);
      end;
    end;
    Exit;
  end;

  if FTok.TestDelete(ttALEFT) then
  begin
    tempArrayIndices := TParamsSymbolTable.Create;
    try
      ReadArrayParams(tempArrayIndices);
    except
      OrphanObject(tempArrayIndices);
      raise;
    end;
  end
  else
    tempArrayIndices := nil;

  if not FTok.TestDelete(ttCOLON) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_ColonExpected);

  typ := ReadType('', tcProperty);

  propSym := TPropertySymbol.Create(name, typ, aVisibility, tempArrayIndices);
  ownerSym.AddProperty(propSym);

  RecordSymbolUse(propSym, propNamePos, [suDeclaration]);

  if coContextMap in FOptions then
    FSourceContextMap.OpenContext(propStartPos, propSym, ttPROPERTY);

  if FTok.TestDelete(ttEXTERNAL) then
  begin
    if not FTok.Test(ttStrVal) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected)
    else
    begin
      propSym.externalName := FTok.GetToken.AsString;
      FTok.KillToken;
    end;
  end;

  if FTok.TestDelete(ttINDEX) then
  begin
    indexExpr := ReadExpr;
    indexTyp := indexExpr.typ;
    if not(indexExpr is TConstExpr) then
    begin
      FMsgs.AddCompilerError(FTok.hotPos, CPE_ConstantExpressionExpected);
    end
    else
    begin
      propSym.SetIndex(TConstExpr(indexExpr).DataContext, indexTyp);
    end;
    OrphanAndNil(indexExpr);
  end
  else
    indexTyp := nil;

  gotReadOrWrite := False;

  if FTok.Test(ttSEMI) and ownerSym.AllowFields then
  begin

    gotReadOrWrite := True;

    // shorthand with anonymous private field
    ReadPropertyDeclAutoField(propSym, classProperty);

  end
  else
  begin

    if FTok.TestDelete(ttREAD) then
    begin

      gotReadOrWrite := True;

      sym := ReadPropertyDeclGetter(propSym, accessPos, classProperty);

      if sym = nil then

        // error already handled

      else if (sym.typ = nil) or ((not sym.typ.SameType(propSym.typ)) and
        (not sym.typ.IsOfType(propSym.typ)) and
        not(sym.IsGeneric or propSym.IsGeneric)) then
      begin

        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_IncompatibleType, [sym.name])

      end
      else if sym is TMethodSymbol then
      begin

        if classProperty and not TMethodSymbol(sym).IsClassMethod then
          FMsgs.AddCompilerError(accessPos,
            CPE_ClassMethodOrConstructorExpected);
        if not CheckPropertyFuncParams(propSym.ArrayIndices, TMethodSymbol(sym),
          indexTyp) then
          FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_IncompatibleParameters,
            [sym.name]);

      end
      else if propSym.HasArrayIndices then
      begin

        FMsgs.AddCompilerError(FTok.hotPos, CPE_FunctionMethodExpected);

      end
      else if classProperty and (sym.ClassType = TFieldSymbol) then
      begin

        FMsgs.AddCompilerError(accessPos, CPE_ClassMemberExpected);

      end;

      propSym.ReadSym := sym;
      RecordSymbolUse(sym, accessPos, [suReference, suRead]);

    end;

    if FTok.TestDelete(ttWRITE) then
    begin

      gotReadOrWrite := True;
      sym := ReadPropertyDeclSetter(propSym, accessPos, classProperty);

      if sym = nil then

        // error already handled

      else if sym is TMethodSymbol then
      begin

        if classProperty and not TMethodSymbol(sym).IsClassMethod then
          FMsgs.AddCompilerError(accessPos,
            CPE_ClassMethodOrConstructorExpected);
        if (not(TMethodSymbol(sym).Kind in [fkProcedure, fkMethod])) or
          (TMethodSymbol(sym).typ <> nil) then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_ProcedureMethodExpected)
        else if not CheckPropertyFuncParams(propSym.ArrayIndices,
          TMethodSymbol(sym), indexTyp, propSym.typ) then
          FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_IncompatibleParameters,
            [sym.name]);

      end
      else if (propSym.typ = nil) or not propSym.typ.SameType(sym.typ) then
      begin

        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_IncompatibleWriteSymbol,
          [sym.name]);

      end
      else if sym is TConstSymbol then
      begin

        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ConstantCannotBeWrittenTo,
          [sym.name]);

      end
      else if sym.ClassType = TFieldSymbol then
      begin

        if classProperty then
          FMsgs.AddCompilerError(accessPos, CPE_ClassMemberExpected);
        if TFieldSymbol(sym).ReadOnly then
          FMsgs.AddCompilerErrorFmt(accessPos, CPE_FieldIsReadOnly, [sym.name]);

      end;

      propSym.WriteSym := sym;
      RecordSymbolUse(sym, accessPos, [suReference, suWrite]);
    end;

  end;

  if not gotReadOrWrite then
    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ReadOrWriteExpected, [name]);

  if FTok.TestDelete(ttDEFAULT) then
  begin

    propSym.DefaultSym := ReadClassConstSymbol(FTok.hotPos, propSym.typ,
      ownerSym, aVisibility);
    if propSym.DefaultSym <> nil then
      propSym.DefaultSym.IncRefCount;

  end;

  if FTok.TestDelete(ttDESCRIPTION) then
  begin
    if FTok.Test(ttStrVal) then
    begin
      propSym.UserDescription := FTok.GetToken.AsString;
      FTok.KillToken;
    end
    else
    begin
      FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected);
    end;
  end;

  if FTok.TestDelete(ttREINTRODUCE) then
  begin
    propSym.IsReintroduce := True;
  end;

  ReadSemiColon;

  // Array-Prop can be default
  if propSym.HasArrayIndices then
  begin
    if FTok.TestDelete(ttDEFAULT) then
    begin
      ReadSemiColon;
      if not ownerSym.AllowDefaultProperty then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_NoDefaultPropertyAllowed)
      else if ownerSym.DefaultProperty <> nil then
        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_MultipleDefaultProperties,
          [ownerSym.name, ownerSym.DefaultProperty.name])
      else
        ownerSym.DefaultProperty := propSym;
    end;
  end;

  if FTok.Test(ttDEPRECATED) then
    propSym.deprecatedMessage := ReadDeprecatedMessage;

  if coContextMap in FOptions then
    FSourceContextMap.CloseContext(FTok.CurrentPos);
end;

// ReadPropertyDeclAutoField
//
procedure TdwsCompiler.ReadPropertyDeclAutoField(propSym: TPropertySymbol;
  classProperty: Boolean);
var
  field: TFieldSymbol;
  classVar: TDataSymbol;
  sym: TSymbol;
  factory: IdwsDataSymbolFactory;
  assignExpr: TProgramExpr;
  initExpr: TTypedExpr;
begin
  if classProperty then
  begin
    factory := TCompositeTypeSymbolFactory.Create(Self, propSym.ownerSymbol,
      propSym.visibility);
    if FTok.TestDeleteAny([ttEQ, ttASSIGN]) <> ttNone then
      initExpr := factory.ReadInitExpr(propSym.typ)
    else
      initExpr := nil;
    assignExpr := CreateNamedVarDeclExpr(factory, '', '', FTok.hotPos,
      propSym.typ, initExpr, classVar);
    if assignExpr <> nil then
      CurrentProg.initExpr.AddStatement(assignExpr);
    sym := classVar;
  end
  else
  begin
    field := TFieldSymbol.Create('', propSym.typ, cvPrivate);
    field.externalName := propSym.name;
    propSym.ownerSymbol.AddField(field);
    sym := field;
  end;
  propSym.ReadSym := sym;
  propSym.WriteSym := sym;
end;

// ReadPropertyExternalField
//
function TdwsCompiler.ReadPropertyExternalField(propSym: TPropertySymbol)
  : TFieldSymbol;
var
  i: Integer;
  Members: TMembersSymbolTable;
  sym: TSymbol;
  externalName: String;
begin
  if not FTok.Test(ttStrVal) then
    FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected)
  else
  begin
    externalName := FTok.GetToken.AsString;
    FTok.KillToken;
  end;

  Members := propSym.ownerSymbol.Members;
  for i := 0 to Members.Count - 1 do
  begin
    sym := Members.Symbols[i];
    if (sym.ClassType = TFieldSymbol) and (sym.externalName = externalName) then
      Exit(TFieldSymbol(sym));
  end;

  result := TFieldSymbol.Create('', propSym.typ, cvPrivate);
  result.externalName := externalName;

  propSym.ownerSymbol.AddField(result);
end;

// ReadPropertyDeclGetter
//
function TdwsCompiler.ReadPropertyDeclGetter(propSym: TPropertySymbol;
  var scriptPos: TScriptPos; classProperty: Boolean): TSymbol;
var
  name: String;
  expr: TTypedExpr;
  resultExpr: TVarExpr;
  meth: TMethodSymbol;
  oldprog: TdwsProgram;
  proc: TdwsProcedure;
begin
  if propSym.ownerSymbol.IsExternal and FTok.TestDelete(ttEXTERNAL) then
  begin

    result := ReadPropertyExternalField(propSym);

  end
  else if not FTok.TestDelete(ttBLEFT) then
  begin

    if not FTok.TestDeleteNamePos(name, scriptPos) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

    result := propSym.ownerSymbol.Members.FindSymbol(name, cvPrivate);

    if (result <> nil) and (result.ClassType = TPropertySymbol) then
    begin
      result := TPropertySymbol(result).ReadSym;
      if result = nil then
      begin
        FMsgs.AddCompilerError(scriptPos, CPE_WriteOnlyProperty);
        Exit;
      end;
    end;

  end
  else
  begin

    scriptPos := FTok.hotPos;

    if not propSym.ownerSymbol.AllowAnonymousMethods then
      FMsgs.AddCompilerError(scriptPos, CPE_AnonymousMethodsNotAllowedHere);

    var
      createOptions: TMethodCreateOptions;
    if classProperty then
      createOptions := [mcoClassMethod]
    else
      createOptions := [];

    meth := propSym.ownerSymbol.CreateAnonymousMethod(fkFunction, cvPrivate,
      createOptions);
    meth.typ := propSym.typ;
    propSym.ownerSymbol.AddMethod(meth);
    meth.AddParams(propSym.ArrayIndices);

    RecordSymbolUse(meth, scriptPos, [suDeclaration, suImplicit]);

    proc := TdwsProcedure.Create(CurrentProg);
    proc.AssignTo(meth);

    oldprog := CurrentProg;
    CurrentProg := proc;
    try
      expr := ReadExpr(propSym.typ);
      if expr <> nil then
      begin
        resultExpr := TVarExpr.CreateTyped(FCompilerContext, scriptPos,
          proc.func.result);
        proc.expr := CreateAssign(scriptPos, ttASSIGN, resultExpr, expr);
      end;
    finally
      CurrentProg := oldprog;
    end;

    result := meth;

    if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_BrackRightExpected);
  end;

  if result = nil then
    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_FieldMethodUnknown, [name]);
end;

// ReadPropertyDeclSetter
//
function TdwsCompiler.ReadPropertyDeclSetter(propSym: TPropertySymbol;
  var scriptPos: TScriptPos; classProperty: Boolean): TSymbol;
var
  name: String;
  instr: TProgramExpr;
  expr: TTypedExpr;
  leftExpr: TDataExpr;
  paramExpr: TVarExpr;
  meth: TMethodSymbol;
  paramSymbol: TParamSymbol;
  oldprog: TdwsProgram;
  proc: TdwsProcedure;
begin
  if propSym.ownerSymbol.IsExternal and FTok.TestDelete(ttEXTERNAL) then
  begin

    result := ReadPropertyExternalField(propSym);

  end
  else if not FTok.TestDelete(ttBLEFT) then
  begin

    if not FTok.TestDeleteNamePos(name, scriptPos) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

    result := propSym.ownerSymbol.Members.FindSymbol(name, cvPrivate);

    if (result <> nil) and (result.ClassType = TPropertySymbol) then
    begin
      result := TPropertySymbol(result).WriteSym;
      if result = nil then
      begin
        FMsgs.AddCompilerError(scriptPos, CPE_WriteOnlyProperty);
        Exit;
      end;
    end;

  end
  else
  begin

    scriptPos := FTok.hotPos;

    if not propSym.ownerSymbol.AllowAnonymousMethods then
      FMsgs.AddCompilerError(scriptPos, CPE_AnonymousMethodsNotAllowedHere);

    var
      createOptions: TMethodCreateOptions;
    if classProperty then
      createOptions := [mcoClassMethod]
    else
      createOptions := [];

    meth := propSym.ownerSymbol.CreateAnonymousMethod(fkProcedure, cvPrivate,
      createOptions);

    meth.AddParams(propSym.ArrayIndices);
    paramSymbol := CreateConstParamSymbol('Value', propSym.typ, [psoInternal]);
    meth.Params.AddSymbol(paramSymbol);

    propSym.ownerSymbol.AddMethod(meth);

    RecordSymbolUse(meth, scriptPos, [suDeclaration, suImplicit]);

    proc := TdwsProcedure.Create(CurrentProg);
    proc.AssignTo(meth);

    oldprog := CurrentProg;
    CurrentProg := proc;
    FPendingSetterValueExpr := nil;
    try
      FPendingSetterValueExpr := GetConstParamExpr(scriptPos, paramSymbol);
      instr := ReadInstr;
      if instr is TNullExpr then
        FMsgs.AddCompilerWarning(scriptPos, CPW_PropertyWriterDoesNothing);
      if (instr is TVarExpr) or (instr is TFieldExpr) or
        (instr is TNoResultWrapperExpr) then
      begin
        if instr is TNoResultWrapperExpr then
          expr := TNoResultWrapperExpr(instr).expr
        else
          expr := TTypedExpr(instr);
        if expr.typ.IsOfType(propSym.typ) then
        begin
          if expr <> nil then
          begin
            TNoResultWrapperExpr(instr).expr := nil;
            OrphanAndNil(instr);
          end;
          instr := nil;
          if (expr is TDataExpr) and TDataExpr(expr).IsWritable then
          begin
            leftExpr := TDataExpr(expr);
            paramExpr := GetConstParamExpr(scriptPos, paramSymbol);
            proc.expr := CreateAssign(scriptPos, ttASSIGN, leftExpr, paramExpr);
          end
          else
          begin
            FMsgs.AddCompilerError(scriptPos, CPE_CantWriteToLeftSide);
            OrphanAndNil(expr);
          end;
        end;
      end;
      if instr <> nil then
        proc.expr := instr;
    finally
      OrphanAndNil(FPendingSetterValueExpr);
      CurrentProg := oldprog;
    end;

    result := meth;

    if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_BrackRightExpected);
  end;

  if result = nil then
    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_FieldMethodUnknown, [name]);
end;

// ReadRecordDecl
//
function TdwsCompiler.ReadRecordDecl(const typeName: String;
  allowNonConstExpressions: Boolean): TRecordSymbol;
var
  meth: TMethodSymbol;
  hotPos: TScriptPos;
  visibility: TdwsVisibility;
  tt: TTokenType;
begin
  result := TRecordSymbol.Create(typeName, CurrentUnitSymbol);
  try
    CurrentProg.table.AddSymbol(result); // auto-forward
    try
      if typeName = '' then
        visibility := cvPublished
      else
        visibility := cvPublic;

      if FTok.TestDelete(ttEXTERNAL) then
        result.SetIsExternal;

      repeat

        hotPos := FTok.hotPos;
        tt := FTok.TestDeleteAny([ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
          ttCLASS, ttPROPERTY, ttFUNCTION, ttPROCEDURE, ttMETHOD, ttCONST]);
        case tt of
          ttPRIVATE, ttPUBLIC, ttPUBLISHED:
            if visibility = cTokenToVisibility[tt] then
              FMsgs.AddCompilerHintFmt(FTok.hotPos,
                CPH_RedundantVisibilitySpecifier, [cTokenStrings[tt]])
            else
              visibility := cTokenToVisibility[tt];
          ttPROTECTED:
            FMsgs.AddCompilerError(FTok.hotPos,
              CPE_NoProtectedVisibilityForRecords);
          ttPROPERTY:
            ReadPropertyDecl(result, visibility, False);
          ttFUNCTION, ttPROCEDURE, ttMETHOD:
            begin
              meth := ReadMethodDecl(hotPos, result, cTokenToFuncKind[tt],
                visibility, []);
              if meth.IsForwarded then
                FMsgs.AddCompilerError(hotPos,
                  CPE_AnonymousRecordMethodsMustBeInline);
            end;
          ttCLASS:
            begin
              tt := FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD,
                ttVAR, ttCONST, ttPROPERTY]);
              case tt of
                ttPROCEDURE, ttFUNCTION, ttMETHOD:
                  begin
                    meth := ReadMethodDecl(hotPos, result, cTokenToFuncKind[tt],
                      visibility, [mcoClassMethod]);
                    if meth.IsForwarded then
                      FMsgs.AddCompilerError(hotPos,
                        CPE_AnonymousRecordMethodsMustBeInline);
                  end;
                ttVAR:
                  ReadClassVars(result, visibility);
                ttCONST:
                  ReadClassConst(result, visibility);
                ttPROPERTY:
                  ReadPropertyDecl(result, visibility, True);
              else
                FMsgs.AddCompilerStop(FTok.hotPos, CPE_ProcOrFuncExpected);
              end;
            end;
          ttCONST:
            ReadClassConst(result, visibility);
        else
          if FTok.Test(ttEND) then
            Break;

          if result.Members.HasMethods then
            FMsgs.AddCompilerStop(FTok.hotPos,
              CPE_RecordFieldsMustBeBeforeMethods);

          ReadFieldsDecl(result, visibility, allowNonConstExpressions,
            result.IsExternal);

          if FTok.Test(ttEND) then
            Break;
          // if not FTok.TestDelete(ttSEMI) then
          // Break;
        end;
      until not FTok.HasTokens;
    finally
      CurrentProg.table.Remove(result);
    end;

    if not FTok.TestDelete(ttEND) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_EndExpected);
    if result.Size = 0 then
    begin
      // accept zero size for anonymous records, provided they actually have a field
      if (result.FirstField = nil) or (result.name <> '') then
        FMsgs.AddCompilerError(FTok.hotPos, RTE_NoRecordFields);
    end;
    CheckNoPendingAttributes;

    result.IsFullyDefined := True;
  except
    OrphanObject(result);
    raise;
  end;
end;

// ReadFieldsDecl
//
procedure TdwsCompiler.ReadFieldsDecl(struct: TStructuredTypeSymbol;
  visibility: TdwsVisibility; allowNonConstExpressions: Boolean;
  forceExternal: Boolean);
var
  x: Integer;
  names: TSimpleStringList;
  sym: TSymbol;
  member: TFieldSymbol;
  typ: TTypeSymbol;
  posArray: TScriptPosArray;
  expr: TTypedExpr;
  exprData: IDataContext;
  exprDyn: TTypedExpr;
  detachTyp: Boolean;
  options: TdwsNameListOptions;
  factory: IdwsDataSymbolFactory;
  readOnlyPos: TScriptPos;
  readOnlyName: String;
  missingColon: Boolean;
begin
  missingColon := False;
  names := FStringListPool.Acquire;
  try
    options := [];
    if struct.name = '' then
      Include(options, nloAllowStrings);
    if struct.IsExternal then
      Include(options, nloNoCheckSpecials);
    ReadNameList(names, posArray, options);

    if FTok.TestDelete(ttCOLON) then
    begin
      typ := ReadType('', tcConstant);
    end
    else
      typ := nil;

    exprDyn := nil;
    if FTok.TestDeleteAny([ttEQ, ttASSIGN]) <> ttNone then
    begin
      detachTyp := False;
      if factory = nil then
        factory := TCompositeTypeSymbolFactory.Create(Self, struct, cvPrivate);
      if (typ is TRecordSymbol) and FTok.Test(ttBLEFT) then
      begin
        exprData := ReadConstRecord(TRecordSymbol(typ));
      end
      else
      begin
        expr := factory.ReadExpr(typ);
        try
          if Assigned(typ) then
          begin
            if not typ.IsCompatible(expr.typ) then
              expr := CompilerUtils.WrapWithImplicitConversion(FCompilerContext,
                expr, typ, FTok.hotPos);
          end
          else
          begin
            typ := expr.typ;
            detachTyp := (typ.name = '');
          end;
          if typ = FCompilerContext.TypNil then
            if not(expr is TBogusConstExpr) then
              FMsgs.AddCompilerError(FTok.hotPos, CPE_TypeCouldNotBeInferenced);

          if (typ = nil) or (expr = nil) then
          begin
            FMsgs.AddCompilerError(FTok.hotPos, CPE_ConstantExpressionExpected);
            if typ = nil then
              typ := FCompilerContext.TypVariant;
          end
          else if expr.IsConstant then
          begin
            if not FMsgs.HasErrors then
            begin
              exprData := TDataContext.CreateStandalone(typ.Size);
              case typ.Size of
                0: // accept zero-size on anonymous structures (useful for JSON f.i.)
                  if struct.name <> '' then
                    FMsgs.AddCompilerError(FTok.hotPos,
                      CPE_ConstantInstruction);
                1:
                  begin
                    expr.EvalAsVariantToDataContext(FExec, exprData, 0);
                  end;
              else
                if expr.ClassType = TArrayConstantExpr then
                  TArrayConstantExpr(expr).Prepare(FCompilerContext, typ.typ);
                FExec.Stack.Push(typ.Size);
                try
                  exprData.WriteData((expr as TDataExpr).DataPtr[FExec],
                    typ.Size);
                finally
                  FExec.Stack.Pop(typ.Size);
                end;
              end;
            end;
          end
          else if not allowNonConstExpressions then
          begin
            FMsgs.AddCompilerError(FTok.hotPos, CPE_ConstantExpressionExpected);
          end
          else
          begin
            exprDyn := expr;
            expr := nil;
            detachTyp := False;
          end;
        finally
          if detachTyp then
          begin
            if not CurrentProg.table.HasSymbol(typ) then
            begin
              CurrentProg.table.AddSymbol(typ);
              if typ.name = '' then
                CurrentProg.RootTable.RemoveFromDestructionList(typ);
            end;
            expr.typ := nil;
          end;
          OrphanAndNil(expr);
        end;
      end;
    end
    else
    begin
      exprData := nil;
      if typ = nil then
      begin
        missingColon := True;
        FMsgs.AddCompilerError(FTok.hotPos, CPE_ColonExpected);
        typ := FCompilerContext.TypVariant;
      end;
    end;
    if (typ = struct) and (typ.ClassType = TRecordSymbol) then
      FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_RecordTypeNotFullyDefined,
        [typ.name]);

    var
    firstAddedFieldIndex := struct.Members.Count;
    member := nil;
    for x := 0 to names.Count - 1 do
    begin
      sym := struct.Members.FindLocal(names[x]);
      if Assigned(sym) then
        MemberSymbolWithNameAlreadyExists(sym, posArray[x]);

      member := TFieldSymbol.Create(names[x], typ, visibility);
      if exprDyn <> nil then
      begin
        case x of
          0:
            member.DefaultExpr := exprDyn;
          1:
            FMsgs.AddCompilerError(FTok.hotPos,
              CPE_OnlyOneFieldExpectedForExternal);
        end;
      end;
      struct.AddField(member);
      if exprData <> nil then
        member.DefaultValue := TDataContext.CreateCopy(exprData);
      if forceExternal then
        member.externalName := member.name;

      // Add member symbols and positions
      RecordSymbolUse(member, posArray[x], [suDeclaration]);
    end;

    if FTok.TestDelete(ttSEMI) then
    begin

      if FTok.TestDelete(ttEXTERNAL) then
      begin

        if names.Count <> 1 then
          FMsgs.AddCompilerError(FTok.hotPos,
            CPE_OnlyOneFieldExpectedForExternal);

        if FTok.Test(ttStrVal) then
        begin
          if member <> nil then
            member.externalName := FTok.GetToken.AsString;
          FTok.KillToken;
        end
        else
          FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected);

        if not FTok.TestDelete(ttSEMI) then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_SemiExpected);

      end;

      if struct.IsClassSymbol then
      begin

        if FTok.Test(ttREADONLY) then
        begin

          // this may not be a qualifier, but a field named readonly
          // we need to acquire its string representation to preserve case
          FTok.TestDeleteNamePos(readOnlyName, readOnlyPos);

          if not FTok.Test(ttSEMI) then
          begin

            // this is not a qualifier but a field named readonly
            FTok.SimulateNameToken(readOnlyPos, readOnlyName);

          end
          else
          begin

            // this actually a readonly qualifier
            for var i := firstAddedFieldIndex to struct.Members.Count - 1 do
              (struct.Members[i] as TFieldSymbol).ReadOnly := True;
            if not FTok.TestDelete(ttSEMI) then
              FMsgs.AddCompilerError(FTok.hotPos, CPE_SemiExpected);

          end;

        end;

      end;

    end
    else if not FTok.Test(ttEND) then
    begin

      if not missingColon then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_SemiExpected);

    end;

  finally
    FStringListPool.Release(names);
  end;
end;

// ReadHelperDecl
//
function TdwsCompiler.ReadHelperDecl(const typeName: String;
  qualifierToken: TTokenType; isStrict: Boolean): THelperSymbol;
var
  hotPos: TScriptPos;
  visibility: TdwsVisibility;
  tt: TTokenType;
  ForType: TTypeSymbol;
  member: TSymbol;
begin
  if not FTok.TestDelete(ttFOR) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_ForExpected);
  hotPos := FTok.hotPos;
  ForType := ReadType('', tcHelper);
  if ForType.AsFuncSymbol <> nil then
    FMsgs.AddCompilerError(hotPos, CPE_HelpersNotAllowedForDelegates);
  case qualifierToken of
    ttNone:
      ;
    ttCLASS:
      if not(ForType.IsClassSymbol or (ForType is TClassOfSymbol)) then
        FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
    ttRECORD:
      if not((ForType is TRecordSymbol) or (ForType is TBaseSymbol)) then
        FMsgs.AddCompilerError(hotPos, CPE_RecordTypeExpected);
    ttINTERFACE:
      if not(ForType is TInterfaceSymbol) then
        FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
  else
    Assert(False);
  end;

  result := THelperSymbol.Create(typeName, CurrentUnitSymbol, ForType,
    CurrentProg.table.Count);
  try
    result.Strict := isStrict;
    visibility := cvPublic;

    repeat

      hotPos := FTok.hotPos;
      tt := FTok.TestDeleteAny([ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
        ttCLASS, ttPROPERTY, ttFUNCTION, ttPROCEDURE, ttMETHOD, ttCONST]);
      case tt of
        ttPRIVATE, ttPUBLIC, ttPUBLISHED:
          if visibility = cTokenToVisibility[tt] then
            FMsgs.AddCompilerHintFmt(FTok.hotPos,
              CPH_RedundantVisibilitySpecifier, [cTokenStrings[tt]])
          else
            visibility := cTokenToVisibility[tt];
        ttPROTECTED:
          FMsgs.AddCompilerError(FTok.hotPos,
            CPE_NoProtectedVisibilityForHelpers);
        ttPROPERTY:
          ReadPropertyDecl(result, visibility, False);
        ttFUNCTION, ttPROCEDURE, ttMETHOD:
          ReadMethodDecl(hotPos, result, cTokenToFuncKind[tt], visibility, []);
        ttCLASS:
          begin
            tt := FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttVAR,
              ttCONST, ttPROPERTY]);
            case tt of
              ttPROCEDURE, ttFUNCTION, ttMETHOD:
                ReadMethodDecl(hotPos, result, cTokenToFuncKind[tt], visibility,
                  [mcoClassMethod]);
              ttVAR:
                ReadClassVars(result, visibility);
              ttCONST:
                ReadClassConst(result, visibility);
              ttPROPERTY:
                ReadPropertyDecl(result, visibility, True);
            else
              FMsgs.AddCompilerStop(FTok.hotPos, CPE_ProcOrFuncExpected);
            end;
          end;
        ttCONST:
          ReadClassConst(result, visibility);
      else
        Break;
      end;
    until not FTok.HasTokens;

    if not FTok.TestDelete(ttEND) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_EndExpected);
    CheckNoPendingAttributes;

    for member in result.Members do
      FCompilerContext.HelperMemberNames.Add(member.name);

  except
    OrphanObject(result);
    raise;
  end;
end;

// ReadTry
//
function TdwsCompiler.ReadTry: TExceptionExpr;
var
  tryBlock: TProgramExpr;
  tt: TTokenType;
  wasExcept: Boolean;
  exceptExpr: TExceptExpr;
  finallyExpr: TFinallyExpr;
begin
  result := nil;
  try
    wasExcept := FIsExcept;
    FIsExcept := False;
    try
      tryBlock := ReadBlocks([ttFINALLY, ttEXCEPT], tt);
      if tt = ttEXCEPT then
      begin
        FIsExcept := True;
        exceptExpr := TExceptExpr.Create(tryBlock);
        result := exceptExpr;
        ReadExcept(exceptExpr, tt);
        tryBlock := result;
      end;
      if tt = ttFINALLY then
      begin
        finallyExpr := TFinallyExpr.Create(tryBlock);
        result := finallyExpr;
        ReadFinally(finallyExpr);
      end;
    finally
      FIsExcept := wasExcept;
    end;
  except
    if result <> nil then
      result.Orphan(FCompilerContext);
    raise;
  end;
end;

// ReadFinally
//
procedure TdwsCompiler.ReadFinally(finallyExpr: TFinallyExpr);
var
  tt: TTokenType;
begin
  FFinallyExprs.Push(True);
  try
    finallyExpr.HandlerExpr := ReadBlocks([ttEND], tt);
  finally
    FFinallyExprs.Pop;
  end;
end;

// ReadExcept
//
procedure TdwsCompiler.ReadExcept(exceptExpr: TExceptExpr;
  var finalToken: TTokenType);
var
  DoExpr: TExceptDoExpr;
  varName: String;
  ClassSym: TTypeSymbol;
  onPos: TScriptPos;
begin
  if FTok.Test(ttON) then
  begin
    while FTok.TestDelete(ttON) do
    begin
      onPos := FTok.CurrentPos;
      if not FTok.TestName then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);
      varName := FTok.GetToken.AsString;
      FTok.KillToken;

      if not FTok.TestDelete(ttCOLON) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_ColonExpected);

      ClassSym := ReadType('', tcExceptionClass);
      if not ClassSym.baseType.IsClassSymbol then
        FMsgs.AddCompilerError(FTok.hotPos, CPE_ClassRefExpected);

      if not FTok.TestDelete(ttDO) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_DoExpected);

      DoExpr := TExceptDoExpr.Create(FCompilerContext, FTok.hotPos);
      exceptExpr.AddDoExpr(DoExpr);

      DoExpr.ExceptionTable.AddSymbol(TScriptDataSymbol.Create(varName,
        ClassSym));
      CurrentProg.EnterSubTable(DoExpr.ExceptionTable);
      if coContextMap in FOptions then
      begin
        FSourceContextMap.OpenContext(onPos, nil, ttON);
        FSourceContextMap.Current.LocalTable := DoExpr.ExceptionTable;
      end;
      try
        DoExpr.DoBlockExpr := ReadBlock;
      finally
        if coContextMap in FOptions then
          FSourceContextMap.CloseContext(FTok.CurrentPos);
        CurrentProg.LeaveSubTable;
      end;

      if FTok.TestAny([ttEND, ttELSE]) = ttNone then
        ReadSemiColon;
    end;

    if FTok.TestDelete(ttELSE) then
      exceptExpr.elseExpr := ReadBlocks([ttEND, ttFINALLY], finalToken)
    else
    begin
      finalToken := FTok.TestDeleteAny([ttEND, ttFINALLY]);
      if finalToken = ttNone then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_EndExpected);
    end;
  end
  else
  begin
    exceptExpr.HandlerExpr := ReadBlocks([ttEND, ttFINALLY], finalToken);
  end;
end;

// ReadGenericParametersDecl
//
function TdwsCompiler.ReadGenericParametersDecl: IGenericParameters;
var
  name: String;
  hotPos: TScriptPos;
  tt: TTokenType;
  param: TGenericTypeParameterSymbol;
  constraintType: TTypeSymbol;
begin
  result := TGenericParameters.Create;
  repeat
    if not FTok.TestDeleteNamePos(name, hotPos) then
    begin
      FMsgs.AddCompilerError(FTok.hotPos, CPE_NameExpected);
      Break;
    end;
    if result.Find(name) <> nil then
      FMsgs.AddCompilerErrorFmt(hotPos, CPE_NameAlreadyExists, [name]);
    param := TGenericTypeParameterSymbol.Create(nil, name);
    result.Add(param);
    RecordSymbolUse(param, hotPos, [suDeclaration]);
    if FTok.TestDelete(ttCOLON) then
    begin
      repeat
        hotPos := FTok.hotPos;
        tt := FTok.TestDeleteAny([ttRECORD]);
        case tt of
          ttRECORD:
            begin
              param.AddConstraint(TGenericConstraintRecord.Create(param));
            end;
        else
          constraintType := ReadType('', tcParameter);
          if constraintType <> nil then
          begin
            param.AddConstraint(TGenericConstraintType.Create(param,
              constraintType));
          end
          else
            FMsgs.AddCompilerError(hotPos, CPE_UnsupportedGenericConstraint);
        end;
      until not FTok.TestDelete(ttCOMMA);
    end;
  until FTok.TestDeleteAny([ttSEMI, ttCOMMA]) = ttNone;
  if result.Count = 0 then
    result := nil;
end;

// ReadSpecializedType
//
function TdwsCompiler.ReadSpecializedType(genericType: TGenericSymbol)
  : TTypeSymbol;
var
  value: TTypeSymbol;
  valueList: TUnSortedSymbolTable;
  valuePosList: TScriptPosArray;
  StartPos, hotPos: TScriptPos;
  checkSuccessful: Boolean;
  n: Integer;
begin
  StartPos := FTok.hotPos;
  if not FTok.TestDelete(ttLESS) then
    FMsgs.AddCompilerStop(StartPos, CPE_GenericParametersListExpected);
  valueList := TUnSortedSymbolTable.Create;
  try
    checkSuccessful := True;
    repeat
      hotPos := FTok.hotPos;
      value := ReadType('', tcGeneric);
      if value <> nil then
      begin
        valueList.AddSymbol(value);
        n := Length(valuePosList);
        SetLength(valuePosList, n + 1);
        valuePosList[n] := hotPos;
      end;
    until not FTok.TestDelete(ttCOMMA);
    if not FTok.TestDelete(ttGTR) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_GreaterExpected);
    if valueList.Count <> genericType.Parameters.Count then
    begin
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_BadNumberOfParameters,
        [genericType.Parameters.Count, valueList.Count]);
      checkSuccessful := False;
    end;

    if checkSuccessful then
    begin
      result := genericType.SpecializationFor(StartPos, CurrentUnitSymbol,
        valueList, valuePosList, FCompilerContext, FOperators);
    end
    else
    begin
      result := genericType;
    end;
  finally
    valueList.Clear;
    valueList.Free;
  end;
end;

// CheckGenericParameters
//
procedure TdwsCompiler.CheckGenericParameters(genericType: TGenericSymbol);
var
  i: Integer;
  name: String;
  namePos: TScriptPos;
  p: TGenericTypeParameterSymbol;
begin
  if not FTok.TestDelete(ttLESS) then
    Msgs.AddCompilerError(FTok.hotPos, CPE_LesserExpected);
  for i := 0 to genericType.Parameters.Count - 1 do
  begin
    if not FTok.TestDeleteNamePos(name, namePos) then
    begin
      Msgs.AddCompilerError(FTok.hotPos, CPE_NameExpected);
      Break;
    end;
    p := genericType.Parameters[i];
    if not SameText(p.name, name) then
    begin
      Msgs.AddCompilerErrorFmt(namePos, CPE_X_ExpectedBut_Y_Found,
        [p.name, name]);
    end
    else if p.name <> name then
    begin
      FMsgs.AddCompilerHintFmt(namePos, CPH_CaseDoesNotMatchDeclaration,
        [name, p.name], hlPedantic);
    end;
  end;
  if not FTok.TestDelete(ttGTR) then
    Msgs.AddCompilerError(FTok.hotPos, CPE_GreaterExpected);
end;

// ReadRaise
//
function TdwsCompiler.ReadRaise: TRaiseBaseExpr;
var
  exceptExpr: TTypedExpr;
  exceptObjTyp: TSymbol;
begin
  if FIsExcept and (FTok.TestAny([ttSEMI, ttEND, ttELSE, ttUNTIL, ttFINALLY,
    ttEXCEPT]) <> ttNone) then
    result := TReraiseExpr.Create(FTok.hotPos)
  else
  begin
    exceptExpr := ReadExpr;
    exceptObjTyp := exceptExpr.typ;
    if not(exceptObjTyp.IsClassSymbol and TClassSymbol(exceptObjTyp)
      .IsOfType(FCompilerContext.TypException)) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_ExceptionObjectExpected);
    result := TRaiseExpr.Create(FCompilerContext, FTok.hotPos, exceptExpr);
  end;
end;

// ReadExit
//
function TdwsCompiler.ReadExit: TNoResultExpr;
var
  gotParenthesis: Boolean;
  leftExpr: TDataExpr;
  assignExpr: TProgramExpr;
  proc: TdwsProcedure;
  exitPos: TScriptPos;
begin
  exitPos := FTok.hotPos;
  if FTok.TestAny([ttEND, ttSEMI, ttELSE, ttUNTIL]) <> ttNone then
    result := TExitExpr.Create(FTok.hotPos)
  else
  begin
    if not(CurrentProg is TdwsProcedure) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NoResultRequired);
    gotParenthesis := FTok.TestDelete(ttBLEFT);
    proc := TdwsProcedure(CurrentProg);
    if proc.func.result = nil then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NoResultRequired);
    RecordSymbolUse(proc.func.result, exitPos,
      [suReference, suWrite, suImplicit]);
    leftExpr := TVarExpr.CreateTyped(FCompilerContext, exitPos,
      proc.func.result);
    try
      assignExpr := ReadAssign(ttASSIGN, leftExpr);
      try
        leftExpr := nil;
        if gotParenthesis and not FTok.TestDelete(ttBRIGHT) then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);
        if assignExpr is TNullExpr then
        begin
          // in case of previous compile error, attempt to keep compiling
          OrphanAndNil(assignExpr);
          result := TExitExpr.Create(exitPos);
        end
        else
          result := TExitValueExpr.Create(FCompilerContext, exitPos,
            assignExpr as TAssignExpr);
      except
        OrphanObject(assignExpr);
        raise;
      end;
    except
      OrphanAndNil(leftExpr);
      raise;
    end;
  end;
end;

// ReadClassExpr
//
function TdwsCompiler.ReadClassExpr(ownerSymbol: TCompositeTypeSymbol;
  expecting: TTypeSymbol = nil): TTypedExpr;
var
  exprTable: TExpressionSymbolTable;
  oldStructure: TCompositeTypeSymbol;
begin
  exprTable := TExpressionSymbolTable.Create(ownerSymbol.Members,
    ownerSymbol.Members.AddrGenerator);
  oldStructure := FCurrentStructure;
  try
    FCurrentStructure := ownerSymbol;
    exprTable.AddParent(CurrentProg.table);
    CurrentProg.EnterSubTable(exprTable);
    try
      result := ReadExpr(expecting);
    finally
      CurrentProg.LeaveSubTable;
    end;
    // anonymous and implicit symbols might have been created,
    // transfer them to the regular table
    if exprTable.Count > 0 then
      exprTable.TransferSymbolsTo(CurrentProg.table);
  finally
    FCurrentStructure := oldStructure;
    OrphanObject(exprTable);
  end;
end;

// ReadType
//
function TdwsCompiler.ReadType(const typeName: String;
  typeContext: TdwsReadTypeContext): TTypeSymbol;

  function ReadClassFlags(token: TTokenType): TTypeSymbol;
  var
    flags: TClassSymbolFlags;
  begin
    case token of
      ttPARTIAL:
        flags := [csfPartial];
      ttSTATIC:
        flags := [csfStatic];
    else
      flags := [];
    end;
    if FTok.TestDelete(ttCLASS) and (typeContext = tcDeclaration) then
      result := ReadClassDecl(typeName, flags, False)
    else
    begin
      result := nil;
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ClassExpected);
    end;
  end;

  function ReadProcType(initialToken: TTokenType; const hotPos: TScriptPos)
    : TTypeSymbol;
  var
    token: TTokenType;
    declOptions: TdwsReadProcDeclOptions;
  begin
    declOptions := [pdoType];
    if initialToken = ttREFERENCE then
    begin
      Include(declOptions, pdoReferenceTo);
      if FTok.TestDelete(ttTO) then
      begin
        if not(coDelphiDialect in FCompilerContext.options) then
          FMsgs.AddCompilerHint(FTok.hotPos, CPH_ReferenceToIsLegacy,
            hlPedantic);
      end
      else
        FMsgs.AddCompilerError(FTok.hotPos, CPE_ToExpected);
      token := FTok.TestDeleteAny([ttPROCEDURE, ttFUNCTION]);
      if token = ttNone then
      begin
        FMsgs.AddCompilerError(FTok.hotPos, CPE_ProcOrFuncExpected);
        token := ttFUNCTION; // keep compiling
      end;
    end
    else
    begin
      Assert(initialToken in [ttPROCEDURE, ttFUNCTION]);
      token := initialToken;
    end;
    result := ReadProcDecl(token, hotPos, declOptions);
    result.SetName(typeName);
    (result as TFuncSymbol).SetIsType;
    // close declaration context
    if coContextMap in options then
      FSourceContextMap.CloseContext(FTok.hotPos);
  end;

var
  tt: TTokenType;
  name, connectorQualifier: String;
  hotPos, namePos: TScriptPos;
  sym: TSymbol;
begin
  hotPos := FTok.hotPos;
  tt := FTok.TestDeleteAny([ttARRAY, ttSET, ttRECORD, ttCLASS, ttINTERFACE,
    ttHELPER, ttBLEFT, ttENUM, ttFLAGS, ttPARTIAL, ttSTATIC, ttSTRICT,
    ttPROCEDURE, ttFUNCTION, ttREFERENCE]);

  case tt of
    ttARRAY:
      begin
        result := ReadArrayType(typeName, typeContext);
        if typeName = '' then
          RecordSymbolUse(result, hotPos, [suReference, suImplicit]);
      end;

    ttSET:
      begin
        result := ReadSetOfType(typeName, typeContext);
        if typeName = '' then
          RecordSymbolUse(result, hotPos, [suReference, suImplicit]);
      end;

    ttRECORD:
      if FTok.TestDelete(ttHELPER) then
        result := ReadHelperDecl(typeName, ttRECORD, False)
      else
      begin
        result := ReadRecordDecl(typeName, False);
        if typeName = '' then
        begin
          CurrentProg.table.AddSymbol(result);
          result.IncRefCount;
        end;
        if typeName = '' then
          RecordSymbolUse(result, hotPos, [suReference, suImplicit]);
      end;

    ttCLASS:
      begin
        tt := FTok.TestDeleteAny([ttOF, ttHELPER]);
        case tt of
          ttOF:
            result := ReadClassOf(typeName);
          ttHELPER:
            result := ReadHelperDecl(typeName, ttCLASS, False);
        else
          if typeContext = tcDeclaration then
            result := ReadClassDecl(typeName, [], False)
          else
          begin
            result := nil;
            FMsgs.AddCompilerStop(FTok.hotPos, CPE_TypeExpected);
          end;
        end;
        if typeName = '' then
          RecordSymbolUse(result, hotPos, [suReference, suImplicit]);
      end;

    ttPARTIAL, ttSTATIC:
      begin
        result := ReadClassFlags(tt);
      end;

    ttINTERFACE:
      begin
        hotPos := FTok.hotPos;
        if FTok.TestDelete(ttHELPER) then
          result := ReadHelperDecl(typeName, ttINTERFACE, False)
        else
        begin
          if typeContext = tcDeclaration then
            result := ReadInterface(typeName)
          else
          begin
            result := nil;
            FMsgs.AddCompilerStop(hotPos, CPE_TypeExpected);
          end;
        end;
      end;

    ttHELPER:
      begin
        result := ReadHelperDecl(typeName, ttNone, False);
      end;

    ttSTRICT:
      begin
        if not FTok.TestDelete(ttHELPER) then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_HelperExpected);
        result := ReadHelperDecl(typeName, ttNone, True);
      end;

    ttENUM, ttFLAGS:
      begin
        // explicitly scoped enum
        if not FTok.TestDelete(ttBLEFT) then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackLeftExpected);
        if tt = ttENUM then
          result := ReadEnumeration(typeName, enumScoped)
        else
          result := ReadEnumeration(typeName, enumFlags);
      end;

    ttBLEFT:
      begin
        // class, globally scoped enum
        result := ReadEnumeration(typeName, enumClassic);
      end;

    ttREFERENCE:
      begin
        result := ReadProcType(tt, hotPos)
      end;

    ttPROCEDURE, ttFUNCTION:
      begin
        result := ReadProcType(tt, hotPos);
      end;

  else

    if FTok.TestName then
    begin

      sym := ReadAliasedNameSymbol(namePos);

      if not Assigned(sym) then
      begin
        // keep compiling
        result := FCompilerContext.TypVariant;
      end
      else if not sym.IsType then
      begin
        FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_InvalidType, [sym.name]);
        result := FCompilerContext.TypVariant; // keep compiling
      end
      else if sym.ClassType = TGenericSymbol then
      begin
        result := ReadSpecializedType(TGenericSymbol(sym));
      end
      else if sym is TConnectorSymbol then
      begin
        connectorQualifier := '';
        if FTok.TestDelete(ttLESS) then
        begin
          repeat
            if not FTok.TestDeleteNamePos(name, namePos) then
              FMsgs.AddCompilerStop(namePos, CPE_NameExpected);
            connectorQualifier := connectorQualifier + name;
            if FTok.TestDelete(ttGTR) then
              Break;
            if not FTok.TestDelete(ttDOT) then
              FMsgs.AddCompilerStop(namePos, CPE_DotExpected);
            connectorQualifier := connectorQualifier + '.';
          until False;
        end;
        if connectorQualifier = '' then
          result := TTypeSymbol(sym)
        else
        begin
          result := TConnectorSymbol(sym).SpecializeConnector(CurrentProg.table,
            connectorQualifier);
          if result = sym then
            FMsgs.AddCompilerErrorFmt(FTok.hotPos,
              CPE_ConnectorCantBeSpecialized, [sym.name])
          else if result = nil then
          begin
            FMsgs.AddCompilerErrorFmt(FTok.hotPos,
              CPE_ConnectorInvalidSpecifier, [sym.name, connectorQualifier]);
            result := TTypeSymbol(sym);
          end;
        end;
      end
      else
        result := TTypeSymbol(sym);

      WarnDeprecatedType(hotPos, result);

      // Create name symbol, e. g.: type a = integer;
      if typeName <> '' then
        result := TAliasSymbol.Create(typeName, result);

      RecordSymbolUse(result, namePos, [suReference]);

    end
    else
    begin

      FMsgs.AddCompilerError(FTok.hotPos, CPE_TypeExpected);
      // keep compiling
      result := TAliasSymbol.Create(typeName, FCompilerContext.TypVariant);

    end;

  end;

  // Ensure that unnamed symbols will be freed
  if (result <> nil) and (result.name = '') then
    CurrentProg.RootTable.AddToDestructionList(result);
end;

// ReadTypeGenericDecl
//
function TdwsCompiler.ReadTypeGenericDecl(const typeName: String;
  typeContext: TdwsReadTypeContext;
  const genericParameters: IGenericParameters = nil): TTypeSymbol;
var
  genericPos: TScriptPos;
  genericSymbol: TGenericSymbol;
  specializeMethod: TSpecializationMethod;
  genericExists: Boolean;
begin
  result := nil;
  genericPos := FTok.hotPos;
  genericExists := (CurrentProg.table.FindLocal(typeName) <> nil);
  Assert(not genericExists);

  genericSymbol := TGenericSymbol.Create(typeName, genericParameters);
  EnterGeneric(genericSymbol);
  try
    result := ReadType(genericSymbol.Caption, typeContext);
  finally
    LeaveGeneric;
    if result <> nil then
    begin
      specializeMethod := result.SpecializeType;
      if TMethod(specializeMethod).code = @TTypeSymbol.SpecializeType then
        FMsgs.AddCompilerErrorFmt(genericPos, CPE_GenericityNotSupportedYet,
          [result.ClassName]);
    end;
    genericSymbol.genericType := result;
    result := genericSymbol;
  end;
end;

// EnterGeneric
//
procedure TdwsCompiler.EnterGeneric(generic: TGenericSymbol);
begin
  CurrentProg.table.AddSymbol(generic); // auto-forward
  CurrentProg.table.InsertParent(0, generic.Parameters.List);
  FGenericSymbol.Push(generic);
end;

// LeaveGeneric
//
procedure TdwsCompiler.LeaveGeneric;
var
  sym: TGenericSymbol;
begin
  sym := FGenericSymbol.Peek;
  FGenericSymbol.Pop;
  CurrentProg.table.RemoveParent(sym.Parameters.List);
  CurrentProg.table.Remove(sym); // auto-forward
end;

// ReadExpr
//
function TdwsCompiler.ReadExpr(expecting: TTypeSymbol = nil): TTypedExpr;
var
  right: TTypedExpr;
  rightTyp: TTypeSymbol;
  resultTyp: TTypeSymbol;
  tt: TTokenType;
  hotPos: TScriptPos;
  opExpr: TTypedExpr;
begin
  // Read left argument
  hotPos := FTok.hotPos;
  result := ReadExprAdd(expecting);
  try
    // Read operator
    repeat
      tt := FTok.TestDeleteAny([ttEQ, ttNOT_EQ, ttEQ_EQ, ttEXCL_EQ, ttEQ_EQ_EQ,
        ttLESS, ttLESS_EQ, ttGTR, ttGTR_EQ, ttIN, ttIS, ttIMPLEMENTS, ttIMPLIES,
        ttPLUS_PLUS, ttMINUS_MINUS]);
      case tt of
        ttNone:
          Break;
        ttIN:
          result := ReadExprIn(result);
      else

        hotPos := FTok.hotPos;
        if tt in [ttPLUS_PLUS, ttMINUS_MINUS] then
        begin
          FMsgs.AddCompilerWarningFmt(hotPos, CPW_AmbiguousOperator,
            [cTokenStrings[tt]]);
          tt := ttPLUS;
        end;

        // Read right argument
        if tt = ttIS then
          right := ReadExprAdd(FCompilerContext.TypAnyType)
        else
          right := ReadExprAdd;
        if right = nil then
          rightTyp := nil
        else
          rightTyp := right.typ;
        try
          case tt of
            ttIS:
              begin

                opExpr := CreateTypedOperatorExpr(tt, hotPos, result, right);
                if opExpr <> nil then
                begin
                  result := opExpr
                end
                else
                begin
                  if not result.typ.IsClassSymbol then
                    FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                  else if not(rightTyp is TClassOfSymbol) then
                    FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected)
                  else if not(TClassSymbol(rightTyp.typ).IsOfType(result.typ) or
                    TClassSymbol(result.typ).IsOfType(rightTyp.typ)) then
                    IncompatibleTypesWarn(hotPos, CPE_IncompatibleTypes,
                      result.typ, rightTyp.typ);
                  result := TIsOpExpr.Create(FCompilerContext, hotPos, tt,
                    result, right)
                end;

              end;
            ttIMPLEMENTS:
              begin

                if not(rightTyp is TInterfaceSymbol) then
                  FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
                if result.typ is TClassOfSymbol then
                  result := TClassImplementsIntfOpExpr.Create(FCompilerContext,
                    hotPos, tt, result, right)
                else
                begin
                  if not result.typ.IsClassSymbol then
                    FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected);
                  result := TImplementsIntfOpExpr.Create(FCompilerContext,
                    hotPos, tt, result, right);
                end;

              end;
          else
            opExpr := CreateTypedOperatorExpr(tt, hotPos, result, right);
            if opExpr = nil then
            begin
              resultTyp := result.typ.UnAliasedType;
              if (tt in [ttEQ, ttNOT_EQ, ttEQ_EQ, ttEXCL_EQ]) and
                (rightTyp <> nil) and
                (resultTyp.IsClassSymbol or (resultTyp is TInterfaceSymbol) or
                (resultTyp is TClassOfSymbol) or
                (resultTyp = FCompilerContext.TypNil)) then
              begin
                if not((rightTyp.ClassType = result.typ.ClassType) or
                  (rightTyp = FCompilerContext.TypNil)) then
                begin
                  if resultTyp.IsClassSymbol then
                    FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                  else if resultTyp is TClassOfSymbol then
                    FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected)
                  else
                    FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
                end;
                if resultTyp.IsClassSymbol then
                  if tt in [ttNOT_EQ, ttEXCL_EQ] then
                    result := TObjCmpNotEqualExpr.Create(FCompilerContext,
                      hotPos, tt, result, right)
                  else
                    result := TObjCmpEqualExpr.Create(FCompilerContext, hotPos,
                      tt, result, right)
                else if resultTyp is TClassOfSymbol then
                begin
                  result := TAssignedMetaClassExpr.Create(FCompilerContext,
                    hotPos, result);
                  if tt = ttEQ then
                    result := TNotBoolExpr.Create(FCompilerContext,
                      hotPos, result);
                  OrphanAndNil(right);
                end
                else
                begin
                  result := TIntfCmpExpr.Create(FCompilerContext, hotPos, tt,
                    result, right);
                  if tt in [ttNOT_EQ, ttEXCL_EQ] then
                    result := TNotBoolExpr.Create(FCompilerContext,
                      hotPos, result);
                end;
              end
              else if (tt in [ttEQ, ttNOT_EQ, ttEQ_EQ, ttEXCL_EQ]) and
                (rightTyp = FCompilerContext.TypNil) and
                (resultTyp.IsOfType(FCompilerContext.TypVariant)) then
              begin
                if tt in [ttEQ, ttEQ_EQ] then
                  result := TRelVarEqualNilExpr.Create(FCompilerContext,
                    hotPos, result)
                else
                  result := TRelVarNotEqualNilExpr.Create(FCompilerContext,
                    hotPos, result);
                OrphanAndNil(right);
              end
              else if (result <> nil) and (right <> nil) and
                (result.IsGeneric or right.IsGeneric) then
              begin
                result := TGenericBinaryOpExpr.Create(FCompilerContext, hotPos,
                  tt, result, right);
              end
              else
              begin
                FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                result := TRelOpExpr.Create(FCompilerContext, hotPos, tt,
                  result, right); // keep going
              end;
            end
            else
              result := opExpr;
          end;
        except
          OrphanAndNil(right);
          raise;
        end;
      end;
    until False;
  except
    OrphanAndNil(result);
    raise;
  end;
end;

// ReadExprAdd
//
function TdwsCompiler.ReadExprAdd(expecting: TTypeSymbol = nil;
  leftExpr: TTypedExpr = nil): TTypedExpr;
var
  right: TTypedExpr;
  tt: TTokenType;
  hotPos: TScriptPos;
  opExpr: TTypedExpr;
begin
  // Read left argument
  if leftExpr = nil then
    result := ReadExprMult(expecting)
  else
    result := leftExpr;
  try

    repeat
      tt := FTok.TestDeleteAny([ttPLUS, ttMINUS, ttOR, ttXOR, ttNOT]);
      if tt = ttNone then
        Break;

      hotPos := FTok.hotPos;

      if tt = ttNOT then
      begin

        if not FTok.TestDelete(ttIN) then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_InExpected);
        result := ReadExprIn(result);
        result := TNotBoolExpr.Create(FCompilerContext, FTok.hotPos, result);

      end
      else
      begin

        // Read right argument
        right := ReadExprMult;
        try
          // Generate function and add left and right argument
          if right = nil then
          begin
            opExpr := nil;
          end
          else if (result.typ = nil) or (right.typ = nil) then
          begin
            FMsgs.AddCompilerError(hotPos, CPE_IncompatibleOperands);
            opExpr := nil;
          end
          else
          begin
            opExpr := CreateTypedOperatorExpr(tt, hotPos, result, right);
            if opExpr = nil then
            begin
              if (tt = ttPLUS) and result.typ.UnAliasedTypeIs(TArraySymbol) and
                right.typ.UnAliasedTypeIs(TArraySymbol) then
              begin
                opExpr := CompilerUtils.ArrayConcat(FCompilerContext, hotPos,
                  result, right);
              end
              else if FGenericSymbol.Count > 0 then
              begin
                opExpr := TGenericBinaryOpExpr.Create(FCompilerContext, hotPos,
                  tt, result, right);
              end
              else
              begin
                FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
              end;
            end;
          end;
          if opExpr = nil then
          begin
            // fake result to keep compiler going and report further issues
            result := TBinaryOpExpr.Create(FCompilerContext, hotPos, tt,
              result, right);
            result.typ := FCompilerContext.TypVariant;
          end
          else
            result := opExpr;
        except
          OrphanAndNil(right);
          raise;
        end;
      end;

      if Optimize then
        result := result.OptimizeToTypedExpr(FCompilerContext, hotPos);
    until False;
  except
    OrphanAndNil(result);
    raise;
  end;
end;

// ReadExprMult
//
function TdwsCompiler.ReadExprMult(expecting: TTypeSymbol = nil;
  leftExpr: TTypedExpr = nil): TTypedExpr;
var
  right: TTypedExpr;
  tt: TTokenType;
  hotPos: TScriptPos;
  opExpr: TTypedExpr;
  rightTyp: TTypeSymbol;
begin
  // Read left argument
  if leftExpr = nil then
    result := ReadTerm(False, expecting)
  else
    result := leftExpr;
  try
    repeat
      tt := FTok.TestDeleteAny([ttTIMES, ttDIVIDE, ttMOD, ttDIV, ttAND, ttCARET,
        ttAS, ttLESS_LESS, ttGTR_GTR, ttQUESTION_QUESTION, ttSHL,
        ttSHR, ttSAR]);
      if tt = ttNone then
        Break;

      // Save position of the operator
      hotPos := FTok.hotPos;

      // Read right argument
      if tt = ttAS then
        right := ReadTerm(False, FCompilerContext.TypAnyType)
      else
        right := ReadTerm;
      try
        if (result.typ = nil) or (right = nil) or (right.typ = nil) then
          FMsgs.AddCompilerStop(hotPos, CPE_IncompatibleOperands);
        case tt of
          ttAS:
            begin
              rightTyp := right.typ;
              var
              asCastExpr := FOperators.CreateAsCastExpr(FCompilerContext,
                result, right.typ, hotPos);
              if asCastExpr <> nil then
              begin
                result := asCastExpr
              end
              else
              begin
                FMsgs.AddCompilerErrorFmt(hotPos, CPE_CannotCastAs,
                  [result.typ.Caption, rightTyp.Caption]);
                result := TConvInvalidExpr.Create(FCompilerContext, hotPos,
                  result, rightTyp);
              end;
              OrphanAndNil(right);
            end;
          ttQUESTION_QUESTION:
            begin
              FCompilerContext.WrapWithImplicitCast(result.typ, hotPos, right);
              rightTyp := right.typ;
              if not result.typ.IsCompatible(rightTyp) then
              begin
                if result.typ.UnAliasedTypeIs(TClassSymbol) and
                  right.typ.UnAliasedTypeIs(TClassSymbol) then
                begin
                  result := TCoalesceClassExpr.Create(FCompilerContext, hotPos,
                    tt, result, right);
                  result.typ := (result.typ.UnAliasedType as TClassSymbol)
                    .CommonAncestor(rightTyp.UnAliasedType);
                  if result.typ = nil then
                  begin
                    FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                    // keep compiling
                    result.typ := TCoalesceClassExpr(result).left.typ;
                  end;
                end
                else
                begin
                  IncompatibleTypes(hotPos, CPE_IncompatibleTypes, result.typ,
                    rightTyp);
                  // fake result to keep compiler going and report further issues
                  result := TBinaryOpExpr.Create(FCompilerContext, hotPos, tt,
                    result, right);
                  result.typ := TBinaryOpExpr(result).left.typ;
                end;
              end
              else if result.typ.IsOfType(FCompilerContext.TypVariant) then
              begin
                result := TCoalesceExpr.Create(FCompilerContext, hotPos, tt,
                  result, right);
                if TCoalesceExpr(result)
                  .right.IsOfType(TCoalesceExpr(result).left.typ) then
                  result.typ := TCoalesceExpr(result).left.typ
                else
                  result.typ := FCompilerContext.TypVariant;
              end
              else if result.typ.IsOfType(FCompilerContext.TypString) then
              begin
                result := TCoalesceStrExpr.Create(FCompilerContext, hotPos, tt,
                  result, right);
              end
              else if result.typ.UnAliasedType.ClassType = TClassSymbol then
              begin
                result := TCoalesceClassExpr.Create(FCompilerContext, hotPos,
                  tt, result, right);
              end
              else if result.typ.UnAliasedType.ClassType = TDynamicArraySymbol
              then
              begin
                result := TCoalesceDynArrayExpr.Create(FCompilerContext, hotPos,
                  tt, result, right);
              end
              else if result.typ.IsOfType(FCompilerContext.TypInteger) then
              begin
                result := TCoalesceIntExpr.Create(FCompilerContext, hotPos, tt,
                  result, right);
              end
              else if result.typ.IsOfType(FCompilerContext.TypFloat) then
              begin
                result := TCoalesceFloatExpr.Create(FCompilerContext, hotPos,
                  tt, result, right);
              end
              else if result.typ.IsOfType(FCompilerContext.TypBoolean) then
              begin
                result := TCoalesceBooleanExpr.Create(FCompilerContext, hotPos,
                  tt, result, right);
              end
              else
              begin
                FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                // fake result to keep compiler going and report further issues
                result := TBinaryOpExpr.Create(FCompilerContext, hotPos, tt,
                  result, right);
                result.typ := TBinaryOpExpr(result).left.typ;
              end;
            end;
        else
          opExpr := CreateTypedOperatorExpr(tt, hotPos, result, right);
          if opExpr = nil then
          begin
            FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
            // fake result to keep compiler going and report further issues
            result := TBinaryOpExpr.Create(FCompilerContext, hotPos, tt,
              result, right);
            result.typ := right.typ;
          end
          else
            result := opExpr;
        end;

      except
        OrphanAndNil(right);
        raise;
      end;

      if Optimize then
        result := result.OptimizeToTypedExpr(FCompilerContext, hotPos);
    until False;
  except
    OrphanAndNil(result);
    raise;
  end;
end;

// ReadBooleanExpr
//
function TdwsCompiler.ReadBooleanExpr: TTypedExpr;
var
  hotPos: TScriptPos;
begin
  hotPos := FTok.hotPos;
  result := ReadExpr;
  if result <> nil then
  begin
    if not(result.IsOfType(FCompilerContext.TypBoolean) or
      result.IsOfType(FCompilerContext.TypVariant)) then
    begin
      if not result.IsGeneric then
      begin
        if result.scriptPos.Defined then
          hotPos := result.scriptPos;
        FMsgs.AddCompilerError(hotPos, CPE_BooleanExpected)
      end;
    end;
  end;
end;

// ReadExprIn
//
function TdwsCompiler.ReadExprIn(var left: TTypedExpr): TTypedExpr;
var
  hotPos: TScriptPos;
  setExpr: TTypedExpr;
  elementType: TTypeSymbol;
  classOpSymbol: TClassOperatorSymbol;
  classOpExpr: TFuncExprBase;
  argPosArray: TScriptPosArray;
begin
  hotPos := FTok.hotPos;

  if FTok.TestDelete(ttALEFT) then
  begin

    result := ReadExprInConditions(left);

  end
  else
  begin

    setExpr := ReadExpr;
    try

      if setExpr = nil then
      begin
        // keep compiling
        result := TConvVarToBoolExpr.Create(FCompilerContext, hotPos, left);
        left := nil;
        Exit;
      end;

      if setExpr.typ is TArraySymbol then
      begin

        elementType := TArraySymbol(setExpr.typ).typ;
        if (left.typ = nil) or not left.typ.IsOfType(elementType) then
        begin
          // attempt cast & typecheck harder
          if (left is TFuncExpr) and (TFuncExpr(left).args.Count = 0) then
          begin
            if left is TFuncPtrExpr then
              left := TFuncPtrExpr(left).Extract
            else
              left := TFuncRefExpr.Create(FCompilerContext, TFuncExpr(left));
          end
          else
          begin
            left := TConvExpr.WrapWithConvCast(FCompilerContext, hotPos,
              elementType, left, CPE_IncompatibleTypes);
          end;
        end;

        result := CompilerUtils.ArrayContains(FCompilerContext, hotPos,
          setExpr, left);

      end
      else if setExpr.typ is TSetOfSymbol then
      begin

        elementType := TDynamicArraySymbol(setExpr.typ).typ;
        if (left.typ = nil) or not left.typ.IsOfType(elementType) then
          IncompatibleTypes(hotPos, CPE_IncompatibleTypes, left.typ,
            elementType);
        result := TSetOfInExpr.CreateOptimal(FCompilerContext, hotPos, left,
          setExpr as TDataExpr);

      end
      else if setExpr.typ is TAssociativeArraySymbol then
      begin

        elementType := TAssociativeArraySymbol(setExpr.typ).keyType;
        if (left.typ = nil) or not left.typ.IsOfType(elementType) then
        begin
          // attempt cast & typecheck harder
          if (left is TFuncExpr) and (TFuncExpr(left).args.Count = 0) then
          begin
            if left is TFuncPtrExpr then
              left := TFuncPtrExpr(left).Extract
            else
              left := TFuncRefExpr.Create(FCompilerContext, TFuncExpr(left));
          end
          else
          begin
            left := TConvExpr.WrapWithConvCast(FCompilerContext, hotPos,
              elementType, left, CPE_IncompatibleTypes);
          end;
        end;

        result := TAssociativeArrayContainsKeyExpr.Create(FCompilerContext,
          hotPos, ttIN, left, setExpr);

      end
      else if not(setExpr is TDataExpr) then
      begin

        FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected);
        // keep compiling
        OrphanAndNil(left);
        OrphanAndNil(setExpr);
        result := FUnifiedConstants.CreateBoolean(False);

      end
      else
      begin

        if setExpr.typ.IsClassSymbol then
        begin
          classOpSymbol := (setExpr.typ as TClassSymbol).FindClassOperator(ttIN,
            left.typ);
          if classOpSymbol <> nil then
          begin
            classOpExpr := GetMethodExpr(classOpSymbol.usesSym,
              (setExpr as TDataExpr), rkObjRef, hotPos, []);
            try
              setExpr := nil;
              classOpExpr.AddArg(left);
              left := nil;
              TypeCheckArguments(FCompilerContext, classOpExpr, argPosArray);
            except
              classOpExpr.Orphan(FCompilerContext);
              raise;
            end;
            result := classOpExpr;
            Exit;
          end;
        end;

        result := CreateTypedOperatorExpr(ttIN, hotPos, left, setExpr);
        if result = nil then
        begin
          FMsgs.AddCompilerError(hotPos, CPE_IncompatibleOperands);
          // fake result to keep compiler going and report further issues
          result := TBinaryOpExpr.Create(FCompilerContext, hotPos, ttIN,
            left, setExpr);
          result.typ := FCompilerContext.TypVariant;
        end;
        left := nil;

      end;

    except
      OrphanAndNil(setExpr);
      raise;
    end;

  end;
  if Optimize then
    result := result.OptimizeToTypedExpr(FCompilerContext, hotPos);
end;

// ReadExprInConditions
//
function TdwsCompiler.ReadExprInConditions(var left: TTypedExpr): TInOpExpr;
var
  condList: TTightList;
  hotPos: TScriptPos;
  cond: TRefCountedObject;
begin
  hotPos := FTok.hotPos;

  condList.Initialize;
  try
    if not FTok.TestDelete(ttARIGHT) then
    begin
      ReadCaseConditions(condList, left);
      if not FTok.TestDelete(ttARIGHT) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_ArrayBracketRightExpected);
    end;

    if left.IsOfType(FCompilerContext.TypString) then
    begin

      if TCaseConditionsHelper.CanOptimizeToTyped(condList, TConstStringExpr)
        and condList.ItemsAllOfClass(TCompareConstStringCaseCondition) then
      begin

        result := TStringInOpStaticSetExpr.Create(FCompilerContext, left)

      end
      else
      begin

        if left is TStringArrayOpExpr then
          result := TCharacterInOpExpr.AttemptCreate(FCompilerContext,
            left, condList)
        else
          result := nil;
        if result = nil then
          result := TStringInOpExpr.Create(FCompilerContext, left);

      end;
    end
    else
    begin

      result := TInOpExpr.Create(FCompilerContext, left);

    end;

    left := nil;

    // Add case conditions to TCaseExpr
    for cond in condList do
      result.AddCaseCondition(cond as TCaseCondition);

    result.Prepare;

    condList.Clear;
  finally
    condList.Clean;
  end;
end;

// ReadTerm
//
function TdwsCompiler.ReadTerm(isWrite: Boolean = False;
  expecting: TTypeSymbol = nil): TTypedExpr;

  function ReadNotTerm: TUnaryOpExpr;
  var
    operand: TTypedExpr;
    hotPos: TScriptPos;
  begin
    hotPos := FTok.hotPos;
    operand := ReadTerm;
    if operand.IsOfType(FCompilerContext.TypBoolean) then
      result := TNotBoolExpr.Create(FCompilerContext, hotPos, operand)
    else if operand.IsOfType(FCompilerContext.TypInteger) then
      result := TNotIntExpr.Create(FCompilerContext, hotPos, operand)
    else
    begin
      if not operand.IsOfType(FCompilerContext.TypVariant) then
        FMsgs.AddCompilerError(hotPos, CPE_BooleanOrIntegerExpected);
      result := TNotVariantExpr.Create(FCompilerContext, hotPos, operand);
    end;
  end;

  function ReadAwaitTerm: TUnaryOpExpr;
  var
    operand: TTypedExpr;
    hotPos: TScriptPos;
  begin
    hotPos := FTok.hotPos;
    if not(coAllowAsyncAwait in compiler.options) then
      FMsgs.AddCompilerError(hotPos, CPE_AwaitNotSupported);
    operand := ReadTerm(False, FCompilerContext.TypAnyType);
    result := TAwaitExpr.Create(FCompilerContext, hotPos, operand);
  end;

  function ReadNull(expecting: TTypeSymbol): TConstExpr;
  begin
    result := TConstExpr.CreateNull(cNullPos, expecting);
  end;

  procedure CheckForClosure(funcSym: TFuncSymbol);

    procedure CheckSubExprs(expr: TExprBase);
    var
      i: Integer;
      funcSym: TFuncSymbol;
    begin
      if expr = nil then
        Exit;
      if (expr is TVarParentExpr) or (expr is TVarParamParentExpr) then
        FMsgs.AddCompilerErrorFmt(expr.scriptPos, CPE_SymbolCannotBeCaptured,
          [(expr as TVarExpr).dataSym.name])
      else if expr is TFuncExprBase then
      begin
        funcSym := TFuncExprBase(expr).funcSym;
        if Assigned(funcSym) and (funcSym.level <> 1) then
          FMsgs.AddCompilerError(expr.scriptPos, CPE_LocalFunctionAsDelegate);
      end;
      for i := 0 to expr.SubExprCount - 1 do
        CheckSubExprs(expr.SubExpr[i]);
    end;

  var
    i: Integer;
  begin
    for i := 0 to funcSym.Executable.SubExprCount - 1 do
      CheckSubExprs(funcSym.Executable.SubExpr(i));
  end;

  function ReadAnonymousMethod(funcType: TTokenType; const hotPos: TScriptPos)
    : TAnonymousFuncRefExpr;
  var
    funcSym: TFuncSymbol;
  begin
    funcSym := ReadProcDecl(funcType, hotPos, [pdoAnonymous]);
    CurrentProg.table.AddSymbol(funcSym);
    ReadProcBody(funcSym);
    result := TAnonymousFuncRefExpr.Create(FCompilerContext,
      GetFuncExpr(funcSym, nil));
    if not(coAllowClosures in options) then
      CheckForClosure(funcSym);
  end;

  function ReadLambda(funcType: TTokenType; const hotPos: TScriptPos)
    : TAnonymousFuncRefExpr;
  var
    funcSym: TFuncSymbol;
    expectingFuncSym: TFuncSymbol;
    expectedType: TTypeSymbol;
    expectingParams: TParamsSymbolTable;
    resultExpr: TTypedExpr;
    resultVar: TVarExpr;
    proc: TdwsProcedure;
    procPos: TScriptPos;
    oldprog: TdwsProgram;
  begin
    expectingFuncSym := expecting.AsFuncSymbol;
    if expectingFuncSym <> nil then
    begin
      expectingParams := expectingFuncSym.Params;
      expectedType := expectingFuncSym.typ;
    end
    else
    begin
      expectingParams := nil;
      expectedType := nil;
    end;

    funcSym := ReadProcDecl(funcType, hotPos, [pdoAnonymous], expectingParams);
    CurrentProg.table.AddSymbol(funcSym);

    if (funcSym.typ = nil) and (expectingFuncSym <> nil) then
      if ((expectedType = nil) or (expectedType.ClassType <> TAnyTypeSymbol))
      then
        funcSym.typ := expectedType;

    if FTok.TestDelete(ttEQ_GTR) then
    begin

      FTok.TestName;
      procPos := FTok.hotPos;

      proc := TdwsProcedure.Create(CurrentProg);
      proc.SetBeginPos(procPos);
      proc.AssignTo(funcSym);

      oldprog := CurrentProg;
      CurrentProg := proc;
      try
        resultExpr := ReadExpr(expectedType);
      finally
        CurrentProg := oldprog;
      end;
      if funcSym.typ = nil then
        funcSym.typ := resultExpr.typ;

      if funcSym.typ = nil then
      begin
        FMsgs.AddCompilerError(procPos, CPE_UnexpectedEqGtrForLambdaStatement);
        proc.expr := resultExpr;
      end
      else
      begin
        resultVar := TVarExpr.CreateTyped(FCompilerContext, procPos,
          proc.func.result);
        proc.expr := CreateAssign(procPos, ttASSIGN, resultVar, resultExpr);
      end;

    end
    else
    begin

      ReadProcBody(funcSym);

    end;
    result := TAnonymousFuncRefExpr.Create(FCompilerContext,
      GetFuncExpr(funcSym, nil));

    if not(coAllowClosures in options) then
      CheckForClosure(funcSym);
  end;

  function ReadAnonymousRecord: TTypedExpr;
  var
    scriptPos: TScriptPos;
    recordType: TRecordSymbol;
    data: IDataContext;
  begin
    scriptPos := FTok.hotPos;
    recordType := ReadRecordDecl('', True);
    CurrentProg.table.AddSymbol(recordType);
    // recordType.SetIsExternal;

    RecordSymbolUseImplicitReference(recordType, scriptPos, False);

    if recordType.IsDynamic then
    begin

      result := TDynamicRecordExpr.Create(FCompilerContext, scriptPos,
        recordType);

    end
    else
    begin

      data := TDataContext.CreateStandalone(recordType.Size);
      recordType.InitDataContext(data, 0);

      result := TConstExpr.CreateTyped(FCompilerContext, recordType, data);

    end;
  end;

  function ReadAnonymousClass: TTypedExpr;
  var
    scriptPos: TScriptPos;
    ClassType: TClassSymbol;
  begin
    scriptPos := FTok.hotPos;
    ClassType := ReadClassDecl('', [csfExternal], True);
    CurrentProg.table.AddSymbol(ClassType);

    RecordSymbolUseImplicitReference(ClassType, scriptPos, False);

    result := TConstructorAnonymousExpr.Create(scriptPos, ClassType);
  end;

  procedure ReportIncompatibleAt(const scriptPos: TScriptPos; expr: TTypedExpr);
  var
    exprTyp: String;
  begin
    if (expr is TFuncExprBase) and (TFuncExprBase(expr).funcSym <> nil) then
      exprTyp := TFuncExprBase(expr).funcSym.Caption
    else if expr.typ <> nil then
      exprTyp := expr.typ.Caption
    else
      exprTyp := SYS_VOID;
    FMsgs.AddCompilerErrorFmt(scriptPos, CPE_IncompatibleTypes,
      [expecting.Caption, exprTyp]);
  end;

  function ReadAt(expecting: TTypeSymbol = nil): TTypedExpr;
  const
    cNilIntf: IUnknown = nil;
  var
    hotPos: TScriptPos;
  begin
    hotPos := FTok.hotPos;
    if (expecting = nil) or expecting.CanExpectAnyFuncSymbol then
      expecting := FCompilerContext.TypAnyFunc
    else if expecting.AsFuncSymbol = nil then
      FMsgs.AddCompilerError(hotPos, CPE_UnexpectedAt)
    else if expecting = FCompilerContext.TypAnyFunc then
      FMsgs.AddCompilerStop(hotPos, CPE_UnexpectedAt);

    result := ReadTerm(isWrite, expecting);

    if result = nil then
    begin
      // error was already reported
      result := TBogusConstExpr.CreateValue(cNullPos, FCompilerContext.TypNil,
        cNilIntf);
    end
    else if (result.typ = nil) or (result.typ.AsFuncSymbol = nil) then
    begin
      if (expecting = FCompilerContext.TypAnyFunc) or (result is TConstExpr)
      then
        FMsgs.AddCompilerError(hotPos, CPE_UnexpectedAt)
      else if result <> nil then // if nil, error was already reported
        ReportIncompatibleAt(hotPos, result);
      // keep compiling
      OrphanAndNil(result);
      result := TBogusConstExpr.CreateValue(cNullPos, FCompilerContext.TypNil,
        cNilIntf);
    end;
  end;

  function ReadImmediate: TTypedExpr;
  var
    constExpr: TConstExpr;
    hasDot: Boolean;
  begin
    constExpr := ReadConstImmediateValue;
    if constExpr <> nil then
    begin
      try
        hasDot := FTok.Test(ttDOT);
      except
        constExpr.Orphan(FCompilerContext);
        raise;
      end;
      if hasDot then
        result := (ReadSymbol(constExpr, isWrite) as TTypedExpr)
      else
        result := constExpr;
    end
    else
      result := nil;
  end;

var
  tt: TTokenType;
  nameExpr: TProgramExpr;
  hotPos: TScriptPos;
begin
  tt := FTok.TestAny([ttPLUS, ttMINUS, ttALEFT, ttNOT, ttBLEFT, ttAT, ttTRUE,
    ttFALSE, ttNIL, ttIF, ttFUNCTION, ttPROCEDURE, ttLAMBDA, ttAWAIT, ttRECORD,
    ttCLASS, ttBRIGHT, ttPLUS_PLUS, ttMINUS_MINUS]);
  case tt of
    ttBRIGHT:
      begin
        // special logic for property write expressions
        if FPendingSetterValueExpr <> nil then
        begin
          result := FPendingSetterValueExpr;
          FPendingSetterValueExpr := nil;
        end
        else
        begin
          FMsgs.AddCompilerError(FTok.hotPos, CPE_ExpressionExpected);
          result := nil;
        end;
        Exit;
      end;
    ttPLUS, ttPLUS_PLUS, ttMINUS_MINUS:
      begin
        FTok.KillToken;
        FTok.TestName;
        hotPos := FTok.hotPos;
        if tt in [ttPLUS_PLUS, ttMINUS_MINUS] then
          FMsgs.AddCompilerWarningFmt(hotPos, CPW_AmbiguousOperator,
            [cTokenStrings[tt]]);
        result := ReadTerm; // (redundant) plus sign
        if result = nil then
          Exit // ReadTerm will have already notified the error
        else if ResolveOperatorFor(CurrentProg, ttMINUS, nil, result.typ) = nil
        then
          FMsgs.AddCompilerError(hotPos, CPE_NumericalExpected);
      end;
    ttMINUS:
      begin
        FTok.KillToken;
        result := ReadNegation;
      end;
    ttALEFT:
      begin
        FTok.KillToken;
        result := ReadArrayConstant(ttARIGHT, expecting);
      end;
    ttNOT:
      begin
        FTok.KillToken;
        result := ReadNotTerm;
      end;
    ttBLEFT:
      begin
        FTok.KillToken;
        result := ReadBracket;
        if FTok.Test(ttDOT) then
          result := (ReadSymbol(result, isWrite) as TTypedExpr);
      end;
    ttAT:
      begin
        FTok.KillToken;
        result := ReadAt(expecting);
      end;
    ttTRUE:
      begin
        FTok.KillToken;
        result := FUnifiedConstants.CreateBoolean(True);
      end;
    ttFALSE:
      begin
        FTok.KillToken;
        result := FUnifiedConstants.CreateBoolean(False);
      end;
    ttNIL:
      begin
        FTok.KillToken;
        result := FUnifiedConstants.CreateNil;
      end;
    ttIF:
      begin
        FTok.KillToken;
        result := ReadIfExpr(expecting);
      end;
    ttPROCEDURE, ttFUNCTION:
      begin
        FTok.KillToken;
        result := ReadAnonymousMethod(tt, FTok.hotPos);
      end;
    ttLAMBDA:
      begin
        FTok.KillToken;
        result := ReadLambda(tt, FTok.hotPos);
      end;
    ttAWAIT:
      begin
        FTok.KillToken;
        result := ReadAwaitTerm;
      end;
    ttRECORD:
      begin
        FTok.KillToken;
        result := ReadAnonymousRecord;
      end;
    ttCLASS:
      begin
        FTok.KillToken;
        if not(coAllowClosures in options) then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_AnonymousClassNotAllowed);
        result := ReadAnonymousClass;
      end;
  else
    if tt <> ttNone then
      FTok.KillToken;
    if (FTok.TestAny([ttINHERITED, ttNEW]) <> ttNone) or FTok.TestName then
    begin
      // Variable or Function
      nameExpr := ReadName(isWrite, expecting);
      if (nameExpr <> nil) and not nameExpr.InheritsFrom(TTypedExpr) then
      begin
        OrphanAndNil(nameExpr);
        FMsgs.AddCompilerError(FTok.hotPos, CPE_ExpressionExpected);
        // keep compiling
        if expecting = nil then
          expecting := FCompilerContext.TypVariant;
        result := ReadNull(expecting);
      end
      else
        result := TTypedExpr(nameExpr);
    end
    else
    begin // Constant values in the code
      result := ReadImmediate;
    end;
  end;

  // No expression found
  if not Assigned(result) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_ExpressionExpected);
end;

// ReadBracket
//
function TdwsCompiler.ReadBracket(expecting: TTypeSymbol = nil): TTypedExpr;
begin
  result := ReadExpr(expecting);
  if not FTok.TestDelete(ttBRIGHT) then
  begin
    OrphanAndNil(result);
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);
  end
  else if Optimize then
    result := result.OptimizeToTypedExpr(FCompilerContext, result.scriptPos);
end;

// ReadNegation
//
function TdwsCompiler.ReadNegation: TTypedExpr;
var
  OpSymbol: TOperatorSymbol;
  negExprClass: TUnaryOpExprClass;
  negTerm: TTypedExpr;
  hotPos: TScriptPos;
begin
  FTok.TestName; // make sure hotpos is on next token
  hotPos := FTok.hotPos;
  negTerm := ReadTerm;
  if negTerm = nil then
  begin
    result := TErrorValueExpr.Create(FSystemTable.TypAnyType);
    Exit;
  end;

  // shortcut for common negations
  if negTerm.typ = FCompilerContext.TypInteger then
    negExprClass := TNegIntExpr
  else if negTerm.typ = FCompilerContext.TypFloat then
    negExprClass := TNegFloatExpr
  else
  begin
    OpSymbol := ResolveOperatorFor(CurrentProg, ttMINUS, nil, negTerm.typ);
    if OpSymbol = nil then
    begin
      FMsgs.AddCompilerError(hotPos, CPE_NumericalExpected);
      negExprClass := TNegVariantExpr; // keep compiling
    end
    else
    begin
      Assert(OpSymbol.OperatorExprClass.InheritsFrom(TUnaryOpExpr));
      negExprClass := TUnaryOpExprClass(OpSymbol.OperatorExprClass);
    end;
  end;
  result := negExprClass.Create(FCompilerContext, hotPos, negTerm);
  if Optimize or (negTerm is TConstExpr) then
    result := result.OptimizeToTypedExpr(FCompilerContext, hotPos);
end;

// ReadIfExpr
//
function TdwsCompiler.ReadIfExpr(expecting: TTypeSymbol = nil): TTypedExpr;
var
  boolExpr: TTypedExpr;
  TrueExpr, falseExpr: TTypedExpr;
  trueTyp, falseTyp: TTypeSymbol;
  typ: TTypeSymbol;
begin
  var
  hotPos := FTok.hotPos;
  boolExpr := ReadBooleanExpr;
  TrueExpr := nil;
  falseExpr := nil;
  try
    if not FTok.TestDelete(ttTHEN) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ThenExpected);

    var
    truePos := FTok.CurrentPos;
    TrueExpr := ReadExpr(expecting);
    if TrueExpr = nil then
      trueTyp := nil
    else
    begin
      truePos.ReplaceIfArgIsDefined(TrueExpr.scriptPos);
      trueTyp := TrueExpr.typ;
      if trueTyp = nil then
        FMsgs.AddCompilerError(truePos, CPE_ExpressionExpected);
      if (expecting <> nil) and not trueTyp.IsOfType(expecting) then
      begin
        TrueExpr := CompilerUtils.WrapWithImplicitConversion(FCompilerContext,
          TrueExpr, expecting, truePos, '', True);
        trueTyp := TrueExpr.typ;
      end;
    end;

    if FTok.TestDelete(ttELSE) then
    begin

      var
      falsePos := FTok.CurrentPos;
      falseExpr := ReadExpr(expecting);
      if falseExpr = nil then
        falseTyp := nil
      else
      begin
        falsePos.ReplaceIfArgIsDefined(falseExpr.scriptPos);
        falseTyp := falseExpr.typ;
        if falseTyp = nil then
          FMsgs.AddCompilerError(falsePos, CPE_ExpressionExpected);
        if (expecting <> nil) and not falseTyp.IsOfType(expecting) then
        begin
          falseExpr := CompilerUtils.WrapWithImplicitConversion
            (FCompilerContext, falseExpr, expecting, falsePos, '', True);
          falseTyp := falseExpr.typ;
        end;
      end;

    end
    else if trueTyp <> nil then
    begin

      falseExpr := CreateTypedDefault(trueTyp);
      falseTyp := trueTyp;

    end
    else
      falseTyp := nil;

    if (trueTyp = nil) or (falseTyp = nil) then
    begin

      typ := nil;

    end
    else if trueTyp = falseTyp then
    begin

      typ := trueTyp;

    end
    else
    begin

      if TrueExpr.IsOfType(FCompilerContext.TypInteger) and
        falseExpr.IsOfType(FCompilerContext.TypFloat) then
      begin
        TrueExpr := TConvIntToFloatExpr.Create(FCompilerContext, hotPos,
          TrueExpr);
        trueTyp := TrueExpr.typ;
      end
      else if TrueExpr.IsOfType(FCompilerContext.TypFloat) and
        falseExpr.IsOfType(FCompilerContext.TypInteger) then
      begin
        falseExpr := TConvIntToFloatExpr.Create(FCompilerContext, hotPos,
          falseExpr);
        falseTyp := falseExpr.typ;
      end;

      if falseTyp.IsOfType(FCompilerContext.TypNil) then
      begin

        typ := trueTyp;
        if not typ.IsCompatible(falseTyp) then
          FMsgs.AddCompilerError(hotPos, CPE_InvalidArgCombination);

      end
      else if trueTyp.IsOfType(FCompilerContext.TypNil) then
      begin

        typ := falseTyp;
        if not typ.IsCompatible(trueTyp) then
          FMsgs.AddCompilerError(hotPos, CPE_InvalidArgCombination);

      end
      else if falseTyp.IsCompatible(trueTyp) then

        typ := falseExpr.typ

      else
      begin

        typ := trueTyp;
        if not typ.IsCompatible(falseTyp) then
        begin
          FMsgs.AddCompilerError(hotPos, CPE_InvalidArgCombination);
        end
        else
        begin
          falseExpr := TConvExpr.WrapWithConvCast(CompilerContext,
            falseExpr.scriptPos, typ, falseExpr, '');
        end;

      end;

    end;

    result := TIfThenElseValueExpr.Create(FCompilerContext, hotPos, typ,
      boolExpr, TrueExpr, falseExpr);
  except
    OrphanAndNil(boolExpr);
    OrphanAndNil(TrueExpr);
    OrphanAndNil(falseExpr);
    raise;
  end;
end;

// ReadConstImmediateValue
//
function TdwsCompiler.ReadConstImmediateValue: TConstExpr;
var
  tt: TTokenType;
  token: TToken;
begin
  result := nil;
  tt := FTok.TestAny([ttStrVal, ttIntVal, ttFloatVal]);
  if tt <> ttNone then
  begin
    token := FTok.GetToken;
    case tt of
      ttIntVal:
        result := FUnifiedConstants.CreateInteger(token.FInteger);
      ttFloatVal:
        result := FUnifiedConstants.CreateFloat(token.FFloat);
      ttStrVal:
        if token.EmptyString then
          result := FUnifiedConstants.CreateEmptyString
        else
          result := FUnifiedConstants.CreateString(token.AsString);
    end;
    FTok.KillToken;
  end;
end;

// ReadConstRecord
//
function TdwsCompiler.ReadConstRecord(symbol: TRecordSymbol): IDataContext;
var
  sym: TSymbol;
  memberSym: TFieldSymbol;
  memberSet: array of Boolean;
  memberTyp: TTypeSymbol;
  expr: TTypedExpr;
  exprPos: TScriptPos;
  factory: IdwsDataSymbolFactory;
begin
  if not FTok.TestDelete(ttBLEFT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackLeftExpected);

  SetLength(memberSet, symbol.Size);

  result := TDataContext.CreateStandalone(symbol.Size);
  symbol.InitDataContext(result, 0);

  factory := TCompositeTypeSymbolFactory.Create(Self, symbol, cvPublic);

  while not FTok.Test(ttBRIGHT) do
  begin
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);
    sym := symbol.Members.FindLocal(FTok.GetToken.AsString);
    if not(sym is TFieldSymbol) then
    begin
      ReportNoMemberForType(FTok.GetToken.AsString,
        FTok.GetToken.FScriptPos, symbol);
      sym := nil;
    end;
    memberSym := TFieldSymbol(sym);
    if memberSym <> nil then
    begin
      memberTyp := memberSym.typ;
      if memberSym.visibility < cvPublic then
        FMsgs.AddCompilerErrorFmt(FTok.GetToken.FScriptPos,
          CPE_MemberSymbolNotVisible, [FTok.GetToken.AsString]);
      if memberSet[memberSym.Offset] then
        FMsgs.AddCompilerError(FTok.GetToken.FScriptPos, CPE_FieldAlreadySet);
      memberSet[memberSym.Offset] := True;
      RecordSymbolUseReference(memberSym, FTok.GetToken.FScriptPos, True);
    end
    else
    begin
      memberTyp := nil;
    end;
    FTok.KillToken;
    if not FTok.TestDelete(ttCOLON) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ColonExpected);
    exprPos := FTok.hotPos;
    expr := factory.ReadInitExpr(memberTyp);
    try
      if (expr = nil) or not expr.IsConstant then
      begin
        FMsgs.AddCompilerError(FTok.hotPos, CPE_ConstantExpressionExpected);
        OrphanAndNil(expr);
      end;
      if (expr <> nil) and (memberTyp <> nil) then
      begin
        if expr.typ.IsOfType(FCompilerContext.TypInteger) and
          memberTyp.IsOfType(FCompilerContext.TypFloat) then
          result[memberSym.Offset] := expr.EvalAsFloat(FExec)
        else if not expr.typ.IsCompatible(memberTyp) then
          FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_InvalidConstTypeVsExpected,
            [expr.typ.Caption, memberTyp.Caption])
        else if memberTyp.Size = 1 then
          expr.EvalAsVariantToDataContext(FExec, result, memberSym.Offset)
        else
        begin
          expr := expr.OptimizeToTypedExpr(FCompilerContext, exprPos);
          result.WriteData(memberSym.Offset, (expr as TDataExpr).DataPtr[FExec],
            0, memberTyp.Size);
        end;
      end;
    finally
      OrphanAndNil(expr);
    end;
    if not FTok.TestDelete(ttSEMI) then
      Break;
  end;

  if not FTok.TestDelete(ttBRIGHT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);
end;

// ReadArrayParams
//
procedure TdwsCompiler.ReadArrayParams(ArrayIndices: TSymbolTable);
var
  i: Integer;
  names: TSimpleStringList;
  typSym: TTypeSymbol;
  isVarParam, isConstParam: Boolean;
  posArray: TScriptPosArray;
begin
  if FTok.TestDelete(ttARIGHT) then
  begin
    FMsgs.AddCompilerError(FTok.hotPos, CPE_ParamsExpected);
    Exit;
  end;

  // At least one argument was found
  names := FStringListPool.Acquire;
  try
    repeat
      isVarParam := FTok.TestDelete(ttVAR);
      if not isVarParam then
        isConstParam := FTok.TestDelete(ttCONST)
      else
        isConstParam := False;

      ReadNameList(names, posArray);

      if not FTok.TestDelete(ttCOLON) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_ColonExpected)
      else
      begin
        typSym := ReadType('', tcParameter);
        for i := 0 to names.Count - 1 do
        begin
          if isVarParam then
            ArrayIndices.AddSymbol(TVarParamSymbol.Create(names[i], typSym, []))
          else if isConstParam then
            ArrayIndices.AddSymbol(CreateConstParamSymbol(names[i], typSym))
          else
            ArrayIndices.AddSymbol(TParamSymbol.Create(names[i], typSym, []));
        end;
      end;
    until not FTok.TestDelete(ttSEMI);

  finally
    FStringListPool.Release(names);
  end;

  if not FTok.TestDelete(ttARIGHT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_ArrayBracketRightExpected);
end;

// ReadParams
//
procedure TdwsCompiler.ReadParams(const hasParamMeth: THasParamSymbolMethod;
  const addParamMeth: TAddParamSymbolMethod;
  forwardedParams: TParamsSymbolTable; expectedLambdaParams: TParamsSymbolTable;
  var posArray: TScriptPosArray);

  procedure GenerateParam(const curName: String; const scriptPos: TScriptPos;
    paramSemantics: TParamSymbolSemantics; paramType: TTypeSymbol;
    const typScriptPos: TScriptPos; var DefaultExpr: TTypedExpr;
    paramOptions: TParamSymbolOptions);
  var
    paramSym: TParamSymbol;
    data: IDataContext;
  begin
    case paramSemantics of
      pssLazy:
        paramSym := TLazyParamSymbol.Create(curName, paramType);
      pssVar:
        paramSym := TVarParamSymbol.Create(curName, paramType, []);
      pssConst:
        paramSym := CreateConstParamSymbol(curName, paramType);
    else
      if Assigned(DefaultExpr) then
      begin
        if DefaultExpr.ClassType = TArrayConstantExpr then
        begin
          FExec.DataContext_CreateEmpty(paramType.Size, data);
          if TArrayConstantExpr(DefaultExpr).ElementCount = 0 then
            paramType.InitDataContext(data, 0)
          else
            TArrayConstantExpr(DefaultExpr).EvalToDataContext(FExec, data, 0);
          paramSym := TParamSymbolWithDefaultValue.Create(curName, paramType,
            data, paramOptions);
        end
        else
        begin
          paramSym := TParamSymbolWithDefaultValue.Create(curName, paramType,
            (DefaultExpr as TConstExpr).DataContext, paramOptions);
        end;
      end
      else
      begin
        paramSym := TParamSymbol.Create(curName, paramType, paramOptions);
      end;
      paramOptions := [];
    end;

    if paramOptions <> [] then
      FMsgs.AddCompilerErrorFmt(scriptPos,
        CPE_StrictParameterTypeCheckNotSupportedFor,
        [cParamSymbolSemanticsPrefix[paramSemantics]]);

    if hasParamMeth(paramSym) then
      FMsgs.AddCompilerErrorFmt(scriptPos, CPE_NameAlreadyExists, [curName]);
    addParamMeth(paramSym);

    // Enter Field symbol in dictionary
    if coSymbolDictionary in FOptions then
    begin
      // add parameter symbol
      if forwardedParams = nil then
      begin
        // no forward, our param symbol is the actual one
        RecordSymbolUse(paramSym, scriptPos, [suDeclaration])
      end
      else
      begin
        // find the original param symbol and register it
        // in case of mismatch, RecordSymbolUse will discard
        // a nil automatically, so we don't have to check here
        RecordSymbolUse(forwardedParams.FindLocal(curName), scriptPos,
          [suReference]);
      end;
      // record field's type symbol
      if not typScriptPos.Defined then
        RecordSymbolUse(paramType, scriptPos, [suReference, suImplicit])
    end;
  end;

var
  i, paramIdx: Integer;
  names: TSimpleStringList;
  typ: TTypeSymbol;
  onlyDefaultParamsNow: Boolean;
  typScriptPos, exprPos: TScriptPos;
  DefaultExpr: TTypedExpr;
  expectedParam: TParamSymbol;
  localPosArray: TScriptPosArray;
  paramSemantics: TParamSymbolSemantics;
  paramOptions: TParamSymbolOptions;
begin
  if FTok.TestDelete(ttBLEFT) then
  begin

    if not FTok.TestDelete(ttBRIGHT) then
    begin
      // At least one argument was found
      names := FStringListPool.Acquire;
      try
        paramIdx := 0;
        onlyDefaultParamsNow := False;
        repeat
          case FTok.TestDeleteAny([ttLAZY, ttVAR, ttCONST]) of
            ttLAZY:
              paramSemantics := pssLazy;
            ttVAR:
              paramSemantics := pssVar;
            ttCONST:
              paramSemantics := pssConst;
          else
            paramSemantics := pssCopy;
          end;

          ReadNameList(names, localPosArray);
          ConcatScriptPosArray(posArray, localPosArray, names.Count);

          if not FTok.TestDelete(ttCOLON) then
          begin

            if (expectedLambdaParams <> nil) and
              (paramIdx + names.Count - 1 < expectedLambdaParams.Count) then
            begin

              DefaultExpr := nil;
              for i := 0 to names.Count - 1 do
              begin
                expectedParam := expectedLambdaParams[paramIdx + i];
                paramSemantics := expectedParam.Semantics;
                GenerateParam(names[i], localPosArray[i], paramSemantics,
                  expectedParam.typ, cNullPos, DefaultExpr, []);
              end;

            end
            else
            begin

              FMsgs.AddCompilerStop(FTok.hotPos, CPE_ColonExpected)

            end;

          end
          else
          begin

            if FTok.TestDelete(ttTYPE) then
              paramOptions := [psoForbidImplicitCasts]
            else
              paramOptions := [];

            typ := ReadType('', tcParameter);

            DefaultExpr := nil;
            try
              typScriptPos := FTok.hotPos;

              if (paramSemantics <> pssConst) and (typ <> nil) and
                (typ.ClassType = TOpenArraySymbol) then
                FMsgs.AddCompilerError(FTok.hotPos,
                  CPE_OpenArrayParamMustBeConst);
              if (paramSemantics = pssLazy) and (typ.AsFuncSymbol <> nil) then
                FMsgs.AddCompilerError(FTok.hotPos,
                  CPE_LazyParamCantBeFunctionPointer);

              if FTok.TestDelete(ttEQ) then
              begin
                onlyDefaultParamsNow := True;
                case paramSemantics of
                  pssLazy:
                    FMsgs.AddCompilerError(FTok.hotPos,
                      CPE_LazyParamCantHaveDefaultValue);
                  pssVar:
                    FMsgs.AddCompilerError(FTok.hotPos,
                      CPE_VarParamCantHaveDefaultValue);
                  pssConst:
                    FMsgs.AddCompilerError(FTok.hotPos,
                      CPE_ConstParamCantHaveDefaultValue);
                end;

                exprPos := FTok.hotPos;
                DefaultExpr := FStandardDataSymbolFactory.ReadInitExpr(typ);

                if DefaultExpr <> nil then
                begin
                  if (not DefaultExpr.IsConstant) or (DefaultExpr.typ = nil)
                  then
                  begin
                    FMsgs.AddCompilerError(FTok.hotPos,
                      CPE_ConstantExpressionExpected);
                    OrphanAndNil(DefaultExpr);
                  end
                  else if not typ.IsCompatible(DefaultExpr.typ) then
                  begin
                    DefaultExpr := CompilerUtils.WrapWithImplicitConversion
                      (FCompilerContext, DefaultExpr, typ, exprPos);
                    if DefaultExpr.ClassType = TConvInvalidExpr then
                      OrphanAndNil(DefaultExpr);
                  end;
                end;
              end
              else if onlyDefaultParamsNow then
              begin
                FMsgs.AddCompilerError(FTok.hotPos, CPE_DefaultValueRequired);
              end;

              if (DefaultExpr <> nil) and not(DefaultExpr is TConstExpr) then
                DefaultExpr := DefaultExpr.OptimizeToTypedExpr
                  (FCompilerContext, exprPos);

              for i := 0 to names.Count - 1 do
                GenerateParam(names[i], localPosArray[i], paramSemantics, typ,
                  typScriptPos, DefaultExpr, paramOptions);

            finally
              OrphanAndNil(DefaultExpr);
            end;

          end;

          Inc(paramIdx, names.Count);

        until not FTok.TestDelete(ttSEMI);

      finally
        FStringListPool.Release(names);
      end;

      if not FTok.TestDelete(ttBRIGHT) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);
    end;

  end
  else if (expectedLambdaParams <> nil) and (expectedLambdaParams.Count > 0)
  then
  begin

    // implicit anonmous lambda params
    DefaultExpr := nil;
    for i := 0 to expectedLambdaParams.Count - 1 do
    begin
      expectedParam := expectedLambdaParams[i];
      paramSemantics := expectedParam.Semantics;
      GenerateParam('_implicit_' + expectedParam.name, cNullPos, paramSemantics,
        expectedParam.typ, cNullPos, DefaultExpr, []);
    end;

  end;
end;

// ReadProcCallQualifiers
//
procedure TdwsCompiler.ReadProcCallQualifiers(funcSymbol: TFuncSymbol);
var
  tt: TTokenType;
begin
  tt := FTok.TestDeleteAny([ttSAFECALL, ttSTDCALL, ttCDECL, ttREGISTER,
    ttPASCAL]);
  if funcSymbol.IsExternal then
  begin
    if tt = ttNone then
      tt := ttREGISTER;
    funcSymbol.ExternalConvention := tt;
  end
  else if tt <> ttNone then
  begin
    FMsgs.AddCompilerHintFmt(FTok.hotPos,
      CPH_CallConventionIsNotSupportedAndIgnored, [cTokenStrings[tt]],
      hlStrict);
    ReadSemiColon;
  end;
end;

// ReadSwitch
//
function TdwsCompiler.ReadSwitch(const switchName: String): Boolean;
var
  sw: TSwitchInstruction;
begin
  sw := StringToSwitchInstruction(switchName);
  if sw <> siNone then
    Exit(True);

  result := False;

  if Assigned(FTok.SwitchProcessor) then
    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_CompilerSwitchUnknown,
      [switchName]);

  while FTok.HasTokens and not FTok.TestDelete(ttCRIGHT) do
    FTok.KillToken;
end;

// ReadInstrSwitch
//
function TdwsCompiler.ReadInstrSwitch(const switchName: String): Boolean;
var
  switch: TSwitchInstruction;
  name: String;
  scriptSource, scriptLocation: String;
  i: Integer;
  conditionalTrue: Boolean;
  switchPos, condPos, fileNamePos: TScriptPos;
  condExpr: TTypedExpr;
  sourceFile: TSourceFile;
  includeSymbol: TIncludeSymbol;
  condInfo: TTokenizerConditionalInfo;
  filter: TdwsFilter;
begin
  result := True;
  if Assigned(FOnReadInstrSwitch) then
  begin
    if FOnReadInstrSwitch(Self) then
      Exit;
  end;

  switchPos := FTok.hotPos;

  switch := StringToSwitchInstruction(switchName);
  FTok.KillToken;

  case switch of
    siIncludeLong, siIncludeShort, siIncludeOnce, siFilterLong, siFilterShort:
      begin

        if FTok.Test(ttPERCENT) and (switch in [siIncludeShort, siIncludeLong])
        then
        begin

          result := ReadExprSwitch(switchPos);
          Exit;

        end
        else if not FTok.Test(ttStrVal) then
        begin

          FMsgs.AddCompilerError(FTok.hotPos, CPE_IncludeFileExpected);
          // skip in attempt to recover from error
          result := False;
          SkipUntilToken(ttCRIGHT);

        end
        else
        begin

          name := FTok.GetToken.AsString;
          FTok.KillToken;

          if coSymbolDictionary in options then
          begin
            includeSymbol := TIncludeSymbol.Create(name);
            CurrentProg.table.AddSymbol(includeSymbol);
            fileNamePos := FTok.hotPos;
            fileNamePos.IncCol; // skip quote
            RecordSymbolUse(includeSymbol, fileNamePos,
              [suReference, suImplicit]);
          end;

          if (switch = siIncludeOnce) then
          begin
            sourceFile := FMainProg.GetSourceFile(name);
            if (sourceFile <> nil) and (sourceFile.name <> name) then
              FMsgs.AddCompilerWarningFmt(switchPos,
                CPW_IncludeOnceWithDifferentCase, [name, sourceFile.name]);
          end
          else
            sourceFile := nil;
          if sourceFile = nil then
          begin
            try
              if switch in [siFilterLong, siFilterShort] then
              begin
                if not Assigned(FFilter) then
                  FMsgs.AddCompilerStop(FTok.hotPos, CPE_NoFilterAvailable);
                // Include file is processed by the filter
                scriptSource := GetIncludeScriptSource(name, scriptLocation);
                filter := FFilter;
                if Assigned(FOnFilter) then
                  FOnFilter(Self, name, scriptSource, filter);
                if filter <> nil then
                  scriptSource := filter.Process(scriptSource, FMsgs);
              end
              else
              begin
                scriptSource := GetIncludeScriptSource(name, scriptLocation);
              end;

              if not FTok.TestDelete(ttCRIGHT) then
                FMsgs.AddCompilerStop(FTok.hotPos, CPE_CurlyRightExpected);

              if StrContains(FTok.PathName, '\') then
                name := Copy(FTok.PathName, 1,
                  LastDelimiter('\', FTok.PathName)) + name;
              sourceFile := FMainProg.SourceList.Add(name, scriptSource,
                stInclude, scriptLocation);
              FTok.BeginSourceFile(sourceFile);
              if coContextMap in options then
              begin
                FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttSWITCH);
                FTok.OnEndSourceFile := DoTokenizerEndSourceFile;
              end;

              FTok.SimulateToken(ttSEMI, FTok.hotPos);

              Exit;
            except
              on e: ECompileError do
                raise;
              on e: Exception do
                FMsgs.AddCompilerStop(FTok.hotPos, e.Message);
            end;
          end;

        end;

      end;
    siDefine:
      begin

        if not FTok.Test(ttNAME) then
        begin
          FMsgs.AddCompilerError(FTok.hotPos, CPE_NameExpected);
          // skip in attempt to recover from error
          SkipUntilToken(ttCRIGHT);
        end
        else
        begin
          FTok.ConditionalDefines.value.Add(FTok.GetToken.AsString);
          FTok.KillToken;
        end;

      end;
    siUndef:
      begin

        if not FTok.Test(ttNAME) then
        begin
          FMsgs.AddCompilerError(FTok.hotPos, CPE_NameExpected);
          // skip in attempt to recover from error
          SkipUntilToken(ttCRIGHT);
        end
        else
        begin
          i := FTok.ConditionalDefines.value.IndexOf(FTok.GetToken.AsString);
          if i >= 0 then
            FTok.ConditionalDefines.value.Delete(i);
          FTok.KillToken;
        end;

      end;
    siIfDef, siIfNDef, siIf:
      begin

        conditionalTrue := True;
        case switch of
          siIfDef, siIfNDef:
            begin
              if not FTok.Test(ttNAME) then
              begin
                FMsgs.AddCompilerError(FTok.hotPos, CPE_NameExpected);
                // skip in attempt to recover from error
                SkipUntilToken(ttCRIGHT);
              end
              else
              begin
                conditionalTrue :=
                  (FTok.ConditionalDefines.value.IndexOf(FTok.GetToken.AsString)
                  >= 0) xor (switch = siIfNDef);
                FTok.KillToken;
              end;
            end;
          siIf:
            begin
              condPos := FTok.hotPos;
              FIsSwitch := True;
              try
                condExpr := ReadExpr;
                try
                  if not condExpr.IsConstant then
                    FMsgs.AddCompilerError(condPos,
                      CPE_ConstantExpressionExpected)
                  else if not condExpr.IsOfType(FCompilerContext.TypBoolean)
                  then
                    FMsgs.AddCompilerError(condPos, CPE_BooleanExpected)
                  else
                    conditionalTrue := condExpr.EvalAsBoolean(FExec);
                finally
                  OrphanAndNil(condExpr);
                end;
              finally
                FIsSwitch := False;
              end;
            end
        end;

        condInfo.scriptPos := switchPos;
        if conditionalTrue then
        begin
          condInfo.Conditional := tcIf;
          FTok.ConditionalDepth.Push(condInfo);
        end
        else
        begin
          if ReadUntilEndOrElseSwitch(True) then
          begin
            condInfo.Conditional := tcElse;
            FTok.ConditionalDepth.Push(condInfo);
          end;
          if not FTok.HasTokens then
            FMsgs.AddCompilerStop(switchPos,
              CPE_UnbalancedConditionalDirective);
        end;

      end;
    siElse:
      begin
        if FTok.ConditionalDepth.Count = 0 then
          FMsgs.AddCompilerError(switchPos, CPE_UnbalancedConditionalDirective)
        else
        begin
          if FTok.ConditionalDepth.Peek.Conditional = tcElse then
            FMsgs.AddCompilerStop(switchPos,
              CPE_UnfinishedConditionalDirective);

          FTok.ConditionalDepth.Pop;
          ReadUntilEndOrElseSwitch(False);
          if not FTok.HasTokens then
            FMsgs.AddCompilerStop(switchPos,
              CPE_UnbalancedConditionalDirective);
        end;
      end;
    siEndIf:
      begin

        if FTok.ConditionalDepth.Count = 0 then
          FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective)
        else
          FTok.ConditionalDepth.Pop;
        // tolerate junk after endif before the curly right
        SkipUntilToken(ttCRIGHT);

      end;
    siResourceLong, siResourceShort:
      begin

        if not FTok.Test(ttStrVal) then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected);
        if Assigned(FOnResource) then
          FOnResource(Self, FTok.GetToken.AsString);
        FTok.KillToken;

      end;
    siCodeGen:
      begin

        if not FTok.Test(ttStrVal) then
          FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected);
        if Assigned(FOnCodeGen) then
          FOnCodeGen(Self, switchPos, FTok.GetToken.AsString);
        FTok.KillToken;

      end;
    siHint, siWarning, siError, siFatal:
      begin

        if not FTok.Test(ttStrVal) then
          if switch <> siFatal then
            FMsgs.AddCompilerError(FTok.hotPos, CPE_StringExpected)
          else
            FMsgs.AddCompilerStop(FTok.hotPos, CPE_StringExpected)
        else
        begin
          case switch of
            siHint:
              FMsgs.AddCompilerHint(switchPos, FTok.GetToken.AsString);
            siWarning:
              FMsgs.AddCompilerWarning(switchPos, FTok.GetToken.AsString);
            siError:
              FMsgs.AddCompilerError(switchPos, FTok.GetToken.AsString,
                TCompilerErrorMessage);
            siFatal:
              FMsgs.AddCompilerStop(switchPos, FTok.GetToken.AsString,
                TCompilerErrorMessage);
          end;
          FTok.KillToken;
        end;

      end;
    siWarnings:
      begin

        if not FTok.TestDeleteNamePos(name, condPos) then
          name := '';
        conditionalTrue := ASCIISameText(name, 'ON');
        if conditionalTrue or ASCIISameText(name, 'OFF') then
          FMsgs.WarningsDisabled := not conditionalTrue
        else
          FMsgs.AddCompilerError(FTok.hotPos, CPE_OnOffExpected);

      end;
    siHints:
      begin

        if not FTok.TestDeleteNamePos(name, condPos) then
          name := '';
        if ASCIISameText(name, 'OFF') then
          FMsgs.HintsLevel := hlDisabled
        else if ASCIISameText(name, 'ON') then
          FMsgs.HintsLevel := FDefaultHintsLevel
        else if ASCIISameText(name, 'NORMAL') then
          FMsgs.HintsLevel := hlNormal
        else if ASCIISameText(name, 'STRICT') then
          FMsgs.HintsLevel := hlStrict
        else if ASCIISameText(name, 'PEDANTIC') then
          FMsgs.HintsLevel := hlPedantic
        else
          FMsgs.AddCompilerError(FTok.hotPos, CPE_OnOffExpected);

      end;
    siRegion, siEndRegion:
      begin

        SkipUntilToken(ttCRIGHT);

      end;
  end;

  if not FTok.Test(ttCRIGHT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_CurlyRightExpected);

  // Simulate a semicolon
  FTok.GetToken.FTyp := ttSEMI
end;

// ReadExprSwitch
//
function TdwsCompiler.ReadExprSwitch(const switchPos: TScriptPos): Boolean;
var
  asNum: Boolean;
  name, value: String;
  numValue: Integer;
  hotPos: TScriptPos;
  funcSym: TFuncSymbol;
begin
  result := False;
  asNum := False;
  numValue := 0;
  hotPos := FTok.hotPos;

  hotPos := FTok.hotPos;
  if FTok.TestDelete(ttPERCENT) then
  begin
    if FTok.TestAny([ttNAME, ttFUNCTION]) <> ttNone then
    begin
      name := FTok.GetToken.AsString;
      FTok.KillToken;
      if not FTok.TestDelete(ttPERCENT) then
        name := '';
    end;
  end;
  if name = '' then
    FMsgs.AddCompilerError(hotPos, CPE_IncludeItemExpected)
  else if ASCIISameText(name, 'FILE') then
    value := hotPos.sourceFile.name
  else if ASCIISameText(name, 'MAINFILE') then
    value := FMainProg.mainFileName
  else if ASCIISameText(name, 'LINE') then
    value := IntToStr(hotPos.Line)
  else if ASCIISameText(name, 'LINENUM') then
  begin
    numValue := hotPos.Line;
    asNum := True;
  end
  else if ASCIISameText(name, 'DATE') then
    value := FormatDateTime('yyyy-mm-dd', Date)
  else if ASCIISameText(name, 'TIME') then
    value := FormatDateTime('hh:nn:ss', Time)
  else if ASCIISameText(name, 'TIMESTAMP') then
  begin
    asNum := True;
    numValue := UnixTime;
  end
  else if ASCIISameText(name, 'EXEVERSION') then
  begin
    value := ApplicationVersion;
  end
  else if ASCIISameText(name, 'FUNCTION') then
  begin
    if CurrentProg is TdwsProcedure then
    begin
      funcSym := TdwsProcedure(CurrentProg).func;
      if funcSym is TMethodSymbol then
        value := TMethodSymbol(funcSym).StructSymbol.name + '.' + funcSym.name
      else
        value := funcSym.name;
    end
    else
      value := MSG_MainFunction;
  end
  else
    FMsgs.AddCompilerErrorFmt(hotPos, CPE_IncludeItemUnknown, [name]);

  if not FTok.TestDelete(ttCRIGHT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_CurlyRightExpected);

  if asNum then
    FTok.SimulateIntegerToken(switchPos, numValue)
  else
    FTok.SimulateStringToken(switchPos, value);
end;

// SkipUntilToken
//
procedure TdwsCompiler.SkipUntilToken(tt: TTokenType);
begin
  while not FTok.Test(tt) do
  begin
    if not FTok.HasTokens then
      Exit;
    FTok.KillToken;
  end;
end;

// ReadUntilEndOrElseSwitch
//
function TdwsCompiler.ReadUntilEndOrElseSwitch(allowElse: Boolean): Boolean;
var
  StartPos: TScriptPos;
  switch: TSwitchInstruction;
  innerDepth: Integer;
begin
  StartPos := FTok.hotPos;

  // flush the switch that triggered the block
  if not FTok.TestDelete(ttCRIGHT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_CurlyRightExpected);

  innerDepth := 0;
  result := False;

  FTok.SwitchProcessor := nil;

  while FTok.HasTokens do
  begin

    // kill everything up to next switch
    while FTok.HasTokens and (not FTok.Test(ttSWITCH)) do
      FTok.KillToken;

    if not FTok.HasTokens then
      FMsgs.AddCompilerStop(StartPos, CPE_UnbalancedConditionalDirective);

    StartPos := FTok.hotPos;
    switch := StringToSwitchInstruction(FTok.GetToken.AsString);
    FTok.KillToken;

    case switch of

      siEndIf:
        begin

          // tolerate junk after endif
          SkipUntilToken(ttCRIGHT);
          Dec(innerDepth);
          if innerDepth < 0 then
            Break;

        end;
      siElse:
        begin

          if innerDepth = 0 then
          begin
            if not allowElse then
              FMsgs.AddCompilerStop(StartPos,
                CPE_UnfinishedConditionalDirective);
            result := True;
            Break;
          end;

        end;
      siIfDef, siIfNDef, siIf:
        begin

          while FTok.HasTokens and not FTok.Test(ttCRIGHT) do
            FTok.KillToken;
          Inc(innerDepth);

        end;
    else
      while FTok.HasTokens and not FTok.Test(ttCRIGHT) do
        FTok.KillToken;
    end;

    if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_CurlyRightExpected);

  end;

  FTok.SwitchProcessor := ReadInstrSwitch;
end;

// Checks if a name already exists in the Symboltable

procedure TdwsCompiler.CheckName(const name: String; const namePos: TScriptPos);
var
  sym: TSymbol;
  i: Integer;
  subTable: TSymbolTable;
begin
  if name = '' then
    Exit;

  sym := CurrentProg.FindLocal(name);

  if Assigned(sym) then
    FMsgs.AddCompilerErrorFmt(namePos, CPE_NameAlreadyExists, [name]);

  if CurrentProg.table.ClassType = TUnitImplementationTable then
    Exit;

  for i := 0 to CurrentProg.SubTableDepth - 1 do
  begin
    subTable := CurrentProg.subTable(i);
    sym := subTable.FindLocal(name);
    if sym <> nil then
    begin
      FMsgs.AddCompilerHintFmt(namePos,
        CPH_NameAmbiguousInScopeContext, [name]);
      Break;
    end;
  end;
end;

// CheckUnitName
//
procedure TdwsCompiler.CheckUnitName(const name: String;
  const namePos: TScriptPos; const locationName: String);
var
  location: String;
begin
  if locationName = '' then
    Exit;
  if location = name then
    Exit;

  location := ExtractFileName(locationName);
  if UnicodeSameText(location, name) then
  begin
    if location <> name then
      FMsgs.AddCompilerHint(namePos, CPH_UnitNameCaseDoesntMatch);
    Exit;
  end;

  location := StrBeforeChar(location, ' ');
  if UnicodeSameText(location, name) then
  begin
    if location <> name then
      FMsgs.AddCompilerHint(namePos, CPH_UnitNameCaseDoesntMatch);
    Exit;
  end;

  location := ChangeFileExt(location, '');
  if location <> name then
  begin
    if UnicodeSameText(location, name) then
      FMsgs.AddCompilerHint(namePos, CPH_UnitNameCaseDoesntMatch)
    else
      FMsgs.AddCompilerWarning(namePos, CPE_UnitNameDoesntMatch)
  end;
end;

// CheckSpecialName
//
procedure TdwsCompiler.CheckSpecialName(const name: String);
begin
  case IdentifySpecialKeyword(name) of
    skNone, skDefault:
      ;
  else
    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_NameIsReserved, [Name]);
  end;
end;

// CheckSpecialNameCase
//
procedure TdwsCompiler.CheckSpecialNameCase(const name: String;
  sk: TSpecialKeywordKind; const namePos: TScriptPos);
begin
  if name <> cSpecialKeywords[sk] then
    FMsgs.AddCompilerHintFmt(namePos, CPH_CaseDoesNotMatchDeclaration,
      [name, cSpecialKeywords[sk]], hlPedantic);
end;

// OpenStreamForFile
//
function TdwsCompiler.OpenStreamForFile(const fileName: String): TStream;
var
  i: Integer;
  fname: String;
begin
  if FScriptPaths.Count = 0 then
  begin
    if FCompileFileSystem.FileExists(fileName) then
      Exit(FCompileFileSystem.OpenFileStream(fileName, fomFastSequentialRead));
  end
  else
  begin
    for i := 0 to FScriptPaths.Count - 1 do
    begin
      if FScriptPaths[i] <> '' then
        fname := IncludeTrailingPathDelimiter(FScriptPaths[i]) + fileName
      else
        fname := fileName;
      if FCompileFileSystem.FileExists(fname) then
        Exit(FCompileFileSystem.OpenFileStream(fname, fomFastSequentialRead));
    end;
  end;
  result := nil;
end;

// GetScriptSource
//
function TdwsCompiler.GetScriptSource(const scriptName: String): String;
var
  stream: TStream;
begin
  stream := OpenStreamForFile(scriptName);
  if stream = nil then
    result := ''
  else
  begin
    try
      result := LoadTextFromStream(stream);
    finally
      stream.Free;
    end;
    if result = '' then
      result := ' ';
  end;
end;

// GetIncludeScriptSource
//
function TdwsCompiler.GetIncludeScriptSource(const scriptName: String;
  var scriptLocation: String): String;
begin
  result := '';
  scriptLocation := '';

  if Assigned(FOnInclude) then
  begin
    FOnInclude(scriptName, result, scriptLocation);
    if result <> '' then
      Exit;
  end;

  result := GetScriptSource(scriptName);
  if result = '' then
    FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_IncludeFileNotFound, [scriptName],
      TCompilerErrorMessage)
end;

// GetVarExpr
//
function TdwsCompiler.GetVarExpr(const aScriptPos: TScriptPos;
  dataSym: TDataSymbol): TVarExpr;
begin
  if (dataSym.ClassType = TClassVarSymbol) and
    (TClassVarSymbol(dataSym).ownerSymbol.IsExternal) then
    result := TExternalVarExpr.Create(aScriptPos, dataSym)
  else if CurrentProg.level = dataSym.level then
  begin
    if FDataSymbolExprReuse <> nil then
    begin
      result := FDataSymbolExprReuse.GetValue(dataSym);
      if result = nil then
      begin
        result := TVarExpr.CreateTyped(FCompilerContext, aScriptPos, dataSym);
        FDataSymbolExprReuse.SetValue(dataSym, result);
      end;
      result.IncRefCount;
    end
    else
      result := TVarExpr.CreateTyped(FCompilerContext, aScriptPos, dataSym);
  end
  else
    result := TVarParentExpr.Create(aScriptPos, dataSym);
end;

// GetLazyParamExpr
//
function TdwsCompiler.GetLazyParamExpr(dataSym: TLazyParamSymbol)
  : TLazyParamExpr;
begin
  result := TLazyParamExpr.Create(FCompilerContext, dataSym);
end;

// GetVarParamExpr
//
function TdwsCompiler.GetVarParamExpr(dataSym: TVarParamSymbol)
  : TByRefParamExpr;
begin
  if CurrentProg.level = dataSym.level then
    result := TVarParamExpr.Create(cNullPos, dataSym)
  else
    result := TVarParamParentExpr.Create(cNullPos, dataSym)
end;

// GetConstByRefParamExpr
//
function TdwsCompiler.GetConstByRefParamExpr(dataSym: TConstByRefParamSymbol)
  : TByRefParamExpr;
begin
  if CurrentProg.level = dataSym.level then
    result := TConstParamExpr.Create(cNullPos, dataSym)
  else
    result := TConstParamParentExpr.Create(cNullPos, dataSym)
end;

// GetConstParamExpr
//
function TdwsCompiler.GetConstParamExpr(const aScriptPos: TScriptPos;
  dataSym: TParamSymbol): TVarExpr;
begin
  if dataSym is TConstByRefParamSymbol then
    result := GetConstByRefParamExpr(TConstByRefParamSymbol(dataSym))
  else
    result := GetVarExpr(aScriptPos, dataSym);
end;

// GetSelfParamExpr
//
function TdwsCompiler.GetSelfParamExpr(const aScriptPos: TScriptPos;
  selfSym: TDataSymbol): TVarExpr;
var
  ct: TClass;
begin
  ct := selfSym.ClassType;
  if ct = TConstByRefParamSymbol then
    result := GetConstByRefParamExpr(TConstByRefParamSymbol(selfSym))
  else if ct = TVarParamSymbol then
    result := GetVarParamExpr(TVarParamSymbol(selfSym))
  else
  begin
    Assert((ct = TSelfSymbol) or (ct = TParamSymbol) or
      (ct = TConstByValueParamSymbol));
    result := GetVarExpr(aScriptPos, selfSym);
  end;
end;

function TdwsCompiler.CheckParams(tableA, tableB: TParamsSymbolTable;
  checkNames: Boolean; skipB: Integer = 0): Boolean;
var
  x: Integer;
  r: Boolean;
  paramA, paramB: TParamSymbol;
  semanticsA, semanticsB: TParamSymbolSemantics;
begin
  result := True;
  for x := 0 to tableA.Count - 1 do
  begin
    r := False;
    paramA := tableA[x];
    paramB := tableB[x + skipB];
    semanticsA := paramA.Semantics;
    semanticsB := paramB.Semantics;
    if checkNames and not UnicodeSameText(paramA.name, paramB.name) then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_BadParameterName,
        [x, tableA[x].name])
    else if not paramA.typ.IsCompatible(paramB.typ) then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_BadParameterType,
        [x, paramA.typ.Caption, paramB.typ.Caption])
    else if (semanticsA = pssVar) and (semanticsB <> pssVar) then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_VarParameterExpected,
        [x, paramA.name])
    else if (semanticsA <> pssVar) and (semanticsB = pssVar) then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ValueParameterExpected,
        [x, paramA.name])
    else if (semanticsA = pssConst) and (semanticsB <> pssConst) then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ConstParameterExpected,
        [x, paramA.name])
    else if (semanticsA <> pssConst) and (semanticsB = pssConst) then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_ValueParameterExpected,
        [x, paramA.name])
    else if (paramA.ClassType <> TParamSymbolWithDefaultValue) and
      (paramB.ClassType = TParamSymbolWithDefaultValue) then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos,
        CPE_MismatchingParameterDefaultValues, [x, paramA.name])
    else if (paramA.ClassType = TParamSymbolWithDefaultValue) and
      (paramB.ClassType = TParamSymbolWithDefaultValue) and
      not TParamSymbolWithDefaultValue(paramA)
      .SameParam(TParamSymbolWithDefaultValue(paramB)) then
    begin
      FMsgs.AddCompilerErrorFmt(FTok.hotPos,
        CPE_MismatchingParameterDefaultValues, [x, paramA.name])
    end
    else
      r := True;
    result := result and r;
  end;
end;

// CompareFuncKinds
//
procedure TdwsCompiler.CompareFuncKinds(a, b: TFuncKind);
const
  cErrorForFunkKind: array [TFuncKind] of String = (CPE_FunctionExpected,
    CPE_ProcedureExpected, CPE_ConstructorExpected, CPE_DestructorExpected,
    CPE_MethodExpected, CPE_LambdaExpected);
begin
  if a <> b then
    FMsgs.AddCompilerError(FTok.hotPos, cErrorForFunkKind[a]);
end;

// CompareFuncSymbolParams
//
procedure TdwsCompiler.CompareFuncSymbolParams(a, b: TFuncSymbol);
begin
  if (a.typ <> nil) and ((b.typ = nil) or not a.typ.IsCompatible(b.typ)) then
    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_BadResultType, [a.typ.Caption]);

  if a.Params.Count <> b.Params.Count then
    FMsgs.AddCompilerErrorFmt(FTok.hotPos, CPE_BadNumberOfParameters,
      [a.Params.Count, b.Params.Count])
  else
    CheckParams(a.Params, b.Params, True);
end;

// CurrentStruct
//
function TdwsCompiler.CurrentStruct: TCompositeTypeSymbol;
var
  Prog: TdwsProgram;
  func: TFuncSymbol;
begin
  if FCurrentStructure <> nil then
    Exit(FCurrentStructure);
  Prog := CurrentProg;
  while Prog.ClassType = TdwsProcedure do
  begin
    func := TdwsProcedure(Prog).func;
    if func is TMethodSymbol then
      Exit(TMethodSymbol(func).StructSymbol)
    else
      Prog := Prog.Parent;
  end;
  result := nil;
end;

// FindStructMember
//
function TdwsCompiler.FindStructMember(typ: TStructuredTypeSymbol;
  const name: String): TSymbol;
begin
  result := typ.Members.FindSymbolFromScope(name, CurrentStruct);
  if result = nil then
  begin
    result := typ.Members.FindSymbol(name, cvMagic);
    if result <> nil then
      FMsgs.AddCompilerErrorFmt(FTok.hotPos,
        CPE_MemberSymbolNotVisible, [name]);
  end;
end;

// HintUnusedSymbols
//
procedure TdwsCompiler.HintUnusedSymbols;
var
  sym: TSymbol;
  symDecl: TSymbolPosition;
  symDic: TdwsSymbolDictionary;
  symPosList: TSymbolPositionList;
begin
  if not(coSymbolDictionary in options) then
    Exit;
  if coHintsDisabled in options then
    Exit;

  symDic := FMainProg.SymbolDictionary;
  for sym in CurrentProg.table do
  begin
    if sym.ClassType = TVarDataSymbol then
    begin
      if (sym.typ <> nil) and
        (sym.typ.UnAliasedType.ClassType = TDynamicArraySymbol) then
        continue;
      symPosList := symDic.FindSymbolPosList(sym);
      symDecl := symPosList.FindUsage(suDeclaration);
      if symDecl <> nil then
      begin
        if symPosList.FindUsage(suReference) = nil then
          FMsgs.AddCompilerHintFmt(symDecl.scriptPos,
            CPH_VariableDeclaredButNotUsed, [sym.name])
        else if symPosList.FindUsage(suWrite) = nil then
          FMsgs.AddCompilerHintFmt(symDecl.scriptPos,
            CPH_VariableDeclaredButNotWrittenTo, [sym.name]);
      end;
    end;
  end;
end;

// HintUnusedPrivateSymbols
//
procedure TdwsCompiler.HintUnusedPrivateSymbols;

  procedure HintIfUnused(sym: TSymbol; const msg: String);
  var
    symPos: TSymbolPosition;
  begin
    if FSymbolDictionary.FindSymbolUsage(sym, suReference) = nil then
    begin
      symPos := FSymbolDictionary.FindSymbolUsage(sym, suDeclaration);
      if symPos <> nil then
        FMsgs.AddCompilerHintFmt(symPos.scriptPos, msg, [sym.name]);
    end;
  end;

  procedure DoHintUnusedPrivateSymbols(table: TSymbolTable);
  var
    sym: TSymbol;
    fieldSym: TFieldSymbol;
    methSym: TMethodSymbol;
  begin
    for sym in table do
    begin
      if sym is TStructuredTypeSymbol then
        DoHintUnusedPrivateSymbols(TStructuredTypeSymbol(sym).Members)
      else if sym is TFieldSymbol then
      begin
        fieldSym := TFieldSymbol(sym);
        if fieldSym.visibility = cvPrivate then
          HintIfUnused(fieldSym, CPH_PrivateFieldDeclaredButNotUsed);
      end
      else if sym is TMethodSymbol then
      begin
        methSym := TMethodSymbol(sym);
        if (methSym.visibility = cvPrivate) and (not methSym.IsInterfaced) then
          HintIfUnused(methSym, CPH_PrivateMethodDeclaredButNotUsed);
      end;
    end;
  end;

var
  i: Integer;
  ums: TUnitMainSymbol;
begin
  if coHintsDisabled in options then
    Exit;
  if not(coSymbolDictionary in options) then
    Exit;

  DoHintUnusedPrivateSymbols(CurrentProg.table);
  for i := 0 to CurrentProg.UnitMains.Count - 1 do
  begin
    ums := CurrentProg.UnitMains[i];
    if ums.name = SYS_SYSTEM then
      continue;
    if ums.name = SYS_INTERNAL then
      continue;
    DoHintUnusedPrivateSymbols(ums.table);
    DoHintUnusedPrivateSymbols(ums.ImplementationTable);
  end;
end;

// HintUnusedResult
//
procedure TdwsCompiler.HintUnusedResult(resultSymbol: TDataSymbol);
begin
  if resultSymbol = nil then
    Exit;
  if not(coSymbolDictionary in FOptions) then
    Exit;
  if coHintsDisabled in options then
    Exit;

  if FSymbolDictionary.FindSymbolUsage(resultSymbol, suReference) = nil then
    FMsgs.AddCompilerHint(FTok.hotPos, CPH_ResultNotUsed);
end;

// HintReferenceConstVarParams
//
procedure TdwsCompiler.HintReferenceConstVarParams(funcSym: TFuncSymbol);
var
  param: TSymbol;
  IsVirtual: Boolean;
  paramPos: TSymbolPosition;
begin
  if not(coSymbolDictionary in FOptions) then
    Exit;
  if coHintsDisabled in options then
    Exit;

  IsVirtual := (funcSym is TMethodSymbol) and TMethodSymbol(funcSym).IsVirtual;

  for param in funcSym.Params do
  begin
    if not(param is TByRefParamSymbol) then
      continue;
    if not param.typ.IsClassSymbol then
      continue;

    paramPos := FSymbolDictionary.FindSymbolUsage(param, suDeclaration);
    if paramPos = nil then
      continue;

    if not IsVirtual then
    begin

      if param is TVarParamSymbol then
      begin

        if FSymbolDictionary.FindSymbolUsage(param, suWrite) = nil then
          FMsgs.AddCompilerHintFmt(paramPos.scriptPos,
            CPH_ReferenceTypeParamAsVarButNeverWrittenTo, [param.name],
            hlPedantic);

      end;

    end;

  end;
end;

// ReportNoMemberForType
//
procedure TdwsCompiler.ReportNoMemberForType(const name: String;
  const namePos: TScriptPos; typ: TTypeSymbol; fatal: Boolean = True);
var
  typeDescription: String;
begin
  if typ = nil then
    typeDescription := SYS_VOID
  else if typ.name = '' then
    typeDescription := typ.Caption
  else
    typeDescription := typ.name;
  if fatal then
    FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMemberForType,
      [name, typeDescription])
  else
    FMsgs.AddCompilerErrorFmt(namePos, CPE_UnknownMemberForType,
      [name, typeDescription]);
end;

// ReportNoMemberForType
//
procedure TdwsCompiler.ReportNoMemberForType(const name: String;
  const namePos: TScriptPos; base: TTypedExpr);
var
  typ: TTypeSymbol;
begin
  if base <> nil then
    typ := base.typ
  else
    typ := nil;
  ReportNoMemberForType(name, namePos, typ);
end;

// ReadConnectorSym
//
function TdwsCompiler.ReadConnectorSym(const name: String; baseExpr: TTypedExpr;
  const connectorType: IConnectorType; isWrite: Boolean): TProgramExpr;

  function TryConnectorCall: TConnectorCallExpr;
  var
    argPosArray: TScriptPosArray;
  begin
    // Try to read the call of a connector function
    result := TConnectorCallExpr.Create(FTok.hotPos, Name, baseExpr, isWrite);
    try
      ReadArguments(result.AddArg, ttBLEFT, ttBRIGHT, argPosArray);
      if not result.AssignConnectorSym(CurrentProg, connectorType) then
      begin
        result.Orphan(FCompilerContext);
        result := nil;
      end;
    except
      result.Orphan(FCompilerContext);
      raise;
    end;
  end;

var
  connWrite: TConnectorWriteMemberExpr;
  connRead: TConnectorReadMemberExpr;
  rightExpr: TTypedExpr;
begin
  if FTok.Test(ttALEFT) then
  begin

    result := ReadConnectorArray(name, baseExpr, connectorType, isWrite);

  end
  else if FTok.Test(ttBLEFT) then
  begin

    result := TryConnectorCall;
    if result = nil then
      result := TConstExpr.CreateNull(cNullPos, FCompilerContext.TypVariant);
    // keep compiling

  end
  else if not isWrite then
  begin

    result := TConnectorReadMemberExpr.CreateNew(FTok.hotPos, name, baseExpr,
      connectorType);
    if result = nil then
    begin
      FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_ConnectorMember,
        [name, connectorType.ConnectorCaption]);
    end;

  end
  else if FTok.TestDelete(ttASSIGN) then
  begin

    // An assignment of the form "connector.member := expr" was found
    // and is transformed into "connector.member(expr)"
    try
      rightExpr := ReadExpr;
      connWrite := TConnectorWriteMemberExpr.CreateNew(FCompilerContext,
        FTok.hotPos, name, baseExpr, rightExpr, connectorType);
      baseExpr := nil;
      if connWrite = nil then
      begin
        connWrite.Orphan(FCompilerContext);
        connWrite := nil;
        FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_ConnectorMember,
          [name, connectorType.ConnectorCaption]);
      end;
      result := connWrite;
    except
      OrphanAndNil(baseExpr);
      raise;
    end;

  end
  else
  begin

    // It's possible that we should read a connector member or
    // call a connector function without arguments.
    connRead := TConnectorReadMemberExpr.CreateNew(FTok.hotPos, name, baseExpr,
      connectorType);

    if connRead = nil then
    begin
      // Try to read a connector call
      result := TryConnectorCall;
      if not Assigned(result) then
        FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_ConnectorMember,
          [name, connectorType.ConnectorCaption]);
    end
    else
      result := connRead;

  end;
end;

// ReadConnectorArray
//
function TdwsCompiler.ReadConnectorArray(const name: String;
  var baseExpr: TTypedExpr; const connectorType: IConnectorType;
  isWrite: Boolean): TConnectorCallExpr;
var
  argPosArray: TScriptPosArray;
begin
  result := TConnectorCallExpr.Create(FTok.hotPos, Name, baseExpr,
    isWrite, True);
  try
    baseExpr := nil;
    ReadArguments(result.AddArg, ttALEFT, ttARIGHT, argPosArray);

    if isWrite then
    begin
      if FTok.TestDelete(ttASSIGN) then
        result.AddArg(ReadExpr)
      else
        result.isWrite := False;
    end;

    if not result.AssignConnectorSym(CurrentProg, connectorType) then
      FMsgs.AddCompilerStopFmt(FTok.hotPos, CPE_ConnectorIndex,
        [connectorType.ConnectorCaption]);
  except
    result.Orphan(FCompilerContext);
    raise;
  end;
end;

function TdwsCompiler.ReadStringArray(expr: TDataExpr; isWrite: Boolean)
  : TProgramExpr;
var
  indexExpr, valueExpr: TTypedExpr;
  scriptPos: TScriptPos;
  n: Integer;
begin
  scriptPos := FTok.hotPos;
  indexExpr := ReadExpr;
  try
    if not(indexExpr.IsOfType(FCompilerContext.TypInteger) or
      indexExpr.IsOfType(FCompilerContext.TypVariant)) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_IntegerExpressionExpected);

    if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_ArrayBracketRightExpected);

    if FTok.TestDelete(ttASSIGN) and isWrite then
    begin
      valueExpr := ReadExpr;
      if valueExpr is TConstStringExpr then
      begin
        n := Length(TConstStringExpr(valueExpr).value);
        if n <> 1 then
          FMsgs.AddCompilerErrorFmt(scriptPos,
            RTE_InvalidInputDataSize, [n, 1]);
      end;
      if expr is TStrVarExpr then
      begin
        if (valueExpr is TMagicStringFuncExpr) and
          (TMagicStringFuncExpr(valueExpr).funcSym.name = 'Chr') then
        begin
          result := TVarStringArraySetChrExpr.Create(FCompilerContext,
            scriptPos, expr, indexExpr, TMagicStringFuncExpr(valueExpr)
            .args[0] as TTypedExpr);
          TMagicStringFuncExpr(valueExpr).args.Clear;
          OrphanAndNil(valueExpr);
        end
        else
          result := TVarStringArraySetExpr.Create(FCompilerContext, scriptPos,
            expr, indexExpr, valueExpr);
      end
      else
      begin
        if not expr.IsWritable then
          FMsgs.AddCompilerError(scriptPos, CPE_CantWriteToLeftSide);
        result := TStringArraySetExpr.Create(FCompilerContext, scriptPos, expr,
          indexExpr, valueExpr);
      end;
    end
    else
      result := TStringArrayOpExpr.CreatePos(FCompilerContext, scriptPos, expr,
        indexExpr);
  except
    OrphanAndNil(indexExpr);
    raise;
  end;
end;

// CreateProgram
//
function TdwsCompiler.CreateProgram(const systemTable: ISystemSymbolTable;
  ResultType: TdwsResultType; const stackParams: TStackParameters;
  const mainFileName: String): TdwsMainProgram;
begin
  result := TdwsMainProgram.Create(systemTable, ResultType, stackParams,
    mainFileName);
end;

// ReadEnumeration
//
function TdwsCompiler.ReadEnumeration(const typeName: String;
  aStyle: TEnumerationSymbolStyle): TEnumerationSymbol;
var
  name, deprecatedMsg: String;
  elemSym: TElementSymbol;
  constExpr: TTypedExpr;
  enumInt, enumIntPrev: Int64;
  namePos: TScriptPos;
  isUserDef, overflowed: Boolean;
begin
  result := TEnumerationSymbol.Create(typeName,
    FCompilerContext.TypInteger, aStyle);
  try
    if aStyle = enumFlags then
      enumInt := 1
    else
      enumInt := 0;
    overflowed := False;

    repeat
      // Read a member of the enumeration
      if not FTok.TestDeleteNamePos(name, namePos) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

      if FTok.Test(ttDEPRECATED) then
        deprecatedMsg := ReadDeprecatedMessage(False)
      else
        deprecatedMsg := '';

      // Member has a user defined value
      if FTok.TestDelete(ttEQ) then
      begin
        if aStyle = enumFlags then
          FMsgs.AddCompilerError(FTok.hotPos,
            CPE_FlagEnumerationCantHaveUserValues);
        constExpr := ReadExpr;

        if constExpr <> nil then
        begin
          if not constExpr.IsConstant then
          begin
            OrphanAndNil(constExpr);
            FMsgs.AddCompilerError(FTok.hotPos, CPE_ConstantExpressionExpected);
          end
          else if not FCompilerContext.TypInteger.IsCompatible(constExpr.typ)
          then
          begin
            OrphanAndNil(constExpr);
            FMsgs.AddCompilerError(FTok.hotPos, CPE_IntegerExpressionExpected);
          end;
        end;

        if Assigned(constExpr) then
        begin
          if aStyle <> enumFlags then
            enumInt := constExpr.EvalAsInteger(FExec);
          OrphanAndNil(constExpr);
        end;

        isUserDef := True;
      end
      else
        isUserDef := False;

      // error for duplicate names
      if result.Elements.FindLocal(name) <> nil then
        FMsgs.AddCompilerErrorFmt(namePos, CPE_NameAlreadyExists, [name]);

      // error for overflow
      if overflowed and (not isUserDef) then
        FMsgs.AddCompilerError(namePos, CPE_EnumerationElementOverflow);

      // Create member symbol
      elemSym := TElementSymbol.Create(name, result, enumInt, isUserDef);

      if deprecatedMsg <> '' then
        elemSym.deprecatedMessage := deprecatedMsg;

      enumIntPrev := enumInt;
      if aStyle = enumFlags then
        enumInt := enumInt * 2
      else
        Inc(enumInt);
      overflowed := (enumInt < enumIntPrev);

      // Add member symbol to table and enumeration type
      if aStyle = enumClassic then
      begin
        CurrentProg.table.AddSymbol(elemSym);
        elemSym.IncRefCount;
      end;
      result.AddElement(elemSym);

      // Add member symbol to Symbol Dictionary
      RecordSymbolUse(elemSym, namePos, [suDeclaration]);

    until not FTok.TestDelete(ttCOMMA);

    if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);

  except
    OrphanObject(result);
    raise;
  end;
end;

// ReadUses
//
procedure TdwsCompiler.ReadUses;
var
  names: TSimpleStringList;
  x, y, z, u: Integer;
  rt, rtInterface: TSymbolTable;
  rSym: TSymbol;
  UnitSymbol: TUnitSymbol;
  posArray: TScriptPosArray;
begin
  names := FStringListPool.Acquire;
  try
    if coContextMap in FOptions then
      FSourceContextMap.OpenContext(FTok.hotPos, nil, ttUSES);

    ReadNameList(names, posArray, [nloAllowDots]);

    if coContextMap in FOptions then
      FSourceContextMap.CloseContext(FTok.hotPos);

    u := 0;
    rtInterface := nil;
    if CurrentUnitSymbol <> nil then
    begin
      if UnitSection = secImplementation then
      begin
        rt := CurrentUnitSymbol.ImplementationTable;
        if rt = nil then
          rt := CurrentProg.table;
        rtInterface := CurrentUnitSymbol.InterfaceTable;
        u := 1;
      end
      else
        rt := CurrentUnitSymbol.InterfaceTable;
    end
    else
      rt := CurrentProg.Root.RootTable;
    for x := 0 to names.Count - 1 do
    begin
      if rtInterface <> nil then
      begin
        UnitSymbol := TUnitSymbol(rtInterface.FindLocalOfClass(names[x],
          TUnitSymbol));
        if (UnitSymbol <> nil) and not UnitSymbol.Implicit then
          FMsgs.AddCompilerHintFmt(posArray[x],
            CPH_UnitAlreadyReferredInInterface, [names[x]]);
      end;
      if names.IndexOf(names[x]) < x then
        FMsgs.AddCompilerHintFmt(posArray[x], CPH_UnitAlreadyReferred,
          [names[x]]);

      y := 0;
      z := -1;
      while (y < rt.Count) do
      begin
        rSym := rt[y];
        if rSym.ClassType = TUnitSymbol then
        begin
          UnitSymbol := TUnitSymbol(rSym);
          if (UnitSymbol.Main <> nil) and UnicodeSameText(rSym.name, names[x])
          then
          begin
            if UnitSymbol.Implicit then
              UnitSymbol.Implicit := False;
            z := rt.IndexOfParent(TUnitSymbol(rSym).table);
            if z >= u then
            begin // uses A,B,A,C => uses A,B,C
              rt.MoveParent(z, u);
              // Inc(u);
            end;
            Break;
          end;
        end;
        Inc(y);
      end;
      if z < 0 then
      begin
        UnitSymbol := HandleExplicitDependency(posArray[x], names[x]);
        if (UnitSymbol <> nil) and (UnitSymbol.Main <> nil) then
        begin
          if UnitSymbol.IsDeprecated then
            WarnDeprecatedSymbol(posArray[x], UnitSymbol.Main,
              UnitSymbol.Main.deprecatedMessage);
          z := rt.IndexOfParent(UnitSymbol.table);
          if z > u then
            rt.MoveParent(z, u);
        end;
      end;
      rSym := CurrentProg.UnitMains.Find(names[x]);
      if rSym <> nil then
      begin
        RecordSymbolUse(rSym, posArray[x], [suReference]);
        if CurrentUnitSymbol <> nil then
        begin
          if rSym.ClassType = TUnitSymbol then
            rSym := TUnitSymbol(rSym).Main;
          Assert(rSym.ClassType = TUnitMainSymbol);
          CurrentUnitSymbol.AddDependency(TUnitMainSymbol(rSym));
        end;
      end;
    end;
  finally
    FStringListPool.Release(names);
  end;
end;

// ReadUnitHeader
//
function TdwsCompiler.ReadUnitHeader: TScriptSourceType;
var
  name, part: String;
  namePos, partPos: TScriptPos;
  contextFix: TdwsSourceContext;
begin
  if not FTok.TestDelete(ttUNIT) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_UnitExpected);

  if FTok.TestDelete(ttNAMESPACE) then
    result := stUnitNamespace
  else
    result := stUnit;

  if not FTok.TestDeleteNamePos(name, namePos) then
    FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);

  while FTok.TestDelete(ttDOT) do
  begin
    if not FTok.TestDeleteNamePos(part, partPos) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_NameExpected);
    name := name + '.' + part;
  end;

  if CurrentUnitSymbol = nil then
  begin
    // special case of a unit compiled directly (not through a main program)
    FCurrentSourceUnit := TSourceUnit.Create(name, CurrentProg.Root.RootTable,
      CurrentProg.UnitMains);
    CurrentSourceUnit.symbol.InitializationRank :=
      FCompilerContext.UnitList.Count;
    FCompilerContext.UnitList.Add(FCurrentSourceUnit);
    FCurrentUnitSymbol := CurrentSourceUnit.symbol;
    CheckUnitName(name, namePos, FTok.location);
  end;

  if coContextMap in options then
  begin
    contextFix := FSourceContextMap.Current;
    while (contextFix <> nil) and (contextFix.token <> ttUNIT) do
      contextFix := contextFix.Parent;
    if contextFix <> nil then
    begin
      Assert(contextFix.ParentSym = nil);
      contextFix.ParentSym := CurrentUnitSymbol;
    end;
  end;

  RecordSymbolUse(CurrentUnitSymbol, namePos, [suDeclaration]);
  if namePos.sourceFile.name <> MSG_MainModule then
    CheckUnitName(name, namePos, namePos.sourceFile.name);

  // usually deprecated statement follows after the semi
  // but for units, Delphi wants it before, this supports both forms
  if FTok.Test(ttDEPRECATED) then
    CurrentUnitSymbol.deprecatedMessage := ReadDeprecatedMessage
  else
  begin
    ReadSemiColon;
    CurrentUnitSymbol.deprecatedMessage := ReadDeprecatedMessage;
  end;

  if FTok.TestDelete(ttINTERFACE) then
  begin
    FUnitSection := secInterface;
    if coContextMap in FOptions then
      FSourceContextMap.OpenContext(FTok.hotPos, CurrentUnitSymbol,
        ttINTERFACE);
  end
  else
    FUnitSection := secMixed;
  DoSectionChanged;
end;

// CreateAssign
//
function TdwsCompiler.CreateAssign(const scriptPos: TScriptPos;
  token: TTokenType; left: TDataExpr; right: TTypedExpr): TProgramExpr;
begin
  result := CreateAssignExpr(FCompilerContext, scriptPos, token, left, right);
end;

// CreateArrayLow
//
function TdwsCompiler.CreateArrayLow(const aScriptPos: TScriptPos;
  baseExpr: TProgramExpr; typ: TArraySymbol; captureBase: Boolean): TTypedExpr;
begin
  if typ is TStaticArraySymbol then
    result := (FCompilerContext.CreateConstExpr(TStaticArraySymbol(typ)
      .IndexType, TStaticArraySymbol(typ).LowBound) as TTypedExpr)
  else
  begin
    Assert(typ.ClassType = TDynamicArraySymbol);
    result := FUnifiedConstants.CreateInteger(0)
  end;
  if captureBase then
    OrphanAndNil(baseExpr);
end;

// CreateArrayHigh
//
function TdwsCompiler.CreateArrayHigh(const aScriptPos: TScriptPos;
  baseExpr: TProgramExpr; typ: TArraySymbol; captureBase: Boolean): TTypedExpr;
begin
  Assert(baseExpr <> nil);
  if typ is TOpenArraySymbol then
  begin
    result := TOpenArrayLengthExpr.Create(FCompilerContext, aScriptPos,
      baseExpr as TDataExpr, captureBase);
    TOpenArrayLengthExpr(result).delta := -1;
  end
  else if typ is TDynamicArraySymbol then
  begin
    result := TArrayLengthExpr.Create(FCompilerContext, aScriptPos,
      baseExpr as TTypedExpr, captureBase);
    TArrayLengthExpr(result).delta := -1;
  end
  else
  begin
    if captureBase then
      OrphanAndNil(baseExpr);
    Assert(typ is TStaticArraySymbol);
    result := (FCompilerContext.CreateConstExpr(TStaticArraySymbol(typ)
      .IndexType, TStaticArraySymbol(typ).HighBound) as TTypedExpr)
  end;
end;

// CreateArrayLength
//
function TdwsCompiler.CreateArrayLength(const aScriptPos: TScriptPos;
  baseExpr: TTypedExpr; typ: TArraySymbol): TTypedExpr;
begin
  Assert(baseExpr <> nil);
  if typ is TOpenArraySymbol then
  begin
    result := TOpenArrayLengthExpr.Create(FCompilerContext, aScriptPos,
      baseExpr as TDataExpr, True);
  end
  else if typ is TDynamicArraySymbol then
  begin
    result := TArrayLengthExpr.Create(FCompilerContext, aScriptPos,
      baseExpr, True);
  end
  else
  begin
    Assert(typ is TStaticArraySymbol);
    OrphanAndNil(baseExpr);
    result := FUnifiedConstants.CreateInteger(TStaticArraySymbol(typ)
      .ElementCount);
  end;
end;

// CreateArrayExpr
//
function TdwsCompiler.CreateArrayExpr(const scriptPos: TScriptPos;
  baseExpr: TDataExpr; indexExpr: TTypedExpr): TArrayExpr;
var
  baseType: TArraySymbol;
begin
  baseType := baseExpr.typ as TArraySymbol;

  if baseType is TStaticArraySymbol then
  begin

    if baseType is TOpenArraySymbol then
      result := TOpenArrayExpr.Create(scriptPos, baseExpr, indexExpr,
        TOpenArraySymbol(baseType))
    else if baseExpr is TConstExpr then
    begin
      result := TConstStaticArrayExpr.Create(scriptPos, baseExpr, indexExpr,
        TStaticArraySymbol(baseType));
    end
    else
    begin
      result := TStaticArrayExpr.Create(scriptPos, baseExpr, indexExpr,
        TStaticArraySymbol(baseType));
    end;

  end
  else
  begin

    result := CreateDynamicArrayExpr(scriptPos, baseExpr, indexExpr);

  end;
end;

// CreateDynamicArrayExpr
//
function TdwsCompiler.CreateDynamicArrayExpr(const scriptPos: TScriptPos;
  baseExpr: TTypedExpr; indexExpr: TTypedExpr): TDynamicArrayExpr;
var
  baseType: TDynamicArraySymbol;
begin
  baseType := baseExpr.typ as TDynamicArraySymbol;

  if baseExpr is TObjectVarExpr then
  begin
    result := TDynamicArrayVarExpr.Create(FTok.hotPos, baseExpr, indexExpr,
      baseType);
  end
  else
  begin
    result := TDynamicArrayExpr.Create(FTok.hotPos, baseExpr, indexExpr,
      baseType);
  end;
end;

// EnsureLoopVarExpr
//
function TdwsCompiler.EnsureLoopVarExpr(const loopPos: TScriptPos;
  const loopVarName: String; const loopVarNamePos: TScriptPos;
  var loopVarExpr: TVarExpr; loopVarTyp: TTypeSymbol): TBlockExpr;
var
  loopVarSymbol: TDataSymbol;
begin
  if loopVarExpr <> nil then
  begin

    if (not loopVarExpr.typ.IsCompatible(loopVarTyp) and
      (not loopVarExpr.typ.IsOfType(loopVarTyp))) then
    begin
      IncompatibleTypes(loopVarNamePos, CPE_IncompatibleTypes, loopVarExpr.typ,
        loopVarTyp);
      OrphanAndNil(loopVarExpr);
    end
    else
      Exit(nil);

  end;

  result := TBlockExpr.Create(FCompilerContext, loopPos);
  loopVarSymbol := TScriptDataSymbol.Create(loopVarName, loopVarTyp);
  result.table.AddSymbol(loopVarSymbol);
  RecordSymbolUse(loopVarSymbol, loopVarNamePos, [suDeclaration, suReference,
    suWrite]);
  loopVarExpr := GetVarExpr(loopVarNamePos, loopVarSymbol);
  CurrentProg.initExpr.AddStatement(TInitDataExpr.Create(FCompilerContext,
    loopVarNamePos, loopVarExpr));
  loopVarExpr.IncRefCount;
end;

// CreateTypedOperatorExpr
//
function TdwsCompiler.CreateTypedOperatorExpr(token: TTokenType;
  const scriptPos: TScriptPos; aLeft, aRight: TTypedExpr): TTypedExpr;
var
  opSym: TOperatorSymbol;
  funcExpr: TFuncExprBase;
begin
  result := nil;
  if (aLeft = nil) or (aRight = nil) then
    Exit;
  opSym := ResolveOperatorFor(CurrentProg, token, aLeft.typ, aRight.typ);
  if opSym = nil then
  begin
    result := CreateSetOperatorExpr(token, scriptPos, aLeft, aRight);
    Exit;
  end;

  if opSym.OperatorExprClass <> nil then
  begin
    result := TBinaryOpExprClass(opSym.OperatorExprClass)
      .Create(FCompilerContext, scriptPos, token, aLeft, aRight);
  end
  else if opSym.usesSym <> nil then
  begin
    if opSym.usesSym is TMethodSymbol then
      funcExpr := CreateMethodExpr(FCompilerContext,
        TMethodSymbol(opSym.usesSym), aLeft, rkObjRef, scriptPos, [])
    else
    begin
      funcExpr := GetFuncExpr(opSym.usesSym);
      funcExpr.AddArg(aLeft);
    end;
    funcExpr.AddArg(aRight);
    TypeCheckArguments(FCompilerContext, funcExpr, nil);
    result := funcExpr;
  end;
  if Optimize then
    result := result.OptimizeToTypedExpr(FCompilerContext, scriptPos);
end;

// CreateAssignOperatorExpr
//
function TdwsCompiler.CreateAssignOperatorExpr(token: TTokenType;
  const scriptPos: TScriptPos; exec: TdwsExecution; aLeft: TDataExpr;
  aRight: TTypedExpr): TProgramExpr;
begin
  result := dwsCompilerUtils.CreateAssignExpr(FCompilerContext, scriptPos,
    token, aLeft, aRight);
end;

// CreateSetOperatorExpr
//
function TdwsCompiler.CreateSetOperatorExpr(token: TTokenType;
  const scriptPos: TScriptPos; aLeft, aRight: TTypedExpr): TTypedExpr;
var
  leftData, rightData: TDataExpr;
  convertedData: TConvStaticArrayToSetOfExpr;
  leftTyp, rightTyp: TTypeSymbol;
  setTyp: TSetOfSymbol;
begin
  result := nil;
  if (aLeft.typ = nil) or (aRight.typ = nil) then
    Exit;

  leftTyp := aLeft.typ.UnAliasedType;
  rightTyp := aRight.typ.UnAliasedType;

  convertedData := nil;
  if leftTyp.ClassType = TSetOfSymbol then
  begin
    setTyp := TSetOfSymbol(leftTyp);
    if rightTyp <> leftTyp then
    begin
      if (aRight.ClassType = TArrayConstantExpr) and
        CompilerUtils.CanConvertArrayToSetOf(FCompilerContext, aRight, setTyp)
      then
      begin
        convertedData := TConvStaticArrayToSetOfExpr.Create(aRight.scriptPos,
          TArrayConstantExpr(aRight), setTyp);
        aRight := convertedData;
      end
      else
        Exit;
    end;
  end
  else if rightTyp.ClassType = TSetOfSymbol then
  begin
    setTyp := TSetOfSymbol(rightTyp);
    if (aLeft.ClassType = TArrayConstantExpr) and
      CompilerUtils.CanConvertArrayToSetOf(FCompilerContext, aLeft, setTyp) then
    begin
      convertedData := TConvStaticArrayToSetOfExpr.Create(aLeft.scriptPos,
        TArrayConstantExpr(aLeft), setTyp);
      aLeft := convertedData;
    end
    else
      Exit;
  end
  else
    Exit;

  try
    leftData := aLeft as TDataExpr;
    rightData := aRight as TDataExpr;
    case token of
      ttPLUS:
        result := TSetOfAddExpr.Create(FCompilerContext, scriptPos, token,
          leftData, rightData);
      ttMINUS:
        result := TSetOfSubExpr.Create(FCompilerContext, scriptPos, token,
          leftData, rightData);
      ttTIMES:
        result := TSetOfMultExpr.Create(FCompilerContext, scriptPos, token,
          leftData, rightData);
      ttEQ:
        result := TSetOfEqualExpr.Create(FCompilerContext, scriptPos, ttEQ,
          leftData, rightData);
      ttNOT_EQ:
        result := TNotBoolExpr.Create(FCompilerContext, scriptPos,
          TSetOfEqualExpr.Create(FCompilerContext, scriptPos, ttEQ, leftData,
          rightData));
      ttLESS_EQ:
        result := TSetOfLeftContainedInRightExpr.Create(FCompilerContext,
          scriptPos, ttLESS_EQ, leftData, rightData);
      ttGTR_EQ:
        result := TSetOfLeftContainedInRightExpr.Create(FCompilerContext,
          scriptPos, ttGTR_EQ, rightData, leftData);
    end;
  except
    if convertedData <> nil then
    begin
      convertedData.expr := nil;
      convertedData.Free;
    end;
    raise;
  end;

  if result = nil then
  begin
    if convertedData <> nil then
    begin
      convertedData.expr := nil;
      convertedData.Free;
    end;
  end
  else if Optimize then
    result := result.OptimizeToTypedExpr(FCompilerContext, scriptPos);
end;

// DoSectionChanged
//
procedure TdwsCompiler.DoSectionChanged;
begin
  if Assigned(FOnSectionChanged) then
    FOnSectionChanged(Self);
end;

// DoTokenizerEndSourceFile
//
procedure TdwsCompiler.DoTokenizerEndSourceFile(sourceFile: TSourceFile);
begin
  Inc(FLineCount, FTok.CurrentPos.Line - 2);
  if coContextMap in options then
  begin
    while (FSourceContextMap.Current <> nil) and
      (FSourceContextMap.Current.StartPos.sourceFile = sourceFile) do
    begin
      FSourceContextMap.CloseContext(FTok.CurrentPos);
    end;
  end;
end;

// EnterUnit
//
procedure TdwsCompiler.EnterUnit(srcUnit: TSourceUnit;
  var oldSrcUnit: TSourceUnit);
begin
  oldSrcUnit := FCurrentSourceUnit;
  FCurrentSourceUnit := srcUnit;

  CurrentUnitSymbol.StoreParents;
  FCurrentUnitSymbol := srcUnit.symbol;
  CurrentUnitSymbol.RestoreParents;
end;

// LeaveUnit
//
procedure TdwsCompiler.LeaveUnit(oldSrcUnit: TSourceUnit);
begin
  CurrentUnitSymbol.StoreParents;
  if oldSrcUnit <> nil then
    FCurrentUnitSymbol := oldSrcUnit.symbol
  else
    FCurrentUnitSymbol := nil;
  CurrentUnitSymbol.RestoreParents;
  FCurrentSourceUnit := oldSrcUnit;
end;

// SwitchTokenizerToUnit
//
procedure TdwsCompiler.SwitchTokenizerToUnit(srcUnit: TSourceUnit;
  const sourceCode, sourceLocation: String);
var
  sourceFile: TSourceFile;
  oldUnit: TSourceUnit;
begin
  sourceFile := FMainProg.SourceList.Add(srcUnit.GetUnitName, sourceCode,
    stUnit, sourceLocation);

  EnterUnit(srcUnit, oldUnit);
  CurrentProg.EnterSubTable(CurrentUnitSymbol.table);
  FUnitsFromStack.Push(sourceFile.name);
  try
    ReadScript(sourceFile, stUnit);
  finally
    FUnitsFromStack.Pop;
    LeaveUnit(oldUnit);
    CurrentProg.LeaveSubTable;
  end;
end;

// ReadSpecialFunction
//
function TdwsCompiler.ReadSpecialFunction(const namePos: TScriptPos;
  specialKind: TSpecialKeywordKind): TProgramExpr;

  function EvaluateDefined(argExpr: TTypedExpr): Boolean;
  var
    name: String;
  begin
    argExpr.EvalAsString(FExec, name);
    result := (FTok.ConditionalDefines.value.IndexOf(name) >= 0);
  end;

  function EvaluateDeclared(argExpr: TTypedExpr): Boolean;
  var
    name: String;
  begin
    argExpr.EvalAsString(FExec, name);
    result := (TDeclaredExpr.FindSymbol(CurrentProg.Root.RootTable,
      name) <> nil);
  end;

  function CreateDefault(argTyp: TTypeSymbol): TExprBase;
  var
    data: IDataContext;
  begin
    data := TDataContext.CreateStandalone(argTyp.Size);
    argTyp.InitDataContext(data, 0);
    if argTyp is TBaseSymbol then
      result := FCompilerContext.CreateConstExpr(argTyp, data[0])
    else
      result := TConstExpr.CreateData(namePos, argTyp, data);
  end;

var
  argExpr, msgExpr, operandExpr: TTypedExpr;
  argTyp: TTypeSymbol;
  argPos: TScriptPos;
  amk: TArrayMethodKind;
begin
  msgExpr := nil;

  RecordSymbolUseReference(FCompilerContext.SpecialSymbol(specialKind),
    namePos, False);

  if specialKind = skDebugBreak then
  begin

    argExpr := nil;
    argTyp := nil;
    if FTok.TestDelete(ttBLEFT) then
      if not FTok.TestDelete(ttBRIGHT) then
        FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);

  end
  else
  begin

    if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackLeftExpected);

    FTok.HasTokens; // get token in buffer for correct argPos below
    argPos := FTok.hotPos;
    case specialKind of
      skAssigned:
        argExpr := ReadExpr(FCompilerContext.TypNil);
      skInc, skDec:
        argExpr := ReadTerm(True);
      skLow, skHigh, skDefault:
        argExpr := ReadExpr(FCompilerContext.TypAnyType);
    else
      argExpr := ReadExpr;
    end;
    if argExpr = nil then
      argTyp := nil
    else
    begin
      argTyp := argExpr.typ;
      if argTyp <> nil then
        argTyp := argTyp.UnAliasedType;
    end;

    if not Assigned(argTyp) then
    begin
      OrphanAndNil(argExpr);
      FMsgs.AddCompilerStop(argPos, CPE_InvalidOperands);
    end;

  end;

  try
    result := nil;

    case specialKind of
      skAssert:
        begin
          argExpr := TConvExpr.WrapWithConvCast(FCompilerContext, argPos,
            FCompilerContext.TypBoolean, argExpr, CPE_BooleanExpected);
          if FTok.TestDelete(ttCOMMA) then
          begin
            FTok.HasTokens;
            argPos := FTok.hotPos;
            msgExpr := ReadExpr;
            if (msgExpr = nil) or
              (not msgExpr.IsOfType(FCompilerContext.TypString)) then
              FMsgs.AddCompilerError(argPos, CPE_StringExpected);
          end;
          if coAssertions in FOptions then
          begin
            result := TAssertExpr.Create(namePos, argExpr, msgExpr);
            argExpr := nil;
            msgExpr := nil;
          end
          else
          begin
            result := TNullExpr.Create(namePos);
            OrphanAndNil(argExpr);
            OrphanAndNil(msgExpr);
          end;
        end;
      skAssigned:
        begin
          if argTyp.IsClassSymbol then
            result := TAssignedInstanceExpr.Create(FCompilerContext,
              namePos, argExpr)
          else if argTyp is TInterfaceSymbol then
            result := TAssignedInterfaceExpr.Create(FCompilerContext,
              namePos, argExpr)
          else if argTyp is TClassOfSymbol then
            result := TAssignedMetaClassExpr.Create(FCompilerContext,
              namePos, argExpr)
          else if argTyp.AsFuncSymbol <> nil then
            result := TAssignedFuncPtrExpr.Create(FCompilerContext,
              namePos, argExpr)
          else
          begin
            FMsgs.AddCompilerError(argPos, CPE_InvalidArgumentType);
            OrphanAndNil(argExpr);
          end;
          argExpr := nil;
        end;
      skHigh:
        begin
          if argTyp is TArraySymbol then
          begin
            result := CreateArrayHigh(namePos, argExpr,
              TArraySymbol(argTyp), True);
          end
          else if argTyp is TEnumerationSymbol then
          begin
            OrphanAndNil(argExpr);
            result := TConstIntExpr.Create(namePos, argTyp,
              TEnumerationSymbol(argTyp).HighBound)
          end
          else if argTyp.IsOfType(FCompilerContext.TypString) and
            Assigned(argExpr) then
          begin
            result := TStringLengthExpr.Create(FCompilerContext,
              namePos, argExpr);
          end
          else if argTyp = FCompilerContext.TypInteger then
          begin
            OrphanAndNil(argExpr);
            result := FUnifiedConstants.CreateInteger(High(Int64));
          end
          else
          begin
            FMsgs.AddCompilerError(argPos, CPE_InvalidArgumentType);
            OrphanAndNil(argExpr);
          end;
          argExpr := nil;
        end;
      skInc, skDec, skSucc, skPred:
        begin
          if (specialKind in [skInc, skDec]) and
            not((argExpr is TDataExpr) and TDataExpr(argExpr).IsWritable) then
            FMsgs.AddCompilerErrorFmt(argPos, CPE_ConstVarParam, [0, 'a'])
          else if argTyp.IsOfType(FCompilerContext.TypInteger) or
            (argTyp is TEnumerationSymbol) then
          begin
            if specialKind in [skInc, skDec] then
              WarnForVarUsage(TVarExpr(argExpr), argPos);
            if FTok.TestDelete(ttCOMMA) then
            begin
              operandExpr := ReadExpr;
              if operandExpr = nil then
                FMsgs.AddCompilerError(FTok.hotPos, CPE_IntegerExpected)
              else
              begin
                if operandExpr.IsOfType(FCompilerContext.TypVariant) then
                  operandExpr := TConvVarToIntegerExpr.Create(FCompilerContext,
                    argPos, operandExpr);
                if not operandExpr.IsOfType(FCompilerContext.TypInteger) then
                  FMsgs.AddCompilerError(FTok.hotPos, CPE_IntegerExpected);
              end;
            end
            else
              operandExpr := FUnifiedConstants.CreateInteger(1);
            case specialKind of
              skInc:
                result := TIncVarFuncExpr.Create(FCompilerContext, argPos,
                  argExpr, operandExpr);
              skDec:
                result := TDecVarFuncExpr.Create(FCompilerContext, argPos,
                  argExpr, operandExpr);
              skSucc:
                result := TSuccFuncExpr.Create(FCompilerContext, argPos,
                  argExpr, operandExpr);
              skPred:
                result := TPredFuncExpr.Create(FCompilerContext, argPos,
                  argExpr, operandExpr);
            end;
            argExpr := nil;
          end
          else
            FMsgs.AddCompilerError(argPos, CPE_IntegerExpected);
        end;
      skLength:
        begin
          if argTyp is TArraySymbol then
          begin
            result := CreateArrayLength(namePos, argExpr, TArraySymbol(argTyp));
          end
          else if ((argTyp = FCompilerContext.TypString) or
            (argTyp.IsOfType(FCompilerContext.TypVariant))) and Assigned(argExpr)
          then
          begin
            result := TStringLengthExpr.Create(FCompilerContext,
              namePos, argExpr);
          end
          else
          begin
            FMsgs.AddCompilerError(argPos, CPE_InvalidArgumentType);
            OrphanAndNil(argExpr);
          end;
          argExpr := nil;
        end;
      skLow:
        begin
          if argTyp is TArraySymbol then
          begin
            result := CreateArrayLow(namePos, argExpr,
              TArraySymbol(argTyp), True);
          end
          else
          begin
            OrphanAndNil(argExpr);
            if argTyp is TEnumerationSymbol then
            begin
              result := TConstIntExpr.Create(namePos, argTyp,
                TEnumerationSymbol(argTyp).LowBound);
            end
            else if argTyp = FCompilerContext.TypString then
            begin
              result := FUnifiedConstants.CreateInteger(1);
            end
            else if (argTyp = FCompilerContext.TypInteger) then
            begin
              result := FUnifiedConstants.CreateInteger(Low(Int64));
            end
            else
            begin
              FMsgs.AddCompilerError(argPos, CPE_InvalidArgumentType);
            end;
          end;
          argExpr := nil;
        end;
      skOrd:
        begin
          if argTyp.IsOfType(FCompilerContext.TypInteger) or
            argTyp.InheritsFrom(TEnumerationSymbol) then
          begin
            result := TOrdIntExpr.Create(FCompilerContext, namePos, argExpr);
          end
          else if argTyp = FCompilerContext.TypBoolean then
          begin
            result := TOrdBoolExpr.Create(FCompilerContext, namePos, argExpr);
          end
          else if argTyp = FCompilerContext.TypString then
          begin
            if argExpr is TStringArrayOpExpr then
              result := TOrdIntExpr.Create(FCompilerContext, namePos, argExpr)
            else
              result := TOrdStrExpr.Create(FCompilerContext, namePos, argExpr);
          end
          else if argTyp = FCompilerContext.TypVariant then
          begin
            result := TOrdExpr.Create(FCompilerContext, namePos, argExpr);
          end
          else
          begin
            FMsgs.AddCompilerError(argPos, CPE_InvalidOperands);
            OrphanAndNil(argExpr);
          end;
          argExpr := nil;
        end;
      skDefault:
        begin
          if argTyp <> nil then
            result := CreateTypedDefault(argTyp) as TProgramExpr
          else
          begin
            FMsgs.AddCompilerError(argPos, CPE_TypeExpected);
            result := TConstExpr.CreateNull(namePos,
              FCompilerContext.TypVariant);
          end;
          OrphanAndNil(argExpr);
        end;
      skSizeOf:
        begin
          result := FUnifiedConstants.CreateInteger(argTyp.Size);
          OrphanAndNil(argExpr);
        end;
      skDefined, skDeclared:
        begin
          if FIsSwitch then
          begin
            if not argExpr.IsOfType(FCompilerContext.TypString) then
              FMsgs.AddCompilerError(argPos, CPE_StringExpected);
            if not argExpr.IsConstant then
              FMsgs.AddCompilerStop(argPos, CPE_ConstantExpressionExpected);
            try
              case specialKind of
                skDefined:
                  result := FUnifiedConstants.CreateBoolean
                    (EvaluateDefined(argExpr));
                skDeclared:
                  result := FUnifiedConstants.CreateBoolean
                    (EvaluateDeclared(argExpr));
              end;
            finally
              OrphanAndNil(argExpr);
            end;
          end
          else
          begin
            case specialKind of
              skDefined:
                result := TDefinedExpr.Create(FCompilerContext,
                  namePos, argExpr);
              skDeclared:
                begin
                  if not argExpr.IsOfType(FCompilerContext.TypString) then
                    FMsgs.AddCompilerError(argPos, CPE_StringExpected);
                  result := TDeclaredExpr.Create(FCompilerContext,
                    namePos, argExpr);
                end;
            end;
            argExpr := nil;
          end;
        end;
      skDebugBreak:
        begin
          result := TDebugBreakExpr.Create(namePos);
        end;
      skConditionalDefined:
        begin
          if not argExpr.IsOfType(FCompilerContext.TypString) then
            FMsgs.AddCompilerError(argPos, CPE_StringExpected);
          result := TConditionalDefinedExpr.Create(FCompilerContext,
            namePos, argExpr);
          argExpr := nil;
        end;
      skInclude, skExclude:
        begin
          if not FTok.TestDelete(ttCOMMA) then
            FMsgs.AddCompilerStop(FTok.hotPos, CPE_CommaExpected);
          if specialKind = skInclude then
            amk := amkInclude
          else
            amk := amkExclude;
          result := ReadIncludeExclude(namePos, amk, argExpr, argPos);
        end;
      skSwap:
        begin
          if not((argExpr is TDataExpr) and TDataExpr(argExpr).IsWritable) then
          begin
            FMsgs.AddCompilerError(argPos, CPE_VariableExpected);
            OrphanAndNil(argExpr);
          end;
          if not FTok.TestDelete(ttCOMMA) then
          begin
            FMsgs.AddCompilerError(FTok.hotPos, CPE_CommaExpected);
            msgExpr := nil;
          end
          else
          begin
            FTok.TestName;
            argPos := FTok.hotPos;
            msgExpr := ReadExpr(argTyp);
            if not((msgExpr is TDataExpr) and TDataExpr(msgExpr).IsWritable)
            then
            begin
              FMsgs.AddCompilerError(argPos, CPE_VariableExpected);
              OrphanAndNil(argExpr);
            end
            else if (argExpr <> nil) then
            begin
              if (msgExpr = nil) or
                not(msgExpr.IsOfType(argTyp) and argTyp.IsOfType(msgExpr.typ))
              then
                IncompatibleTypes(namePos, CPE_IncompatibleTypes, argTyp,
                  msgExpr.typ);
            end;
          end;
          result := TSwapExpr.Create(FCompilerContext, namePos,
            TDataExpr(argExpr), TDataExpr(msgExpr));
          argExpr := nil;
          msgExpr := nil;
        end;
    end;

    if argExpr <> nil then
    begin
      OrphanAndNil(argExpr);
    end;
    if result = nil then
    begin
      // fake expression to keep compiling
      case specialKind of
        skDefined, skDeclared, skAssigned:
          result := FUnifiedConstants.CreateBoolean(False);
      else
        result := FUnifiedConstants.CreateInteger(0);
      end;
    end
    else if Optimize then
      result := result.Optimize(FCompilerContext);

    if specialKind <> skDebugBreak then
    begin
      try
        if not FTok.TestDelete(ttBRIGHT) then
          FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);
      except
        OrphanAndNil(result);
        raise;
      end;
    end;

  except
    OrphanAndNil(argExpr);
    OrphanAndNil(msgExpr);
    raise;
  end;
end;

// ReadIncludeExclude
//
function TdwsCompiler.ReadIncludeExclude(const namePos: TScriptPos;
  methodKind: TArrayMethodKind; var argExpr: TTypedExpr;
  const argPos: TScriptPos): TProgramExpr;
var
  operandExpr: TTypedExpr;
  typ: TTypeSymbol;
  setType: TSetOfSymbol;
  operandPos: TScriptPos;
begin
  if not((argExpr is TDataExpr) and TDataExpr(argExpr).IsWritable) then
    FMsgs.AddCompilerError(argPos, CPE_VariableExpected);

  if coSymbolDictionary in CompilerContext.options then
    RecordSymbolUse(TDataExpr(argExpr).DataSymbol, argExpr.scriptPos,
      [suWrite]);

  typ := argExpr.typ.UnAliasedType;
  if typ is TSetOfSymbol then
  begin
    setType := TSetOfSymbol(typ);
    typ := setType.typ;
  end
  else
  begin
    FMsgs.AddCompilerError(argPos, CPE_SetExpected);
    typ := nil;
  end;

  operandPos := FTok.hotPos;
  operandExpr := ReadExpr(typ);
  if (typ <> nil) and (operandExpr <> nil) and not operandExpr.IsOfType(typ)
  then
    IncompatibleTypes(operandPos, CPE_IncompatibleParameterTypes, typ,
      operandExpr.typ);

  if methodKind = amkInclude then
    result := TSetOfIncludeExpr.Create(FCompilerContext, namePos,
      TDataExpr(argExpr), operandExpr)
  else
    result := TSetOfExcludeExpr.Create(FCompilerContext, namePos,
      TDataExpr(argExpr), operandExpr);
  argExpr := nil;
end;

// ReadTypeCast
//
function TdwsCompiler.ReadTypeCast(const namePos: TScriptPos;
  typeSym: TTypeSymbol): TTypedExpr;
var
  argExpr: TTypedExpr;
  argTyp: TTypeSymbol;
  hotPos: TScriptPos;
  connCast: IConnectorCast;
  casterExprClass: TTypedExprClass;
begin
  hotPos := FTok.CurrentPos;
  argExpr := ReadExpr;

  result := nil;
  try
    if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerStop(FTok.hotPos, CPE_BrackRightExpected);

    if argExpr = nil then
    begin

      result := CreateTypedDefault(typeSym);
      Exit;

    end;

    argTyp := argExpr.typ;
    if argTyp <> nil then
      argTyp := argTyp.UnAliasedType;

    casterExprClass := FOperators.FindCaster(typeSym, argTyp);
    if casterExprClass <> nil then
    begin

      if casterExprClass.InheritsFrom(TUnaryOpExpr) then
        result := TUnaryOpExprClass(casterExprClass).Create(FCompilerContext,
          namePos, argExpr)
      else
      begin
        Assert(casterExprClass.InheritsFrom(TUnaryOpDataExpr));
        result := TUnaryOpDataExprClass(casterExprClass)
          .Create(FCompilerContext, namePos, argExpr);
      end;

    end
    else if typeSym.IsOfType(FCompilerContext.TypInteger) then
    begin

      // Cast Integer(...)
      if argTyp is TEnumerationSymbol then
        result := TConvOrdToIntegerExpr.Create(FCompilerContext,
          namePos, argExpr)
      else if argExpr.IsOfType(FCompilerContext.TypBoolean) then
        result := TOrdBoolExpr.Create(FCompilerContext, namePos, argExpr)
      else if argExpr.IsOfType(FCompilerContext.TypInteger) then
        if argExpr.typ <> typeSym then
          result := TConvOrdToIntegerExpr.Create(FCompilerContext,
            namePos, argExpr)
        else
          result := argExpr
      else if argExpr.IsOfType(FCompilerContext.TypFloat) then
        result := TConvVarToIntegerExpr.Create(FCompilerContext,
          namePos, argExpr)
      else if argTyp is TSetOfSymbol then
      begin
        if TSetOfSymbol(argTyp).CountValue > 31 then
          FMsgs.AddCompilerError(hotPos, CPE_SetTooLargeForCastToInteger);
        result := TConvSetOfToIntegerExpr.Create(FCompilerContext,
          namePos, argExpr)
      end
      else
      begin
        if not argExpr.IsOfType(FCompilerContext.TypVariant) then
          FMsgs.AddCompilerError(hotPos, CPE_IntegerCastInvalid);
        result := TConvVarToIntegerExpr.Create(FCompilerContext,
          namePos, argExpr)
      end;
      result.typ := typeSym;

    end
    else if typeSym = FCompilerContext.TypFloat then
    begin

      // Cast Float(...)
      if argExpr.IsOfType(FCompilerContext.TypFloat) then
        result := argExpr
      else
      begin
        if not argExpr.IsOfType(FCompilerContext.TypVariant) then
          FMsgs.AddCompilerError(hotPos, CPE_NumericalExpected);
        result := TConvVarToFloatExpr.Create(FCompilerContext, namePos,
          argExpr);
      end;

    end
    else if typeSym = FCompilerContext.TypString then
    begin

      // Cast String(...)
      if argExpr.IsOfType(FCompilerContext.TypString) then
        result := argExpr
      else
      begin
        if not argExpr.IsOfType(FCompilerContext.TypVariant) then
          FMsgs.AddCompilerError(hotPos, CPE_VariantExpected);
        result := TConvVarToStringExpr.Create(FCompilerContext,
          namePos, argExpr);
      end;

    end
    else if typeSym = FCompilerContext.TypBoolean then
    begin

      // Cast Boolean(...)
      if argExpr.IsOfType(FCompilerContext.TypInteger) then
        result := TConvIntToBoolExpr.Create(FCompilerContext, namePos, argExpr)
      else if argExpr.IsOfType(FCompilerContext.TypFloat) then
        result := TConvFloatToBoolExpr.Create(FCompilerContext,
          namePos, argExpr)
      else if argExpr.IsOfType(FCompilerContext.TypBoolean) then
        result := argExpr
      else
      begin
        if not argExpr.IsOfType(FCompilerContext.TypVariant) then
          FMsgs.AddCompilerError(hotPos, CPE_BooleanOrIntegerExpected);
        result := TConvVarToBoolExpr.Create(FCompilerContext, namePos, argExpr);
      end;

    end
    else if typeSym = FCompilerContext.TypVariant then

      // Cast Variant(...)
      result := TConvVariantExpr.Create(FCompilerContext, namePos, argExpr)

    else if typeSym is TClassOfSymbol then
    begin

      // Cast Class(...)
      if argTyp.IsClassSymbol then
        result := TObjAsClassExpr.Create(FCompilerContext, hotPos,
          argExpr, typeSym)
      else if argTyp is TClassOfSymbol then
        result := TClassAsClassExpr.Create(FCompilerContext, hotPos,
          argExpr, typeSym)
      else
        FMsgs.AddCompilerStop(hotPos, CPE_InvalidOperands);

    end
    else if typeSym is TSetOfSymbol then
    begin

      // Cast Set of ( ... )
      if argExpr.IsOfType(FCompilerContext.TypInteger) then
      begin

        if TSetOfSymbol(typeSym).CountValue > 31 then
          FMsgs.AddCompilerError(hotPos, CPE_SetTooLargeForCastToInteger);
        result := TConvIntegerToSetOfExpr.Create(hotPos, argExpr,
          TSetOfSymbol(typeSym));

      end
      else
        FMsgs.AddCompilerStop(hotPos, CPE_InvalidOperands);

    end
    else if argExpr.typ is TConnectorSymbol then
    begin

      connCast := TConnectorSymbol(argExpr.typ).connectorType.HasCast(typeSym);
      if connCast = nil then
        FMsgs.AddCompilerStop(hotPos, CPE_InvalidOperands);
      result := TConnectorCastExpr.CreateCast(FCompilerContext, namePos,
        argExpr, connCast);
      result.typ := typeSym;

    end
    else
      FMsgs.AddCompilerStop(hotPos, CPE_InvalidTypeCast);

    if Optimize then
      result := result.OptimizeToTypedExpr(FCompilerContext, hotPos);

  except
    OrphanAndNil(argExpr);
    raise;
  end;
end;

// ReadTypeExpr
//
function TdwsCompiler.ReadTypeExpr(const namePos: TScriptPos;
  typeSym: TTypeSymbol; isWrite: Boolean; expecting: TTypeSymbol = nil)
  : TProgramExpr;

  function CreateClassSymbolExpr(typeSym: TTypeSymbol): TConstExpr;
  begin
    result := TConstIntExpr.Create(namePos, typeSym,
      Int64(TClassOfSymbol(typeSym).TypClassSymbol));
  end;

var
  typeExpr: TTypedExpr;
begin
  if typeSym.ClassType = TClassOfSymbol then
    typeExpr := CreateClassSymbolExpr(typeSym)
  else
    typeExpr := TTypeReferenceExpr.Create(typeSym, namePos);

  if FTok.Test(ttDOT) then
    result := ReadSymbol(typeExpr, isWrite, expecting)
  else
  begin
    if (expecting <> FCompilerContext.TypAnyType) and
      not((typeSym.ClassType = TClassSymbol) or
      (typeSym.ClassType = TClassOfSymbol)) then
      FMsgs.AddCompilerError(FTok.hotPos, CPE_BrackLeftExpected);
    result := typeExpr;
  end;
end;

// ReadAttributes
//
procedure TdwsCompiler.ReadAttributes(tokenALEFTAlreadyDeleted: Boolean);
var
  expr: TProgramExpr;
  customAttribute: TClassSymbol;
  hotPos: TScriptPos;
begin
  if not(tokenALEFTAlreadyDeleted or FTok.TestDelete(ttALEFT)) then
    Exit;

  customAttribute := FCompilerContext.systemTable.TypCustomAttribute;

  repeat
    hotPos := FTok.hotPos;
    expr := ReadNew(customAttribute, True);
    if not((expr is TMethodExpr) and
      (TMethodExpr(expr).methSym.Kind = fkConstructor)) then
      FMsgs.AddCompilerError(hotPos, CPE_AttributeConstructorExpected);
    FPendingAttributes.Add(TdwsSymbolAttribute.Create(hotPos,
      TMethodExpr(expr)));
    if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(hotPos, CPE_ArrayBracketRightExpected);
  until not FTok.TestDelete(ttALEFT);
end;

// BagPendingAttributes
//
function TdwsCompiler.BagPendingAttributes: ISymbolAttributesBag;
var
  bag: TSymbolAttributesBag;
begin
  if FPendingAttributes.Count > 0 then
  begin
    bag := TSymbolAttributesBag.Create;
    bag.FAttributes := FPendingAttributes;
    FPendingAttributes := TdwsSymbolAttributes.Create;
    result := bag;
  end
  else
    result := nil;
end;

// AttachBaggedAttributes
//
procedure TdwsCompiler.AttachBaggedAttributes(symbol: TSymbol;
  const bag: ISymbolAttributesBag);
var
  i: Integer;
  attrs: TdwsSymbolAttributes;
  attr: TdwsSymbolAttribute;
begin
  if bag = nil then
    Exit;

  attrs := bag.Attributes;
  for i := 0 to attrs.Count - 1 do
  begin
    attr := attrs[i];
    attr.symbol := symbol;
    FMainProg.Attributes.Add(attr);
  end;
  attrs.ExtractAll;
end;

// CheckNoPendingAttributes
//
procedure TdwsCompiler.CheckNoPendingAttributes;

  procedure ErrorDanglingAttributes;
  var
    i: Integer;
    attr: TdwsSymbolAttribute;
  begin
    for i := 0 to FPendingAttributes.Count - 1 do
    begin
      attr := FPendingAttributes[i];
      FMsgs.AddCompilerError(attr.scriptPos, CPE_DanglingAttribute);
    end;
    FPendingAttributes.Clear;
  end;

begin
  if FPendingAttributes.Count > 0 then
    ErrorDanglingAttributes;
end;

// AddProcHelper
//
procedure TdwsCompiler.AddProcHelper(func: TFuncSymbol);
var
  name: String;
  namePos: TScriptPos;
  param: TParamSymbol;
begin
  if func.Params.Count = 0 then
  begin
    FMsgs.AddCompilerError(FTok.hotPos, CPE_ParamsExpected);
    param := nil;
  end
  else
    param := func.Params[0];

  if not FTok.TestDeleteNamePos(name, namePos) then
    name := func.name;
  ReadSemiColon;

  if param = nil then
    Exit;

  CompilerUtils.AddProcHelper(name, CurrentProg.table, func, CurrentUnitSymbol);
  FCompilerContext.HelperMemberNames.Add(name);
end;

// EnumerateHelpers
//
function TdwsCompiler.EnumerateHelpers(typeSym: TTypeSymbol): THelperSymbols;
begin
  result := THelperSymbols.Create;
  if typeSym.ClassType = THelperSymbol then
    result.Add(THelperSymbol(typeSym))
  else
    CurrentProg.table.EnumerateHelpers(typeSym, result.AddHelper);
end;

// ReadTypeHelper
//
function TdwsCompiler.ReadTypeHelper(expr: TTypedExpr; const name: String;
  const namePos: TScriptPos; expecting: TTypeSymbol; isWrite: Boolean;
  killNameToken: Boolean): TProgramExpr;
var
  i: Integer;
  helper, bestHelper: THelperSymbol;
  helpers: THelperSymbols;
  sym, candidate: TSymbol;
  bestDist, helperDist: Integer;
  meth: TMethodSymbol;
  meta: TStructuredTypeMetaSymbol;
  typeSym: TTypeSymbol;
begin
  result := nil;
  typeSym := expr.typ;
  bestDist := MaxInt;

  helpers := EnumerateHelpers(typeSym);
  try
    if helpers.Count = 0 then
      Exit;

    sym := nil;
    bestHelper := nil;
    for i := 0 to helpers.Count - 1 do
    begin
      helper := helpers[i];
      helperDist := expr.typ.DistanceTo(helper.ForType);
      if helperDist < bestDist then
      begin
        candidate := helper.Members.FindSymbol(name, cvPublic);
        if candidate <> nil then
        begin
          sym := candidate;
          bestDist := helperDist;
          bestHelper := helper;
          if helperDist = 0 then
            Break;
        end;
      end;
    end;

    if sym <> nil then
    begin
      if not(coHintsDisabled in FOptions) then
        CheckMatchingDeclarationCase(name, sym, namePos);

      if killNameToken then
        FTok.KillToken;

      if not sym.IsOverloaded then
      begin
        RecordSymbolUseReference(sym, namePos, False);
        if sym.ClassType = TAliasMethodSymbol then
          RecordSymbolUseImplicitReference(TAliasMethodSymbol(sym).Alias,
            namePos, False);
      end;

      if sym.ClassType = TPropertySymbol then
      begin

        Assert(expr is TTypedExpr);
        result := ReadPropertyExpr(TTypedExpr(expr),
          TPropertySymbol(sym), isWrite);

      end
      else if sym.ClassType = TClassVarSymbol then
      begin

        OrphanAndNil(expr);
        result := GetVarExpr(namePos, TClassVarSymbol(sym));

      end
      else if sym is TConstSymbol then
      begin

        OrphanAndNil(expr);
        result := TConstExpr.CreateTyped(FCompilerContext, sym.typ,
          TConstSymbol(sym));

      end
      else if sym is TMethodSymbol then
      begin

        meth := TMethodSymbol(sym);
        if meth.IsClassMethod then
        begin

          if (bestHelper.ForType is TStructuredTypeSymbol) then
          begin
            meta := TStructuredTypeSymbol(bestHelper.ForType).MetaSymbol;
            if meta <> nil then
            begin
              if expr <> nil then
              begin
                if expr.typ is TStructuredTypeSymbol then
                  expr := TObjToClassTypeExpr.Create(FCompilerContext,
                    namePos, expr)
              end
              else
                expr := TConstIntExpr.Create(namePos, meta,
                  Int64(bestHelper.ForType));
            end
            else
            begin
              OrphanAndNil(expr);
            end;
          end;

        end
        else
        begin

          if typeSym.ClassType <> THelperSymbol then
          begin
            if (expr is TTypeReferenceExpr) or
              (expr.typ is TStructuredTypeMetaSymbol) then
            begin
              FMsgs.AddCompilerError(namePos,
                CPE_ClassMethodOrConstructorExpected);
              // keep compiling
              expr := TConvExpr.Create(FCompilerContext, namePos, expr);
              expr.typ := meth.Params[0].typ;
            end;
          end;

        end;

        if (expr <> nil) and (expr.typ is THelperSymbol) then
        begin
          OrphanAndNil(expr);
        end;

        if meth.IsOverloaded then
          result := ReadMethOverloaded(meth, expr, namePos, expecting)
        else
          result := ReadMethod(meth, expr, namePos, expecting);

      end;
    end;
  finally
    helpers.Free;
  end;
end;

// ReadSelfTypeHelper
//
function TdwsCompiler.ReadSelfTypeHelper(const name: TToken;
  const namePos: TScriptPos; expecting: TTypeSymbol): TProgramExpr;
var
  progMeth: TMethodSymbol;
  structSym: TCompositeTypeSymbol;
  selfExpr: TTypedExpr;
begin
  progMeth := CurrentProg.ContextMethodSymbol;

  if progMeth <> nil then
  begin
    if progMeth.IsStatic then
    begin
      structSym := progMeth.StructSymbol;
      if structSym.MetaSymbol = nil then
        Exit(nil)
      else
        selfExpr := TConstIntExpr.Create(namePos, structSym.MetaSymbol,
          Int64(structSym));
    end
    else if progMeth.selfSym = nil then
      Exit(nil)
    else if progMeth.selfSym.ClassType = TConstByRefParamSymbol then
      selfExpr := GetConstParamExpr(namePos,
        TConstByRefParamSymbol(progMeth.selfSym))
    else
      selfExpr := GetSelfParamExpr(namePos, progMeth.selfSym);
    result := ReadTypeHelper(selfExpr, name.AsString, namePos, expecting,
      False, True);
    if result = nil then
      OrphanAndNil(selfExpr);
  end
  else
    result := nil;
end;

// ------------------
// ------------------ TdwsConfiguration ------------------
// ------------------

// Create
//
constructor TdwsConfiguration.Create(Owner: TComponent);
begin
  inherited Create;
  FOwner := Owner;
  FConnectors := TStringList.Create;
  FScriptPaths := TStringList.Create;
  FConditionals := TStringList.Create;
  FUnits := TIdwsUnitList.Create;
  FUnits.Add(dwsInternalUnit);
  FStackChunkSize := cDefaultStackChunkSize;
  FDefaultResultType := TdwsDefaultResultType.Create(nil);
  FResultType := FDefaultResultType;
  FCompilerOptions := cDefaultCompilerOptions;
  FHintsLevel := hlStrict;
  FMaxRecursionDepth := cDefaultMaxRecursionDepth;
  FMaxExceptionDepth := cDefaultMaxExceptionDepth;
end;

destructor TdwsConfiguration.Destroy;
begin
  inherited;
  FSystemSymbols := nil;
  FConnectors.Free;
  FScriptPaths.Free;
  FConditionals.Free;
  FUnits.Free;
  FDefaultResultType.Free;
end;

procedure TdwsConfiguration.Assign(Source: TPersistent);
begin
  if Source is TdwsConfiguration then
  begin
    FCompilerOptions := TdwsConfiguration(Source).CompilerOptions;
    FMaxDataSize := TdwsConfiguration(Source).MaxDataSize;
    FScriptPaths.Assign(TdwsConfiguration(Source).ScriptPaths);
    FTimeoutMilliseconds := TdwsConfiguration(Source).TimeoutMilliseconds;
    FCompileFileSystem := TdwsConfiguration(Source).CompileFileSystem;
    FRuntimeFileSystem := TdwsConfiguration(Source).RuntimeFileSystem;
  end
  else
    inherited;
end;

// InitSystemTable
//
procedure TdwsConfiguration.InitSystemTable;
var
  clsDelphiException, clsAssertionFailed: TClassSymbol;
  meth: TMethodSymbol;
  fldSym: TFieldSymbol;
  propSym: TPropertySymbol;
  sysTable: TSystemSymbols;
begin
  sysTable := TSystemSymbols.Create(nil);
  FSystemSymbols := sysTable;

  // Create base data types
  sysTable.TypBoolean := TBaseBooleanSymbol.Create;
  sysTable.AddSymbol(sysTable.TypBoolean);

  sysTable.TypInteger := TBaseIntegerSymbol.Create;
  sysTable.AddSymbol(sysTable.TypInteger);
  sysTable.AddSymbol(TDynamicArraySymbol.Create(SYS_ARRAY_OF_INTEGER,
    sysTable.TypInteger, sysTable.TypInteger));

  sysTable.TypFloat := TBaseFloatSymbol.Create;
  sysTable.AddSymbol(sysTable.TypFloat);
  sysTable.AddSymbol(TDynamicArraySymbol.Create(SYS_ARRAY_OF_FLOAT,
    sysTable.TypFloat, sysTable.TypInteger));

  sysTable.TypString := TBaseStringSymbol.Create;
  sysTable.AddSymbol(sysTable.TypString);
  sysTable.AddSymbol(TDynamicArraySymbol.Create(SYS_ARRAY_OF_STRING,
    sysTable.TypString, sysTable.TypInteger));

  if Assigned(FOnCreateBaseVariantSymbol) then
    FOnCreateBaseVariantSymbol(sysTable)
  else
  begin
    sysTable.TypVariant := TBaseVariantSymbol.Create;
    sysTable.AddSymbol(sysTable.TypVariant);
  end;
  if sysTable.TypVariant <> nil then
  begin
    sysTable.AddSymbol(TConstSymbol.CreateValue('Null',
      sysTable.TypVariant, Null));
    sysTable.AddSymbol(TConstSymbol.CreateValue('Unassigned',
      sysTable.TypVariant, Unassigned));
    if sysTable.TypVariant.SupportsEmptyParam then
      sysTable.AddSymbol(TConstSymbol.CreateValue('EmptyParam',
        sysTable.TypVariant, EmptyParam));
    sysTable.AddSymbol(TOpenArraySymbol.Create('array of const',
      sysTable.TypVariant, sysTable.TypInteger));
    sysTable.AddSymbol(TDynamicArraySymbol.Create(SYS_ARRAY_OF_VARIANT,
      sysTable.TypVariant, sysTable.TypInteger));
  end;

  sysTable.TypNil := TNilSymbol.Create;

  sysTable.TypInterface := TInterfaceSymbol.Create(SYS_IINTERFACE, nil);
  sysTable.AddSymbol(sysTable.TypInterface);

  sysTable.TypAnyType := TAnyTypeSymbol.Create(SYS_ANY_TYPE, nil);
  sysTable.AddSymbol(sysTable.TypAnyType);

  sysTable.TypAnyFunc := TAnyFuncSymbol.Create('', fkFunction, 0);
  sysTable.AddSymbol(sysTable.TypAnyFunc);

  // Create "root" class Object
  sysTable.TypObject := TClassSymbol.Create(SYS_OBJECT, nil);
  sysTable.AddSymbol(sysTable.TypObject);

  // Create "almost root" class TObject
  sysTable.TypTObject := TClassSymbol.Create(SYS_TOBJECT, nil);
  sysTable.TypTObject.InheritFrom(sysTable.TypObject);
  sysTable.AddSymbol(sysTable.TypTObject);
  // Add constructor Create
  meth := TMethodSymbol.Create(SYS_TOBJECT_CREATE, fkConstructor,
    sysTable.TypTObject, cvPublic, []);
  meth.Executable := ICallable(TEmptyFunc.Create);
  meth.IsDefault := True;
  sysTable.TypTObject.AddMethod(meth);
  // Add destructor Destroy
  TObjectDestroyMethod.Create(mkDestructor, [maVirtual], SYS_TOBJECT_DESTROY,
    [], '', sysTable.TypTObject, cvPublic, sysTable);
  // Add procedure Free
  TObjectFreeMethod.Create(mkProcedure, [], SYS_TOBJECT_FREE, [], '',
    sysTable.TypTObject, cvPublic, sysTable);
  // Add ClassName method
  TObjectClassNameMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSNAME, [],
    SYS_STRING, sysTable.TypTObject, cvPublic, sysTable);

  // Create "root" metaclass TClass
  sysTable.TypClass := TClassOfSymbol.Create(SYS_TCLASS, sysTable.TypTObject);
  sysTable.AddSymbol(sysTable.TypClass);

  // Add ClassType method
  TObjectClassTypeMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSTYPE, [],
    SYS_TCLASS, sysTable.TypTObject, cvPublic, sysTable);
  // Add ClassParent method
  TObjectClassParentMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSPARENT,
    [], SYS_TCLASS, sysTable.TypTObject, cvPublic, sysTable);

  // Create class Exception
  sysTable.TypException := TClassSymbol.Create(SYS_EXCEPTION, nil);
  sysTable.TypException.InheritFrom(sysTable.TypTObject);
  fldSym := TFieldSymbol.Create(SYS_EXCEPTION_MESSAGE_FIELD, sysTable.TypString,
    cvProtected);
  sysTable.TypException.AddField(fldSym);
  propSym := TPropertySymbol.Create(SYS_EXCEPTION_MESSAGE, sysTable.TypString,
    cvPublic, nil);
  propSym.ReadSym := fldSym;
  propSym.WriteSym := fldSym;
  sysTable.TypException.AddProperty(propSym);
  fldSym := TFieldSymbol.Create(SYS_EXCEPTION_DEBUGGER_FIELD,
    sysTable.TypInteger, cvProtected);
  sysTable.TypException.AddField(fldSym);
  TExceptionCreateMethod.Create(mkConstructor, [], SYS_TOBJECT_CREATE,
    ['Msg', SYS_STRING], '', sysTable.TypException, cvPublic, sysTable);
  TExceptionDestroyMethod.Create(mkDestructor, [maVirtual, maOverride],
    SYS_TOBJECT_DESTROY, [], '', sysTable.TypException, cvPublic, sysTable);
  TExceptionStackTraceMethod.Create(mkFunction, [], SYS_EXCEPTION_STACKTRACE,
    [], SYS_STRING, sysTable.TypException, cvPublic, sysTable);
  sysTable.AddSymbol(sysTable.TypException);

  // Create class EAssertionFailed
  clsAssertionFailed := TClassSymbol.Create(SYS_EASSERTIONFAILED, nil);
  clsAssertionFailed.InheritFrom(sysTable.TypException);
  sysTable.AddSymbol(clsAssertionFailed);

  // Create class EDelphi
  clsDelphiException := TClassSymbol.Create(SYS_EDELPHI, nil);
  clsDelphiException.InheritFrom(sysTable.TypException);
  fldSym := TFieldSymbol.Create(SYS_EDELPHI_EXCEPTIONCLASS_FIELD,
    sysTable.TypString, cvProtected);
  clsDelphiException.AddField(fldSym);
  propSym := TPropertySymbol.Create(SYS_EDELPHI_EXCEPTIONCLASS,
    sysTable.TypString, cvPublic, nil);
  propSym.ReadSym := fldSym;
  propSym.WriteSym := fldSym;
  clsDelphiException.AddProperty(propSym);
  TDelphiExceptionCreateMethod.Create(mkConstructor, [], SYS_TOBJECT_CREATE,
    ['Cls', SYS_STRING, 'Msg', SYS_STRING], '', clsDelphiException, cvPublic,
    sysTable);
  sysTable.AddSymbol(clsDelphiException);

  // Create TCustomAttribute
  sysTable.TypCustomAttribute := TClassSymbol.Create(SYS_TCUSTOMATTRIBUTE, nil);
  sysTable.TypCustomAttribute.InheritFrom(sysTable.TypTObject);
  sysTable.TypCustomAttribute.IsAttribute := True;
  sysTable.AddSymbol(sysTable.TypCustomAttribute);

  // ExceptObj function
  TExceptObjFunc.Create(sysTable, 'ExceptObject', [], SYS_EXCEPTION, []);

  // Runtime parameters
  if sysTable.TypVariant <> nil then
    TParamFunc.Create(sysTable, 'Param', ['Index', SYS_INTEGER],
      SYS_VARIANT, []);
  TParamStrFunc.Create(sysTable, 'ParamStr', ['Index', SYS_INTEGER],
    SYS_STRING, []);
  TParamCountFunc.Create(sysTable, 'ParamCount', [], SYS_INTEGER, []);

  // CompilerVersion
  sysTable.AddSymbol(TConstSymbol.CreateValue(SYS_COMPILER_VERSION,
    sysTable.TypFloat, cCompilerVersion));

  if Assigned(FOnCreateSystemSymbols) then
    FOnCreateSystemSymbols(sysTable);

  sysTable.FOperators := TSystemOperators.Create(sysTable);

  if Assigned(FOnRegisterSystemOperators) then
    FOnRegisterSystemOperators(sysTable, sysTable.FOperators);
end;

// SetFilter
//
procedure TdwsConfiguration.SetFilter(const value: TdwsFilter);
begin
  if FFilter = value then
    Exit;

  if Assigned(FFilter) then
    FFilter.RemoveFreeNotification(FOwner);

  FFilter := value;

  if Assigned(FFilter) then
    FFilter.FreeNotification(FOwner);
end;

// SetResultType
//
procedure TdwsConfiguration.SetResultType(const value: TdwsResultType);
begin
  if Assigned(FResultType) and (FResultType <> FDefaultResultType) then
    FResultType.RemoveFreeNotification(FOwner);

  FResultType := value;

  if Assigned(FResultType) then
    FResultType.FreeNotification(FOwner)
  else
    FResultType := FDefaultResultType;
end;

// SetTimeOut
//
procedure TdwsConfiguration.SetTimeOut(const val: Integer);
begin
  TimeoutMilliseconds := val * 1000;
end;

// SetCompileFileSystem
//
procedure TdwsConfiguration.SetCompileFileSystem
  (const val: TdwsCustomFileSystem);
begin
  if Assigned(FCompileFileSystem) then
    FOwner.RemoveFreeNotification(FCompileFileSystem);

  FCompileFileSystem := val;

  if Assigned(FCompileFileSystem) then
    FOwner.FreeNotification(FCompileFileSystem);
end;

// SetRuntimeFileSystem
//
procedure TdwsConfiguration.SetRuntimeFileSystem
  (const val: TdwsCustomFileSystem);
begin
  if Assigned(FRuntimeFileSystem) then
    FOwner.RemoveFreeNotification(FRuntimeFileSystem);

  FRuntimeFileSystem := val;

  if Assigned(FRuntimeFileSystem) then
    FOwner.FreeNotification(FRuntimeFileSystem);
end;

// Notification
//
procedure TdwsConfiguration.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if AComponent = filter then
      filter := nil
    else if AComponent = ResultType then
      ResultType := nil
    else if AComponent = FLocalizer then
      SetLocalizer(nil)
    else
    begin
      // the same file system can be referred in two roles
      if AComponent = CompileFileSystem then
        CompileFileSystem := nil;
      if AComponent = RuntimeFileSystem then
        RuntimeFileSystem := nil;
    end;
  end;
end;

// DetachSystemTable
//
procedure TdwsConfiguration.DetachSystemTable;
begin
  FSystemSymbols := nil;
end;

// SetScriptPaths
//
procedure TdwsConfiguration.SetScriptPaths(const values: TStrings);
begin
  FScriptPaths.Assign(values);
end;

// SetConditionals
//
procedure TdwsConfiguration.SetConditionals(const val: TStringList);
begin
  FConditionals.Assign(val);
end;

// GetSystemSymbols
//
function TdwsConfiguration.GetSystemSymbols: ISystemSymbols;
begin
  if FSystemSymbols = nil then
    InitSystemTable;
  result := FSystemSymbols;
end;

// SetLocalizer
//
procedure TdwsConfiguration.SetLocalizer(const val: TdwsLocalizerComponent);
begin
  if FLocalizer <> nil then
    FLocalizer.RemoveFreeNotification(FOwner);
  FLocalizer := val;
  if FLocalizer <> nil then
    FLocalizer.FreeNotification(FOwner);
end;

// DoGetLocalizer
//
function TdwsConfiguration.DoGetLocalizer: IdwsLocalizer;
begin
  if FLocalizer <> nil then
    result := FLocalizer.GetLocalizer
  else
    result := nil;
end;

// DoIncludeEx
//
procedure TdwsConfiguration.DoIncludeEx(const scriptName: String;
  var scriptSource, scriptLocation: String);
begin
  if Assigned(FOnIncludeEx) then
    FOnIncludeEx(scriptName, scriptSource, scriptLocation)
  else if Assigned(FOnInclude) then
  begin
    FOnInclude(scriptName, scriptSource);
    scriptLocation := '';
  end;
end;

// DoNeedUnitEx
//
function TdwsConfiguration.DoNeedUnitEx(const unitName: String;
  var unitSource, unitLocation: String): IdwsUnit;
begin
  if Assigned(FOnNeedUnitEx) then
    result := FOnNeedUnitEx(unitName, unitSource, unitLocation)
  else if Assigned(FOnNeedUnit) then
  begin
    result := FOnNeedUnit(unitName, unitSource);
    unitLocation := '';
  end;
end;

// ------------------
// ------------------ TExceptObjFunc ------------------
// ------------------

procedure TExceptObjFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var result: Variant);
begin
  if args.exec.ExceptionObjectStack.Count > 0 then
    VarCopySafe(result, args.exec.ExceptionObjectStack.Peek)
  else
    VarCopySafe(result, IUnknown(nil));
end;

// ------------------
// ------------------ TParamFunc ------------------
// ------------------

procedure TParamFunc.DoEvalAsVariant(const args: TExprBaseListExec;
  var result: Variant);
var
  idx: Integer;
  progExec: TdwsProgramExecution;
begin
  progExec := (args.exec as TdwsProgramExecution);
  idx := args.AsInteger[0];
  if Cardinal(idx) < Cardinal(Length(progExec.Parameters)) then
    VarCopySafe(result, progExec.Parameters[idx])
  else
    VarClearSafe(result);
end;

// ------------------
// ------------------ TParamStrFunc ------------------
// ------------------

procedure TParamStrFunc.DoEvalAsString(const args: TExprBaseListExec;
  var result: String);
var
  idx: Integer;
  progExec: TdwsProgramExecution;
begin
  progExec := (args.exec as TdwsProgramExecution);
  idx := args.AsInteger[0];
  if Cardinal(idx) < Cardinal(Length(progExec.Parameters)) then
    VariantToString(progExec.Parameters[idx], result)
  else
    result := '';
end;

// ------------------
// ------------------ TParamCountFunc ------------------
// ------------------

function TParamCountFunc.DoEvalAsInteger(const args: TExprBaseListExec): Int64;
begin
  result := Length((args.exec as TdwsProgramExecution).Parameters);
end;

// ------------------
// ------------------ TdwsOptimizationMessageList ------------------
// ------------------

// AddMsg
//
procedure TdwsOptimizationMessageList.AddMessage(aMessage: TdwsMessage);
begin
  inherited;
  FCompileMsgs.AddMessage(aMessage);
end;

// ------------------
// ------------------ TdwsCompilerExecution ------------------
// ------------------

// Create
//
constructor TdwsCompilerExecution.Create(const stackParams: TStackParameters;
  compiler: TdwsCompiler);
begin
  inherited Create(stackParams);
  FCompiler := compiler;
  FOptimMsgs := TdwsOptimizationMessageList.Create;
  FOptimMsgs.FCompileMsgs := compiler.FMsgs;
end;

// Destroy
//
destructor TdwsCompilerExecution.Destroy;
begin
  inherited;
  FOptimMsgs.Free;
end;

// GetCallStack
//
function TdwsCompilerExecution.GetCallStack: TdwsExprLocationArray;
begin
  result := nil;
end;

// CallStackLastExpr
//
function TdwsCompilerExecution.CallStackLastExpr: TExprBase;
begin
  result := nil;
end;

// CallStackLastProg
//
function TdwsCompilerExecution.CallStackLastProg: TObject;
begin
  result := FCompiler.CurrentProg;
end;

// CallStackDepth
//
function TdwsCompilerExecution.CallStackDepth: Integer;
begin
  result := 0;
end;

// DebuggerNotifyException
//
procedure TdwsCompilerExecution.DebuggerNotifyException(const exceptObj
  : IScriptObj);
begin
  // nothing
end;

// GetMsgs
//
function TdwsCompilerExecution.GetMsgs: TdwsRuntimeMessageList;
begin
  result := FOptimMsgs;
end;

// ------------------
// ------------------ TdwsCompilerUnitContextStack ------------------
// ------------------

// Destroy
//
destructor TdwsCompilerUnitContextStack.Destroy;
begin
  Clean;
  inherited;
end;

// Clean
//
procedure TdwsCompilerUnitContextStack.Clean;
begin
  while Count > 0 do
  begin
    Peek.Tokenizer.Free;
    Pop;
  end;
end;

// PushContext
//
procedure TdwsCompilerUnitContextStack.PushContext(compiler: TdwsCompiler);
var
  Context: TdwsCompilerUnitContext;
begin
  Context.SourceUnit := compiler.CurrentSourceUnit;
  Context.Tokenizer := compiler.FTok;
  Context.UnitSymbol := compiler.CurrentUnitSymbol;
  Context.Context := compiler.FSourceContextMap.SuspendContext;
  Push(Context);
end;

// PopContext
//
procedure TdwsCompilerUnitContextStack.PopContext(compiler: TdwsCompiler;
  var oldSourceUnit: TSourceUnit);
begin
  compiler.FTok := Peek.Tokenizer;
  compiler.EnterUnit(Peek.SourceUnit, oldSourceUnit);
  compiler.FSourceContextMap.ResumeContext(Peek.Context);
  Pop;
end;

{ TTypeLookupData }

constructor TTypeLookupData.Create(event: TTypeConvertEvent; info: PTypeInfo);
begin
  Self.event := event;
  Self.info := info;
end;

end.
// D2009: if you after a build get:
// [DCC Fatal Error] dwsCompiler.pas: F2051 Unit dwsCompiler was compiled with a different version of dwsUtils.TSimpleObjectObjectHash`2.GetItemHashCode
// Just do a re-compile, and it should go away... - HV
