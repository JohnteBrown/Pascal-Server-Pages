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
unit dwsExprs;

{$I dws.inc}

interface

uses
  System.Classes, System.SysUtils, System.Types, System.SyncObjs,
  dwsSymbols, dwsErrors, dwsUtils, dwsDataContext, dwsExprList,
  dwsStrings, dwsStack, dwsFileSystem, dwsTokenTypes, dwsUnitSymbols,
  dwsXPlatform, dwsInfo, dwsScriptSource, dwsCustomData, dwsSymbolDictionary,
  dwsContextMap, dwsCompilerContext;

type
  TRelOps = (roEqual, roUnEqual, roLess, roLessEqual, roMore, roMoreEqual);

  TRefKind = (rkObjRef, rkIntfRef, rkClassOfRef);

  TTypedExpr = class;
  TNoResultExpr = class;
  TBlockInitExpr = class;
  TBlockFinalExpr = class;
  TTypedExprList = class;
  TdwsProgram = class;
  TdwsMainProgram = class;
  IdwsProgram = interface;
  TdwsProgramExecution = class;
  TdwsProgramExecutionClass = class of TdwsProgramExecution;
  IdwsProgramExecution = interface;
  TFuncExprBase = class;
  TScriptObj = class;
  TSourceConditions = class;
  TSourcePreConditions = class;
  TSourcePostConditions = class;
  TBlockExprBase = class;
  TProgramExpr = class;
  TDataExpr = class;

  TProgramExprList = array [0 .. MaxInt shr 4] of TProgramExpr;
  PProgramExprList = ^TProgramExprList;
  PProgramExpr = ^TProgramExpr;

  TdwsExecutionEvent = procedure(exec: TdwsProgramExecution) of object;

  TFuncFastEvalEvent = function(const args: TExprBaseListExec)
    : Variant of object;

  TMethodFastEvalEvent = function(baseExpr: TTypedExpr;
    const args: TExprBaseListExec): Variant of object;
  TMethodFastEvalNoResultEvent = procedure(baseExpr: TTypedExpr;
    const args: TExprBaseListExec) of object;
  TMethodFastEvalStringEvent = procedure(baseExpr: TTypedExpr;
    const args: TExprBaseListExec; var result: String) of object;
  TMethodFastEvalIntegerEvent = function(baseExpr: TTypedExpr;
    const args: TExprBaseListExec): Int64 of object;
  TMethodFastEvalFloatEvent = function(baseExpr: TTypedExpr;
    const args: TExprBaseListExec): Double of object;
  TMethodFastEvalBooleanEvent = function(baseExpr: TTypedExpr;
    const args: TExprBaseListExec): Boolean of object;
  TMethodFastEvalScriptObjEvent = procedure(baseExpr: TTypedExpr;
    const args: TExprBaseListExec; var result: IScriptObj) of object;

  // Symbol attributes information
  TdwsSymbolAttribute = class(TRefCountedObject)
  private
    FSymbol: TSymbol;
    FScriptPos: TScriptPos;
    FAttributeConstructor: TFuncExprBase;

  protected

  public
    constructor Create(const aScriptPos: TScriptPos;
      aConstructor: TFuncExprBase);
    destructor Destroy; override;

    property Symbol: TSymbol read FSymbol write FSymbol;
    property ScriptPos: TScriptPos read FScriptPos;
    property AttributeConstructor: TFuncExprBase read FAttributeConstructor;
  end;

  TdwsSymbolAttributeArray = array of TdwsSymbolAttribute;

  // Holds all symbol attributes
  TdwsSymbolAttributes = class(TObjectList<TdwsSymbolAttribute>)
  private

  protected

  public
    function AttributesFor(aSymbol: TSymbol): TdwsSymbolAttributeArray;
  end;

  TProgramEvent = procedure(context: TdwsCompilerContext) of object;

  TdwsResultType = class;

  TdwsResult = class
  private
    FResultType: TdwsResultType;

  protected
    property ResultType: TdwsResultType read FResultType;

  public
    constructor Create(ResultType: TdwsResultType); virtual;

    procedure AddString(const str: String); overload; virtual; abstract;
    procedure AddString(const i: Int64); overload; virtual;
    procedure AddCRLF; virtual;
    procedure Clear; virtual; abstract;

    function ToUTF8String: UTF8String; virtual;
    function ToDataString: RawByteString; virtual;
  end;

  TdwsDefaultResult = class(TdwsResult)
  private
    FTextBuilder: TWriteOnlyBlockStream;
    function GetText: String; inline;

  public
    constructor Create(ResultType: TdwsResultType); override;
    destructor Destroy; override;

    procedure AddString(const str: String); override;
    procedure AddString(const i: Int64); override;
    procedure AddCRLF; override;
    procedure Clear; override;

    function ToString: String; override;
    function ToDataString: RawByteString; override;

    property Text: String read GetText;
  end;

  TdwsResultType = class(TComponent)
  private
    FOnInitializeProgram: TProgramEvent;
    FOnFinalizeProgram: TProgramEvent;

  public
    procedure AddResultSymbols(SymbolTable: TSymbolTable); virtual;
    function CreateProgResult: TdwsResult; virtual;

  published
    property OnInitializeProgram: TProgramEvent read FOnInitializeProgram
      write FOnInitializeProgram;
    property OnFinalizeProgram: TProgramEvent read FOnFinalizeProgram
      write FOnFinalizeProgram;
  end;

  TdwsDefaultResultType = class(TdwsResultType)
  public
    procedure AddResultSymbols(SymbolTable: TSymbolTable); override;
    function CreateProgResult: TdwsResult; override;
  end;

  // use methods only when field of a class
  TdwsProgramExecStats = record
  private
    FLock: Pointer;
    FTimeMSec: Int64;
    FCount: Integer;

    procedure Lock; inline;
    procedure Unlock; inline;

  public
    procedure RecordExec(const durationMSec: Integer);
    procedure CopyTo(var dest: TdwsProgramExecStats);

    property Count: Integer read FCount;
    property TimeMSec: Int64 read FTimeMSec;
  end;

  TdwsGuardedExecution = class(TRefCountedObject)
  public
    exec: IdwsProgramExecution;
    TimeOutAt: Int64;
    Next: TdwsGuardedExecution;
    procedure FreeAll;
  end;

  // TdwsGuardianThread
  //
  // Stops the script after given time (Timeout)
  TdwsGuardianThread = class(TdwsThread)
  private
    FEvent: TEvent;
    FExecutions: TdwsGuardedExecution;
    FExecutionsLock: TdwsCriticalSection;

  protected
    procedure Execute; override;

    class var vThread: TdwsGuardianThread;
    class var vExecutionsPool: TdwsGuardedExecution;

  public
    constructor Create;
    destructor Destroy; override;

  const
    PulseIntervalMilliSeconds: Integer = 2500;

    class procedure Initialize; static;
    class procedure Finalize; static;

    class procedure GuardExecution(const exec: IdwsProgramExecution;
      aMilliSecToLive: Integer); static;
    class procedure ForgetExecution(const exec: IdwsProgramExecution); static;
  end;

  IdwsLocalizer = interface(IGetSelf)
    ['{2AFDC297-FF85-43F5-9913-45DE5C1330AB}']
    procedure LocalizeSymbol(aResSymbol: TResourceStringSymbol;
      var result: String);
    procedure LocalizeString(const aString: String; var result: String);
  end;

  TProgramInfo = class;

  IdwsProgramExecution = interface(IdwsExecution)
    ['{3E955C01-E78D-4B5E-9A7E-F78ECDBE1DA8}']
    function GetInfo: TProgramInfo;
    function GetResult: TdwsResult;
    function GetObjectCount: Integer;
    function GetProg: IdwsProgram;
    function HasProgram: Boolean;
    function HasCompileErrors: Boolean;
    function GetLocalizer: IdwsLocalizer;
    procedure SetLocalizer(const loc: IdwsLocalizer);
    function GetExecutionTimedOut: Boolean;

    procedure Execute(aTimeoutMilliSeconds: Integer = 0); overload;
    procedure ExecuteParam(const params: TVariantDynArray;
      aTimeoutMilliSeconds: Integer = 0); overload;
    procedure ExecuteParam(const params: OleVariant;
      aTimeoutMilliSeconds: Integer = 0); overload;

    function BeginProgram: Boolean;
    procedure RunProgram(aTimeoutMilliSeconds: Integer);
    procedure Stop;
    procedure StopForTimeout;
    procedure EndProgram;

    property Prog: IdwsProgram read GetProg;
    property Info: TProgramInfo read GetInfo;
    property result: TdwsResult read GetResult;
    property ObjectCount: Integer read GetObjectCount;
    property Localizer: IdwsLocalizer read GetLocalizer write SetLocalizer;
    property ExecutionTimedOut: Boolean read GetExecutionTimedOut;
  end;

  IdwsProgram = interface(IGetSelf)
    ['{AD513983-F033-44AF-9F2B-9CFFF94B9BB3}']
    function GetMsgs: TdwsMessageList;
    function GetConditionalDefines: IAutoStrings;
    function GetLineCount: Integer;
    function GetTimeStamp: TDateTime;
    function GetExecStats: TdwsProgramExecStats;
    function GetTable: TSymbolTable;
    function GetTimeoutMilliseconds: Integer;
    procedure SetTimeoutMilliseconds(const val: Integer);
    function GetDefaultUserObject: TObject;
    procedure SetDefaultUserObject(const val: TObject);
    function GetSymbolDictionary: TdwsSymbolDictionary;
    function GetSourceContextMap: TdwsSourceContextMap;
    function GetSourceList: TScriptSourceList;
    function GetUnitMains: TUnitMainSymbols;
    function GetProgramObject: TdwsMainProgram;
    procedure SetExecutionsClass(aClass: TdwsProgramExecutionClass);
    function IsEmpty: Boolean;

    function CreateNewExecution: IdwsProgramExecution;
    function BeginNewExecution: IdwsProgramExecution;
    function Execute(aTimeoutMilliSeconds: Integer = 0): IdwsProgramExecution;
    function ExecuteParam(const params: TVariantDynArray;
      aTimeoutMilliSeconds: Integer = 0): IdwsProgramExecution; overload;
    function ExecuteParam(const params: OleVariant;
      aTimeoutMilliSeconds: Integer = 0): IdwsProgramExecution; overload;

    procedure DropMapAndDictionary;

    property Table: TSymbolTable read GetTable;
    property Msgs: TdwsMessageList read GetMsgs;
    property ConditionalDefines: IAutoStrings read GetConditionalDefines;
    property TimeoutMilliseconds: Integer read GetTimeoutMilliseconds
      write SetTimeoutMilliseconds;
    property DefaultUserObject: TObject read GetDefaultUserObject
      write SetDefaultUserObject;

    property SymbolDictionary: TdwsSymbolDictionary read GetSymbolDictionary;
    property SourceContextMap: TdwsSourceContextMap read GetSourceContextMap;
    property SourceList: TScriptSourceList read GetSourceList;
    property UnitMains: TUnitMainSymbols read GetUnitMains;
    property ProgramObject: TdwsMainProgram read GetProgramObject;
    property LineCount: Integer read GetLineCount;
    property TimeStamp: TDateTime read GetTimeStamp;
    property ExecStats: TdwsProgramExecStats read GetExecStats;
    property ExecutionsClass: TdwsProgramExecutionClass
      write SetExecutionsClass;
  end;

  // holds execution context for a script
  TdwsProgramExecution = class(TdwsExecution, IdwsProgramExecution)
  private
    FProg: TdwsMainProgram;
    FCurrentProg: TdwsProgram;
    FCompilerContext: TdwsCompilerContext;

    FFirstObject, FLastObject: TScriptObj;
    FObjectCount: Integer;

    FProgramInfo: TProgramInfo;
    FProgInfoPool: TProgramInfo;

    FResult: TdwsResult;
    FParameters: TData;
    FFileSystem: IdwsFileSystemRW;
    FLocalizer: IdwsLocalizer;
    FRTTIRawAttributes: IScriptDynArray;

    FCustomStates: TdwsCustomStates;
    FCustomInterfaces: TdwsCustomInterfaces;

    FOnExecutionStarted: TdwsExecutionEvent;
    FOnExecutionEnded: TdwsExecutionEvent;

    FRuntimeMsgs: TdwsRuntimeMessageList;

    FDebuggerFieldAddr: Integer;
    FStartTicks: Int64;
    FExecutionTimedOut: Boolean;
{$IFDEF WIN32}
    F8087CW: Cardinal;
{$ENDIF}
  protected
    procedure ReleaseObjects;

    procedure ScriptObjCreated(scriptObj: TScriptObj);
    procedure ScriptObjDestroyed(scriptObj: TScriptObj);
    procedure DestroyScriptObj(const scriptObj: IScriptObj);

    function GetMsgs: TdwsRuntimeMessageList; override;

    function GetCustomStates: TdwsCustomStates;
    function GetCustomInterfaces: TdwsCustomInterfaces;

    // for interface only, script exprs use direct properties
    function GetProg: IdwsProgram;
    function HasProgram: Boolean;
    function HasCompileErrors: Boolean;
    function GetInfo: TProgramInfo;
    function GetResult: TdwsResult;
    function GetObjectCount: Integer;
    function GetLocalizer: IdwsLocalizer;
    procedure SetLocalizer(const val: IdwsLocalizer);
    function GetExecutionTimedOut: Boolean;

    procedure RaiseMaxRecursionReached;
    procedure SetCurrentProg(const val: TdwsProgram); inline;

    procedure RunProgramExpr(expr: TProgramExpr);

  public
    constructor Create(aProgram: TdwsMainProgram;
      const stackParams: TStackParameters); virtual;
    destructor Destroy; override;

    procedure Execute(aTimeoutMilliSeconds: Integer = 0); overload;
    procedure ExecuteParam(const params: TVariantDynArray;
      aTimeoutMilliSeconds: Integer = 0); overload;
    procedure ExecuteParam(const params: OleVariant;
      aTimeoutMilliSeconds: Integer = 0); overload;

    function BeginProgram: Boolean; virtual;
    procedure RunProgram(aTimeoutMilliSeconds: Integer);
    procedure Stop;
    procedure StopForTimeout;
    procedure EndProgram; virtual;

    procedure EnterRecursion(caller: TExprBase);
    procedure LeaveRecursion;

    function CallStackDepth: Integer; override;
    function GetCallStack: TdwsExprLocationArray; override;
    function CallStackLastExpr: TExprBase; override;
    function CallStackLastProg: TObject; override;

    function DebuggerFieldAddr: Integer;
    procedure DebuggerNotifyException(const exceptObj: IScriptObj); override;

    class function CallStackToString(const callStack: TdwsExprLocationArray)
      : String; static;
    procedure RaiseAssertionFailed(fromExpr: TExprBase; const msg: String;
      const ScriptPos: TScriptPos; exec: TdwsExecution);
    procedure RaiseAssertionFailedFmt(fromExpr: TExprBase; const fmt: String;
      const args: array of const; const ScriptPos: TScriptPos;
      exec: TdwsExecution);

    function CreateEDelphiObj(const ClassName: String; const Message: String)
      : IScriptObj;

    procedure EnterExceptionBlock(var exceptObj: IScriptObj); override;

    function AcquireProgramInfo(funcSym: TFuncSymbol): TProgramInfo;
    procedure ReleaseProgramInfo(Info: TProgramInfo);

    procedure LocalizeSymbol(aResSymbol: TResourceStringSymbol;
      var result: String); override;
    procedure LocalizeString(const aString: String;
      var result: String); override;

    function ValidateFileName(const path: String): String; override;
    function FileSystem: IdwsFileSystemRW; override;

    property Prog: TdwsMainProgram read FProg;
    property CurrentProg: TdwsProgram read FCurrentProg write SetCurrentProg;
    property CompilerContext: TdwsCompilerContext read FCompilerContext;
    property ProgramInfo: TProgramInfo read FProgramInfo;

    property Parameters: TData read FParameters;
    property result: TdwsResult read FResult;
    property CustomStates: TdwsCustomStates read GetCustomStates;
    function HasCustomStates: Boolean;
    property CustomInterfaces: TdwsCustomInterfaces read GetCustomInterfaces;
    function HasCustomInterfaces: Boolean;
    property Localizer: IdwsLocalizer read FLocalizer write FLocalizer;
    property RTTIRawAttributes: IScriptDynArray read FRTTIRawAttributes
      write FRTTIRawAttributes;

    property ObjectCount: Integer read FObjectCount;

    property ExecutionTimedOut: Boolean read FExecutionTimedOut;

    property OnExecutionStarted: TdwsExecutionEvent read FOnExecutionStarted
      write FOnExecutionStarted;
    property OnExecutionEnded: TdwsExecutionEvent read FOnExecutionEnded
      write FOnExecutionEnded;
  end;

  TdwsProgramType = (ptScript, ptProgram, ptLibrary);

  // A script executable program
  TdwsProgram = class(TInterfacedSelfObject)
  private
    FExpr: TProgramExpr;
    FInitExpr: TBlockInitExpr;
    FAddrGenerator: TAddrGeneratorRec;
    FCompileMsgs: TdwsCompileMessageList;
    FParent: TdwsProgram;
    FRoot: TdwsMainProgram;
    FRootTable: TProgramSymbolTable;
    FTable: TSymbolTable;
    FUnitMains: TUnitMainSymbols;
    FSubTables: TTightList;

  protected
    function GetLevel: Integer; inline;
    function GetUnitMains: TUnitMainSymbols;
    function GetAddrGeneratorDataSize: Integer; inline;
    function SubExpr(i: Integer): TExprBase;
    function SubExprCount: Integer;

  public
    constructor Create(const systemTable: ISystemSymbolTable);
    destructor Destroy; override;

    function GetGlobalAddr(DataSize: Integer): Integer;
    function GetTempAddr(DataSize: Integer = -1): Integer;
    function FindLocal(const name: String): TSymbol; virtual;

    procedure ResetExprs;

    procedure EnterSubTable(subTable: TSymbolTable);
    procedure LeaveSubTable;
    function SubTableDepth: Integer;
    function subTable(depth: Integer): TSymbolTable;

    function ContextMethodSymbol: TMethodSymbol;

    function IsEmpty: Boolean; virtual;

    property expr: TProgramExpr read FExpr write FExpr;
    property InitExpr: TBlockInitExpr read FInitExpr write FInitExpr;
    property Level: Integer read GetLevel;
    property CompileMsgs: TdwsCompileMessageList read FCompileMsgs
      write FCompileMsgs;
    property Parent: TdwsProgram read FParent;
    property Root: TdwsMainProgram read FRoot write FRoot;
    property DataSize: Integer read GetAddrGeneratorDataSize;

    property RootTable: TProgramSymbolTable read FRootTable;
    property UnitMains: TUnitMainSymbols read FUnitMains;
    property Table: TSymbolTable read FTable;
  end;

  // A script main executable program
  TdwsMainProgram = class(TdwsProgram, IdwsProgram)
  private
    FFinalExpr: TBlockFinalExpr;

    FUnifiedConstants: TObject;

    FResourceStringList: TResourceStringSymbolList;

    FDefaultUserObject: TObject;

    FStackParameters: TStackParameters;

    FResultType: TdwsResultType;
    FRuntimeFileSystem: TdwsCustomFileSystem;
    FExecutions: TTightList;
    FExecutionsLock: TdwsCriticalSection;
    FTimeoutMilliseconds: Integer;

    FSourceContextMap: TdwsSourceContextMap;
    FSymbolDictionary: TdwsSymbolDictionary;
    FAttributes: TdwsSymbolAttributes;

    FSystemTable: ISystemSymbolTable;
    FOperators: TObject;
    FConditionalDefines: IAutoStrings;
    FSourceList: TScriptSourceList;
    FUnitList: TIdwsUnitList;
    FLineCount: Integer;
    FTimeStamp: TDateTime;
    FCompileDurationMSec: Integer;
    FCompiler: TObject;
    FCompilerContext: TdwsCompilerContext;

    FExecStats: TdwsProgramExecStats;

    FMainFileName: String;
    FDefaultEnvironment: IdwsEnvironment;
    FDefaultLocalizer: IdwsLocalizer;
    FOnExecutionStarted: TdwsExecutionEvent;
    FOnExecutionEnded: TdwsExecutionEvent;
    FOnDestroy: TNotifyEvent;

    FExecutionsClass: TdwsProgramExecutionClass;

    FProgramType: TdwsProgramType;

    FTagInterface: IGetSelf;

  protected
    function GetConditionalDefines: IAutoStrings;
    function GetDefaultUserObject: TObject;
    procedure SetDefaultUserObject(const val: TObject);

    function GetSourceList: TScriptSourceList;
    function GetLineCount: Integer;
    function GetTimeStamp: TDateTime;
    function GetExecStats: TdwsProgramExecStats;

    procedure NotifyExecutionDestruction(exec: TdwsProgramExecution);
    procedure RecordExecution(const durationMSec: Integer);

    // for interface only, script exprs use direct properties
    function GetMsgs: TdwsMessageList;
    function GetTable: TSymbolTable;
    function GetTimeoutMilliseconds: Integer;
    procedure SetTimeoutMilliseconds(const val: Integer);
    function GetSymbolDictionary: TdwsSymbolDictionary;
    function GetSourceContextMap: TdwsSourceContextMap;
    function GetProgramObject: TdwsMainProgram;
    procedure SetExecutionsClass(aClass: TdwsProgramExecutionClass);

  public
    constructor Create(const systemTable: ISystemSymbolTable;
      ResultType: TdwsResultType; const stackParameters: TStackParameters;
      const mainFileName: String);
    destructor Destroy; override;

    function CreateNewExecution: IdwsProgramExecution;
    function BeginNewExecution: IdwsProgramExecution;
    function Execute(aTimeoutMilliSeconds: Integer = 0): IdwsProgramExecution;
    function ExecuteParam(const params: TVariantDynArray;
      aTimeoutMilliSeconds: Integer = 0): IdwsProgramExecution; overload;
    function ExecuteParam(const params: OleVariant;
      aTimeoutMilliSeconds: Integer = 0): IdwsProgramExecution; overload;

    function GetSourceFile(const aSourceFile: String): TSourceFile;

    function IsEmpty: Boolean; override;

    function NextStackLevel(Level: Integer): Integer;

    procedure DropMapAndDictionary;

    function CollectAllPublishedSymbols(ignoreImplementationPublished: Boolean)
      : TSimpleSymbolList;

    procedure OrphanObject(obj: TRefCountedObject); inline;

    property FinalExpr: TBlockFinalExpr read FFinalExpr write FFinalExpr;

    procedure AddFinalExpr(expr: TProgramExpr);

    property mainFileName: String read FMainFileName write FMainFileName;
    property TimeoutMilliseconds: Integer read FTimeoutMilliseconds
      write FTimeoutMilliseconds;
    property MaxRecursionDepth: Integer read FStackParameters.MaxRecursionDepth
      write FStackParameters.MaxRecursionDepth;
    property MaxExceptionDepth: Integer read FStackParameters.MaxExceptionDepth
      write FStackParameters.MaxExceptionDepth;
    property MaxDataSize: Integer read FStackParameters.MaxByteSize
      write FStackParameters.MaxByteSize;
    property StackChunkSize: Integer read FStackParameters.ChunkSize
      write FStackParameters.ChunkSize;
    property UnifiedConstants: TObject read FUnifiedConstants;
    property ResourceStringList: TResourceStringSymbolList
      read FResourceStringList write FResourceStringList;
    property RuntimeFileSystem: TdwsCustomFileSystem read FRuntimeFileSystem
      write FRuntimeFileSystem;

    property systemTable: ISystemSymbolTable read FSystemTable;
    property CompilerContext: TdwsCompilerContext read FCompilerContext;
    property Operators: TObject read FOperators write FOperators;
    property ConditionalDefines: IAutoStrings read FConditionalDefines;
    property Compiler: TObject read FCompiler write FCompiler;
    property SourceContextMap: TdwsSourceContextMap read FSourceContextMap;
    property SymbolDictionary: TdwsSymbolDictionary read FSymbolDictionary;
    property Attributes: TdwsSymbolAttributes read FAttributes;
    property SourceList: TScriptSourceList read FSourceList;
    property LineCount: Integer read FLineCount write FLineCount;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property CompileDurationMSec: Integer read FCompileDurationMSec
      write FCompileDurationMSec;
    property ExecStats: TdwsProgramExecStats read FExecStats;

    property DefaultEnvironment: IdwsEnvironment read FDefaultEnvironment
      write FDefaultEnvironment;
    property DefaultLocalizer: IdwsLocalizer read FDefaultLocalizer
      write FDefaultLocalizer;
    property DefaultUserObject: TObject read FDefaultUserObject
      write FDefaultUserObject;

    property OnExecutionStarted: TdwsExecutionEvent read FOnExecutionStarted
      write FOnExecutionStarted;
    property OnExecutionEnded: TdwsExecutionEvent read FOnExecutionEnded
      write FOnExecutionEnded;

    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;

    property ProgramType: TdwsProgramType read FProgramType write FProgramType;
    property TagInterface: IGetSelf read FTagInterface write FTagInterface;
  end;

  // Functions callable from a script program implement this interfaces
  ICallable = interface(IExecutable)
    ['{9C39791A-D3D3-4830-B015-C358621D8867}']
    procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
    procedure CompileTimeCheck(context: TdwsCompilerContext;
      expr: TFuncExprBase);
  end;

  IExternalRoutine = interface(ICallable)
    ['{1595278A-94F5-4B46-8173-C3604C93959C}']
    procedure SetExternalPointer(value: Pointer);
  end;

  TExternalRoutineFactory = function(funcSymbol: TFuncSymbol;
    mainProg: TdwsMainProgram): IExternalRoutine;

  // A script procedure
  TdwsProcedure = class sealed(TdwsProgram, IUnknown, ICallable, IExecutable)
  private
    FFunc: TFuncSymbol;
    FPreConditions: TSourcePreConditions;
    FPostConditions: TSourcePostConditions;

  public
    constructor Create(aParent: TdwsProgram);
    destructor Destroy; override;

    procedure AssignTo(sym: TFuncSymbol);
    procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
    procedure CompileTimeCheck(context: TdwsCompilerContext;
      expr: TFuncExprBase);
    procedure InitSymbol(Symbol: TSymbol; const Msgs: TdwsCompileMessageList);
    procedure InitExpression(expr: TExprBase);
    function FindLocal(const name: String): TSymbol; override;

    function Specialize(const context: ISpecializationContext): IExecutable;

    procedure OptimizeConstAssignments(blockExpr: TBlockExprBase);

    procedure SetBeginPos(const ScriptPos: TScriptPos);

    property func: TFuncSymbol read FFunc write FFunc;

    property PreConditions: TSourcePreConditions read FPreConditions
      write FPreConditions;
    property PostConditions: TSourcePostConditions read FPostConditions
      write FPostConditions;
  end;

  TdwsExceptionContext = class
  private
    FCallStack: TdwsExprLocationArray;

  public
    constructor Create(const aCallStack: TdwsExprLocationArray);
    procedure Skip(n: Integer);
    procedure ReplaceTop(expr: TExprBase);

    property callStack: TdwsExprLocationArray read FCallStack;
  end;

  // Base class of all expressions attached to a program
  TProgramExpr = class(TExprBase)
  protected
    function GetType: TTypeSymbol; virtual;
    function GetBaseType: TTypeSymbol; virtual;

  public
    function Optimize(context: TdwsCompilerContext): TProgramExpr; virtual;
    procedure Orphan(context: TdwsCompilerContext); virtual;

    function Specialize(const context: ISpecializationContext): TExprBase;
      override; final;
    function SpecializeProgramExpr(const context: ISpecializationContext)
      : TProgramExpr; virtual;

    function EvalAsInteger(exec: TdwsExecution): Int64; override;
    function EvalAsBoolean(exec: TdwsExecution): Boolean; override;
    function EvalAsFloat(exec: TdwsExecution): Double; override;
    procedure EvalAsString(exec: TdwsExecution; var result: String); override;
    procedure EvalAsUnicodeString(exec: TdwsExecution;
      var result: UnicodeString); inline;
    procedure EvalAsInterface(exec: TdwsExecution;
      var result: IUnknown); override;
    procedure EvalAsScriptObj(exec: TdwsExecution;
      var result: IScriptObj); override;
    procedure EvalAsScriptObjInterface(exec: TdwsExecution;
      var result: IScriptObjInterface); override;
    procedure EvalAsScriptDynArray(exec: TdwsExecution;
      var result: IScriptDynArray); override;
    procedure EvalAsScriptAssociativeArray(exec: TdwsExecution;
      var result: IScriptAssociativeArray); override;

    procedure AssignValue(exec: TdwsExecution; const value: Variant); override;
    procedure AssignValueAsInteger(exec: TdwsExecution;
      const value: Int64); override;
    procedure AssignValueAsBoolean(exec: TdwsExecution;
      const value: Boolean); override;
    procedure AssignValueAsFloat(exec: TdwsExecution;
      const value: Double); override;
    procedure AssignValueAsString(exec: TdwsExecution;
      const value: String); override;
    procedure AssignValueAsScriptObj(exec: TdwsExecution;
      const value: IScriptObj); override;
    procedure AssignValueAsScriptDynArray(exec: TdwsExecution;
      const value: IScriptDynArray); override;

    procedure RaiseUpperExceeded(exec: TdwsExecution; index: Integer);
    procedure RaiseLowerExceeded(exec: TdwsExecution; index: Integer);
    procedure BoundsCheckFailed(exec: TdwsExecution; index: Integer);

    procedure CheckScriptObject(exec: TdwsExecution;
      const scriptObj: IScriptObj);

    function ScriptLocation(Prog: TObject): String; override;

    procedure EnumerateSteppableExprs(const callback: TExprBaseProc); virtual;

    function InterruptsFlow: Boolean; virtual;

    property Typ: TTypeSymbol read GetType;
    property BaseType: TTypeSymbol read GetBaseType;
  end;

  TProgramExprClass = class of TProgramExpr;

  // Base class of all typed expressions
  TTypedExpr = class(TProgramExpr)
  protected
    FTyp: TTypeSymbol;

    function GetType: TTypeSymbol; override;
    function GetBaseType: TTypeSymbol; override;

  public
    function OptimizeToTypedExpr(context: TdwsCompilerContext;
      const hotPos: TScriptPos): TTypedExpr;
    function OptimizeToFloatConstant(context: TdwsCompilerContext): TTypedExpr;

    function SpecializeProgramExpr(const context: ISpecializationContext)
      : TProgramExpr; override; final;
    function SpecializeTypedExpr(const context: ISpecializationContext)
      : TTypedExpr; virtual;
    function SpecializeBooleanExpr(const context: ISpecializationContext)
      : TTypedExpr;
    function SpecializeIntegerExpr(const context: ISpecializationContext)
      : TTypedExpr;

    function ScriptPos: TScriptPos; override;

    procedure CheckInterface(exec: TdwsExecution; const scriptObj: IScriptObj);
      overload; inline;
    procedure CheckInterface(exec: TdwsExecution;
      const intf: IScriptObjInterface); overload; inline;
    procedure RaiseInterfaceIsNil(exec: TdwsExecution);

    function IsOfType(typSym: TTypeSymbol): Boolean;
    function IsGeneric: Boolean; virtual;
    function IsWritable: Boolean; virtual;
    function AssignsAsDataExpr: Boolean; virtual;

    function SameDataExpr(expr: TTypedExpr): Boolean; virtual;

    property Typ: TTypeSymbol read FTyp write FTyp;
  end;

  TTypedExprClass = class of TTypedExpr;

  // hosts a type reference
  TTypeReferenceExpr = class sealed(TTypedExpr)
  private
    FScriptPos: TScriptPos;

  public
    constructor Create(aTyp: TTypeSymbol; const aScriptPos: TScriptPos);

    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    function ScriptPos: TScriptPos; override;
  end;

  // base class of expressions that return no result
  TNoResultExpr = class(TProgramExpr)
  protected
    FScriptPos: TScriptPos;

  public
    constructor Create(const aPos: TScriptPos);

    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    procedure EvalNoResult(exec: TdwsExecution); override;

    function ScriptPos: TScriptPos; override;
    procedure SetScriptPos(const aPos: TScriptPos);
  end;

  // Does nothing! E. g.: "for x := 1 to 10 do {TNullExpr};"
  TNullExpr = class(TNoResultExpr)
  public
    procedure EvalNoResult(exec: TdwsExecution); override;
    function SpecializeProgramExpr(const context: ISpecializationContext)
      : TProgramExpr; override;
  end;

  // invalid statement
  TErrorExpr = class sealed(TNullExpr)
  end;

  // invalid expression
  TErrorValueExpr = class sealed(TTypedExpr)
  public
    constructor Create(aTyp: TAnyTypeSymbol);
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // statement; statement; statement;
  TBlockExprBase = class(TNoResultExpr)
  protected
    FStatements: PProgramExprList;
    FCount: Integer;

    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

    procedure SpecializeTable(const context: ISpecializationContext;
      destination: TBlockExprBase); virtual;

  public
    destructor Destroy; override;
    procedure Orphan(context: TdwsCompilerContext); override;

    procedure AddStatement(expr: TProgramExpr);
    procedure ReplaceStatement(index: Integer; expr: TProgramExpr);
    function ExtractStatement(index: Integer): TProgramExpr;

    function SpecializeProgramExpr(const context: ISpecializationContext)
      : TProgramExpr; override;
    procedure EnumerateSteppableExprs(const callback: TExprBaseProc); override;

    function Taxonomy: TExprBaseTaxonomy; override;

    property StatementCount: Integer read FCount;
  end;

  // statement; statement; statement;
  TBlockRawExpr = class(TBlockExprBase)
  public
    procedure EvalNoResult(exec: TdwsExecution); override;
  end;

  // var initialization + finalization block
  TBlockInitExpr = class sealed(TBlockRawExpr)
  end;

  // finalization block
  TBlockFinalExpr = class sealed(TBlockRawExpr)
  end;

  // Encapsulates data
  TDataExpr = class(TTypedExpr)
  private
    FScriptPos: TScriptPos;

  protected
    function GetDataPtrFunc(exec: TdwsExecution): IDataContext; inline;

  public
    constructor Create(const aScriptPos: TScriptPos; aTyp: TTypeSymbol);

    procedure AssignData(exec: TdwsExecution;
      const source: IDataContext); virtual;
    procedure AssignDataExpr(exec: TdwsExecution; DataExpr: TDataExpr); virtual;
    procedure AssignExpr(exec: TdwsExecution; expr: TTypedExpr); virtual;
    procedure AssignValue(exec: TdwsExecution; const value: Variant); override;

    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;

    function IsWritable: Boolean; override;
    function IsExternal: Boolean; virtual;
    function AssignsAsDataExpr: Boolean; override;

    function DataSymbol: TDataSymbol; virtual;

    procedure GetDataPtr(exec: TdwsExecution; var result: IDataContext);
      virtual; abstract;
    procedure GetRelativeDataPtr(exec: TdwsExecution;
      var result: IDataContext); virtual;

    function ScriptPos: TScriptPos; override; final;

    property DataPtr[exec: TdwsExecution]: IDataContext read GetDataPtrFunc;

    function IncValue(exec: TdwsExecution; delta: Int64): Int64; virtual;

    function SpecializeTypedExpr(const context: ISpecializationContext)
      : TTypedExpr; override; final;
    function SpecializeDataExpr(const context: ISpecializationContext)
      : TDataExpr; virtual;
  end;

  // TExternalFuncHandler
  //
  TExternalFuncHandler = class(TInterfacedSelfObject, IUnknown, ICallable,
    IExecutable)
  public
    procedure InitSymbol(Symbol: TSymbol; const Msgs: TdwsCompileMessageList);
    procedure InitExpression(expr: TExprBase);
    procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
    procedure CompileTimeCheck(context: TdwsCompilerContext;
      expr: TFuncExprBase);
    function SubExpr(i: Integer): TExprBase;
    function SubExprCount: Integer;
    function Specialize(const context: ISpecializationContext): IExecutable;
  end;

  EdwsExternalFuncHandler = class(Exception);

  TCreateFunctionOption = (cfoForceStatic, cfoInheritedCall);
  TCreateFunctionOptions = set of TCreateFunctionOption;

  // TFuncExprBase
  //
  TFuncExprBase = class(TDataExpr)
  private
    FFunc: TFuncSymbol;
    FParamSize: Integer;

  protected
    FArgs: TExprBaseListRec;
    FResultAddr: Integer;

    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

    function GetIsConstant: Boolean; override;

    property ParamSize: Integer read FParamSize;

  public
    constructor Create(const aScriptPos: TScriptPos; aFunc: TFuncSymbol);
    destructor Destroy; override;
    procedure Orphan(context: TdwsCompilerContext); override;

    procedure AddArg(arg: TTypedExpr);
    procedure ClearArgs;
    function ExpectedArg: TParamSymbol; virtual; abstract;
    function GetArgType(idx: Integer): TTypeSymbol;
    function ArgIsOfType(idx: Integer; aTyp: TTypeSymbol): Boolean;
    function Optimize(context: TdwsCompilerContext): TProgramExpr; override;
    procedure CompileTimeCheck(context: TdwsCompilerContext); virtual;

    procedure Initialize(Compiler: TdwsCompilerContext); virtual;

    procedure InitializeResultAddr(Prog: TdwsProgram);
    procedure SetResultAddr(aResultAddr: Integer); inline;
    property ResultAddr: Integer read FResultAddr write FResultAddr;

    function ChangeFuncSymbol(context: TdwsCompilerContext;
      newFuncSym: TFuncSymbol; options: TCreateFunctionOptions)
      : TFuncExprBase; virtual;

    procedure GetDataPtr(exec: TdwsExecution;
      var result: IDataContext); override;

    property funcSym: TFuncSymbol read FFunc;
    property args: TExprBaseListRec read FArgs write FArgs;
  end;

  TFuncExprOverloadsHelper = class
  private
    FExpr: TFuncExprBase;
    FOverloads: TFuncSymbolList;

  public
    constructor Create(expr: TFuncExprBase; overloads: TFuncSymbolList);
    function ExpectedArg: TParamSymbol;
  end;

  TPushOperatorType = (potUnknown, potAddr, potPassAddr, potTempAddr,
    potTempData, potTempArrayAddr, potTempArray, potResult, potResultInteger,
    potResultFloat, potResultBoolean, potResultString, potResultConstString,
    potData, potConstData, potArrayExpr, potLazy, potInitResult);
  PPushOperator = ^TPushOperator;

  TPushOperator = packed record
    FStackAddr: Integer;
    FArgExpr: TTypedExpr;
    FTypeParamSym: TSymbol; // TSymbol / TPushOperatorType union

    procedure InitPushAddr(addr: Integer; argExpr: TTypedExpr);
    procedure InitPushTempAddr(stackAddr: Integer; argExpr: TTypedExpr);
    procedure InitPushTempArrayAddr(stackAddr: Integer; argExpr: TTypedExpr);
    procedure InitPushTempArray(stackAddr: Integer; argExpr: TTypedExpr);
    procedure InitPushArrayExpr(stackAddr: Integer; argExpr: TTypedExpr);
    procedure InitPushResult(context: TdwsCompilerContext; stackAddr: Integer;
      argExpr: TTypedExpr);
    procedure InitPushData(stackAddr: Integer; argExpr: TTypedExpr;
      ParamSym: TSymbol);
    procedure InitPushInitResult(stackAddr: Integer; argExpr: TTypedExpr);
    procedure InitPushLazy(stackAddr: Integer; argExpr: TTypedExpr);

    procedure Execute(exec: TdwsExecution); // inline;

    procedure ExecuteAddr(exec: TdwsExecution);
    procedure ExecutePassAddr(exec: TdwsExecution);
    procedure ExecuteTempAddr(exec: TdwsExecution);
    procedure ExecuteTempData(exec: TdwsExecution);
    procedure ExecuteTempArrayAddr(exec: TdwsExecution);
    procedure ExecuteTempArray(exec: TdwsExecution);
    procedure ExecuteArrayExpr(exec: TdwsExecution);
    procedure ExecuteResult(exec: TdwsExecution);
    procedure ExecuteResultBoolean(exec: TdwsExecution);
    procedure ExecuteResultInteger(exec: TdwsExecution);
    procedure ExecuteResultFloat(exec: TdwsExecution);
    procedure ExecuteResultString(exec: TdwsExecution);
    procedure ExecuteResultConstString(exec: TdwsExecution);
    procedure ExecuteData(exec: TdwsExecution);
    procedure ExecuteConstData(exec: TdwsExecution);
    procedure ExecuteInitResult(exec: TdwsExecution);
    procedure ExecuteLazy(exec: TdwsExecution);
  end;

  TPushOperatorArray = packed array [0 .. MaxInt shr 5] of TPushOperator;
  PPushOperatorArray = ^TPushOperatorArray;

  // Function call: func(arg0, arg1, ...);
  TFuncExpr = class(TFuncExprBase)
  private
    FPushExprs: PPushOperatorArray;
    FCallerID: TFuncExpr;
    FPushExprsCount: SmallInt;
    FLevel: SmallInt;
    FResultVarType: TVarType;

  protected
    procedure StaticPostCall(exec: TdwsExecution; var result: Variant);
    procedure StaticPostCallInteger(exec: TdwsExecution; var result: Int64);
    procedure StaticPostCallFloat(exec: TdwsExecution; var result: Double);
    procedure StaticPostCallString(exec: TdwsExecution; var result: String);

    procedure EvalPushExprs(exec: TdwsExecution); inline;

    procedure DoEvalCall(exec: TdwsExecution; func: TFuncSymbol);

  public
    constructor Create(context: TdwsCompilerContext;
      const ScriptPos: TScriptPos; func: TFuncSymbol);
    destructor Destroy; override;

    function ExpectedArg: TParamSymbol; override;

    procedure AddPushExprs(context: TdwsCompilerContext);

    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;

    procedure Initialize(context: TdwsCompilerContext); override;
    function IsWritable: Boolean; override;
    procedure CompileTimeCheck(context: TdwsCompilerContext); override;

    function FuncSymQualifiedName: String; override;

    property CallerID: TFuncExpr read FCallerID write FCallerID;
    property Level: SmallInt read FLevel write FLevel;
  end;

  // A simple function/procedure (not a method, not a function pointer);
  TFuncSimpleExpr = class(TFuncExpr)
  public
    procedure EvalNoResult(exec: TdwsExecution); override;
    function EvalAsInteger(exec: TdwsExecution): Int64; override;
    function EvalAsFloat(exec: TdwsExecution): Double; override;
    procedure EvalAsString(exec: TdwsExecution; var result: String); override;
  end;

  IFuncPointer = interface
    function GetFuncExpr: TFuncExprBase;
    function SameFunc(const v: Variant): Boolean;
    procedure EvalNoResult(exec: TdwsExecution; caller: TFuncExpr);
    procedure EvalAsVariant(exec: TdwsExecution; caller: TFuncExpr;
      var result: Variant);
    function EvalAsInteger(exec: TdwsExecution; caller: TFuncExpr): Int64;
    function EvalAsBoolean(exec: TdwsExecution; caller: TFuncExpr): Boolean;
    procedure EvalAsString(exec: TdwsExecution; caller: TFuncExpr;
      var result: String);
    function EvalDataPtr(exec: TdwsExecution; caller: TFuncExpr;
      ResultAddr: Integer): IDataContext;
  end;

  // Encapsulates a function or method pointer
  TFuncPointer = class(TInterfacedObject, IUnknown, IFuncPointer)
  private
    FFuncExpr: TFuncExprBase;
    FEvalAsMagic: Boolean;

  protected
    procedure EvalMagicAsVariant(exec: TdwsExecution; caller: TFuncExpr;
      var result: Variant);
    procedure EvalMagicAsString(exec: TdwsExecution; caller: TFuncExpr;
      var result: String);
    function EvalMagicAsInteger(exec: TdwsExecution; caller: TFuncExpr): Int64;

    procedure EvalFuncAsVariant(exec: TdwsExecution; caller: TFuncExpr;
      var result: Variant);
    procedure EvalFuncAsString(exec: TdwsExecution; caller: TFuncExpr;
      var result: String);
    function EvalFuncAsInteger(exec: TdwsExecution; caller: TFuncExpr): Integer;

  public
    constructor Create(exec: TdwsExecution; funcExpr: TFuncExprBase);
    destructor Destroy; override;

    function GetFuncExpr: TFuncExprBase;
    function SameFunc(const v: Variant): Boolean;

    procedure EvalNoResult(exec: TdwsExecution; caller: TFuncExpr);
    procedure EvalAsVariant(exec: TdwsExecution; caller: TFuncExpr;
      var result: Variant);
    function EvalAsInteger(exec: TdwsExecution; caller: TFuncExpr): Int64;
    function EvalAsBoolean(exec: TdwsExecution; caller: TFuncExpr): Boolean;
    procedure EvalAsString(exec: TdwsExecution; caller: TFuncExpr;
      var result: String);
    function EvalDataPtr(exec: TdwsExecution; caller: TFuncExpr;
      ResultAddr: Integer): IDataContext;
  end;

  // returns an IFuncPointer to the FuncExpr
  TAnonymousFuncRefExpr = class(TDataExpr)
  private
    FFuncExpr: TFuncExprBase;

  protected
    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

  public
    constructor Create(context: TdwsCompilerContext; funcExpr: TFuncExprBase);
    destructor Destroy; override;

    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;

    function Extract: TFuncExprBase; // also a destructor

    procedure GetDataPtr(exec: TdwsExecution;
      var result: IDataContext); override;

    property funcExpr: TFuncExprBase read FFuncExpr write FFuncExpr;
  end;

  TFuncRefExpr = class(TAnonymousFuncRefExpr)
  end;

  TFuncPtrExpr = class sealed(TFuncExpr)
  private
    FCodeExpr: TTypedExpr;

  protected
    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

    function GetIsConstant: Boolean; override;

  public
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; codeExpr: TTypedExpr);
    destructor Destroy; override;

    procedure EvalAsFuncPointer(exec: TdwsExecution;
      var result: IFuncPointer); inline;

    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    procedure EvalNoResult(exec: TdwsExecution); override;

    function Extract: TTypedExpr; // also a destructor

    property codeExpr: TTypedExpr read FCodeExpr write FCodeExpr;
  end;

  TMethodObjExpr = class(TDataExpr)
  private
    FBaseExpr: TDataExpr;

  public
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; baseExpr: TDataExpr);
    procedure GetDataPtr(exec: TdwsExecution;
      var result: IDataContext); override;
  end;

  // Temporary holder for overload resolution
  TOverloadedExpr = class sealed(TFuncExpr)
  private
    FBaseExpr: TTypedExpr;

  protected
    procedure TransferArguments(context: TdwsCompilerContext;
      dest: TFuncExprBase);

  public
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; aFunc: TFuncSymbol; aBaseExpr: TTypedExpr);
    destructor Destroy; override;

    function ChangeFuncSymbol(context: TdwsCompilerContext;
      newFuncSym: TFuncSymbol; options: TCreateFunctionOptions)
      : TFuncExprBase; override;

    property baseExpr: TTypedExpr read FBaseExpr;
  end;

  TSourceCondition = class(TInterfacedSelfObject, IBooleanEvalable,
    IStringEvalable)
  private
    FScriptPos: TScriptPos;
    FTest: TTypedExpr;
    FMsg: TTypedExpr;

  public
    constructor Create(const aScriptPos: TScriptPos; aTest, aMsg: TTypedExpr);
    destructor Destroy; override;

    procedure InitSymbol(Symbol: TSymbol; const Msgs: TdwsCompileMessageList);
    procedure InitExpression(expr: TExprBase);
    function SubExpr(i: Integer): TExprBase;
    function SubExprCount: Integer;

    function Specialize(const context: ISpecializationContext): IExecutable;

    function EvalAsBoolean(exec: TdwsExecution): Boolean;
    procedure EvalAsString(exec: TdwsExecution; var result: String);

    property ScriptPos: TScriptPos read FScriptPos write FScriptPos;
    property Test: TTypedExpr read FTest;
    property msg: TTypedExpr read FMsg write FMsg;
  end;

  TSourceConditions = class
  private
    FProg: TdwsProcedure;
    FItems: TTightList;
    FAncestor: TSourceConditions;

    function GetConditions(idx: Integer): TSourceCondition;

  public
    constructor Create(aProg: TdwsProcedure);
    destructor Destroy; override;

    procedure AddCondition(condition: TSourceCondition);

    procedure RaiseConditionFailed(exec: TdwsExecution; funcSym: TFuncSymbol;
      const ScriptPos: TScriptPos; const msg: IStringEvalable);
      virtual; abstract;
    function Test(exec: TdwsExecution): TSourceCondition;
    procedure EvalNoResult(exec: TdwsExecution); virtual;

    property Ancestor: TSourceConditions read FAncestor write FAncestor;
    property Conditions[idx: Integer]: TSourceCondition
      read GetConditions; default;
    property Count: Integer read FItems.FCount;
  end;

  TSourceConditionsClass = class of TSourceConditions;

  TSourcePreConditions = class(TSourceConditions)
  public
    procedure RaiseConditionFailed(exec: TdwsExecution; funcSym: TFuncSymbol;
      const ScriptPos: TScriptPos; const msg: IStringEvalable); override;
  end;

  TSourcePostConditions = class(TSourceConditions)
  public
    procedure RaiseConditionFailed(exec: TdwsExecution; funcSym: TFuncSymbol;
      const ScriptPos: TScriptPos; const msg: IStringEvalable); override;
  end;

  TSourceMethodPreConditions = class(TSourcePreConditions)
  public
    procedure EvalNoResult(exec: TdwsExecution); override;
  end;

  TSourceMethodPostConditions = class(TSourcePostConditions)
  public
    procedure EvalNoResult(exec: TdwsExecution); override;
  end;

  TUnaryOpDataExpr = class(TDataExpr)
  protected
    FExpr: TTypedExpr;

    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

    function GetIsConstant: Boolean; override;

  public
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr); virtual;
    destructor Destroy; override;

    property expr: TTypedExpr read FExpr write FExpr;
  end;

  TUnaryOpDataExprClass = class of TUnaryOpDataExpr;

  TUnaryOpExpr = class(TTypedExpr)
  protected
    FExpr: TTypedExpr;
    FScriptPos: TScriptPos;

    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

    function GetIsConstant: Boolean; override;

  public
    constructor Create(context: TdwsBaseSymbolsContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr); virtual;
    destructor Destroy; override;

    function ScriptPos: TScriptPos; override;

    property expr: TTypedExpr read FExpr write FExpr;
  end;

  TUnaryOpExprClass = class of TUnaryOpExpr;

  // bool unary result
  TUnaryOpBoolExpr = class(TUnaryOpExpr)
  public
    constructor Create(context: TdwsBaseSymbolsContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr); override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
  end;

  // int unary result
  TUnaryOpIntExpr = class(TUnaryOpExpr)
  public
    constructor Create(context: TdwsBaseSymbolsContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr); override;
    procedure Orphan(context: TdwsCompilerContext); override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant);
      override; final;
    function Optimize(context: TdwsCompilerContext): TProgramExpr; override;
  end;

  // float unary result
  TUnaryOpFloatExpr = class(TUnaryOpExpr)
  public
    constructor Create(context: TdwsBaseSymbolsContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr); override;
    procedure Orphan(context: TdwsCompilerContext); override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant);
      override; final;
    function Optimize(context: TdwsCompilerContext): TProgramExpr; override;
  end;

  // String unary result
  TUnaryOpStringExpr = class(TUnaryOpExpr)
  public
    constructor Create(context: TdwsBaseSymbolsContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr); override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant);
      override; final;
  end;

  // variant unary result
  TUnaryOpVariantExpr = class(TUnaryOpExpr)
  public
    constructor Create(context: TdwsBaseSymbolsContext;
      const aScriptPos: TScriptPos; expr: TTypedExpr); override;
  end;

  // wraps an expression with a result into a no-result one and discard the result
  TNoResultWrapperExpr = class(TNoResultExpr)
  protected
    FExpr: TTypedExpr;

    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

    function GetIsConstant: Boolean; override;

  public
    constructor Create(const aScriptPos: TScriptPos; expr: TTypedExpr);
    destructor Destroy; override;

    procedure EvalNoResult(exec: TdwsExecution); override;

    function ScriptPos: TScriptPos; override;

    property expr: TTypedExpr read FExpr write FExpr;
  end;

  // left "op" right
  TBinaryOpExpr = class(TTypedExpr)
  protected
    FScriptPos: TScriptPos;
    FOp: TTokenType;
    FLeft: TTypedExpr;
    FRight: TTypedExpr;

    function GetSubExpr(i: Integer): TExprBase; override;
    function GetSubExprCount: Integer; override;

    function GetIsConstant: Boolean; override;

    procedure OrphanSubExprs(context: TdwsCompilerContext);

  public
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; const anOp: TTokenType;
      aLeft, aRight: TTypedExpr); virtual;
    destructor Destroy; override;

    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;

    function ScriptPos: TScriptPos; override;

    procedure OptimizeConstantOperandsToFloats(context: TdwsCompilerContext);

    procedure Swap;

    property Typ: TTypeSymbol read FTyp write FTyp;
    property Op: TTokenType read FOp write FOp;
    property Left: TTypedExpr read FLeft write FLeft;
    property Right: TTypedExpr read FRight write FRight;
  end;

  TBinaryOpExprClass = class of TBinaryOpExpr;

  TStaticallyTypedBinOpExpr = class(TBinaryOpExpr)
    function SpecializeTypedExpr(const context: ISpecializationContext)
      : TTypedExpr; override;
  end;

  TVariantBinOpExpr = class(TStaticallyTypedBinOpExpr)
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; const anOp: TTokenType;
      aLeft, aRight: TTypedExpr); override;
    function Optimize(context: TdwsCompilerContext): TProgramExpr; override;
  end;

  TIntegerBinOpExpr = class(TStaticallyTypedBinOpExpr)
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; const anOp: TTokenType;
      aLeft, aRight: TTypedExpr); override;
    procedure Orphan(context: TdwsCompilerContext); override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    function EvalAsFloat(exec: TdwsExecution): Double; override;
    function Optimize(context: TdwsCompilerContext): TProgramExpr; override;
  end;

  TStringBinOpExpr = class(TStaticallyTypedBinOpExpr)
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; const anOp: TTokenType;
      aLeft, aRight: TTypedExpr); override;
    procedure Orphan(context: TdwsCompilerContext); override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    function Optimize(context: TdwsCompilerContext): TProgramExpr; override;
  end;

  TFloatBinOpExpr = class(TStaticallyTypedBinOpExpr)
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; const anOp: TTokenType;
      aLeft, aRight: TTypedExpr); override;
    procedure Orphan(context: TdwsCompilerContext); override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    function Optimize(context: TdwsCompilerContext): TProgramExpr; override;
  end;

  TBooleanBinOpExpr = class(TStaticallyTypedBinOpExpr)
    constructor Create(context: TdwsCompilerContext;
      const aScriptPos: TScriptPos; const anOp: TTokenType;
      aLeft, aRight: TTypedExpr); override;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); override;
    function Optimize(context: TdwsCompilerContext): TProgramExpr; override;
  end;

  // A list of typed expressions
  TTypedExprList = class
  protected
    FList: TTightList;
    FTable: TSymbolTable;
    FDefaultExpected: TParamSymbol;

    function GetExpr(const x: Integer): TTypedExpr;
    procedure SetExpr(const x: Integer; const value: TTypedExpr);

  public
    destructor Destroy; override;
    procedure Orphan(context: TdwsCompilerContext);
    procedure OrphanItems(context: TdwsCompilerContext);

    procedure AddExpr(expr: TTypedExpr);
    function ExpectedArg: TParamSymbol;

    procedure Insert0(expr: TExprBase);
    procedure Delete(index: Integer);
    procedure Clear; inline;

    property expr[const x: Integer]: TTypedExpr read GetExpr
      write SetExpr; default;
    property Count: Integer read FList.FCount;
    property Table: TSymbolTable read FTable write FTable;
    property DefaultExpected: TParamSymbol read FDefaultExpected
      write FDefaultExpected;
  end;

  // Informations about the program in external procedures
  TProgramInfo = class
  private
    FExecution: TdwsProgramExecution;
    FFuncSym: TFuncSymbol;
    FLevel: Integer;
    FScriptObj: IScriptObj;
    FTable: TSymbolTable;

  protected
    function GetData(const s: String): TData;
    function GetFunc(const s: String): IInfo;
    function GetFuncBySym(funcSym: TFuncSymbol): IInfo;
    procedure SetFuncSym(const value: TFuncSymbol);
    function GetValueAsVariant(const s: String): Variant;
    function GetVars(const str: String): IInfo;
    function GetParams(const index: Integer): IInfo;
    procedure SetData(const s: String; const value: TData);
    procedure SetValueAsVariant(const s: String; const value: Variant);
    function GetResultAsVariant: Variant;
    function GetResultVars: IInfo;

    function GetValueAsString(const s: String): String;
    procedure SetValueAsString(const s: String; const value: String);
    function GetValueAsChar(const s: String): WideChar;
    function GetValueAsDataString(const s: String): RawByteString;
    procedure SetValueAsDataString(const s: String; const value: RawByteString);
    function GetValueAsInteger(const s: String): Int64;
    procedure SetValueAsInteger(const s: String; const value: Int64);
    function GetValueAsBoolean(const s: String): Boolean;
    procedure SetValueAsBoolean(const s: String; const value: Boolean);
    function GetValueAsFloat(const s: String): Double;
    procedure SetValueAsFloat(const s: String; const value: Double);
    function GetValueAsObject(const s: String): TObject;
    function GetValueAsClassSymbol(const s: String): TClassSymbol;
    function GetValueAsTStrings(const s: String): TStrings;

    function GetResultAsPVariant: PVariant;

    procedure SetResultAsVariant(const value: Variant);
    procedure SetResultAsString(const value: String);
    procedure SetResultAsDataString(const value: RawByteString);
    procedure SetResultAsInteger(const value: Int64);
    procedure SetResultAsBoolean(const value: Boolean);
    procedure SetResultAsFloat(const value: Double);

    function GetParamDataContext(index: Integer): IDataContext;

    function GetParamAsDataContext(index: Integer): IDataContext;

    function GetParamAsVariant(index: Integer): Variant;
    procedure SetParamAsVariant(index: Integer; const v: Variant);
    function GetParamAsInteger(index: Integer): Int64;
    procedure SetParamAsInteger(index: Integer; const v: Int64);
    function GetParamAsString(index: Integer): String;
    procedure SetParamAsString(index: Integer; const v: String);
    function GetParamAsDataString(index: Integer): RawByteString;
    procedure SetParamAsDataString(index: Integer; const v: RawByteString);
    function GetParamAsFileName(index: Integer): String;
    function GetParamAsFloat(index: Integer): Double;
    function GetParamAsBoolean(index: Integer): Boolean;
    function GetParamAsObject(index: Integer): TObject;
    function GetParamAsScriptObj(index: Integer): IScriptObj;
    function GetParamAsScriptDynArray(index: Integer): IScriptDynArray;

    function CreateUnitList: TUnitSymbolRefList;
    function FindSymbolInUnits(aUnitList: TUnitSymbolRefList;
      const aName: String): TSymbol; overload;
    function GetSystemTable: TSystemSymbolTable;

  public
    procedure PrepareScriptObj;

    function RegisterExternalObject(AObject: TObject; AutoFree: Boolean = False;
      ExactClassMatch: Boolean = True): IScriptObj;
    function GetExternalObjForVar(const s: String): TObject;
    // cycle ancestry hierarchy and find the nearest matching type
    function FindClassMatch(AObject: TObject; ExactMatch: Boolean = True)
      : TClassSymbol;
    function FindSymbolInUnits(const aName: String): TSymbol; overload;
    function GetTemp(const DataType: String): IInfo;
    function RootInfo(const aName: String): IInfo; overload;
    procedure GetSymbolInfo(sym: TSymbol; var Info: IInfo);
    function CompilerContext: TdwsCompilerContext; inline;

    procedure RaiseExceptObj(const msg: String; const obj: IScriptObj);

    procedure SetResultAsStringArray(const a: TStringDynArray); overload;
    procedure SetResultAsStringArray(const s: TStrings); overload;

    property Table: TSymbolTable read FTable write FTable;
    property systemTable: TSystemSymbolTable read GetSystemTable;
    property Execution: TdwsProgramExecution read FExecution write FExecution;
    property Level: Integer read FLevel write FLevel;
    property Data[const s: String]: TData read GetData write SetData;
    property func[const s: String]: IInfo read GetFunc;
    property FuncBySym[funcSym: TFuncSymbol]: IInfo read GetFuncBySym;
    property funcSym: TFuncSymbol read FFuncSym write SetFuncSym;
    property Method[const s: String]: IInfo read GetFunc;
    property scriptObj: IScriptObj read FScriptObj write FScriptObj;
    property ResultAsVariant: Variant read GetResultAsVariant
      write SetResultAsVariant;
    property ResultVars: IInfo read GetResultVars;
    property Vars[const s: String]: IInfo read GetVars;
    property params[const Index: Integer]: IInfo read GetParams;
    function ParamCount: Integer;

    property ValueAsVariant[const s: String]: Variant read GetValueAsVariant
      write SetValueAsVariant;
    property ValueAsChar[const s: String]: WideChar read GetValueAsChar;
    property ValueAsString[const s: String]: String read GetValueAsString
      write SetValueAsString;
    property ValueAsDataString[const s: String]: RawByteString
      read GetValueAsDataString write SetValueAsDataString;
    property ValueAsInteger[const s: String]: Int64 read GetValueAsInteger
      write SetValueAsInteger;
    property ValueAsBoolean[const s: String]: Boolean read GetValueAsBoolean
      write SetValueAsBoolean;
    property ValueAsFloat[const s: String]: Double read GetValueAsFloat
      write SetValueAsFloat;
    property ValueAsObject[const s: String]: TObject read GetValueAsObject;
    property ValueAsClassSymbol[const s: String]: TClassSymbol
      read GetValueAsClassSymbol;
    property ValueAsTStrings[const s: String]: TStrings read GetValueAsTStrings;

    // property ParamAsPVariant[index : Integer] : PVariant read GetParamAsPVariant;
    property ParamAsVariant[index: Integer]: Variant read GetParamAsVariant
      write SetParamAsVariant;
    property ParamAsInteger[index: Integer]: Int64 read GetParamAsInteger
      write SetParamAsInteger;
    property ParamAsString[index: Integer]: String read GetParamAsString
      write SetParamAsString;
    property ParamAsFileName[index: Integer]: String read GetParamAsFileName;
    property ParamAsDataString[index: Integer]: RawByteString
      read GetParamAsDataString write SetParamAsDataString;
    property ParamAsFloat[index: Integer]: Double read GetParamAsFloat;
    property ParamAsBoolean[index: Integer]: Boolean read GetParamAsBoolean;
    property ParamAsObject[index: Integer]: TObject read GetParamAsObject;
    property ParamAsScriptObj[index: Integer]: IScriptObj
      read GetParamAsScriptObj;
    property ParamAsScriptDynArray[index: Integer]: IScriptDynArray
      read GetParamAsScriptDynArray;
    property ParamAsDataContext[index: Integer]: IDataContext
      read GetParamAsDataContext;

    property ResultAsString: String write SetResultAsString;
    property ResultAsDataString: RawByteString write SetResultAsDataString;
    property ResultAsBoolean: Boolean write SetResultAsBoolean;
    property ResultAsInteger: Int64 write SetResultAsInteger;
    property ResultAsFloat: Double write SetResultAsFloat;
    procedure ResultSetNull;

    property ResultAsStringArray: TStringDynArray write SetResultAsStringArray;
  end;

  EdwsProgramInfoException = class(Exception)
  end;

  // An instance of a script class FClassSym. Instance data in FData,
  TScriptObj = class(TDataContext)
  private
    FNextObject, FPrevObject: TScriptObj;

  protected
    procedure SetExecutionContext(exec: TdwsProgramExecution); virtual;

  public
    property NextObject: TScriptObj read FNextObject write FNextObject;
    property PrevObject: TScriptObj read FPrevObject write FPrevObject;
  end;

  TScriptObjInstance = class sealed(TScriptObj, IScriptObj)
  private
    FExternalObject: TObject;
    FDestroyed: Boolean;
    FClassSym: TClassSymbol;
    FExecutionContext: TdwsProgramExecution;
    FOnObjectDestroy: TObjectDestroyEvent;

  protected
    function GetClassSym: TClassSymbol;
    function GetExternalObject: TObject;
    procedure SetExternalObject(value: TObject);
    function GetDestroyed: Boolean;
    procedure SetDestroyed(const val: Boolean);
    procedure SetExecutionContext(exec: TdwsProgramExecution); override;

  public
    constructor Create(aClassSym: TClassSymbol;
      executionContext: TdwsProgramExecution = nil);
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    class function NewInstance: TObject; override;
    procedure FreeInstance; override;

    function ToString: String; override;
    function ScriptTypeName: String; override;

    procedure ClearData; override;

    function FieldAddress(const fieldName: String): Integer;

    function FieldAsString(const fieldName: String): String;
    function FieldAsInteger(const fieldName: String): Int64;
    function FieldAsFloat(const fieldName: String): Double;
    function FieldAsBoolean(const fieldName: String): Boolean;
    function FieldAsScriptDynArray(const fieldName: String): IScriptDynArray;

    property ClassSym: TClassSymbol read FClassSym;
    property executionContext: TdwsProgramExecution read FExecutionContext
      write FExecutionContext;
    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy
      write FOnObjectDestroy;
    property Destroyed: Boolean read FDestroyed write FDestroyed;
    property ExternalObject: TObject read FExternalObject write FExternalObject;
  end;

  TScriptInterface = class(TScriptObj, IScriptObjInterface)
  private
    FTyp: TInterfaceSymbol;
    FInstance: IScriptObj;
    FVMT: TMethodSymbolArray;
    FExecutionContext: TdwsProgramExecution;

  protected
    function GetScriptObj: IScriptObj;

  public
    constructor Create(const instance: IScriptObj;
      const resolvedInterface: TResolvedInterface;
      executionContext: TdwsProgramExecution = nil);
    procedure BeforeDestruction; override;

    function ToString: String; override;
    function ScriptTypeName: String; override;

    property Typ: TInterfaceSymbol read FTyp;
    property instance: IScriptObj read FInstance;
    property VMT: TMethodSymbolArray read FVMT write FVMT;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  System.Variants, System.TypInfo,
  dwsFunctions, dwsCoreExprs, dwsMagicExprs, dwsMethodExprs,
  dwsUnifiedConstants,
  dwsInfoClasses, dwsCompilerUtils, dwsConstExprs, dwsResultFunctions,
  dwsSpecializationContext, dwsDynamicArrays, dwsArrayExprs,
  dwsAssociativeArrays;

// TScriptDynamicArray_InitData
//
procedure TScriptDynamicArray_InitData(elemTyp: TTypeSymbol;
  const resultDC: IDataContext; offset: NativeInt);
var
  intf: IScriptDynArray;
begin
  CreateNewDynamicArray(elemTyp, intf);
  resultDC.AsInterface[offset] := intf;
end;

{ TScriptObjectWrapper }

// wrapper to interact with an released script object
type
  TScriptObjectWrapper = class(TDataContext, IUnknown, IScriptObj)
  private
    FScriptObj: TScriptObjInstance;

  protected
    { IScriptObj }
    function GetClassSym: TClassSymbol;
    function GetSelf: TObject;
    function ScriptTypeName: String; override;
    function GetExternalObject: TObject;
    procedure SetExternalObject(value: TObject);
    function GetDestroyed: Boolean;
    procedure SetDestroyed(const val: Boolean);

  public
    constructor Create(scriptObj: TScriptObjInstance);

    function FieldAsString(const fieldName: String): String;
    function FieldAsInteger(const fieldName: String): Int64;
    function FieldAsFloat(const fieldName: String): Double;
    function FieldAsBoolean(const fieldName: String): Boolean;
    function FieldAsScriptDynArray(const fieldName: String): IScriptDynArray;
  end;

  // Create
  //
constructor TScriptObjectWrapper.Create(scriptObj: TScriptObjInstance);
begin
  inherited Create;
  FScriptObj := scriptObj;
  DirectData := scriptObj.DirectData;
end;

// GetClassSym
//
function TScriptObjectWrapper.GetClassSym: TClassSymbol;
begin
  result := FScriptObj.FClassSym;
end;

// GetSelf
//
function TScriptObjectWrapper.GetSelf: TObject;
begin
  result := FScriptObj;
end;

// ScriptTypeName
//
function TScriptObjectWrapper.ScriptTypeName: String;
begin
  result := FScriptObj.ScriptTypeName;
end;

function TScriptObjectWrapper.GetExternalObject: TObject;
begin
  result := FScriptObj.GetExternalObject;
end;

procedure TScriptObjectWrapper.SetExternalObject(value: TObject);
begin
  FScriptObj.SetExternalObject(value);
end;

// GetDestroyed
//
function TScriptObjectWrapper.GetDestroyed: Boolean;
begin
  result := FScriptObj.Destroyed
end;

// SetDestroyed
//
procedure TScriptObjectWrapper.SetDestroyed(const val: Boolean);
begin
  FScriptObj.Destroyed := val;
end;

// RaiseVariableNotFound
//
procedure RaiseVariableNotFound(const s: String);
begin
  raise EdwsProgramInfoException.CreateFmt(RTE_VariableNotFound, [s]);
end;

// RaiseIncorrectParameterIndex
//
procedure RaiseIncorrectParameterIndex(i: Integer);
begin
  raise EdwsProgramInfoException.CreateFmt(RTE_IncorrectParameterIndex, [i]);
end;

// RaiseOnlyVarSymbols
//
procedure RaiseOnlyVarSymbols(sym: TSymbol);
begin
  raise EdwsProgramInfoException.CreateFmt(RTE_OnlyVarSymbols, [sym.Caption]);
end;

// FieldAsString
//
function TScriptObjectWrapper.FieldAsString(const fieldName: String): String;
begin
  result := FScriptObj.FieldAsString(fieldName);
end;

// FieldAsInteger
//
function TScriptObjectWrapper.FieldAsInteger(const fieldName: String): Int64;
begin
  result := FScriptObj.FieldAsInteger(fieldName);
end;

// FieldAsFloat
//
function TScriptObjectWrapper.FieldAsFloat(const fieldName: String): Double;
begin
  result := FScriptObj.FieldAsFloat(fieldName);
end;

// FieldAsBoolean
//
function TScriptObjectWrapper.FieldAsBoolean(const fieldName: String): Boolean;
begin
  result := FScriptObj.FieldAsBoolean(fieldName);
end;

// FieldAsScriptDynArray
//
function TScriptObjectWrapper.FieldAsScriptDynArray(const fieldName: String)
  : IScriptDynArray;
begin
  result := FScriptObj.FieldAsScriptDynArray(fieldName);
end;

// ------------------
// ------------------ TdwsProgramExecution ------------------
// ------------------

// Create
//
constructor TdwsProgramExecution.Create(aProgram: TdwsMainProgram;
  const stackParams: TStackParameters);
begin
  inherited Create(stackParams);

  FProg := aProgram;
  FProg._AddRef;

  FCompilerContext := aProgram.CompilerContext;

  if aProgram.CompileMsgs.HasErrors then
    FProgramState := psUndefined
  else
    FProgramState := psReadyToRun;

  FCallStack.Push(nil);
  FCallStack.Push(FProg);
end;

// Destroy
//
destructor TdwsProgramExecution.Destroy;
begin
  if ProgramState = psRunning then
    raise Exception.Create(RTE_ScriptStillRunning);
  if ProgramState = psRunningStopped then
    EndProgram;

  FProg.NotifyExecutionDestruction(Self);

  ReleaseObjects;

  FProgramInfo.Free;
  FProgInfoPool.Free;
  FResult.Free;
  FRuntimeMsgs.Free;
  FEnvironment := nil;

  FProg._Release;
  inherited;
end;

// Execute
//
procedure TdwsProgramExecution.Execute(aTimeoutMilliSeconds: Integer = 0);
begin
  if BeginProgram then
  begin
    if ProgramState = psRunning then
      RunProgram(aTimeoutMilliSeconds);
  end;
  if ProgramState in [psRunning, psRunningStopped] then
    EndProgram;
end;

// ExecuteParam
//
procedure TdwsProgramExecution.ExecuteParam(const params: TVariantDynArray;
  aTimeoutMilliSeconds: Integer = 0);
var
  x, index: Integer;
begin
  SetLength(FParameters, High(params) - Low(params) + 1);
  index := 0;
  for x := Low(params) to High(params) do
  begin
    FParameters[index] := params[x];
    Inc(index);
  end;

  Execute(aTimeoutMilliSeconds);
end;

// ExecuteParam
//
procedure TdwsProgramExecution.ExecuteParam(const params: OleVariant;
  aTimeoutMilliSeconds: Integer = 0);
var
  x, n: Integer;
begin
  if VariantIsArray(params) then
  begin
    n := VariantArrayHighBound(params, 1);
    SetLength(FParameters, n + 1);
    for x := 0 to n do
      FParameters[x] := params[x];
  end
  else
  begin
    SetLength(FParameters, 1);
    FParameters[0] := params;
  end;

  Execute(aTimeoutMilliSeconds);
end;

// BeginProgram
//
function TdwsProgramExecution.BeginProgram: Boolean;
begin
  result := False;

  // Check program state
  if FProgramState <> psReadyToRun then
  begin
    case FProgramState of
      psRunning, psRunningStopped:
        Msgs.AddRuntimeError(RTE_ScriptAlreadyRunning);
      psUndefined:
        Msgs.AddRuntimeError(RTE_CantRunScript);
    else
      Msgs.AddRuntimeError(RTE_StateReadyToRunExpected);
    end;
    Exit;
  end;

  if Assigned(FOnExecutionStarted) then
    FOnExecutionStarted(Self);
  FStartTicks := GetSystemMilliseconds;

{$IFDEF WIN32}
  F8087CW := DirectSet8087CW($133F);
{$ENDIF}
  FProgramState := psRunning;
  try
    Msgs.Clear;

    FCurrentProg := FProg;

    // Stack
    Stack.Reset;

    FProgramInfo := TProgramInfo.Create;
    FProgramInfo.Table := FProg.FTable;
    FProgramInfo.Execution := Self;

    // allocate global stack space
    Stack.Push(FProg.DataSize);
    Stack.PushBp(0, Stack.BasePointer);

    // Initialize Result
    FResult.Free;
    FResult := FProg.FResultType.CreateProgResult;

    // Debugger
    StartDebug;

    // Prepare FileSystem
    if FProg.RuntimeFileSystem <> nil then
      FFileSystem := FProg.RuntimeFileSystem.AllocateFileSystemRW
    else
      FFileSystem := TdwsOSFileSystem.Create;

    // Initialize global variables
    Status := esrNone;
    RunProgramExpr(FProg.FInitExpr);

    if not(FProg.expr.Taxonomy = ebtBlockExprBase) then
      DoStep(FProg.FExpr);

    result := True;
  except
    // we should never end up here
    FProgramState := psRunningStopped;
    raise;
  end;
end;

// RunProgramExpr
//
procedure TdwsProgramExecution.RunProgramExpr(expr: TProgramExpr);

  procedure Handle_EScriptAssertionFailed(e: EScriptAssertionFailed);
  begin
    if IsDebugging then
      Debugger.NotifyException(Self, e.ExceptionObj);
    Msgs.AddRuntimeError(e.ScriptPos, Copy(e.Message, 1, LastDelimiter('[',
      e.Message) - 2) + StrAfterChar(e.Message, ']'), e.ScriptCallStack);
  end;

  procedure Handle_Exception(e: Exception);
  var
    debugPos: TScriptPos;
    exceptObj: IScriptObj;
  begin
    EnterExceptionBlock(exceptObj);
    try
      if LastScriptError <> nil then
      begin
        if LastScriptError is TFuncExpr then
          Msgs.AddRuntimeError(LastScriptError.ScriptPos,
            e.Message + ' in ' + TFuncExpr(LastScriptError)
            .funcSym.QualifiedName, LastScriptCallStack)
        else
          Msgs.AddRuntimeError(LastScriptError.ScriptPos, e.Message,
            LastScriptCallStack)
      end
      else if (Debugger <> nil) and (Debugger.LastDebugStepExpr <> nil) then
      begin
        debugPos := Debugger.LastDebugStepExpr.ScriptPos;
        debugPos.Col := 0;
        Msgs.AddRuntimeError(debugPos, e.Message, LastScriptCallStack)
      end
      else
        Msgs.AddRuntimeError(cNullPos, e.Message, LastScriptCallStack);
    finally
      LeaveExceptionBlock;
    end;
  end;

begin
  Status := esrNone;
  try
    // Run the script
    expr.EvalNoResult(Self);

    if Status <> esrNone then
    begin
      case Status of
        esrBreak:
          Msgs.AddRuntimeError(RTE_InvalidBreak);
        esrContinue:
          Msgs.AddRuntimeError(RTE_InvalidContinue);
      end;
    end;
  except
    on e: EScriptAssertionFailed do
      Handle_EScriptAssertionFailed(e);
    on e: EScriptException do
    begin
      if IsDebugging then
        Debugger.NotifyException(Self, e.ExceptionObj);
      Msgs.AddRuntimeError(e.ScriptPos, e.Message, e.ScriptCallStack);
    end;
    on e: EScriptError do
    begin
      if IsDebugging then
        Debugger.NotifyException(Self, nil);
      Msgs.AddRuntimeError(e.ScriptPos, e.Message, e.ScriptCallStack);
    end;
    on e: EScriptStackException do
      Msgs.AddRuntimeError(LastScriptError.ScriptPos, e.Message,
        LastScriptCallStack);
    on e: Exception do
      Handle_Exception(e);
  end;
end;

// RunProgram
//
procedure TdwsProgramExecution.RunProgram(aTimeoutMilliSeconds: Integer);
var
  stackBaseReqSize: Integer;
begin
  if FProgramState <> psRunning then
  begin
    Msgs.AddRuntimeError('Program state psRunning expected');
    Exit;
  end;

  FExecutionTimedOut := False;
  if aTimeoutMilliSeconds = 0 then
    aTimeoutMilliSeconds := FProg.TimeoutMilliseconds;
  if aTimeoutMilliSeconds > 0 then
    TdwsGuardianThread.GuardExecution(Self, aTimeoutMilliSeconds);

  stackBaseReqSize := FProg.DataSize;
  if Stack.StackPointer < stackBaseReqSize then
    Stack.FixBaseStack(stackBaseReqSize);

  try
    RunProgramExpr(FProg.FExpr);
  finally
    if aTimeoutMilliSeconds > 0 then
      TdwsGuardianThread.ForgetExecution(Self);
  end;

  ClearScriptError;
end;

// Stop
//
procedure TdwsProgramExecution.Stop;
begin
  if FProgramState = psRunning then
    FProgramState := psRunningStopped;
end;

// StopForTimeout
//
procedure TdwsProgramExecution.StopForTimeout;
begin
  FExecutionTimedOut := True;
  Stop;
end;

// EndProgram
//
procedure TdwsProgramExecution.EndProgram;
begin
  if not(FProgramState in [psRunning, psRunningStopped]) then
    raise Exception.Create('Program was not started!');

  if FProg.FinalExpr <> nil then
    RunProgramExpr(FProg.FFinalExpr);

  FProgramState := psTerminated;
  try
    // Stack
    Stack.PopBp(0);
    Stack.Pop(Stack.StackPointer);
    // FProg.FAddrGenerator.DataSize + FProg.FGlobalAddrGenerator.DataSize);

    // Object Cycles
    ReleaseObjects;

    // Custom states
    FCustomStates.Free;
    FCustomStates := nil;
    FCustomInterfaces.Free;
    FCustomInterfaces := nil;

    // Debugger
    StopDebug;

    // FileSystem
    FFileSystem := nil;

    FProgramInfo.Free;
    FProgramInfo := nil;

    FProgramState := psReadyToRun;
  except
    on e: EScriptError do
      Msgs.AddRuntimeError(e.ScriptPos, e.Message, e.ScriptCallStack);
    on e: Exception do
      Msgs.AddRuntimeError(e.Message);
  end;

{$IFDEF WIN32}
  DirectSet8087CW(F8087CW);
{$ENDIF}
  FProg.RecordExecution(GetSystemMilliseconds - FStartTicks - FSleepTime);
  if Assigned(FOnExecutionEnded) then
    FOnExecutionEnded(Self);
end;

// CallStackToString
//
class function TdwsProgramExecution.CallStackToString(const callStack
  : TdwsExprLocationArray): String;
begin
  result := TExprBase.CallStackToString(callStack);
end;

// RaiseAssertionFailed
//
procedure TdwsProgramExecution.RaiseAssertionFailed(fromExpr: TExprBase;
  const msg: String; const ScriptPos: TScriptPos; exec: TdwsExecution);
begin
  RaiseAssertionFailedFmt(fromExpr, RTE_AssertionFailed,
    [ScriptPos.AsInfo, msg], ScriptPos, exec);
end;

// RaiseAssertionFailedFmt
//
procedure TdwsProgramExecution.RaiseAssertionFailedFmt(fromExpr: TExprBase;
  const fmt: String; const args: array of const; const ScriptPos: TScriptPos;
  exec: TdwsExecution);
var
  exceptObj: IScriptObj;
  fmtMsg: String;
  e: EScriptAssertionFailed;
begin
  SetScriptError(fromExpr);
  fmtMsg := Format(fmt, args);
  exceptObj := IScriptObj(IUnknown(ProgramInfo.Vars[SYS_EASSERTIONFAILED].Method
    [SYS_TOBJECT_CREATE].Call([fmtMsg]).value));
  (exceptObj.ExternalObject as TdwsExceptionContext).Skip(1);
  // temporary constructor expression
  if IsDebugging then
    DebuggerNotifyException(exceptObj);
  e := EScriptAssertionFailed.Create(fmtMsg, exceptObj, ScriptPos);
  if exec <> nil then
    e.ScriptCallStack := exec.GetCallStack;
  raise e;
end;

// CreateEDelphiObj
//
function TdwsProgramExecution.CreateEDelphiObj(const ClassName: String;
  const Message: String): IScriptObj;
begin
  result := IScriptObj(IUnknown(ProgramInfo.Vars[SYS_EDELPHI].Method
    [SYS_TOBJECT_CREATE].Call([ClassName, Message]).value));
  (result.ExternalObject as TdwsExceptionContext).ReplaceTop(LastScriptError);
  // temporary constructor expression
end;

// EnterExceptionBlock
//
procedure TdwsProgramExecution.EnterExceptionBlock(var exceptObj: IScriptObj);
var
  mainException: Exception;
  err: EScriptError;
  msg: String;
begin
  if ExceptionObjectStack.Count > Stack.MaxExceptionDepth then
    raise EScriptExceptionOverflow.CreateFmt(RTE_MaximalExceptionDepthExceeded,
      [ExceptionObjectStack.Count]);

{$IFDEF FPC}
  mainException := SysUtils.ExceptObject as Exception;
{$ELSE}
  mainException := System.ExceptObject as Exception;
{$ENDIF}
  if mainException is EScriptException then
  begin
    // a raise-statement created an Exception object
    exceptObj := EScriptException(mainException).ExceptionObj
  end
  else if mainException is EScriptError then
  begin
    msg := mainException.Message;
    err := EScriptError(mainException);
    if Length(err.ScriptCallStack) > 0 then
      msg := msg + ' in ' + (err.ScriptCallStack[High(err.ScriptCallStack)
        ].expr as TFuncExpr).funcSym.QualifiedName;
    if EScriptError(mainException).ScriptPos.Defined then
      msg := msg + EScriptError(mainException).ScriptPos.AsInfo;
    exceptObj := CreateEDelphiObj(mainException.ClassName, msg);
  end
  else if mainException is EScriptStackOverflow then
  begin
    exceptObj := nil
  end
  else
  begin
    // A Delphi exception. Transform it to a EDelphi-dws exception
    exceptObj := CreateEDelphiObj(mainException.ClassName,
      mainException.Message);
  end;

  inherited EnterExceptionBlock(exceptObj);
  if IsDebugging then
    DebuggerNotifyException(exceptObj);
end;

// AcquireProgramInfo
//
function TdwsProgramExecution.AcquireProgramInfo(funcSym: TFuncSymbol)
  : TProgramInfo;
begin
  if FProgInfoPool = nil then
  begin
    result := TProgramInfo.Create;
    result.Execution := Self;
  end
  else
  begin
    result := FProgInfoPool;
    FProgInfoPool := nil;
  end;
  result.funcSym := funcSym;
  if funcSym <> nil then
    result.FTable := funcSym.params
  else
    result.FTable := FProg.Table;
end;

// ReleaseProgramInfo
//
procedure TdwsProgramExecution.ReleaseProgramInfo(Info: TProgramInfo);
begin
  if FProgInfoPool = nil then
  begin
    FProgInfoPool := Info;
    Info.scriptObj := nil;
  end
  else
    Info.Free;
end;

// LocalizeSymbol
//
procedure TdwsProgramExecution.LocalizeSymbol(aResSymbol: TResourceStringSymbol;
  var result: String);
begin
  if Assigned(Localizer) then
    Localizer.LocalizeSymbol(aResSymbol, result)
  else
    result := aResSymbol.value;
end;

// LocalizeString
//
procedure TdwsProgramExecution.LocalizeString(const aString: String;
  var result: String);
begin
  if Assigned(Localizer) then
    Localizer.LocalizeString(aString, result)
  else
    result := aString;
end;

// ValidateFileName
//
function TdwsProgramExecution.ValidateFileName(const path: String): String;
begin
  result := FileSystem.ValidateFileName(path);
  if result = '' then
    raise EScriptException.CreateFmt(RTE_UnauthorizedFilePath, [path]);
end;

// HasCustomStates
//
function TdwsProgramExecution.HasCustomStates: Boolean;
begin
  result := (FCustomStates <> nil);
end;

// HasCustomInterfaces
//
function TdwsProgramExecution.HasCustomInterfaces: Boolean;
begin
  result := (FCustomInterfaces <> nil);
end;

// ReleaseObjects
//
procedure TdwsProgramExecution.ReleaseObjects;
var
  i: Integer;
  iter: TScriptObj;
  buffer: array of TScriptObj;
begin
  FRTTIRawAttributes := nil;

  if FObjectCount = 0 then
    Exit;

  // add refcount to keep all alive during cleanup
  // detach from execution, add to buffer
  SetLength(buffer, FObjectCount);
  i := 0;
  iter := FFirstObject;
  while iter <> nil do
  begin
    buffer[i] := iter;
    Inc(i);
    iter._AddRef;
    iter.SetExecutionContext(nil);
    iter := iter.NextObject;
  end;

  // clear all datas, kill references
  for i := 0 to FObjectCount - 1 do
  begin
    iter := buffer[i];
    iter.ClearData;
    iter.PrevObject := nil;
    iter.NextObject := nil;
  end;

  // dec refcount
  for i := 0 to FObjectCount - 1 do
  begin
    iter := buffer[i];
    iter._Release;
  end;

  // all remaining objects should now be referred only outside of scripts
  // can't do anything about them without triggering crashes
  FFirstObject := nil;
  FLastObject := nil;
  FObjectCount := 0;
end;

// ScriptObjCreated
//
procedure TdwsProgramExecution.ScriptObjCreated(scriptObj: TScriptObj);
begin
  scriptObj.SetExecutionContext(Self);
  if FObjectCount = 0 then
  begin
    FFirstObject := scriptObj;
    FLastObject := scriptObj;
  end
  else
  begin
    scriptObj.PrevObject := FLastObject;
    FLastObject.NextObject := scriptObj;
    FLastObject := scriptObj;
  end;
  Inc(FObjectCount);
end;

// ScriptObjDestroyed
//
procedure TdwsProgramExecution.ScriptObjDestroyed(scriptObj: TScriptObj);
begin
  scriptObj.SetExecutionContext(nil);
  Dec(FObjectCount);

  if FObjectCount > 0 then
  begin
    if scriptObj.PrevObject <> nil then
      scriptObj.PrevObject.NextObject := scriptObj.NextObject
    else
    begin
      FFirstObject := scriptObj.NextObject;
      FFirstObject.PrevObject := nil;
    end;
    if scriptObj.NextObject <> nil then
      scriptObj.NextObject.PrevObject := scriptObj.PrevObject
    else
    begin
      FLastObject := scriptObj.PrevObject;
      FLastObject.NextObject := nil;
    end;
  end
  else
  begin
    FFirstObject := nil;
    FLastObject := nil;
  end;
end;

// DestroyScriptObj
//
procedure TdwsProgramExecution.DestroyScriptObj(const scriptObj: IScriptObj);
var
  destroySym: TMethodSymbol;
  expr: TDestructorVirtualExpr;
  oldStatus: TExecutionStatusResult;
  caller: TExprBase;
begin
  if scriptObj.ClassSym.IsExternalRooted then
    Exit;
  try
    destroySym := CompilerContext.TypDefaultDestructor;
    expr := TDestructorVirtualExpr.Create(CompilerContext, cNullPos, destroySym,
      TConstExpr.CreateValue(cNullPos, scriptObj.ClassSym, scriptObj));

    caller := CallStackLastExpr;
    if caller <> nil then
    begin
      // called from script
      expr.Level := (caller as TFuncExpr).Level;
      if expr.Level = 0 then
        expr.Level := 1;
    end
    else
    begin
      // called from Delphi-side outside of script
      expr.Level := 0;
    end;

    oldStatus := Status;
    try
      Status := esrNone;
      scriptObj.Destroyed := False;
      expr.EvalNoResult(Self);
    finally
      scriptObj.Destroyed := True;
      Status := oldStatus;
      expr.Free;
    end;
  except
    on e: Exception do
      Msgs.AddRuntimeError(e.Message);
  end;
end;

// GetInfo
//
function TdwsProgramExecution.GetInfo: TProgramInfo;
begin
  Assert(FProgramState <> psReadyToRun,
    'Must call BeginProgram before accessing Info');
  if ProgramInfo <> nil then
    result := ProgramInfo
  else
    result := AcquireProgramInfo(nil);
end;

// GetResult
//
function TdwsProgramExecution.GetResult: TdwsResult;
begin
  result := FResult;
end;

// GetObjectCount
//
function TdwsProgramExecution.GetObjectCount: Integer;
begin
  result := FObjectCount;
end;

// GetLocalizer
//
function TdwsProgramExecution.GetLocalizer: IdwsLocalizer;
begin
  result := FLocalizer;
end;

// SetLocalizer
//
procedure TdwsProgramExecution.SetLocalizer(const val: IdwsLocalizer);
begin
  FLocalizer := val;
end;

// GetExecutionTimedOut
//
function TdwsProgramExecution.GetExecutionTimedOut: Boolean;
begin
  result := FExecutionTimedOut;
end;

// GetFileSystem
//
function TdwsProgramExecution.FileSystem: IdwsFileSystemRW;
begin
  if FFileSystem = nil then
  begin
    if FProg.RuntimeFileSystem <> nil then
      FFileSystem := FProg.RuntimeFileSystem.AllocateFileSystemRW
    else
      FFileSystem := TdwsOSFileSystem.Create;
  end;
  result := FFileSystem;
end;

// GetMsgs
//
function TdwsProgramExecution.GetMsgs: TdwsRuntimeMessageList;
begin
  if FRuntimeMsgs = nil then
    FRuntimeMsgs := TdwsRuntimeMessageList.Create;
  result := FRuntimeMsgs;
end;

// GetCustomStates
//
function TdwsProgramExecution.GetCustomStates: TdwsCustomStates;
begin
  if FCustomStates = nil then
    FCustomStates := TdwsCustomStates.Create;
  result := FCustomStates;
end;

// GetCustomInterfaces
//
function TdwsProgramExecution.GetCustomInterfaces: TdwsCustomInterfaces;
begin
  if FCustomInterfaces = nil then
    FCustomInterfaces := TdwsCustomInterfaces.Create;
  result := FCustomInterfaces;
end;

// GetProg
//
function TdwsProgramExecution.GetProg: IdwsProgram;
begin
  result := FProg;
end;

// HasProgram
//
function TdwsProgramExecution.HasProgram: Boolean;
begin
  result := FProg <> nil;
end;

// HasCompileErrors
//
function TdwsProgramExecution.HasCompileErrors: Boolean;
begin
  result := (FProg <> nil) and FProg.CompileMsgs.HasErrors;
end;

// EnterRecursion
//
procedure TdwsProgramExecution.EnterRecursion(caller: TExprBase);
begin
  FCallStack.Push(caller);
  FCallStack.Push(FCurrentProg);
  if (FCallStack.Count shr 1) > FStack.MaxRecursionDepth then
    RaiseMaxRecursionReached;

  if IsDebugging then
    Debugger.EnterFunc(Self, caller);
end;

// LeaveRecursion
//
procedure TdwsProgramExecution.LeaveRecursion;
begin
  if IsDebugging then
    Debugger.LeaveFunc(Self, TExprBase(FCallStack.Peek(1)));

  FCurrentProg := TdwsProgram(FCallStack.Peek);

  FCallStack.Pop;
  FCallStack.Pop;
end;

// SetCurrentProg
//
procedure TdwsProgramExecution.SetCurrentProg(const val: TdwsProgram);
begin
  FCurrentProg := val;
end;

// RaiseMaxRecursionReached
//
procedure TdwsProgramExecution.RaiseMaxRecursionReached;
begin
  FCallStack.Pop;
  SetScriptError(TExprBase(FCallStack.Peek));
  FCallStack.Pop;
  raise EScriptStackOverflow.CreateFmt(RTE_MaximalRecursionExceeded,
    [FStack.MaxRecursionDepth]);
end;

// CallStackDepth
//
function TdwsProgramExecution.CallStackDepth: Integer;
begin
  result := (FCallStack.Count - 2) div 2;
end;

// GetCallStack
//
function TdwsProgramExecution.GetCallStack: TdwsExprLocationArray;
var
  i, n: Integer;
begin
  n := CallStackDepth;
  SetLength(result, n);
  for i := 0 to n - 1 do
  begin
    result[n - 1 - i].expr :=
      (TObject(FCallStack.List[i * 2 + 2]) as TExprBase);
    result[n - 1 - i].Prog := TObject(FCallStack.List[i * 2 + 3]);
  end;
end;

// CallStackLastExpr
//
function TdwsProgramExecution.CallStackLastExpr: TExprBase;
var
  n: Integer;
begin
  n := FCallStack.Count - 2;
  if n >= 0 then
    result := (TObject(FCallStack.List[n]) as TExprBase)
  else
    result := nil;
end;

// CallStackLastProg
//
function TdwsProgramExecution.CallStackLastProg: TObject;
var
  n: Integer;
begin
  n := FCallStack.Count - 1;
  if n >= 0 then
    result := TObject(FCallStack.List[n])
  else
    result := nil;
end;

// DebuggerFieldAddr
//
function TdwsProgramExecution.DebuggerFieldAddr: Integer;
var
  field: TFieldSymbol;
begin
  if FDebuggerFieldAddr = 0 then
  begin
    field := TFieldSymbol(CompilerContext.TypException.Members.FindSymbol
      (SYS_EXCEPTION_DEBUGGER_FIELD, cvMagic, TFieldSymbol));
    Assert(field <> nil);
    FDebuggerFieldAddr := field.offset;
  end;
  result := FDebuggerFieldAddr;
end;

// DebuggerNotifyException
//
procedure TdwsProgramExecution.DebuggerNotifyException(const exceptObj
  : IScriptObj);
var
  addr, i: Integer;
begin
  if (exceptObj <> nil) and IsDebugging then
  begin
    addr := DebuggerFieldAddr;
    i := exceptObj.AsInteger[addr];
    exceptObj.AsInteger[addr] := i + 1;
    if i = 0 then
      Debugger.NotifyException(Self, exceptObj);
  end;
end;

// ------------------
// ------------------ TdwsProgram ------------------
// ------------------

// Create
//
constructor TdwsProgram.Create(const systemTable: ISystemSymbolTable);
begin
  inherited Create;

  FCompileMsgs := TdwsCompileMessageList.Create;

  FAddrGenerator := TAddrGeneratorRec.CreatePositive(0);

  // Initialize the system table
  FRootTable := TProgramSymbolTable.Create(systemTable.SymbolTable,
    @FAddrGenerator);
  FTable := FRootTable;

  FInitExpr := TBlockInitExpr.Create(cNullPos);
end;

// Destroy
//
destructor TdwsProgram.Destroy;
begin
  FExpr.Free;
  FInitExpr.Free;
  FRootTable.Free;
  FUnitMains.Free;

  FCompileMsgs.Free;
  FSubTables.Clear;

  inherited;
end;

function TdwsProgram.GetLevel: Integer;
begin
  result := FAddrGenerator.Level;
end;

// GetUnitMains
//
function TdwsProgram.GetUnitMains: TUnitMainSymbols;
begin
  result := FUnitMains;
end;

// GetAddrGeneratorDataSize
//
function TdwsProgram.GetAddrGeneratorDataSize: Integer;
begin
  result := FAddrGenerator.DataSize;
end;

// SubExpr
//
function TdwsProgram.SubExpr(i: Integer): TExprBase;
begin
  if i = 0 then
    result := InitExpr
  else
    result := expr;
end;

// SubExprCount
//
function TdwsProgram.SubExprCount: Integer;
begin
  result := 2;
end;

// FindLocal
//
function TdwsProgram.FindLocal(const name: String): TSymbol;
begin
  result := Table.FindLocal(name);
end;

function TdwsProgram.GetGlobalAddr(DataSize: Integer): Integer;
begin
  result := FRoot.FAddrGenerator.GetStackAddr(DataSize);
end;

function TdwsProgram.GetTempAddr(DataSize: Integer): Integer;
begin
  result := FAddrGenerator.GetStackAddr(DataSize);
end;

// ResetExprs
//
procedure TdwsProgram.ResetExprs;
begin
  FExpr.Free;
  FExpr := nil;
  FInitExpr.Free;
  FInitExpr := TBlockInitExpr.Create(cNullPos);
end;

// EnterSubTable
//
procedure TdwsProgram.EnterSubTable(subTable: TSymbolTable);
begin
  FSubTables.Add(Table);
  FTable := subTable;
end;

// LeaveSubTable
//
procedure TdwsProgram.LeaveSubTable;
var
  n: Integer;
begin
  n := FSubTables.Count - 1;
  FTable := TSymbolTable(FSubTables.List[n]);
  FSubTables.Delete(n);
end;

// SubTableDepth
//
function TdwsProgram.SubTableDepth: Integer;
begin
  result := FSubTables.Count;
end;

// SubTable
//
function TdwsProgram.subTable(depth: Integer): TSymbolTable;
begin
  result := TSymbolTable(FSubTables.List[depth]);
end;

// ContextMethodSymbol
//
function TdwsProgram.ContextMethodSymbol: TMethodSymbol;
var
  progIter: TdwsProgram;
begin
  progIter := Self;
  while (progIter <> nil) and (progIter.ClassType = TdwsProcedure) do
  begin
    if TdwsProcedure(progIter).func is TMethodSymbol then
    begin
      result := TMethodSymbol(TdwsProcedure(progIter).func);
      Exit;
    end;
    progIter := progIter.Parent;
  end;
  result := nil;
end;

// IsEmpty
//
function TdwsProgram.IsEmpty: Boolean;
begin
  result := ((FExpr = nil) or (FExpr is TNullExpr)) and
    (FInitExpr.SubExprCount = 0);
end;

// ------------------
// ------------------ TdwsMainProgram ------------------
// ------------------

// Create
//
constructor TdwsMainProgram.Create(const systemTable: ISystemSymbolTable;
  ResultType: TdwsResultType; const stackParameters: TStackParameters;
  const mainFileName: String);
var
  systemUnitTable: TLinkedSymbolTable;
  systemUnit: TUnitMainSymbol;
begin
  inherited Create(systemTable);

  FResultType := ResultType;

  FMainFileName := mainFileName;

  FExecutionsLock := TdwsCriticalSection.Create;

  FStackParameters := stackParameters;
  FStackParameters.MaxLevel := 1;

  FSourceContextMap := TdwsSourceContextMap.Create;

  FSymbolDictionary := TdwsSymbolDictionary.Create;

  FAttributes := TdwsSymbolAttributes.Create;

  FConditionalDefines := TAutoStrings.Create;
  FConditionalDefines.value.Duplicates := dupIgnore;
  FConditionalDefines.value.Sorted := True;

  FSourceList := TScriptSourceList.Create;
  FUnitList := TIdwsUnitList.Create;

  FRoot := Self;

  FUnifiedConstants := TUnifiedConstants.Create(systemTable.SymbolTable);

  FResourceStringList := TResourceStringSymbolList.Create;

  FUnitMains := TUnitMainSymbols.Create;

  FSystemTable := systemTable;
  systemUnitTable := TLinkedSymbolTable.Create(systemTable.SymbolTable);
  systemUnit := TUnitMainSymbol.Create(SYS_SYSTEM, systemUnitTable, FUnitMains);
  systemUnit.ReferenceInSymbolTable(FRootTable, True);

  FCompilerContext := TdwsCompilerContext.Create;
  FCompilerContext.Msgs := CompileMsgs;
  FCompilerContext.systemTable := FSystemTable.SymbolTable;
  FCompilerContext.UnifiedConstants := FUnifiedConstants;
  FCompilerContext.UnitList := FUnitList;

  FRoot := Self;
end;

// Destroy
//
destructor TdwsMainProgram.Destroy;
begin
  FExecutionsLock.Enter;
  try
    if FExecutions.Count > 0 then
      raise Exception.CreateFmt(RTE_ScriptHasLiveExecutions,
        [FExecutions.Count]);
  finally
    FExecutionsLock.Leave;
  end;
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);

  FExecutionsLock.Free;

  inherited;

  FCompilerContext.Free;
  FUnitList.Clear;
  FUnitList.Free;

  FFinalExpr.Free;
  FSourceContextMap.Free;
  FSymbolDictionary.Free;
  FAttributes.Free;
  FUnifiedConstants.Free;
  FResourceStringList.Free;
  FSourceList.Free;
end;

// CreateNewExecution
//
function TdwsMainProgram.CreateNewExecution: IdwsProgramExecution;
var
  exec: TdwsProgramExecution;
begin
  if CompileMsgs.HasErrors then
    raise EScriptException.Create(RTE_CantRunScript);
  if FExecutionsClass = nil then
    exec := TdwsProgramExecution.Create(Self, FStackParameters)
  else
    exec := FExecutionsClass.Create(Self, FStackParameters);
  exec.UserObject := DefaultUserObject;
  exec.Environment := DefaultEnvironment;
  exec.Localizer := DefaultLocalizer;
  exec.OnExecutionStarted := OnExecutionStarted;
  exec.OnExecutionEnded := OnExecutionEnded;
  FExecutionsLock.Enter;
  try
    FExecutions.Add(exec);
  finally
    FExecutionsLock.Leave;
  end;
  result := exec;
end;

// BeginNewExecution
//
function TdwsMainProgram.BeginNewExecution: IdwsProgramExecution;
begin
  result := CreateNewExecution;
  result.BeginProgram;
end;

// NotifyExecutionDestruction
//
procedure TdwsMainProgram.NotifyExecutionDestruction
  (exec: TdwsProgramExecution);
begin
  FExecutionsLock.Enter;
  try
    FExecutions.Remove(exec);
  finally
    FExecutionsLock.Leave;
  end;
end;

// Execute
//
function TdwsMainProgram.Execute(aTimeoutMilliSeconds: Integer = 0)
  : IdwsProgramExecution;
begin
  result := CreateNewExecution;
  result.Execute(aTimeoutMilliSeconds);
end;

// ExecuteParam
//
function TdwsMainProgram.ExecuteParam(const params: TVariantDynArray;
  aTimeoutMilliSeconds: Integer = 0): IdwsProgramExecution;
begin
  result := CreateNewExecution;
  result.ExecuteParam(params, aTimeoutMilliSeconds);
end;

// ExecuteParam
//
function TdwsMainProgram.ExecuteParam(const params: OleVariant;
  aTimeoutMilliSeconds: Integer = 0): IdwsProgramExecution;
begin
  result := CreateNewExecution;
  result.ExecuteParam(params, aTimeoutMilliSeconds);
end;

// GetMsgs
//
function TdwsMainProgram.GetMsgs: TdwsMessageList;
begin
  result := FCompileMsgs;
end;

// GetTable
//
function TdwsMainProgram.GetTable: TSymbolTable;
begin
  result := FTable;
end;

// GetTimeoutMilliseconds
//
function TdwsMainProgram.GetTimeoutMilliseconds: Integer;
begin
  result := FTimeoutMilliseconds;
end;

// SetTimeoutMilliseconds
//
procedure TdwsMainProgram.SetTimeoutMilliseconds(const val: Integer);
begin
  FTimeoutMilliseconds := val;
end;

// GetSymbolDictionary
//
function TdwsMainProgram.GetSymbolDictionary: TdwsSymbolDictionary;
begin
  result := FSymbolDictionary;
end;

// GetSourceContextMap
//
function TdwsMainProgram.GetSourceContextMap: TdwsSourceContextMap;
begin
  result := FSourceContextMap;
end;

// GetProgramObject
//
function TdwsMainProgram.GetProgramObject: TdwsMainProgram;
begin
  result := Self;
end;

// SetExecutionsClass
//
procedure TdwsMainProgram.SetExecutionsClass(aClass: TdwsProgramExecutionClass);
begin
  FExecutionsClass := aClass;
end;

// GetSourceFile
//
function TdwsMainProgram.GetSourceFile(const aSourceFile: String): TSourceFile;
var
  i: Integer;
begin
  i := FSourceList.IndexOf(aSourceFile);
  if i >= 0 then
    result := FSourceList[i].SourceFile
  else
    result := nil;
end;

// IsEmpty
//
function TdwsMainProgram.IsEmpty: Boolean;
begin
  result := (inherited IsEmpty) and
    ((FFinalExpr = nil) or (FinalExpr.SubExprCount = 0));
end;

// GetSourceList
//
function TdwsMainProgram.GetSourceList: TScriptSourceList;
begin
  result := FSourceList;
end;

// GetLineCount
//
function TdwsMainProgram.GetLineCount: Integer;
begin
  result := FLineCount;
end;

// GetTimeStamp
//
function TdwsMainProgram.GetTimeStamp: TDateTime;
begin
  result := FTimeStamp;
end;

// RecordExecution
//
procedure TdwsMainProgram.RecordExecution(const durationMSec: Integer);
begin
  FExecStats.RecordExec(durationMSec);
end;

// GetExecStats
//
function TdwsMainProgram.GetExecStats: TdwsProgramExecStats;
begin
  FExecStats.CopyTo(result);
end;

// GetDefaultUserObject
//
function TdwsMainProgram.GetDefaultUserObject: TObject;
begin
  result := FDefaultUserObject;
end;

// SetDefaultUserObject
//
procedure TdwsMainProgram.SetDefaultUserObject(const val: TObject);
begin
  FDefaultUserObject := val;
end;

// NextStackLevel
//
function TdwsMainProgram.NextStackLevel(Level: Integer): Integer;
begin
  result := Level + 1;
  if result > FStackParameters.MaxLevel then
    FStackParameters.MaxLevel := result;
end;

// DropMapAndDictionary
//
procedure TdwsMainProgram.DropMapAndDictionary;
begin
  FSymbolDictionary.Free;
  FSymbolDictionary := nil;
  FSourceContextMap.Free;
  FSourceContextMap := nil;
end;

// CollectAllPublishedSymbols
//
function TdwsMainProgram.CollectAllPublishedSymbols
  (ignoreImplementationPublished: Boolean): TSimpleSymbolList;
begin
  result := TSimpleSymbolList.Create;
  UnitMains.CollectPublishedSymbols(result, ignoreImplementationPublished);
  FRootTable.CollectPublishedSymbols(result);
end;

// OrphanObject
//
procedure TdwsMainProgram.OrphanObject(obj: TRefCountedObject);
begin
  FCompilerContext.OrphanObject(obj);
end;

// AddFinalExpr
//
procedure TdwsMainProgram.AddFinalExpr(expr: TProgramExpr);
begin
  if FFinalExpr = nil then
    FFinalExpr := TBlockFinalExpr.Create(cNullPos);
  FFinalExpr.AddStatement(expr);
end;

// GetConditionalDefines
//
function TdwsMainProgram.GetConditionalDefines: IAutoStrings;
begin
  result := FConditionalDefines;
end;

// ------------------
// ------------------ TdwsProcedure ------------------
// ------------------

// Create
//
constructor TdwsProcedure.Create(aParent: TdwsProgram);
begin
  FParent := aParent;

  // Create a local symbol table and connect it to the parent symboltable
  FAddrGenerator := TAddrGeneratorRec.CreatePositive(aParent.Level + 1);
  FRootTable := TProgramSymbolTable.Create(Parent.Table, @FAddrGenerator);
  FTable := FRootTable;
  FCompileMsgs := Parent.CompileMsgs;
  FUnitMains := Parent.UnitMains;

  FInitExpr := TBlockInitExpr.Create(cNullPos);

  // Connect the procedure to the root TdwsProgram
  FRoot := Parent.Root;
end;

destructor TdwsProcedure.Destroy;
begin
  FRootTable.Free;
  FExpr.Free;
  FInitExpr.Free;
  FPreConditions.Free;
  FPostConditions.Free;
end;

procedure TdwsProcedure.AssignTo(sym: TFuncSymbol);
begin
  // Add parameter symboltable into the symboltable chain
  FTable.InsertParent(0, sym.params);
  sym.Executable := ICallable(Self);
  FFunc := sym;
end;

// Call
//
procedure TdwsProcedure.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
  stackSize: Integer;
  oldStatus: TExecutionStatusResult;
begin
  exec.CurrentProg := Self;

  // Allocate stack space for local variables
  stackSize := DataSize;
  exec.Stack.Push(stackSize);

  // Run the procedure
  oldStatus := exec.Status;
  try
    exec.Status := esrNone;

    if FPreConditions <> nil then
      FPreConditions.EvalNoResult(exec);

    if FInitExpr.StatementCount > 0 then
    begin
      exec.DoStep(FInitExpr);
      FInitExpr.EvalNoResult(exec);
    end;

    exec.DoStep(FExpr);
    FExpr.EvalNoResult(exec);

    if FPostConditions <> nil then
      FPostConditions.EvalNoResult(exec);

  finally
    exec.Status := oldStatus;

    // Free stack space for local variables
    exec.Stack.Pop(stackSize);
  end;
end;

// CompileTimeCheck
//
procedure TdwsProcedure.CompileTimeCheck(context: TdwsCompilerContext;
  expr: TFuncExprBase);
begin
  // nothing yet
end;

procedure TdwsProcedure.InitSymbol(Symbol: TSymbol;
  const Msgs: TdwsCompileMessageList);
begin
  FTable.Initialize(Msgs);
end;

procedure TdwsProcedure.InitExpression(expr: TExprBase);
begin
end;

// OptimizeConstAssignments
//
procedure TdwsProcedure.OptimizeConstAssignments(blockExpr: TBlockExprBase);
{$IFDEF COALESCE_VAR_INITIALIZATION}
var
  i, j: Integer;
  SubExpr, initSubExpr: TExprBase;
  AssignExpr: TAssignExpr;
  assignExprSym: TDataSymbol;
  initAssign: TAssignConstExpr;
begin
  if (InitExpr.SubExprCount = 0) or (blockExpr.SubExprCount = 0) then
    Exit;

  // merges all initial constant assignments in blockExpr in InitExpr

  // valid only if InitExpr is only made of constant assignments
  for i := 0 to InitExpr.SubExprCount - 1 do
  begin
    initSubExpr := InitExpr.SubExpr[i];
    if not(initSubExpr is TAssignConstExpr) then
      Exit;
  end;

  i := 0;
  while i < blockExpr.SubExprCount do
  begin
    SubExpr := blockExpr.SubExpr[i];
    if SubExpr is TAssignConstExpr then
    begin
      AssignExpr := TAssignExpr(SubExpr);
      if AssignExpr.Left is TVarExpr then
      begin
        assignExprSym := TVarExpr(AssignExpr.Left).DataSym;
        for j := 0 to InitExpr.SubExprCount - 1 do
        begin
          initSubExpr := InitExpr.SubExpr[j];
          if initSubExpr.ClassType = AssignExpr.ClassType then
          begin
            initAssign := TAssignConstExpr(initSubExpr);
            if (initAssign.Left is TVarExpr) and
              (TVarExpr(initAssign.Left).DataSym = assignExprSym) then
            begin
              InitExpr.ReplaceStatement(j, blockExpr.ExtractStatement(i));
              Dec(i);
              Break;
            end;
          end;
        end;
      end;
    end
    else
      Break;
    Inc(i);
  end;

  // the first assignment can be moved to the last of the InitExpr
  // if its expression doesn't use the variable itself
  if i < blockExpr.SubExprCount then
  begin
    SubExpr := blockExpr.SubExpr[i];
    if (SubExpr is TAssignExpr) and not(SubExpr is TOpAssignExpr) then
    begin
      AssignExpr := TAssignExpr(SubExpr);
      if AssignExpr.Left is TVarExpr then
      begin
        assignExprSym := TVarExpr(AssignExpr.Left).DataSym;
        j := InitExpr.SubExprCount - 1;
        initSubExpr := InitExpr.SubExpr[j];
        initAssign := (initSubExpr as TAssignConstExpr);
        if (initAssign.Left is TVarExpr) and
          (TVarExpr(initAssign.Left).DataSym = assignExprSym) then
        begin
          if not AssignExpr.Right.ReferencesVariable(assignExprSym) then
            InitExpr.ReplaceStatement(j, blockExpr.ExtractStatement(i));
        end;
      end;
    end;
  end;
{$ELSE}

begin
{$ENDIF}
end;

// SetBeginPos
//
procedure TdwsProcedure.SetBeginPos(const ScriptPos: TScriptPos);
begin
  FInitExpr.FScriptPos := ScriptPos;
end;

// FindLocal
//
function TdwsProcedure.FindLocal(const name: String): TSymbol;
begin
  result := inherited FindLocal(name);
  if result = nil then
  begin
    result := FFunc.params.FindLocal(name);
    if result = nil then
      result := FFunc.InternalParams.FindLocal(name);
  end;
end;

// Specialize
//
function TdwsProcedure.Specialize(const context: ISpecializationContext)
  : IExecutable;
var
  i: Integer;
  specialized: TdwsProcedure;
  specializedParent: TRefCountedObject;
begin
  specializedParent := context.SpecializedObject(Parent);
  if specializedParent <> nil then
    specialized := TdwsProcedure.Create(specializedParent as TdwsProgram)
  else
    specialized := TdwsProcedure.Create(Parent);
  result := specialized as IExecutable;

  context.RegisterSpecializedObject(Self, specialized);

  specialized.FFunc := FFunc;

  context.SpecializeTable(FTable, specialized.FTable);

  specialized.FExpr := FExpr.SpecializeProgramExpr(context);

  for i := 0 to FInitExpr.SubExprCount - 1 do
    specialized.FInitExpr.AddStatement
      (FInitExpr.SpecializeProgramExpr(context));
end;

// ------------------
// ------------------ TdwsExceptionContext ------------------
// ------------------

// Create
//
constructor TdwsExceptionContext.Create(const aCallStack
  : TdwsExprLocationArray);
begin
  FCallStack := aCallStack
end;

// Skip
//
procedure TdwsExceptionContext.Skip(n: Integer);
var
  i: Integer;
begin
  for i := 0 to High(FCallStack) - n do
    FCallStack[i] := FCallStack[i + n];
  SetLength(FCallStack, Length(callStack) - n);
end;

// ReplaceTop
//
procedure TdwsExceptionContext.ReplaceTop(expr: TExprBase);
begin
  FCallStack[0].expr := expr;
end;

// ------------------
// ------------------ TdwsResultType ------------------
// ------------------

procedure TdwsResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
  // no symbols
end;

function TdwsResultType.CreateProgResult: TdwsResult;
begin
  result := TdwsDefaultResult.Create(Self);
end;

// ------------------
// ------------------ TdwsResult ------------------
// ------------------

constructor TdwsResult.Create(ResultType: TdwsResultType);
begin
  FResultType := ResultType;
end;

// ToDataString
//
function TdwsResult.ToDataString: RawByteString;
begin
  result := ScriptStringToRawByteString(ToString);
end;

// ToUTF8String
//
function TdwsResult.ToUTF8String: UTF8String;
begin
  result := UTF8Encode(ToString);
end;

// AddCRLF
//
procedure TdwsResult.AddCRLF;
begin
  AddString(#13#10);
end;

// AddString
//
procedure TdwsResult.AddString(const i: Int64);
begin
  AddString(IntToStr(i));
end;

// ------------------
// ------------------ TdwsDefaultResultType ------------------
// ------------------

// CreateProgResult
//
function TdwsDefaultResultType.CreateProgResult: TdwsResult;
begin
  result := TdwsDefaultResult.Create(Self);
end;

// AddResultSymbols
//
procedure TdwsDefaultResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
  inherited;
  RegisterStandardResultFunctions(SymbolTable);
end;

// ------------------
// ------------------ TdwsProgramExecStats ------------------
// ------------------

// Lock
//
procedure TdwsProgramExecStats.Lock;
begin
  while InterlockedCompareExchangePointer(FLock, @Self, nil) <> nil do;
end;

// Unlock
//
procedure TdwsProgramExecStats.Unlock;
begin
  FLock := nil;
end;

// RecordExec
//
procedure TdwsProgramExecStats.RecordExec(const durationMSec: Integer);
begin
  Lock;
  Inc(FCount);
  Inc(FTimeMSec, durationMSec);
  Unlock;
end;

// CopyTo
//
procedure TdwsProgramExecStats.CopyTo(var dest: TdwsProgramExecStats);
begin
  Lock;
  dest := Self;
  dest.FLock := nil;
  Unlock;
end;

// ------------------
// ------------------ TdwsDefaultResult ------------------
// ------------------

// Create
//
constructor TdwsDefaultResult.Create(ResultType: TdwsResultType);
begin
  inherited;
  FTextBuilder := TWriteOnlyBlockStream.AllocFromPool;
end;

// Destroy
//
destructor TdwsDefaultResult.Destroy;
begin
  inherited;
  FTextBuilder.ReturnToPool;
end;

// AddString
//
procedure TdwsDefaultResult.AddString(const str: String);
begin
  FTextBuilder.WriteString(str);
end;

// AddString
//
procedure TdwsDefaultResult.AddString(const i: Int64);
begin
  FTextBuilder.WriteString(i);
end;

// AddCRLF
//
procedure TdwsDefaultResult.AddCRLF;
begin
  FTextBuilder.WriteCRLF;
end;

// Clear
//
procedure TdwsDefaultResult.Clear;
begin
  FTextBuilder.Clear;
end;

// ToString
//
function TdwsDefaultResult.ToString: String;
begin
  result := GetText;
end;

// ToDataString
//
function TdwsDefaultResult.ToDataString: RawByteString;
begin
  result := ScriptStringToRawByteString(FTextBuilder.ToString);
end;

// GetText
//
function TdwsDefaultResult.GetText: String;
begin
  result := FTextBuilder.ToString;
end;

// ------------------
// ------------------ TdwsGuardedExecution ------------------
// ------------------

// FreeAll
//
procedure TdwsGuardedExecution.FreeAll;
begin
  if Assigned(Self) then
  begin
    Next.FreeAll;
    Destroy;
  end;
end;

// ------------------
// ------------------ TdwsGuardianThread ------------------
// ------------------

// Create
//
constructor TdwsGuardianThread.Create;
begin
  FEvent := TEvent.Create(nil, False, False, '');
  FExecutionsLock := TdwsCriticalSection.Create;
  FreeOnTerminate := False;

  inherited Create(True);

  SetTimeCriticalPriority;
end;

// Destroy
//
destructor TdwsGuardianThread.Destroy;
begin
  FExecutions.FreeAll;
  FEvent.Free;
  FExecutionsLock.Free;
  inherited;
end;

// Initialize
//
class procedure TdwsGuardianThread.Initialize;
begin
  vThread := TdwsGuardianThread.Create;
  vThread.Start;
end;

// Finalize
//
class procedure TdwsGuardianThread.Finalize;
var
  guardian: TdwsGuardianThread;
begin
  if vThread <> nil then
  begin
    guardian := vThread;
    vThread := nil;
    guardian.Terminate;
    guardian.FEvent.SetEvent;
    guardian.WaitFor;
    guardian.Destroy;
  end;
end;

// GuardExecution
//
class procedure TdwsGuardianThread.GuardExecution
  (const exec: IdwsProgramExecution; aMilliSecToLive: Integer);
var
  thread: TdwsGuardianThread;
  item, prev, iter: TdwsGuardedExecution;
  TimeOutAt: Int64;
begin
  thread := vThread;
  TimeOutAt := GetSystemMilliseconds + aMilliSecToLive;
  thread.FExecutionsLock.Enter;
  try
    if vExecutionsPool <> nil then
    begin
      item := vExecutionsPool;
      vExecutionsPool := item.Next;
      item.Next := nil;
    end
    else
      item := TdwsGuardedExecution.Create;
    item.exec := exec;
    item.TimeOutAt := TimeOutAt;

    iter := thread.FExecutions;
    if iter = nil then
      thread.FExecutions := item
    else if iter.TimeOutAt >= item.TimeOutAt then
    begin
      item.Next := thread.FExecutions;
      thread.FExecutions := item
    end
    else
    begin
      repeat
        prev := iter;
        iter := iter.Next;
      until (iter = nil) or (iter.TimeOutAt >= item.TimeOutAt);
      item.Next := iter;
      prev.Next := item;
    end;
  finally
    thread.FExecutionsLock.Leave;
  end;
  if aMilliSecToLive < PulseIntervalMilliSeconds then
    thread.FEvent.SetEvent;
end;

// ForgetExecution
//
class procedure TdwsGuardianThread.ForgetExecution
  (const exec: IdwsProgramExecution);
var
  thread: TdwsGuardianThread;
  iter, prev: TdwsGuardedExecution;
begin
  thread := vThread;
  if thread = nil then
    Exit;
  thread.FExecutionsLock.Enter;
  try
    iter := thread.FExecutions;
    if iter <> nil then
    begin
      if iter.exec = exec then
        thread.FExecutions := iter.Next
      else
      begin
        repeat
          prev := iter;
          iter := iter.Next;
        until (iter = nil) or (iter.exec = exec);
        if iter <> nil then
          prev.Next := iter.Next;
      end;
      if iter <> nil then
      begin
        iter.exec := nil;
        iter.Next := vExecutionsPool;
        vExecutionsPool := iter;
      end;
    end;
  finally
    thread.FExecutionsLock.Leave;
  end;
end;

// Execute
//
procedure TdwsGuardianThread.Execute;
var
  currentTime: Int64;
  item: TdwsGuardedExecution;
  millisecs: Cardinal;
  timeLeft: Int64;
begin
  SetThreadName('DWScript Guardian');

  while not Terminated do
  begin

    timeLeft := INFINITE;

    if FExecutions <> nil then
    begin

      currentTime := GetSystemMilliseconds;

      FExecutionsLock.Enter;
      try
        item := FExecutions;
        while (item <> nil) and (item.TimeOutAt <= currentTime) do
        begin
          item.exec.StopForTimeout;
          FExecutions := item.Next;
          item.exec := nil;
          item.Next := vExecutionsPool;
          vExecutionsPool := item;
          item := FExecutions;
        end;
        item := FExecutions;
        if item <> nil then
          timeLeft := item.TimeOutAt - currentTime;
      finally
        FExecutionsLock.Leave;
      end;

    end;

    Assert(timeLeft > 0);
    if timeLeft > PulseIntervalMilliSeconds then
      millisecs := PulseIntervalMilliSeconds
    else
      millisecs := timeLeft;
    FEvent.WaitFor(millisecs);

  end;
end;

// ------------------
// ------------------ TProgramExpr ------------------
// ------------------

// Optimize
//
function TProgramExpr.Optimize(context: TdwsCompilerContext): TProgramExpr;
begin
  result := Self;
end;

// Orphan
//
procedure TProgramExpr.Orphan(context: TdwsCompilerContext);
begin
  context.OrphanObject(Self);
end;

// Specialize
//
function TProgramExpr.Specialize(const context: ISpecializationContext)
  : TExprBase;
begin
  result := SpecializeProgramExpr(context);
end;

// SpecializeProgramExpr
//
function TProgramExpr.SpecializeProgramExpr(const context
  : ISpecializationContext): TProgramExpr;
begin
  context.AddCompilerErrorFmt('Unsupported specialization of %s', [ClassName]);
  result := nil;
end;

// GetType
//
function TProgramExpr.GetType: TTypeSymbol;
begin
  result := nil;
end;

// GetBaseType
//
function TProgramExpr.GetBaseType: TTypeSymbol;
begin
  result := nil;
end;

// EvalAsInterface
//
procedure TProgramExpr.EvalAsInterface(exec: TdwsExecution;
  var result: IUnknown);
var
  buf: Variant;
begin
  EvalAsVariant(exec, buf);
  Assert(VariantType(buf) = varUnknown);
  result := IUnknown(TVarData(buf).VUnknown);
end;

// EvalAsScriptObj
//
procedure TProgramExpr.EvalAsScriptObj(exec: TdwsExecution;
  var result: IScriptObj);
begin
  EvalAsInterface(exec, IUnknown(result));
  result := IUnknown(result) as IScriptObj;
end;

// EvalAsScriptObjInterface
//
procedure TProgramExpr.EvalAsScriptObjInterface(exec: TdwsExecution;
  var result: IScriptObjInterface);
var
  buf: Variant;
begin
  EvalAsVariant(exec, buf);
  Assert(VariantType(buf) = varUnknown);
  result := (IUnknown(TVarData(buf).VUnknown) as IScriptObjInterface);
end;

// EvalAsScriptDynArray
//
procedure TProgramExpr.EvalAsScriptDynArray(exec: TdwsExecution;
  var result: IScriptDynArray);
begin
  EvalAsInterface(exec, IUnknown(result));
end;

// EvalAsScriptAssociativeArray
//
procedure TProgramExpr.EvalAsScriptAssociativeArray(exec: TdwsExecution;
  var result: IScriptAssociativeArray);
begin
  EvalAsInterface(exec, IUnknown(result));
end;

// AssignValue
//
procedure TProgramExpr.AssignValue(exec: TdwsExecution; const value: Variant);
begin
  raise EScriptError.CreateFmt('Cannot assign to %s', [ClassName]);
end;

// AssignValueAsInteger
//
procedure TProgramExpr.AssignValueAsInteger(exec: TdwsExecution;
  const value: Int64);
begin
  AssignValue(exec, value);
end;

// AssignValueAsBoolean
//
procedure TProgramExpr.AssignValueAsBoolean(exec: TdwsExecution;
  const value: Boolean);
begin
  AssignValue(exec, value);
end;

// AssignValueAsFloat
//
procedure TProgramExpr.AssignValueAsFloat(exec: TdwsExecution;
  const value: Double);
begin
  AssignValue(exec, value);
end;

// AssignValueAsString
//
procedure TProgramExpr.AssignValueAsString(exec: TdwsExecution;
  const value: String);
begin
  AssignValue(exec, value);
end;

// AssignValueAsScriptObj
//
procedure TProgramExpr.AssignValueAsScriptObj(exec: TdwsExecution;
  const value: IScriptObj);
begin
  AssignValue(exec, value);
end;

// AssignValueAsScriptDynArray
//
procedure TProgramExpr.AssignValueAsScriptDynArray(exec: TdwsExecution;
  const value: IScriptDynArray);
begin
  AssignValue(exec, value);
end;

// EvalAsInteger
//
function TProgramExpr.EvalAsInteger(exec: TdwsExecution): Int64;
var
  v: Variant;
begin
  EvalAsVariant(exec, v);
  result := VariantToInt64(v);
end;

// EvalAsBoolean
//
function TProgramExpr.EvalAsBoolean(exec: TdwsExecution): Boolean;
var
  v: Variant;
begin
  EvalAsVariant(exec, v);
  result := VariantToBool(v);
end;

// EvalAsFloat
//
function TProgramExpr.EvalAsFloat(exec: TdwsExecution): Double;
var
  v: Variant;
begin
  EvalAsVariant(exec, v);
  result := VariantToFloat(v);
end;

// EvalAsString
//
procedure TProgramExpr.EvalAsString(exec: TdwsExecution; var result: String);
var
  v: Variant;
  p: PVarData;
begin
  EvalAsVariant(exec, v);
  p := PVarData(@v);
{$IFDEF FPC}
  if p^.VType = varString then
    result := String(p.VString)
{$ELSE}
  if p^.VType = varUString then
    result := String(p.VUString)
{$ENDIF}
  else
    VariantToString(v, result);
end;

// EvalAsUnicodeString
//
procedure TProgramExpr.EvalAsUnicodeString(exec: TdwsExecution;
  var result: UnicodeString);
{$IFDEF FPC}
var
  s: String;
begin
  EvalAsString(exec, s);
  result := UnicodeString(s);
{$ELSE}
begin
  EvalAsString(exec, result);
{$ENDIF}
end;

// RaiseUpperExceeded
//
procedure TProgramExpr.RaiseUpperExceeded(exec: TdwsExecution; index: Integer);
begin
  RaiseScriptError(exec, EScriptOutOfBounds.CreateFmt
    (RTE_ArrayUpperBoundExceeded, [index]));
end;

// RaiseLowerExceeded
//
procedure TProgramExpr.RaiseLowerExceeded(exec: TdwsExecution; index: Integer);
begin
  RaiseScriptError(exec, EScriptOutOfBounds.CreateFmt
    (RTE_ArrayLowerBoundExceeded, [index]));
end;

// BoundsCheckFailed
//
procedure TProgramExpr.BoundsCheckFailed(exec: TdwsExecution; index: Integer);
begin
  if index < 0 then
    RaiseLowerExceeded(exec, index)
  else
    RaiseUpperExceeded(exec, index);
end;

// CheckScriptObject
//
procedure TProgramExpr.CheckScriptObject(exec: TdwsExecution;
  const scriptObj: IScriptObj);
begin
  if scriptObj = nil then
    RaiseObjectNotInstantiated(exec)
  else if scriptObj.Destroyed then
    RaiseObjectAlreadyDestroyed(exec);
end;

// ScriptLocation
//
function TProgramExpr.ScriptLocation(Prog: TObject): String;
begin
  if Prog is TdwsProcedure then
    result := TdwsProcedure(Prog).func.QualifiedName + ScriptPos.AsInfo
  else
    result := ScriptPos.AsInfo;
end;

// EnumerateSteppableExprs
//
procedure TProgramExpr.EnumerateSteppableExprs(const callback: TExprBaseProc);
begin
  // empty
end;

// InterruptsFlow
//
function TProgramExpr.InterruptsFlow: Boolean;
begin
  result := False;
end;

// ------------------
// ------------------ TProgramExpr ------------------
// ------------------

// OptimizeToTypedExpr
//
function TTypedExpr.OptimizeToTypedExpr(context: TdwsCompilerContext;
  const hotPos: TScriptPos): TTypedExpr;
var
  optimized: TProgramExpr;
begin
  try
    optimized := Optimize(context);
    if optimized <> Self then
    begin
      Assert(optimized is TTypedExpr);
      result := TTypedExpr(optimized);
    end
    else
      result := Self;
  except
    on e: Exception do
    begin
      context.Msgs.AddCompilerException(hotPos, e);
      result := Self;
    end;
  end;
end;

// OptimizeToFloatConstant
//
function TTypedExpr.OptimizeToFloatConstant(context: TdwsCompilerContext)
  : TTypedExpr;
begin
  if IsConstant then
  begin
    if Typ.IsOfType(context.TypInteger) or Typ.IsOfType(context.TypFloat) then
    begin
      result := TConstFloatExpr.Create(cNullPos, context.TypFloat,
        EvalAsFloat(context.Execution));
      Orphan(context);
    end
    else
      result := OptimizeToTypedExpr(context, ScriptPos);
  end
  else
    result := Self;
end;

// SpecializeProgramExpr
//
function TTypedExpr.SpecializeProgramExpr(const context: ISpecializationContext)
  : TProgramExpr;
begin
  result := SpecializeTypedExpr(context);
end;

// SpecializeTypedExpr
//
function TTypedExpr.SpecializeTypedExpr(const context: ISpecializationContext)
  : TTypedExpr;
begin
  context.AddCompilerErrorFmt('Unsupported specialization of %s', [ClassName]);
  result := nil;
end;

// SpecializeBooleanExpr
//
function TTypedExpr.SpecializeBooleanExpr(const context: ISpecializationContext)
  : TTypedExpr;
begin
  result := SpecializeTypedExpr(context);
  if (result <> nil) and not result.Typ.IsOfType
    (CompilerContextFromSpecialization(context).TypBoolean) then
    context.AddCompilerError(CPE_BooleanExpected);
end;

// SpecializeIntegerExpr
//
function TTypedExpr.SpecializeIntegerExpr(const context: ISpecializationContext)
  : TTypedExpr;
begin
  result := SpecializeTypedExpr(context);
  if (result <> nil) and not result.Typ.IsOfType
    (CompilerContextFromSpecialization(context).TypInteger) then
    context.AddCompilerError(CPE_IntegerExpected);
end;

// ScriptPos
//
function TTypedExpr.ScriptPos: TScriptPos;
begin
  result := cNullPos;
end;

// CheckInterface
//
procedure TTypedExpr.CheckInterface(exec: TdwsExecution;
  const scriptObj: IScriptObj);
begin
  if scriptObj = nil then
    RaiseInterfaceIsNil(exec)
end;

// CheckInterface
//
procedure TTypedExpr.CheckInterface(exec: TdwsExecution;
  const intf: IScriptObjInterface);
begin
  if intf = nil then
    RaiseInterfaceIsNil(exec)
end;

// RaiseInterfaceIsNil
//
procedure TTypedExpr.RaiseInterfaceIsNil(exec: TdwsExecution);
begin
  RaiseScriptError(exec, EScriptError, RTE_IntfIsNil);
end;

// IsOfType
//
function TTypedExpr.IsOfType(typSym: TTypeSymbol): Boolean;
begin
  result := (Self <> nil) and (Typ <> nil) and (Typ.IsOfType(typSym));
end;

// IsGeneric
//
function TTypedExpr.IsGeneric: Boolean;
begin
  result := (Typ <> nil) and (Typ.IsGeneric);
end;

// IsWritable
//
function TTypedExpr.IsWritable: Boolean;
begin
  result := False;
end;

// AssignsAsDataExpr
//
function TTypedExpr.AssignsAsDataExpr: Boolean;
begin
  result := False;
end;

// SameDataExpr
//
function TTypedExpr.SameDataExpr(expr: TTypedExpr): Boolean;
begin
  result := False;
end;

// GetBaseType
//
function TTypedExpr.GetBaseType: TTypeSymbol;
begin
  if Assigned(Typ) then
    result := Typ.BaseType
  else
    result := nil;
end;

// GetType
//
function TTypedExpr.GetType: TTypeSymbol;
begin
  result := FTyp;
end;

// ------------------
// ------------------ TTypeReferenceExpr ------------------
// ------------------

// Create
//
constructor TTypeReferenceExpr.Create(aTyp: TTypeSymbol;
  const aScriptPos: TScriptPos);
begin
  inherited Create;
  Typ := aTyp;
  FScriptPos := aScriptPos;
end;

// EvalAsVariant
//
procedure TTypeReferenceExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  Assert(False);
end;

// ScriptPos
//
function TTypeReferenceExpr.ScriptPos: TScriptPos;
begin
  result := FScriptPos;
end;

// ------------------
// ------------------ TNoResultExpr ------------------
// ------------------

// Create
//
constructor TNoResultExpr.Create(const aPos: TScriptPos);
begin
  inherited Create;
  FScriptPos := aPos;
end;

// EvalAsVariant
//
procedure TNoResultExpr.EvalAsVariant(exec: TdwsExecution; var result: Variant);
begin
  EvalNoResult(exec);
  Assert(exec.Status = esrNone);
end;

// EvalNoResult
//
procedure TNoResultExpr.EvalNoResult(exec: TdwsExecution);
begin
  // nothing
end;

// ScriptPos
//
function TNoResultExpr.ScriptPos: TScriptPos;
begin
  result := FScriptPos;
end;

// SetScriptPos
//
procedure TNoResultExpr.SetScriptPos(const aPos: TScriptPos);
begin
  FScriptPos := aPos;
end;

// ------------------
// ------------------ TNullExpr ------------------
// ------------------

procedure TNullExpr.EvalNoResult(exec: TdwsExecution);
begin
  // nothing
end;

// SpecializeProgramExpr
//
function TNullExpr.SpecializeProgramExpr(const context: ISpecializationContext)
  : TProgramExpr;
begin
  result := TNullExpr.Create(ScriptPos);
end;

// ------------------
// ------------------ TErrorValueExpr ------------------
// ------------------

// Create
//
constructor TErrorValueExpr.Create(aTyp: TAnyTypeSymbol);
begin
  inherited Create;
  Typ := aTyp;
end;

// EvalAsVariant
//
procedure TErrorValueExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  Assert(False);
end;

// ------------------
// ------------------ TBlockExprBase ------------------
// ------------------

// Destroy
//
destructor TBlockExprBase.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FStatements[i].Free;
  FreeMem(FStatements);
  inherited;
end;

// Orphan
//
procedure TBlockExprBase.Orphan(context: TdwsCompilerContext);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    context.OrphanObject(FStatements[i]);
  FCount := 0;
  DecRefCount;
end;

// AddStatement
//
procedure TBlockExprBase.AddStatement(expr: TProgramExpr);
begin
  ReallocMem(FStatements, (FCount + 1) * SizeOf(TNoResultExpr));
  FStatements[FCount] := expr;
  Inc(FCount);
end;

// ReplaceStatement
//
procedure TBlockExprBase.ReplaceStatement(index: Integer; expr: TProgramExpr);
begin
  FStatements[index].Free;
  FStatements[index] := expr;
end;

// ExtractStatement
//
function TBlockExprBase.ExtractStatement(index: Integer): TProgramExpr;
begin
  result := FStatements[index];
  if index < FCount - 1 then
    Move(FStatements[index + 1], FStatements[index], (FCount - index - 1) *
      SizeOf(TNoResultExpr));
  Dec(FCount);
end;

// SpecializeProgramExpr
//
function TBlockExprBase.SpecializeProgramExpr(const context
  : ISpecializationContext): TProgramExpr;
var
  i: Integer;
  blockExpr: TBlockExpr;
begin
  blockExpr := TBlockExpr.Create(CompilerContextFromSpecialization(context),
    ScriptPos);

  SpecializeTable(context, blockExpr);
  for i := 0 to FCount - 1 do
    blockExpr.AddStatement(FStatements[i].SpecializeProgramExpr(context));

  result := blockExpr;
end;

// EnumerateSteppableExprs
//
procedure TBlockExprBase.EnumerateSteppableExprs(const callback: TExprBaseProc);
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    callback(FStatements[i]);
end;

// Taxonomy
//
function TBlockExprBase.Taxonomy: TExprBaseTaxonomy;
begin
  result := ebtBlockExprBase;
end;

// SpecializeTable
//
procedure TBlockExprBase.SpecializeTable(const context: ISpecializationContext;
  destination: TBlockExprBase);
begin
  // no table by default
end;

// GetSubExpr
//
function TBlockExprBase.GetSubExpr(i: Integer): TExprBase;
begin
  result := FStatements[i];
end;

// GetSubExprCount
//
function TBlockExprBase.GetSubExprCount: Integer;
begin
  result := FCount;
end;

// ------------------
// ------------------ TBlockRawExpr ------------------
// ------------------

// EvalNoResult
//
procedure TBlockRawExpr.EvalNoResult(exec: TdwsExecution);
var
  i: Integer;
  expr: PProgramExpr;
begin
  expr := @FStatements[0];
  for i := 1 to FCount do
  begin
    expr.EvalNoResult(exec);
    Inc(expr);
  end;
end;

// ------------------
// ------------------ TDataExpr ------------------
// ------------------

constructor TDataExpr.Create(const aScriptPos: TScriptPos; aTyp: TTypeSymbol);
begin
  inherited Create;
  FScriptPos := aScriptPos;
  FTyp := aTyp;
end;

// EvalAsVariant
//
procedure TDataExpr.EvalAsVariant(exec: TdwsExecution; var result: Variant);
begin
  DataPtr[exec].EvalAsVariant(0, result);
end;

// IsWritable
//
function TDataExpr.IsWritable: Boolean;
begin
  result := True;
end;

// IsExternal
//
function TDataExpr.IsExternal: Boolean;
begin
  result := False;
end;

// AssignsAsDataExpr
//
function TDataExpr.AssignsAsDataExpr: Boolean;
begin
  result := Typ.AssignsAsDataExpr;
end;

// DataSymbol
//
function TDataExpr.DataSymbol: TDataSymbol;
begin
  result := nil;
end;

// GetDataPtrFunc
//
function TDataExpr.GetDataPtrFunc(exec: TdwsExecution): IDataContext;
begin
  GetDataPtr(exec, result);
end;

// AssignData
//
procedure TDataExpr.AssignData(exec: TdwsExecution; const source: IDataContext);
begin
  Assert(IsWritable);
  if Typ.Size > 0 then
    DataPtr[exec].WriteData(source, Typ.Size);
end;

// AssignValue
//
procedure TDataExpr.AssignValue(exec: TdwsExecution; const value: Variant);
begin
  Assert(IsWritable);
  DataPtr[exec][0] := value;
end;

// AssignExpr
//
procedure TDataExpr.AssignExpr(exec: TdwsExecution; expr: TTypedExpr);
var
  buf: Variant;
begin
  expr.EvalAsVariant(exec, buf);
  DataPtr[exec].AsVariant[0] := buf;
end;

procedure TDataExpr.AssignDataExpr(exec: TdwsExecution; DataExpr: TDataExpr);
begin
  DataPtr[exec].WriteData(DataExpr.DataPtr[exec], Typ.Size);
end;

// GetRelativeDataPtr
//
procedure TDataExpr.GetRelativeDataPtr(exec: TdwsExecution;
  var result: IDataContext);
begin
  GetDataPtr(exec, result);
end;

// ScriptPos
//
function TDataExpr.ScriptPos: TScriptPos;
begin
  result := FScriptPos;
end;

// IncValue
//
function TDataExpr.IncValue(exec: TdwsExecution; delta: Int64): Int64;
begin
  result := DataPtr[exec].IncInteger(0, delta);
end;

// SpecializeTypedExpr
//
function TDataExpr.SpecializeTypedExpr(const context: ISpecializationContext)
  : TTypedExpr;
begin
  result := SpecializeDataExpr(context);
end;

// SpecializeDataExpr
//
function TDataExpr.SpecializeDataExpr(const context: ISpecializationContext)
  : TDataExpr;
begin
  context.AddCompilerErrorFmt('Unsupported specialization of %s', [ClassName]);
  result := nil;
end;

// ------------------
// ------------------ TFuncExprBase ------------------
// ------------------

// Create
//
constructor TFuncExprBase.Create(const aScriptPos: TScriptPos;
  aFunc: TFuncSymbol);
begin
  inherited Create(aScriptPos, nil);
  FFunc := aFunc;
  if Assigned(aFunc) then
  begin
    FTyp := aFunc.Typ;
    FParamSize := aFunc.ParamSize;
  end;
end;

// Destroy
//
destructor TFuncExprBase.Destroy;
begin
  FArgs.Clean;
  inherited;
end;

// Orphan
//
procedure TFuncExprBase.Orphan(context: TdwsCompilerContext);
var
  i: Integer;
begin
  for i := 0 to FArgs.Count - 1 do
    TTypedExpr(FArgs.ExprBase[i]).Orphan(context);
  FArgs.Clear;
  DecRefCount;
end;

// AddArg
//
procedure TFuncExprBase.AddArg(arg: TTypedExpr);
begin
  FArgs.Add(arg);
end;

// ClearArgs
//
procedure TFuncExprBase.ClearArgs;
begin
  FArgs.Clean;
end;

// Optimize
//
function TFuncExprBase.Optimize(context: TdwsCompilerContext): TProgramExpr;
var
  buf: Variant;
  Prog: TdwsProgram;
  resultDC: IDataContext;
begin
  result := Self;
  if IsConstant then
  begin
    Prog := context.Prog as TdwsProgram;
    Initialize(context);
    try
      if (Typ = nil) or (Typ.Size <= 1) then
      begin
        EvalAsVariant(context.Execution, buf);
        result := (context.CreateConstExpr(Typ, buf) as TProgramExpr);
      end
      else
      begin
        context.Execution.Stack.Push(Prog.DataSize);
        try
          GetDataPtr(context.Execution, resultDC);
          result := TConstExpr.CreateData(ScriptPos, Typ, resultDC);
        finally
          context.Execution.Stack.Pop(Prog.DataSize);
        end;
      end;
    except
      on e: EScriptError do
      begin
        Prog.CompileMsgs.AddCompilerErrorFmt(e.ScriptPos,
          CPE_FunctionOptimizationFailed, [funcSym.name, e.RawClassName,
          e.Message], TCompilerErrorMessage);
      end;
      on e: Exception do
      begin
        Prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos,
          CPE_FunctionOptimizationFailed, [funcSym.name, e.ClassName,
          e.Message], TCompilerErrorMessage);
      end;
    end;
    if result <> Self then
      Orphan(context);
  end;
end;

// CompileTimeCheck
//
procedure TFuncExprBase.CompileTimeCheck(context: TdwsCompilerContext);
begin
  // nothing here
end;

// GetIsConstant
//
function TFuncExprBase.GetIsConstant: Boolean;
var
  i: Integer;
begin
  if (funcSym <> nil) and (not funcSym.IsStateless) then
    Exit(False);

  for i := 0 to FArgs.Count - 1 do
    if not TTypedExpr(FArgs.ExprBase[i]).IsConstant then
      Exit(False);

  result := True;
end;

// InitializeResultAddr
//
procedure TFuncExprBase.InitializeResultAddr(Prog: TdwsProgram);
begin
  FResultAddr := Prog.GetTempAddr(FTyp.Size);
end;

// SetResultAddr
//
procedure TFuncExprBase.SetResultAddr(aResultAddr: Integer);
begin
  FResultAddr := aResultAddr;
end;

// GetSubExpr
//
function TFuncExprBase.GetSubExpr(i: Integer): TExprBase;
begin
  result := FArgs.ExprBase[i];
end;

// GetSubExprCount
//
function TFuncExprBase.GetSubExprCount: Integer;
begin
  result := FArgs.Count;
end;

// ChangeFuncSymbol
//
function TFuncExprBase.ChangeFuncSymbol(context: TdwsCompilerContext;
  newFuncSym: TFuncSymbol; options: TCreateFunctionOptions): TFuncExprBase;
begin
  if (funcSym is TMagicFuncSymbol) or (newFuncSym is TMagicFuncSymbol) then
  begin
    result := TMagicFuncExpr.CreateMagicFuncExpr(context, ScriptPos,
      TMagicFuncSymbol(newFuncSym));
    result.args.Assign(args);
    args.Clear;
    TMagicFuncExpr(result).FResultAddr := FResultAddr;
    Free;
  end
  else
  begin
    FFunc := newFuncSym;
    FParamSize := newFuncSym.ParamSize;
    if Assigned(newFuncSym) then
    begin
      // don't update type for a constructor as the "return type" of a constructor
      // isn't specified by the constructor symbol, but by the meta the constructor
      // is invoked upon
      if newFuncSym.Kind <> fkConstructor then
        FTyp := newFuncSym.Typ
    end
    else
      FTyp := nil;
    result := Self;
  end
end;

// GetDataPtr
//
procedure TFuncExprBase.GetDataPtr(exec: TdwsExecution;
  var result: IDataContext);
var
  buf: Variant;
begin
  EvalAsVariant(exec, buf);
  exec.DataContext_CreateBase(FResultAddr, result);
end;

// Initialize
//
procedure TFuncExprBase.Initialize(Compiler: TdwsCompilerContext);
begin
  if Assigned(FFunc) and Assigned(FFunc.Executable) then
    FFunc.Executable.InitExpression(Self);
end;

// GetArgType
//
function TFuncExprBase.GetArgType(idx: Integer): TTypeSymbol;
var
  expr: TTypedExpr;
begin
  if Cardinal(idx) < Cardinal(FArgs.Count) then
  begin
    expr := TTypedExpr(FArgs[idx]);
    if expr <> nil then
      result := expr.Typ
    else
      result := nil;
  end
  else
    result := nil;
end;

// ArgIsOfType
//
function TFuncExprBase.ArgIsOfType(idx: Integer; aTyp: TTypeSymbol): Boolean;
var
  argTyp: TTypeSymbol;
begin
  argTyp := GetArgType(idx);
  result := (argTyp <> nil) and argTyp.IsOfType(aTyp);
end;

// ------------------
// ------------------ TPushOperator ------------------
// ------------------

// InitPushAddr
//
procedure TPushOperator.InitPushAddr(addr: Integer; argExpr: TTypedExpr);
begin
  if argExpr is TVarParamExpr then
    FTypeParamSym := TSymbol(potPassAddr)
  else
    FTypeParamSym := TSymbol(potAddr);
  FStackAddr := addr;
  FArgExpr := argExpr;
end;

// InitPushTempAddr
//
procedure TPushOperator.InitPushTempAddr(stackAddr: Integer;
  argExpr: TTypedExpr);
var
  Typ: TTypeSymbol;
begin
  Typ := argExpr.Typ;
  if (Typ <> nil) and (Typ.Size > 1) then
    FTypeParamSym := TSymbol(potTempData)
  else
    FTypeParamSym := TSymbol(potTempAddr);
  FStackAddr := stackAddr;
  FArgExpr := argExpr;
end;

// InitPushTempArrayAddr
//
procedure TPushOperator.InitPushTempArrayAddr(stackAddr: Integer;
  argExpr: TTypedExpr);
begin
  FTypeParamSym := TSymbol(potTempArrayAddr);
  FStackAddr := stackAddr;
  FArgExpr := argExpr;
end;

// InitPushTempArray
//
procedure TPushOperator.InitPushTempArray(stackAddr: Integer;
  argExpr: TTypedExpr);
begin
  FTypeParamSym := TSymbol(potTempArray);
  FStackAddr := stackAddr;
  if argExpr is TConstParamExpr then
    FArgExpr := argExpr
  else
    FArgExpr := nil; // error caught earlier, if not, ensure runtime crash
end;

// InitPushArrayExpr
//
procedure TPushOperator.InitPushArrayExpr(stackAddr: Integer;
  argExpr: TTypedExpr);
begin
  Assert(argExpr is TDynamicArrayExpr);
  FTypeParamSym := TSymbol(potArrayExpr);
  FStackAddr := stackAddr;
  FArgExpr := argExpr;
end;

// InitPushResult
//
procedure TPushOperator.InitPushResult(context: TdwsCompilerContext;
  stackAddr: Integer; argExpr: TTypedExpr);
var
  argTyp: TTypeSymbol;
begin
  argTyp := argExpr.Typ;
  if argTyp.IsOfType(context.TypInteger) then
    FTypeParamSym := TSymbol(potResultInteger)
  else if argTyp.IsOfType(context.TypFloat) then
    FTypeParamSym := TSymbol(potResultFloat)
  else if argTyp.IsOfType(context.TypBoolean) then
    FTypeParamSym := TSymbol(potResultBoolean)
  else if argTyp.IsOfType(context.TypString) then
    if argExpr.InheritsFrom(TConstStringExpr) then
      FTypeParamSym := TSymbol(potResultConstString)
    else
      FTypeParamSym := TSymbol(potResultString)
  else
    FTypeParamSym := TSymbol(potResult);
  FStackAddr := stackAddr;
  FArgExpr := argExpr;
end;

// InitPushData
//
procedure TPushOperator.InitPushData(stackAddr: Integer; argExpr: TTypedExpr;
  ParamSym: TSymbol);
begin
  FStackAddr := stackAddr;
  FArgExpr := argExpr;
  if argExpr is TConstExpr then
    FTypeParamSym := TSymbol(potConstData)
  else
    FTypeParamSym := ParamSym;
end;

// InitPushInitResult
//
procedure TPushOperator.InitPushInitResult(stackAddr: Integer;
  argExpr: TTypedExpr);
begin
  FTypeParamSym := TSymbol(potInitResult);
  FStackAddr := stackAddr;
  FArgExpr := TTypedExpr((argExpr as TFuncExpr).Typ); // dirty hack
end;

// InitPushLazy
//
procedure TPushOperator.InitPushLazy(stackAddr: Integer; argExpr: TTypedExpr);
begin
  FTypeParamSym := TSymbol(potLazy);
  FStackAddr := stackAddr;
  FArgExpr := argExpr;
end;

// Execute
//
procedure TPushOperator.Execute(exec: TdwsExecution);
begin
  case NativeInt(FTypeParamSym) of
    NativeInt(potAddr):
      ExecuteAddr(exec);
    NativeInt(potPassAddr):
      ExecutePassAddr(exec);
    NativeInt(potTempAddr):
      ExecuteTempAddr(exec);
    NativeInt(potTempData):
      ExecuteTempData(exec);
    NativeInt(potTempArrayAddr):
      ExecuteTempArrayAddr(exec);
    NativeInt(potTempArray):
      ExecuteTempArray(exec);
    NativeInt(potArrayExpr):
      ExecuteArrayExpr(exec);
    NativeInt(potResultBoolean):
      ExecuteResultBoolean(exec);
    NativeInt(potResultInteger):
      ExecuteResultInteger(exec);
    NativeInt(potResultFloat):
      ExecuteResultFloat(exec);
    NativeInt(potResultString):
      ExecuteResultString(exec);
    NativeInt(potResultConstString):
      ExecuteResultConstString(exec);
    NativeInt(potResult):
      ExecuteResult(exec);
    NativeInt(potConstData):
      ExecuteConstData(exec);
    NativeInt(potInitResult):
      ExecuteInitResult(exec);
    NativeInt(potLazy):
      ExecuteLazy(exec);
  else
    ExecuteData(exec);
  end;
end;

// ExecuteAddr
//
procedure TPushOperator.ExecuteAddr(exec: TdwsExecution);
var
  dc: IDataContext;
begin
  TDataExpr(FArgExpr).GetRelativeDataPtr(exec, dc);
  exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer + FStackAddr, dc);
end;

// ExecutePassAddr
//
procedure TPushOperator.ExecutePassAddr(exec: TdwsExecution);
var
  vpd: Pointer;
begin
  vpd := TVarParamExpr(FArgExpr).GetVarParamDataAsPointer(exec);
  exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer + FStackAddr,
    IUnknown(vpd));
end;

// ExecuteTempAddr
//
procedure TPushOperator.ExecuteTempAddr(exec: TdwsExecution);
var
  vpd: IDataContext;
begin
  exec.DataContext_CreateEmpty(1, vpd);
  FArgExpr.EvalAsVariantToDataContext(exec, vpd, 0);
  exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer + FStackAddr, vpd);
end;

// ExecuteTempData
//
procedure TPushOperator.ExecuteTempData(exec: TdwsExecution);
var
  vpd: IDataContext;
  DataExpr: TDataExpr;
begin
  DataExpr := TDataExpr(FArgExpr);

  exec.DataContext_CreateEmpty(FArgExpr.Typ.Size, vpd);
  vpd.WriteData(DataExpr.DataPtr[exec], FArgExpr.Typ.Size);

  exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer + FStackAddr, vpd);
end;

// ExecuteTempArrayAddr
//
procedure TPushOperator.ExecuteTempArrayAddr(exec: TdwsExecution);
var
  ace: TArrayConstantExpr;
  vpd: IDataContext;
begin
  ace := TArrayConstantExpr(FArgExpr);

  exec.DataContext_CreateEmpty(ace.Size, vpd);

  ace.EvalToDataContext(exec, vpd, 0);

  exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer + FStackAddr, vpd);
end;

// ExecuteTempArray
//
procedure TPushOperator.ExecuteTempArray(exec: TdwsExecution);
begin
  exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer + FStackAddr,
    TConstParamExpr(FArgExpr).DataPtr[exec]);
end;

// ExecuteArrayExpr
//
procedure TPushOperator.ExecuteArrayExpr(exec: TdwsExecution);
var
  expr: TDynamicArrayExpr;
  elementDC: IDataContext;
begin
  expr := TDynamicArrayExpr(FArgExpr); // type already checked at init
  expr.CreateArrayElementDataContext(exec, elementDC);

  exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer + FStackAddr,
    elementDC);
end;

// ExecuteResult
//
procedure TPushOperator.ExecuteResult(exec: TdwsExecution);
var
  buf: Variant;
begin
  // temporary variable is required because FArgExpr could involve stack reallocation
  FArgExpr.EvalAsVariant(exec, buf);
  exec.Stack.Data[exec.Stack.StackPointer + FStackAddr] := buf;
end;

// ExecuteResultBoolean
//
procedure TPushOperator.ExecuteResultBoolean(exec: TdwsExecution);
begin
  exec.Stack.WriteBoolValue(exec.Stack.StackPointer + FStackAddr,
    FArgExpr.EvalAsBoolean(exec));
end;

// ExecuteResultInteger
//
procedure TPushOperator.ExecuteResultInteger(exec: TdwsExecution);
begin
  exec.Stack.WriteIntValue(exec.Stack.StackPointer + FStackAddr,
    FArgExpr.EvalAsInteger(exec));
end;

// ExecuteResultFloat
//
procedure TPushOperator.ExecuteResultFloat(exec: TdwsExecution);
begin
  exec.Stack.WriteFloatValue(exec.Stack.StackPointer + FStackAddr,
    FArgExpr.EvalAsFloat(exec));
end;

// ExecuteResultString
//
procedure TPushOperator.ExecuteResultString(exec: TdwsExecution);
var
  buf: String;
begin
  FArgExpr.EvalAsString(exec, buf);
  exec.Stack.WriteStrValue(exec.Stack.StackPointer + FStackAddr, buf);
end;

// ExecuteResultConstString
//
procedure TPushOperator.ExecuteResultConstString(exec: TdwsExecution);
begin
  exec.Stack.WriteStrValue(exec.Stack.StackPointer + FStackAddr,
    TConstStringExpr(FArgExpr).value);
end;

// ExecuteData
//
procedure TPushOperator.ExecuteData(exec: TdwsExecution);
var
  DataExpr: TDataExpr;
begin
  DataExpr := TDataExpr(FArgExpr);
  exec.Stack.WriteData(exec.Stack.StackPointer + FStackAddr,
    DataExpr.DataPtr[exec], 0, FTypeParamSym.Typ.Size);
end;

// ExecuteConstData
//
procedure TPushOperator.ExecuteConstData(exec: TdwsExecution);
var
  constExpr: TConstExpr;
begin
  constExpr := TConstExpr(FArgExpr);
  exec.Stack.WriteData(exec.Stack.StackPointer + FStackAddr,
    constExpr.DataContext, 0, constExpr.Typ.Size);
end;

// ExecuteInitResult
//
procedure TPushOperator.ExecuteInitResult(exec: TdwsExecution);
var
  buf: IDataContext;
begin
  exec.DataContext_CreateStack(FStackAddr, buf);
  TTypeSymbol(FArgExpr).InitDataContext(buf, 0);
end;

// ExecuteLazy
//
procedure TPushOperator.ExecuteLazy(exec: TdwsExecution);
{$IFNDEF WIN32}
var
  p: PVarData;
{$ENDIF}
begin
{$IFDEF WIN32}
  exec.Stack.WriteIntValue(exec.Stack.StackPointer + FStackAddr,
    Int64(FArgExpr) + (Int64(exec.Stack.BasePointer) shl 32));
{$ELSE}
  p := @exec.Stack.Data[exec.Stack.StackPointer + FStackAddr];
  VarClearSafe(PVariant(p)^);
  p.VType := varRecord;
  p.VRecord.PRecord := FArgExpr;
  p.VRecord.RecInfo := Pointer(exec.Stack.BasePointer);
{$ENDIF}
end;

// ------------------
// ------------------ TFuncExpr ------------------
// ------------------

// Create
//
constructor TFuncExpr.Create(context: TdwsCompilerContext;
  const ScriptPos: TScriptPos; func: TFuncSymbol);
begin
  inherited Create(ScriptPos, func);
  FCallerID := Self;
  FLevel := context.Level;
  FResultAddr := -1;
  FResultVarType := TVarType(-1);
end;

// Destroy
//
destructor TFuncExpr.Destroy;
begin
  FreeMem(FPushExprs);
  inherited;
end;

// ExpectedArg
//
function TFuncExpr.ExpectedArg: TParamSymbol;
begin
  if FArgs.Count < FFunc.params.Count then
    result := (FFunc.params[FArgs.Count] as TParamSymbol)
  else
    result := nil;
end;

// EvalPushExprs
//
procedure TFuncExpr.EvalPushExprs(exec: TdwsExecution);
var
  i: Integer;
  p: PPushOperator;
begin
  p := PPushOperator(FPushExprs);
  for i := 1 to FPushExprsCount do
  begin
    p.Execute(exec);
    Inc(p);
  end;
end;

// DoEvalCall
//
procedure TFuncExpr.DoEvalCall(exec: TdwsExecution; func: TFuncSymbol);
var
  oldBasePointer: Integer;
begin
  EvalPushExprs(exec);

  oldBasePointer := exec.Stack.SwitchFrame(Level);
  TdwsProgramExecution(exec).EnterRecursion(CallerID);
  try
    ICallable(func.Executable).Call(TdwsProgramExecution(exec), func);
  finally
    TdwsProgramExecution(exec).LeaveRecursion;
    exec.Stack.RestoreFrame(Level, oldBasePointer);
  end;
end;

// EvalAsVariant
//
procedure TFuncExpr.EvalAsVariant(exec: TdwsExecution; var result: Variant);
begin
  try
    // Allocate memory for parameters on the stack
    exec.Stack.Push(ParamSize);
    try
      DoEvalCall(exec, funcSym);

      if Typ <> nil then
        StaticPostCall(exec, result);
    finally
      // Remove parameters from stack
      exec.Stack.Pop(ParamSize);
    end;
  except
    exec.SetScriptError(Self);
    raise;
  end;
end;

// StaticPostCall
//
procedure TFuncExpr.StaticPostCall(exec: TdwsExecution; var result: Variant);
var
  sourceAddr, destAddr: Integer;
begin
  // Result.StackAddr is relative to BasePointer of the called function
  // But the frame is already restored so its relative to the stackpointer here
  sourceAddr := exec.Stack.StackPointer + funcSym.result.stackAddr;
  // Copy return value
  if funcSym.Typ.Size = 1 then
    exec.Stack.ReadValue(sourceAddr, result);

  if ResultAddr >= 0 then
  begin
    destAddr := exec.Stack.BasePointer + ResultAddr;
    exec.Stack.CopyData(sourceAddr, destAddr, funcSym.Typ.Size);
  end;
end;

// StaticPostCallInteger
//
procedure TFuncExpr.StaticPostCallInteger(exec: TdwsExecution;
  var result: Int64);

  function Fallback(exec: TdwsExecution; addr: Integer): Int64;
  var
    buf: Variant;
  begin
    exec.Stack.ReadValue(addr, buf);
    result := VariantToInt64(buf);
  end;

var
  sourceAddr, destAddr: Integer;
begin
  sourceAddr := exec.Stack.StackPointer + funcSym.result.stackAddr;
  case FResultVarType of
    varInt64:
      result := exec.Stack.ReadIntValue(sourceAddr);
    TVarType(-1):
      ;
  else
    result := Fallback(exec, sourceAddr);
  end;

  if ResultAddr >= 0 then
  begin
    destAddr := exec.Stack.BasePointer + ResultAddr;
    exec.Stack.CopyData(sourceAddr, destAddr, funcSym.Typ.Size);
  end;
end;

// StaticPostCallFloat
//
procedure TFuncExpr.StaticPostCallFloat(exec: TdwsExecution;
  var result: Double);

  function Fallback(exec: TdwsExecution; addr: Integer): Double;
  var
    buf: Variant;
  begin
    exec.Stack.ReadValue(addr, buf);
    result := VariantToFloat(buf);
  end;

var
  sourceAddr, destAddr: Integer;
begin
  sourceAddr := exec.Stack.StackPointer + funcSym.result.stackAddr;
  case FResultVarType of
    varDouble:
      result := exec.Stack.ReadFloatValue(sourceAddr);
    varInt64:
      result := exec.Stack.ReadIntValue(sourceAddr);
    TVarType(-1):
      ;
  else
    result := Fallback(exec, sourceAddr);
  end;

  if ResultAddr >= 0 then
  begin
    destAddr := exec.Stack.BasePointer + ResultAddr;
    exec.Stack.CopyData(sourceAddr, destAddr, funcSym.Typ.Size);
  end;
end;

// StaticPostCallString
//
procedure TFuncExpr.StaticPostCallString(exec: TdwsExecution;
  var result: String);

  procedure Fallback(exec: TdwsExecution; addr: Integer; var result: String);
  var
    buf: Variant;
  begin
    exec.Stack.ReadValue(addr, buf);
    VariantToString(buf, result);
  end;

var
  sourceAddr, destAddr: Integer;
begin
  sourceAddr := exec.Stack.StackPointer + funcSym.result.stackAddr;
  case FResultVarType of
    varUString:
      exec.Stack.ReadStrValue(sourceAddr, result);
    TVarType(-1):
      ;
  else
    Fallback(exec, sourceAddr, result);
  end;

  if ResultAddr >= 0 then
  begin
    destAddr := exec.Stack.BasePointer + ResultAddr;
    exec.Stack.CopyData(sourceAddr, destAddr, funcSym.Typ.Size);
  end;
end;

// AddPushExprs
//
procedure TFuncExpr.AddPushExprs(context: TdwsCompilerContext);
var
  i: Integer;
  arg: TTypedExpr;
  param: TParamSymbol;
  pushOperator: PPushOperator;
begin
  if Assigned(FFunc.result) then
    FPushExprsCount := FArgs.Count + 1
  else
    FPushExprsCount := FArgs.Count;
  ReallocMem(FPushExprs, FPushExprsCount * SizeOf(TPushOperator));

  for i := 0 to FArgs.Count - 1 do
  begin
    pushOperator := @FPushExprs[i];
    arg := TTypedExpr(FArgs.ExprBase[i]);
    param := TParamSymbol(FFunc.params[i]);
    if param.ClassType = TLazyParamSymbol then
      pushOperator.InitPushLazy(param.stackAddr, arg)
    else if arg is TDataExpr then
    begin
      if param.Typ is TOpenArraySymbol then
      begin
        if arg.ClassType = TArrayConstantExpr then
          pushOperator.InitPushTempArrayAddr(param.stackAddr, arg)
        else
          pushOperator.InitPushTempArray(param.stackAddr, arg);
      end
      else if param is TByRefParamSymbol then
      begin
        if arg is TFuncExprBase then
          pushOperator.InitPushTempAddr(param.stackAddr, arg)
        else if arg is TDynamicArrayExpr then
          pushOperator.InitPushArrayExpr(param.stackAddr, arg)
        else
          pushOperator.InitPushAddr(param.stackAddr, arg);
      end
      else if param.Size > 1 then
        pushOperator.InitPushData(param.stackAddr, TDataExpr(arg), param)
      else
        pushOperator.InitPushResult(context, param.stackAddr, arg)
    end
    else
    begin
      if param.InheritsFrom(TByRefParamSymbol) then
        pushOperator.InitPushTempAddr(param.stackAddr, arg)
      else
        pushOperator.InitPushResult(context, param.stackAddr, arg);
    end;
  end;

  if Assigned(FFunc.result) then
    FPushExprs[FArgs.Count].InitPushInitResult(FFunc.result.stackAddr, Self);
end;

// Initialize
//
procedure TFuncExpr.Initialize(context: TdwsCompilerContext);
var
  rt: TTypeSymbol;
begin
  inherited;
  AddPushExprs(context);
  if (Typ <> nil) and (Typ.Size = 1) then
  begin
    rt := Typ.UnaliasedType;
    if rt = context.TypInteger then
      FResultVarType := varInt64
    else if rt = context.TypFloat then
      FResultVarType := varDouble
    else if rt = context.TypString then
      FResultVarType := varUString
    else
      FResultVarType := varVariant;
  end;
end;

// IsWritable
//
function TFuncExpr.IsWritable: Boolean;
begin
  result := False;
end;

// CompileTimeCheck
//
procedure TFuncExpr.CompileTimeCheck(context: TdwsCompilerContext);
begin
  if funcSym.Executable <> nil then
    ICallable(funcSym.Executable).CompileTimeCheck(context, Self);
end;

// FuncSymQualifiedName
//
function TFuncExpr.FuncSymQualifiedName: String;
begin
  result := funcSym.QualifiedName;
end;

// ------------------
// ------------------ TFuncSimpleExpr ------------------
// ------------------

// EvalNoResult
//
procedure TFuncSimpleExpr.EvalNoResult(exec: TdwsExecution);
begin
  try
    exec.Stack.Push(ParamSize);
    try
      DoEvalCall(exec, funcSym);
    finally
      exec.Stack.Pop(ParamSize);
    end;
  except
    exec.SetScriptError(Self);
    raise;
  end;
end;

// EvalAsInteger
//
function TFuncSimpleExpr.EvalAsInteger(exec: TdwsExecution): Int64;
begin
  try
    exec.Stack.Push(ParamSize);
    try
      DoEvalCall(exec, funcSym);
      StaticPostCallInteger(exec, result);
    finally
      exec.Stack.Pop(ParamSize);
    end;
  except
    exec.SetScriptError(Self);
    raise;
  end;
end;

// EvalAsFloat
//
function TFuncSimpleExpr.EvalAsFloat(exec: TdwsExecution): Double;
begin
  try
    exec.Stack.Push(ParamSize);
    try
      DoEvalCall(exec, funcSym);
      StaticPostCallFloat(exec, result);
    finally
      exec.Stack.Pop(ParamSize);
    end;
  except
    exec.SetScriptError(Self);
    raise;
  end;
end;

// EvalAsString
//
procedure TFuncSimpleExpr.EvalAsString(exec: TdwsExecution; var result: String);
begin
  try
    exec.Stack.Push(ParamSize);
    try
      DoEvalCall(exec, funcSym);
      StaticPostCallString(exec, result);
    finally
      exec.Stack.Pop(ParamSize);
    end;
  except
    exec.SetScriptError(Self);
    raise;
  end;
end;

// ------------------
// ------------------ TFuncPointer ------------------
// ------------------

// Create
//
constructor TFuncPointer.Create(exec: TdwsExecution; funcExpr: TFuncExprBase);
var
  baseExpr: TTypedExpr;
  scriptObj: IScriptObj;
  scriptObjIntf: IScriptObjInterface;
  ClassSym: TClassSymbol;
  magicFuncSym: TMagicFuncSymbol;
  baseTyp: TTypeSymbol;
  CompilerContext: TdwsCompilerContext;
begin
  CompilerContext := (exec as TdwsProgramExecution).CompilerContext;
  if funcExpr is TMethodExpr then
  begin

    baseExpr := TMethodExpr(funcExpr).baseExpr;
    baseTyp := baseExpr.Typ.UnaliasedType;
    if baseTyp is TClassOfSymbol then
    begin
      ClassSym := TClassSymbol(baseExpr.EvalAsInteger(exec));
      FFuncExpr := CreateFuncExpr(CompilerContext, cNullPos, funcExpr.funcSym,
        nil, ClassSym, []);
    end
    else
    begin
      if baseTyp is TInterfaceSymbol then
      begin
        baseExpr.EvalAsScriptObjInterface(exec, scriptObjIntf);
        FFuncExpr := CreateIntfExpr(CompilerContext, funcExpr.funcSym,
          scriptObjIntf);
      end
      else
      begin
        baseExpr.EvalAsScriptObj(exec, scriptObj);
        FFuncExpr := CreateFuncExpr(CompilerContext, cNullPos, funcExpr.funcSym,
          scriptObj, scriptObj.ClassSym, []);
      end;
    end;

  end
  else if funcExpr is TMagicFuncExpr then
  begin

    magicFuncSym := funcExpr.funcSym as TMagicFuncSymbol;
    FFuncExpr := TMagicFuncExpr.CreateMagicFuncExpr(CompilerContext, cNullPos,
      magicFuncSym);

  end
  else
  begin

    FFuncExpr := CreateFuncExpr(CompilerContext, cNullPos, funcExpr.funcSym,
      nil, nil, []);

  end;

  if FFuncExpr is TMagicFuncExpr then
    FEvalAsMagic := True
  else
  begin
    Assert(FFuncExpr is TFuncExpr);
    // handled as Level 1 in the context it will be called from,
    // which may be different from the context in which it was acquired
    TFuncExpr(FFuncExpr).Level := 1;

    FEvalAsMagic := False;
  end;
end;

// Destroy
//
destructor TFuncPointer.Destroy;
begin
  inherited;
  FFuncExpr.Free;
end;

// GetFuncExpr
//
function TFuncPointer.GetFuncExpr: TFuncExprBase;
begin
  result := FFuncExpr;
end;

// SameFunc
//
function TFuncPointer.SameFunc(const v: Variant): Boolean;
var
  ptr: IFuncPointer;
  expr: TFuncExprBase;
  c1, c2: TConstExpr;
begin
  ptr := IFuncPointer(IUnknown(v));
  if ptr = nil then
    Exit(FFuncExpr = nil);
  expr := ptr.GetFuncExpr;
  result := (expr.ClassType = FFuncExpr.ClassType) and
    (expr.funcSym = FFuncExpr.funcSym);
  if result and (FFuncExpr is TMethodExpr) then
  begin
    c1 := TMethodExpr(FFuncExpr).baseExpr as TConstExpr;
    c2 := TMethodExpr(expr).baseExpr as TConstExpr;
    result := c1.SameValueAs(c2);
  end;
end;

// EvalMagicAsVariant
//
procedure TFuncPointer.EvalMagicAsVariant(exec: TdwsExecution;
  caller: TFuncExpr; var result: Variant);
var
  oldArgs: TExprBaseListRec;
begin
  oldArgs := FFuncExpr.args;
  FFuncExpr.args := caller.args;
  try
    FFuncExpr.EvalAsVariant(exec, result);
  finally
    FFuncExpr.args := oldArgs;
  end;
end;

// EvalMagicAsString
//
procedure TFuncPointer.EvalMagicAsString(exec: TdwsExecution; caller: TFuncExpr;
  var result: String);
var
  oldArgs: TExprBaseListRec;
begin
  oldArgs := FFuncExpr.args;
  FFuncExpr.args := caller.args;
  try
    FFuncExpr.EvalAsString(exec, result);
  finally
    FFuncExpr.args := oldArgs;
  end;
end;

// EvalMagicAsInteger
//
function TFuncPointer.EvalMagicAsInteger(exec: TdwsExecution;
  caller: TFuncExpr): Int64;
var
  oldArgs: TExprBaseListRec;
begin
  oldArgs := FFuncExpr.args;
  FFuncExpr.args := caller.args;
  try
    result := FFuncExpr.EvalAsInteger(exec);
  finally
    FFuncExpr.args := oldArgs;
  end;
end;

// EvalNoResult
//
procedure TFuncPointer.EvalNoResult(exec: TdwsExecution; caller: TFuncExpr);
var
  v: Variant;
begin
  EvalAsVariant(exec, caller, v);
end;

// EvalFuncAsVariant
//
procedure TFuncPointer.EvalFuncAsVariant(exec: TdwsExecution; caller: TFuncExpr;
  var result: Variant);
var
  funcExpr: TFuncExpr;
  i: Integer;
begin
  funcExpr := TFuncExpr(FFuncExpr);

  funcExpr.ClearArgs;
  for i := 0 to caller.args.Count - 1 do
    funcExpr.AddArg(caller.args.ExprBase[i] as TTypedExpr);
  funcExpr.AddPushExprs((exec as TdwsProgramExecution).CompilerContext);
  funcExpr.CallerID := caller;

  try
    funcExpr.EvalAsVariant(exec, result);
  finally
    for i := 0 to caller.args.Count - 1 do
      funcExpr.args.ExprBase[i] := nil;
  end;
end;

// EvalFuncAsString
//
procedure TFuncPointer.EvalFuncAsString(exec: TdwsExecution; caller: TFuncExpr;
  var result: String);
var
  v: Variant;
begin
  EvalFuncAsVariant(exec, caller, v);
  VariantToString(v, result);
end;

// EvalFuncAsInteger
//
function TFuncPointer.EvalFuncAsInteger(exec: TdwsExecution;
  caller: TFuncExpr): Integer;
var
  v: Variant;
begin
  EvalFuncAsVariant(exec, caller, v);
  result := VariantToInt64(v);
end;

// EvalAsVariant
//
procedure TFuncPointer.EvalAsVariant(exec: TdwsExecution; caller: TFuncExpr;
  var result: Variant);
begin
  if FEvalAsMagic then
    EvalMagicAsVariant(exec, caller, result)
  else
    EvalFuncAsVariant(exec, caller, result);
end;

// EvalAsInteger
//
function TFuncPointer.EvalAsInteger(exec: TdwsExecution;
  caller: TFuncExpr): Int64;
begin
  if FEvalAsMagic then
    result := EvalMagicAsInteger(exec, caller)
  else
    result := EvalFuncAsInteger(exec, caller);
end;

// EvalAsBoolean
//
function TFuncPointer.EvalAsBoolean(exec: TdwsExecution;
  caller: TFuncExpr): Boolean;

  function Fallback(var v: TVarData): Boolean;
  begin
    try
      result := VariantToBool(Variant(v));
    finally
      VarClearSafe(Variant(v));
    end;
  end;

var
  v: TVarData;
begin
  v.VType := varBoolean;
  if FEvalAsMagic then
    EvalMagicAsVariant(exec, caller, Variant(v))
  else
    EvalFuncAsVariant(exec, caller, Variant(v));
  if v.VType = varBoolean then
    result := v.VBoolean
  else
    result := Fallback(v);
end;

// EvalAsString
//
procedure TFuncPointer.EvalAsString(exec: TdwsExecution; caller: TFuncExpr;
  var result: String);
begin
  if FEvalAsMagic then
    EvalMagicAsString(exec, caller, result)
  else
    EvalFuncAsString(exec, caller, result);
end;

// EvalDataPtr
//
function TFuncPointer.EvalDataPtr(exec: TdwsExecution; caller: TFuncExpr;
  ResultAddr: Integer): IDataContext;
var
  funcExpr: TFuncExpr;
  i: Integer;
begin
  funcExpr := TFuncExpr(FFuncExpr);

  funcExpr.ClearArgs;
  for i := 0 to caller.args.Count - 1 do
    funcExpr.AddArg(caller.args.ExprBase[i] as TTypedExpr);
  funcExpr.AddPushExprs((exec as TdwsProgramExecution).CompilerContext);
  funcExpr.CallerID := caller;
  funcExpr.SetResultAddr(ResultAddr);

  try
    result := funcExpr.DataPtr[exec];
  finally
    for i := 0 to caller.args.Count - 1 do
      funcExpr.args.ExprBase[i] := nil;
  end;
end;

// ------------------
// ------------------ TAnonymousFuncRefExpr ------------------
// ------------------

// Create
//
constructor TAnonymousFuncRefExpr.Create(context: TdwsCompilerContext;
  funcExpr: TFuncExprBase);
begin
  FFuncExpr := funcExpr;
  inherited Create(funcExpr.ScriptPos, funcExpr.funcSym);
end;

// Destroy
//
destructor TAnonymousFuncRefExpr.Destroy;
begin
  inherited;
  FFuncExpr.Free;
end;

// Extract
//
function TAnonymousFuncRefExpr.Extract: TFuncExprBase;
begin
  result := FFuncExpr;
  FFuncExpr := nil;
  Free;
end;

// EvalAsVariant
//
procedure TAnonymousFuncRefExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  funcPtr: TFuncPointer;
begin
  if FFuncExpr.ClassType = TFuncPtrExpr then
    TFuncPtrExpr(FFuncExpr).codeExpr.EvalAsVariant(exec, result)
  else
  begin
    funcPtr := TFuncPointer.Create(exec, FFuncExpr);
    VarCopySafe(result, IFuncPointer(funcPtr));
  end;
end;

// GetSubExpr
//
function TAnonymousFuncRefExpr.GetSubExpr(i: Integer): TExprBase;
begin
  result := FFuncExpr
end;

// GetSubExprCount
//
function TAnonymousFuncRefExpr.GetSubExprCount: Integer;
begin
  result := 1;
end;

// GetDataPtr
//
procedure TAnonymousFuncRefExpr.GetDataPtr(exec: TdwsExecution;
  var result: IDataContext);
begin
  exec.DataContext_CreateEmpty(1, result);
  EvalAsVariantToDataContext(exec, result, 0);
end;

// ------------------
// ------------------ TFuncPtrExpr ------------------
// ------------------

// Create
//
constructor TFuncPtrExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; codeExpr: TTypedExpr);
var
  funcSym: TFuncSymbol;
  codeExprType: TTypeSymbol;
begin
  codeExprType := codeExpr.Typ.UnaliasedType;
  if codeExprType is TFuncSymbol then
    funcSym := TFuncSymbol(codeExprType)
  else
  begin
    funcSym := codeExprType.AsFuncSymbol;
    if funcSym = nil then
    begin
      Assert(codeExprType.CanExpectAnyFuncSymbol);
      funcSym := context.TypAnyFunc
    end;
  end;
  inherited Create(context, aScriptPos, funcSym);
  FCodeExpr := codeExpr;
end;

// Destroy
//
destructor TFuncPtrExpr.Destroy;
begin
  inherited;
  FCodeExpr.Free;
end;

// Extract
//
function TFuncPtrExpr.Extract: TTypedExpr;
begin
  result := FCodeExpr;
  FCodeExpr := nil;
  Free;
end;

// EvalAsFuncPointer
//
procedure TFuncPtrExpr.EvalAsFuncPointer(exec: TdwsExecution;
  var result: IFuncPointer);
var
  val: Variant;
begin
  FCodeExpr.EvalAsVariant(exec, val);
  result := IFuncPointer(IUnknown(val));
end;

// EvalAsVariant
//
procedure TFuncPtrExpr.EvalAsVariant(exec: TdwsExecution; var result: Variant);
var
  funcPointer: IFuncPointer;
begin
  EvalAsFuncPointer(exec, funcPointer);
  if funcPointer = nil then
    RaiseScriptError(exec, EScriptError, RTE_FuncPointerIsNil);
  funcPointer.EvalAsVariant(exec, Self, result);
end;

// EvalNoResult
//
procedure TFuncPtrExpr.EvalNoResult(exec: TdwsExecution);
var
  buf: Variant;
begin
  EvalAsVariant(exec, buf);
end;

// GetIsConstant
//
function TFuncPtrExpr.GetIsConstant: Boolean;
begin
  result := False;
end;

// GetSubExpr
//
function TFuncPtrExpr.GetSubExpr(i: Integer): TExprBase;
begin
  if i = 0 then
    result := FCodeExpr
  else
    result := inherited GetSubExpr(i - 1);
end;

// GetSubExprCount
//
function TFuncPtrExpr.GetSubExprCount: Integer;
begin
  result := 1 + inherited GetSubExprCount;
end;

// ------------------
// ------------------ TTypedExprList ------------------
// ------------------

// Destroy
//
destructor TTypedExprList.Destroy;
begin
  FList.Clean;
  FDefaultExpected.Free;
  inherited;
end;

// Orphan
//
procedure TTypedExprList.Orphan(context: TdwsCompilerContext);
begin
  OrphanItems(context);
  if DefaultExpected <> nil then
  begin
    context.OrphanObject(DefaultExpected);
    FDefaultExpected := nil;
  end;
  Free;
end;

// OrphanItems
//
procedure TTypedExprList.OrphanItems(context: TdwsCompilerContext);
var
  i: Integer;
  e: TProgramExpr;
begin
  for i := 0 to Count - 1 do
  begin
    e := expr[i];
    if e <> nil then
      e.Orphan(context);
  end;
  FList.Clear;
end;

procedure TTypedExprList.AddExpr(expr: TTypedExpr);
begin
  FList.Add(expr);
end;

// ExpectedArg
//
function TTypedExprList.ExpectedArg: TParamSymbol;
begin
  if (FTable <> nil) and (FList.Count < FTable.Count) then
    result := FTable[FList.Count] as TParamSymbol
  else
    result := FDefaultExpected;
end;

// Insert0
//
procedure TTypedExprList.Insert0(expr: TExprBase);
begin
  FList.Insert(0, expr);
end;

// Delete
//
procedure TTypedExprList.Delete(index: Integer);
begin
  FList.Delete(index);
end;

// Clear
//
procedure TTypedExprList.Clear;
begin
  FList.Clear;
end;

function TTypedExprList.GetExpr(const x: Integer): TTypedExpr;
begin
  result := TTypedExpr(FList.List[x]);
end;

procedure TTypedExprList.SetExpr(const x: Integer; const value: TTypedExpr);
begin
  FList.List[x] := value;
end;

// ------------------
// ------------------ TBinaryOpExpr ------------------
// ------------------

constructor TBinaryOpExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; const anOp: TTokenType;
  aLeft, aRight: TTypedExpr);
begin
  inherited Create;
  FScriptPos := aScriptPos;
  FOp := anOp;
  FLeft := aLeft;
  FRight := aRight;
end;

destructor TBinaryOpExpr.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

// EvalAsVariant
//
procedure TBinaryOpExpr.EvalAsVariant(exec: TdwsExecution; var result: Variant);
begin
  Assert(False);
end;

// ScriptPos
//
function TBinaryOpExpr.ScriptPos: TScriptPos;
begin
  result := FScriptPos;
end;

// GetIsConstant
//
function TBinaryOpExpr.GetIsConstant: Boolean;
begin
  result := FLeft.IsConstant and FRight.IsConstant;
end;

// OrphanSubExprs
//
procedure TBinaryOpExpr.OrphanSubExprs(context: TdwsCompilerContext);
begin
  if FLeft <> nil then
  begin
    FLeft.Orphan(context);
    FLeft := nil;
  end;
  if FRight <> nil then
  begin
    FRight.Orphan(context);
    FRight := nil;
  end;
  DecRefCount;
end;

// OptimizeConstantOperandsToFloats
//
procedure TBinaryOpExpr.OptimizeConstantOperandsToFloats
  (context: TdwsCompilerContext);
begin
  FLeft := FLeft.OptimizeToFloatConstant(context);
  FRight := FRight.OptimizeToFloatConstant(context);
end;

// Swap
//
procedure TBinaryOpExpr.Swap;
var
  t: TTypedExpr;
begin
  t := FLeft;
  FLeft := FRight;
  FRight := t;
end;

// GetSubExpr
//
function TBinaryOpExpr.GetSubExpr(i: Integer): TExprBase;
begin
  if i = 0 then
    result := FLeft
  else
    result := FRight;
end;

// GetSubExprCount
//
function TBinaryOpExpr.GetSubExprCount: Integer;
begin
  result := 2;
end;

// ------------------
// ------------------ TStaticallyTypedBinOpExpr ------------------
// ------------------

// SpecializeTypedExpr
//
function TStaticallyTypedBinOpExpr.SpecializeTypedExpr(const context
  : ISpecializationContext): TTypedExpr;
begin
  result := TBinaryOpExprClass(ClassType)
    .Create(CompilerContextFromSpecialization(context), ScriptPos, Op,
    Left.SpecializeTypedExpr(context), Right.SpecializeTypedExpr(context));
end;

// ------------------
// ------------------ TVariantBinOpExpr ------------------
// ------------------

// Create
//
constructor TVariantBinOpExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; const anOp: TTokenType;
  aLeft, aRight: TTypedExpr);
begin
  inherited;
  FTyp := context.TypVariant;
end;

// Optimize
//
function TVariantBinOpExpr.Optimize(context: TdwsCompilerContext): TProgramExpr;
var
  v: Variant;
begin
  if IsConstant then
  begin
    EvalAsVariant(context.Execution, v);
    result := TConstExpr.CreateValue(ScriptPos, context.TypVariant, v);
    Orphan(context);
  end
  else
    result := Self;
end;

// ------------------
// ------------------ TIntegerBinOpExpr ------------------
// ------------------

// Create
//
constructor TIntegerBinOpExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; const anOp: TTokenType;
  aLeft, aRight: TTypedExpr);
begin
  inherited;
  if aLeft.Typ = aRight.Typ then
    FTyp := aLeft.Typ
  else
    FTyp := context.TypInteger;
end;

// Orphan
//
procedure TIntegerBinOpExpr.Orphan(context: TdwsCompilerContext);
begin
  if Typ = context.TypInteger then
    OrphanSubExprs(context)
  else
    inherited;
end;

// EvalAsVariant
//
procedure TIntegerBinOpExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  result := EvalAsInteger(exec);
end;

// EvalAsFloat
//
function TIntegerBinOpExpr.EvalAsFloat(exec: TdwsExecution): Double;
begin
  result := EvalAsInteger(exec);
end;

// Optimize
//
function TIntegerBinOpExpr.Optimize(context: TdwsCompilerContext): TProgramExpr;
begin
  if IsConstant then
  begin
    result := TConstIntExpr.Create(ScriptPos, Typ,
      EvalAsInteger(context.Execution));
    Orphan(context);
  end
  else
    result := Self;
end;

// ------------------
// ------------------ TStringBinOpExpr ------------------
// ------------------

// Create
//
constructor TStringBinOpExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; const anOp: TTokenType;
  aLeft, aRight: TTypedExpr);
begin
  inherited;
  FTyp := context.TypString;
end;

// Orphan
//
procedure TStringBinOpExpr.Orphan(context: TdwsCompilerContext);
begin
  if Typ = context.TypString then
    OrphanSubExprs(context)
  else
    inherited;
end;

// EvalAsVariant
//
procedure TStringBinOpExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  buf: String;
begin
  EvalAsString(exec, buf);
  VarCopySafe(result, buf);
end;

// Optimize
//
function TStringBinOpExpr.Optimize(context: TdwsCompilerContext): TProgramExpr;
var
  buf: String;
begin
  if IsConstant then
  begin
    EvalAsString(context.Execution, buf);
    result := TConstStringExpr.Create(ScriptPos, context.TypString, buf);
    Orphan(context);
  end
  else
    result := Self;
end;

// ------------------
// ------------------ TFloatBinOpExpr ------------------
// ------------------

// Create
//
constructor TFloatBinOpExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; const anOp: TTokenType;
  aLeft, aRight: TTypedExpr);
begin
  inherited;
  FTyp := context.TypFloat;
end;

// Orphan
//
procedure TFloatBinOpExpr.Orphan(context: TdwsCompilerContext);
begin
  if Typ = context.TypFloat then
    OrphanSubExprs(context)
  else
    inherited;
end;

// EvalAsVariant
//
procedure TFloatBinOpExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  result := EvalAsFloat(exec);
end;

// Optimize
//
function TFloatBinOpExpr.Optimize(context: TdwsCompilerContext): TProgramExpr;
begin
  if IsConstant then
  begin
    result := TConstFloatExpr.Create(ScriptPos, Typ,
      EvalAsFloat(context.Execution));
    Orphan(context);
  end
  else
  begin
    OptimizeConstantOperandsToFloats(context);
    result := Self;
  end;
end;

// ------------------
// ------------------ TBooleanBinOpExpr ------------------
// ------------------

// Create
//
constructor TBooleanBinOpExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; const anOp: TTokenType;
  aLeft, aRight: TTypedExpr);
begin
  inherited;
  FTyp := context.TypBoolean;
end;

// EvalAsVariant
//
procedure TBooleanBinOpExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  result := EvalAsBoolean(exec);
end;

// Optimize
//
function TBooleanBinOpExpr.Optimize(context: TdwsCompilerContext): TProgramExpr;
begin
  if IsConstant then
  begin
    result := TConstBooleanExpr.Create(ScriptPos, context.TypBoolean,
      EvalAsBoolean(context.Execution));
    Orphan(context);
  end
  else
    result := Self;
end;

// ------------------
// ------------------ TUnaryOpDataExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpDataExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr);
begin
  inherited Create(aScriptPos, expr.Typ);
  FExpr := expr;
end;

// Destroy
//
destructor TUnaryOpDataExpr.Destroy;
begin
  FExpr.Free;
  inherited;
end;

// GetIsConstant
//
function TUnaryOpDataExpr.GetIsConstant: Boolean;
begin
  result := FExpr.IsConstant;
end;

// GetSubExpr
//
function TUnaryOpDataExpr.GetSubExpr(i: Integer): TExprBase;
begin
  result := FExpr;
end;

// GetSubExprCount
//
function TUnaryOpDataExpr.GetSubExprCount: Integer;
begin
  result := 1;
end;

// ------------------
// ------------------ TUnaryOpExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpExpr.Create(context: TdwsBaseSymbolsContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr);
begin
  inherited Create;
  FScriptPos := aScriptPos;
  FExpr := expr;
end;

// Destroy
//
destructor TUnaryOpExpr.Destroy;
begin
  FExpr.Free;
  inherited;
end;

// ScriptPos
//
function TUnaryOpExpr.ScriptPos: TScriptPos;
begin
  result := FScriptPos;
end;

// GetIsConstant
//
function TUnaryOpExpr.GetIsConstant: Boolean;
begin
  result := FExpr.IsConstant;
end;

// GetSubExpr
//
function TUnaryOpExpr.GetSubExpr(i: Integer): TExprBase;
begin
  result := FExpr;
end;

// GetSubExprCount
//
function TUnaryOpExpr.GetSubExprCount: Integer;
begin
  result := 1;
end;

// ------------------
// ------------------ TUnaryOpBoolExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpBoolExpr.Create(context: TdwsBaseSymbolsContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr);
begin
  inherited;
  Typ := context.TypBoolean;
end;

// EvalAsVariant
//
procedure TUnaryOpBoolExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  VarCopySafe(result, EvalAsBoolean(exec));
end;

// ------------------
// ------------------ TUnaryOpIntExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpIntExpr.Create(context: TdwsBaseSymbolsContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr);
begin
  inherited;
  Typ := context.TypInteger;
end;

// Orphan
//
procedure TUnaryOpIntExpr.Orphan(context: TdwsCompilerContext);
begin
  if Typ = context.TypInteger then
  begin
    if expr <> nil then
    begin
      expr.Orphan(context);
      FExpr := nil;
    end;
    DecRefCount;
  end
  else
    inherited;
end;

// EvalAsVariant
//
procedure TUnaryOpIntExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  VarCopySafe(result, EvalAsInteger(exec));
end;

// Optimize
//
function TUnaryOpIntExpr.Optimize(context: TdwsCompilerContext): TProgramExpr;
begin
  if IsConstant then
  begin
    result := TConstIntExpr.Create(ScriptPos, Typ,
      EvalAsInteger(context.Execution));
    Orphan(context);
  end
  else
    result := Self;
end;

// ------------------
// ------------------ TUnaryOpFloatExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpFloatExpr.Create(context: TdwsBaseSymbolsContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr);
begin
  inherited;
  Typ := context.TypFloat;
end;

// Orphan
//
procedure TUnaryOpFloatExpr.Orphan(context: TdwsCompilerContext);
begin
  if Typ = context.TypFloat then
  begin
    if expr <> nil then
    begin
      expr.Orphan(context);
      FExpr := nil;
    end;
    DecRefCount;
  end
  else
    inherited;
end;

// EvalAsVariant
//
procedure TUnaryOpFloatExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
begin
  VarCopySafe(result, EvalAsFloat(exec));
end;

// Optimize
//
function TUnaryOpFloatExpr.Optimize(context: TdwsCompilerContext): TProgramExpr;
begin
  if IsConstant then
  begin
    result := TConstFloatExpr.Create(ScriptPos, Typ,
      EvalAsFloat(context.Execution));
    Orphan(context);
  end
  else
    result := Self;
end;

// ------------------
// ------------------ TUnaryOpStringExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpStringExpr.Create(context: TdwsBaseSymbolsContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr);
begin
  inherited;
  Typ := context.TypString;
end;

// EvalAsVariant
//
procedure TUnaryOpStringExpr.EvalAsVariant(exec: TdwsExecution;
  var result: Variant);
var
  buf: String;
begin
  EvalAsString(exec, buf);
  VarCopySafe(result, buf);
end;

// ------------------
// ------------------ TUnaryOpVariantExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpVariantExpr.Create(context: TdwsBaseSymbolsContext;
  const aScriptPos: TScriptPos; expr: TTypedExpr);
begin
  inherited;
  Typ := context.TypVariant;
end;

// ------------------
// ------------------ TProgramInfo ------------------
// ------------------

// CompilerContext
//
function TProgramInfo.CompilerContext: TdwsCompilerContext;
begin
  result := FExecution.CompilerContext;
end;

function TProgramInfo.GetValueAsVariant(const s: String): Variant;
begin
  result := GetVars(s).value;
end;

function TProgramInfo.GetData(const s: String): TData;
begin
  result := GetVars(s).Data;
end;

procedure TProgramInfo.SetValueAsVariant(const s: String; const value: Variant);
begin
  GetVars(s).value := value;
end;

procedure TProgramInfo.SetData(const s: String; const value: TData);
begin
  GetVars(s).Data := value;
end;

// GetSymbolInfo
//
procedure TProgramInfo.GetSymbolInfo(sym: TSymbol; var Info: IInfo);

  procedure GetExternalVarSymbolInfo(sym: TSymbol; var result: IInfo);
  var
    dat: TData;
    extVDM: TExternalVarDataMaster;
    locData: IDataContext;
  begin
    SetLength(dat, sym.Typ.Size);
    extVDM := TExternalVarDataMaster.Create(Execution, TExternalVarSymbol(sym));
    if sym.Typ.IsClassSymbol then
      extVDM.Read(Execution, dat); // initialize 'Self'-Object
    Execution.DataContext_Create(dat, 0, locData);
    TInfo.SetChild(result, Self, sym.Typ, locData, extVDM);
  end;

  procedure GetTypeSymbolInfo(sym: TSymbol; var result: IInfo);
  var
    locData: IDataContext;
  begin
    if sym.BaseType.IsClassSymbol then
    begin
      Execution.DataContext_CreateEmpty(1, locData);
      result := TInfoClassObj.Create(Self, sym, locData);
    end
    else
      result := nil;
  end;

  procedure GetVarParamVars(sym: TDataSymbol; BasePointer: Integer;
    var result: IInfo);
  var
    dp: IDataContext;
  begin
    dp := IDataContext(IUnknown(Execution.Stack.Data[BasePointer +
      sym.stackAddr]));
    TInfo.SetChild(result, Self, sym.Typ, dp);
  end;

  procedure GetDataSymbol(sym: TDataSymbol; var result: IInfo);
  var
    BasePointer: Integer;
    pin: TProgramInfo;
    exec: TdwsExecution;
    locData: IDataContext;
  begin
    pin := Self;
    exec := pin.Execution;
    if sym.Level = pin.FLevel then
      BasePointer := exec.Stack.BasePointer
    else
      BasePointer := exec.Stack.GetSavedBp(pin.Level);
    if (sym is TByRefParamSymbol) or (sym.Typ is TOpenArraySymbol) or
      ((sym is TSelfSymbol) and (sym.Typ is TRecordSymbol)) then
    begin
      GetVarParamVars(sym, BasePointer, result);
    end
    else
    begin
      exec.DataContext_CreateBase(sym.stackAddr, locData);
      TInfo.SetChild(result, pin, sym.Typ, locData);
    end;
  end;

  procedure GetFieldSymbol(sym: TFieldSymbol; var result: IInfo);
  var
    locData: IDataContext;
  begin
    // Field of the Self object
    if sym.StructSymbol is TRecordSymbol then
    begin
      Execution.DataContext_Create(Self.GetData(SYS_SELF), sym.offset, locData);
    end
    else
    begin
      Execution.DataContext_CreateOffset(FScriptObj, sym.offset, locData);
    end;
    TInfo.SetChild(result, Self, sym.Typ, locData);
  end;

  procedure GetConstSymbol(sym: TConstSymbol; var result: IInfo);
  begin
    TInfo.SetChild(result, Self, sym.Typ, sym.DataContext);
  end;

begin
  if sym is TDataSymbol then
    GetDataSymbol(TDataSymbol(sym), Info)
  else if sym is TConstSymbol then
    GetConstSymbol(TConstSymbol(sym), Info)
  else if sym is TFieldSymbol then
    GetFieldSymbol(TFieldSymbol(sym), Info)
  else if sym is TExternalVarSymbol then
    GetExternalVarSymbolInfo(sym, Info)
  else if sym is TTypeSymbol then
    GetTypeSymbolInfo(sym, Info)
  else
    RaiseOnlyVarSymbols(sym);
end;

// GetVars
//
function TProgramInfo.GetVars(const str: String): IInfo;

var
  sym: TSymbol;
begin
  sym := FTable.FindSymbol(str, cvMagic);

  if not Assigned(sym) then
    RaiseVariableNotFound(str)
  else
    GetSymbolInfo(sym, result);
end;

// RootInfo
//
function TProgramInfo.RootInfo(const aName: String): IInfo;
var
  sym: TSymbol;
  funcSym: TFuncSymbol;
begin
  sym := FExecution.FProg.FTable.FindSymbol(aName, cvMagic);

  if not Assigned(sym) then
    RaiseVariableNotFound(aName);

  funcSym := sym.AsFuncSymbol;
  if funcSym <> nil then
    result := GetFuncBySym(funcSym)
  else
    GetSymbolInfo(sym, result);
end;

// GetParams
//
function TProgramInfo.GetParams(const index: Integer): IInfo;
var
  ip: TSymbolTable;
  sym: TSymbol;
begin
  if (funcSym = nil) or (Cardinal(index) >= Cardinal(funcSym.params.Count)) then
  begin
    RaiseIncorrectParameterIndex(index);
    result := nil;
  end
  else
  begin
    ip := funcSym.params;
    sym := ip[index];
    if not Assigned(sym) then
      RaiseVariableNotFound(ip[index].name)
    else
      GetSymbolInfo(sym, result);
  end;
end;

// ParamCount
//
function TProgramInfo.ParamCount: Integer;
begin
  result := funcSym.params.Count;
end;

// GetFunc
//
function TProgramInfo.GetFunc(const s: String): IInfo;
var
  sym: TSymbol;
  funcSym: TFuncSymbol;
begin
  sym := FTable.FindSymbol(s, cvMagic);

  if not Assigned(sym) then
    raise Exception.CreateFmt(RTE_FunctionNotFound, [s]);

  funcSym := sym.AsFuncSymbol;
  if funcSym <> nil then
    result := GetFuncBySym(funcSym)
  else
    raise Exception.CreateFmt(RTE_OnlyFuncSymbols, [sym.Caption]);
end;

// GetFuncBySym
//
function TProgramInfo.GetFuncBySym(funcSym: TFuncSymbol): IInfo;
begin
  if Assigned(FScriptObj) then
  begin
    result := TInfoFunc.Create(Self, funcSym, Execution.DataContext_Nil, nil,
      FScriptObj, FScriptObj.ClassSym)
  end
  else
  begin
    result := TInfoFunc.Create(Self, funcSym, Execution.DataContext_Nil,
      nil, nil, nil)
  end;
end;

function TProgramInfo.GetTemp(const DataType: String): IInfo;
var
  Data: IDataContext;
  typSym: TTypeSymbol;
begin
  typSym := FTable.FindTypeSymbol(DataType, cvMagic);

  if not Assigned(typSym) then
    raise Exception.CreateFmt(RTE_DatatypeNotFound, [DataType]);

  Data := TDataContext.CreateStandalone(typSym.Size);
  typSym.InitDataContext(Data, 0);

  TInfo.SetChild(result, Self, typSym, Data);
end;

// RaiseExceptObj
//
procedure TProgramInfo.RaiseExceptObj(const msg: String; const obj: IScriptObj);
begin
  raise EScriptException.Create(msg, obj, cNullPos);
end;

// ResultSetNull
//
procedure TProgramInfo.ResultSetNull;
begin
  VarSetNull(GetResultAsPVariant^);
end;

procedure TProgramInfo.SetFuncSym(const value: TFuncSymbol);
begin
  FFuncSym := value;
  if Assigned(FFuncSym) then
    FLevel := FFuncSym.Level // 1
  else
    FLevel := 0;
end;

function TProgramInfo.GetResultAsVariant: Variant;
begin
  result := GetResultVars.value;
end;

// GetResultVars
//
function TProgramInfo.GetResultVars: IInfo;
begin
  result := GetVars(SYS_RESULT);
end;

function TProgramInfo.GetValueAsString(const s: String): String;
begin
  result := GetVars(s).ValueAsString;
end;

procedure TProgramInfo.SetValueAsString(const s: String; const value: String);
begin
  GetVars(s).value := value;
end;

// GetValueAsChar
//
function TProgramInfo.GetValueAsChar(const s: String): WideChar;
var
  buf: String;
begin
  buf := GetVars(s).ValueAsString;
  if buf <> '' then
    result := buf[1]
  else
    result := #0;
end;

// GetValueAsDataString
//
function TProgramInfo.GetValueAsDataString(const s: String): RawByteString;
begin
  result := ScriptStringToRawByteString(GetValueAsString(s));
end;

// SetValueAsDataString
//
procedure TProgramInfo.SetValueAsDataString(const s: String;
  const value: RawByteString);
begin
  SetValueAsString(s, RawByteStringToScriptString(value));
end;

function TProgramInfo.GetValueAsInteger(const s: String): Int64;
begin
  result := GetVars(s).ValueAsInteger;
end;

procedure TProgramInfo.SetValueAsInteger(const s: String; const value: Int64);
begin
  GetVars(s).value := value;
end;

function TProgramInfo.GetValueAsBoolean(const s: String): Boolean;
begin
  result := GetVars(s).value;
end;

procedure TProgramInfo.SetValueAsBoolean(const s: String; const value: Boolean);
begin
  GetVars(s).value := value;
end;

function TProgramInfo.GetValueAsFloat(const s: String): Double;
begin
  result := GetVars(s).ValueAsFloat;
end;

procedure TProgramInfo.SetValueAsFloat(const s: String; const value: Double);
begin
  GetVars(s).value := value;
end;

function TProgramInfo.GetValueAsObject(const s: String): TObject;
var
  Info: IInfo;
  scriptObj: IScriptObj;
begin
  Info := GetVars(s);
  if Assigned(Info) then
  begin
    scriptObj := Info.scriptObj;
    if Assigned(scriptObj) then
      result := scriptObj.ExternalObject
    else
      result := nil;
  end
  else
    result := nil;
end;

// GetValueAsClassSymbol
//
function TProgramInfo.GetValueAsClassSymbol(const s: String): TClassSymbol;
begin
  result := TClassSymbol(GetVars(s).ValueAsInteger);
end;

function TProgramInfo.GetValueAsTStrings(const s: String): TStrings;
var
  obj: TObject;
begin
  obj := GetValueAsObject(s);
  if Assigned(obj) then
    result := obj as TStrings
  else
    result := nil;
end;

// GetResultAsPVariant
//
function TProgramInfo.GetResultAsPVariant: PVariant;
var
  sym: TDataSymbol;
  stackAddr: Integer;
  exec: TdwsExecution;
begin
  sym := funcSym.result;
  if sym = nil then
    RaiseVariableNotFound(SYS_RESULT);
  exec := Execution;
  if sym.Level = FLevel then
    stackAddr := sym.stackAddr + exec.Stack.BasePointer
  else
    stackAddr := sym.stackAddr + exec.Stack.GetSavedBp(Level);
  result := @exec.Stack.Data[stackAddr];
end;

procedure TProgramInfo.SetResultAsVariant(const value: Variant);
begin
  VarCopySafe(GetResultAsPVariant^, value);
end;

// SetResultAsString
//
procedure TProgramInfo.SetResultAsString(const value: String);
begin
  VarCopySafe(GetResultAsPVariant^, value);
end;

// SetResultAsDataString
//
procedure TProgramInfo.SetResultAsDataString(const value: RawByteString);
begin
  SetResultAsString(RawByteStringToScriptString(value));
end;

// SetResultAsInteger
//
procedure TProgramInfo.SetResultAsInteger(const value: Int64);
begin
  VarCopySafe(GetResultAsPVariant^, value);
end;

// SetResultAsBoolean
//
procedure TProgramInfo.SetResultAsBoolean(const value: Boolean);
begin
  VarCopySafe(GetResultAsPVariant^, value);
end;

// SetResultAsFloat
//
procedure TProgramInfo.SetResultAsFloat(const value: Double);
begin
  VarCopySafe(GetResultAsPVariant^, value);
end;

// SetResultAsStringArray
//
procedure TProgramInfo.SetResultAsStringArray(const a: TStringDynArray);
var
  i, n: Integer;
  result: IScriptDynArray;
begin
  result := ResultVars.ScriptDynArray;
  n := Length(a);
  result.ArrayLength := n;
  for i := 0 to n - 1 do
    result.SetAsString(i, a[i]);
end;

// SetResultAsStringArray
//
procedure TProgramInfo.SetResultAsStringArray(const s: TStrings);
var
  i, n: Integer;
  result: IScriptDynArray;
begin
  result := ResultVars.ScriptDynArray;
  n := s.Count;
  result.ArrayLength := n;
  for i := 0 to n - 1 do
    result.SetAsString(i, s[i]);
end;

// GetParamDataContext
//
function TProgramInfo.GetParamDataContext(index: Integer): IDataContext;
var
  ip: TSymbolTable;
  sym: TSymbol;
  DataSym: TDataSymbol;
  stackAddr: Integer;
  exec: TdwsExecution;
begin
  ip := funcSym.params;
  if Cardinal(index) >= Cardinal(ip.Count) then
  begin
    RaiseIncorrectParameterIndex(index);
    result := nil;
  end
  else
  begin
    sym := ip[index];
    Assert(sym.IsDataSymbol);
    DataSym := TDataSymbol(sym);
    exec := Execution;
    if DataSym.Level = FLevel then
      stackAddr := DataSym.stackAddr + exec.Stack.BasePointer
    else
      stackAddr := DataSym.stackAddr + exec.Stack.GetSavedBp(Level);
    if DataSym.InheritsFrom(TByRefParamSymbol) then
    begin
      result := IDataContext(IUnknown(Execution.Stack.Data[stackAddr]))
    end
    else
      result := exec.Stack.CreateDataContext(exec.Stack.Data, stackAddr);
  end;
end;

// GetParamAsVariant
//
function TProgramInfo.GetParamAsVariant(index: Integer): Variant;
begin
  GetParamDataContext(index).EvalAsVariant(0, result);
end;

// SetParamAsVariant
//
procedure TProgramInfo.SetParamAsVariant(index: Integer; const v: Variant);
begin
  GetParamDataContext(index).AsVariant[0] := v;
end;

// GetParamAsInteger
//
function TProgramInfo.GetParamAsInteger(index: Integer): Int64;
begin
  result := GetParamDataContext(index).AsInteger[0];
end;

// SetParamAsInteger
//
procedure TProgramInfo.SetParamAsInteger(index: Integer; const v: Int64);
begin
  GetParamDataContext(index).AsInteger[0] := v;
end;

// GetParamAsString
//
function TProgramInfo.GetParamAsString(index: Integer): String;
begin
  GetParamDataContext(index).EvalAsString(0, result);
end;

// SetParamAsString
//
procedure TProgramInfo.SetParamAsString(index: Integer; const v: String);
begin
  GetParamDataContext(index).AsString[0] := v;
end;

// GetParamAsDataString
//
function TProgramInfo.GetParamAsDataString(index: Integer): RawByteString;
begin
  result := ScriptStringToRawByteString(GetParamAsString(index));
end;

// SetParamAsDataString
//
procedure TProgramInfo.SetParamAsDataString(index: Integer;
  const v: RawByteString);
begin
  GetParamDataContext(index).AsString[0] := RawByteStringToScriptString(v);
end;

// GetParamAsFileName
//
function TProgramInfo.GetParamAsFileName(index: Integer): String;
begin
  result := Execution.ValidateFileName(GetParamAsString(index));
end;

// GetParamAsFloat
//
function TProgramInfo.GetParamAsFloat(index: Integer): Double;
begin
  result := GetParamDataContext(index).AsFloat[0];
end;

// GetParamAsBoolean
//
function TProgramInfo.GetParamAsBoolean(index: Integer): Boolean;
begin
  result := GetParamDataContext(index).AsBoolean[0];
end;

// GetParamAsObject
//
function TProgramInfo.GetParamAsObject(index: Integer): TObject;
var
  intf: IUnknown;
begin
  GetParamDataContext(index).EvalAsInterface(0, intf);
  if intf <> nil then
    result := (intf as IScriptObj).ExternalObject
  else
    result := nil;
end;

// GetParamAsScriptObj
//
function TProgramInfo.GetParamAsScriptObj(index: Integer): IScriptObj;
var
  intf: IUnknown;
begin
  GetParamDataContext(index).EvalAsInterface(0, intf);
  if intf <> nil then
    result := intf as IScriptObj
  else
    result := nil;
end;

// GetParamAsScriptDynArray
//
function TProgramInfo.GetParamAsScriptDynArray(index: Integer): IScriptDynArray;
var
  intf: IUnknown;
begin
  GetParamDataContext(index).EvalAsInterface(0, intf);
  if intf <> nil then
    result := intf as IScriptDynArray
  else
    result := nil;
end;

// GetParamAsDataContext
//
function TProgramInfo.GetParamAsDataContext(index: Integer): IDataContext;
var
  intf: IUnknown;
begin
  GetParamDataContext(index).EvalAsInterface(0, intf);
  if intf <> nil then
    result := intf as IDataContext
  else
    result := nil;
end;

function TProgramInfo.FindClassMatch(AObject: TObject; ExactMatch: Boolean)
  : TClassSymbol;
var
{$IFDEF FPC}
  ParentRTTI: PTypeInfo;
{$ELSE}
  ParentRTTI: PPTypeInfo;
{$ENDIF}
  UnitList: TUnitSymbolRefList;
  // build the list once, may search for many symbols
  typeSym: TSymbol;
begin
  result := nil;
  if AObject = nil then
    Exit;

  { Cycle the AObject class hierarchy and determine the DWS class type that most
    closely matches the AObject type. Return the IInfo for that matching
    class. It should *always* at least match at TObject. }
  UnitList := CreateUnitList;
  try
    // Check current class type. If not found cycle object ancestry
    typeSym := FindSymbolInUnits(UnitList, AObject.ClassName);
    // If no exact match found then look for supported ancestors
    if typeSym.IsClassSymbol then
      result := TClassSymbol(typeSym);

    // Allowed to look through ancestor types
    if not(ExactMatch or Assigned(typeSym)) then
    begin
      if AObject.ClassInfo <> nil then
      begin
        ParentRTTI := GetTypeData(AObject.ClassInfo).ParentInfo;
        repeat
          typeSym := FindSymbolInUnits(UnitList, String(ParentRTTI^.name));
          if typeSym.IsClassSymbol then // match found, stop searching
          begin
            result := TClassSymbol(typeSym);
            Break;
          end
          else // if no match found yet, try higher ancestor
{$IFDEF FPC}
            ParentRTTI := GetTypeData(ParentRTTI)^.ParentInfo;
{$ELSE}
            ParentRTTI := GetTypeData(ParentRTTI^)^.ParentInfo;
{$ENDIF}
        until ParentRTTI = nil;
      end; { if Assigned }
    end; { if not ExactMatch }
    // If no matches found, error
    if result = nil then
      raise Exception.CreateFmt(RTE_ClassMatchNotFound, [AObject.ClassName]);
  finally
    UnitList.Free;
  end;
end;

function TProgramInfo.RegisterExternalObject(AObject: TObject;
  AutoFree: Boolean; ExactClassMatch: Boolean): IScriptObj;
var
  NewScriptObj: IScriptObj;
  ClassSym: TClassSymbol;
  context: TdwsProgramExecution;
begin
  Assert(Assigned(Execution));
  { This will register an external object (known or not known to the system)
    with the DWS system. If an object that is already registered is passed in
    it will NOT point to the same script object. Currently it is too difficult
    (if not impossible) to obtain the IScriptObj for an existing
    registered external object. This is very useful for registering a new object
    like a TField (ie TDataset.FieldByName()) so the script can refer to the
    object and act on it. }
  ClassSym := FindClassMatch(AObject, ExactClassMatch) as TClassSymbol;
  if Assigned(ClassSym) and Assigned(AObject) then
  begin
    if AutoFree then
      context := Execution
    else
      context := nil;
    NewScriptObj := TScriptObjInstance.Create(ClassSym, context);
    NewScriptObj.ExternalObject := AObject;
    result := NewScriptObj;
  end
  else // no class returned or no object provided
    result := nil; // return 'nil' Id
end;

function TProgramInfo.GetExternalObjForVar(const s: String): TObject;
var
  sObj: IScriptObj;
begin
  sObj := Vars[s].scriptObj;
  if Assigned(sObj) then
    result := sObj.ExternalObject
  else
    result := nil;
end;

function TProgramInfo.FindSymbolInUnits(aUnitList: TUnitSymbolRefList;
  const aName: String): TSymbol;
var
  i: Integer;
  Table: TUnitSymbolTable;
begin
  // Search all units for the symbol
  for i := 0 to aUnitList.Count - 1 do
  begin
    Table := aUnitList[i].Table;
    if Table <> nil then
    begin
      result := Table.FindLocal(aName);
      if Assigned(result) then
        Exit;
    end;
  end;
  result := nil;
end;

// GetSystemTable
//
function TProgramInfo.GetSystemTable: TSystemSymbolTable;
begin
  result := Execution.Prog.systemTable.SymbolTable;
end;

// PrepareScriptObj
//
procedure TProgramInfo.PrepareScriptObj;
begin
  FScriptObj := FExecution.SelfScriptObject^;
end;

// FindSymbolInUnits
//
function TProgramInfo.FindSymbolInUnits(const aName: String): TSymbol;
var
  List: TUnitSymbolRefList;
begin
  List := CreateUnitList;
  try
    result := FindSymbolInUnits(List, aName);
  finally
    List.Free;
  end;
end;

// CreateUnitList
//
function TProgramInfo.CreateUnitList: TUnitSymbolRefList;
var
  Root: TSymbolTable;
  sym: TSymbol;
  unitSym: TUnitSymbol;
  i: Integer;
begin
  // Find the root table for the full compiled program (not just the function)
  if Assigned(Execution) then
    Root := Execution.Prog.RootTable
  else
  begin
    // if no caller provided, make a 'best effort' to find a root.
    Root := FTable;
    while Root.ParentCount > 0 do
      Root := Root.Parents[0];
  end;

  // caller reponsible for freeing
  result := TUnitSymbolRefList.Create;

  // Add all unit symbols to a list
  for i := 0 to Root.Count - 1 do
  begin
    sym := Root.Symbols[i];
    if sym.ClassType = TUnitSymbol then
    begin
      unitSym := TUnitSymbol(sym);
      if result.IndexOf(unitSym) < 0 then // (units may reuse others)
        result.Add(unitSym);
    end;
  end;
end;

// ------------------
// ------------------ TScriptObj ------------------
// ------------------

// SetExecutionContext
//
procedure TScriptObj.SetExecutionContext(exec: TdwsProgramExecution);
begin
  // ignore
end;

// ------------------
// ------------------ TScriptObjInstance ------------------
// ------------------

// Create
//
constructor TScriptObjInstance.Create(aClassSym: TClassSymbol;
  executionContext: TdwsProgramExecution);
var
  externalClass: TClassSymbol;
  fieldIter: TFieldSymbol;
begin
  FClassSym := aClassSym;
  if aClassSym = nil then
    Exit;

  if executionContext <> nil then
    executionContext.ScriptObjCreated(Self);

  SetDataLength(aClassSym.ScriptInstanceSize);

  // initialize fields
  fieldIter := aClassSym.FirstField;
  while fieldIter <> nil do
  begin
    fieldIter.InitDataContext(Self, 0);
    fieldIter := fieldIter.NextField;
  end;

  // initialize OnObjectDestroy
  externalClass := aClassSym;
  while (externalClass <> nil) and
    not Assigned(externalClass.OnObjectDestroy) do
    externalClass := externalClass.Parent;
  if externalClass <> nil then
    FOnObjectDestroy := externalClass.OnObjectDestroy;
end;

// Destroy
//
destructor TScriptObjInstance.Destroy;
begin
  if Assigned(FOnObjectDestroy) then
    FOnObjectDestroy(FExternalObject);
  inherited;
end;

// BeforeDestruction
//
type
  TInterfaceObjectCracker = class(TObject)
    FRefCount: Integer;
  end;

procedure TScriptObjInstance.BeforeDestruction;

  procedure CallDestructor;
  var
    iso: IScriptObj;
  begin
    iso := TScriptObjectWrapper.Create(Self);
    executionContext.DestroyScriptObj(iso);
  end;

var
  destroySymDefault: TMethodSymbol;
  destroySym: TMethodSymbol;
begin
  if Assigned(FExecutionContext) then
  begin
    // we are released, so never do: Self as IScriptObj
    if not FDestroyed then
    begin
      destroySymDefault := executionContext.CompilerContext.
        TypDefaultDestructor;
      destroySym := ClassSym.VMTMethod(destroySymDefault.VMTIndex);
      if (destroySym = destroySymDefault) or (destroySym = nil) then
      begin
        Destroyed := True
      end
      else
      begin
        CallDestructor;
        // workaround for self reference cleared from within constructor
        if RefCount = -1 then
          _AddRef;
      end;
    end;
    executionContext.ScriptObjDestroyed(Self);
  end;
  inherited;
end;

// NewInstance
//
var
  vScriptObjTemplate: TClassInstanceTemplate<TScriptObjInstance>;

class function TScriptObjInstance.NewInstance: TObject;
begin
  if not vScriptObjTemplate.Initialized then
    result := inherited NewInstance
  else
    result := vScriptObjTemplate.CreateInstance;
end;

// FreeInstance
//
procedure TScriptObjInstance.FreeInstance;
begin
  ClearData;
  vScriptObjTemplate.ReleaseInstance(Self);
end;

// ToString
//
function TScriptObjInstance.ToString: String;
begin
  result := ScriptTypeName;
end;

// ScriptTypeName
//
function TScriptObjInstance.ScriptTypeName: String;
begin
  result := FClassSym.name;
end;

// ClearData
//
procedure TScriptObjInstance.ClearData;
begin
  inherited;
  FClassSym := nil;
end;

// FieldAddress
//
function TScriptObjInstance.FieldAddress(const fieldName: String): Integer;
var
  clsSym: TClassSymbol;
  field: TFieldSymbol;
  sym: TSymbol;
begin
  field := nil;
  clsSym := FClassSym;
  repeat
    sym := clsSym.Members.FindLocal(fieldName);
    if (sym <> nil) and (sym.ClassType = TFieldSymbol) then
    begin
      field := TFieldSymbol(sym);
      Break;
    end;
    clsSym := clsSym.Parent;
  until clsSym = nil;
  if field = nil then
    raise Exception.CreateFmt(RTE_FieldNotFoundInClass,
      [fieldName, FClassSym.name]);
  result := field.offset;
end;

// FieldAsString
//
function TScriptObjInstance.FieldAsString(const fieldName: String): String;
begin
  EvalAsString(FieldAddress(fieldName), result);
end;

// FieldAsInteger
//
function TScriptObjInstance.FieldAsInteger(const fieldName: String): Int64;
begin
  result := AsInteger[FieldAddress(fieldName)];
end;

// FieldAsFloat
//
function TScriptObjInstance.FieldAsFloat(const fieldName: String): Double;
begin
  result := AsFloat[FieldAddress(fieldName)];
end;

// FieldAsBoolean
//
function TScriptObjInstance.FieldAsBoolean(const fieldName: String): Boolean;
begin
  result := AsBoolean[FieldAddress(fieldName)];
end;

// FieldAsScriptDynArray
//
function TScriptObjInstance.FieldAsScriptDynArray(const fieldName: String)
  : IScriptDynArray;
var
  intf: IUnknown;
begin
  EvalAsInterface(FieldAddress(fieldName), intf);
  result := intf as IScriptDynArray;
end;

// GetClassSym
//
function TScriptObjInstance.GetClassSym: TClassSymbol;
begin
  result := FClassSym;
end;

// GetExternalObject
//
function TScriptObjInstance.GetExternalObject: TObject;
begin
  result := FExternalObject;
end;

// SetExternalObject
//
procedure TScriptObjInstance.SetExternalObject(value: TObject);
begin
  FExternalObject := value;
end;

// GetDestroyed
//
function TScriptObjInstance.GetDestroyed: Boolean;
begin
  result := FDestroyed;
end;

// SetDestroyed
//
procedure TScriptObjInstance.SetDestroyed(const val: Boolean);
begin
  if Assigned(FOnObjectDestroy) then
  begin
    FOnObjectDestroy(FExternalObject);
    FOnObjectDestroy := nil;
    FExternalObject := nil;
  end;
  FDestroyed := True;
end;

// SetExecutionContext
//
procedure TScriptObjInstance.SetExecutionContext(exec: TdwsProgramExecution);
begin
  FExecutionContext := exec;
end;

// ------------------
// ------------------ TScriptInterface ------------------
// ------------------

// Create
//
constructor TScriptInterface.Create(const instance: IScriptObj;
  const resolvedInterface: TResolvedInterface;
  executionContext: TdwsProgramExecution = nil);
begin
  FInstance := instance;
  FTyp := resolvedInterface.IntfSymbol;
  FVMT := resolvedInterface.VMT;

  if executionContext <> nil then
  begin
    executionContext.ScriptObjCreated(Self);
    FExecutionContext := executionContext;
  end;
end;

// BeforeDestruction
//
procedure TScriptInterface.BeforeDestruction;
begin
  if Assigned(FExecutionContext) then
    FExecutionContext.ScriptObjDestroyed(Self);
  inherited;
end;

// ToString
//
function TScriptInterface.ToString: String;
begin
  result := FTyp.ClassName;
end;

// ScriptTypeName
//
function TScriptInterface.ScriptTypeName: String;
begin
  result := FTyp.ClassName;
end;

// GetScriptObj
//
function TScriptInterface.GetScriptObj: IScriptObj;
begin
  result := instance;
end;

// ------------------
// ------------------ TMethodObjExpr ------------------
// ------------------

constructor TMethodObjExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; baseExpr: TDataExpr);
begin
  Assert(baseExpr.Typ is TMethodSymbol);
  inherited Create(aScriptPos, TMethodSymbol(baseExpr.Typ).StructSymbol);
  FBaseExpr := baseExpr;
end;

// GetDataPtr
//
procedure TMethodObjExpr.GetDataPtr(exec: TdwsExecution;
  var result: IDataContext);
begin
  FBaseExpr.DataPtr[exec].CreateOffset(1, result);
end;

// ------------------
// ------------------ TOverloadedExpr ------------------
// ------------------

// Create
//
constructor TOverloadedExpr.Create(context: TdwsCompilerContext;
  const aScriptPos: TScriptPos; aFunc: TFuncSymbol; aBaseExpr: TTypedExpr);
begin
  if aFunc.ClassType = TAliasMethodSymbol then
  begin

    inherited Create(context, aScriptPos, TAliasMethodSymbol(aFunc).Alias);
    AddArg(aBaseExpr);

  end
  else
  begin

    inherited Create(context, aScriptPos, aFunc);

    if aBaseExpr <> nil then
    begin

      if aFunc is TMethodSymbol then
      begin
        if (aFunc.params.Count > 0) and (aFunc.params[0].name = SYS_SELF) then
        begin
          AddArg(aBaseExpr);
          aBaseExpr.IncRefCount;
        end;
      end;
      FBaseExpr := aBaseExpr;

    end;
  end;
end;

// Destroy
//
destructor TOverloadedExpr.Destroy;
begin
  FBaseExpr.Free;
  inherited;
end;

// ChangeFuncSymbol
//
function TOverloadedExpr.ChangeFuncSymbol(context: TdwsCompilerContext;
  newFuncSym: TFuncSymbol; options: TCreateFunctionOptions): TFuncExprBase;
var
  newMeth: TMethodSymbol;
  refKind: TRefKind;
begin
  if newFuncSym is TMethodSymbol then
  begin

    newMeth := TMethodSymbol(newFuncSym);
    if (baseExpr = nil) or (baseExpr.Typ is TStructuredTypeMetaSymbol) then
      refKind := rkClassOfRef
    else
      refKind := rkObjRef;
    result := CreateMethodExpr(context, newMeth, Self.FBaseExpr, refKind,
      ScriptPos, options);
    Self.FBaseExpr := nil;
    if args.Count > 0 then
    begin
      result.args.Clean;
      TransferArguments(context, result);
    end;
    Self.Free;

  end
  else
  begin

    result := CreateSimpleFuncExpr(context, ScriptPos, newFuncSym);
    TransferArguments(context, result);
    TFuncExprBase(result).ResultAddr := FResultAddr;
    Self.Free;

  end;

  if (newFuncSym.Typ <> nil) and (newFuncSym.Typ.Size > 1) and
    result.InheritsFrom(TFuncExpr) then
    TFuncExpr(result).InitializeResultAddr(context.Prog as TdwsProgram);
end;

// TransferArguments
//
procedure TOverloadedExpr.TransferArguments(context: TdwsCompilerContext;
  dest: TFuncExprBase);
begin
  var
  destParams := dest.funcSym.params;
  Assert(destParams.Count >= args.Count);
  var
  i := 0;
  while i < args.Count do
  begin
    var
    arg := TTypedExpr(args.ExprBase[i]);
    var
    destType := destParams[i].Typ;
    if not destType.IsCompatible(arg.Typ) then
    begin
      arg := CompilerUtils.WrapWithImplicitConversion(context, arg, destType,
        arg.ScriptPos, CPE_IncompatibleParameterTypes);
    end;
    dest.AddArg(arg);
    Inc(i);
  end;
  args.Clear;
end;

// ------------------
// ------------------ TNoResultWrapperExpr ------------------
// ------------------

// Create
//
constructor TNoResultWrapperExpr.Create(const aScriptPos: TScriptPos;
  expr: TTypedExpr);
begin
  inherited Create(aScriptPos);
  FExpr := expr;
end;

// Destroy
//
destructor TNoResultWrapperExpr.Destroy;
begin
  FExpr.Free;
  inherited;
end;

// EvalNoResult
//
procedure TNoResultWrapperExpr.EvalNoResult(exec: TdwsExecution);
begin
  expr.EvalNoResult(exec);
end;

// GetIsConstant
//
function TNoResultWrapperExpr.GetIsConstant: Boolean;
begin
  result := FExpr.IsConstant;
end;

// ScriptPos
//
function TNoResultWrapperExpr.ScriptPos: TScriptPos;
begin
  result := FExpr.ScriptPos;
end;

// GetSubExpr
//
function TNoResultWrapperExpr.GetSubExpr(i: Integer): TExprBase;
begin
  result := FExpr;
end;

// GetSubExprCount
//
function TNoResultWrapperExpr.GetSubExprCount: Integer;
begin
  result := 1;
end;

// ------------------
// ------------------ TSourceCondition ------------------
// ------------------

// Create
//
constructor TSourceCondition.Create(const aScriptPos: TScriptPos;
  aTest, aMsg: TTypedExpr);
begin
  inherited Create;
  FScriptPos := aScriptPos;
  FTest := aTest;
  FMsg := aMsg;
end;

// Destroy
//
destructor TSourceCondition.Destroy;
begin
  inherited;
  FTest.Free;
  FMsg.Free;
end;

// InitSymbol
//
procedure TSourceCondition.InitSymbol(Symbol: TSymbol;
  const Msgs: TdwsCompileMessageList);
begin
end;

// InitExpression
//
procedure TSourceCondition.InitExpression(expr: TExprBase);
begin
  // nothing
end;

// SubExpr
//
function TSourceCondition.SubExpr(i: Integer): TExprBase;
begin
  if i = 0 then
    result := FTest
  else
    result := FMsg;
end;

// SubExprCount
//
function TSourceCondition.SubExprCount: Integer;
begin
  result := 2;
end;

// Specialize
//
function TSourceCondition.Specialize(const context: ISpecializationContext)
  : IExecutable;
begin
  context.AddCompilerError('Conditions cannot yet be specialized');
end;

// EvalAsBoolean
//
function TSourceCondition.EvalAsBoolean(exec: TdwsExecution): Boolean;
begin
  result := FTest.EvalAsBoolean(exec);
end;

// EvalAsString
//
procedure TSourceCondition.EvalAsString(exec: TdwsExecution;
  var result: String);
begin
  FMsg.EvalAsString(exec, result);
end;

// ------------------
// ------------------ TSourceConditions ------------------
// ------------------

// Create
//
constructor TSourceConditions.Create(aProg: TdwsProcedure);
begin
  inherited Create;
  FProg := aProg;
end;

// Destroy
//
destructor TSourceConditions.Destroy;
var
  elem: TRefCountedObject;
begin
  inherited;
  for elem in FItems do
    TSourceCondition(elem)._Release;
  FItems.Clear;
end;

// AddCondition
//
procedure TSourceConditions.AddCondition(condition: TSourceCondition);
begin
  condition._AddRef;
  FItems.Add(condition);
end;

// Test
//
function TSourceConditions.Test(exec: TdwsExecution): TSourceCondition;
var
  i: Integer;
  ptrList: PObjectTightList;
begin
  ptrList := FItems.List;
  for i := 0 to FItems.Count - 1 do
  begin
    result := TSourceCondition(ptrList[i]);
    if not result.EvalAsBoolean(exec) then
      Exit;
  end;
  result := nil;
end;

// EvalNoresult
//
procedure TSourceConditions.EvalNoResult(exec: TdwsExecution);
var
  failed: TSourceCondition;
begin
  failed := Test(exec);
  if failed <> nil then
    RaiseConditionFailed(exec, FProg.FFunc, failed.ScriptPos, failed);
end;

// GetConditions
//
function TSourceConditions.GetConditions(idx: Integer): TSourceCondition;
begin
  result := TSourceCondition(FItems.List[idx]);
end;

// ------------------
// ------------------ TSourcePreConditions ------------------
// ------------------

// RaiseConditionFailed
//
procedure TSourcePreConditions.RaiseConditionFailed(exec: TdwsExecution;
  funcSym: TFuncSymbol; const ScriptPos: TScriptPos;
  const msg: IStringEvalable);
var
  msgStr: String;
begin
  msg.EvalAsString(exec, msgStr);
  (exec as TdwsProgramExecution).RaiseAssertionFailedFmt(nil,
    RTE_PreConditionFailed, [funcSym.QualifiedName, ScriptPos.AsInfo, msgStr],
    ScriptPos, exec);
end;

// ------------------
// ------------------ TSourcePostConditions ------------------
// ------------------

// RaiseConditionFailed
//
procedure TSourcePostConditions.RaiseConditionFailed(exec: TdwsExecution;
  funcSym: TFuncSymbol; const ScriptPos: TScriptPos;
  const msg: IStringEvalable);
var
  msgStr: String;
begin
  msg.EvalAsString(exec, msgStr);
  (exec as TdwsProgramExecution).RaiseAssertionFailedFmt(nil,
    RTE_PostConditionFailed, [funcSym.QualifiedName, ScriptPos.AsInfo, msgStr],
    ScriptPos, exec);
end;

// ------------------
// ------------------ TSourceMethodPreConditions ------------------
// ------------------

// EvalNoResult
//
procedure TSourceMethodPreConditions.EvalNoResult(exec: TdwsExecution);
var
  methSym: TMethodSymbol;
  current: TConditionSymbol;
  conds: TConditionsSymbolTable;
  i: Integer;
begin
  // for pre-conditions find the root and test against those
  methSym := (FProg.func as TMethodSymbol);
  if not methSym.IsOverride then
  begin
    inherited EvalNoResult(exec);
    Exit;
  end;

  while (methSym.ParentMeth <> nil) and (methSym.IsOverride) do
    methSym := methSym.ParentMeth;
  if methSym.IsOverride then
    Exit;

  conds := methSym.Conditions;
  for i := 0 to conds.Count - 1 do
  begin
    current := TConditionSymbol(conds[i]);
    if (current.ClassType = TPreConditionSymbol) and
      not current.condition.EvalAsBoolean(exec) then
      RaiseConditionFailed(exec, methSym, current.ScriptPos, current.Message);
  end;
end;

// ------------------
// ------------------ TSourceMethodPostConditions ------------------
// ------------------

// EvalNoResult
//
procedure TSourceMethodPostConditions.EvalNoResult(exec: TdwsExecution);
var
  methSym: TMethodSymbol;
  current: TConditionSymbol;
  conds: TConditionsSymbolTable;
  i: Integer;
begin
  // for post-conditions, all must pass
  inherited EvalNoResult(exec);
  methSym := (FProg.func as TMethodSymbol);
  if not methSym.IsOverride then
    Exit;
  while methSym <> nil do
  begin
    conds := methSym.Conditions;
    for i := 0 to conds.Count - 1 do
    begin
      current := TConditionSymbol(conds[i]);
      if (current.ClassType = TPostConditionSymbol) and
        not current.condition.EvalAsBoolean(exec) then
        RaiseConditionFailed(exec, methSym, current.ScriptPos, current.Message);
    end;
    methSym := methSym.ParentMeth;
  end;
end;

// ------------------
// ------------------ TExternalFuncHandler ------------------
// ------------------

// InitSymbol
//
procedure TExternalFuncHandler.InitSymbol(Symbol: TSymbol;
  const Msgs: TdwsCompileMessageList);
begin
  // nothing
end;

// InitExpression
//
procedure TExternalFuncHandler.InitExpression(expr: TExprBase);
begin
  // nothing
end;

// Call
//
procedure TExternalFuncHandler.Call(exec: TdwsProgramExecution;
  func: TFuncSymbol);
var
  locArray: TdwsExprLocationArray;
begin
  locArray := exec.GetCallStack;
  raise EdwsExternalFuncHandler.CreateFmt(RTE_UnHandledExternalCall,
    [func.name, locArray[High(locArray)].Location]);
end;

// CompileTimeCheck
//
procedure TExternalFuncHandler.CompileTimeCheck(context: TdwsCompilerContext;
  expr: TFuncExprBase);
begin
  // nothing yet
end;

// SubExpr
//
function TExternalFuncHandler.SubExpr(i: Integer): TExprBase;
begin
  result := nil;
end;

// SubExprCount
//
function TExternalFuncHandler.SubExprCount: Integer;
begin
  result := 0;
end;

// Specialize
//
function TExternalFuncHandler.Specialize(const context: ISpecializationContext)
  : IExecutable;
begin
  context.AddCompilerError('External functions cannot be specialized');
end;

// ------------------
// ------------------ TFuncExprOverloadsHelper ------------------
// ------------------

// Create
//
constructor TFuncExprOverloadsHelper.Create(expr: TFuncExprBase;
  overloads: TFuncSymbolList);
begin
  FExpr := expr;
  FOverloads := overloads;
end;

// ExpectedArg
//
function TFuncExprOverloadsHelper.ExpectedArg: TParamSymbol;
var
  i, n: Integer;
  func: TFuncSymbol;
begin
  result := nil;
  n := FExpr.FArgs.Count;
  for i := 0 to FOverloads.Count - 1 do
  begin
    func := FOverloads.Items[i];
    if n < func.params.Count then
    begin
      if result = nil then
        result := (func.params[n] as TParamSymbol)
      else if not result.Typ.IsOfType(func.params[n].Typ) then
        Exit(nil);
    end
    else
      Exit(nil);
  end;
end;

// ------------------
// ------------------ TdwsSymbolAttribute ------------------
// ------------------

// Create
//
constructor TdwsSymbolAttribute.Create(const aScriptPos: TScriptPos;
  aConstructor: TFuncExprBase);
begin
  inherited Create;
  FScriptPos := aScriptPos;
  FAttributeConstructor := aConstructor;
end;

// Destroy
//
destructor TdwsSymbolAttribute.Destroy;
begin
  FAttributeConstructor.Free;
  inherited;
end;

// ------------------
// ------------------ TdwsSymbolAttributes ------------------
// ------------------

// AttributesFor
//
function TdwsSymbolAttributes.AttributesFor(aSymbol: TSymbol)
  : TdwsSymbolAttributeArray;
var
  i, n: Integer;
begin
  // slow and ugly, for testing until structure gets finalized
  SetLength(result, 0);
  n := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Symbol = aSymbol then
    begin
      SetLength(result, n + 1);
      result[n] := Items[i];
      Inc(n);
    end;
  end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

TdwsGuardianThread.Initialize;

TDynamicArraySymbol.SetInitDynamicArrayProc(TScriptDynamicArray_InitData);

vScriptObjTemplate.Initialize;

finalization

TdwsGuardianThread.Finalize;
TdwsGuardianThread.vExecutionsPool.FreeAll;

vScriptObjTemplate.Finalize;

end.
