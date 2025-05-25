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
unit dwsSymbols;

{$I dws.inc}

interface

uses
  System.SysUtils, System.Classes, System.Types,
  dwsErrors, dwsUtils, dwsDateTime, dwsScriptSource, dwsSpecialKeywords,
  dwsTokenTypes, dwsStack, dwsDataContext, dwsArrayMethodKinds, dwsFileSystem
{$IFDEF FPC}, LazUTF8{$ENDIF};

type

  IScriptObj = interface;
  IScriptObjInterface = interface;
  IScriptDynArray = interface;
  IScriptAssociativeArray = interface;
  IExecutable = interface;

  PIScriptObj = ^IScriptObj;

  TdwsExecution = class;
  TExprBase = class;
  TSymbol = class;
  TBaseSymbol = class;
  TDataSymbol = class;
  TClassSymbol = class;
  TCompositeTypeSymbol = class;
  TStructuredTypeSymbol = class;
  TMethodSymbol = class;
  TFieldSymbol = class;
  TTypeSymbol = class;
  TParamSymbol = class;
  THelperSymbol = class;
  TOperatorSymbol = class;
  TPropertySymbol = class;
  TSymbolTable = class;
  TUnSortedSymbolTable = class;
  TdwsRuntimeMessageList = class;
  EScriptError = class;
  EScriptErrorClass = class of EScriptError;
  TFuncSymbol = class;
  TdwsBaseSymbolsContext = class;
  TPseudoMethodSymbol = class;

  TdwsExprLocation = record
    Expr: TExprBase;
    Prog: TObject;
    function Line: Integer; inline;
    function SourceName: String; inline;
    function Location: String;
  end;

  TdwsExprLocationArray = array of TdwsExprLocation;

  // Interface for external debuggers
  IDebugger = interface
    ['{8D534D14-4C6B-11D5-8DCB-0000216D9E86}']
    procedure StartDebug(exec: TdwsExecution);
    procedure DoDebug(exec: TdwsExecution; Expr: TExprBase);
    procedure StopDebug(exec: TdwsExecution);
    procedure EnterFunc(exec: TdwsExecution; funcExpr: TExprBase);
    procedure LeaveFunc(exec: TdwsExecution; funcExpr: TExprBase);
    function LastDebugStepExpr: TExprBase;
    procedure DebugMessage(const msg: String);
    procedure NotifyException(exec: TdwsExecution; const exceptObj: IScriptObj);
  end;

  TProgramState = (psUndefined, psReadyToRun, psRunning, psRunningStopped,
    psTerminated);

  // Attached and owned by its program execution
  IdwsEnvironment = interface(IGetSelf)
    ['{CCAA438D-76F4-49C2-A3A2-82445BC2976A}']
  end;

  IdwsExecution = interface(dwsUtils.IGetSelf)
    ['{8F2D1D7E-9954-4391-B919-86EF1EE21C8C}']
    function GetMsgs: TdwsRuntimeMessageList;
    function GetDebugger: IDebugger;
    procedure SetDebugger(const aDebugger: IDebugger);
    function GetExecutionObject: TdwsExecution;
    function GetUserObject: TObject;
    procedure SetUserObject(const value: TObject);
    function GetStack: TStack;
    function GetProgramState: TProgramState;
    function GetSleeping: Boolean;

    function GetCallStack: TdwsExprLocationArray;
    function GetLastScriptErrorExpr: TExprBase;

    procedure SuspendDebug;
    procedure ResumeDebug;

    function GetEnvironment: IdwsEnvironment;
    procedure SetEnvironment(const env: IdwsEnvironment);

    property ProgramState: TProgramState read GetProgramState;
    property Sleeping: Boolean read GetSleeping;
    property Stack: TStack read GetStack;
    property Msgs: TdwsRuntimeMessageList read GetMsgs;
    property Debugger: IDebugger read GetDebugger write SetDebugger;
    property ExecutionObject: TdwsExecution read GetExecutionObject;
    property UserObject: TObject read GetUserObject write SetUserObject;
    property Environment: IdwsEnvironment read GetEnvironment
      write SetEnvironment;
  end;

  ISpecializationContext = interface(IGetSelf)
    ['{88E5D42F-5E6E-4DFC-B32E-258333B5A6E7}']
    function Specialize(sym: TSymbol): TSymbol;
    function SpecializeType(typ: TTypeSymbol): TTypeSymbol;
    function SpecializeDataSymbol(ds: TDataSymbol): TDataSymbol;
    function SpecializeField(fld: TFieldSymbol): TFieldSymbol;
    procedure SpecializeTable(source, destination: TSymbolTable);
    function SpecializeExecutable(const exec: IExecutable): IExecutable;

    procedure RegisterSpecialization(generic, specialized: TSymbol);

    function SpecializedObject(obj: TRefCountedObject): TRefCountedObject;
    procedure RegisterSpecializedObject(generic, specialized
      : TRefCountedObject);

    procedure RegisterInternalType(sym: TSymbol);

    procedure AddCompilerHint(const msg: String);
    procedure AddCompilerError(const msg: String);
    procedure AddCompilerErrorFmt(const msgFmt: String;
      const params: array of const); overload;
    procedure AddCompilerErrorFmt(const aScriptPos: TScriptPos;
      const msgFmt: String; const params: array of const); overload;

    function Name: String;
    function Parameters: TUnSortedSymbolTable;
    function Values: TUnSortedSymbolTable;
    function UnitSymbol: TSymbol;
    function Msgs: TdwsCompileMessageList;
    function Optimize: Boolean;
    function BaseSymbols: TdwsBaseSymbolsContext;
    function GenericSymbol: TSymbol;
    function GenericSymbolType: TSymbol;

    procedure EnterComposite(sym: TCompositeTypeSymbol);
    procedure LeaveComposite;
    function CompositeSymbol: TCompositeTypeSymbol;

    procedure EnterFunction(funcSym: TFuncSymbol);
    procedure LeaveFunction;
    function FuncSymbol: TFuncSymbol;
  end;

  TRuntimeErrorMessage = class(TErrorMessage)
  private
    FCallStack: TdwsExprLocationArray;

  public
    function AsInfo: String; override;

    property CallStack: TdwsExprLocationArray read FCallStack;
  end;

  // TdwsRuntimeMessageList
  //
  TdwsRuntimeMessageList = class(TdwsMessageList)
  public
    procedure AddRuntimeError(const Text: String); overload;
    procedure AddRuntimeError(e: Exception); overload;
    procedure AddRuntimeError(const scriptPos: TScriptPos; const Text: String;
      const CallStack: TdwsExprLocationArray); overload;
  end;

  TExecutionStatusResult = (esrNone, esrExit, esrBreak, esrContinue);

  TExprBaseProc = procedure(Expr: TExprBase) of object;
  TExprBaseEnumeratorProc = procedure(parent, Expr: TExprBase;
    var abort: Boolean) of object;

  // Is thrown by "raise" statements in script code
  EScriptException = class(Exception)
  private
    FExceptObj: IScriptObj;
    FScriptPos: TScriptPos;
    FScriptCallStack: TdwsExprLocationArray;

  public
    constructor Create(const msgString: String;
      const anExceptionObj: IScriptObj; const aScriptPos: TScriptPos); overload;

    property ExceptionObj: IScriptObj read FExceptObj;
    property scriptPos: TScriptPos read FScriptPos write FScriptPos;
    property ScriptCallStack: TdwsExprLocationArray read FScriptCallStack
      write FScriptCallStack;
  end;

  // Is thrown by failed Assert() statements in script code
  EScriptAssertionFailed = class(EScriptException)
  end;
  // Base class for all Exprs

  { TExprBase }

  TExprBaseClass = class of TExprBase;

  TExprBaseTaxonomy = (ebtExprBase, ebtBlockExprBase);

  TExprBase = class(TRefCountedObject)
  protected
    function GetSubExpr(i: Integer): TExprBase; virtual;
    function GetSubExprCount: Integer; virtual;

    function GetIsConstant: Boolean; virtual;

  public
    function IsConstant: Boolean; inline;

    function Eval(exec: TdwsExecution): Variant;
      deprecated 'Use appropriate EvalAsXxx instead';
    function EvalAsInteger(exec: TdwsExecution): Int64; virtual; abstract;
    function EvalAsBoolean(exec: TdwsExecution): Boolean; virtual; abstract;
    function EvalAsFloat(exec: TdwsExecution): Double; virtual; abstract;
    procedure EvalAsString(exec: TdwsExecution; var result: String); overload;
      virtual; abstract;
    procedure EvalAsVariant(exec: TdwsExecution; var result: Variant); overload;
      virtual; abstract;
    procedure EvalAsVariantToDataContext(exec: TdwsExecution;
      const dc: IDataContext; offset: Integer);
    procedure EvalAsInterface(exec: TdwsExecution; var result: IUnknown);
      virtual; abstract;
    procedure EvalAsScriptObj(exec: TdwsExecution; var result: IScriptObj);
      virtual; abstract;
    procedure EvalAsScriptObjInterface(exec: TdwsExecution;
      var result: IScriptObjInterface); virtual; abstract;
    procedure EvalAsScriptDynArray(exec: TdwsExecution;
      var result: IScriptDynArray); virtual; abstract;
    procedure EvalAsScriptAssociativeArray(exec: TdwsExecution;
      var result: IScriptAssociativeArray); virtual; abstract;
    procedure EvalNoResult(exec: TdwsExecution); virtual;

    procedure EvalAsSafeScriptObj(exec: TdwsExecution;
      var result: IScriptObj); overload;

    procedure AssignValue(exec: TdwsExecution; const value: Variant);
      virtual; abstract;
    procedure AssignValueAsInteger(exec: TdwsExecution; const value: Int64);
      virtual; abstract;
    procedure AssignValueAsBoolean(exec: TdwsExecution; const value: Boolean);
      virtual; abstract;
    procedure AssignValueAsFloat(exec: TdwsExecution; const value: Double);
      virtual; abstract;
    procedure AssignValueAsString(exec: TdwsExecution; const value: String);
      virtual; abstract;
    procedure AssignValueAsScriptObj(exec: TdwsExecution;
      const value: IScriptObj); virtual; abstract;
    procedure AssignValueAsScriptDynArray(exec: TdwsExecution;
      const value: IScriptDynArray); virtual; abstract;

    property SubExpr[i: Integer]: TExprBase read GetSubExpr;
    property SubExprCount: Integer read GetSubExprCount;

    function scriptPos: TScriptPos; virtual; abstract;
    function ScriptLocation(Prog: TObject): String; virtual; abstract;

    function Taxonomy: TExprBaseTaxonomy; virtual;

    function FuncSymQualifiedName: String; virtual;
    class function CallStackToString(const CallStack: TdwsExprLocationArray)
      : String; static;

    // returns True if aborted
    function RecursiveEnumerateSubExprs(const callback
      : TExprBaseEnumeratorProc): Boolean;
    function ReferencesVariable(varSymbol: TDataSymbol): Boolean; virtual;
    function IndexOfSubExpr(Expr: TExprBase): Integer;

    procedure RaiseScriptError(exec: TdwsExecution; e: EScriptError); overload;
    procedure RaiseScriptError(exec: TdwsExecution); overload;
    procedure RaiseScriptError(exec: TdwsExecution; const msg: String);
      overload;
    procedure RaiseScriptError(exec: TdwsExecution;
      exceptClass: EScriptErrorClass; const msg: String); overload;
    procedure RaiseScriptError(exec: TdwsExecution;
      exceptClass: EScriptErrorClass; const msg: String;
      const args: array of const); overload;

    procedure RaiseObjectNotInstantiated(exec: TdwsExecution);
    procedure RaiseObjectAlreadyDestroyed(exec: TdwsExecution);

    function Specialize(const context: ISpecializationContext)
      : TExprBase; virtual;
  end;

  // All functions callable from the script implement this interface
  IExecutable = interface(IGetSelf)
    ['{8D534D18-4C6B-11D5-8DCB-0000216D9E86}']
    procedure InitSymbol(symbol: TSymbol; const Msgs: TdwsCompileMessageList);
    procedure InitExpression(Expr: TExprBase);
    function SubExpr(i: Integer): TExprBase;
    function SubExprCount: Integer;
    function Specialize(const context: ISpecializationContext): IExecutable;
  end;

  IBooleanEvalable = interface(IExecutable)
    ['{C984224C-92FC-41EF-845A-CE5CA0F8C77D}']
    function EvalAsBoolean(exec: TdwsExecution): Boolean;
  end;

  IStringEvalable = interface(IExecutable)
    ['{6D0552ED-6FBD-4BC7-AADA-8D8F8DBDF29B}']
    procedure EvalAsString(exec: TdwsExecution; var result: String);
  end;

  TAddrGeneratorSign = (agsPositive, agsNegative);

  // TAddrGenerator
  //
  TAddrGeneratorRec = record
  private
    FLevel: SmallInt;
    FSign: TAddrGeneratorSign;

  public
    DataSize: Integer;

    class function CreatePositive(aLevel: SmallInt; anInitialSize: Integer = 0)
      : TAddrGeneratorRec; static;
    class function CreateNegative(aLevel: SmallInt): TAddrGeneratorRec; static;

    function GetStackAddr(size: Integer): Integer;

    property Level: SmallInt read FLevel;
  end;

  TAddrGenerator = ^TAddrGeneratorRec;

  TdwsVisibility = (cvMagic, cvPrivate, cvProtected, cvPublic, cvPublished);
  TdwsVisibilities = set of TdwsVisibility;

  TdwsSymbolTaxonomy = set of (stValueSymbol, stConstSymbol, stDataSymbol,
    stTypeSymbol, stClassSymbol, stAliasSymbol);

  // TSymbol
  //
  // Named item in the script
  TSymbol = class(TRefCountedObject)
  private
    FName: String;
    FExternalName: String;

  protected
    FTyp: TTypeSymbol;
    FSize: Integer;

    function SafeGetCaption: String;
    function GetCaption: String; virtual;
    function GetDescription: String; virtual;
    function GetAsFuncSymbol: TFuncSymbol; virtual;
    function GetIsGeneric: Boolean; virtual;
    function GetExternalName: String;
    function GetIsOverloaded: Boolean; virtual;

    function GetIsClassSymbol: Boolean; virtual;
    function GetIsDataSymbol: Boolean; virtual;

  public
    constructor Create(const aName: String; aType: TTypeSymbol);

    procedure Initialize(const Msgs: TdwsCompileMessageList); virtual;
    function BaseType: TTypeSymbol; virtual;
    procedure SetName(const newName: String; force: Boolean = False);

    class function IsBaseType: Boolean; virtual;
    function IsType: Boolean; virtual;
    function IsPointerType: Boolean; virtual;

    function Taxonomy: TdwsSymbolTaxonomy; virtual;
    function IsClassSymbol: Boolean; inline;
    function IsDataSymbol: Boolean; inline;

    function AsFuncSymbol: TFuncSymbol; overload;
    function AsFuncSymbol(var funcSym: TFuncSymbol): Boolean; overload;
    function IsGeneric: Boolean;
    property IsOverloaded: Boolean read GetIsOverloaded;

    function QualifiedName: String; virtual;

    function IsVisibleFor(const aVisibility: TdwsVisibility): Boolean; virtual;

    function Specialize(const context: ISpecializationContext)
      : TSymbol; virtual;

    // Caption is a short way to describe the symbol,
    // using the Name when set or a type summary for anonymous symbols
    // it should be single-ligne
    property Caption: String read SafeGetCaption;

    // Description is a more detailed description than Caption
    // and can span multiple lines, explicit constant values, etc.
    property Description: String read GetDescription;

    property Name: String read FName;
    property typ: TTypeSymbol read FTyp write FTyp;
    property size: Integer read FSize;

    function HasExternalName: Boolean; inline;
    property ExternalName: String read GetExternalName write FExternalName;
  end;

  TSymbolClass = class of TSymbol;

  // return True to abort
  TSymbolEnumerationCallback = function(symbol: TSymbol): Boolean of object;

  THelperSymbolEnumerationCallback = function(helper: THelperSymbol)
    : Boolean of object;

  TOperatorSymbolEnumerationCallback = function(opSym: TOperatorSymbol)
    : Boolean of object;

  TSymbolTableFlag = (stfSorted, stfHasChildTables, stfHasHelpers,
    stfHasLocalOperators, stfHasParentOperators, stfHasOperators,
    stfHasClassSymbols, stfHasNestedTypes);
  TSymbolTableFlags = set of TSymbolTableFlag;

  TSimpleSymbolList = TSimpleList<TSymbol>;

  // A table of symbols connected to other symboltables (property Parents)
  TSymbolTable = class(TRefCountedObject)
  private
    FAddrGenerator: TAddrGenerator;
    FSymbols: TTightList;
    FParents: TTightList;
    FFlags: TSymbolTableFlags;

    function GetParents(Index: Integer): TSymbolTable; inline;

  protected
    function GetSymbol(Index: Integer): TSymbol; inline;

    procedure SortSymbols(minIndex, maxIndex: Integer);

  public
    constructor Create(parent: TSymbolTable = nil;
      addrGenerator: TAddrGenerator = nil);
    destructor Destroy; override;

    procedure InsertParent(Index: Integer; parent: TSymbolTable); virtual;
    function RemoveParent(parent: TSymbolTable): Integer; virtual;
    function IndexOfParent(parent: TSymbolTable): Integer;
    procedure MoveParent(curIndex, newIndex: Integer);
    procedure ClearParents; virtual;
    procedure AddParent(parent: TSymbolTable);

    function AddSymbol(sym: TSymbol): Integer;
    function AddSymbolDirect(sym: TSymbol): Integer;
    function FindLocal(const aName: String): TSymbol; virtual;
    function FindLocalOfClass(const aName: String;
      ofClass: TSymbolClass): TSymbol;
    function FindTypeLocal(const aName: String): TTypeSymbol;
    function FindSymbolAtStackAddr(const stackAddr, Level: Integer)
      : TDataSymbol;
    function Remove(sym: TSymbol): Integer;
    procedure Clear;

    procedure TransferSymbolsTo(destTable: TSymbolTable);

    function FindSymbol(const aName: String; minVisibility: TdwsVisibility;
      ofClass: TSymbolClass = nil): TSymbol; virtual;
    function FindTypeSymbol(const aName: String; minVisibility: TdwsVisibility)
      : TTypeSymbol;

    // returns True if aborted
    function EnumerateLocalSymbolsOfName(const aName: String;
      const callback: TSymbolEnumerationCallback): Boolean; virtual;
    function EnumerateSymbolsOfNameInScope(const aName: String;
      const callback: TSymbolEnumerationCallback): Boolean; virtual;

    function EnumerateLocalHelpers(helpedType: TTypeSymbol;
      const callback: THelperSymbolEnumerationCallback): Boolean; virtual;
    function EnumerateHelpers(helpedType: TTypeSymbol;
      const callback: THelperSymbolEnumerationCallback): Boolean; virtual;

    function EnumerateLocalOperatorsFor(aToken: TTokenType;
      aLeftType, aRightType: TTypeSymbol;
      const callback: TOperatorSymbolEnumerationCallback): Boolean; virtual;
    function EnumerateOperatorsFor(aToken: TTokenType;
      aLeftType, aRightType: TTypeSymbol;
      const callback: TOperatorSymbolEnumerationCallback): Boolean; virtual;
    function FindImplicitCastOperatorFor(fromType, toType: TTypeSymbol)
      : TOperatorSymbol; virtual;
    function HasSameLocalOperator(anOpSym: TOperatorSymbol): Boolean; virtual;
    function HasOperators: Boolean; inline;

    procedure CollectPublishedSymbols(symbolList: TSimpleSymbolList); virtual;

    function HasChildTables: Boolean; inline;
    function HasClass(const aClass: TSymbolClass): Boolean;
    function HasSymbol(sym: TSymbol): Boolean;
    function HasMethods: Boolean;
    function HasClassSymbols: Boolean;
    function HasNestedtypes: Boolean; inline;
    class function IsUnitTable: Boolean; virtual;

    procedure Initialize(const Msgs: TdwsCompileMessageList); virtual;

    property addrGenerator: TAddrGenerator read FAddrGenerator;
    property Count: Integer read FSymbols.FCount;
    property Symbols[x: Integer]: TSymbol read GetSymbol; default;
    property ParentCount: Integer read FParents.FCount;
    property Parents[Index: Integer]: TSymbolTable read GetParents;

  type
    TSymbolTableEnumerator = record
      Index: Integer;
      Table: TSymbolTable;
      function MoveNext: Boolean;
      function GetCurrent: TSymbol;
      property Current: TSymbol read GetCurrent;
    end;

  function GetEnumerator: TSymbolTableEnumerator;
  end;

  TSymbolTableClass = class of TSymbolTable;

  // TUnSortedSymbolTable
  //
  TUnSortedSymbolTable = class(TSymbolTable)
  public
    function FindLocal(const aName: String): TSymbol; override;
    function IndexOf(sym: TSymbol): Integer;
  end;

  // TConditionsSymbolTable
  //
  TConditionsSymbolTable = class(TUnSortedSymbolTable)
  end;

  // TParamsSymbolTable
  //
  TParamsSymbolTable = class(TUnSortedSymbolTable)
  protected
    function GetSymbol(x: Integer): TParamSymbol;

  public
    function Description(skip: Integer): String;

    property Symbols[x: Integer]: TParamSymbol read GetSymbol; default;
  end;

  // TExpressionSymbolTable
  //
  TExpressionSymbolTable = class(TSymbolTable)
  end;

  // A resource string (hybrid between a constant and a function)
  TResourceStringSymbol = class sealed(TSymbol)
  private
    FValue: String;
    FIndex: Integer;

  protected
    function GetCaption: String; override;
    function GetDescription: String; override;

  public
    constructor Create(const aName, aValue: String);

    property value: String read FValue;
    property Index: Integer read FIndex write FIndex;
  end;

  TResourceStringSymbolList = class(TSimpleList<TResourceStringSymbol>)
  public
    procedure ComputeIndexes;
  end;

  // All Symbols containing a value
  TValueSymbol = class(TSymbol)
  protected
    function GetCaption: String; override;
    function GetDescription: String; override;

  public
    constructor Create(const aName: String; aType: TTypeSymbol);

    function Taxonomy: TdwsSymbolTaxonomy; override;
  end;

  // named constant: const x = 123;
  TConstSymbol = class(TValueSymbol)
  protected
    FDataContext: IDataContext;
    FDeprecatedMessage: String;

    function GetCaption: String; override;
    function GetDescription: String; override;
    function GetIsDeprecated: Boolean; inline;

  public
    constructor Create(const Name: String; typ: TTypeSymbol);
    constructor CreateValue(const Name: String; typ: TTypeSymbol;
      const value: Variant); overload;
    constructor CreateData(const Name: String; typ: TTypeSymbol;
      const data: IDataContext); overload;

    function Taxonomy: TdwsSymbolTaxonomy; override;

    procedure Initialize(const Msgs: TdwsCompileMessageList); override;

    property DataContext: IDataContext read FDataContext;

    property DeprecatedMessage: String read FDeprecatedMessage
      write FDeprecatedMessage;
    property IsDeprecated: Boolean read GetIsDeprecated;
  end;

  TConstSymbolClass = class of TConstSymbol;

  TScriptDataSymbolPurpose = (sdspGeneral,
    // general purpose / unspecified use case
    sdspLoopIterator, // iterator variable in a for loop
    sdspScriptInternal // internal use for script engine only
    );

  // variable: var x: Integer;
  TDataSymbol = class(TValueSymbol)
  protected
    FStackAddr: Integer;
    FLevel: SmallInt;
    FUsedBySubLevel: Boolean;

    function GetDescription: String; override;

  public
    procedure AllocateStackAddr(generator: TAddrGenerator);

    function Taxonomy: TdwsSymbolTaxonomy; override;
    function GetIsDataSymbol: Boolean; override; final;

    function IsWritable: Boolean; virtual;
    function GetPurpose: TScriptDataSymbolPurpose; virtual;

    property Level: SmallInt read FLevel write FLevel;
    property UsedBySubLevel: Boolean read FUsedBySubLevel write FUsedBySubLevel;
    property stackAddr: Integer read FStackAddr write FStackAddr;
  end;

  // used for script engine internal purposes
  TScriptDataSymbol = class sealed(TDataSymbol)
  private
    FPurpose: TScriptDataSymbolPurpose;

  public
    constructor Create(const aName: String; aType: TTypeSymbol;
      aPurpose: TScriptDataSymbolPurpose = sdspGeneral);
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;

    function GetPurpose: TScriptDataSymbolPurpose; override;
    property Purpose: TScriptDataSymbolPurpose read FPurpose write FPurpose;
  end;

  // used for variables
  TVarDataSymbol = class sealed(TDataSymbol)
  public
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;
  end;

  TParamSymbolSemantics = (pssCopy, pssConst, pssVar, pssLazy);
  TParamSymbolOption = (psoForbidImplicitCasts, psoInternal);
  TParamSymbolOptions = set of TParamSymbolOption;

  // parameter: procedure P(x: Integer);
  TParamSymbol = class(TDataSymbol)
  private
    FOptions: TParamSymbolOptions;

  protected
    function GetDescription: String; override;

  public
    constructor Create(const aName: String; aType: TTypeSymbol;
      options: TParamSymbolOptions = []);

    function Clone: TParamSymbol; virtual;
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;
    function SameParam(other: TParamSymbol): Boolean; virtual;
    function Semantics: TParamSymbolSemantics; virtual;
    function ForbidImplicitCasts: Boolean;
    function IsInternal: Boolean;
  end;

  THasParamSymbolMethod = function(param: TParamSymbol): Boolean of object;
  TAddParamSymbolMethod = procedure(param: TParamSymbol) of object;

  TParamSymbolWithDefaultValue = class sealed(TParamSymbol)
  private
    FDefaultValue: IDataContext;

  protected
    function GetDescription: String; override;

  public
    constructor Create(const aName: String; aType: TTypeSymbol;
      const srcDC: IDataContext; options: TParamSymbolOptions = []); overload;

    function Clone: TParamSymbol; override;
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;
    function SameParam(other: TParamSymbol): Boolean; override;

    property DefaultValue: IDataContext read FDefaultValue;
  end;

  // const/var parameter: procedure P(const/var x: Integer)
  TByRefParamSymbol = class(TParamSymbol)
  public
    constructor Create(const aName: String; aTyp: TTypeSymbol;
      const options: TParamSymbolOptions);
    function Clone: TParamSymbol; override;
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;
  end;

  // lazy parameter: procedure P(lazy x: Integer)
  TLazyParamSymbol = class sealed(TParamSymbol)
  public
    function Clone: TParamSymbol; override;
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;
    function Semantics: TParamSymbolSemantics; override;
  end;

  // const parameter: procedure P(const(ref) x: Integer)
  TConstByRefParamSymbol = class sealed(TByRefParamSymbol)
  public
    function Clone: TParamSymbol; override;
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;
    function IsWritable: Boolean; override;
    function Semantics: TParamSymbolSemantics; override;
  end;

  // const parameter: procedure P(const(copy) x: Integer)
  TConstByValueParamSymbol = class(TParamSymbol)
  public
    function Clone: TParamSymbol; override;
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;
    function IsWritable: Boolean; override;
    function Semantics: TParamSymbolSemantics; override;
  end;

  // var parameter: procedure P(var x: Integer)
  TVarParamSymbol = class sealed(TByRefParamSymbol)
  public
    function Clone: TParamSymbol; override;
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;
    function Semantics: TParamSymbolSemantics; override;
  end;

  TTypeSymbolClass = class of TTypeSymbol;
  TTypeSymbols = array of TTypeSymbol;

  // Base class for all types
  TTypeSymbol = class(TSymbol)
  private
    FDeprecatedMessage: String;

  protected
    function DoIsOfType(typSym: TTypeSymbol): Boolean; virtual;
    function GetUnAliasedType: TTypeSymbol; virtual;

    function GetIsDeprecated: Boolean; inline;

  public
    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); virtual;
    procedure InitString(var s: String); virtual;
    function DynamicInitialization: Boolean; virtual;

    function IsType: Boolean; override;
    function BaseType: TTypeSymbol; override;
    function UnAliasedType: TTypeSymbol; inline;
    function UnAliasedTypeIs(const typeSymbolClass: TTypeSymbolClass)
      : Boolean; inline;
    function IsOfType(typSym: TTypeSymbol): Boolean;
    function IsCompatible(typSym: TTypeSymbol): Boolean; virtual;
    function CanExpectAnyFuncSymbol: Boolean; virtual;
    function IsCompatibleWithAnyFuncSymbol: Boolean; virtual;

    function Taxonomy: TdwsSymbolTaxonomy; override;

    function DistanceTo(typeSym: TTypeSymbol): Integer; virtual;
    // doesn't treat aliases of a type as the the same type,
    // but identical declarations are
    function SameType(typSym: TTypeSymbol): Boolean; virtual;
    function HasMetaSymbol: Boolean; virtual;
    function IsForwarded: Boolean; virtual;
    function AssignsAsDataExpr: Boolean; virtual;

    function Specialize(const context: ISpecializationContext): TSymbol;
      override; final;
    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; virtual;

    property DeprecatedMessage: String read FDeprecatedMessage
      write FDeprecatedMessage;
    property IsDeprecated: Boolean read GetIsDeprecated;
  end;

  TAnyTypeSymbol = class sealed(TTypeSymbol)
  public
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
  end;

  TFuncKind = (fkFunction, fkProcedure, fkConstructor, fkDestructor, fkMethod,
    fkLambda);

  // Record used for TFuncSymbol.Generate
  PParamRec = ^TParamRec;

  TParamRec = record
    ParamName: String;
    ParamType: String;
    IsVarParam: Boolean;
    IsConstParam: Boolean;
    HasDefaultValue: Boolean;
    options: TParamSymbolOptions;
    DefaultValue: IDataContext;
  end;

  TParamArray = array of TParamRec;

  // Condition, as part of contracts
  TConditionSymbol = class(TSymbol)
  private
    FScriptPos: TScriptPos;
    FCondition: IBooleanEvalable;
    FMessage: IStringEvalable;

  protected

  public
    constructor Create(const aScriptPos: TScriptPos;
      const cond: IBooleanEvalable; const msg: IStringEvalable);

    property scriptPos: TScriptPos read FScriptPos write FScriptPos;
    property Condition: IBooleanEvalable read FCondition write FCondition;
    property Message: IStringEvalable read FMessage write FMessage;
  end;

  TConditionSymbolClass = class of TConditionSymbol;

  TPreConditionSymbol = class(TConditionSymbol)
  private

  protected

  public

  end;

  TPostConditionSymbol = class(TConditionSymbol)
  private

  protected

  public

  end;

  TClassInvariantSymbol = class(TConditionSymbol)
  private

  protected

  public

  end;

  TResultSymbol = class sealed(TDataSymbol)
  public
    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;
  end;

  TFuncSymbolFlag = (fsfStateless, fsfExternal, fsfType, fsfOverloaded,
    fsfLambda, fsfInline, fsfProperty, fsfExport, fsfOfObject, fsfReferenceTo,
    fsfAsync);
  TFuncSymbolFlags = set of TFuncSymbolFlag;

  // A script function / procedure: procedure X(param: Integer);
  TFuncSymbol = class(TTypeSymbol)
  protected
    FAddrGenerator: TAddrGeneratorRec;
    FExecutable: IExecutable;
    FInternalParams: TSymbolTable;
    FForwardPosition: PScriptPos;
    FParams: TParamsSymbolTable;
    FResult: TDataSymbol;
    FConditions: TConditionsSymbolTable;
    FFlags: TFuncSymbolFlags;
    FKind: TFuncKind;
    FExternalConvention: TTokenType;

    procedure SetType(const value: TTypeSymbol);

    function GetCaption: String; override;
    function GetDescription: String; override;
    function GetLevel: SmallInt; inline;
    function GetParamSize: Integer; inline;

    function GetIsStateless: Boolean; inline;
    procedure SetIsStateless(const val: Boolean); inline;
    function GetIsExternal: Boolean; inline;
    procedure SetIsExternal(const val: Boolean); inline;
    function GetIsExport: Boolean; inline;
    procedure SetIsExport(const val: Boolean); inline;
    function GetIsProperty: Boolean; inline;
    procedure SetIsProperty(const val: Boolean); inline;
    function GetIsOverloaded: Boolean; override;
    function GetIsOverloadedDirect: Boolean; inline;
    procedure SetIsOverloaded(const val: Boolean); inline;
    function GetIsLambda: Boolean; inline;
    procedure SetIsLambda(const val: Boolean); inline;
    function GetIsOfObject: Boolean; inline;
    procedure SetIsOfObject(const val: Boolean); inline;
    function GetIsReferenceTo: Boolean; inline;
    procedure SetIsReferenceTo(const val: Boolean); inline;
    function GetIsAsync: Boolean; inline;
    procedure SetIsAsync(const val: Boolean); inline;

    function GetDeclarationPosition: TScriptPos; virtual;
    procedure SetDeclarationPosition(const val: TScriptPos); virtual;
    function GetImplementationPosition: TScriptPos; virtual;
    procedure SetImplementationPosition(const val: TScriptPos); virtual;

    function GetSourceSubExpr(i: Integer): TExprBase;
    function GetSourceSubExprCount: Integer;

    function DoIsOfType(typSym: TTypeSymbol): Boolean; override;

    procedure InternalSpecialize(destination: TFuncSymbol;
      const context: ISpecializationContext);

  public
    constructor Create(const Name: String; funcKind: TFuncKind;
      funcLevel: SmallInt);
    destructor Destroy; override;

    constructor Generate(Table: TSymbolTable; const funcName: String;
      const funcParams: TParamArray; const funcType: String);
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function IsType: Boolean; override;
    procedure SetIsType;
    function GetAsFuncSymbol: TFuncSymbol; override;
    procedure SetInline;
    procedure AddParam(param: TParamSymbol);
    procedure AddParams(params: TParamsSymbolTable);
    function HasParam(param: TParamSymbol): Boolean;
    procedure GenerateParams(Table: TSymbolTable;
      const funcParams: TParamArray);
    function GetParamType(idx: Integer): TTypeSymbol;
    function ParamTypeForbidsImplicitCasts(idx: Integer): Boolean;
    procedure Initialize(const Msgs: TdwsCompileMessageList); override;
    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    procedure AddCondition(cond: TConditionSymbol);

    function IsValidOverloadOf(other: TFuncSymbol): Boolean;
    function IsSameOverloadOf(other: TFuncSymbol): Boolean; virtual;
    function SameType(typSym: TTypeSymbol): Boolean; override;

    function ParamsDescription: String; virtual;

    procedure SetForwardedPos(const aScriptPos: TScriptPos);
    procedure ClearIsForwarded;

    property SubExpr[i: Integer]: TExprBase read GetSourceSubExpr;
    property SubExprCount: Integer read GetSourceSubExprCount;

    property Executable: IExecutable read FExecutable write FExecutable;
    property IsDeprecated: Boolean read GetIsDeprecated;
    property IsStateless: Boolean read GetIsStateless write SetIsStateless;
    function IsForwarded: Boolean; override;
    property IsOverloaded: Boolean read GetIsOverloadedDirect
      write SetIsOverloaded;
    property IsExternal: Boolean read GetIsExternal write SetIsExternal;
    property IsExport: Boolean read GetIsExport write SetIsExport;
    property IsProperty: Boolean read GetIsProperty write SetIsProperty;
    property IsOfObject: Boolean read GetIsOfObject write SetIsOfObject;
    property IsReferenceTo: Boolean read GetIsReferenceTo
      write SetIsReferenceTo;
    property IsAsync: Boolean read GetIsAsync write SetIsAsync;
    property Kind: TFuncKind read FKind write FKind;
    property ExternalConvention: TTokenType read FExternalConvention
      write FExternalConvention;
    property IsLambda: Boolean read GetIsLambda write SetIsLambda;
    property Level: SmallInt read GetLevel;
    property InternalParams: TSymbolTable read FInternalParams;
    property params: TParamsSymbolTable read FParams;
    property ParamSize: Integer read FAddrGenerator.DataSize;
    property result: TDataSymbol read FResult;
    property typ: TTypeSymbol read FTyp write SetType;
    property Conditions: TConditionsSymbolTable read FConditions;

    property DeclarationPosition: TScriptPos read GetDeclarationPosition
      write SetDeclarationPosition;
    property ImplementationPosition: TScriptPos read GetImplementationPosition
      write SetImplementationPosition;
  end;

  // referring list of function symbols
  TFuncSymbolList = class(TSimpleList<TFuncSymbol>)
  public
    function ContainsChildMethodOf(methSym: TMethodSymbol): Boolean;
  end;

  TAnyFuncSymbol = class(TFuncSymbol)
  public
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function IsCompatibleWithAnyFuncSymbol: Boolean; override;

    procedure Initialize(const Msgs: TdwsCompileMessageList); override;
  end;

  TSourceFuncSymbol = class sealed(TFuncSymbol)
  private
    FDeclarationPosition: TScriptPos;
    FImplementationPosition: TScriptPos;

  protected
    function GetDeclarationPosition: TScriptPos; override;
    procedure SetDeclarationPosition(const val: TScriptPos); override;
    function GetImplementationPosition: TScriptPos; override;
    procedure SetImplementationPosition(const val: TScriptPos); override;

  public
    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; override;

    property SubExpr;
    property SubExprCount;
  end;

  // TSelfSymbol
  //
  TSelfSymbol = class sealed(TDataSymbol)
  end;

  TMethodKind = (mkProcedure, mkFunction, mkConstructor, mkDestructor, mkMethod,
    mkClassProcedure, mkClassFunction, mkClassMethod);
  TMethodAttribute = (maVirtual, maOverride, maReintroduce, maAbstract,
    maOverlap, maClassMethod, maFinal, maDefault, maInterfaced, maStatic,
    maIgnoreMissingImplementation);
  TMethodAttributes = set of TMethodAttribute;
  TMethodCreateOption = (mcoClassMethod, mcoHelperMethod);
  TMethodCreateOptions = set of TMethodCreateOption;

  // A method of a script class: TMyClass = class procedure X(param: String); end;
  TMethodSymbol = class(TFuncSymbol)
  private
    FStructSymbol: TCompositeTypeSymbol;
    FParentMeth: TMethodSymbol;
    FSelfSym: TDataSymbol;
    FVMTIndex: Integer;
    FVisibility: TdwsVisibility;
    FAttributes: TMethodAttributes;

  protected
    function GetIsClassMethod: Boolean;

    function GetIsOverride: Boolean; inline;
    procedure SetIsOverride(const val: Boolean); inline;
    function GetIsOverlap: Boolean; inline;
    procedure SetIsOverlap(const val: Boolean); inline;
    function GetIsVirtual: Boolean; inline;
    procedure SetIsVirtual(const val: Boolean);
    function GetIsAbstract: Boolean; inline;
    procedure SetIsAbstract(const val: Boolean); inline;
    function GetIsFinal: Boolean; inline;
    function GetIsInterfaced: Boolean; inline;
    procedure SetIsInterfaced(const val: Boolean); inline;
    function GetIsDefault: Boolean; inline;
    procedure SetIsDefault(const val: Boolean); inline;
    function GetIsStatic: Boolean; inline;
    function GetIgnoreMissingImplementation: Boolean;
    procedure SetIgnoreMissingImplementation(const val: Boolean);

    function GetCaption: String; override;
    function GetDescription: String; override;

    function GetRootParentMeth: TMethodSymbol;

  public
    constructor Create(const Name: String; funcKind: TFuncKind;
      aStructSymbol: TCompositeTypeSymbol; aVisibility: TdwsVisibility;
      aCreateOptions: TMethodCreateOptions; funcLevel: Integer = 1); virtual;

    constructor Generate(Table: TSymbolTable; MethKind: TMethodKind;
      const Attributes: TMethodAttributes; const MethName: String;
      const MethParams: TParamArray; const MethType: String;
      Cls: TCompositeTypeSymbol; aVisibility: TdwsVisibility;
      overloaded: Boolean);

    procedure SetOverride(meth: TMethodSymbol);
    procedure SetOverlap(meth: TMethodSymbol);
    procedure SetIsFinal;
    procedure SetIsStatic;
    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function QualifiedName: String; override;
    function ParamsDescription: String; override;
    function HasConditions: Boolean;
    function IsVisibleFor(const aVisibility: TdwsVisibility): Boolean; override;
    function IsSameOverloadOf(other: TFuncSymbol): Boolean; override;

    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; override;

    property StructSymbol: TCompositeTypeSymbol read FStructSymbol;
    property VMTIndex: Integer read FVMTIndex;

    property IsDefault: Boolean read GetIsDefault write SetIsDefault;
    property IsAbstract: Boolean read GetIsAbstract write SetIsAbstract;
    property IsVirtual: Boolean read GetIsVirtual write SetIsVirtual;
    property IsOverride: Boolean read GetIsOverride;
    property IsInterfaced: Boolean read GetIsInterfaced write SetIsInterfaced;
    property IsFinal: Boolean read GetIsFinal;
    property IsOverlap: Boolean read GetIsOverlap;
    property IsClassMethod: Boolean read GetIsClassMethod;
    property IsStatic: Boolean read GetIsStatic;
    property ParentMeth: TMethodSymbol read FParentMeth;
    property RootParentMeth: TMethodSymbol read GetRootParentMeth;
    property SelfSym: TDataSymbol read FSelfSym;
    property Visibility: TdwsVisibility read FVisibility;
    property IgnoreMissingImplementation: Boolean
      read GetIgnoreMissingImplementation write SetIgnoreMissingImplementation;
  end;

  TMethodSymbolClass = class of TMethodSymbol;

  TMethodSymbolArray = array of TMethodSymbol;

  TSourceMethodSymbol = class(TMethodSymbol)
  private
    FDeclarationPosition: TScriptPos;
    FImplementationPosition: TScriptPos;

  protected
    function GetDeclarationPosition: TScriptPos; override;
    procedure SetDeclarationPosition(const val: TScriptPos); override;
    function GetImplementationPosition: TScriptPos; override;
    procedure SetImplementationPosition(const val: TScriptPos); override;

  public
    property SubExpr;
    property SubExprCount;
  end;

  TAliasMethodSymbol = class sealed(TSourceMethodSymbol)
  private
    FAlias: TFuncSymbol;

  protected
    function GetDeclarationPosition: TScriptPos; override;
    function GetImplementationPosition: TScriptPos; override;

  public
    function IsPointerType: Boolean; override;

    function ParamsDescription: String; override;

    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; override;

    property Alias: TFuncSymbol read FAlias write FAlias;
  end;

  TOperatorSymbol = class sealed(TSymbol)
  private
    FToken: TTokenType;
    FParams: TTypeSymbols;
    FUsesSym: TFuncSymbol;
    FOperatorExprClass: TExprBaseClass;
    FAssignExprClass: TExprBaseClass;

  protected
    function GetCaption: String; override;

  public
    constructor Create(const aTokenType: TTokenType);

    procedure AddParam(p: TTypeSymbol);

    property Token: TTokenType read FToken write FToken;
    property params: TTypeSymbols read FParams;
    property UsesSym: TFuncSymbol read FUsesSym write FUsesSym;
    property OperatorExprClass: TExprBaseClass read FOperatorExprClass
      write FOperatorExprClass;
    property AssignExprClass: TExprBaseClass read FAssignExprClass
      write FAssignExprClass;
  end;

  // type x = TMyType;
  TAliasSymbol = class sealed(TTypeSymbol)
  protected
    function DoIsOfType(typSym: TTypeSymbol): Boolean; override;
    function GetUnAliasedType: TTypeSymbol; override;
    function GetAsFuncSymbol: TFuncSymbol; override;
    function GetDescription: String; override;
    function GetCaption: String; override;

  public
    function BaseType: TTypeSymbol; override;
    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function IsPointerType: Boolean; override;

    function Taxonomy: TdwsSymbolTaxonomy; override;
  end;

  // integer/String/float/boolean/variant
  TBaseSymbol = class(TTypeSymbol)
  public
    constructor Create(const Name: String);

    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    class function IsBaseType: Boolean; override; final;

    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; override;
  end;

  TBaseIntegerSymbol = class sealed(TBaseSymbol)
  public
    constructor Create;

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
  end;

  TBaseFloatSymbol = class sealed(TBaseSymbol)
  public
    constructor Create;

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
  end;

  TBaseStringSymbol = class sealed(TBaseSymbol)
  private
    FLengthPseudoSymbol: TPseudoMethodSymbol;
    FHighPseudoSymbol: TPseudoMethodSymbol;
    FLowPseudoSymbol: TPseudoMethodSymbol;

    function InitPseudoSymbol(var p: TPseudoMethodSymbol;
      sk: TSpecialKeywordKind; BaseSymbols: TdwsBaseSymbolsContext)
      : TPseudoMethodSymbol;

  public
    constructor Create;
    destructor Destroy; override;

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    procedure InitString(var s: String); override;

    function LengthPseudoSymbol(BaseSymbols: TdwsBaseSymbolsContext)
      : TPseudoMethodSymbol; inline;
    function HighPseudoSymbol(BaseSymbols: TdwsBaseSymbolsContext)
      : TPseudoMethodSymbol; inline;
    function LowPseudoSymbol(BaseSymbols: TdwsBaseSymbolsContext)
      : TPseudoMethodSymbol; inline;
  end;

  TBaseBooleanSymbol = class sealed(TBaseSymbol)
  public
    constructor Create;

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
  end;

  TBaseVariantSymbol = class(TBaseSymbol)
  public
    constructor Create(const Name: String = '');

    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function SupportsEmptyParam: Boolean; virtual;
  end;

  TTypeWithPseudoMethodsSymbol = class;

  TPseudoMethodSymbol = class sealed(TFuncSymbol)
  private
    FOwnerTyp: TTypeSymbol;

  public
    constructor Create(owner: TTypeSymbol; const Name: String;
      funcKind: TFuncKind; funcLevel: SmallInt);

    property OwnerTyp: TTypeSymbol read FOwnerTyp write FOwnerTyp;
  end;

  TTypeWithPseudoMethodsSymbol = class abstract(TTypeSymbol)
  private
    FPseudoMethods: array [TArrayMethodKind] of TPseudoMethodSymbol;

  protected
    class var vZeroDC: IDataContext;
    class function GetZeroDC: IDataContext; static;

    function InitializePseudoMethodSymbol(methodKind: TArrayMethodKind;
      BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol; virtual;

  public
    destructor Destroy; override;

    function PseudoMethodSymbol(methodKind: TArrayMethodKind;
      BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol;

  end;

  TEnumerationSymbol = class;
  TElementSymbol = class;

  TSetOfSymbol = class sealed(TTypeWithPseudoMethodsSymbol)
  private
    FMinValue: Integer;
    FCountValue: Integer;

  protected
    function GetMaxValue: Integer; inline;

    function InitializePseudoMethodSymbol(methodKind: TArrayMethodKind;
      BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol; override;

  public
    constructor Create(const Name: String; indexType: TTypeSymbol;
      aMin, aMax: Integer);

    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function SameType(typSym: TTypeSymbol): Boolean; override;

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;

    function AssignsAsDataExpr: Boolean; override;

    function ValueToOffsetMask(value: Integer; var mask: Int64)
      : Integer; inline;
    function ValueToByteOffsetMask(value: Integer; var mask: Byte)
      : Integer; inline;

    function GetValueDescription(const value: IDataContext): String;

    function ElementByValue(value: Integer): TElementSymbol;

    property MinValue: Integer read FMinValue write FMinValue;
    property MaxValue: Integer read GetMaxValue;
    property CountValue: Integer read FCountValue write FCountValue;
  end;

  TArraySymbol = class abstract(TTypeWithPseudoMethodsSymbol)
  private
    FIndexType: TTypeSymbol;
    FSortFunctionType: TFuncSymbol;
    FMapFunctionType: TFuncSymbol;
    FFilterFunctionType: TFuncSymbol;
    FForEachFunctionType: TFuncSymbol;

  protected
    function ElementSize: Integer;

    function InitializePseudoMethodSymbol(methodKind: TArrayMethodKind;
      BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol; override;

  public
    constructor Create(const Name: String; elementType, indexType: TTypeSymbol);
    destructor Destroy; override;

    function DynamicInitialization: Boolean; override;

    function AssignsAsDataExpr: Boolean; override;

    function SortFunctionType(BaseSymbols: TdwsBaseSymbolsContext)
      : TFuncSymbol; virtual;
    function MapFunctionType(BaseSymbols: TdwsBaseSymbolsContext)
      : TFuncSymbol; virtual;
    function FilterFunctionType(BaseSymbols: TdwsBaseSymbolsContext)
      : TFuncSymbol; virtual;
    function ForEachFunctionType(BaseSymbols: TdwsBaseSymbolsContext)
      : TFuncSymbol; virtual;

    property indexType: TTypeSymbol read FIndexType write FIndexType;
  end;

  TInitDataProc = procedure(typ: TTypeSymbol; const resultDC: IDataContext;
    offset: NativeInt);

  // array of FTyp
  TDynamicArraySymbol = class sealed(TArraySymbol)
  protected
    function GetCaption: String; override;
    function GetDescription: String; override;
    function DoIsOfType(typSym: TTypeSymbol): Boolean; override;

  protected
    function InitializePseudoMethodSymbol(methodKind: TArrayMethodKind;
      BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol; override;

  public
    constructor Create(const Name: String; elementType, indexType: TTypeSymbol);
    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function IsPointerType: Boolean; override;
    function SameType(typSym: TTypeSymbol): Boolean; override;
    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; override;

    class procedure SetInitDynamicArrayProc(const aProc: TInitDataProc);
  end;

  // array [FLowBound..FHighBound] of FTyp
  TStaticArraySymbol = class(TArraySymbol)
  private
    FHighBound: Integer;
    FLowBound: Integer;
    FElementCount: Integer;

  protected
    function GetCaption: String; override;
    function DoIsOfType(typSym: TTypeSymbol): Boolean; override;

  public
    constructor Create(const Name: String; elementType, indexType: TTypeSymbol;
      lowBound, highBound: Integer);

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function SameType(typSym: TTypeSymbol): Boolean; override;
    procedure AddElement;
    function IsEmptyArray: Boolean;

    property highBound: Integer read FHighBound;
    property lowBound: Integer read FLowBound;
    property ElementCount: Integer read FElementCount;
  end;

  // static array whose bounds are contextual
  TOpenArraySymbol = class sealed(TStaticArraySymbol)
  protected
    function GetCaption: String; override;

  public
    constructor Create(const Name: String; elementType, indexType: TTypeSymbol);
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
  end;

  // associative array aka dictionary
  TAssociativeArraySymbol = class sealed(TTypeWithPseudoMethodsSymbol)
  private
    FKeyType: TTypeSymbol;
    FKeyArrayType: TDynamicArraySymbol;

  protected
    function GetCaption: String; override;
    function DoIsOfType(typSym: TTypeSymbol): Boolean; override;

    function InitializePseudoMethodSymbol(methodKind: TArrayMethodKind;
      BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol; override;

  public
    constructor Create(const Name: String; elementType, keyType: TTypeSymbol);
    destructor Destroy; override;

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function DynamicInitialization: Boolean; override;

    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function IsPointerType: Boolean; override;
    function SameType(typSym: TTypeSymbol): Boolean; override;

    function KeysArrayType(BaseSymbols: TdwsBaseSymbolsContext)
      : TDynamicArraySymbol; virtual;

    function KeyAndElementSizeAreBaseTypesOfSizeOne: Boolean; inline;

    property keyType: TTypeSymbol read FKeyType;

    class procedure SetInitAssociativeArrayProc(const aProc: TInitDataProc);
  end;

  // TMembersSymbolTable
  //
  TMembersSymbolTable = class(TSymbolTable)
  private
    FOwner: TCompositeTypeSymbol;

  public
    procedure AddParent(parent: TMembersSymbolTable);

    function FindSymbol(const aName: String; minVisibility: TdwsVisibility;
      ofClass: TSymbolClass = nil): TSymbol; override;
    function FindSymbolFromScope(const aName: String;
      scopeSym: TCompositeTypeSymbol): TSymbol; reintroduce;

    function VisibilityFromScope(scopeSym: TCompositeTypeSymbol)
      : TdwsVisibility;
    function Visibilities: TdwsVisibilities;

    property owner: TCompositeTypeSymbol read FOwner write FOwner;
  end;

  TStructuredTypeMetaSymbol = class;

  // Const attached to a class
  TClassConstSymbol = class sealed(TConstSymbol)
  protected
    FOwnerSymbol: TCompositeTypeSymbol;
    FVisibility: TdwsVisibility;

  public
    function QualifiedName: String; override;
    function IsVisibleFor(const aVisibility: TdwsVisibility): Boolean; override;

    property OwnerSymbol: TCompositeTypeSymbol read FOwnerSymbol
      write FOwnerSymbol;
    property Visibility: TdwsVisibility read FVisibility write FVisibility;
  end;

  // Var attached to a class
  TClassVarSymbol = class sealed(TDataSymbol)
  protected
    FOwnerSymbol: TCompositeTypeSymbol;
    FVisibility: TdwsVisibility;

  public
    constructor Create(const aName: String; aType: TTypeSymbol;
      aVisibility: TdwsVisibility);

    function QualifiedName: String; override;
    function IsVisibleFor(const aVisibility: TdwsVisibility): Boolean; override;

    property OwnerSymbol: TCompositeTypeSymbol read FOwnerSymbol
      write FOwnerSymbol;
    property Visibility: TdwsVisibility read FVisibility write FVisibility;
  end;

  // type symbol with members
  TCompositeTypeSymbol = class(TTypeSymbol)
  private
    FUnitSymbol: TSymbol;
    FParent: TCompositeTypeSymbol;
    FMembers: TMembersSymbolTable;
    FDefaultProperty: TPropertySymbol;
    FFirstField: TFieldSymbol;

  protected
    function CreateMembersTable(addrGenerator: TAddrGenerator)
      : TMembersSymbolTable; virtual;

    function GetIsStatic: Boolean; virtual;
    function GetIsExternal: Boolean; virtual;
    function GetIsExternalRooted: Boolean; virtual;
    function GetIsPartial: Boolean; virtual;
    function GetIsImmutable: Boolean; virtual;

    procedure CheckMethodsImplemented(const Msgs: TdwsCompileMessageList);

    function PrepareFirstField: TFieldSymbol;

    function GetMetaSymbol: TStructuredTypeMetaSymbol; virtual; abstract;

    function GetIsGeneric: Boolean; override;

    procedure SpecializeMembers(destination: TCompositeTypeSymbol;
      const context: ISpecializationContext);

  public
    constructor Create(const Name: String; aUnit: TSymbol);
    destructor Destroy; override;

    procedure AddConst(sym: TClassConstSymbol); overload;
    procedure AddConst(sym: TClassConstSymbol;
      Visibility: TdwsVisibility); overload;
    procedure AddClassVar(sym: TClassVarSymbol);
    procedure AddProperty(propSym: TPropertySymbol);
    procedure AddMethod(methSym: TMethodSymbol); virtual;
    procedure AddField(fieldSym: TFieldSymbol); virtual;

    function FieldAtOffset(offset: Integer): TFieldSymbol; virtual;

    function AllowVirtualMembers: Boolean; virtual;
    function AllowOverloads: Boolean; virtual;
    function AllowDefaultProperty: Boolean; virtual; abstract;
    function AllowFields: Boolean; virtual;
    function AllowAnonymousMethods: Boolean; virtual;

    function FindDefaultConstructor(minVisibility: TdwsVisibility)
      : TMethodSymbol; virtual;

    function MembersVisibilities: TdwsVisibilities;

    function HasNestedtypes: Boolean; inline;

    function CreateSelfParameter(methSym: TMethodSymbol): TDataSymbol;
      virtual; abstract;
    function CreateAnonymousMethod(aFuncKind: TFuncKind;
      aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
      : TMethodSymbol; virtual; abstract;

    function FirstField: TFieldSymbol; inline;

    function ExternalRoot: TCompositeTypeSymbol;

    property UnitSymbol: TSymbol read FUnitSymbol;
    property parent: TCompositeTypeSymbol read FParent;
    property Members: TMembersSymbolTable read FMembers;
    property DefaultProperty: TPropertySymbol read FDefaultProperty
      write FDefaultProperty;
    property MetaSymbol: TStructuredTypeMetaSymbol read GetMetaSymbol;

    property IsStatic: Boolean read GetIsStatic;
    property IsPartial: Boolean read GetIsPartial;
    property IsExternal: Boolean read GetIsExternal;
    property IsExternalRooted: Boolean read GetIsExternalRooted;
    property IsImmutable: Boolean read GetIsImmutable;
  end;

  // class, record, interface
  TStructuredTypeSymbol = class(TCompositeTypeSymbol)
  private
    FMetaSymbol: TStructuredTypeMetaSymbol;
    FForwardPosition: PScriptPos;

  protected
    function GetIsExternal: Boolean; override;

    function GetMetaSymbol: TStructuredTypeMetaSymbol; override;

    procedure DoInheritFrom(ancestor: TStructuredTypeSymbol);

  public
    destructor Destroy; override;

    function DuckTypedMatchingMethod(methSym: TMethodSymbol;
      Visibility: TdwsVisibility): TMethodSymbol; virtual;

    function NthParentOf(structType: TCompositeTypeSymbol): Integer;
    function DistanceTo(typeSym: TTypeSymbol): Integer; override;
    function FindDefaultConstructor(minVisibility: TdwsVisibility)
      : TMethodSymbol; override;
    function AllowDefaultProperty: Boolean; override;

    procedure SetForwardedPos(const aScriptPos: TScriptPos);
    procedure ClearIsForwarded;

    function IsForwarded: Boolean; override;

    property MetaSymbol: TStructuredTypeMetaSymbol read FMetaSymbol;
  end;

  // class of, record of
  TStructuredTypeMetaSymbol = class(TTypeSymbol)
  public
    constructor Create(const Name: String; typ: TStructuredTypeSymbol);

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;

    function StructSymbol: TStructuredTypeSymbol; inline;

    function parent: TStructuredTypeMetaSymbol;
  end;

  // field of a script object
  TFieldSymbol = class sealed(TValueSymbol)
  protected
    FStructSymbol: TCompositeTypeSymbol;
    FOffset: Integer;
    FVisibility: TdwsVisibility;
    FDefaultValue: IDataContext;
    FDefaultExpr: TExprBase;
    FNextField: TFieldSymbol;
    FReadOnly: Boolean;

  public
    constructor Create(const Name: String; typ: TTypeSymbol;
      aVisibility: TdwsVisibility);
    destructor Destroy; override;

    function QualifiedName: String; override;
    function IsVisibleFor(const aVisibility: TdwsVisibility): Boolean; override;

    procedure InitDataContext(const dc: IDataContext; structOffset: Integer);

    property StructSymbol: TCompositeTypeSymbol read FStructSymbol;
    property offset: Integer read FOffset;
    property Visibility: TdwsVisibility read FVisibility write FVisibility;
    property DefaultValue: IDataContext read FDefaultValue write FDefaultValue;
    property DefaultExpr: TExprBase read FDefaultExpr write FDefaultExpr;
    property NextField: TFieldSymbol read FNextField write FNextField;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;

  TRecordSymbolFlag = (rsfDynamic,
    // indicates some fields have non-constant initialization expressions (for anonymous records)
    rsfFullyDefined, // set when the declaration is complete
    rsfImmutable, // immutable record, cannot be altered at runtime
    rsfExternal, // external record
    rsfDynamicInitialization
    // contains fields that require dynamic initialization (dynamic arrays...)
    );
  TRecordSymbolFlags = set of TRecordSymbolFlag;

  // record member1: Integer; member2: Integer end;
  TRecordSymbol = class sealed(TStructuredTypeSymbol)
  private
    FFlags: TRecordSymbolFlags;

  protected
    function GetCaption: String; override;
    function GetDescription: String; override;

    function GetIsDynamic: Boolean; inline;
    procedure SetIsDynamic(const val: Boolean);
    function GetIsImmutable: Boolean; override;
    procedure SetIsImmutable(const val: Boolean);
    function GetIsFullyDefined: Boolean; inline;
    procedure SetIsFullyDefined(const val: Boolean);
    function GetIsExternal: Boolean; override;

  public
    constructor Create(const Name: String; aUnit: TSymbol);

    procedure AddField(fieldSym: TFieldSymbol); override;
    procedure AddMethod(methSym: TMethodSymbol); override;
    procedure Initialize(const Msgs: TdwsCompileMessageList); override;

    function AllowFields: Boolean; override;

    function CreateSelfParameter(methSym: TMethodSymbol): TDataSymbol; override;
    function CreateAnonymousMethod(aFuncKind: TFuncKind;
      aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
      : TMethodSymbol; override;

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function DynamicInitialization: Boolean; override;
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function AssignsAsDataExpr: Boolean; override;

    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; override;

    property IsDynamic: Boolean read GetIsDynamic write SetIsDynamic;
    property IsImmutable: Boolean read GetIsImmutable write SetIsImmutable;
    property IsFullyDefined: Boolean read GetIsFullyDefined
      write SetIsFullyDefined;

    procedure SetIsExternal;
  end;

  // interface
  TInterfaceSymbol = class sealed(TStructuredTypeSymbol)
  private
    FMethodCount: Integer;

  protected
    function GetCaption: String; override;
    function GetDescription: String; override;
    function DoIsOfType(typSym: TTypeSymbol): Boolean; override;

  public
    constructor Create(const Name: String; aUnit: TSymbol);

    procedure InheritFrom(ancestor: TInterfaceSymbol);

    procedure AddMethod(methSym: TMethodSymbol); override;

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    procedure Initialize(const Msgs: TdwsCompileMessageList); override;
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function IsPointerType: Boolean; override;

    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; override;

    function CreateSelfParameter(methSym: TMethodSymbol): TDataSymbol; override;
    function CreateAnonymousMethod(aFuncKind: TFuncKind;
      aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
      : TMethodSymbol; override;

    function parent: TInterfaceSymbol; inline;
    property MethodCount: Integer read FMethodCount;
  end;

  // property X: Integer read FReadSym write FWriteSym;
  TPropertySymbol = class sealed(TValueSymbol)
  private
    FOwnerSymbol: TCompositeTypeSymbol;
    FReadSym: TSymbol;
    FWriteSym: TSymbol;
    FArrayIndices: TParamsSymbolTable;
    FIndexSym: TTypeSymbol;
    FIndexValue: IDataContext;
    FDefaultSym: TConstSymbol;
    FVisibility: TdwsVisibility;
    FDeprecatedMessage: String;
    FUserDescription: String;
    FIsReintroduce: Boolean;

  protected
    function GetCaption: String; override;
    function GetDescription: String; override;
    function GetIsDefault: Boolean;
    function GetArrayIndices: TParamsSymbolTable;
    procedure AddParam(param: TParamSymbol);
    function GetIsDeprecated: Boolean; inline;

  public
    constructor Create(const Name: String; typ: TTypeSymbol;
      aVisibility: TdwsVisibility; aArrayIndices: TParamsSymbolTable);
    destructor Destroy; override;

    procedure GenerateParams(Table: TSymbolTable;
      const funcParams: TParamArray);
    procedure SetIndex(const data: IDataContext; sym: TTypeSymbol);
    function GetArrayIndicesDescription: String;
    function QualifiedName: String; override;
    function IsVisibleFor(const aVisibility: TdwsVisibility): Boolean; override;
    function HasArrayIndices: Boolean;

    function Specialize(const context: ISpecializationContext)
      : TSymbol; override;

    property OwnerSymbol: TCompositeTypeSymbol read FOwnerSymbol;
    property Visibility: TdwsVisibility read FVisibility write FVisibility;
    property ArrayIndices: TParamsSymbolTable read GetArrayIndices;
    property ReadSym: TSymbol read FReadSym write FReadSym;
    property WriteSym: TSymbol read FWriteSym write FWriteSym;
    property IsDefault: Boolean read GetIsDefault;
    property IndexValue: IDataContext read FIndexValue;
    property IndexSym: TTypeSymbol read FIndexSym;
    property DefaultSym: TConstSymbol read FDefaultSym write FDefaultSym;
    property DeprecatedMessage: String read FDeprecatedMessage
      write FDeprecatedMessage;
    property IsDeprecated: Boolean read GetIsDeprecated;
    property UserDescription: String read FUserDescription
      write FUserDescription;
    property IsReintroduce: Boolean read FIsReintroduce write FIsReintroduce;
  end;

  // class operator X (params) uses method;
  TClassOperatorSymbol = class sealed(TSymbol)
  private
    FCompositeSymbol: TCompositeTypeSymbol;
    FTokenType: TTokenType;
    FUsesSym: TMethodSymbol;

  protected
    function GetCaption: String; override;
    function GetDescription: String; override;

  public
    constructor Create(tokenType: TTokenType);
    function QualifiedName: String; override;

    property CompositeSymbol: TCompositeTypeSymbol read FCompositeSymbol
      write FCompositeSymbol;
    property tokenType: TTokenType read FTokenType write FTokenType;
    property UsesSym: TMethodSymbol read FUsesSym write FUsesSym;
  end;

  // type X = class of TMyClass;
  TClassOfSymbol = class sealed(TStructuredTypeMetaSymbol)
  protected
    function GetCaption: String; override;
    function GetDescription: String; override;
    function DoIsOfType(typSym: TTypeSymbol): Boolean; override;

  public
    constructor Create(const Name: String; typ: TClassSymbol);

    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function SameType(typSym: TTypeSymbol): Boolean; override;
    function TypClassSymbol: TClassSymbol; inline;
  end;

  // A resolved interface (attached to a class symbol)
  TResolvedInterface = record
    IntfSymbol: TInterfaceSymbol;
    VMT: TMethodSymbolArray;
  end;

  TResolvedInterfaces = class(TSimpleHash<TResolvedInterface>)
  protected
    function SameItem(const item1, item2: TResolvedInterface): Boolean;
      override;
    function GetItemHashCode(const item1: TResolvedInterface)
      : Cardinal; override;
  end;

  TObjectDestroyEvent = procedure(ExternalObject: TObject) of object;

  TClassSymbolFlag = (
    // requirement or declarations flags
    csfExplicitAbstract, // class that was explicity marked as abstract
    csfSealed, // class that cannot be sublclasses
    csfStatic, // class that cannot have instances
    csfExternal, // class exposed but not implemented in script
    csfPartial,
    // class whose declaration and implementation spans multiple units
    csfInternal,
    // class for internal use which cannot be subclassed or constructed from script
    // script engine flags
    csfAbstract, // class was marked abstract or has abstract methods
    csfNoVirtualMembers, // class does not have virtual members
    csfNoOverloads, // class does not allow overloads
    csfHasOwnMethods, csfHasOwnFields, csfExternalRooted, csfInitialized,
    csfAttribute);
  TClassSymbolFlags = set of TClassSymbolFlag;

  // type X = class ... end;
  TClassSymbol = class sealed(TStructuredTypeSymbol)
  private
    FFlags: TClassSymbolFlags;
    FOperators: TTightList;
    FScriptInstanceSize: Integer;
    FOnObjectDestroy: TObjectDestroyEvent;
    FVirtualMethodTable: TMethodSymbolArray;
    FInterfaces: TResolvedInterfaces;

  protected
    function GetDescription: String; override;
    function GetIsExplicitAbstract: Boolean; inline;
    procedure SetIsExplicitAbstract(const val: Boolean); inline;
    function GetIsAbstract: Boolean; inline;
    function GetIsSealed: Boolean; inline;
    procedure SetIsSealed(const val: Boolean); inline;
    function GetIsStatic: Boolean; override;
    procedure SetIsStatic(const val: Boolean); inline;
    function GetIsExternal: Boolean; override;
    procedure SetIsExternal(const val: Boolean); inline;
    function GetIsExternalRooted: Boolean; override;
    function GetIsPartial: Boolean; override;
    function GetIsAttribute: Boolean; inline;
    procedure SetIsAttribute(const val: Boolean); inline;
    function GetIsInternal: Boolean; inline;
    procedure SetIsInternal(const val: Boolean); inline;

    function GetIsClassSymbol: Boolean; override; final;

    function DoIsOfType(typSym: TTypeSymbol): Boolean; override;

    function ProcessOverriddenInterfaceCallback(const item: TResolvedInterface)
      : TSimpleHashAction;
    procedure ProcessOverriddenInterfaces;
    function ProcessOverriddenInterface(const ancestorResolved
      : TResolvedInterface): Boolean; // True if added

  public
    constructor Create(const Name: String; aUnit: TSymbol);
    destructor Destroy; override;

    procedure AddField(fieldSym: TFieldSymbol); override;
    procedure AddMethod(methSym: TMethodSymbol); override;
    procedure AddOperator(sym: TClassOperatorSymbol);

    function AddInterface(intfSym: TInterfaceSymbol; Visibility: TdwsVisibility;
      var missingMethod: TMethodSymbol): Boolean; // True if added
    function ResolveInterface(intfSym: TInterfaceSymbol;
      var resolved: TResolvedInterface): Boolean;
    function ImplementsInterface(intfSym: TInterfaceSymbol): Boolean;
    procedure SetIsPartial; inline;
    procedure SetNoVirtualMembers; inline;
    procedure SetNoOverloads; inline;

    function FieldAtOffset(offset: Integer): TFieldSymbol; override;
    procedure InheritFrom(ancestorClassSym: TClassSymbol);
    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    procedure Initialize(const Msgs: TdwsCompileMessageList); override;
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function IsPointerType: Boolean; override;
    function HasMetaSymbol: Boolean; override;
    function Taxonomy: TdwsSymbolTaxonomy; override;

    function CommonAncestor(otherClass: TTypeSymbol): TClassSymbol;

    function VMTMethod(Index: Integer): TMethodSymbol;
    function VMTCount: Integer;

    function FindClassOperatorStrict(tokenType: TTokenType; ParamType: TSymbol;
      recursive: Boolean): TClassOperatorSymbol;
    function FindClassOperator(tokenType: TTokenType; ParamType: TTypeSymbol)
      : TClassOperatorSymbol;

    function FindDefaultConstructor(minVisibility: TdwsVisibility)
      : TMethodSymbol; override;

    procedure CollectPublishedSymbols(symbolList: TSimpleSymbolList);

    function AllowVirtualMembers: Boolean; override;
    function AllowOverloads: Boolean; override;
    function AllowFields: Boolean; override;
    function AllowAnonymousMethods: Boolean; override;

    function CreateSelfParameter(methSym: TMethodSymbol): TDataSymbol; override;
    function CreateAnonymousMethod(aFuncKind: TFuncKind;
      aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
      : TMethodSymbol; override;

    class function VisibilityToString(Visibility: TdwsVisibility)
      : String; static;

    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; override;

    function parent: TClassSymbol; inline;
    property ScriptInstanceSize: Integer read FScriptInstanceSize;
    property Interfaces: TResolvedInterfaces read FInterfaces;
    property Flags: TClassSymbolFlags read FFlags;

    property IsExplicitAbstract: Boolean read GetIsExplicitAbstract
      write SetIsExplicitAbstract;
    property IsAbstract: Boolean read GetIsAbstract;
    property IsSealed: Boolean read GetIsSealed write SetIsSealed;
    property IsStatic: Boolean read GetIsStatic write SetIsStatic;
    property IsExternal: Boolean read GetIsExternal write SetIsExternal;
    property IsInternal: Boolean read GetIsInternal write SetIsInternal;
    property IsPartial: Boolean read GetIsPartial;
    property IsAttribute: Boolean read GetIsAttribute write SetIsAttribute;

    function IsPureStatic: Boolean;

    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy
      write FOnObjectDestroy;
  end;

  // class or type helper
  THelperSymbol = class sealed(TCompositeTypeSymbol)
  private
    FForType: TTypeSymbol;
    FUnAliasedForType: TTypeSymbol;
    FMetaForType: TStructuredTypeMetaSymbol;
    FPriority: Integer;
    FStrict: Boolean;

  protected
    function GetMetaSymbol: TStructuredTypeMetaSymbol; override;

  public
    constructor Create(const Name: String; aUnit: TSymbol;
      aForType: TTypeSymbol; priority: Integer);

    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function IsType: Boolean; override;
    function AllowDefaultProperty: Boolean; override;
    function CreateSelfParameter(methSym: TMethodSymbol): TDataSymbol; override;
    function CreateAnonymousMethod(aFuncKind: TFuncKind;
      aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
      : TMethodSymbol; override;

    procedure Initialize(const Msgs: TdwsCompileMessageList); override;

    function HelpsType(typ: TTypeSymbol): Boolean;

    property ForType: TTypeSymbol read FForType;
    property priority: Integer read FPriority;
    property Strict: Boolean read FStrict write FStrict;
  end;

  THelperSymbols = class(TSimpleList<THelperSymbol>)
  public
    function AddHelper(helper: THelperSymbol): Boolean;
  end;

  // nil "class"
  TNilSymbol = class sealed(TTypeSymbol)
  protected
    function GetCaption: String; override;

  public
    constructor Create;

    function IsCompatible(typSym: TTypeSymbol): Boolean; override;
    function IsCompatibleWithAnyFuncSymbol: Boolean; override;

    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;

    function SpecializeType(const context: ISpecializationContext)
      : TTypeSymbol; override;
  end;

  // Element of an enumeration type. E. g. "type DummyEnum = (Elem1, Elem2, Elem3);"
  TElementSymbol = class sealed(TConstSymbol)
  private
    FEnumeration: TEnumerationSymbol;
    FIsUserDef: Boolean;

  protected
    function GetDescription: String; override;
    function GetValue: Int64; inline;

  public
    constructor Create(const Name: String; typ: TTypeSymbol;
      const aValue: Int64; isUserDef: Boolean);

    function StandardName: String; inline;
    function QualifiedName: String; override;

    property Enumeration: TEnumerationSymbol read FEnumeration;
    property isUserDef: Boolean read FIsUserDef;
    property value: Int64 read GetValue;
  end;

  TEnumerationSymbolStyle = (enumClassic, enumScoped, enumFlags);

  // Enumeration type. E. g. "type myEnum = (One, Two, Three);"
  TEnumerationSymbol = class sealed(TTypeWithPseudoMethodsSymbol)
  private
    FElements: TSymbolTable;
    FLowBound, FHighBound: Int64;
    FStyle: TEnumerationSymbolStyle;
    FContinuous: Boolean;

  protected
    function GetCaption: String; override;
    function GetDescription: String; override;
    function DoIsOfType(typSym: TTypeSymbol): Boolean; override;

    function InitializePseudoMethodSymbol(methodKind: TArrayMethodKind;
      BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol; override;

  public
    constructor Create(const Name: String; BaseType: TTypeSymbol;
      aStyle: TEnumerationSymbolStyle);
    destructor Destroy; override;

    function DefaultValue: Int64;
    procedure InitDataContext(const data: IDataContext;
      offset: NativeInt); override;
    function BaseType: TTypeSymbol; override;
    function IsCompatible(typSym: TTypeSymbol): Boolean; override;

    procedure AddElement(element: TElementSymbol);
    function ElementByValue(const value: Int64): TElementSymbol;
    function ElementByName(const Name: String): TElementSymbol;

    property Elements: TSymbolTable read FElements;
    property Style: TEnumerationSymbolStyle read FStyle;
    property Continuous: Boolean read FContinuous write FContinuous;
    property lowBound: Int64 read FLowBound write FLowBound;
    property highBound: Int64 read FHighBound write FHighBound;
    function ShortDescription: String;
  end;

  // variable with functions for read/write: var x: integer; extern 'type' in 'selector';
  TExternalVarSymbol = class sealed(TValueSymbol)
  private
    FReadFunc: TFuncSymbol;
    FWriteFunc: TFuncSymbol;

  protected
    function GetReadFunc: TFuncSymbol; virtual;
    function GetWriteFunc: TFuncSymbol; virtual;

  public
    destructor Destroy; override;

    property ReadFunc: TFuncSymbol read GetReadFunc write FReadFunc;
    property WriteFunc: TFuncSymbol read GetWriteFunc write FWriteFunc;
  end;

  TdwsBaseSymbolTypes = record
    TypBoolean: TBaseBooleanSymbol;
    TypFloat: TBaseFloatSymbol;
    TypInteger: TBaseIntegerSymbol;
    TypString: TBaseStringSymbol;
    TypVariant: TBaseVariantSymbol;

    TypNil: TNilSymbol;
    TypObject: TClassSymbol;
    TypTObject: TClassSymbol;
    TypClass: TClassOfSymbol;

    TypException: TClassSymbol;
    TypInterface: TInterfaceSymbol;
    TypCustomAttribute: TClassSymbol;
    TypAnyType: TAnyTypeSymbol;
    TypAnyFunc: TAnyFuncSymbol;
  end;

  TdwsBaseSymbolsContext = class
  private
    FBaseTypes: TdwsBaseSymbolTypes;

  protected
    procedure SetBaseTypes(const bt: TdwsBaseSymbolTypes);

  public
    function FindType(const typName: String): TTypeSymbol; virtual; abstract;

    property TypBoolean: TBaseBooleanSymbol read FBaseTypes.TypBoolean;
    property TypFloat: TBaseFloatSymbol read FBaseTypes.TypFloat;
    property TypInteger: TBaseIntegerSymbol read FBaseTypes.TypInteger;
    property TypNil: TNilSymbol read FBaseTypes.TypNil;
    property TypObject: TClassSymbol read FBaseTypes.TypObject;
    property TypTObject: TClassSymbol read FBaseTypes.TypTObject;
    property TypString: TBaseStringSymbol read FBaseTypes.TypString;
    property TypVariant: TBaseVariantSymbol read FBaseTypes.TypVariant;
    property TypException: TClassSymbol read FBaseTypes.TypException;
    property TypInterface: TInterfaceSymbol read FBaseTypes.TypInterface;
    property TypAnyType: TAnyTypeSymbol read FBaseTypes.TypAnyType;
    property TypAnyFunc: TAnyFuncSymbol read FBaseTypes.TypAnyFunc;
  end;

  // TdwsExecution
  //
  TdwsExecution = class abstract(TInterfacedSelfObject, IdwsExecution)
  protected
    FStack: TStackMixIn;
    FStatus: TExecutionStatusResult;
    FCallStack: TTightStack; // expr + prog duples
    FSelfScriptObject: PIScriptObj;
    FSelfScriptClassSymbol: TClassSymbol;

    FDebugger: IDebugger;
    FIsDebugging: Boolean;
    FDebugSuspended: Integer;

    FSleepTime: Integer;
    FSleeping: Boolean;

    FInternalExecution: Integer;

    FEnvironment: IdwsEnvironment;

  protected
    FProgramState: TProgramState; // here to reduce its offset

    function GetEnvironment: IdwsEnvironment;
    procedure SetEnvironment(const val: IdwsEnvironment);

  private
    FExternalObject: TObject;
    FUserObject: TObject;

    FExceptionObjectStack: TSimpleStack<IScriptObj>;
    FLastScriptError: TExprBase;
    FLastScriptCallStack: TdwsExprLocationArray;

    FRandSeed: UInt64;

    FFormatSettings: TdwsFormatSettings;

  protected
    function GetDebugger: IDebugger;
    procedure SetDebugger(const aDebugger: IDebugger);
    procedure StartDebug;
    procedure StopDebug;

    function GetMsgs: TdwsRuntimeMessageList; virtual; abstract;

    function GetExecutionObject: TdwsExecution;

    function GetUserObject: TObject; virtual;
    procedure SetUserObject(const value: TObject); virtual;
    procedure SetRandSeed(const val: UInt64);

    function GetStack: TStack;

    function GetProgramState: TProgramState;

    function GetSleeping: Boolean;

    function GetFormatSettings: TdwsFormatSettings;

  public
    constructor Create(const stackParams: TStackParameters);
    destructor Destroy; override;

    procedure DoStep(Expr: TExprBase);

    property Status: TExecutionStatusResult read FStatus write FStatus;
    property Stack: TStackMixIn read FStack;
    property SelfScriptObject: PIScriptObj read FSelfScriptObject
      write FSelfScriptObject;
    property SelfScriptClassSymbol: TClassSymbol read FSelfScriptClassSymbol
      write FSelfScriptClassSymbol;

    class function Status_Offset: Integer;
    class function StackMixin_Offset: Integer;

    function GetLastScriptErrorExpr: TExprBase;
    procedure SetScriptError(Expr: TExprBase);
    procedure ClearScriptError;

    function GetCallStack: TdwsExprLocationArray; virtual; abstract;
    function CallStackLastExpr: TExprBase; virtual; abstract;
    function CallStackLastProg: TObject; virtual; abstract;
    function CallStackDepth: Integer; virtual; abstract;

    procedure SuspendDebug;
    procedure ResumeDebug;

    procedure DataContext_Create(const data: TData; addr: Integer;
      var result: IDataContext); inline;
    procedure DataContext_CreateEmpty(size: Integer;
      var result: IDataContext); inline;
    procedure DataContext_CreateValue(const value: Variant;
      var result: IDataContext); inline;
    procedure DataContext_CreateBase(addr: Integer;
      var result: IDataContext); inline;
    procedure DataContext_CreateStack(addr: Integer;
      var result: IDataContext); inline;
    procedure DataContext_CreateLevel(Level, addr: Integer;
      var result: IDataContext); inline;
    procedure DataContext_CreateOffset(const data: IDataContext;
      offset: Integer; var result: IDataContext); inline;
    function DataContext_Nil: IDataContext; inline;

    function GetStackPData: PData;

    procedure LocalizeSymbol(aResSymbol: TResourceStringSymbol;
      var result: String); virtual;
    procedure LocalizeString(const aString: String;
      var result: String); virtual;

    function ValidateFileName(const path: String): String; virtual;
    function FileSystem: IdwsFileSystemRW; virtual;

    function Random: Double;

    // interruptible sleep (in case program is stopped)
    procedure Sleep(msec, sleepCycle: Integer);
    property Sleeping: Boolean read FSleeping;

    property LastScriptError: TExprBase read FLastScriptError;
    property LastScriptCallStack: TdwsExprLocationArray
      read FLastScriptCallStack;
    property ExceptionObjectStack: TSimpleStack<IScriptObj>
      read FExceptionObjectStack;

    procedure EnterExceptionBlock(var exceptObj: IScriptObj); virtual;
    procedure LeaveExceptionBlock;

    property ProgramState: TProgramState read FProgramState;

    property Debugger: IDebugger read FDebugger write SetDebugger;
    property IsDebugging: Boolean read FIsDebugging;

    procedure DebuggerNotifyException(const exceptObj: IScriptObj);
      virtual; abstract;

    procedure BeginInternalExecution; inline;
    procedure EndInternalExecution; inline;
    function InternalExecution: Boolean; inline;

    property Msgs: TdwsRuntimeMessageList read GetMsgs;

    // per-execution randseed
    property RandSeed: UInt64 read FRandSeed write SetRandSeed;

    property FormatSettings: TdwsFormatSettings read GetFormatSettings;

    // specifies an external object for IInfo constructors, temporary
    property ExternalObject: TObject read FExternalObject write FExternalObject;

    // user object, to attach to an execution
    property UserObject: TObject read GetUserObject write SetUserObject;

    // user environment
    property Environment: IdwsEnvironment read FEnvironment write FEnvironment;
  end;

  // IScriptObj
  //
  IScriptObj = interface(IDataContext)
    ['{8D534D1E-4C6B-11D5-8DCB-0000216D9E86}']
    function GetClassSym: TClassSymbol;
    function GetExternalObject: TObject;
    procedure SetExternalObject(value: TObject);
    function GetDestroyed: Boolean;
    procedure SetDestroyed(const val: Boolean);

    property ClassSym: TClassSymbol read GetClassSym;
    property ExternalObject: TObject read GetExternalObject
      write SetExternalObject;
    property Destroyed: Boolean read GetDestroyed write SetDestroyed;

    function FieldAsString(const fieldName: String): String;
    function FieldAsInteger(const fieldName: String): Int64;
    function FieldAsFloat(const fieldName: String): Double;
    function FieldAsBoolean(const fieldName: String): Boolean;
    function FieldAsScriptDynArray(const fieldName: String): IScriptDynArray;
  end;

  // IScriptObjInterface
  //
  IScriptObjInterface = interface(IDataContext)
    ['{86B77C28-C396-4D53-812B-8FF1867A6128}']
    function GetScriptObj: IScriptObj;
  end;

  // IScriptDynArray
  //
  IScriptDynArray = interface(IGetSelf)
    ['{29767B6E-05C0-40E1-A41A-94DF54142312}']
    function GetElementSize: Integer;
    property ElementSize: Integer read GetElementSize;
    function GetElementType: TTypeSymbol;
    property elementType: TTypeSymbol read GetElementType;

    function GetArrayLength: NativeInt;
    procedure SetArrayLength(n: NativeInt);
    property ArrayLength: NativeInt read GetArrayLength write SetArrayLength;

    function BoundsCheckPassed(Index: NativeInt): Boolean;

    function ToStringArray: TStringDynArray;
    function ToInt64Array: TInt64DynArray;

    procedure Insert(Index: NativeInt);
    procedure Delete(Index, Count: NativeInt);
    procedure MoveItem(source, destination: NativeInt);
    procedure Swap(index1, index2: NativeInt);

    function IndexOfValue(const item: Variant; fromIndex: NativeInt): NativeInt;
    function IndexOfInteger(item: Int64; fromIndex: NativeInt): NativeInt;
    function IndexOfFloat(item: Double; fromIndex: NativeInt): NativeInt;
    function IndexOfString(const item: String; fromIndex: NativeInt): NativeInt;
    function IndexOfInterface(const item: IUnknown; fromIndex: NativeInt)
      : NativeInt;
    function IndexOfFuncPtr(const item: Variant; fromIndex: NativeInt)
      : NativeInt;

    procedure WriteData(destAddr: NativeInt; const src: IDataContext;
      srcAddr, size: NativeInt); overload;
    procedure Concat(const src: IScriptDynArray; Index, size: NativeInt);

    procedure Reverse;
    procedure NaturalSort;

    procedure AddStrings(sl: TStrings);
    procedure AppendString(Index: NativeInt; const str: String);

    function GetAsFloat(Index: NativeInt): Double;
    procedure SetAsFloat(Index: NativeInt; const v: Double);
    property AsFloat[index: NativeInt]: Double read GetAsFloat write SetAsFloat;

    function GetAsInteger(Index: NativeInt): Int64;
    procedure SetAsInteger(Index: NativeInt; const v: Int64);
    property AsInteger[index: NativeInt]: Int64 read GetAsInteger
      write SetAsInteger;

    function GetAsBoolean(Index: NativeInt): Boolean;
    procedure SetAsBoolean(Index: NativeInt; const v: Boolean);
    property AsBoolean[index: NativeInt]: Boolean read GetAsBoolean
      write SetAsBoolean;

    procedure SetAsVariant(Index: NativeInt; const v: Variant);
    procedure EvalAsVariant(Index: NativeInt; var result: Variant);
    property AsVariant[index: NativeInt]: Variant write SetAsVariant;

    procedure SetAsString(Index: NativeInt; const v: String);
    procedure EvalAsString(Index: NativeInt; var result: String);
    property AsString[index: NativeInt]: String write SetAsString;

    procedure SetAsInterface(Index: NativeInt; const v: IUnknown);
    procedure EvalAsInterface(Index: NativeInt; var result: IUnknown);
    property AsInterface[index: NativeInt]: IUnknown write SetAsInterface;

    procedure AddFromExpr(exec: TdwsExecution; valueExpr: TExprBase);
    function SetFromExpr(Index: NativeInt; exec: TdwsExecution;
      valueExpr: TExprBase): Boolean;

    function IsEmpty(addr: NativeInt): Boolean;
    function VarType(addr: NativeInt): TVarType;

    function HashCode(addr: NativeInt; size: NativeInt): Cardinal;
  end;

  PIScriptDynArray = ^IScriptDynArray;

  // IScriptAssociativeArray
  IScriptAssociativeArray = interface(IGetSelf)
    ['{1162D4BD-6033-4505-8D8C-0715588C768C}']
    procedure Clear;
    function Count: NativeInt;
  end;

  TPerfectMatchEnumerator = class
    funcSym, Match: TFuncSymbol;
    function callback(sym: TSymbol): Boolean;
  end;

  // The script has to be stopped because of an error
  EScriptError = class(Exception)
  private
    FScriptPos: TScriptPos;
    FScriptCallStack: TdwsExprLocationArray;
    FRawClassName: String;

    procedure SetScriptPos(const aPos: TScriptPos);

  public
    constructor CreatePosFmt(const aScriptPos: TScriptPos; const msg: String;
      const args: array of const);

    property scriptPos: TScriptPos read FScriptPos write SetScriptPos;
    // FScriptPos;
    property ScriptCallStack: TdwsExprLocationArray read FScriptCallStack
      write FScriptCallStack;
    property RawClassName: String read FRawClassName write FRawClassName;
  end;

  EScriptStopped = class(EScriptError)
  public
    class procedure DoRaise(exec: TdwsExecution; stoppedOn: TExprBase); static;
  end;

const
  cFuncKindToString: array [Low(TFuncKind) .. High(TFuncKind)
    ] of String = ('function', 'procedure', 'constructor', 'destructor',
    'method', 'lambda');
  cParamSymbolSemanticsPrefix
    : array [Low(TParamSymbolSemantics) .. High(TParamSymbolSemantics)
    ] of String = ('', 'const', 'var', 'lazy');
  cFirstFieldUnprepared = Pointer(-1);
  cDefaultRandSeed: UInt64 = 88172645463325252;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  System.Math, System.Variants, System.TypInfo,
  dwsCompilerUtils, dwsStrings, dwsXPlatform;

// ------------------
// ------------------ TdwsExprLocation ------------------
// ------------------

// Line
//
function TdwsExprLocation.Line: Integer;
begin
  result := Expr.scriptPos.Line;
end;

// SourceName
//
function TdwsExprLocation.SourceName: String;
begin
  result := Expr.scriptPos.SourceName;
end;

// Location
//
function TdwsExprLocation.Location: String;
begin
  result := Expr.ScriptLocation(Prog);
end;

// ------------------
// ------------------ TExprBase ------------------
// ------------------

// CallStackToString
//
class function TExprBase.CallStackToString(const CallStack
  : TdwsExprLocationArray): String;
var
  i: Integer;
  buffer: TWriteOnlyBlockStream;
  Expr: TExprBase;
begin
  buffer := TWriteOnlyBlockStream.Create;
  try
    for i := 0 to High(CallStack) do
    begin
      if i > 0 then
        buffer.WriteString(#13#10);
      Expr := CallStack[i].Expr;
      if Expr <> nil then
        buffer.WriteString(Expr.ScriptLocation(CallStack[i].Prog));
    end;
    result := buffer.ToString;
  finally
    buffer.Free;
  end;
end;

// RecursiveEnumerateSubExprs
//
function TExprBase.RecursiveEnumerateSubExprs(const callback
  : TExprBaseEnumeratorProc): Boolean;
var
  i: Integer;
  abort: Boolean;
  base, Expr: TExprBase;
  Stack: TSimpleStack<TExprBase>;
begin
  if Self = nil then
    Exit(False);
  Stack := TSimpleStack<TExprBase>.Create;
  try
    abort := False;
    Stack.Push(Self);
    repeat
      base := Stack.Peek;
      Stack.Pop;
      for i := 0 to base.SubExprCount - 1 do
      begin
        Expr := base.SubExpr[i];
        if Expr <> nil then
        begin
          Stack.Push(Expr);
          callback(base, Expr, abort);
          if abort then
            Exit(True);
        end;
      end;
    until Stack.Count = 0;
  finally
    Stack.Free;
  end;
  result := False;
end;

// ReferencesVariable
//
function TExprBase.ReferencesVariable(varSymbol: TDataSymbol): Boolean;
var
  i: Integer;
  sub: TExprBase;
begin
  for i := 0 to SubExprCount - 1 do
  begin
    sub := SubExpr[i];
    if (sub <> nil) and sub.ReferencesVariable(varSymbol) then
      Exit(True)
  end;
  result := False;
end;

// IndexOfSubExpr
//
function TExprBase.IndexOfSubExpr(Expr: TExprBase): Integer;
var
  i: Integer;
begin
  for i := 0 to SubExprCount - 1 do
    if SubExpr[i] = Expr then
      Exit(i);
  result := -1;
end;

// GetSubExpr
//
function TExprBase.GetSubExpr(i: Integer): TExprBase;
begin
  result := nil;
end;

// GetSubExprCount
//
function TExprBase.GetSubExprCount: Integer;
begin
  result := 0;
end;

procedure TExprBase.EvalNoResult(exec: TdwsExecution);
var
  buf: Variant;
begin
  EvalAsVariant(exec, buf);
end;

// GetIsConstant
//
function TExprBase.GetIsConstant: Boolean;
begin
  result := False;
end;

// IsConstant
//
function TExprBase.IsConstant: Boolean;
begin
  result := (Self <> nil) and GetIsConstant;
end;

// Eval
//
function TExprBase.Eval(exec: TdwsExecution): Variant;
begin
  EvalAsVariant(exec, result);
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec: TdwsExecution; e: EScriptError);
begin
  e.scriptPos := scriptPos;
  e.ScriptCallStack := exec.GetCallStack;
  raise e;
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec: TdwsExecution);
var
  exc: Exception;
  e: EScriptError;
begin
  if (ExceptObject is EScriptError) or (ExceptObject is EScriptException) then
    raise Exception(AcquireExceptionObject);
  exc := ExceptObject as Exception;
  e := EScriptError.Create(exc.Message);
  e.RawClassName := exc.ClassName;
  RaiseScriptError(exec, e);
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec: TdwsExecution; const msg: String);
begin
  RaiseScriptError(exec, EScriptError, msg);
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec: TdwsExecution;
  exceptClass: EScriptErrorClass; const msg: String);
begin
  RaiseScriptError(exec, exceptClass.Create(msg));
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec: TdwsExecution;
  exceptClass: EScriptErrorClass; const msg: String;
  const args: array of const);
begin
  RaiseScriptError(exec, exceptClass.CreateFmt(msg, args));
end;

// RaiseObjectNotInstantiated
//
procedure TExprBase.RaiseObjectNotInstantiated(exec: TdwsExecution);
begin
  RaiseScriptError(exec, EScriptError, RTE_ObjectNotInstantiated);
end;

// RaiseObjectAlreadyDestroyed
//
procedure TExprBase.RaiseObjectAlreadyDestroyed(exec: TdwsExecution);
begin
  RaiseScriptError(exec, EScriptError, RTE_ObjectAlreadyDestroyed);
end;

// Specialize
//
function TExprBase.Specialize(const context: ISpecializationContext): TExprBase;
begin
  context.AddCompilerErrorFmt('Specialization of %s is not supported yet.',
    [Self.ClassName]);
  result := nil;
end;

// FuncSymQualifiedName
//
function TExprBase.FuncSymQualifiedName: String;
begin
  result := '';
end;

// EvalAsSafeScriptObj
//
procedure TExprBase.EvalAsSafeScriptObj(exec: TdwsExecution;
  var result: IScriptObj);
begin
  EvalAsScriptObj(exec, result);
  if result = nil then
    RaiseObjectNotInstantiated(exec)
  else if result.Destroyed then
    RaiseObjectAlreadyDestroyed(exec);
end;

// EvalAsVariantToDataContext
//
procedure TExprBase.EvalAsVariantToDataContext(exec: TdwsExecution;
  const dc: IDataContext; offset: Integer);
var
  buf: Variant;
begin
  EvalAsVariant(exec, buf);
  dc.AsVariant[offset] := buf;
end;

// Taxonomy
//
function TExprBase.Taxonomy: TExprBaseTaxonomy;
begin
  result := ebtExprBase;
end;

// ------------------
// ------------------ TSymbol ------------------
// ------------------

// Create
//
constructor TSymbol.Create(const aName: String; aType: TTypeSymbol);
begin
  inherited Create;
  FName := aName;
  FTyp := aType;
  if Assigned(aType) then
    FSize := aType.FSize;
end;

// SafeGetCaption
//
function TSymbol.SafeGetCaption: String;
begin
  if Self = nil then
    result := SYS_VOID
  else
    result := GetCaption;
end;

// GetCaption
//
function TSymbol.GetCaption: String;
begin
  if Name <> '' then
    result := Name
  else
    result := ClassName;
end;

// GetDescription
//
function TSymbol.GetDescription: String;
begin
  if FName <> '' then
    result := FName
  else
    result := Caption;
end;

// Initialize
//
procedure TSymbol.Initialize(const Msgs: TdwsCompileMessageList);
begin
end;

// IsBaseType
//
class function TSymbol.IsBaseType: Boolean;
begin
  result := False;
end;

// IsType
//
function TSymbol.IsType: Boolean;
begin
  result := False;
end;

// IsPointerType
//
function TSymbol.IsPointerType: Boolean;
begin
  result := False;
end;

// Taxonomy
//
function TSymbol.Taxonomy: TdwsSymbolTaxonomy;
begin
  result := [];
end;

// IsClassSymbol
//
function TSymbol.IsClassSymbol: Boolean;
begin
  result := (Self <> nil) and Self.GetIsClassSymbol;
end;

// IsDataSymbol
//
function TSymbol.IsDataSymbol: Boolean;
begin
  result := (Self <> nil) and Self.GetIsDataSymbol;
end;

// GetAsFuncSymbol
//
function TSymbol.GetAsFuncSymbol: TFuncSymbol;
begin
  result := nil;
end;

// GetIsGeneric
//
function TSymbol.GetIsGeneric: Boolean;
begin
  if FTyp <> nil then
    result := FTyp.IsGeneric
  else
    result := False;
end;

// GetExternalName
//
function TSymbol.GetExternalName: String;
begin
  if FExternalName <> '' then
    result := FExternalName
  else
    result := FName;
end;

// GetIsClassSymbol
//
function TSymbol.GetIsClassSymbol: Boolean;
begin
  result := False;
end;

// GetIsDataSymbol
//
function TSymbol.GetIsDataSymbol: Boolean;
begin
  result := False;
end;

// AsFuncSymbol
//
function TSymbol.AsFuncSymbol: TFuncSymbol;
begin
  if Self <> nil then
    result := GetAsFuncSymbol
  else
    result := nil;
end;

// IsGeneric
//
function TSymbol.IsGeneric: Boolean;
begin
  if Self <> nil then
    result := GetIsGeneric
  else
    result := False;
end;

// GetIsOverloaded
//
function TSymbol.GetIsOverloaded: Boolean;
begin
  result := False;
end;

// AsFuncSymbol
//
function TSymbol.AsFuncSymbol(var funcSym: TFuncSymbol): Boolean;
begin
  if Self <> nil then
    funcSym := GetAsFuncSymbol
  else
    funcSym := nil;
  result := (funcSym <> nil);
end;

// QualifiedName
//
function TSymbol.QualifiedName: String;
begin
  result := String(Name);
end;

// IsVisibleFor
//
function TSymbol.IsVisibleFor(const aVisibility: TdwsVisibility): Boolean;
begin
  result := True;
end;

// Specialize
//
function TSymbol.Specialize(const context: ISpecializationContext): TSymbol;
begin
  context.AddCompilerErrorFmt(CPE_SpecializationNotSupportedYet, [ClassName]);
  result := nil;
end;

// HasExternalName
//
function TSymbol.HasExternalName: Boolean;
begin
  result := (FExternalName <> '');
end;

function TSymbol.BaseType: TTypeSymbol;
begin
  result := nil;
end;

// SetName
//
procedure TSymbol.SetName(const newName: String; force: Boolean = False);
begin
  Assert(force or (FName = ''));
  FName := newName;
end;

// ------------------
// ------------------ TCompositeTypeSymbol ------------------
// ------------------

// Create
//
constructor TCompositeTypeSymbol.Create(const Name: String; aUnit: TSymbol);
begin
  inherited Create(name, nil);
  FUnitSymbol := aUnit;
  FMembers := CreateMembersTable(nil);
  FFirstField := cFirstFieldUnprepared;
end;

// Destroy
//
destructor TCompositeTypeSymbol.Destroy;
begin
  FMembers.Free;
  inherited;
end;

// AddConst
//
procedure TCompositeTypeSymbol.AddConst(sym: TClassConstSymbol);
begin
  sym.OwnerSymbol := Self;
  FMembers.AddSymbol(sym);
end;

// AddConst
//
procedure TCompositeTypeSymbol.AddConst(sym: TClassConstSymbol;
  Visibility: TdwsVisibility);
begin
  sym.Visibility := Visibility;
  AddConst(sym);
end;

// AddClassVar
//
procedure TCompositeTypeSymbol.AddClassVar(sym: TClassVarSymbol);
begin
  sym.OwnerSymbol := Self;
  FMembers.AddSymbol(sym);
end;

// AddProperty
//
procedure TCompositeTypeSymbol.AddProperty(propSym: TPropertySymbol);
begin
  FMembers.AddSymbol(propSym);
  propSym.FOwnerSymbol := Self;
end;

// AddMethod
//
procedure TCompositeTypeSymbol.AddMethod(methSym: TMethodSymbol);
begin
  FMembers.AddSymbol(methSym);
  methSym.FStructSymbol := Self;
end;

// AddField
//
procedure TCompositeTypeSymbol.AddField(fieldSym: TFieldSymbol);
begin
  Assert(FFirstField = cFirstFieldUnprepared);
  FMembers.AddSymbol(fieldSym);
  fieldSym.FStructSymbol := Self;
end;

// FieldAtOffset
//
function TCompositeTypeSymbol.FieldAtOffset(offset: Integer): TFieldSymbol;
var
  sym: TSymbol;
begin
  for sym in Members do
  begin
    if sym.ClassType = TFieldSymbol then
    begin
      result := TFieldSymbol(sym);
      if result.offset = offset then
        Exit;
    end;
  end;
  result := nil;
end;

// AllowVirtualMembers
//
function TCompositeTypeSymbol.AllowVirtualMembers: Boolean;
begin
  result := False;
end;

// AllowOverloads
//
function TCompositeTypeSymbol.AllowOverloads: Boolean;
begin
  result := True;
end;

// CreateMembersTable
//
function TCompositeTypeSymbol.CreateMembersTable(addrGenerator: TAddrGenerator)
  : TMembersSymbolTable;
begin
  result := TMembersSymbolTable.Create(nil, addrGenerator);
  result.owner := Self;
end;

// GetIsStatic
//
function TCompositeTypeSymbol.GetIsStatic: Boolean;
begin
  result := False;
end;

// GetIsExternal
//
function TCompositeTypeSymbol.GetIsExternal: Boolean;
begin
  result := False;
end;

// GetIsExternalRooted
//
function TCompositeTypeSymbol.GetIsExternalRooted: Boolean;
begin
  result := IsExternal;
end;

// GetIsPartial
//
function TCompositeTypeSymbol.GetIsPartial: Boolean;
begin
  result := False;
end;

// GetIsImmutable
//
function TCompositeTypeSymbol.GetIsImmutable: Boolean;
begin
  result := False;
end;

// FindDefaultConstructor
//
function TCompositeTypeSymbol.FindDefaultConstructor(minVisibility
  : TdwsVisibility): TMethodSymbol;
begin
  result := nil;
end;

// MembersVisibilities
//
function TCompositeTypeSymbol.MembersVisibilities: TdwsVisibilities;
begin
  result := Members.Visibilities;
  if parent <> nil then
    result := result + parent.MembersVisibilities;
end;

// HasNestedTypes
//
function TCompositeTypeSymbol.HasNestedtypes: Boolean;
begin
  result := Members.HasNestedtypes;
end;

// CheckMethodsImplemented
//
function CompareSourceMethSymbolByDeclarePos(a, b: Pointer): Integer;
begin
  result := TSourceMethodSymbol(a).DeclarationPosition.Compare
    (TSourceMethodSymbol(b).DeclarationPosition);
end;

procedure TCompositeTypeSymbol.CheckMethodsImplemented
  (const Msgs: TdwsCompileMessageList);

  procedure CreateAutoFixAddImplementation(msg: TScriptMessage;
    methSym: TMethodSymbol);
  var
    afa: TdwsAFAAddImplementation;
    buf: String;
    k: Integer;
  begin
    afa := TdwsAFAAddImplementation.Create(msg, AFA_AddImplementation);
    buf := methSym.GetDescription;
    FastStringReplace(buf, '()', ' ');
    afa.Text := #10 + TrimRight(buf) + ';'#10'begin'#10#9'|'#10'end;'#10;
    k := Pos(methSym.Name, afa.Text);
    afa.Text := Copy(afa.Text, 1, k - 1) + methSym.StructSymbol.Name + '.' +
      Copy(afa.Text, k);
  end;

var
  methSym: TMethodSymbol;
  msg: TScriptMessage;
  errorList: TTightList;
begin
  errorList.Initialize;
  for var i := 0 to FMembers.Count - 1 do
  begin
    if FMembers[i] is TMethodSymbol then
    begin
      methSym := TMethodSymbol(FMembers[i]);
      if methSym.ClassType = TAliasMethodSymbol then
        continue;
      if methSym.IgnoreMissingImplementation then
        continue;
      if methSym.IsAbstract then
        continue;

      if Assigned(methSym.FExecutable) then
        methSym.FExecutable.InitSymbol(FMembers[i], Msgs)
      else if not methSym.IsExternal then
      begin
        if methSym is TSourceMethodSymbol then
        begin
          errorList.Add(methSym)
        end
        else
        begin
          Msgs.AddCompilerErrorFmt(cNullPos, CPE_MethodNotImplemented,
            [methSym.Name, methSym.StructSymbol.Caption]);
        end;
      end;
    end;
  end;
  if errorList.Count > 0 then
  begin
    errorList.Sort(CompareSourceMethSymbolByDeclarePos);
    for var i := 0 to errorList.Count - 1 do
    begin
      methSym := TMethodSymbol(errorList.List[i]);
      msg := Msgs.AddCompilerErrorFmt(methSym.DeclarationPosition,
        CPE_MethodNotImplemented, [methSym.Name, methSym.StructSymbol.Caption]);
      CreateAutoFixAddImplementation(msg, methSym);
    end;
    errorList.Clear;
  end;
end;

// ExternalRoot
//
function TCompositeTypeSymbol.ExternalRoot: TCompositeTypeSymbol;
begin
  result := Self;
  while (result <> nil) and not result.IsExternal do
    result := result.parent;
end;

// AllowFields
//
function TCompositeTypeSymbol.AllowFields: Boolean;
begin
  result := False;
end;

// AllowAnonymousMethods
//
function TCompositeTypeSymbol.AllowAnonymousMethods: Boolean;
begin
  result := True;
end;

// PrepareFirstField
//
function TCompositeTypeSymbol.PrepareFirstField: TFieldSymbol;
var
  member: TSymbol;
begin
  if parent <> nil then
    result := parent.FirstField
  else
    result := nil;
  for member in Members do
  begin
    if member is TFieldSymbol then
    begin
      TFieldSymbol(member).NextField := result;
      result := TFieldSymbol(member);
    end;
  end;
  FFirstField := result;
end;

// FirstField
//
function TCompositeTypeSymbol.FirstField: TFieldSymbol;
begin
  if FFirstField = cFirstFieldUnprepared then
    PrepareFirstField;
  result := FFirstField;
end;

// SpecializeMembers
//
procedure TCompositeTypeSymbol.SpecializeMembers(destination
  : TCompositeTypeSymbol; const context: ISpecializationContext);
var
  i: Integer;
  member: TSymbol;
  specialized: TSymbol;
  field, specializedField: TFieldSymbol;
  fieldType: TTypeSymbol;
  specializedProp: TPropertySymbol;
  firstPropertyIndex: Integer;
begin
  firstPropertyIndex := Members.Count;
  // specialize fields in a first pass
  for i := 0 to Members.Count - 1 do
  begin
    member := Members[i];
    if member is TFieldSymbol then
    begin
      field := TFieldSymbol(member);
      fieldType := context.SpecializeType(field.typ);
      specializedField := TFieldSymbol.Create(field.Name, fieldType,
        field.Visibility);
      specializedField.ExternalName := field.ExternalName;
      destination.AddField(specializedField);
      context.RegisterSpecialization(field, specializedField);
    end;
  end;
  // specialize methods
  for i := 0 to Members.Count - 1 do
  begin
    member := Members[i];
    if member is TFieldSymbol then
    begin
      // already specialized
      continue;
    end
    else if member is TMethodSymbol then
    begin
      specialized := TMethodSymbol(member).SpecializeType(context);
      if specialized <> nil then
      begin
        destination.AddMethod(specialized as TMethodSymbol);
        context.RegisterSpecialization(member, specialized);
        specialized.IncRefCount;
      end;
    end
    else if member is TPropertySymbol then
    begin
      // specialize properties in a separate pass as they refer other members
      if i < firstPropertyIndex then
        firstPropertyIndex := i;
    end
    else
    begin
      context.AddCompilerErrorFmt(CPE_SpecializationNotSupportedYet,
        [member.ClassName]);
    end;
  end;
  // specialize properties
  for i := firstPropertyIndex to Members.Count - 1 do
  begin
    member := Members[i];
    if member is TPropertySymbol then
    begin
      specializedProp := member.Specialize(context) as TPropertySymbol;
      destination.AddProperty(specializedProp);
    end;
  end;
end;

// GetIsGeneric
//
function TCompositeTypeSymbol.GetIsGeneric: Boolean;
begin
  result := False; // TODO
end;

// ------------------
// ------------------ TStructuredTypeSymbol ------------------
// ------------------

// Destroy
//
destructor TStructuredTypeSymbol.Destroy;
begin
  if FForwardPosition <> nil then
    Dispose(FForwardPosition);
  FMetaSymbol.Free;
  inherited;
end;

// DoInheritFrom
//
procedure TStructuredTypeSymbol.DoInheritFrom(ancestor: TStructuredTypeSymbol);
begin
  Assert(FParent = nil);
  Assert(FMembers.Count = 0);

  FMembers.AddParent(ancestor.Members);
  FParent := ancestor;
end;

// NthParentOf
//
function TStructuredTypeSymbol.NthParentOf(structType
  : TCompositeTypeSymbol): Integer;
begin
  result := 0;
  while structType <> nil do
  begin
    if structType = Self then
      Exit
    else
    begin
      structType := structType.parent;
      Inc(result);
    end;
  end;
  result := -1;
end;

// DistanceTo
//
function TStructuredTypeSymbol.DistanceTo(typeSym: TTypeSymbol): Integer;
begin
  if typeSym = Self then
    result := 0
  else if typeSym is TStructuredTypeSymbol then
    result := TStructuredTypeSymbol(typeSym).NthParentOf(Self)
  else
    result := MaxInt;
end;

// IsForwarded
//
function TStructuredTypeSymbol.IsForwarded: Boolean;
begin
  result := Assigned(FForwardPosition);
end;

// DuckTypedMatchingMethod
//
function TStructuredTypeSymbol.DuckTypedMatchingMethod(methSym: TMethodSymbol;
  Visibility: TdwsVisibility): TMethodSymbol;
var
  sym: TSymbol;
  meth: TMethodSymbol;
begin
  for sym in Members do
  begin
    if sym is TMethodSymbol then
    begin
      meth := TMethodSymbol(sym);
      if (meth.Visibility >= Visibility) and UnicodeSameText(meth.Name,
        methSym.Name) and meth.IsCompatible(methSym) then
        Exit(meth);
    end;
  end;
  if parent <> nil then
    result := (parent as TStructuredTypeSymbol).DuckTypedMatchingMethod
      (methSym, cvPublic)
  else
    result := nil;
end;

// FindDefaultConstructor
//
function TStructuredTypeSymbol.FindDefaultConstructor(minVisibility
  : TdwsVisibility): TMethodSymbol;
begin
  result := nil;
end;

// AllowDefaultProperty
//
function TStructuredTypeSymbol.AllowDefaultProperty: Boolean;
begin
  result := True;
end;

// SetForwardedPos
//
procedure TStructuredTypeSymbol.SetForwardedPos(const aScriptPos: TScriptPos);
begin
  if FForwardPosition = nil then
    New(FForwardPosition);
  FForwardPosition^ := aScriptPos;
end;

// ClearIsForwarded
//
procedure TStructuredTypeSymbol.ClearIsForwarded;
begin
  Dispose(FForwardPosition);
  FForwardPosition := nil;
end;

// GetIsExternal
//
function TStructuredTypeSymbol.GetIsExternal: Boolean;
begin
  result := False;
end;

// GetMetaSymbol
//
function TStructuredTypeSymbol.GetMetaSymbol: TStructuredTypeMetaSymbol;
begin
  result := FMetaSymbol;
end;

// ------------------
// ------------------ TStructuredTypeMetaSymbol ------------------
// ------------------

// Create
//
constructor TStructuredTypeMetaSymbol.Create(const Name: String;
  typ: TStructuredTypeSymbol);
begin
  inherited Create(name, typ);
  FSize := 1;
end;

// InitDataContext
//
procedure TStructuredTypeMetaSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetZeroInt64(offset);
end;

// IsCompatible
//
function TStructuredTypeMetaSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym is TStructuredTypeMetaSymbol) and
    (typ.BaseType = typSym.typ.BaseType);
end;

// StructSymbol
//
function TStructuredTypeMetaSymbol.StructSymbol: TStructuredTypeSymbol;
begin
  result := TStructuredTypeSymbol(typ);
end;

// Parent
//
function TStructuredTypeMetaSymbol.parent: TStructuredTypeMetaSymbol;
var
  p: TCompositeTypeSymbol;
begin
  p := StructSymbol.parent;
  if (p = nil) or not(p is TStructuredTypeSymbol) then
    result := nil
  else
    result := TStructuredTypeSymbol(p).MetaSymbol;
end;

// ------------------
// ------------------ TRecordSymbol ------------------
// ------------------

// Create
//
constructor TRecordSymbol.Create(const Name: String; aUnit: TSymbol);
begin
  inherited Create(name, aUnit);
  FMetaSymbol := TStructuredTypeMetaSymbol.Create('meta of ' + name, Self);
end;

// AddField
//
procedure TRecordSymbol.AddField(fieldSym: TFieldSymbol);
begin
  inherited;
  fieldSym.FOffset := FSize;
  FSize := FSize + fieldSym.typ.size;
  if fieldSym.DefaultExpr <> nil then
    IsDynamic := True;
  if fieldSym.typ.DynamicInitialization then
    Include(FFlags, rsfDynamicInitialization);
end;

// AddMethod
//
procedure TRecordSymbol.AddMethod(methSym: TMethodSymbol);
begin
  inherited;
  if methSym.IsClassMethod then
    methSym.SetIsStatic;
end;

// Initialize
//
procedure TRecordSymbol.Initialize(const Msgs: TdwsCompileMessageList);
begin
  CheckMethodsImplemented(Msgs);
end;

// AllowFields
//
function TRecordSymbol.AllowFields: Boolean;
begin
  result := True;
end;

// CreateSelfParameter
//
function TRecordSymbol.CreateSelfParameter(methSym: TMethodSymbol): TDataSymbol;
begin
  if methSym.IsClassMethod then
    result := nil
  else
  begin
    result := TVarParamSymbol.Create(SYS_SELF, Self, [psoInternal]);
    methSym.params.AddSymbol(result);
  end;
end;

// CreateAnonymousMethod
//
function TRecordSymbol.CreateAnonymousMethod(aFuncKind: TFuncKind;
  aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
  : TMethodSymbol;
begin
  result := TSourceMethodSymbol.Create('', aFuncKind, Self, aVisibility,
    aCreateOptions);
  if mcoClassMethod in aCreateOptions then
    TSourceMethodSymbol(result).SetIsStatic;
end;

// InitDataContext
//
procedure TRecordSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
var
  field: TFieldSymbol;
begin
  field := FirstField;
  while field <> nil do
  begin
    field.InitDataContext(data, offset);
    field := field.NextField;
  end;
end;

// DynamicInitialization
//
function TRecordSymbol.DynamicInitialization: Boolean;
begin
  result := rsfDynamicInitialization in FFlags;
end;

// IsCompatible
//
function TRecordSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  if typSym = nil then
    Exit(False);
  typSym := typSym.UnAliasedType.BaseType;
  if not(typSym is TRecordSymbol) then
    Exit(False);
  if Self = typSym then
    Exit(True);

  result := False;
end;

// AssignsAsDataExpr
//
function TRecordSymbol.AssignsAsDataExpr: Boolean;
begin
  result := True;
end;

// SpecializeType
//
function TRecordSymbol.SpecializeType(const context: ISpecializationContext)
  : TTypeSymbol;
var
  specializedRecord: TRecordSymbol;
begin
  Assert(rsfFullyDefined in FFlags);

  specializedRecord := TRecordSymbol.Create(context.Name, context.UnitSymbol);

  context.EnterComposite(specializedRecord);
  try
    SpecializeMembers(specializedRecord, context);
  finally
    context.LeaveComposite;
  end;

  specializedRecord.FFlags := FFlags;
  result := specializedRecord;
end;

// SetIsExternal
//
procedure TRecordSymbol.SetIsExternal;
begin
  Include(FFlags, rsfExternal);
end;

// GetCaption
//
function TRecordSymbol.GetCaption: String;
begin
  if Name = '' then
    result := 'anonymous record'
  else
    result := Name;
end;

// GetDescription
//
function TRecordSymbol.GetDescription: String;
var
  member: TSymbol;
begin
  if Name = '' then
    result := 'anonymous record'#13#10
  else
    result := Name + ' = record'#13#10;
  for member in FMembers do
  begin
    if member is TFieldSymbol then
      result := result + '   ' + member.Name + ' : ' + member.typ.Name +
        ';'#13#10;
  end;
  result := result + 'end;';
end;

// GetIsDynamic
//
function TRecordSymbol.GetIsDynamic: Boolean;
begin
  result := (rsfDynamic in FFlags);
end;

// SetIsDynamic
//
procedure TRecordSymbol.SetIsDynamic(const val: Boolean);
begin
  if val then
    Include(FFlags, rsfDynamic)
  else
    Exclude(FFlags, rsfDynamic);
end;

// GetIsImmutable
//
function TRecordSymbol.GetIsImmutable: Boolean;
begin
  result := (rsfImmutable in FFlags);
end;

// SetIsImmutable
//
procedure TRecordSymbol.SetIsImmutable(const val: Boolean);
begin
  if val then
    Include(FFlags, rsfImmutable)
  else
    Exclude(FFlags, rsfImmutable);
end;

// GetIsFullyDefined
//
function TRecordSymbol.GetIsFullyDefined: Boolean;
begin
  result := (rsfFullyDefined in FFlags);
end;

// SetIsFullyDefined
//
procedure TRecordSymbol.SetIsFullyDefined(const val: Boolean);
begin
  if val then
    Include(FFlags, rsfFullyDefined)
  else
    Exclude(FFlags, rsfFullyDefined);
end;

// GetIsExternal
//
function TRecordSymbol.GetIsExternal: Boolean;
begin
  result := rsfExternal in FFlags;
end;

// ------------------
// ------------------ TInterfaceSymbol ------------------
// ------------------

// Create
//
constructor TInterfaceSymbol.Create(const Name: String; aUnit: TSymbol);
begin
  inherited;
  FSize := 1;
end;

// GetCaption
//
function TInterfaceSymbol.GetCaption: String;
begin
  result := Name;
end;

// GetDescription
//
function TInterfaceSymbol.GetDescription: String;
begin
  result := Name + ' = interface';
  if parent <> nil then
    result := result + '(' + parent.Name + ')';
end;

// InheritFrom
//
procedure TInterfaceSymbol.InheritFrom(ancestor: TInterfaceSymbol);
begin
  DoInheritFrom(ancestor);
  FMethodCount := ancestor.MethodCount;
end;

// AddMethod
//
procedure TInterfaceSymbol.AddMethod(methSym: TMethodSymbol);
begin
  inherited;
  if methSym.Name <> '' then
  begin
    methSym.FVMTIndex := FMethodCount;
    Inc(FMethodCount);
  end;
end;

// InitDataContext
//
procedure TInterfaceSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetNilInterface(offset);
end;

// Initialize
//
procedure TInterfaceSymbol.Initialize(const Msgs: TdwsCompileMessageList);
begin
  // Check validity of the interface declaration
  if IsForwarded then
    Msgs.AddCompilerErrorFmt(FForwardPosition^,
      CPE_InterfaceNotCompletelyDefined, [Name]);
end;

// IsCompatible
//
function TInterfaceSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  typSym := typSym.UnAliasedType;
  if typSym is TNilSymbol then
    result := True
  else if typSym is TInterfaceSymbol then
    result := (NthParentOf(TInterfaceSymbol(typSym)) >= 0)
  else
    result := False;
end;

// IsPointerType
//
function TInterfaceSymbol.IsPointerType: Boolean;
begin
  result := True;
end;

// SpecializeType
//
function TInterfaceSymbol.SpecializeType(const context: ISpecializationContext)
  : TTypeSymbol;
var
  specializedInterface: TInterfaceSymbol;
begin
  specializedInterface := TInterfaceSymbol.Create(context.Name,
    context.UnitSymbol);

  context.EnterComposite(specializedInterface);
  try
    SpecializeMembers(specializedInterface, context);
  finally
    context.LeaveComposite;
  end;

  result := specializedInterface;
end;

// CreateSelfParameter
//
function TInterfaceSymbol.CreateSelfParameter(methSym: TMethodSymbol)
  : TDataSymbol;
begin
  Assert(not methSym.IsClassMethod);
  result := TSelfSymbol.Create(SYS_SELF, Self);
  methSym.InternalParams.AddSymbol(result);
end;

// CreateAnonymousMethod
//
function TInterfaceSymbol.CreateAnonymousMethod(aFuncKind: TFuncKind;
  aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
  : TMethodSymbol;
begin
  result := TSourceMethodSymbol.Create('', aFuncKind, Self, aVisibility,
    aCreateOptions);
end;

// DoIsOfType
//
function TInterfaceSymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
begin
  typSym := typSym.UnAliasedType;
  if typSym is TInterfaceSymbol then
    result := (NthParentOf(TInterfaceSymbol(typSym)) >= 0)
  else
    result := False;
end;

// Parent
//
function TInterfaceSymbol.parent: TInterfaceSymbol;
begin
  result := TInterfaceSymbol(FParent);
end;

// ------------------
// ------------------ TFieldSymbol ------------------
// ------------------

// Create
//
constructor TFieldSymbol.Create(const Name: String; typ: TTypeSymbol;
  aVisibility: TdwsVisibility);
begin
  inherited Create(Name, typ);
  FVisibility := aVisibility;
end;

// Destroy
//
destructor TFieldSymbol.Destroy;
begin
  FDefaultExpr.Free;
  inherited;
end;

// QualifiedName
//
function TFieldSymbol.QualifiedName: String;
begin
  result := String(StructSymbol.QualifiedName + '.' + Name);
end;

// IsVisibleFor
//
function TFieldSymbol.IsVisibleFor(const aVisibility: TdwsVisibility): Boolean;
begin
  result := (FVisibility >= aVisibility);
end;

// InitDataContext
//
procedure TFieldSymbol.InitDataContext(const dc: IDataContext;
  structOffset: Integer);
begin
  if DefaultValue <> nil then
    dc.WriteData(structOffset + offset, DefaultValue, 0, typ.size)
  else
    typ.InitDataContext(dc, structOffset + offset);
end;

// ------------------
// ------------------ TClassConstSymbol ------------------
// ------------------

// QualifiedName
//
function TClassConstSymbol.QualifiedName: String;
begin
  result := String(OwnerSymbol.QualifiedName + '.' + Name);
end;

// IsVisibleFor
//
function TClassConstSymbol.IsVisibleFor(const aVisibility
  : TdwsVisibility): Boolean;
begin
  result := (FVisibility >= aVisibility);
end;

// ------------------
// ------------------ TClassVarSymbol ------------------
// ------------------

// Create
//
constructor TClassVarSymbol.Create(const aName: String; aType: TTypeSymbol;
  aVisibility: TdwsVisibility);
begin
  inherited Create(aName, aType);
  Visibility := aVisibility;
end;

// QualifiedName
//
function TClassVarSymbol.QualifiedName: String;
begin
  result := String(OwnerSymbol.QualifiedName + '.' + Name);
end;

// IsVisibleFor
//
function TClassVarSymbol.IsVisibleFor(const aVisibility
  : TdwsVisibility): Boolean;
begin
  result := (FVisibility >= aVisibility);
end;

// ------------------
// ------------------ TResultSymbol ------------------
// ------------------

// Specialize
//
function TResultSymbol.Specialize(const context
  : ISpecializationContext): TSymbol;
begin
  result := TResultSymbol.Create(Name, context.SpecializeType(typ));
end;

// ------------------
// ------------------ TFuncSymbol ------------------
// ------------------

// Create
//
constructor TFuncSymbol.Create(const Name: String; funcKind: TFuncKind;
  funcLevel: SmallInt);
begin
  inherited Create(name, nil);
  FKind := funcKind;
  FAddrGenerator := TAddrGeneratorRec.CreateNegative(funcLevel);
  FInternalParams := TUnSortedSymbolTable.Create(nil, @FAddrGenerator);
  FParams := TParamsSymbolTable.Create(FInternalParams, @FAddrGenerator);
  FSize := 1;
end;

// Destroy
//
destructor TFuncSymbol.Destroy;
begin
  if FForwardPosition <> nil then
    Dispose(FForwardPosition);
  FParams.Free;
  FInternalParams.Free;
  FConditions.Free;
  inherited;
end;

// Generate
//
constructor TFuncSymbol.Generate(Table: TSymbolTable; const funcName: String;
  const funcParams: TParamArray; const funcType: String);
var
  typSym: TTypeSymbol;
begin
  if funcType <> '' then
  begin
    Self.Create(funcName, fkFunction, 1);
    // Set function type
    typSym := Table.FindTypeSymbol(funcType, cvMagic);
    if (typSym = nil) or (typSym.BaseType = nil) then
      raise Exception.CreateFmt(CPE_TypeIsUnknown, [funcType]);
    Self.SetType(typSym);
  end
  else
  begin
    Self.Create(funcName, fkProcedure, 1);
  end;

  GenerateParams(Table, funcParams);
end;

// AddParam
//
procedure TFuncSymbol.AddParam(param: TParamSymbol);
begin
  params.AddSymbol(param);
end;

// AddParams
//
procedure TFuncSymbol.AddParams(params: TParamsSymbolTable);
var
  i: Integer;
begin
  for i := 0 to params.Count - 1 do
    AddParam(params[i].Clone);
end;

// HasParam
//
function TFuncSymbol.HasParam(param: TParamSymbol): Boolean;
begin
  result := (params.FindLocal(param.Name) <> nil);
end;

// SetType
//
procedure TFuncSymbol.SetType(const value: TTypeSymbol);
begin
  FTyp := value;
  Assert(FResult = nil);
  if FTyp <> nil then
  begin
    FResult := TResultSymbol.Create(SYS_RESULT, value);
    FInternalParams.AddSymbol(FResult);
  end;
end;

// GenerateParams
//
procedure GenerateParams(Table: TSymbolTable; const funcParams: TParamArray;
  const addProc: TAddParamSymbolMethod);

  procedure ConvertSetOfdefaultValue(paramRec: PParamRec);
  var
    v: Variant;
  begin
    v := paramRec.DefaultValue[0];
    if VariantIsString(v) then
    begin
      Assert((v = '[]') or (v = ''),
        'Unsupported "set of" default string value');
      paramRec.DefaultValue.AsInteger[0] := 0;
    end
    else if VariantIsOrdinal(v) or VariantIsFloat(v) then
    begin
      paramRec.DefaultValue.AsInteger[0] := Round(v);
    end
    else
      Assert(False, 'Unsupported "set of" default value');
  end;

var
  typSym: TTypeSymbol;
  baseTypClass: TClass;
  paramSym: TParamSymbol;
  paramSymWithDefault: TParamSymbolWithDefaultValue;
  paramRec: PParamRec;
begin
  typSym := nil;

  for var i := 0 to High(funcParams) do
  begin

    paramRec := @funcParams[i];
    if (typSym = nil) or not UnicodeSameText(typSym.Name, paramRec.ParamType)
    then
      typSym := Table.FindTypeSymbol(paramRec.ParamType, cvMagic);
    if (typSym = nil) and (paramRec.ParamType = 'array of Any Type') then
    begin
      typSym := TDynamicArraySymbol.Create('',
        Table.FindTypeSymbol(SYS_ANY_TYPE, cvPublic),
        Table.FindTypeSymbol(SYS_INTEGER, cvPublic));
      Table.AddSymbol(typSym);
    end;

    if not Assigned(typSym) then
      raise Exception.CreateFmt(CPE_TypeForParamNotFound,
        [paramRec.ParamType, paramRec.ParamName]);

    if paramRec.HasDefaultValue then
    begin

      if paramRec.IsVarParam then
        raise Exception.Create(CPE_VarParamCantHaveDefaultValue);
      if paramRec.IsConstParam then
        raise Exception.Create(CPE_ConstParamCantHaveDefaultValue);

      Assert(paramRec.DefaultValue.DataLength = 1);
      baseTypClass := typSym.BaseType.UnAliasedType.ClassType;
      if not baseTypClass.InheritsFrom(TBaseStringSymbol) then
      begin
        if baseTypClass.InheritsFrom(TBaseIntegerSymbol) then
        begin
          paramRec.DefaultValue[0] := VariantToInt64(paramRec.DefaultValue[0]);
        end
        else if baseTypClass.InheritsFrom(TBaseFloatSymbol) then
        begin
          paramRec.DefaultValue[0] := VariantToFloat(paramRec.DefaultValue[0]);
        end
        else if baseTypClass.InheritsFrom(TBaseBooleanSymbol) then
        begin
          paramRec.DefaultValue[0] :=
            SameText(paramRec.DefaultValue[0], 'True');
        end
        else if baseTypClass.InheritsFrom(TSetOfSymbol) then
        begin
          ConvertSetOfdefaultValue(paramRec);
        end;
      end;

      paramSymWithDefault := TParamSymbolWithDefaultValue.Create
        (paramRec.ParamName, typSym, paramRec.DefaultValue);
      paramSym := paramSymWithDefault;

    end
    else
    begin

      if paramRec.IsVarParam then
        paramSym := TVarParamSymbol.Create(paramRec.ParamName, typSym, [])
      else if paramRec.IsConstParam then
        paramSym := CreateConstParamSymbol(paramRec.ParamName, typSym)
      else
        paramSym := TParamSymbol.Create(paramRec.ParamName, typSym,
          paramRec.options);

    end;

    addProc(paramSym);

  end;
end;

// GenerateParams
//
procedure TFuncSymbol.GenerateParams(Table: TSymbolTable;
  const funcParams: TParamArray);
begin
  dwsSymbols.GenerateParams(Table, funcParams, AddParam);
end;

// GetParamType
//
function TFuncSymbol.GetParamType(idx: Integer): TTypeSymbol;
begin
  if Cardinal(idx) < Cardinal(params.Count) then
    result := params[idx].typ
  else
    result := nil;
end;

// ParamTypeForbidsImplicitCasts
//
function TFuncSymbol.ParamTypeForbidsImplicitCasts(idx: Integer): Boolean;
begin
  if Cardinal(idx) < Cardinal(params.Count) then
    result := params[idx].ForbidImplicitCasts
  else
    result := False;
end;

// GetCaption
//
function TFuncSymbol.GetCaption: String;
var
  i: Integer;
  nam: String;
  p: TParamSymbol;
  Semantics: TParamSymbolSemantics;
begin
  nam := cFuncKindToString[Kind] + ' ' + Name;

  if params.Count > 0 then
  begin
    result := '(';
    for i := 0 to params.Count - 1 do
    begin
      if i > 0 then
        result := result + ', ';
      p := params[i];
      Semantics := p.Semantics;
      if cParamSymbolSemanticsPrefix[Semantics] <> '' then
        result := result + cParamSymbolSemanticsPrefix[Semantics] + ' ';
      result := result + params[i].typ.Caption;
    end;
    result := result + ')';
  end
  else
    result := '';

  if typ <> nil then
    if typ.Name <> '' then
      result := nam + result + ': ' + typ.Name
    else
      result := nam + result + ': ' + typ.Caption
  else
    result := nam + result;
end;

// GetIsForwarded
//
function TFuncSymbol.IsForwarded: Boolean;
begin
  result := Assigned(FForwardPosition);
end;

// GetDescription
//
function TFuncSymbol.GetDescription: String;
begin
  result := cFuncKindToString[Kind] + ' ' + Name + ParamsDescription;
  if typ <> nil then
    if typ.Name <> '' then
      result := result + ': ' + typ.Name
    else
      result := result + ': ' + typ.Caption;
end;

// Initialize
//
procedure TFuncSymbol.Initialize(const Msgs: TdwsCompileMessageList);
var
  msg: TScriptMessage;
  afa: TdwsAFAAddImplementation;
begin
  inherited;
  if IsExternal then
    Exit;
  FInternalParams.Initialize(Msgs);
  if Assigned(FExecutable) then
    FExecutable.InitSymbol(Self, Msgs)
  else if Level >= 0 then
  begin
    msg := Msgs.AddCompilerErrorFmt(FForwardPosition^,
      CPE_ForwardNotImplemented, [Name]);
    afa := TdwsAFAAddImplementation.Create(msg, AFA_AddImplementation);
    afa.Text := #13#10 + TrimRight(StringReplace(GetDescription, '()', ' ',
      [rfIgnoreCase])) + ';'#13#10'begin'#13#10#9'|'#13#10'end;'#13#10;
  end;
end;

// GetLevel
//
function TFuncSymbol.GetLevel: SmallInt;
begin
  result := FAddrGenerator.Level;
end;

// GetParamSize
//
function TFuncSymbol.GetParamSize: Integer;
begin
  result := FAddrGenerator.DataSize;
end;

// GetIsStateless
//
function TFuncSymbol.GetIsStateless: Boolean;
begin
  result := (fsfStateless in FFlags);
end;

// SetIsStateless
//
procedure TFuncSymbol.SetIsStateless(const val: Boolean);
begin
  if val then
    Include(FFlags, fsfStateless)
  else
    Exclude(FFlags, fsfStateless);
end;

// GetIsExternal
//
function TFuncSymbol.GetIsExternal: Boolean;
begin
  result := (fsfExternal in FFlags);
end;

// SetIsExternal
//
procedure TFuncSymbol.SetIsExternal(const val: Boolean);
begin
  if val then
    Include(FFlags, fsfExternal)
  else
    Exclude(FFlags, fsfExternal);
end;

// GetIsExport
//
function TFuncSymbol.GetIsExport: Boolean;
begin
  result := (fsfExport in FFlags);
end;

// SetIsExport
//
procedure TFuncSymbol.SetIsExport(const val: Boolean);
begin
  if val then
    Include(FFlags, fsfExport)
  else
    Exclude(FFlags, fsfExport);
end;

// GetIsProperty
//
function TFuncSymbol.GetIsProperty: Boolean;
begin
  result := (fsfProperty in FFlags);
end;

// SetIsProperty
//
procedure TFuncSymbol.SetIsProperty(const val: Boolean);
begin
  if val then
    Include(FFlags, fsfProperty)
  else
    Exclude(FFlags, fsfProperty);
end;

// GetIsOverloadedDirect
//
function TFuncSymbol.GetIsOverloadedDirect: Boolean;
begin
  result := (fsfOverloaded in FFlags);
end;

// GetIsOverloaded
//
function TFuncSymbol.GetIsOverloaded: Boolean;
begin
  result := GetIsOverloadedDirect;
end;

// SetIsOverloaded
//
procedure TFuncSymbol.SetIsOverloaded(const val: Boolean);
begin
  if val then
    Include(FFlags, fsfOverloaded)
  else
    Exclude(FFlags, fsfOverloaded);
end;

// GetIsLambda
//
function TFuncSymbol.GetIsLambda: Boolean;
begin
  result := (fsfLambda in FFlags);
end;

// SetIsLambda
//
procedure TFuncSymbol.SetIsLambda(const val: Boolean);
begin
  if val then
    Include(FFlags, fsfLambda)
  else
    Exclude(FFlags, fsfLambda);
end;

// GetIsOfObject
//
function TFuncSymbol.GetIsOfObject: Boolean;
begin
  result := (fsfOfObject in FFlags);
end;

// SetIsOfObject
//
procedure TFuncSymbol.SetIsOfObject(const val: Boolean);
begin
  if val then
    Include(FFlags, fsfOfObject)
  else
    Exclude(FFlags, fsfOfObject);
end;

// GetIsReferenceTo
//
function TFuncSymbol.GetIsReferenceTo: Boolean;
begin
  result := (fsfReferenceTo in FFlags);
end;

// SetIsReferenceTo
//
procedure TFuncSymbol.SetIsReferenceTo(const val: Boolean);
begin
  if val then
    Include(FFlags, fsfReferenceTo)
  else
    Exclude(FFlags, fsfReferenceTo);
end;

// GetIsAsync
//
function TFuncSymbol.GetIsAsync: Boolean;
begin
  result := (fsfAsync in FFlags);
end;

// SetIsAsync
//
procedure TFuncSymbol.SetIsAsync(const val: Boolean);
begin
  if val then
    Include(FFlags, fsfAsync)
  else
    Exclude(FFlags, fsfAsync);
end;

// GetDeclarationPosition
//
function TFuncSymbol.GetDeclarationPosition: TScriptPos;
begin
  result := cNullPos;
end;

// SetDeclarationPosition
//
procedure TFuncSymbol.SetDeclarationPosition(const val: TScriptPos);
begin
  Assert(False);
end;

// GetImplementationPosition
//
function TFuncSymbol.GetImplementationPosition: TScriptPos;
begin
  result := cNullPos;
end;

// SetImplementationPosition
//
procedure TFuncSymbol.SetImplementationPosition(const val: TScriptPos);
begin
  Assert(False);
end;

// GetSourceSubExpr
//
function TFuncSymbol.GetSourceSubExpr(i: Integer): TExprBase;
begin
  result := FExecutable.SubExpr(i);
end;

// GetSourceSubExprCount
//
function TFuncSymbol.GetSourceSubExprCount: Integer;
begin
  if FExecutable <> nil then
    result := FExecutable.SubExprCount
  else
    result := 0;
end;

// IsCompatible
//
function TFuncSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
const
  cCompatibleKinds: array [TFuncKind, TFuncKind] of Boolean =
  // fkFunction, fkProcedure, fkConstructor, fkDestructor, fkMethod, fkLambda
    ((True, False, False, False, True, True), // fkFunction
    (False, True, False, False, True, True), // fkProcedure
    (False, False, True, False, False, False), // fkConstructor
    (False, False, False, True, False, False), // fkDestructor
    (True, True, False, False, True, True), // fkMethod
    (True, True, False, False, True, True)); // fkLambda
var
  funcSym: TFuncSymbol;
  i: Integer;
  param, otherParam: TSymbol;
begin
  if typSym = nil then
    Exit(False);
  typSym := typSym.BaseType;
  if typSym.IsCompatibleWithAnyFuncSymbol then
    result := True
  else
  begin
    result := False;
    funcSym := typSym.AsFuncSymbol;
    if funcSym = nil then
      Exit;
    if params.Count <> funcSym.params.Count then
      Exit;
    if not cCompatibleKinds[Kind, funcSym.Kind] then
      Exit;
    if (typ = funcSym.typ) or (typ.IsOfType(funcSym.typ)) or
      (funcSym.typ is TAnyTypeSymbol) then
    begin
      for i := 0 to params.Count - 1 do
      begin
        param := params[i];
        otherParam := funcSym.params[i];
        if param.ClassType <> otherParam.ClassType then
          Exit;
        if param.typ <> otherParam.typ then
        begin
          if not param.typ.IsCompatible(otherParam.typ) then
            Exit;
          if not otherParam.typ.IsCompatible(param.typ) then
            Exit;
        end;
      end;
      result := True;
    end;
  end;
end;

// DoIsOfType
//
function TFuncSymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
var
  i: Integer;
  funcSym: TFuncSymbol;
begin
  funcSym := typSym.AsFuncSymbol;
  if funcSym = nil then
    Exit(False);

  result := (Kind = funcSym.Kind) and (params.Count = funcSym.params.Count);
  if not result then
    Exit;

  if typ = nil then
  begin
    if funcSym.typ <> nil then
      Exit(False)
  end
  else if not typ.IsCompatible(funcSym.typ) then
    Exit(False);

  for i := 0 to params.Count - 1 do
  begin
    if not params[i].typ.DoIsOfType(funcSym.params[i].typ) then
      Exit(False);
  end;
  result := True;
end;

// InternalSpecialize
//
procedure TFuncSymbol.InternalSpecialize(destination: TFuncSymbol;
  const context: ISpecializationContext);
var
  i, skip: Integer;
  specializedParam: TSymbol;
  param: TParamSymbol;
begin
  if FConditions <> nil then
    context.AddCompilerError
      ('Functions with conditions cannot be specialized yet');

  destination.FFlags := FFlags;
  if fsfExternal in FFlags then
    if HasExternalName then
      destination.ExternalName := FExternalName
    else
      destination.ExternalName := Name
  else
    destination.ExternalName := FExternalName;

  destination.FExternalConvention := FExternalConvention;

  // internal paramps are all pre-specialized
  // but Result is handled separately
  if Self.result <> nil then
    skip := 1
  else
    skip := 0;
  Assert(destination.InternalParams.Count = InternalParams.Count - skip);
  for i := 0 to InternalParams.Count - 1 - skip do
  begin
    specializedParam := destination.InternalParams[i];
    context.RegisterSpecialization(InternalParams[i], specializedParam);
  end;

  // some params can be pre-specialized
  Assert(destination.params.Count <= params.Count);
  for i := 0 to destination.params.Count - 1 do
  begin
    specializedParam := destination.params[i];
    context.RegisterSpecialization(params[i], specializedParam);
  end;

  // specialize remaining parameters
  for i := destination.params.Count to params.Count - 1 do
  begin
    param := params[i];
    specializedParam := param.Specialize(context);
    destination.params.AddSymbol(specializedParam);
    context.RegisterSpecialization(param, specializedParam);
  end;

  destination.typ := context.SpecializeType(typ);
  context.RegisterSpecialization(Self.result, destination.result);

  destination.Executable := context.SpecializeExecutable(FExecutable);
end;

// IsType
//
function TFuncSymbol.IsType: Boolean;
begin
  result := (fsfType in FFlags);
end;

// SetIsType
//
procedure TFuncSymbol.SetIsType;
begin
  Include(FFlags, fsfType);
end;

// GetAsFuncSymbol
//
function TFuncSymbol.GetAsFuncSymbol: TFuncSymbol;
begin
  result := Self;
end;

// SetInline
//
procedure TFuncSymbol.SetInline;
begin
  Include(FFlags, fsfInline);
end;

// InitDataContext
//
procedure TFuncSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetNilInterface(offset);
end;

// AddCondition
//
procedure TFuncSymbol.AddCondition(cond: TConditionSymbol);
begin
  if FConditions = nil then
    FConditions := TConditionsSymbolTable.Create(nil, @FAddrGenerator);
  FConditions.AddSymbol(cond);
end;

// IsValidOverloadOf
//
function TFuncSymbol.IsValidOverloadOf(other: TFuncSymbol): Boolean;
var
  i: Integer;
  n: Integer;
  locParam, otherParam: TParamSymbol;
begin
  // overload is valid if parameter types differ,
  // and there is no ambiguity with default params

  n := Min(params.Count, other.params.Count);

  // check special case of an overload of a parameter-less function
  if (params.Count = 0) and (other.params.Count = 0) then
    Exit(False);

  // check parameters positions defined in both
  for i := 0 to n - 1 do
  begin
    locParam := params[i];
    otherParam := other.params[i];
    if (locParam.ClassType = TParamSymbolWithDefaultValue) and
      (otherParam.ClassType = TParamSymbolWithDefaultValue) then
      Exit(False);
    if not locParam.typ.IsOfType(otherParam.typ) then
      Exit(True);
  end;

  // check that there is at least one remaining param that is not with a default
  if params.Count > n then
    result := (params[n].ClassType <> TParamSymbolWithDefaultValue)
  else if other.params.Count > n then
    result := (other.params[n].ClassType <> TParamSymbolWithDefaultValue)
  else
    result := False;
end;

// IsSameOverloadOf
//
function TFuncSymbol.IsSameOverloadOf(other: TFuncSymbol): Boolean;
var
  i: Integer;
begin
  result := (Kind = other.Kind) and (params.Count = other.params.Count);
  if not result then
    Exit;
  if typ = nil then
    result := (other.typ = nil)
  else
    result := typ.SameType(other.typ);
  if result then
  begin
    for i := 0 to params.Count - 1 do
    begin
      if not params[i].SameParam(other.params[i]) then
      begin
        result := False;
        Break;
      end;
    end;
  end;
end;

// SameType
//
function TFuncSymbol.SameType(typSym: TTypeSymbol): Boolean;
var
  otherFunc: TFuncSymbol;
  i: Integer;
begin
  result := False;
  if (typSym = nil) or (ClassType <> typSym.ClassType) then
    Exit;
  if (typ = nil) xor (typSym.typ = nil) then
    Exit;
  if (typ <> nil) and not typ.SameType(typSym.typ) then
    Exit;

  otherFunc := TFuncSymbol(typSym);
  if params.Count <> otherFunc.params.Count then
    Exit;
  for i := 0 to params.Count - 1 do
    if not params[i].SameParam(otherFunc.params[i]) then
      Exit;

  result := True;
end;

// ParamsDescription
//
function TFuncSymbol.ParamsDescription: String;
begin
  result := params.Description(0);
end;

// SetForwardedPos
//
procedure TFuncSymbol.SetForwardedPos(const aScriptPos: TScriptPos);
begin
  if FForwardPosition = nil then
    New(FForwardPosition);
  FForwardPosition^ := aScriptPos;
end;

// ClearIsForwarded
//
procedure TFuncSymbol.ClearIsForwarded;
begin
  Dispose(FForwardPosition);
  FForwardPosition := nil;
end;

// ------------------
// ------------------ TSourceFuncSymbol ------------------
// ------------------

// GetDeclarationPosition
//
function TSourceFuncSymbol.GetDeclarationPosition: TScriptPos;
begin
  result := FDeclarationPosition;
end;

// SetDeclarationPosition
//
procedure TSourceFuncSymbol.SetDeclarationPosition(const val: TScriptPos);
begin
  FDeclarationPosition := val;
end;

// GetImplementationPosition
//
function TSourceFuncSymbol.GetImplementationPosition: TScriptPos;
begin
  result := FImplementationPosition;
end;

// SetImplementationPosition
//
procedure TSourceFuncSymbol.SetImplementationPosition(const val: TScriptPos);
begin
  FImplementationPosition := val;
end;

// SpecializeType
//
function TSourceFuncSymbol.SpecializeType(const context: ISpecializationContext)
  : TTypeSymbol;
var
  specializedFunc: TSourceFuncSymbol;
begin
  specializedFunc := TSourceFuncSymbol.Create(context.Name, Kind, Level);

  specializedFunc.DeclarationPosition := DeclarationPosition;
  specializedFunc.ImplementationPosition := ImplementationPosition;

  InternalSpecialize(specializedFunc, context);
  result := specializedFunc;

  context.RegisterInternalType(result);
end;

// ------------------
// ------------------ TMethodSymbol ------------------
// ------------------

// Create
//
constructor TMethodSymbol.Create(const Name: String; funcKind: TFuncKind;
  aStructSymbol: TCompositeTypeSymbol; aVisibility: TdwsVisibility;
  aCreateOptions: TMethodCreateOptions; funcLevel: Integer);
begin
  inherited Create(Name, funcKind, funcLevel);
  FStructSymbol := aStructSymbol;
  if mcoClassMethod in aCreateOptions then
    Include(FAttributes, maClassMethod);
  FSelfSym := aStructSymbol.CreateSelfParameter(Self);
  FSize := 1; // wrapped in a interface
  FParams.AddParent(FStructSymbol.Members);
  FVisibility := aVisibility;
  FVMTIndex := -1;
  if aStructSymbol.IsExternal then
    IsExternal := True;
end;

constructor TMethodSymbol.Generate(Table: TSymbolTable; MethKind: TMethodKind;
  const Attributes: TMethodAttributes; const MethName: String;
  const MethParams: TParamArray; const MethType: String;
  Cls: TCompositeTypeSymbol; aVisibility: TdwsVisibility; overloaded: Boolean);
var
  typSym: TTypeSymbol;
  meth: TSymbol;
  enumerator: TPerfectMatchEnumerator;
begin
  // Initialize MethodSymbol
  case MethKind of
    mkConstructor:
      Create(MethName, fkConstructor, Cls, aVisibility, []);
    mkDestructor:
      Create(MethName, fkDestructor, Cls, aVisibility, []);
    mkProcedure:
      Create(MethName, fkProcedure, Cls, aVisibility, []);
    mkFunction:
      Create(MethName, fkFunction, Cls, aVisibility, []);
    mkMethod:
      Create(MethName, fkMethod, Cls, aVisibility, []);
    mkClassProcedure:
      Create(MethName, fkProcedure, Cls, aVisibility, [mcoClassMethod]);
    mkClassFunction:
      Create(MethName, fkFunction, Cls, aVisibility, [mcoClassMethod]);
    mkClassMethod:
      Create(MethName, fkMethod, Cls, aVisibility, [mcoClassMethod]);
  else
    Assert(False);
  end;

  // Set Result type
  if MethType <> '' then
  begin
    if not(Kind in [fkFunction, fkMethod]) then
      raise Exception.Create(CPE_NoResultTypeRequired);

    typSym := Table.FindTypeSymbol(MethType, cvMagic);
    if not Assigned(typSym) then
      raise Exception.CreateFmt(CPE_TypeIsUnknown, [MethType]);
    SetType(typSym);
  end;

  if (Kind = fkFunction) and (MethType = '') then
    raise Exception.Create(CPE_ResultTypeExpected);

  GenerateParams(Table, MethParams);

  // Check if name is already used
  if overloaded or (maOverride in Attributes) then
  begin
    enumerator := TPerfectMatchEnumerator.Create;
    try
      enumerator.funcSym := Self;
      Cls.Members.EnumerateSymbolsOfNameInScope(MethName, enumerator.callback);
      meth := enumerator.Match;
    finally
      enumerator.Free;
    end;
  end
  else
  begin
    meth := Cls.Members.FindSymbol(MethName, cvPrivate);
  end;
  if meth <> nil then
  begin
    if meth is TFieldSymbol then
      raise Exception.CreateFmt(CPE_FieldExists, [MethName])
    else if meth is TPropertySymbol then
      raise Exception.CreateFmt(CPE_PropertyExists, [MethName])
    else if meth is TMethodSymbol then
    begin
      if TMethodSymbol(meth).StructSymbol = Cls then
      begin
        if not overloaded then
          raise EScriptError.CreateFmt(CPE_MethodExists, [MethName])
        else if not TMethodSymbol(meth).IsOverloaded then
          raise EScriptError.CreateFmt(UNT_PreviousNotOverloaded, [MethName])
      end;
    end;
  end;

  if overloaded then
    IsOverloaded := True;
  if Assigned(meth) then
    SetOverlap(TMethodSymbol(meth));

  if Attributes = [maVirtual] then
    IsVirtual := True
  else if Attributes = [maVirtual, maAbstract] then
  begin
    IsVirtual := True;
    IsAbstract := True;
  end
  else if Attributes = [maVirtual, maOverride] then
  begin
    if (not IsOverlap) or (ParentMeth = nil) then
      raise EScriptError.CreateFmt(CPE_CanNotOverride, [Name])
    else if (not ParentMeth.IsVirtual) then
      raise EScriptError.CreateFmt(CPE_CantOverrideNotVirtual, [Name])
    else if ParentMeth.IsFinal then
      raise EScriptError.CreateFmt(CPE_CantOverrideFinal, [Name])
    else
      SetOverride(TMethodSymbol(meth));
  end
  else if Attributes = [maReintroduce] then
    //
  else if IsClassMethod and ((Attributes = [maStatic]) or
    (Attributes = [maStatic, maClassMethod])) then
    SetIsStatic
  else if Attributes = [] then
    //
  else
    raise EScriptError.Create(CPE_InvalidArgCombination);
end;

// GetIsClassMethod
//
function TMethodSymbol.GetIsClassMethod: Boolean;
begin
  result := (maClassMethod in FAttributes);
end;

// GetIsOverride
//
function TMethodSymbol.GetIsOverride: Boolean;
begin
  result := maOverride in FAttributes;
end;

// SetIsOverride
//
procedure TMethodSymbol.SetIsOverride(const val: Boolean);
begin
  if val then
    Include(FAttributes, maOverride)
  else
    Exclude(FAttributes, maOverride);
end;

// GetIsOverlap
//
function TMethodSymbol.GetIsOverlap: Boolean;
begin
  result := maOverlap in FAttributes;
end;

// SetIsOverlap
//
procedure TMethodSymbol.SetIsOverlap(const val: Boolean);
begin
  if val then
    Include(FAttributes, maOverlap)
  else
    Exclude(FAttributes, maOverlap);
end;

// GetIsVirtual
//
function TMethodSymbol.GetIsVirtual: Boolean;
begin
  result := maVirtual in FAttributes;
end;

// SetIsVirtual
//
procedure TMethodSymbol.SetIsVirtual(const val: Boolean);
begin
  if val then
    Include(FAttributes, maVirtual)
  else
    Exclude(FAttributes, maVirtual);
end;

// GetIsAbstract
//
function TMethodSymbol.GetIsAbstract: Boolean;
begin
  result := maAbstract in FAttributes;
end;

// SetIsAbstract
//
procedure TMethodSymbol.SetIsAbstract(const val: Boolean);
begin
  if val then
    Include(FAttributes, maAbstract)
  else
    Exclude(FAttributes, maAbstract);
end;

// GetIsFinal
//
function TMethodSymbol.GetIsFinal: Boolean;
begin
  result := maFinal in FAttributes;
end;

// GetIsInterfaced
//
function TMethodSymbol.GetIsInterfaced: Boolean;
begin
  result := maInterfaced in FAttributes;
end;

// SetIsInterfaced
//
procedure TMethodSymbol.SetIsInterfaced(const val: Boolean);
begin
  if val then
    Include(FAttributes, maInterfaced)
  else
    Exclude(FAttributes, maInterfaced);
end;

// GetIsDefault
//
function TMethodSymbol.GetIsDefault: Boolean;
begin
  result := maDefault in FAttributes;
end;

// SetIsDefault
//
procedure TMethodSymbol.SetIsDefault(const val: Boolean);
begin
  if val then
    Include(FAttributes, maDefault)
  else
    Exclude(FAttributes, maDefault);
end;

// GetIsStatic
//
function TMethodSymbol.GetIsStatic: Boolean;
begin
  result := maStatic in FAttributes;
end;

// GetIgnoreMissingImplementation
//
function TMethodSymbol.GetIgnoreMissingImplementation: Boolean;
begin
  result := maIgnoreMissingImplementation in FAttributes;
end;

// SetIgnoreMissingImplementation
//
procedure TMethodSymbol.SetIgnoreMissingImplementation(const val: Boolean);
begin
  if val then
    Include(FAttributes, maIgnoreMissingImplementation)
  else
    Exclude(FAttributes, maIgnoreMissingImplementation);
end;

// SetIsStatic
//
procedure TMethodSymbol.SetIsStatic;
begin
  Include(FAttributes, maStatic);
  if FSelfSym <> nil then
  begin
    FInternalParams.Remove(FSelfSym);
    FParams.Remove(FSelfSym);
    FSelfSym.Free;
    FSelfSym := nil;
  end;
end;

// SetIsFinal
//
procedure TMethodSymbol.SetIsFinal;
begin
  Include(FAttributes, maFinal);
end;

// GetCaption
//
function TMethodSymbol.GetCaption: String;
begin
  result := inherited GetCaption;
  if IsClassMethod then
    result := 'class ' + result;
end;

// GetDescription
//
function TMethodSymbol.GetDescription: String;
begin
  result := inherited GetDescription;
  if IsClassMethod then
    result := 'class ' + result;
end;

// GetRootParentMeth
//
function TMethodSymbol.GetRootParentMeth: TMethodSymbol;
begin
  result := Self;
  while result.IsOverride do
    result := result.ParentMeth;
end;

// InitDataContext
//
procedure TMethodSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetNilInterface(offset);
  if size = 2 then
    data.SetNilInterface(offset + 1);
end;

// QualifiedName
//
function TMethodSymbol.QualifiedName: String;
begin
  result := String(StructSymbol.QualifiedName + '.' + Name);
end;

// ParamsDescription
//
function TMethodSymbol.ParamsDescription: String;
begin
  if IsStatic or not(StructSymbol is THelperSymbol) then
    result := params.Description(0)
  else
    result := params.Description(1);
end;

// HasConditions
//
function TMethodSymbol.HasConditions: Boolean;
begin
  result := (FConditions <> nil);
  if (not result) and IsOverride and (ParentMeth <> nil) then
    result := ParentMeth.HasConditions;
end;

// IsVisibleFor
//
function TMethodSymbol.IsVisibleFor(const aVisibility: TdwsVisibility): Boolean;
begin
  result := (FVisibility >= aVisibility);
end;

// IsSameOverloadOf
//
function TMethodSymbol.IsSameOverloadOf(other: TFuncSymbol): Boolean;
begin
  result := inherited IsSameOverloadOf(other) and (other is TMethodSymbol) and
    (IsClassMethod = TMethodSymbol(other).IsClassMethod);
end;

// SpecializeType
//
function TMethodSymbol.SpecializeType(const context: ISpecializationContext)
  : TTypeSymbol;
var
  specializedMethod: TMethodSymbol;
  createOptions: TMethodCreateOptions;
begin
  if IsClassMethod then
    createOptions := [mcoClassMethod]
  else
    createOptions := [];

  specializedMethod := TMethodSymbolClass(ClassType)
    .Create(Name, Kind, context.CompositeSymbol, Visibility,
    createOptions, Level);

  if IsStatic then
    specializedMethod.SetIsStatic;
  if IsStateless then
    specializedMethod.SetIsStateless(True);
  InternalSpecialize(specializedMethod, context);
  result := specializedMethod;

  context.RegisterInternalType(result);
end;

// SetOverride
//
procedure TMethodSymbol.SetOverride(meth: TMethodSymbol);
begin
  FParentMeth := meth;
  FVMTIndex := meth.FVMTIndex;
  IsVirtual := True;
  SetIsOverride(True);
  SetIsOverlap(False);
end;

// SetOverlap
//
procedure TMethodSymbol.SetOverlap(meth: TMethodSymbol);
begin
  FParentMeth := meth;
  SetIsOverride(False);
  SetIsOverlap(True);
end;

// ------------------
// ------------------ TSourceMethodSymbol ------------------
// ------------------

// GetDeclarationPosition
//
function TSourceMethodSymbol.GetDeclarationPosition: TScriptPos;
begin
  result := FDeclarationPosition;
end;

// SetDeclarationPosition
//
procedure TSourceMethodSymbol.SetDeclarationPosition(const val: TScriptPos);
begin
  FDeclarationPosition := val;
end;

// GetImplementationPosition
//
function TSourceMethodSymbol.GetImplementationPosition: TScriptPos;
begin
  result := FImplementationPosition;
end;

// SetImplementationPosition
//
procedure TSourceMethodSymbol.SetImplementationPosition(const val: TScriptPos);
begin
  FImplementationPosition := val;
end;

// ------------------
// ------------------ TPropertySymbol ------------------
// ------------------

// Create
//
constructor TPropertySymbol.Create(const Name: String; typ: TTypeSymbol;
  aVisibility: TdwsVisibility; aArrayIndices: TParamsSymbolTable);
begin
  inherited Create(Name, typ);
  FIndexValue := nil;
  FVisibility := aVisibility;
  FArrayIndices := aArrayIndices;
end;

destructor TPropertySymbol.Destroy;
begin
  FArrayIndices.Free;
  FDefaultSym.Free;
  inherited;
end;

// GetArrayIndices
//
function TPropertySymbol.GetArrayIndices: TParamsSymbolTable;
begin
  if FArrayIndices = nil then
    FArrayIndices := TParamsSymbolTable.Create;
  result := FArrayIndices;
end;

// AddParam
//
procedure TPropertySymbol.AddParam(param: TParamSymbol);
begin
  ArrayIndices.AddSymbol(param);
end;

// GetIsDeprecated
//
function TPropertySymbol.GetIsDeprecated: Boolean;
begin
  result := (FDeprecatedMessage <> '');
end;

procedure TPropertySymbol.GenerateParams(Table: TSymbolTable;
  const funcParams: TParamArray);
begin
  dwsSymbols.GenerateParams(Table, funcParams, AddParam);
end;

function TPropertySymbol.GetCaption: String;
begin
  result := GetDescription;
end;

function TPropertySymbol.GetArrayIndicesDescription: String;
var
  i, j: Integer;
  sym, nextSym: TSymbol;
begin
  if (FArrayIndices = nil) or (ArrayIndices.Count = 0) then
    result := ''
  else
  begin
    result := '[';
    i := 0;
    while i < ArrayIndices.Count do
    begin
      sym := ArrayIndices[i];
      if i > 0 then
        result := result + ', ';
      result := result + sym.Name;
      for j := i + 1 to ArrayIndices.Count - 1 do
      begin
        nextSym := ArrayIndices[j];
        if nextSym.typ <> sym.typ then
          Break;
        result := result + ', ' + nextSym.Name;
        i := j;
      end;
      result := result + ': ' + sym.typ.Name;
      Inc(i);
    end;
    result := result + ']';
  end;
end;

// QualifiedName
//
function TPropertySymbol.QualifiedName: String;
begin
  result := String(OwnerSymbol.QualifiedName + '.' + Name);
end;

// IsVisibleFor
//
function TPropertySymbol.IsVisibleFor(const aVisibility
  : TdwsVisibility): Boolean;
begin
  result := (FVisibility >= aVisibility);
end;

// HasArrayIndices
//
function TPropertySymbol.HasArrayIndices: Boolean;
begin
  result := Assigned(FArrayIndices) and (FArrayIndices.Count > 0);
end;

// Specialize
//
function TPropertySymbol.Specialize(const context
  : ISpecializationContext): TSymbol;
var
  i: Integer;
  specializedArrayIndices: TParamsSymbolTable;
  specializedProperty: TPropertySymbol;
begin
  if HasArrayIndices then
  begin
    specializedArrayIndices := TParamsSymbolTable.Create;
    for i := 0 to ArrayIndices.Count - 1 do
      specializedArrayIndices.AddSymbol(ArrayIndices[i].Specialize(context));
  end
  else
    specializedArrayIndices := nil;

  specializedProperty := TPropertySymbol.Create(Name,
    context.SpecializeType(typ), Visibility, specializedArrayIndices);

  specializedProperty.FReadSym := context.Specialize(FReadSym);
  specializedProperty.FWriteSym := context.Specialize(FWriteSym);
  specializedProperty.FIndexSym := context.SpecializeType(FIndexSym);
  specializedProperty.FIndexValue := FIndexValue;
  specializedProperty.FDefaultSym := FDefaultSym;
  if FDefaultSym <> nil then
    FDefaultSym.IncRefCount;
  specializedProperty.FDeprecatedMessage := FDeprecatedMessage;

  result := specializedProperty;
end;

// GetDescription
//
function TPropertySymbol.GetDescription: String;
begin
  result := 'property ' + Name + GetArrayIndicesDescription + ': ' +
    typ.Caption;

  if Assigned(FIndexSym) then
    result := result + ' index ' + VariantToString(FIndexValue[0]);

  if Assigned(FReadSym) then
    if FReadSym.Name <> '' then
      result := result + ' read ' + FReadSym.Name
    else
      result := result + ' read';

  if Assigned(FWriteSym) then
    if FWriteSym.Name <> '' then
      result := result + ' write ' + FWriteSym.Name
    else
      result := result + ' write';

  if IsDefault then
    result := result + '; default;';
end;

// GetIsDefault
//
function TPropertySymbol.GetIsDefault: Boolean;
begin
  result := (OwnerSymbol.DefaultProperty = Self);
end;

procedure TPropertySymbol.SetIndex(const data: IDataContext; sym: TTypeSymbol);
begin
  FIndexSym := sym;
  FIndexValue := TDataContext.CreateStandalone(FIndexSym.size);
  FIndexValue.WriteData(data, FIndexSym.size);
end;

// ------------------
// ------------------ TClassOperatorSymbol ------------------
// ------------------

// Create
//
constructor TClassOperatorSymbol.Create(tokenType: TTokenType);
begin
  inherited Create(cTokenStrings[tokenType], nil);
  FTokenType := tokenType;
end;

// QualifiedName
//
function TClassOperatorSymbol.QualifiedName: String;
begin
  result := String(CompositeSymbol.QualifiedName + '.' + Name);
end;

// GetCaption
//
function TClassOperatorSymbol.GetCaption: String;
begin
  result := 'class operator ' + cTokenStrings[tokenType] + ' ';
  if (UsesSym <> nil) and (UsesSym.params.Count > 0) then
    result := result + UsesSym.params[0].typ.Name
  else
    result := result + '???';
  result := result + ' uses ' + FUsesSym.Name;
end;

// GetDescription
//
function TClassOperatorSymbol.GetDescription: String;
begin
  result := GetCaption;
end;

// ------------------
// ------------------ TClassSymbol ------------------
// ------------------

// Create
//
constructor TClassSymbol.Create(const Name: String; aUnit: TSymbol);
begin
  inherited;
  FSize := 1;
  FMetaSymbol := TClassOfSymbol.Create('class of ' + Name, Self);
end;

// Destroy
//
destructor TClassSymbol.Destroy;
begin
  FOperators.Free;
  FInterfaces.Free;
  inherited;
end;

// GetIsExplicitAbstract
//
function TClassSymbol.GetIsExplicitAbstract: Boolean;
begin
  result := (csfExplicitAbstract in FFlags);
end;

// SetIsExplicitAbstract
//
procedure TClassSymbol.SetIsExplicitAbstract(const val: Boolean);
begin
  if val then
    Include(FFlags, csfExplicitAbstract)
  else
    Exclude(FFlags, csfExplicitAbstract);
end;

// GetIsAbstract
//
function TClassSymbol.GetIsAbstract: Boolean;
begin
  result := (([csfAbstract, csfExplicitAbstract] * FFlags) <> []);
end;

// GetIsSealed
//
function TClassSymbol.GetIsSealed: Boolean;
begin
  result := (csfSealed in FFlags);
end;

// SetIsSealed
//
procedure TClassSymbol.SetIsSealed(const val: Boolean);
begin
  if val then
    Include(FFlags, csfSealed)
  else
    Exclude(FFlags, csfSealed);
end;

// GetIsStatic
//
function TClassSymbol.GetIsStatic: Boolean;
begin
  result := (csfStatic in FFlags);
end;

// SetIsStatic
//
procedure TClassSymbol.SetIsStatic(const val: Boolean);
begin
  if val then
    Include(FFlags, csfStatic)
  else
    Exclude(FFlags, csfStatic);
end;

// GetIsExternal
//
function TClassSymbol.GetIsExternal: Boolean;
begin
  result := (csfExternal in FFlags);
end;

// SetIsExternal
//
procedure TClassSymbol.SetIsExternal(const val: Boolean);
begin
  if val then
    Include(FFlags, csfExternal)
  else
    Exclude(FFlags, csfExternal);
end;

// GetIsExternalRooted
//
function TClassSymbol.GetIsExternalRooted: Boolean;
begin
  result := IsExternal or (csfExternalRooted in FFlags);
end;

// GetIsPartial
//
function TClassSymbol.GetIsPartial: Boolean;
begin
  result := (csfPartial in FFlags);
end;

// GetIsAttribute
//
function TClassSymbol.GetIsAttribute: Boolean;
begin
  result := (csfAttribute in FFlags);
end;

// SetIsAttribute
//
procedure TClassSymbol.SetIsAttribute(const val: Boolean);
begin
  if val then
    Include(FFlags, csfAttribute)
  else
    Exclude(FFlags, csfAttribute);
end;

// GetIsInternal
//
function TClassSymbol.GetIsInternal: Boolean;
begin
  result := (csfInternal in FFlags);
end;

// SetIsInternal
//
procedure TClassSymbol.SetIsInternal(const val: Boolean);
begin
  if val then
    Include(FFlags, csfInternal)
  else
    Exclude(FFlags, csfInternal);
end;

// SetIsPartial
//
procedure TClassSymbol.SetIsPartial;
begin
  Include(FFlags, csfPartial);
end;

// SetNoVirtualMembers
//
procedure TClassSymbol.SetNoVirtualMembers;
begin
  Include(FFlags, csfNoVirtualMembers);
end;

// SetNoOverloads
//
procedure TClassSymbol.SetNoOverloads;
begin
  Include(FFlags, csfNoOverloads);
end;

// AddField
//
procedure TClassSymbol.AddField(fieldSym: TFieldSymbol);
begin
  inherited;
  fieldSym.FOffset := FScriptInstanceSize;
  FScriptInstanceSize := FScriptInstanceSize + fieldSym.typ.size;
  Include(FFlags, csfHasOwnFields);
end;

// AddMethod
//
procedure TClassSymbol.AddMethod(methSym: TMethodSymbol);
begin
  inherited;
  if methSym.IsAbstract then
    Include(FFlags, csfAbstract);
  Include(FFlags, csfHasOwnMethods);
end;

// AddOperator
//
procedure TClassSymbol.AddOperator(sym: TClassOperatorSymbol);
begin
  sym.CompositeSymbol := Self;
  FMembers.AddSymbol(sym);
  FOperators.Add(sym);
end;

// AddInterface
//
function TClassSymbol.AddInterface(intfSym: TInterfaceSymbol;
  Visibility: TdwsVisibility; var missingMethod: TMethodSymbol): Boolean;
var
  sym: TSymbol;
  iter: TInterfaceSymbol;
  resolved: TResolvedInterface;
  lookup, Match: TMethodSymbol;
begin
  resolved.IntfSymbol := intfSym;
  SetLength(resolved.VMT, intfSym.MethodCount);
  iter := intfSym;
  while iter <> nil do
  begin
    for sym in iter.Members do
    begin
      if sym.Name = '' then
        continue;
      if sym is TMethodSymbol then
      begin
        lookup := TMethodSymbol(sym);
        Match := DuckTypedMatchingMethod(lookup, Visibility);
        if Match = nil then
        begin
          missingMethod := lookup;
          Exit(False);
        end
        else
        begin
          resolved.VMT[lookup.VMTIndex] := Match;
          Match.IsInterfaced := True;
        end;
      end;
    end;
    iter := iter.parent;
  end;

  if FInterfaces = nil then
    FInterfaces := TResolvedInterfaces.Create;
  FInterfaces.Add(resolved);
  missingMethod := nil;
  result := True;
end;

// ProcessOverriddenInterface
//
function TClassSymbol.ProcessOverriddenInterface(const ancestorResolved
  : TResolvedInterface): Boolean;
var
  i: Integer;
  newResolved: TResolvedInterface;
  meth: TMethodSymbol;
begin
  result := False;
  newResolved := ancestorResolved;
  if (FInterfaces <> nil) and FInterfaces.Contains(newResolved) then
    Exit;
  SetLength(newResolved.VMT, Length(newResolved.VMT)); // make unique
  for i := 0 to High(newResolved.VMT) do
  begin
    meth := newResolved.VMT[i];
    if meth.IsVirtual then
    begin
      if FVirtualMethodTable[meth.VMTIndex] <> meth then
      begin
        newResolved.VMT[i] := FVirtualMethodTable[meth.VMTIndex];
        result := True;
      end;
    end;
  end;
  if result then
  begin
    if FInterfaces = nil then
      FInterfaces := TResolvedInterfaces.Create;
    FInterfaces.Add(newResolved);
  end;
end;

// ProcessOverriddenInterfaces
//
procedure TClassSymbol.ProcessOverriddenInterfaces;
var
  iter: TClassSymbol;
  loopProtection: TList;
  ri: TResolvedInterfaces;
begin
  iter := parent;
  loopProtection := TList.Create;
  try
    while iter <> nil do
    begin
      if loopProtection.IndexOf(iter) > 0 then
        Break;
      loopProtection.Add(iter);
      ri := iter.Interfaces;
      if ri <> nil then
      begin
        ri.Enumerate(ProcessOverriddenInterfaceCallback);
      end;
      iter := iter.parent;
    end;
  finally
    loopProtection.Free;
  end;
end;

// ProcessOverriddenInterfaceCallback
//
function TClassSymbol.ProcessOverriddenInterfaceCallback
  (const item: TResolvedInterface): TSimpleHashAction;
begin
  ProcessOverriddenInterface(item);
  result := shaNone;
end;

// ResolveInterface
//
function TClassSymbol.ResolveInterface(intfSym: TInterfaceSymbol;
  var resolved: TResolvedInterface): Boolean;
begin
  if FInterfaces <> nil then
  begin;
    resolved.IntfSymbol := intfSym;
    result := FInterfaces.Match(resolved);
    if result then
      Exit;
  end;
  if parent <> nil then
    result := parent.ResolveInterface(intfSym, resolved)
  else
    result := False;
end;

// ImplementsInterface
//
function TClassSymbol.ImplementsInterface(intfSym: TInterfaceSymbol): Boolean;
var
  resolved: TResolvedInterface;
begin
  result := ResolveInterface(intfSym, resolved);
end;

// FieldAtOffset
//
function TClassSymbol.FieldAtOffset(offset: Integer): TFieldSymbol;
begin
  result := inherited FieldAtOffset(offset);
  if result = nil then
  begin
    if parent <> nil then
      result := parent.FieldAtOffset(offset);
  end;
end;

// InitDataContext
//
procedure TClassSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetNilInterface(offset);
end;

// Initialize
//
procedure TClassSymbol.Initialize(const Msgs: TdwsCompileMessageList);
var
  i, a, v: Integer;
  differentVMT: Boolean;
  sym: TSymbol;
  field: TFieldSymbol;
  meth: TMethodSymbol;
begin
  if csfInitialized in Flags then
    Exit;
  Include(FFlags, csfInitialized);

  // Check validity of the class declaration
  if IsForwarded then
  begin
    Msgs.AddCompilerErrorFmt(FForwardPosition^,
      CPE_ClassNotCompletelyDefined, [Name]);
    Exit;
  end;

  if parent <> nil then
  begin
    parent.Initialize(Msgs);
    a := parent.ScriptInstanceSize;
    FVirtualMethodTable := parent.FVirtualMethodTable;
  end
  else
  begin
    a := 0;
    FVirtualMethodTable := nil;
  end;
  v := Length(FVirtualMethodTable);
  differentVMT := False;

  // remap field offset & vmt index (cares for partial classes)
  for i := 0 to FMembers.Count - 1 do
  begin
    sym := FMembers[i];
    if sym.ClassType = TFieldSymbol then
    begin
      field := TFieldSymbol(sym);
      field.FOffset := a;
      Inc(a, field.typ.size);
    end
    else if sym is TMethodSymbol then
    begin
      meth := TMethodSymbol(sym);
      if meth.IsVirtual then
      begin
        differentVMT := True;
        if meth.IsOverride then
          meth.FVMTIndex := meth.ParentMeth.VMTIndex
        else
        begin
          meth.FVMTIndex := v;
          Inc(v);
        end;
      end;
    end;
  end;
  FScriptInstanceSize := a;
  // prepare VMT
  if differentVMT then
  begin
    SetLength(FVirtualMethodTable, v); // make unique (and resize if necessary)
    for sym in FMembers do
    begin
      if sym is TMethodSymbol then
      begin
        meth := TMethodSymbol(sym);
        if meth.IsVirtual and (meth.FVMTIndex >= 0) then
        begin
          Assert(meth.FVMTIndex < v);
          FVirtualMethodTable[meth.FVMTIndex] := meth;
        end;
      end;
    end;
  end;
  // update abstract flag
  if csfAbstract in FFlags then
  begin
    if differentVMT then
    begin
      Exclude(FFlags, csfAbstract);
      for i := 0 to High(FVirtualMethodTable) do
      begin
        if FVirtualMethodTable[i].IsAbstract then
        begin
          Include(FFlags, csfAbstract);
          Break;
        end;
      end;
    end
    else if not(csfAbstract in parent.FFlags) then
      Exclude(FFlags, csfAbstract);
  end;
  // process overridden interfaces
  ProcessOverriddenInterfaces;

  CheckMethodsImplemented(Msgs);
end;

// InheritFrom
//
procedure TClassSymbol.InheritFrom(ancestorClassSym: TClassSymbol);
begin
  DoInheritFrom(ancestorClassSym);

  if csfAbstract in ancestorClassSym.FFlags then
    Include(FFlags, csfAbstract);
  FScriptInstanceSize := ancestorClassSym.ScriptInstanceSize;

  IsStatic := IsStatic or ancestorClassSym.IsStatic;

  if ancestorClassSym.IsAttribute then
    Include(FFlags, csfAttribute);

  if [csfExternalRooted, csfExternal] * ancestorClassSym.Flags <> [] then
    Include(FFlags, csfExternalRooted);

  if csfNoVirtualMembers in ancestorClassSym.FFlags then
    SetNoVirtualMembers;
  if (csfNoOverloads in ancestorClassSym.FFlags) or
    ((csfExternalRooted in FFlags) and not(csfExternal in FFlags)) then
    SetNoOverloads;
end;

// IsCompatible
//
function TClassSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  if typSym = nil then
    result := False
  else
  begin
    typSym := typSym.UnAliasedType;
    if typSym is TNilSymbol then
      result := True
    else if typSym.IsClassSymbol then
      result := (NthParentOf(TClassSymbol(typSym)) >= 0)
    else
      result := False;
  end;
end;

// GetIsClassSymbol
//
function TClassSymbol.GetIsClassSymbol: Boolean;
begin
  result := True;
end;

// Taxonomy
//
function TClassSymbol.Taxonomy: TdwsSymbolTaxonomy;
begin
  result := [stTypeSymbol, stClassSymbol];
end;

// IsPointerType
//
function TClassSymbol.IsPointerType: Boolean;
begin
  result := True;
end;

// HasMetaSymbol
//
function TClassSymbol.HasMetaSymbol: Boolean;
begin
  result := True;
end;

// CommonAncestor
//
function TClassSymbol.CommonAncestor(otherClass: TTypeSymbol): TClassSymbol;
begin
  result := Self;
  while (result <> nil) and not otherClass.IsOfType(result) do
    result := result.parent;
end;

// DoIsOfType
//
function TClassSymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
begin
  result := (Self = typSym.UnAliasedType);
  if result or (Self = nil) then
    Exit;
  if parent <> nil then
    result := parent.DoIsOfType(typSym.UnAliasedType)
  else
    result := False;
end;

// VMTMethod
//
function TClassSymbol.VMTMethod(Index: Integer): TMethodSymbol;
begin
  if Cardinal(index) < Cardinal(Length(FVirtualMethodTable)) then
    result := FVirtualMethodTable[index]
  else
    result := nil;
end;

// VMTCount
//
function TClassSymbol.VMTCount: Integer;
begin
  result := Length(FVirtualMethodTable);
end;

function TClassSymbol.GetDescription: String;
var
  i: Integer;
begin
  if FParent <> nil then
    result := Name + ' = class (' + FParent.Name + ')'#13#10
  else
    result := Name + ' = class'#13#10;

  for i := 0 to Members.Count - 1 do
    result := result + '   ' + Members.Symbols[i].Description + ';'#13#10;

  result := result + 'end';
end;

// FindClassOperatorStrict
//
function TClassSymbol.FindClassOperatorStrict(tokenType: TTokenType;
  ParamType: TSymbol; recursive: Boolean): TClassOperatorSymbol;
var
  i: Integer;
begin
  for i := 0 to FOperators.Count - 1 do
  begin
    result := TClassOperatorSymbol(FOperators.List[i]);
    if (result.tokenType = tokenType) and (result.typ = ParamType) then
      Exit;
  end;
  if recursive and (parent <> nil) then
    result := parent.FindClassOperatorStrict(tokenType, ParamType, True)
  else
    result := nil;
end;

// FindClassOperator
//
function TClassSymbol.FindClassOperator(tokenType: TTokenType;
  ParamType: TTypeSymbol): TClassOperatorSymbol;
var
  i: Integer;
begin
  result := FindClassOperatorStrict(tokenType, ParamType, False);
  if result <> nil then
    Exit;

  if FOperators.Count > 0 then
  begin
    for i := 0 to FOperators.Count - 1 do
    begin
      result := TClassOperatorSymbol(FOperators.List[i]);
      if (result.tokenType = tokenType) and ParamType.DoIsOfType(result.typ)
      then
        Exit;
    end;
    for i := 0 to FOperators.Count - 1 do
    begin
      result := TClassOperatorSymbol(FOperators.List[i]);
      if (result.tokenType = tokenType) and result.typ.IsCompatible(ParamType)
      then
        Exit;
    end;
  end;
  if parent <> nil then
    result := parent.FindClassOperator(tokenType, ParamType)
  else
    result := nil;
end;

// FindDefaultConstructor
//
function TClassSymbol.FindDefaultConstructor(minVisibility: TdwsVisibility)
  : TMethodSymbol;
var
  i: Integer;
  member: TSymbol;
  createConstructor: TMethodSymbol;
begin
  createConstructor := nil;
  for i := 0 to FMembers.Count - 1 do
  begin
    member := FMembers[i];
    if member is TMethodSymbol then
    begin
      result := TMethodSymbol(member);
      if (result.Visibility >= minVisibility) and (result.Kind = fkConstructor)
      then
      begin
        if result.IsDefault then
          Exit;
        if UnicodeSameText(result.Name, 'Create') then
          createConstructor := result;
      end;
    end;
  end;
  if createConstructor <> nil then
    result := createConstructor
  else if parent <> nil then
  begin
    if minVisibility = cvPrivate then
      minVisibility := cvProtected;
    result := parent.FindDefaultConstructor(minVisibility);
  end
  else
    result := nil;
end;

// CollectPublishedSymbols
//
procedure TClassSymbol.CollectPublishedSymbols(symbolList: TSimpleSymbolList);
var
  i: Integer;
  member: TSymbol;
begin
  for i := 0 to Members.Count - 1 do
  begin
    member := Members[i];
    if member.ClassType = TPropertySymbol then
    begin
      if TPropertySymbol(member).Visibility = cvPublished then
        symbolList.Add(member);
    end
    else if member.ClassType = TFieldSymbol then
    begin
      if TFieldSymbol(member).Visibility = cvPublished then
        symbolList.Add(member);
    end
    else if member.InheritsFrom(TMethodSymbol) then
    begin
      if TMethodSymbol(member).Visibility = cvPublished then
        symbolList.Add(member);
    end;
  end;
end;

// AllowVirtualMembers
//
function TClassSymbol.AllowVirtualMembers: Boolean;
begin
  result := not(csfNoVirtualMembers in FFlags);
end;

// AllowOverloads
//
function TClassSymbol.AllowOverloads: Boolean;
begin
  result := not(csfNoOverloads in FFlags);
end;

// AllowFields
//
function TClassSymbol.AllowFields: Boolean;
begin
  result := True;
end;

// AllowAnonymousMethods
//
function TClassSymbol.AllowAnonymousMethods: Boolean;
begin
  result := (not IsExternal);
end;

// CreateSelfParameter
//
function TClassSymbol.CreateSelfParameter(methSym: TMethodSymbol): TDataSymbol;
begin
  if methSym.IsClassMethod then
    result := TSelfSymbol.Create(SYS_SELF, MetaSymbol)
  else
    result := TSelfSymbol.Create(SYS_SELF, Self);
  methSym.InternalParams.AddSymbol(result);
end;

// CreateAnonymousMethod
//
function TClassSymbol.CreateAnonymousMethod(aFuncKind: TFuncKind;
  aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
  : TMethodSymbol;
begin
  result := TSourceMethodSymbol.Create('', aFuncKind, Self, aVisibility,
    aCreateOptions);
end;

// VisibilityToString
//
class function TClassSymbol.VisibilityToString
  (Visibility: TdwsVisibility): String;
const
  cVisibilityNames: array [TdwsVisibility] of String = ('magic', 'private',
    'protected', 'public', 'published');
begin
  result := cVisibilityNames[Visibility];
end;

// SpecializeType
//
function TClassSymbol.SpecializeType(const context: ISpecializationContext)
  : TTypeSymbol;
var
  specializedClass: TClassSymbol;
begin
  if csfPartial in FFlags then
    context.AddCompilerError(CPE_PartialClassesCannotBeSpecialized);

  // temporary errors while generic support is in progress, so no standard error string
  if not(csfExternal in FFlags) then
  begin
    if parent.IsGeneric then
      context.AddCompilerError
        ('Subclasses of a generic class connt be specialized right now')
    else if FOperators.Count > 0 then
      context.AddCompilerError
        ('Classes with operators cannot be specialized right now');
  end;
  if FOperators.Count <> 0 then
    context.AddCompilerError
      ('Specialization of class operators not yet supported');
  if (FInterfaces <> nil) and (FInterfaces.Count <> 0) then
    context.AddCompilerError
      ('Specialization of classes with interfaces not yet supported');
  if Assigned(FOnObjectDestroy) then
    context.AddCompilerError
      ('Specialization of classes with custom destructor not yet supported');

  specializedClass := TClassSymbol.Create(context.Name, context.UnitSymbol);

  context.EnterComposite(specializedClass);
  try
    specializedClass.FFlags := FFlags - [csfInitialized];
    if csfExternal in FFlags then
      if FExternalName <> '' then
        specializedClass.FExternalName := FExternalName
      else
        specializedClass.FExternalName := Name
    else
      specializedClass.FExternalName := FExternalName;

    if parent <> nil then
      specializedClass.InheritFrom(parent);

    if context.GenericSymbolType = Self then
      context.RegisterSpecialization(Self, specializedClass);

    SpecializeMembers(specializedClass, context);
    specializedClass.Initialize(context.Msgs);
  finally
    context.LeaveComposite;
  end;
  result := specializedClass;
end;

// Parent
//
function TClassSymbol.parent: TClassSymbol;
begin
  result := TClassSymbol(FParent);
end;

// IsPureStatic
//
function TClassSymbol.IsPureStatic: Boolean;
var
  sym: TSymbol;
  symClass: TClass;
  meth: TMethodSymbol;
begin
  result := IsStatic and IsSealed;
  if not result then
    Exit;

  for sym in FMembers do
  begin
    symClass := sym.ClassType;
    if symClass = TFieldSymbol then
      Exit;
    if symClass = TClassConstSymbol then
      continue;
    if symClass = TClassVarSymbol then
      continue;
    if symClass = TPropertySymbol then
      continue;
    if symClass.InheritsFrom(TMethodSymbol) then
    begin
      meth := TMethodSymbol(symClass);
      if not meth.IsStatic then
        Exit;
      if not meth.IsClassMethod then
        Exit;
    end;
    Exit;
  end;
  result := True;
end;

// ------------------
// ------------------ TNilSymbol ------------------
// ------------------

constructor TNilSymbol.Create;
begin
  inherited Create('<nil>', nil);
  FSize := 1;
end;

function TNilSymbol.GetCaption: String;
begin
  result := 'nil';
end;

function TNilSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  typSym := typSym.BaseType;
  result := typSym.IsClassSymbol or (typSym is TNilSymbol);
end;

// IsCompatibleWithAnyFuncSymbol
//
function TNilSymbol.IsCompatibleWithAnyFuncSymbol: Boolean;
begin
  result := True;
end;

// InitDataContext
//
procedure TNilSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetNilInterface(offset);
end;

// SpecializeType
//
function TNilSymbol.SpecializeType(const context: ISpecializationContext)
  : TTypeSymbol;
begin
  result := Self;
end;

// ------------------
// ------------------ TClassOfSymbol ------------------
// ------------------

constructor TClassOfSymbol.Create(const Name: String; typ: TClassSymbol);
begin
  inherited Create(Name, typ);
end;

function TClassOfSymbol.GetCaption: String;
begin
  if Name <> '' then
    result := Name
  else
    result := GetDescription;
end;

// GetDescription
//
function TClassOfSymbol.GetDescription: String;
begin
  if typ <> nil then
    result := 'class of ' + typ.Name
  else
    result := 'class of ???';
end;

function TClassOfSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  typSym := typSym.BaseType;
  result := (typSym is TNilSymbol) or
    ((typSym is TClassOfSymbol) and typ.IsCompatible(typSym.typ));
end;

// SameType
//
function TClassOfSymbol.SameType(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym <> nil) and (typSym.ClassType = TClassOfSymbol) and
    (typ.SameType(typSym.typ));
end;

// DoIsOfType
//
function TClassOfSymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
begin
  if typSym is TClassOfSymbol then
    result := typ.DoIsOfType(typSym.typ.UnAliasedType)
  else
    result := False;
end;

// TypClassSymbol
//
function TClassOfSymbol.TypClassSymbol: TClassSymbol;
begin
  result := TClassSymbol(typ);
end;

// ------------------
// ------------------ TBaseSymbol ------------------
// ------------------

// Create
//
constructor TBaseSymbol.Create(const Name: String);
begin
  inherited Create(name, nil);
  FSize := 1;
end;

// IsCompatible
//
function TBaseSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym <> nil) and (UnAliasedType = typSym.UnAliasedType);
end;

// IsBaseType
//
class function TBaseSymbol.IsBaseType: Boolean;
begin
  result := True;
end;

// SpecializeType
//
function TBaseSymbol.SpecializeType(const context: ISpecializationContext)
  : TTypeSymbol;
begin
  result := Self;
end;

// ------------------
// ------------------ TBaseIntegerSymbol ------------------
// ------------------

// Create
//
constructor TBaseIntegerSymbol.Create;
begin
  inherited Create(SYS_INTEGER);
end;

// InitDataContext
//
procedure TBaseIntegerSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetZeroInt64(offset);
end;

// IsCompatible
//
function TBaseIntegerSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  if typSym <> nil then
  begin
    result := (UnAliasedType = typSym.UnAliasedType) or
      ((typSym.ClassType = TEnumerationSymbol) and IsCompatible(typSym.typ));
  end
  else
    result := False;
end;

// ------------------
// ------------------ TBaseFloatSymbol ------------------
// ------------------

// Create
//
constructor TBaseFloatSymbol.Create;
begin
  inherited Create(SYS_FLOAT);
end;

// InitDataContext
//
procedure TBaseFloatSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetZeroFloat(offset);
end;

// ------------------
// ------------------ TBaseStringSymbol ------------------
// ------------------

// Create
//
constructor TBaseStringSymbol.Create;
begin
  inherited Create(SYS_STRING);
end;

// Destroy
//
destructor TBaseStringSymbol.Destroy;
begin
  inherited;
  FLengthPseudoSymbol.Free;
  FHighPseudoSymbol.Free;
  FLowPseudoSymbol.Free;
end;

// InitDataContext
//
procedure TBaseStringSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetEmptyString(offset);
end;

// InitString
//
procedure TBaseStringSymbol.InitString(var s: String);
begin
  s := '';
end;

// InitPseudoSymbol
//
function TBaseStringSymbol.InitPseudoSymbol(var p: TPseudoMethodSymbol;
  sk: TSpecialKeywordKind; BaseSymbols: TdwsBaseSymbolsContext)
  : TPseudoMethodSymbol;

  function DoInit(var p: TPseudoMethodSymbol; sk: TSpecialKeywordKind;
    BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol;
  begin
    p := TPseudoMethodSymbol.Create(Self, cSpecialKeywords[sk], fkFunction, 0);
    p.typ := BaseSymbols.TypInteger;
    result := p;
  end;

begin
  result := p;
  if result = nil then
    result := DoInit(p, sk, BaseSymbols);
end;

// LengthPseudoSymbol
//
function TBaseStringSymbol.LengthPseudoSymbol(BaseSymbols
  : TdwsBaseSymbolsContext): TPseudoMethodSymbol;
begin
  result := InitPseudoSymbol(FLengthPseudoSymbol, skLength, BaseSymbols);
end;

// HighPseudoSymbol
//
function TBaseStringSymbol.HighPseudoSymbol(BaseSymbols: TdwsBaseSymbolsContext)
  : TPseudoMethodSymbol;
begin
  result := InitPseudoSymbol(FHighPseudoSymbol, skHigh, BaseSymbols);
end;

// LowPseudoSymbol
//
function TBaseStringSymbol.LowPseudoSymbol(BaseSymbols: TdwsBaseSymbolsContext)
  : TPseudoMethodSymbol;
begin
  result := InitPseudoSymbol(FLowPseudoSymbol, skLow, BaseSymbols);
end;

// ------------------
// ------------------ TBaseBooleanSymbol ------------------
// ------------------

// Create
//
constructor TBaseBooleanSymbol.Create;
begin
  inherited Create(SYS_BOOLEAN);
end;

// InitDataContext
//
procedure TBaseBooleanSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetFalseBoolean(offset);
end;

// ------------------
// ------------------ TBaseVariantSymbol ------------------
// ------------------

// Create
//
constructor TBaseVariantSymbol.Create(const Name: String = '');
begin
  if name = '' then
    inherited Create(SYS_VARIANT)
  else
    inherited Create(name);
end;

// IsCompatible
//
function TBaseVariantSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
var
  ct: TClass;
begin
  if typSym <> nil then
  begin
    typSym := typSym.GetUnAliasedType;
    if typSym.InheritsFrom(TBaseSymbol) then
      result := True
    else
    begin
      ct := typSym.ClassType;
      result := (ct = TEnumerationSymbol) or (ct = TClassSymbol) or
        (ct = TNilSymbol) or (ct = TInterfaceSymbol);
    end;
  end
  else
    result := False;
end;

// InitDataContext
//
procedure TBaseVariantSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.SetEmptyVariant(offset);
end;

// SupportsEmptyParam
//
function TBaseVariantSymbol.SupportsEmptyParam: Boolean;
begin
  result := True;
end;

// ------------------
// ------------------ TParamsSymbolTable ------------------
// ------------------

// GetSymbol
//
function TParamsSymbolTable.GetSymbol(x: Integer): TParamSymbol;
begin
  result := TParamSymbol(inherited Symbols[x]);
  Assert(result is TParamSymbol);
end;

// Description
//
function TParamsSymbolTable.Description(skip: Integer): String;
var
  i: Integer;
begin
  if Count > skip then
  begin
    result := Symbols[skip].Description;
    for i := skip + 1 to Count - 1 do
      result := result + '; ' + Symbols[i].Description;
    result := '(' + result + ')';
  end
  else
    result := '()';
end;

// ------------------
// ------------------ TValueSymbol ------------------
// ------------------

// Create
//
constructor TValueSymbol.Create(const aName: String; aType: TTypeSymbol);
begin
  inherited;
  FName := aName;
  FTyp := aType;
  FSize := aType.size;
end;

// Taxonomy
//
function TValueSymbol.Taxonomy: TdwsSymbolTaxonomy;
begin
  result := [stValueSymbol];
end;

function TValueSymbol.GetCaption: String;
begin
  result := Name + ': ' + typ.Caption;
end;

function TValueSymbol.GetDescription: String;
begin
  result := Name + ': ' + typ.Caption;
end;

// ------------------
// ------------------ TConstSymbol ------------------
// ------------------

// Create
//
constructor TConstSymbol.Create(const Name: String; typ: TTypeSymbol);
begin
  inherited Create(name, typ);
  FDataContext := TDataContext.CreateStandalone(typ.size);
end;

// CreateValue
//
constructor TConstSymbol.CreateValue(const Name: String; typ: TTypeSymbol;
  const value: Variant);
begin
  inherited Create(Name, typ);
  Assert(typ.size = 1);
  FDataContext := TDataContext.CreateStandalone(1);
  FDataContext.AsVariant[0] := value;
end;

// CreateData
//
constructor TConstSymbol.CreateData(const Name: String; typ: TTypeSymbol;
  const data: IDataContext);
begin
  inherited Create(Name, typ);
  FDataContext := data;
end;

// Taxonomy
//
function TConstSymbol.Taxonomy: TdwsSymbolTaxonomy;
begin
  result := [stValueSymbol, stConstSymbol];
end;

// GetCaption
//
function TConstSymbol.GetCaption: String;
begin
  result := 'const ' + inherited GetCaption;
end;

function TConstSymbol.GetDescription: String;

  function EncodeString: String;
  var
    nbApos, nbQuotes: Integer;
  begin
    var
    uTyp := typ.UnAliasedType;
    if uTyp is TSetOfSymbol then
      Exit(TSetOfSymbol(uTyp).GetValueDescription(FDataContext));

    FDataContext.EvalAsString(0, result);
    if uTyp is TBaseStringSymbol then
    begin
      nbApos := StrCountChar(result, '''');
      if nbApos = 0 then
        nbQuotes := 1
      else
        nbQuotes := StrCountChar(result, '"');
      if nbApos < nbQuotes then
      begin
        if nbApos > 0 then
          result := '''' + StringReplace(result, '''', '''''',
            [rfReplaceAll]) + ''''
        else
          result := '''' + result + '''';
      end
      else
      begin
        if nbQuotes > 0 then
          result := '"' + StringReplace(result, '"', '""', [rfReplaceAll]) + '"'
        else
          result := '"' + result + '"';
      end;
    end;
  end;

begin
  result := 'const ' + inherited GetDescription + ' = ';
  if typ.size > 0 then
    result := result + EncodeString
  else
    result := result + '???';
end;

// GetIsDeprecated
//
function TConstSymbol.GetIsDeprecated: Boolean;
begin
  result := (FDeprecatedMessage <> '');
end;

procedure TConstSymbol.Initialize(const Msgs: TdwsCompileMessageList);
begin
end;

// ------------------
// ------------------ TDataSymbol ------------------
// ------------------

function TDataSymbol.GetDescription: String;
begin
  if Assigned(typ) then
    if typ.Name <> '' then
      result := Name + ': ' + typ.Name
    else
      result := Name + ': ' + typ.Description
  else
    result := Name + ': ???';
end;

// AllocateStackAddr
//
procedure TDataSymbol.AllocateStackAddr(generator: TAddrGenerator);
begin
  FLevel := generator.Level;
  FStackAddr := generator.GetStackAddr(size);
end;

// Taxonomy
//
function TDataSymbol.Taxonomy: TdwsSymbolTaxonomy;
begin
  result := [stValueSymbol, stDataSymbol];
end;

// GetIsDataSymbol
//
function TDataSymbol.GetIsDataSymbol: Boolean;
begin
  result := True;
end;

// IsWritable
//
function TDataSymbol.IsWritable: Boolean;
begin
  result := True;
end;

// GetPurpose
//
function TDataSymbol.GetPurpose: TScriptDataSymbolPurpose;
begin
  result := sdspGeneral;
end;

// ------------------
// ------------------ TScriptDataSymbol ------------------
// ------------------

// Create
//
constructor TScriptDataSymbol.Create(const aName: String; aType: TTypeSymbol;
  aPurpose: TScriptDataSymbolPurpose = sdspGeneral);
begin
  inherited Create(aName, aType);
  Purpose := aPurpose;
end;

// Specialize
//
function TScriptDataSymbol.Specialize(const context
  : ISpecializationContext): TSymbol;
begin
  result := TScriptDataSymbol.Create(Name, context.SpecializeType(typ),
    Purpose);
end;

// GetPurpose
//
function TScriptDataSymbol.GetPurpose: TScriptDataSymbolPurpose;
begin
  result := Purpose;
end;

// ------------------
// ------------------ TVarDataSymbol ------------------
// ------------------

// Specialize
//
function TVarDataSymbol.Specialize(const context
  : ISpecializationContext): TSymbol;
begin
  result := TScriptDataSymbol.Create(Name, context.SpecializeType(typ));
end;

// ------------------
// ------------------ TParamSymbol ------------------
// ------------------

// Create
//
constructor TParamSymbol.Create(const aName: String; aType: TTypeSymbol;
  options: TParamSymbolOptions = []);
begin
  inherited Create(aName, aType);
  FOptions := options;
end;

// SameParam
//
function TParamSymbol.SameParam(other: TParamSymbol): Boolean;
begin
  result := ((ClassType = other.ClassType) or ((ClassType = TParamSymbol) and
    (other.ClassType = TParamSymbolWithDefaultValue))) and
    typ.SameType(other.typ) and UnicodeSameText(Name, other.Name) and
    (FOptions = other.FOptions);
end;

// Semantics
//
function TParamSymbol.Semantics: TParamSymbolSemantics;
begin
  result := pssCopy;
end;

// ForbidImplicitCasts
//
function TParamSymbol.ForbidImplicitCasts: Boolean;
begin
  result := psoForbidImplicitCasts in FOptions;
end;

// IsInternal
//
function TParamSymbol.IsInternal: Boolean;
begin
  result := psoInternal in FOptions;
end;

// Clone
//
function TParamSymbol.Clone: TParamSymbol;
begin
  result := TParamSymbol.Create(Name, typ);
end;

// Specialize
//
function TParamSymbol.Specialize(const context: ISpecializationContext)
  : TSymbol;
begin
  result := TParamSymbol.Create(Name, context.SpecializeType(typ));
end;

// GetDescription
//
function TParamSymbol.GetDescription: String;
var
  Semantics: TParamSymbolSemantics;
begin
  Semantics := Self.Semantics;
  if cParamSymbolSemanticsPrefix[Semantics] = '' then
    result := inherited GetDescription
  else
    result := cParamSymbolSemanticsPrefix[Semantics] + ' ' +
      inherited GetDescription;
  if psoForbidImplicitCasts in FOptions then
    FastStringReplace(result, ': ', ': type ');
end;

// ------------------
// ------------------ TParamSymbolWithDefaultValue ------------------
// ------------------

// Create
//
constructor TParamSymbolWithDefaultValue.Create(const aName: String;
  aType: TTypeSymbol; const srcDC: IDataContext;
  options: TParamSymbolOptions = []);
begin
  inherited Create(aName, aType, options);
  FDefaultValue := TDataContext.CreateStandalone(typ.size);
  if srcDC.DataLength > 0 then
    FDefaultValue.WriteData(srcDC, typ.size);
end;

// Clone
//
function TParamSymbolWithDefaultValue.Clone: TParamSymbol;
begin
  result := TParamSymbolWithDefaultValue.Create(Name, typ, FDefaultValue);
end;

// Specialize
//
function TParamSymbolWithDefaultValue.Specialize(const context
  : ISpecializationContext): TSymbol;
begin
  result := TParamSymbolWithDefaultValue.Create(Name,
    context.SpecializeType(typ), FDefaultValue);
end;

// SameParam
//
function TParamSymbolWithDefaultValue.SameParam(other: TParamSymbol): Boolean;
begin
  result := inherited SameParam(other);
  if result then
  begin
    result := FDefaultValue.SameData((other as TParamSymbolWithDefaultValue)
      .FDefaultValue);
    if not result then
    begin
      result := typ.UnAliasedTypeIs(TDynamicArraySymbol);
    end;
  end;
end;

function TParamSymbolWithDefaultValue.GetDescription: String;
begin
  result := inherited GetDescription;

  // Has a default parameter. Format display of param to show it.
  if FDefaultValue.DataLength > 0 then
  begin
    if (typ is TBaseStringSymbol) then
      result := result + ' = ''' + VariantToString(FDefaultValue[0]) + ''''
      // put quotes around value
    else if (typ is TArraySymbol) then
      result := result + ' = []'
    else if (typ is TSetOfSymbol) then
      result := result + ' = ' + TSetOfSymbol(typ).GetValueDescription
        (FDefaultValue)
    else
      result := result + ' = ' + VariantToString(FDefaultValue[0]);
  end;
end;

// ------------------
// ------------------ TByRefParamSymbol ------------------
// ------------------

constructor TByRefParamSymbol.Create(const aName: String; aTyp: TTypeSymbol;
  const options: TParamSymbolOptions);
begin
  inherited Create(aName, aTyp, options);
  FSize := 1;
end;

// Clone
//
function TByRefParamSymbol.Clone: TParamSymbol;
begin
  result := TByRefParamSymbol.Create(Name, typ, FOptions);
end;

// Specialize
//
function TByRefParamSymbol.Specialize(const context
  : ISpecializationContext): TSymbol;
begin
  result := TByRefParamSymbol.Create(Name, context.SpecializeType(typ),
    FOptions);
end;

// ------------------
// ------------------ TLazyParamSymbol ------------------
// ------------------

// Clone
//
function TLazyParamSymbol.Clone: TParamSymbol;
begin
  result := TLazyParamSymbol.Create(Name, typ);
end;

// Specialize
//
function TLazyParamSymbol.Specialize(const context
  : ISpecializationContext): TSymbol;
begin
  result := TLazyParamSymbol.Create(Name, context.SpecializeType(typ));
end;

// Semantics
//
function TLazyParamSymbol.Semantics: TParamSymbolSemantics;
begin
  result := pssLazy;
end;

// ------------------
// ------------------ TConstByRefParamSymbol ------------------
// ------------------

// Clone
//
function TConstByRefParamSymbol.Clone: TParamSymbol;
begin
  result := TConstByRefParamSymbol.Create(Name, typ, FOptions);
end;

// Specialize
//
function TConstByRefParamSymbol.Specialize(const context
  : ISpecializationContext): TSymbol;
begin
  result := TConstByRefParamSymbol.Create(Name, context.SpecializeType(typ),
    FOptions);
end;

// IsWritable
//
function TConstByRefParamSymbol.IsWritable: Boolean;
begin
  result := False;
end;

// Semantics
//
function TConstByRefParamSymbol.Semantics: TParamSymbolSemantics;
begin
  result := pssConst;
end;

// ------------------
// ------------------ TConstByValueParamSymbol ------------------
// ------------------

// Clone
//
function TConstByValueParamSymbol.Clone: TParamSymbol;
begin
  result := TConstByValueParamSymbol.Create(Name, typ);
end;

// Specialize
//
function TConstByValueParamSymbol.Specialize(const context
  : ISpecializationContext): TSymbol;
begin
  result := TConstByValueParamSymbol.Create(Name, context.SpecializeType(typ));
end;

// IsWritable
//
function TConstByValueParamSymbol.IsWritable: Boolean;
begin
  result := False;
end;

// Semantics
//
function TConstByValueParamSymbol.Semantics: TParamSymbolSemantics;
begin
  result := pssConst;
end;

// ------------------
// ------------------ TVarParamSymbol ------------------
// ------------------

// Clone
//
function TVarParamSymbol.Clone: TParamSymbol;
begin
  result := TVarParamSymbol.Create(Name, typ, FOptions);
end;

// Specialize
//
function TVarParamSymbol.Specialize(const context
  : ISpecializationContext): TSymbol;
begin
  result := TVarParamSymbol.Create(Name, context.SpecializeType(typ), FOptions);
end;

// Semantics
//
function TVarParamSymbol.Semantics: TParamSymbolSemantics;
begin
  result := pssVar;
end;

// ------------------
// ------------------ TSymbolTable ------------------
// ------------------

// Create
//
constructor TSymbolTable.Create(parent: TSymbolTable;
  addrGenerator: TAddrGenerator);
begin
  inherited Create;
  FAddrGenerator := addrGenerator;
  if Assigned(parent) then
    AddParent(parent);
end;

// Destroy
//
destructor TSymbolTable.Destroy;
begin
  FSymbols.Clean;
  FParents.Clear;
  inherited;
end;

// GetSymbol
//
function TSymbolTable.GetSymbol(Index: Integer): TSymbol;
begin
  result := TSymbol(FSymbols.List[Index]);
end;

procedure TSymbolTable.Initialize(const Msgs: TdwsCompileMessageList);
var
  i: Integer;
  ptrList: PObjectTightList;
begin
  ptrList := FSymbols.List;
  for i := 0 to FSymbols.Count - 1 do
    TSymbol(ptrList[i]).Initialize(Msgs);
end;

// FindLocal
//
function TSymbolTable.FindLocal(const aName: String): TSymbol;
var
  lo, hi, mid, cmpResult: Integer;
  ptrList: PObjectTightList;
begin
  hi := FSymbols.Count - 1;
  if hi < 0 then
    Exit(nil);

  if not(stfSorted in FFlags) then
  begin
    if hi > 0 then
      SortSymbols(0, hi);
    Include(FFlags, stfSorted);
  end;

  lo := 0;
  ptrList := FSymbols.List;
  while lo <= hi do
  begin
    mid := (lo + hi) shr 1;
{$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
      result := TSymbol(ptrList[mid]);
{$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}
    cmpResult := UnicodeCompareText(result.Name, aName);
    if cmpResult < 0 then
      lo := mid + 1
    else
    begin
      if cmpResult = 0 then
      begin
        Exit;
      end
      else
        hi := mid - 1;
    end;
  end;
  result := nil;
end;

// FindLocalOfClass
//
function TSymbolTable.FindLocalOfClass(const aName: String;
  ofClass: TSymbolClass): TSymbol;
begin
  result := FindLocal(aName);
  if (result <> nil) and (ofClass <> nil) and not result.InheritsFrom(ofClass)
  then
    result := nil;
end;

// FindTypeLocal
//
function TSymbolTable.FindTypeLocal(const aName: String): TTypeSymbol;
var
  sym: TSymbol;
begin
  sym := FindLocal(aName);
  if (sym <> nil) and (stTypeSymbol in sym.Taxonomy) then
    result := TTypeSymbol(sym)
  else
    result := nil;
end;

// FindSymbolAtStackAddr
//
function TSymbolTable.FindSymbolAtStackAddr(const stackAddr, Level: Integer)
  : TDataSymbol;
var
  i: Integer;
  sym: TSymbol;
begin
  for i := 0 to FSymbols.Count - 1 do
  begin
    sym := TSymbol(FSymbols.List[i]);
    if sym.InheritsFrom(TDataSymbol) then
    begin
      result := TDataSymbol(sym);
      if (result.stackAddr = stackAddr) and (result.Level = Level) then
        Exit;
    end;
  end;

  for i := 0 to ParentCount - 1 do
  begin
    result := Parents[i].FindSymbolAtStackAddr(stackAddr, Level);
    if Assigned(result) then
      Exit;
  end;

  result := nil;
end;

// SortSymbols
//
procedure TSymbolTable.SortSymbols(minIndex, maxIndex: Integer);
var
  i, j, p: Integer;
  pSym: TSymbol;
  ptrList: PObjectTightList;
begin
  if maxIndex <= minIndex then
    Exit;
  ptrList := FSymbols.List;
  repeat
    i := minIndex;
    j := maxIndex;
    p := ((i + j) shr 1);
    repeat
      pSym := TSymbol(ptrList[p]);
      while UnicodeCompareText(TSymbol(ptrList[i]).Name, pSym.Name) < 0 do
        Inc(i);
      while UnicodeCompareText(TSymbol(ptrList[j]).Name, pSym.Name) > 0 do
        Dec(j);
      if i <= j then
      begin
        FSymbols.Exchange(i, j);
        if p = i then
          p := j
        else if p = j then
          p := i;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if minIndex < j then
      SortSymbols(minIndex, j);
    minIndex := i;
  until i >= maxIndex;
end;

// FindSymbol
//
function TSymbolTable.FindSymbol(const aName: String;
  minVisibility: TdwsVisibility; ofClass: TSymbolClass = nil): TSymbol;
var
  i: Integer;
begin
  // Find Symbol in the local List
  result := FindLocalOfClass(aName, ofClass);
  if Assigned(result) then
  begin
    if result.IsVisibleFor(minVisibility) then
      Exit
    else
      result := nil;
  end;

  // Find Symbol in all parent lists
  for i := 0 to ParentCount - 1 do
  begin
    result := Parents[i].FindSymbol(aName, minVisibility, ofClass);
    if Assigned(result) then
      Break;
  end;
end;

// FindTypeSymbol
//
function TSymbolTable.FindTypeSymbol(const aName: String;
  minVisibility: TdwsVisibility): TTypeSymbol;
begin
  result := TTypeSymbol(FindSymbol(aName, minVisibility, TTypeSymbol));
end;

// EnumerateLocalSymbolsOfName
//
function TSymbolTable.EnumerateLocalSymbolsOfName(const aName: String;
  const callback: TSymbolEnumerationCallback): Boolean;
begin
  // TODO: optimize to take advantage of sorting
  var
  List := FSymbols.List;
  var
  nameLen := Length(aName);
  for var i := 0 to Count - 1 do
  begin
{$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
    var
    sym := TSymbol(List[i]);
{$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}
    if (Length(sym.Name) = nameLen) and
      ((nameLen = 0) or (UnicodeCompareLen(Pointer(sym.Name), Pointer(aName),
      nameLen) = 0)) then
    begin
      if callback(sym) then
        Exit(True);
    end;
  end;
  result := False;
end;

// EnumerateSymbolsOfNameInScope
//
function TSymbolTable.EnumerateSymbolsOfNameInScope(const aName: String;
  const callback: TSymbolEnumerationCallback): Boolean;
var
  i: Integer;
  visitedTables: TSimpleObjectHash<TSymbolTable>;
  tableStack: TSimpleStack<TSymbolTable>;
  Current: TSymbolTable;
begin
  visitedTables := TSimpleObjectHash<TSymbolTable>.Create;
  tableStack := TSimpleStack<TSymbolTable>.Create;
  try
    tableStack.Push(Self);
    while tableStack.Count > 0 do
    begin
      Current := tableStack.Peek;
      tableStack.Pop;
      if visitedTables.Add(Current) then
      begin
        if Current.EnumerateLocalSymbolsOfName(aName, callback) then
          Exit(True);
        for i := 0 to Current.ParentCount - 1 do
          tableStack.Push(Current.Parents[i]);
      end;
    end;
    result := False;
  finally
    tableStack.Free;
    visitedTables.Free;
  end;
end;

// EnumerateLocalHelpers
//
function TSymbolTable.EnumerateLocalHelpers(helpedType: TTypeSymbol;
  const callback: THelperSymbolEnumerationCallback): Boolean;
var
  List: PObjectTightList;
begin
  if stfHasHelpers in FFlags then
  begin
    List := FSymbols.List;
    for var i := 0 to FSymbols.Count - 1 do
    begin
{$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
      var
      sym := TSymbol(List[i]);
{$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}
      if sym.ClassType = THelperSymbol then
        if THelperSymbol(sym).HelpsType(helpedType) then
        begin
          if callback(THelperSymbol(sym)) then
            Exit(True);
        end;
    end;
  end;
  result := False;
end;

// EnumerateHelpers
//
function TSymbolTable.EnumerateHelpers(helpedType: TTypeSymbol;
  const callback: THelperSymbolEnumerationCallback): Boolean;
var
  i: Integer;
  p: TSymbolTable;
begin
  if EnumerateLocalHelpers(helpedType, callback) then
    Exit(True);
  for i := 0 to ParentCount - 1 do
  begin
    p := Parents[i];
    if p.IsUnitTable then
    begin
      if p.EnumerateLocalHelpers(helpedType, callback) then
        Exit(True)
    end;
    if p.EnumerateHelpers(helpedType, callback) then
      Exit(True);
  end;
  result := False;
end;

// EnumerateLocalOperatorsFor
//
function TSymbolTable.EnumerateLocalOperatorsFor(aToken: TTokenType;
  aLeftType, aRightType: TTypeSymbol;
  const callback: TOperatorSymbolEnumerationCallback): Boolean;
var
  i: Integer;
  sym: TSymbol;
  opSym: TOperatorSymbol;
  leftParam, rightParam: TTypeSymbol;
  List: PObjectTightList;
begin
  if stfHasLocalOperators in FFlags then
  begin
    List := FSymbols.List;
    for i := 0 to FSymbols.Count - 1 do
    begin
      sym := TSymbol(List[i]);
      if sym.ClassType = TOperatorSymbol then
      begin
        opSym := TOperatorSymbol(sym);
        if opSym.Token <> aToken then
          continue;
        leftParam := opSym.params[0];
        if (aLeftType <> leftParam) and not aLeftType.IsOfType(leftParam) then
          continue;
        rightParam := opSym.params[1];
        if (aRightType <> rightParam) and not aRightType.IsOfType(rightParam)
        then
          continue;
        if callback(opSym) then
          Exit(True);
      end;
    end;
  end;
  result := False;
end;

// EnumerateOperatorsFor
//
function TSymbolTable.EnumerateOperatorsFor(aToken: TTokenType;
  aLeftType, aRightType: TTypeSymbol;
  const callback: TOperatorSymbolEnumerationCallback): Boolean;
var
  i: Integer;
  p: TSymbolTable;
begin
  if stfHasLocalOperators in FFlags then
    if EnumerateLocalOperatorsFor(aToken, aLeftType, aRightType, callback) then
      Exit(True);
  if stfHasParentOperators in FFlags then
  begin
    for i := 0 to ParentCount - 1 do
    begin
      p := Parents[i];
      if p.EnumerateOperatorsFor(aToken, aLeftType, aRightType, callback) then
        Exit(True);
    end;
  end;
  result := False;
end;

// FindImplicitCastOperatorFor
//
function TSymbolTable.FindImplicitCastOperatorFor(fromType, toType: TTypeSymbol)
  : TOperatorSymbol;
var
  i: Integer;
  sym: TSymbol;
  List: PObjectTightList;
begin
  if stfHasLocalOperators in FFlags then
  begin
    List := FSymbols.List;
    for i := 0 to FSymbols.Count - 1 do
    begin
      sym := TSymbol(List[i]);
      if sym.ClassType = TOperatorSymbol then
      begin
        result := TOperatorSymbol(sym);
        if (result.Token = ttIMPLICIT) and (result.typ = toType) and
          (result.params[0] = fromType) and (result.UsesSym <> nil) then
          Exit;
      end;
    end;
  end;
  if stfHasParentOperators in FFlags then
  begin
    for i := 0 to ParentCount - 1 do
    begin
      result := Parents[i].FindImplicitCastOperatorFor(fromType, toType);
      if result <> nil then
        Exit;
    end;
  end;
  result := nil;
end;

// HasSameLocalOperator
//
function TSymbolTable.HasSameLocalOperator(anOpSym: TOperatorSymbol): Boolean;
var
  i: Integer;
  sym: TSymbol;
  opSym: TOperatorSymbol;
  leftType, rightType: TTypeSymbol;
begin
  result := False;
  if not(stfHasLocalOperators in FFlags) then
    Exit;
  if Length(anOpSym.params) <> 2 then
    Exit;
  leftType := anOpSym.params[0];
  rightType := anOpSym.params[1];
  if (leftType = nil) or (rightType = nil) then
    Exit;

  leftType := leftType.GetUnAliasedType;
  rightType := rightType.GetUnAliasedType;
  for i := 0 to Count - 1 do
  begin
    sym := Symbols[i];
    if sym = anOpSym then
      continue;
    if sym.ClassType = TOperatorSymbol then
    begin
      opSym := TOperatorSymbol(sym);
      if (opSym.Token = anOpSym.Token) and
        (leftType = opSym.params[0].UnAliasedType) and
        (rightType = opSym.params[1].UnAliasedType) then
      begin
        Exit(True);
      end;
    end;
  end;
end;

// HasOperators
//
function TSymbolTable.HasOperators: Boolean;
begin
  result := (stfHasOperators in FFlags);
end;

// CollectPublishedSymbols
//
procedure TSymbolTable.CollectPublishedSymbols(symbolList: TSimpleSymbolList);
var
  sym: TSymbol;
begin
  for sym in Self do
  begin
    if sym.ClassType = TClassSymbol then
    begin
      TClassSymbol(sym).CollectPublishedSymbols(symbolList);
    end;
  end;
end;

// HasChildTables
//
function TSymbolTable.HasChildTables: Boolean;
begin
  result := stfHasChildTables in FFlags;
end;

// HasClass
//
function TSymbolTable.HasClass(const aClass: TSymbolClass): Boolean;
var
  i: Integer;
  ptrList: PObjectTightList;
begin
  ptrList := FSymbols.List;
  for i := FSymbols.Count - 1 downto 0 do
  begin
    if TSymbol(ptrList[i]) is aClass then
      Exit(True);
  end;
  result := False;
end;

// HasSymbol
//
function TSymbolTable.HasSymbol(sym: TSymbol): Boolean;
begin
  result := Assigned(Self) and (FSymbols.IndexOf(sym) >= 0);
end;

// HasMethods
//
function TSymbolTable.HasMethods: Boolean;
var
  i: Integer;
  ptrList: PObjectTightList;
begin
  ptrList := FSymbols.List;
  for i := FSymbols.Count - 1 downto 0 do
  begin
    if TSymbol(ptrList[i]).AsFuncSymbol <> nil then
      Exit(True);
  end;
  result := False;
end;

// HasClassSymbols
//
function TSymbolTable.HasClassSymbols: Boolean;
begin
  result := stfHasClassSymbols in FFlags;
end;

// HasNestedtypes
//
function TSymbolTable.HasNestedtypes: Boolean;
begin
  result := stfHasNestedTypes in FFlags;
end;

// IsUnitTable
//
class function TSymbolTable.IsUnitTable: Boolean;
begin
  result := False;
end;

// AddSymbol
//
function TSymbolTable.AddSymbol(sym: TSymbol): Integer;
var
  ct: TClass;
begin
  result := AddSymbolDirect(sym);
  if sym.IsDataSymbol then
  begin
    if FAddrGenerator <> nil then
      TDataSymbol(sym).AllocateStackAddr(FAddrGenerator);
  end
  else
  begin
    ct := sym.ClassType;
    if ct = TOperatorSymbol then
      FFlags := FFlags + [stfHasOperators, stfHasLocalOperators]
    else if ct = TClassSymbol then
      FFlags := FFlags + [stfHasClassSymbols, stfHasNestedTypes]
    else if ct = THelperSymbol then
      FFlags := FFlags + [stfHasHelpers, stfHasNestedTypes]
    else if (ct = TRecordSymbol) or (ct = TAliasSymbol) then
      Include(FFlags, stfHasNestedTypes);
  end;
end;

// AddSymbolDirect
//
function TSymbolTable.AddSymbolDirect(sym: TSymbol): Integer;
var
  n: Integer;
  ptrList: PObjectTightList;
begin
  if stfSorted in FFlags then
  begin
    result := 0;
    n := FSymbols.Count;
    ptrList := FSymbols.List;
    while result < n do
    begin
      if UnicodeCompareText(TSymbol(ptrList[result]).Name, sym.Name) >= 0 then
        Break;
      Inc(result);
    end;
    FSymbols.Insert(result, sym);
  end
  else
    result := FSymbols.Add(sym);
end;

// Remove
//
function TSymbolTable.Remove(sym: TSymbol): Integer;
begin
  result := FSymbols.Remove(sym);
end;

// Clear
//
procedure TSymbolTable.Clear;
begin
  FSymbols.Clear;
end;

// TransferSymbolsTo
//
procedure TSymbolTable.TransferSymbolsTo(destTable: TSymbolTable);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    destTable.AddSymbol(Symbols[i]);
  FSymbols.Clear;
end;

// AddParent
//
procedure TSymbolTable.AddParent(parent: TSymbolTable);
begin
  InsertParent(ParentCount, parent);
end;

// InsertParent
//
procedure TSymbolTable.InsertParent(Index: Integer; parent: TSymbolTable);
begin
  Include(parent.FFlags, stfHasChildTables);
  FParents.Insert(Index, parent);
  if stfHasOperators in parent.FFlags then
    FFlags := FFlags + [stfHasOperators, stfHasParentOperators];
end;

// RemoveParent
//
function TSymbolTable.RemoveParent(parent: TSymbolTable): Integer;
begin
  result := FParents.Remove(parent);
end;

// ClearParents
//
procedure TSymbolTable.ClearParents;
begin
  FParents.Clear;
end;

// GetParents
//
function TSymbolTable.GetParents(Index: Integer): TSymbolTable;
begin
  result := TSymbolTable(FParents.List[Index]);
end;

// IndexOfParent
//
function TSymbolTable.IndexOfParent(parent: TSymbolTable): Integer;
begin
  result := FParents.IndexOf(parent)
end;

// MoveParent
//
procedure TSymbolTable.MoveParent(curIndex, newIndex: Integer);
begin
  FParents.MoveItem(curIndex, newIndex);
end;

// MoveNext
//
function TSymbolTable.TSymbolTableEnumerator.MoveNext: Boolean;
begin
  Dec(Index);
  result := (Index >= 0);
end;

// GetCurrent
//
function TSymbolTable.TSymbolTableEnumerator.GetCurrent: TSymbol;
begin
  result := Table[Index];
end;

// GetEnumerator
//
function TSymbolTable.GetEnumerator: TSymbolTableEnumerator;
begin
  if Self = nil then
  begin
    result.Table := nil;
    result.Index := 0;
  end
  else
  begin
    result.Table := Self;
    result.Index := Count;
  end;
end;

// ------------------
// ------------------ TMembersSymbolTable ------------------
// ------------------

// AddParent
//
procedure TMembersSymbolTable.AddParent(parent: TMembersSymbolTable);
begin
  inherited AddParent(parent);
end;

// FindSymbol
//
function TMembersSymbolTable.FindSymbol(const aName: String;
  minVisibility: TdwsVisibility; ofClass: TSymbolClass = nil): TSymbol;
var
  i: Integer;
begin
  // Find Symbol in the local List
  result := FindLocalOfClass(aName, ofClass);
  if Assigned(result) then
  begin
    if result.IsVisibleFor(minVisibility) then
      Exit;
    // try harder in case of overload with different visibility
    for result in Self do
    begin
      if (UnicodeCompareText(result.Name, aName) = 0) and
        result.IsVisibleFor(minVisibility) then
        Exit;
    end;
  end;
  result := nil;

  // Find Symbol in all parent lists
  if minVisibility = cvPrivate then
    minVisibility := cvProtected;

  for i := 0 to ParentCount - 1 do
  begin
    result := Parents[i].FindSymbol(aName, minVisibility, ofClass);
    if Assigned(result) then
      Break;
  end;
end;

// VisibilityFromScope
//
function TMembersSymbolTable.VisibilityFromScope(scopeSym: TCompositeTypeSymbol)
  : TdwsVisibility;
begin
  if scopeSym = nil then
    result := cvPublic
  else if (scopeSym = owner) or ((scopeSym.UnitSymbol <> nil) and
    (scopeSym.UnitSymbol = owner.UnitSymbol)) then
    result := cvPrivate
  else if scopeSym.DoIsOfType(owner) then
    result := cvProtected
  else
    result := cvPublic;
end;

// FindSymbolFromScope
//
function TMembersSymbolTable.FindSymbolFromScope(const aName: String;
  scopeSym: TCompositeTypeSymbol): TSymbol;
begin
  result := FindSymbol(aName, VisibilityFromScope(scopeSym));
end;

// Visibilities
//
function TMembersSymbolTable.Visibilities: TdwsVisibilities;
var
  sym: TSymbol;
  symClass: TClass;
begin
  result := [];
  for sym in Self do
  begin
    symClass := sym.ClassType;
    if symClass = TFieldSymbol then
      Include(result, TFieldSymbol(sym).Visibility)
    else if symClass.InheritsFrom(TPropertySymbol) then
      Include(result, TPropertySymbol(sym).Visibility)
    else if symClass.InheritsFrom(TMethodSymbol) then
      Include(result, TMethodSymbol(sym).Visibility)
  end;
end;

// ------------------
// ------------------ TUnSortedSymbolTable ------------------
// ------------------

// FindLocal
//
function TUnSortedSymbolTable.FindLocal(const aName: String): TSymbol;
var
  n: Integer;
  ptrList: PPointer;
begin
  n := FSymbols.Count;
  if n > 0 then
  begin
    ptrList := PPointer(FSymbols.List);
    repeat
      result := TSymbol(ptrList^);
      if UnicodeSameText(result.Name, aName) then
      begin
        Exit;
      end;
      Inc(ptrList);
      Dec(n);
    until n <= 0;
  end;
  result := nil;
end;

// IndexOf
//
function TUnSortedSymbolTable.IndexOf(sym: TSymbol): Integer;
begin
  result := FSymbols.IndexOf(sym);
end;

// ------------------
// ------------------ TExternalVarSymbol ------------------
// ------------------

destructor TExternalVarSymbol.Destroy;
begin
  FReadFunc.Free;
  FWriteFunc.Free;
  inherited;
end;

function TExternalVarSymbol.GetReadFunc: TFuncSymbol;
begin
  result := FReadFunc;
end;

function TExternalVarSymbol.GetWriteFunc: TFuncSymbol;
begin
  result := FWriteFunc;
end;

// ------------------
// ------------------ TAddrGeneratorRec ------------------
// ------------------

// CreatePositive
//
class function TAddrGeneratorRec.CreatePositive(aLevel: SmallInt;
  anInitialSize: Integer = 0): TAddrGeneratorRec;
begin
  result.DataSize := anInitialSize;
  result.FLevel := aLevel;
  result.FSign := agsPositive;
end;

// CreateNegative
//
class function TAddrGeneratorRec.CreateNegative(aLevel: SmallInt)
  : TAddrGeneratorRec;
begin
  result.DataSize := 0;
  result.FLevel := aLevel;
  result.FSign := agsNegative;
end;

// GetStackAddr
//
function TAddrGeneratorRec.GetStackAddr(size: Integer): Integer;
begin
  if FSign = agsPositive then
  begin
    result := DataSize;
    Inc(DataSize, size);
  end
  else
  begin
    Inc(DataSize, size);
    result := -DataSize;
  end;
end;

// ------------------
// ------------------ TSetOfSymbol ------------------
// ------------------

// Create
//
constructor TSetOfSymbol.Create(const Name: String; indexType: TTypeSymbol;
  aMin, aMax: Integer);
begin
  inherited Create(name, indexType);
  FMinValue := aMin;
  FCountValue := aMax - aMin + 1;
  FSize := 1 + (FCountValue shr 6);
end;

// IsCompatible
//
function TSetOfSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  typSym := typSym.UnAliasedType;
  if typSym.ClassType = TSetOfSymbol then
  begin
    result := typ.SameType(typSym.typ) or
      (typSym.typ.IsOfType(typ) and (TSetOfSymbol(typSym).MinValue = MinValue)
      and (TSetOfSymbol(typSym).CountValue = CountValue));
  end
  else
    result := False;
end;

// SameType
//
function TSetOfSymbol.SameType(typSym: TTypeSymbol): Boolean;
begin
  result := Assigned(typSym) and (typSym.ClassType = TSetOfSymbol) and
    typ.SameType(typSym.typ);
end;

// InitDataContext
//
procedure TSetOfSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
var
  i: NativeInt;
begin
  for i := offset to offset + size - 1 do
    data.SetZeroInt64(i);
end;

// AssignsAsDataExpr
//
function TSetOfSymbol.AssignsAsDataExpr: Boolean;
begin
  result := True;
end;

// ValueToOffsetMask
//
function TSetOfSymbol.ValueToOffsetMask(value: Integer;
  var mask: Int64): Integer;
begin
  result := (value - MinValue) shr 6;
  mask := Int64(1) shl (value and 63);
end;

// ValueToByteOffsetMask
//
function TSetOfSymbol.ValueToByteOffsetMask(value: Integer;
  var mask: Byte): Integer;
begin
  result := (value - MinValue) shr 3;
  mask := 1 shl (value and 7);
end;

// GetValueDescription
//
function TSetOfSymbol.GetValueDescription(const value: IDataContext): String;
begin
  result := '';
  for var i := 0 to size - 1 do
  begin
    var
    v := UInt64(value.AsInteger[i]);
    while v <> 0 do
    begin
      for var j := 0 to 63 do
      begin
        var
        bit := UInt64(1) shl j;
        if (v and bit) <> 0 then
        begin
          v := v - bit;
          var
          elemValue := j + (i shl 6);
          var
          elem := ElementByValue(elemValue);
          if result <> '' then
            result := result + ', ';
          if elem <> nil then
            result := result + elem.StandardName
          else
            result := result + typ.Name + '(' + IntToStr(elemValue) + ')';
        end;
      end;
    end;
  end;
  if result = '' then
    result := '[]'
  else
    result := '[ ' + result + ' ]';
end;

// ElementByValue
//
function TSetOfSymbol.ElementByValue(value: Integer): TElementSymbol;
begin
  result := (typ.UnAliasedType as TEnumerationSymbol).ElementByValue(value)
end;

// GetMaxValue
//
function TSetOfSymbol.GetMaxValue: Integer;
begin
  result := MinValue + CountValue - 1;
end;

// InitializePseudoMethodSymbol
//
function TSetOfSymbol.InitializePseudoMethodSymbol(methodKind: TArrayMethodKind;
  BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol;
var
  methodName: String;
begin
  result := nil;

  methodName := Copy(GetEnumName(TypeInfo(TArrayMethodKind),
    Ord(methodKind)), 4);
  case methodKind of
    amkInclude, amkExclude:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('element', typ));
      end;
  end;
  if result <> nil then
    FPseudoMethods[methodKind] := result
end;

// ------------------
// ------------------ TPseudoMethodSymbol ------------------
// ------------------

// Create
//
constructor TPseudoMethodSymbol.Create(owner: TTypeSymbol; const Name: String;
  funcKind: TFuncKind; funcLevel: SmallInt);
begin
  inherited Create(name, funcKind, funcLevel);
  FOwnerTyp := owner;
end;

// ------------------
// ------------------ TTypeWithPseudoMethodsSymbol ------------------
// ------------------

// GetZeroDC
//
class function TTypeWithPseudoMethodsSymbol.GetZeroDC: IDataContext;
begin
  if vZeroDC = nil then
  begin
    vZeroDC := TDataContext.CreateStandalone(1);
    vZeroDC.SetZeroInt64(0);
  end;
  result := vZeroDC;
end;

// InitializePseudoMethodSymbol
//
function TTypeWithPseudoMethodsSymbol.InitializePseudoMethodSymbol
  (methodKind: TArrayMethodKind; BaseSymbols: TdwsBaseSymbolsContext)
  : TPseudoMethodSymbol;
begin
  result := nil;
end;

// Destroy
//
destructor TTypeWithPseudoMethodsSymbol.Destroy;
var
  f: TPseudoMethodSymbol;
begin
  inherited;
  for f in FPseudoMethods do
    f.Free;
end;

// PseudoMethodSymbol
//
function TTypeWithPseudoMethodsSymbol.PseudoMethodSymbol
  (methodKind: TArrayMethodKind; BaseSymbols: TdwsBaseSymbolsContext)
  : TPseudoMethodSymbol;
begin
  result := FPseudoMethods[methodKind];
  if result = nil then
    result := InitializePseudoMethodSymbol(methodKind, BaseSymbols);
end;

// ------------------
// ------------------ TArraySymbol ------------------
// ------------------

// Create
//
constructor TArraySymbol.Create(const Name: String;
  elementType, indexType: TTypeSymbol);
begin
  inherited Create(name, elementType);
  FIndexType := indexType;
end;

// Destroy
//
destructor TArraySymbol.Destroy;
begin
  FSortFunctionType.Free;
  FMapFunctionType.Free;
  FFilterFunctionType.Free;
  inherited;
end;

// DynamicInitialization
//
function TArraySymbol.DynamicInitialization: Boolean;
begin
  result := True;
end;

// AssignsAsDataExpr
//
function TArraySymbol.AssignsAsDataExpr: Boolean;
begin
  result := True;
end;

// InitializePseudoMethodSymbol
//
function TArraySymbol.InitializePseudoMethodSymbol(methodKind: TArrayMethodKind;
  BaseSymbols: TdwsBaseSymbolsContext): TPseudoMethodSymbol;
var
  methodName: String;
begin
  result := nil;

  methodName := Copy(GetEnumName(TypeInfo(TArrayMethodKind),
    Ord(methodKind)), 4);
  case methodKind of
    amkLength, amkCount:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.typ := BaseSymbols.TypInteger;
      end;
    amkHigh, amkLow:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.typ := indexType;
      end;
    amkIndexOf:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('item', typ));
        result.params.AddSymbol(TParamSymbolWithDefaultValue.Create('index',
          BaseSymbols.TypInteger, GetZeroDC));
        result.typ := BaseSymbols.TypInteger;
      end;
    amkContains:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('item', typ));
        result.typ := BaseSymbols.TypBoolean;
      end;
  end;
  if result <> nil then
    FPseudoMethods[methodKind] := result
end;

// ElementSize
//
function TArraySymbol.ElementSize: Integer;
begin
  if typ <> nil then
    result := typ.size
  else
    result := 0;
end;

// SortFunctionType
//
function TArraySymbol.SortFunctionType(BaseSymbols: TdwsBaseSymbolsContext)
  : TFuncSymbol;
begin
  if FSortFunctionType = nil then
  begin
    FSortFunctionType := TFuncSymbol.Create('', fkFunction, 0);
    FSortFunctionType.typ := BaseSymbols.TypInteger;
    FSortFunctionType.AddParam(TParamSymbol.Create('left', typ));
    FSortFunctionType.AddParam(TParamSymbol.Create('right', typ));
  end;
  result := FSortFunctionType;
end;

// MapFunctionType
//
function TArraySymbol.MapFunctionType(BaseSymbols: TdwsBaseSymbolsContext)
  : TFuncSymbol;
begin
  if FMapFunctionType = nil then
  begin
    FMapFunctionType := TFuncSymbol.Create('', fkFunction, 0);
    FMapFunctionType.typ := BaseSymbols.TypAnyType;
    FMapFunctionType.AddParam(TParamSymbol.Create('v', typ));
  end;
  result := FMapFunctionType;
end;

// FilterFunctionType
//
function TArraySymbol.FilterFunctionType(BaseSymbols: TdwsBaseSymbolsContext)
  : TFuncSymbol;
begin
  if FFilterFunctionType = nil then
  begin
    FFilterFunctionType := TFuncSymbol.Create('', fkFunction, 0);
    FFilterFunctionType.typ := BaseSymbols.TypBoolean;
    FFilterFunctionType.AddParam(TParamSymbol.Create('v', typ));
  end;
  result := FFilterFunctionType;
end;

// ForEachFunctionType
//
function TArraySymbol.ForEachFunctionType(BaseSymbols: TdwsBaseSymbolsContext)
  : TFuncSymbol;
begin
  if FForEachFunctionType = nil then
  begin
    FForEachFunctionType := TFuncSymbol.Create('', fkProcedure, 0);
    FForEachFunctionType.AddParam(TParamSymbol.Create('v', typ));
  end;
  result := FForEachFunctionType;
end;

// ------------------
// ------------------ TDynamicArraySymbol ------------------
// ------------------

var
  vInitDynamicArray: TInitDataProc;

  // Create
  //
constructor TDynamicArraySymbol.Create(const Name: String;
  elementType, indexType: TTypeSymbol);
begin
  inherited;
  FSize := 1;
end;

// GetCaption
//
function TDynamicArraySymbol.GetCaption: String;
begin
  if Name <> '' then
    result := Name
  else
    result := GetDescription;
end;

// GetDescription
//
function TDynamicArraySymbol.GetDescription: String;
begin
  result := 'array of ' + typ.Caption;
end;

// InitDataContext
//
procedure TDynamicArraySymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  vInitDynamicArray(Self.typ, data, offset);
end;

// DoIsOfType
//
function TDynamicArraySymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym = Self) or ((typSym is TDynamicArraySymbol) and
    typSym.typ.DoIsOfType(typ));
end;

// InitializePseudoMethodSymbol
//
function TDynamicArraySymbol.InitializePseudoMethodSymbol
  (methodKind: TArrayMethodKind; BaseSymbols: TdwsBaseSymbolsContext)
  : TPseudoMethodSymbol;
var
  methodName: String;
begin
  result := nil;

  methodName := Copy(GetEnumName(TypeInfo(TArrayMethodKind),
    Ord(methodKind)), 4);
  case methodKind of
    amkAdd, amkPush:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('item', typ));
        result.typ := Self;
      end;
    amkIndexOf, amkRemove:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('item', typ));
        result.typ := indexType;
      end;
    amkPop, amkPeek:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.typ := typ;
      end;
    amkMap:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('func',
          MapFunctionType(BaseSymbols)));
        result.typ := Self;
      end;
    amkSort:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('func',
          SortFunctionType(BaseSymbols)));
        result.typ := Self;
      end;
    amkFilter:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('func',
          FilterFunctionType(BaseSymbols)));
        result.typ := Self;
      end;
    amkForEach:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('func',
          ForEachFunctionType(BaseSymbols)));
        result.typ := Self;
      end;
    amkDelete:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkProcedure, 0);
        result.params.AddSymbol(TParamSymbol.Create('index', indexType));
        result.params.AddSymbol(TParamSymbolWithDefaultValue.Create('count',
          BaseSymbols.TypInteger, GetZeroDC));
      end;
    amkInsert:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkProcedure, 0);
        result.params.AddSymbol(TParamSymbol.Create('index',
          BaseSymbols.TypInteger));
        result.params.AddSymbol(TParamSymbol.Create('item', typ));
      end;
    amkSetLength:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkProcedure, 0);
        result.params.AddSymbol(TParamSymbol.Create('newLength',
          BaseSymbols.TypInteger));
      end;
    amkClear:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkProcedure, 0);
      end;
    amkSwap:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('index1', indexType));
        result.params.AddSymbol(TParamSymbol.Create('index2', indexType));
        result.typ := Self;
      end;
    amkMove:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('fromIndex', indexType));
        result.params.AddSymbol(TParamSymbol.Create('toIndex', indexType));
        result.typ := Self;
      end;
    amkCopy:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('startIndex', indexType));
        result.params.AddSymbol(TParamSymbolWithDefaultValue.Create('count',
          BaseSymbols.TypInteger, GetZeroDC));
        result.typ := Self;
      end;
    amkReverse:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.typ := Self;
      end;
  end;
  if result <> nil then
    FPseudoMethods[methodKind] := result
  else
    result := inherited InitializePseudoMethodSymbol(methodKind, BaseSymbols);
end;

// IsCompatible
//
function TDynamicArraySymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := ((typSym is TDynamicArraySymbol) and
    (typ.IsCompatible(typSym.typ) or (typSym.typ is TNilSymbol)) or
    ((typSym is TStaticArraySymbol) and TStaticArraySymbol(typSym)
    .IsEmptyArray));
end;

// IsPointerType
//
function TDynamicArraySymbol.IsPointerType: Boolean;
begin
  result := True;
end;

// SameType
//
function TDynamicArraySymbol.SameType(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym <> nil) and (typSym.ClassType = TDynamicArraySymbol) and
    typ.SameType(typSym.typ);
end;

// SpecializeType
//
function TDynamicArraySymbol.SpecializeType(const context
  : ISpecializationContext): TTypeSymbol;
begin
  result := TDynamicArraySymbol.Create(Name, context.SpecializeType(typ),
    context.SpecializeType(indexType));
  context.RegisterInternalType(result);
end;

// SetInitDynamicArrayProc
//
class procedure TDynamicArraySymbol.SetInitDynamicArrayProc
  (const aProc: TInitDataProc);
begin
  vInitDynamicArray := aProc;
end;

// ------------------
// ------------------ TStaticArraySymbol ------------------
// ------------------

// Create
//
constructor TStaticArraySymbol.Create(const Name: String;
  elementType, indexType: TTypeSymbol; lowBound, highBound: Integer);
begin
  inherited Create(name, elementType, indexType);
  FLowBound := lowBound;
  FHighBound := highBound;
  FElementCount := highBound - lowBound + 1;
  FSize := FElementCount * ElementSize;
end;

// InitDataContext
//
procedure TStaticArraySymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
var
  i, s: NativeInt;
begin
  s := typ.BaseType.size;
  for i := 1 to ElementCount do
  begin
    typ.InitDataContext(data, offset);
    Inc(offset, s);
  end;
end;

// IsCompatible
//
function TStaticArraySymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  typSym := typSym.UnAliasedType;
  result := (typSym is TStaticArraySymbol) and
    (size = TStaticArraySymbol(typSym).size) and typ.IsCompatible(typSym.typ);
end;

// SameType
//
function TStaticArraySymbol.SameType(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym <> nil) and (typSym.ClassType = ClassType) and
    (typ.SameType(typSym.typ)) and
    (lowBound = TStaticArraySymbol(typSym).lowBound) and
    (highBound = TStaticArraySymbol(typSym).highBound);
end;

// DoIsOfType
//
function TStaticArraySymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
begin
  result := inherited DoIsOfType(typSym);
  if not result then
  begin
    if typSym.ClassType = TStaticArraySymbol then
      result := (lowBound = TStaticArraySymbol(typSym).lowBound) and
        (highBound = TStaticArraySymbol(typSym).highBound) and
        typ.IsCompatible(typSym.typ)
    else if typSym is TOpenArraySymbol then
      result := (ElementCount = 0) or (typ.IsCompatible(typSym.typ))
  end;
end;

// AddElement
//
procedure TStaticArraySymbol.AddElement;
begin
  Inc(FHighBound);
  Inc(FElementCount);
  FSize := FElementCount * ElementSize;
end;

// IsEmptyArray
//
function TStaticArraySymbol.IsEmptyArray: Boolean;
begin
  result := (highBound < lowBound);
end;

// GetCaption
//
function TStaticArraySymbol.GetCaption;
begin
  result := 'array [' + IntToStr(FLowBound) + '..' + IntToStr(FHighBound) +
    '] of ' + typ.Caption;
end;

// ------------------
// ------------------ TOpenArraySymbol ------------------
// ------------------

// Create
//
constructor TOpenArraySymbol.Create(const Name: String;
  elementType, indexType: TTypeSymbol);
begin
  inherited Create(name, elementType, indexType, 0, -1);
  FSize := 1;
end;

// IsCompatible
//
function TOpenArraySymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  if typSym = nil then
    Exit(False);
  typSym := typSym.BaseType;
  result := (typSym is TStaticArraySymbol) and typ.IsCompatible(typSym.typ);
end;

// GetCaption
//
function TOpenArraySymbol.GetCaption: String;
begin
  result := 'array of const';
end;

// ------------------
// ------------------ TAssociativeArraySymbol ------------------
// ------------------

var
  vInitAssociativeArray: TInitDataProc;

  // Create
  //
constructor TAssociativeArraySymbol.Create(const Name: String;
  elementType, keyType: TTypeSymbol);
begin
  inherited Create(name, elementType);
  FKeyType := keyType;
  FSize := 1;
end;

// Destroy
//
destructor TAssociativeArraySymbol.Destroy;
begin
  inherited;
  FKeyArrayType.Free;
end;

// InitDataContext
//
procedure TAssociativeArraySymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  vInitAssociativeArray(Self, data, offset);
end;

// SetInitAssociativeArrayProc
//
class procedure TAssociativeArraySymbol.SetInitAssociativeArrayProc
  (const aProc: TInitDataProc);
begin
  vInitAssociativeArray := aProc;
end;

// DynamicInitialization
//
function TAssociativeArraySymbol.DynamicInitialization: Boolean;
begin
  result := True;
end;

// IsCompatible
//
function TAssociativeArraySymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym is TAssociativeArraySymbol) and typ.IsCompatible(typSym.typ)
    and keyType.IsCompatible(TAssociativeArraySymbol(typSym).keyType);
end;

// IsPointerType
//
function TAssociativeArraySymbol.IsPointerType: Boolean;
begin
  result := True;
end;

// SameType
//
function TAssociativeArraySymbol.SameType(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym <> nil) and (typSym.ClassType = TAssociativeArraySymbol) and
    typ.SameType(typSym.typ) and
    keyType.SameType(TAssociativeArraySymbol(typSym).keyType);
end;

// KeysArrayType
//
function TAssociativeArraySymbol.KeysArrayType(BaseSymbols
  : TdwsBaseSymbolsContext): TDynamicArraySymbol;
begin
  if FKeyArrayType = nil then
    FKeyArrayType := TDynamicArraySymbol.Create('', keyType,
      BaseSymbols.TypInteger);
  result := FKeyArrayType;
end;

// KeyAndElementSizeAreBaseTypesOfSizeOne
//
function TAssociativeArraySymbol.KeyAndElementSizeAreBaseTypesOfSizeOne
  : Boolean;
begin
  result := (FKeyType.size = 1) and (typ.size = 1) and FKeyType.IsBaseType and
    typ.IsBaseType;
end;

// GetCaption
//
function TAssociativeArraySymbol.GetCaption: String;
begin
  result := 'array [' + keyType.Caption + '] of ' + typ.Caption;
end;

// DoIsOfType
//
function TAssociativeArraySymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
begin
  result := SameType(typSym.UnAliasedType);
end;

// InitializePseudoMethodSymbol
//
function TAssociativeArraySymbol.InitializePseudoMethodSymbol
  (methodKind: TArrayMethodKind; BaseSymbols: TdwsBaseSymbolsContext)
  : TPseudoMethodSymbol;
var
  methodName: String;
begin
  result := nil;

  methodName := Copy(GetEnumName(TypeInfo(TArrayMethodKind),
    Ord(methodKind)), 4);
  case methodKind of
    amkLength, amkCount:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.typ := BaseSymbols.TypInteger;
      end;
    amkClear:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkProcedure, 0);
      end;
    amkDelete:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('key', keyType));
        result.typ := BaseSymbols.TypBoolean;
      end;
    amkKeys:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.typ := KeysArrayType(BaseSymbols);
      end;
  end;
  if result <> nil then
    FPseudoMethods[methodKind] := result
  else
    result := inherited InitializePseudoMethodSymbol(methodKind, BaseSymbols);
end;

// ------------------
// ------------------ TElementSymbol ------------------
// ------------------

// Create
//
constructor TElementSymbol.Create(const Name: String; typ: TTypeSymbol;
  const aValue: Int64; isUserDef: Boolean);
begin
  inherited CreateValue(Name, typ, aValue);
  FIsUserDef := isUserDef;
end;

// StandardName
//
function TElementSymbol.StandardName: String;
begin
  if Enumeration.Style = enumClassic then
    result := Name
  else
    result := QualifiedName;
end;

// QualifiedName
//
function TElementSymbol.QualifiedName: String;
begin
  result := String(Enumeration.Name + '.' + Name);
end;

// GetDescription
//
function TElementSymbol.GetDescription: String;
begin
  if FIsUserDef then
    result := Name + ' = ' + FastInt64ToStr(value)
  else
    result := Name;
end;

// GetValue
//
function TElementSymbol.GetValue: Int64;
begin
  result := FDataContext.AsInteger[0];
end;

// ------------------
// ------------------ TEnumerationSymbol ------------------
// ------------------

// Create
//
constructor TEnumerationSymbol.Create(const Name: String; BaseType: TTypeSymbol;
  aStyle: TEnumerationSymbolStyle);
begin
  inherited Create(Name, BaseType);
  FElements := TUnSortedSymbolTable.Create;
  FLowBound := MaxInt;
  FHighBound := -MaxInt;
  FStyle := aStyle;
  FContinuous := True;
end;

// Destroy
//
destructor TEnumerationSymbol.Destroy;
begin
  FElements.Free;
  inherited;
end;

// DefaultValue
//
function TEnumerationSymbol.DefaultValue: Int64;
begin
  if FElements.Count > 0 then
    result := TElementSymbol(FElements[0]).value
  else
    result := 0;
end;

// InitDataContext
//
procedure TEnumerationSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  data.AsInteger[offset] := DefaultValue;
end;

// BaseType
//
function TEnumerationSymbol.BaseType: TTypeSymbol;
begin
  result := typ;
end;

// IsCompatible
//
function TEnumerationSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym.UnAliasedType = Self);
end;

// DoIsOfType
//
function TEnumerationSymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
begin
  result := inherited DoIsOfType(typSym) or BaseType.DoIsOfType(typSym);
end;

// InitializePseudoMethodSymbol
//
function TEnumerationSymbol.InitializePseudoMethodSymbol
  (methodKind: TArrayMethodKind; BaseSymbols: TdwsBaseSymbolsContext)
  : TPseudoMethodSymbol;
var
  methodName: String;
begin
  result := nil;

  methodName := Copy(GetEnumName(TypeInfo(TArrayMethodKind),
    Ord(methodKind)), 4);
  case methodKind of
    amkByName:
      begin
        result := TPseudoMethodSymbol.Create(Self, methodName, fkFunction, 0);
        result.params.AddSymbol(TParamSymbol.Create('name', typ));
      end;
  end;
  if result <> nil then
    FPseudoMethods[methodKind] := result
end;

// AddElement
//
procedure TEnumerationSymbol.AddElement(element: TElementSymbol);
begin
  if FContinuous and (FElements.Count > 0) then
    if element.value <> FHighBound + 1 then
      FContinuous := False;
  if element.value < FLowBound then
    FLowBound := element.value;
  if element.value > FHighBound then
    FHighBound := element.value;
  FElements.AddSymbol(element);
  element.FEnumeration := Self;
end;

// ElementByValue
//
function TEnumerationSymbol.ElementByValue(const value: Int64): TElementSymbol;
var
  i: Integer;
begin
  if (value >= FLowBound) and (value <= FHighBound) then
  begin
    if Continuous then
    begin
      result := TElementSymbol(Elements[value - FLowBound]);
      Exit;
    end
    else
    begin
      for i := 0 to Elements.Count - 1 do
      begin
        result := TElementSymbol(Elements[i]);
        if result.value = value then
          Exit;
      end;
    end;
  end;
  result := nil;
end;

// ElementByName
//
function TEnumerationSymbol.ElementByName(const Name: String): TElementSymbol;
begin
  result := TElementSymbol(Elements.FindLocal(name));
end;

// GetCaption
//
function TEnumerationSymbol.GetCaption: String;
begin
  result := Name;
end;

// GetDescription
//
function TEnumerationSymbol.GetDescription: String;
var
  i: Integer;
begin
  result := '(';
  for i := 0 to FElements.Count - 1 do
  begin
    if i <> 0 then
      result := result + ', ';
    result := result + FElements[i].GetDescription;
  end;
  result := result + ')';
end;

// ShortDescription
//
function TEnumerationSymbol.ShortDescription: String;
begin
  case FElements.Count of
    0:
      result := ' ';
    1:
      result := FElements[0].GetDescription;
  else
    result := FElements[0].Name + ',...';
  end;
  result := '(' + result + ')';
end;

// ------------------
// ------------------ TAliasSymbol ------------------
// ------------------

// BaseType
//
function TAliasSymbol.BaseType: TTypeSymbol;
begin
  result := typ.BaseType;
end;

// GetUnAliasedType
//
function TAliasSymbol.GetUnAliasedType: TTypeSymbol;
begin
  result := typ.UnAliasedType;
end;

// InitDataContext
//
procedure TAliasSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  typ.InitDataContext(data, offset);
end;

// IsCompatible
//
function TAliasSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := typ.IsCompatible(typSym);
end;

// IsPointerType
//
function TAliasSymbol.IsPointerType: Boolean;
begin
  result := typ.IsPointerType;
end;

// Taxonomy
//
function TAliasSymbol.Taxonomy: TdwsSymbolTaxonomy;
begin
  result := [stTypeSymbol, stAliasSymbol];
end;

// DoIsOfType
//
function TAliasSymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
begin
  result := typ.DoIsOfType(typSym);
end;

// GetAsFuncSymbol
//
function TAliasSymbol.GetAsFuncSymbol: TFuncSymbol;
begin
  result := typ.GetAsFuncSymbol;
end;

// GetDescription
//
function TAliasSymbol.GetDescription: String;
begin
  result := Name + ' = ' + typ.Name;
end;

// GetCaption
//
function TAliasSymbol.GetCaption: String;
begin
  if Name <> '' then
    result := Name
  else
    result := typ.Caption;
end;

// ------------------
// ------------------ TTypeSymbol ------------------
// ------------------

// BaseType
//
function TTypeSymbol.BaseType: TTypeSymbol;
begin
  result := Self;
end;

// GetUnAliasedType
//
function TTypeSymbol.GetUnAliasedType: TTypeSymbol;
begin
  result := Self;
end;

// UnaliasedType
//
function TTypeSymbol.UnAliasedType: TTypeSymbol;
begin
  if Self = nil then
    result := nil
  else
    result := GetUnAliasedType;
end;

// UnAliasedTypeIs
//
function TTypeSymbol.UnAliasedTypeIs(const typeSymbolClass
  : TTypeSymbolClass): Boolean;
begin
  result := (Self <> nil) and GetUnAliasedType.InheritsFrom(typeSymbolClass);
end;

// IsOfType
//
function TTypeSymbol.IsOfType(typSym: TTypeSymbol): Boolean;
begin
  if Self = nil then
    result := (typSym = nil)
  else
    result := (typSym <> nil) and DoIsOfType(typSym);
end;

// DoIsOfType
//
function TTypeSymbol.DoIsOfType(typSym: TTypeSymbol): Boolean;
begin
  result := (Self = typSym.UnAliasedType);
end;

// GetIsDeprecated
//
function TTypeSymbol.GetIsDeprecated: Boolean;
begin
  result := (FDeprecatedMessage <> '');
end;

// IsCompatible
//
function TTypeSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := BaseType.IsCompatible(typSym.BaseType);
end;

// CanExpectAnyFuncSymbol
//
function TTypeSymbol.CanExpectAnyFuncSymbol: Boolean;
begin
  result := False;
end;

// IsCompatibleWithAnyFuncSymbol
//
function TTypeSymbol.IsCompatibleWithAnyFuncSymbol: Boolean;
begin
  result := False;
end;

// Taxonomy
//
function TTypeSymbol.Taxonomy: TdwsSymbolTaxonomy;
begin
  result := [stTypeSymbol];
end;

// DistanceTo
//
function TTypeSymbol.DistanceTo(typeSym: TTypeSymbol): Integer;
begin
  if Self = typeSym then
    result := 0
  else if UnAliasedType = typeSym.UnAliasedType then
    result := 1
  else if IsCompatible(typeSym) then
    result := 2
  else
    result := 3;
end;

// SameType
//
function TTypeSymbol.SameType(typSym: TTypeSymbol): Boolean;
begin
  result := (Self = typSym);
end;

// HasMetaSymbol
//
function TTypeSymbol.HasMetaSymbol: Boolean;
begin
  result := False;
end;

// IsForwarded
//
function TTypeSymbol.IsForwarded: Boolean;
begin
  result := False;
end;

// AssignsAsDataExpr
//
function TTypeSymbol.AssignsAsDataExpr: Boolean;
begin
  result := (size <> 1);
end;

// Specialize
//
function TTypeSymbol.Specialize(const context: ISpecializationContext): TSymbol;
begin
  result := SpecializeType(context);
end;

// SpecializeType
//
function TTypeSymbol.SpecializeType(const context: ISpecializationContext)
  : TTypeSymbol;
begin
  context.AddCompilerErrorFmt(CPE_SpecializationNotSupportedYet, [ClassName]);
  result := Self;
end;

// IsType
//
function TTypeSymbol.IsType: Boolean;
begin
  result := True;
end;

// InitDataContext
//
procedure TTypeSymbol.InitDataContext(const data: IDataContext;
  offset: NativeInt);
begin
  Assert(False);
end;

// InitString
//
procedure TTypeSymbol.InitString(var s: String);
var
  dc: IDataContext;
begin
  Assert(size = 1);
  dc := TDataContext.CreateStandalone(1);
  InitDataContext(dc, 0);
  dc.EvalAsString(0, s);
end;

// DynamicInitialization
//
function TTypeSymbol.DynamicInitialization: Boolean;
begin
  result := False;
end;

// ------------------
// ------------------ TAnyTypeSymbol ------------------
// ------------------

// IsCompatible
//
function TAnyTypeSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym <> nil);
end;

// ------------------
// ------------------ EScriptError ------------------
// ------------------

// CreatePosFmt
//
constructor EScriptError.CreatePosFmt(const aScriptPos: TScriptPos;
  const msg: String; const args: array of const);
begin
  inherited CreateFmt(msg, args);
  FScriptPos := aScriptPos;
end;

// SetScriptPos
//
procedure EScriptError.SetScriptPos(const aPos: TScriptPos);
begin
  FScriptPos := aPos;
end;

// ------------------
// ------------------ EScriptStopped ------------------
// ------------------

// DoRaise
//
class procedure EScriptStopped.DoRaise(exec: TdwsExecution;
  stoppedOn: TExprBase);
var
  e: EScriptStopped;
begin
  e := EScriptStopped.CreatePosFmt(stoppedOn.scriptPos, RTE_ScriptStopped, []);
  e.ScriptCallStack := exec.GetCallStack;
  raise e;
end;

// ------------------
// ------------------ EScriptException ------------------
// ------------------

// Create
//
constructor EScriptException.Create(const msgString: String;
  const anExceptionObj: IScriptObj; const aScriptPos: TScriptPos);
begin
  inherited Create(msgString);
  FExceptObj := anExceptionObj;
  FScriptPos := aScriptPos;
end;

// ------------------
// ------------------ TdwsBaseSymbolsContext ------------------
// ------------------

// SetBaseTypes
//
procedure TdwsBaseSymbolsContext.SetBaseTypes(const bt: TdwsBaseSymbolTypes);
begin
  FBaseTypes := bt;
end;

// ------------------
// ------------------ TdwsExecution ------------------
// ------------------

// Create
//
constructor TdwsExecution.Create(const stackParams: TStackParameters);
begin
  inherited Create;
  FStack.Initialize(stackParams);
  FStack.Reset;
  FExceptionObjectStack := TSimpleStack<IScriptObj>.Create;
  FRandSeed := cDefaultRandSeed xor (UInt64(System.Random($7FFFFFFF)) shl 15);
end;

// Destroy
//
destructor TdwsExecution.Destroy;
begin
  Assert(not Assigned(FSelfScriptObject));
  FExceptionObjectStack.Free;
  FStack.Finalize;
  FCallStack.Free;
  FFormatSettings.Free;
  inherited;
end;

// DoStep
//
procedure TdwsExecution.DoStep(Expr: TExprBase);

  procedure DoDebug(exec: TdwsExecution; Expr: TExprBase);
  begin
    exec.Debugger.DoDebug(exec, Expr);
    if exec.ProgramState = psRunningStopped then
      EScriptStopped.DoRaise(exec, Expr);
  end;

begin
  if ProgramState = psRunningStopped then
    EScriptStopped.DoRaise(Self, Expr)
  else if IsDebugging then
    DoDebug(Self, Expr);
end;

// Status_Offset
//
class function TdwsExecution.Status_Offset: Integer;
begin
  Assert(SizeOf(TdwsExecution(nil).FStatus) = 1);
  result := IntPtr(@TdwsExecution(nil).FStatus);
end;

// StackMixin_Offset
//
class function TdwsExecution.StackMixin_Offset: Integer;
begin
  result := IntPtr(@TdwsExecution(nil).FStack);
end;

// GetLastScriptErrorExpr
//
function TdwsExecution.GetLastScriptErrorExpr: TExprBase;
begin
  result := FLastScriptError;
end;

// SetScriptError
//
procedure TdwsExecution.SetScriptError(Expr: TExprBase);
begin
  if FLastScriptError = nil then
  begin
    FLastScriptError := Expr;
    FLastScriptCallStack := GetCallStack;
  end;
end;

// ClearScriptError
//
procedure TdwsExecution.ClearScriptError;
begin
  if FLastScriptError <> nil then
  begin
    FLastScriptError := nil;
    FLastScriptCallStack := nil;
  end;
end;

// GetDebugger
//
function TdwsExecution.GetDebugger: IDebugger;
begin
  result := FDebugger;
end;

// SetDebugger
//
procedure TdwsExecution.SetDebugger(const aDebugger: IDebugger);
begin
  FDebugger := aDebugger;
  FIsDebugging := (aDebugger <> nil);
end;

// StartDebug
//
procedure TdwsExecution.StartDebug;
begin
  FIsDebugging := Assigned(FDebugger);
  if FIsDebugging then
    FDebugger.StartDebug(Self);
end;

// StopDebug
//
procedure TdwsExecution.StopDebug;
begin
  if Assigned(FDebugger) then
    FDebugger.StopDebug(Self);
  FIsDebugging := False;
end;

// GetUserObject
//
function TdwsExecution.GetUserObject: TObject;
begin
  result := FUserObject;
end;

// SetUserObject
//
procedure TdwsExecution.SetUserObject(const value: TObject);
begin
  FUserObject := value;
end;

// GetStack
//
function TdwsExecution.GetStack: TStack;
begin
  result := @FStack;
end;

// GetProgramState
//
function TdwsExecution.GetProgramState: TProgramState;
begin
  result := FProgramState;
end;

// GetFormatSettings
//
function TdwsExecution.GetFormatSettings: TdwsFormatSettings;
begin
  if FFormatSettings = nil then
    FFormatSettings := TdwsFormatSettings.Create;
  result := FFormatSettings;
end;

// GetExecutionObject
//
function TdwsExecution.GetExecutionObject: TdwsExecution;
begin
  result := Self;
end;

// Random
//
{$IFOPT R+}
{$DEFINE RANGEON}
{$R-}
{$ELSE}
{$UNDEF RANGEON}
{$ENDIF}
{$Q-}

function TdwsExecution.Random: Double;
// Marsaglia, George (July 2003). "Xorshift RNGs". Journal of Statistical Software Vol. 8 (Issue  14).
const
  cScale: Double = (2.0 / $10000 / $10000 / $10000 / $10000); // 2^-63
var
  buf: UInt64;
begin
  if FRandSeed = 0 then
    buf := cDefaultRandSeed
  else
  begin
    buf := FRandSeed xor (FRandSeed shl 13);
    buf := buf xor (buf shr 17);
    buf := buf xor (buf shl 5);
  end;
  FRandSeed := buf;
  result := (buf shr 1) * cScale;
end;
{$IFDEF RANGEON}
{$R+}
{$UNDEF RANGEON}
{$ENDIF}

// Sleep
//
procedure TdwsExecution.Sleep(msec, sleepCycle: Integer);
var
  stopTicks, tStart, tNow: Int64;
begin
  // this is an abortable sleep with a granulosity
  if msec < 0 then
    Exit;
  FSleeping := True;
  tStart := GetSystemMilliseconds;
  if msec = 0 then
  begin
    // special case of relinquishing current time slice
    SystemSleep(0);
    tNow := GetSystemMilliseconds;
  end
  else
  begin
    tNow := tStart;
    stopTicks := tStart + msec;
    repeat
      msec := stopTicks - tNow;
      if msec < 0 then
        Break;
      if msec > sleepCycle then
        msec := sleepCycle;
      SystemSleep(msec);
      tNow := GetSystemMilliseconds;
    until ProgramState <> psRunning;
  end;
  FSleepTime := FSleepTime + tNow - tStart;
  FSleeping := False;
end;

// GetSleeping
//
function TdwsExecution.GetSleeping: Boolean;
begin
  result := FSleeping;
end;

// EnterExceptionBlock
//
procedure TdwsExecution.EnterExceptionBlock(var exceptObj: IScriptObj);
begin
  ExceptionObjectStack.Push(exceptObj);
end;

// LeaveExceptionBlock
//
procedure TdwsExecution.LeaveExceptionBlock;
begin
  ExceptionObjectStack.Peek := nil;
  ExceptionObjectStack.Pop;
end;

// SetRandSeed
//
procedure TdwsExecution.SetRandSeed(const val: UInt64);
begin
  if val = 0 then
    FRandSeed := cDefaultRandSeed
  else
    FRandSeed := val;
end;

// LocalizeSymbol
//
procedure TdwsExecution.LocalizeSymbol(aResSymbol: TResourceStringSymbol;
  var result: String);
begin
  LocalizeString(aResSymbol.value, result);
end;

// LocalizeString
//
procedure TdwsExecution.LocalizeString(const aString: String;
  var result: String);
begin
  result := aString;
end;

// ValidateFileName
//
function TdwsExecution.ValidateFileName(const path: String): String;
begin
  raise EScriptException.CreateFmt(RTE_UnauthorizedFilePath, [path]);
end;

// FileSystem
//
function TdwsExecution.FileSystem: IdwsFileSystemRW;
begin
  raise EScriptException.Create(RTE_UnauthorizedFileSystem);
end;

// DataContext_Create
//
procedure TdwsExecution.DataContext_Create(const data: TData; addr: Integer;
  var result: IDataContext);
begin
  result := FStack.CreateDataContext(data, addr);
end;

// DataContext_CreateEmpty
//
procedure TdwsExecution.DataContext_CreateEmpty(size: Integer;
  var result: IDataContext);
begin
  result := FStack.CreateEmpty(size);
end;

// DataContext_CreateValue
//
procedure TdwsExecution.DataContext_CreateValue(const value: Variant;
  var result: IDataContext);
var
  data: TData;
begin
  SetLength(data, 1);
  data[0] := value;
  result := FStack.CreateDataContext(data, 0);
end;

// DataContext_CreateBase
//
procedure TdwsExecution.DataContext_CreateBase(addr: Integer;
  var result: IDataContext);
begin
  FStack.InitBaseDataPtr(result, addr);
end;

// DataContext_CreateStack
//
procedure TdwsExecution.DataContext_CreateStack(addr: Integer;
  var result: IDataContext);
begin
  FStack.InitStackDataPtr(result, addr);
end;

// DataContext_CreateLevel
//
procedure TdwsExecution.DataContext_CreateLevel(Level, addr: Integer;
  var result: IDataContext);
begin
  FStack.InitDataPtrLevel(result, Level, addr);
end;

// DataContext_CreateOffset
//
procedure TdwsExecution.DataContext_CreateOffset(const data: IDataContext;
  offset: Integer; var result: IDataContext);
begin
  result := FStack.CreateDataContext(data.AsPData^, offset);
end;

// DataContext_Nil
//
function TdwsExecution.DataContext_Nil: IDataContext;
begin
  result := FStack.CreateDataContext(nil, 0);
end;

// GetStackPData
//
function TdwsExecution.GetStackPData: PData;
begin
  result := PData(IntPtr(@FStack) + TStackMixIn.vFDataOffset);
end;

// SuspendDebug
//
procedure TdwsExecution.SuspendDebug;
begin
  if FDebugSuspended = 0 then
  begin
    if FIsDebugging then
      FDebugSuspended := 1
    else
      FDebugSuspended := -1;
    FIsDebugging := False;
  end
  else if FDebugSuspended > 0 then
    Inc(FDebugSuspended)
  else
    Dec(FDebugSuspended);
end;

// ResumeDebug
//
procedure TdwsExecution.ResumeDebug;
begin
  case FDebugSuspended of
    0:
      Assert(False);
    1:
      begin
        FDebugSuspended := 0;
        FIsDebugging := True;
      end;
    -1:
      FDebugSuspended := 0;
  else
    if FDebugSuspended > 0 then
      Dec(FDebugSuspended)
    else
      Inc(FDebugSuspended);
  end;
end;

// GetEnvironment
//
function TdwsExecution.GetEnvironment: IdwsEnvironment;
begin
  result := FEnvironment;
end;

// SetEnvironment
//
procedure TdwsExecution.SetEnvironment(const val: IdwsEnvironment);
begin
  FEnvironment := val;
end;

// BeginInternalExecution
//
procedure TdwsExecution.BeginInternalExecution;
begin
  Inc(FInternalExecution);
end;

// EndInternalExecution
//
procedure TdwsExecution.EndInternalExecution;
begin
  Dec(FInternalExecution);
end;

// InternalExecution
//
function TdwsExecution.InternalExecution: Boolean;
begin
  result := (FInternalExecution > 0);
end;

// ------------------
// ------------------ TConditionSymbol ------------------
// ------------------

// Create
//
constructor TConditionSymbol.Create(const aScriptPos: TScriptPos;
  const cond: IBooleanEvalable; const msg: IStringEvalable);
begin
  inherited Create('', nil);
  FScriptPos := aScriptPos;
  FCondition := cond;
  FMessage := msg;
end;

// ------------------
// ------------------ TRuntimeErrorMessage ------------------
// ------------------

// AsInfo
//
function TRuntimeErrorMessage.AsInfo: String;
begin
  result := Text;
  if scriptPos.Defined then
    result := result + scriptPos.AsInfo
  else if Length(FCallStack) > 0 then
    result := result + ' in ' + FCallStack[High(FCallStack)
      ].Expr.FuncSymQualifiedName;
  if Length(FCallStack) > 0 then
  begin
    result := result + #13#10 + TExprBase.CallStackToString(FCallStack);
  end;
  result := Format(MSG_RuntimeError, [result]);
end;

// ------------------
// ------------------ TdwsRuntimeMessageList ------------------
// ------------------

// AddRuntimeError
//
procedure TdwsRuntimeMessageList.AddRuntimeError(const Text: String);
begin
  AddRuntimeError(cNullPos, Text, nil);
end;

// AddRuntimeError
//
procedure TdwsRuntimeMessageList.AddRuntimeError(e: Exception);
begin
  AddRuntimeError(e.ClassName + ': ' + e.Message);
end;

// AddRuntimeError
//
procedure TdwsRuntimeMessageList.AddRuntimeError(const scriptPos: TScriptPos;
  const Text: String; const CallStack: TdwsExprLocationArray);
var
  msg: TRuntimeErrorMessage;
begin
  msg := TRuntimeErrorMessage.Create(Self, Text, scriptPos);
  msg.FCallStack := CallStack;
end;

// ------------------
// ------------------ TOperatorSymbol ------------------
// ------------------

// Create
//
constructor TOperatorSymbol.Create(const aTokenType: TTokenType);
begin
  inherited Create(UnifiedString('operator ' + cTokenStrings[aTokenType]), nil);
  FToken := aTokenType;
end;

// AddParam
//
procedure TOperatorSymbol.AddParam(p: TTypeSymbol);
var
  n: Integer;
begin
  n := Length(FParams);
  SetLength(FParams, n + 1);
  FParams[n] := p;
end;

// GetCaption
//
function TOperatorSymbol.GetCaption: String;
var
  i: Integer;
begin
  result := 'operator ' + cTokenStrings[Token] + ' (';
  for i := 0 to High(params) do
  begin
    if i > 0 then
      result := result + ', ';
    result := result + params[i].typ.Caption;
  end;
  result := result + ') : ' + typ.Caption + ' uses ' + FUsesSym.Name;
end;

// ------------------
// ------------------ TResolvedInterfaces ------------------
// ------------------

// SameItem
//
function TResolvedInterfaces.SameItem(const item1,
  item2: TResolvedInterface): Boolean;
begin
  result := (item1.IntfSymbol = item2.IntfSymbol);
end;

// GetItemHashCode
//
function TResolvedInterfaces.GetItemHashCode(const item1: TResolvedInterface)
  : Cardinal;
begin
  result := SimplePointerHash(item1.IntfSymbol);
end;

// ------------------
// ------------------ TAnyFuncSymbol ------------------
// ------------------

// IsCompatible
//
function TAnyFuncSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym.AsFuncSymbol <> nil);
end;

// IsCompatibleWithAnyFuncSymbol
//
function TAnyFuncSymbol.IsCompatibleWithAnyFuncSymbol: Boolean;
begin
  result := True;
end;

// Initialize
//
procedure TAnyFuncSymbol.Initialize(const Msgs: TdwsCompileMessageList);
begin
  // nothing
end;

// ------------------
// ------------------ TResourceStringSymbol ------------------
// ------------------

// Create
//
constructor TResourceStringSymbol.Create(const aName, aValue: String);
begin
  inherited Create(aName, nil);
  FValue := aValue;
  FIndex := -1;
end;

// GetCaption
//
function TResourceStringSymbol.GetCaption: String;
begin
  result := 'resourcestring ' + Name;
end;

// GetDescription
//
function TResourceStringSymbol.GetDescription: String;
begin
  result := value;
  FastStringReplace(result, '''', '''''');
  result := 'resourcestring ' + Name + ' = ''' + result + '''';
end;

// ------------------
// ------------------ TResourceStringSymbolList ------------------
// ------------------

// ComputeIndexes
//
procedure TResourceStringSymbolList.ComputeIndexes;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Index := i;
end;

// ------------------
// ------------------ TFuncSymbolList ------------------
// ------------------

// ContainsChildMethodOf
//
function TFuncSymbolList.ContainsChildMethodOf(methSym: TMethodSymbol): Boolean;
var
  i: Integer;
  funcSym: TFuncSymbol;
  meth: TMethodSymbol;
begin
  for i := 0 to Count - 1 do
  begin
    funcSym := Items[i];
    if funcSym is TMethodSymbol then
    begin
      meth := TMethodSymbol(funcSym);
      repeat
        if meth = methSym then
          Exit(True);
        meth := meth.ParentMeth;
      until meth = nil;
    end;
  end;
  result := False;
end;

// ------------------
// ------------------ THelperSymbol ------------------
// ------------------

// Create
//
constructor THelperSymbol.Create(const Name: String; aUnit: TSymbol;
  aForType: TTypeSymbol; priority: Integer);
begin
  inherited Create(name, aUnit);
  FForType := aForType;
  FUnAliasedForType := aForType.UnAliasedType;
  if FUnAliasedForType is TStructuredTypeSymbol then
    FMetaForType := TStructuredTypeSymbol(FUnAliasedForType).MetaSymbol;
end;

// IsCompatible
//
function THelperSymbol.IsCompatible(typSym: TTypeSymbol): Boolean;
begin
  result := (typSym = Self);
end;

// IsType
//
function THelperSymbol.IsType: Boolean;
begin
  result := False;
end;

// AllowDefaultProperty
//
function THelperSymbol.AllowDefaultProperty: Boolean;
begin
  result := False;
end;

// CreateSelfParameter
//
function THelperSymbol.CreateSelfParameter(methSym: TMethodSymbol): TDataSymbol;
var
  meta: TStructuredTypeMetaSymbol;
begin
  if methSym.IsClassMethod then
  begin
    if ForType is TStructuredTypeSymbol then
    begin
      meta := TStructuredTypeSymbol(ForType).MetaSymbol;
      if meta <> nil then
        result := TParamSymbol.Create(SYS_SELF, meta)
      else
        result := nil;
    end
    else
      result := nil
  end
  else
  begin
    if ForType.IsClassSymbol or (ForType is TInterfaceSymbol) or
      (ForType is TDynamicArraySymbol) then
      result := TParamSymbol.Create(SYS_SELF, ForType)
    else
      result := CreateConstParamSymbol(SYS_SELF, ForType);
  end;
  if result <> nil then
  begin
    methSym.params.AddSymbol(result);
    if result.typ is TCompositeTypeSymbol then
      methSym.params.AddParent(TCompositeTypeSymbol(result.typ).Members)
    else if result.typ is TStructuredTypeMetaSymbol then
      methSym.params.AddParent(TStructuredTypeMetaSymbol(result.typ)
        .StructSymbol.Members)
  end;
end;

// CreateAnonymousMethod
//
function THelperSymbol.CreateAnonymousMethod(aFuncKind: TFuncKind;
  aVisibility: TdwsVisibility; aCreateOptions: TMethodCreateOptions)
  : TMethodSymbol;
begin
  result := TSourceMethodSymbol.Create('', aFuncKind, Self, aVisibility,
    aCreateOptions);
  if (mcoClassMethod in aCreateOptions) and (not ForType.HasMetaSymbol) then
    TSourceMethodSymbol(result).SetIsStatic;
end;

// Initialize
//
procedure THelperSymbol.Initialize(const Msgs: TdwsCompileMessageList);
begin
  CheckMethodsImplemented(Msgs);
end;

// HelpsType
//
function THelperSymbol.HelpsType(typ: TTypeSymbol): Boolean;
begin
  if typ = ForType then
    result := True
  else if (typ = nil) or Strict then
    result := False
  else if typ.IsOfType(FUnAliasedForType) then
    result := True
  else if FMetaForType <> nil then
    result := typ.IsOfType(FMetaForType)
  else
    result := False;
end;

// GetMetaSymbol
//
function THelperSymbol.GetMetaSymbol: TStructuredTypeMetaSymbol;
begin
  result := FMetaForType;
end;

// ------------------
// ------------------ THelperSymbols ------------------
// ------------------

// AddHelper
//
function THelperSymbols.AddHelper(helper: THelperSymbol): Boolean;
begin
  Add(helper);
  result := False;
end;

// ------------------
// ------------------ TAliasMethodSymbol ------------------
// ------------------

// GetDeclarationPosition
//
function TAliasMethodSymbol.GetDeclarationPosition: TScriptPos;
begin
  result := Alias.GetDeclarationPosition;
end;

// GetImplementationPosition
//
function TAliasMethodSymbol.GetImplementationPosition: TScriptPos;
begin
  result := Alias.GetImplementationPosition;
end;

// IsPointerType
//
function TAliasMethodSymbol.IsPointerType: Boolean;
begin
  result := Alias.IsPointerType;
end;

// ParamsDescription
//
function TAliasMethodSymbol.ParamsDescription: String;
begin
  result := params.Description(1);
end;

// SpecializeType
//
function TAliasMethodSymbol.SpecializeType(const context
  : ISpecializationContext): TTypeSymbol;
begin
  context.AddCompilerErrorFmt(CPE_SpecializationNotSupportedYet, [ClassName]);
  result := Self;
end;

// ------------------
// ------------------ TPerfectMatchEnumerator ------------------
// ------------------

// Callback
//
function TPerfectMatchEnumerator.callback(sym: TSymbol): Boolean;
var
  locSym: TFuncSymbol;
begin
  locSym := sym.AsFuncSymbol;
  if locSym <> nil then
  begin
    if locSym.Level = funcSym.Level then
    begin
      if funcSym.IsSameOverloadOf(locSym) then
      begin
        Match := locSym;
        Exit(True);
      end;
    end;
  end;
  result := False;
end;

end.
