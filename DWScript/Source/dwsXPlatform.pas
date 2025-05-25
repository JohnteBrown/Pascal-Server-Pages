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
unit dwsXPlatform;

{$I dws.inc}
//
// This unit should concentrate all non-UI cross-platform aspects,
// cross-Delphi versions, ifdefs and other conditionals
//
// no ifdefs in the main code.

{$WARN SYMBOL_PLATFORM OFF}
{$IFDEF FPC}
{$DEFINE VER200}  // FPC compatibility = D2009
{$ENDIF}
{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}  // Define Delphi <==> FPC "WINDOWS" Compiler Switch
{$ENDIF}
{$IFDEF LINUX}
{$DEFINE UNIX}  // Define Delphi <==> FPC "UNIX" Compiler Switch
{$ENDIF}
{$ifdef UNIX}
{$DEFINE POSIXSYSLOG} // If defined Posix Syslog is used in Unix environments
{$endif}

interface

uses
  System.Classes, System.SysUtils, System.Types,
{$IFDEF DELPHI_XE3_PLUS}
  DateUtils,
{$ENDIF}
{$IFDEF FPC}
{$IFDEF WINDOWS}
  Windows
{$ELSE}
  LCLIntf
{$ENDIF}
{$ELSE}
    System.IOUtils,
{$IFDEF WINDOWS}
  Winapi.Windows, System.Win.Registry
{$ENDIF}
{$IFDEF UNIX}
{$IFDEF POSIXSYSLOG}Posix.Syslog, {$ENDIF}
  System.Internal.ICU, System.SyncObjs,
  Posix.Unistd, Posix.Time, Posix.Pthread, Posix.Base, Posix.Stdlib,
  Posix.Stdio,
  dwsXPlatformTimer
{$ENDIF}
{$ENDIF}
    ;

const
{$IFDEF UNIX}
  cLineTerminator = #10;
{$ELSE}
  cLineTerminator = #13#10;
{$ENDIF}
  // following is missing from D2010
  INVALID_HANDLE_VALUE = NativeUInt(-1);

{$IFDEF FPC}
  // FreePascal RTL declares this constant, but does not support it,
  // so it just leads to runtime crashes, this attempts to trigger compile-time crashes instead
  varUString = 'varUString is not supported by FreePascal';
{$ENDIF}

type

  // see http://delphitools.info/2011/11/30/fixing-tcriticalsection/
{$HINTS OFF}
{$IFDEF UNIX}
  TdwsCriticalSection = class(TCriticalSection)
  public
    function TryEnterOrTimeout(delayMSec: Integer): Boolean;
  end;
{$ELSE}

  TdwsCriticalSection = class
  private
    FDummy: array [0 .. 95 - SizeOf(TRTLCRiticalSection) - 2 *
      SizeOf(Pointer)] of Byte;
    FCS: TRTLCRiticalSection;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Enter;
    procedure Leave;

    function TryEnter: Boolean;
    function TryEnterOrTimeout(delayMSec: Integer): Boolean;
  end;
{$ENDIF}

  IMultiReadSingleWrite = interface
    procedure BeginRead;
    function TryBeginRead: Boolean;
    procedure EndRead;

    procedure BeginWrite;
    function TryBeginWrite: Boolean;
    procedure EndWrite;
  end;

  TMultiReadSingleWriteState = (mrswUnlocked, mrswReadLock, mrswWriteLock);

{$IFDEF UNIX}{$DEFINE SRW_FALLBACK}{$ENDIF}

  TMultiReadSingleWrite = class(TInterfacedObject, IMultiReadSingleWrite)
  private
{$IFNDEF SRW_FALLBACK}
    FSRWLock: Pointer;
    FDummy: array [0 .. 95 - 4 * SizeOf(Pointer)] of Byte; // padding
{$ELSE}
    FLock: TdwsCriticalSection;
{$ENDIF}
  public
{$IFDEF SRW_FALLBACK}
    constructor Create;
    destructor Destroy; override;
{$ENDIF}
    procedure BeginRead; inline;
    function TryBeginRead: Boolean; inline;
    procedure EndRead; inline;

    procedure BeginWrite; inline;
    function TryBeginWrite: Boolean; inline;
    procedure EndWrite; inline;

    // use for diagnostic only
    function State: TMultiReadSingleWriteState;
  end;

{$HINTS ON}

procedure SetDecimalSeparator(c: Char);
function GetDecimalSeparator: Char;

type
  TCollectFileProgressEvent = procedure(const directory: TFileName;
    var skipScan: Boolean) of object;

procedure CollectFiles(const directory, fileMask: TFileName; list: TStrings;
  recurseSubdirectories: Boolean = False;
  onProgress: TCollectFileProgressEvent = nil);
procedure CollectSubDirs(const directory: TFileName; list: TStrings);

type
{$IFNDEF FPC}
{$IF CompilerVersion<22.0}
  // NativeUInt broken in D2009, and PNativeInt is missing in D2010
  // http://qc.embarcadero.com/wc/qcmain.aspx?d=71292
  NativeInt = Integer;
  PNativeInt = ^NativeInt;
  NativeUInt = Cardinal;
  PNativeUInt = ^NativeUInt;
{$IFEND}
{$ENDIF}
{$IFDEF FPC}
  TBytes = array of Byte;

  RawByteString = String;

  PNativeInt = ^NativeInt;
  PUInt64 = ^UInt64;
{$ENDIF}

  TdwsLargeInteger = record
    case Integer of
      0:
        (LowPart: DWORD;
          HighPart: DWORD;
        );
      1:
        (QuadPart: Int64
        );
  end;

  TPath = class
    class function GetTempPath: String; static;
    class function GetTempFileName: String; static;
  end;

  TFile = class
    class function ReadAllBytes(const filename: String): TBytes; static; inline;
  end;

  TdwsThread = class(TThread)
{$IFNDEF FPC}
{$IFDEF VER200}
    procedure Start;
{$ENDIF}
{$ENDIF}
    procedure SetTimeCriticalPriority;
  end;

  // Wrap in a record so it is not assignment compatible without explicit casts
  // Internal representation is UnixTime in milliseconds (same as JavaScript)
  TdwsDateTime = record
  private
    FValue: Int64;

    function GetAsUnixTime: Int64;
    procedure SetAsUnixTime(const val: Int64);

    function GetAsFileTime: TFileTime;
    procedure SetAsFileTime(const val: TFileTime);
    function GetAsDosDateTime: Integer;

    function GetAsLocalDateTime: TDateTime;
    procedure SetAsLocalDateTime(const val: TDateTime);
    function GetAsUTCDateTime: TDateTime;
    procedure SetAsUTCDateTime(const val: TDateTime);

  public
    class function Now: TdwsDateTime; static;
    class function FromLocalDateTime(const dt: TDateTime): TdwsDateTime; static;

    procedure Clear; inline;
    function IsZero: Boolean; inline;

    class operator Equal(const a, b: TdwsDateTime): Boolean; static; inline;
    class operator NotEqual(const a, b: TdwsDateTime): Boolean; static; inline;
    class operator GreaterThan(const a, b: TdwsDateTime): Boolean;
      static; inline;
    class operator GreaterThanOrEqual(const a, b: TdwsDateTime): Boolean;
      static; inline;
    class operator LessThan(const a, b: TdwsDateTime): Boolean; static; inline;
    class operator LessThanOrEqual(const a, b: TdwsDateTime): Boolean;
      static; inline;

    function MillisecondsAheadOf(const d: TdwsDateTime): Int64; inline;
    procedure IncMilliseconds(const msec: Int64); inline;

    property Value: Int64 read FValue write FValue;

    property AsUnixTime: Int64 read GetAsUnixTime write SetAsUnixTime;
    property AsJavaScriptTime: Int64 read FValue write FValue;
    property AsFileTime: TFileTime read GetAsFileTime write SetAsFileTime;
    property AsDosDateTime: Integer read GetAsDosDateTime;

    property AsLocalDateTime: TDateTime read GetAsLocalDateTime
      write SetAsLocalDateTime;
    property AsUTCDateTime: TDateTime read GetAsUTCDateTime
      write SetAsUTCDateTime;
  end;

  // 64bit system clock reference in milliseconds since boot
function GetSystemMilliseconds: Int64;
function UTCDateTime: TDateTime;
function UnixTime: Int64;
function EpochTimeStamp: Int64;

function LocalDateTimeToUTCDateTime(t: TDateTime): TDateTime;
function UTCDateTimeToLocalDateTime(t: TDateTime): TDateTime;

function SystemMillisecondsToUnixTime(t: Int64): Int64;
function UnixTimeToSystemMilliseconds(ut: Int64): Int64;

procedure SystemSleep(msec: Integer);

function FirstWideCharOfString(const s: String; const default: WideChar = #0)
  : WideChar; inline;
procedure CodePointToUnicodeString(c: Integer; var result: UnicodeString);
procedure CodePointToString(const c: Integer; var result: String); inline;

{$IFNDEF FPC}
function UnicodeCompareStr(const S1, S2: String): Integer; inline;
function UnicodeStringReplace(const s, oldPattern, newPattern: String;
  flags: TReplaceFlags): String; inline;
{$ENDIF}
function UnicodeCompareP(p1: PWideChar; n1: Integer; p2: PWideChar; n2: Integer)
  : Integer; overload;
function UnicodeCompareP(p1, p2: PWideChar; n: Integer): Integer; overload;
function UnicodeCompareEx(const a, b: UnicodeString;
  const locale: UnicodeString; caseSensitive: Boolean): Integer;

procedure UnicodeLowerCase(const s: UnicodeString;
  var result: UnicodeString); overload;
function UnicodeLowerCase(const s: UnicodeString): UnicodeString; overload;
  inline; deprecated 'use procedure form';

procedure UnicodeUpperCase(const s: UnicodeString;
  var result: UnicodeString); overload;
function UnicodeUpperCase(const s: UnicodeString): UnicodeString; overload;
  inline; deprecated 'use procedure form';

{$IFDEF FPC}
function UnicodeLowerCase(const s: String): String; overload;
function UnicodeUpperCase(const s: String): String; overload;
{$ENDIF}
function ASCIICompareText(const S1, S2: String): Integer; inline;
function ASCIISameText(const S1, S2: String): Boolean; inline;

function NormalizeString(const s, form: String): String;
function StripAccents(const s: String): String;

function InterlockedIncrement(var val: Integer): Integer; overload;
{$IFDEF PUREPASCAL} inline; {$ENDIF} deprecated 'use AtomicIncrement';
function InterlockedDecrement(var val: Integer): Integer;
{$IFDEF PUREPASCAL} inline; {$ENDIF} deprecated 'use AtomicDecrement';

procedure FastInterlockedIncrement(var val: Integer);
{$IFDEF PUREPASCAL} inline; {$ENDIF}
procedure FastInterlockedDecrement(var val: Integer);
{$IFDEF PUREPASCAL} inline; {$ENDIF}
function InterlockedExchangePointer(var target: Pointer; val: Pointer): Pointer;
{$IFDEF PUREPASCAL} inline; {$ENDIF}
function InterlockedCompareExchangePointer(var destination: Pointer;
  exchange, comparand: Pointer): Pointer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
procedure SetThreadName(const threadName: PAnsiChar;
  threadID: Cardinal = Cardinal(-1));

procedure OutputDebugString(const msg: String);

procedure WriteToOSEventLog(const logName, logCaption, logDetails: String;
  const logRawData: RawByteString = ''); overload;

{$IFDEF FPC}
procedure VarCopy(out dest: Variant; const src: Variant); inline;
{$ELSE}
function VarToUnicodeStr(const v: Variant): String; inline;
{$ENDIF}
{$IFDEF FPC}
function Utf8ToUnicodeString(const buf: RawByteString): UnicodeString; inline;
{$ENDIF}
function RawByteStringToBytes(const buf: RawByteString): TBytes;
function BytesToRawByteString(const buf: TBytes; startIndex: Integer = 0)
  : RawByteString; overload;
function BytesToRawByteString(p: Pointer; size: Integer)
  : RawByteString; overload;

function PosExA(const needle, haystack: RawByteString;
  hayStackOffset: Integer): Integer;

procedure BytesToScriptString(const p: PByteArray; n: Integer;
  var result: UnicodeString);

procedure WordsToBytes(src: PWordArray; dest: PByteArray; nbWords: Integer);
procedure BytesToWords(src: PByteArray; dest: PWordArray; nbBytes: Integer);
procedure BytesToWordsInPlace(p: Pointer; n: NativeInt);

function LoadDataFromFile(const filename: TFileName): TBytes;
procedure SaveDataToFile(const filename: TFileName; const data: TBytes);

function LoadRawBytesFromFile(const filename: TFileName): RawByteString;
function SaveRawBytesToFile(const filename: TFileName;
  const data: RawByteString): Integer;

procedure LoadRawBytesAsScriptStringFromFile(const filename: TFileName;
  var result: String);

function LoadTextFromBuffer(const buf: TBytes): UnicodeString;
function LoadTextFromRawBytes(const buf: RawByteString): UnicodeString;
function LoadTextFromStream(aStream: TStream): UnicodeString;
function LoadTextFromFile(const filename: TFileName): UnicodeString;
procedure SaveTextToUTF8File(const filename: TFileName; const text: String);
procedure AppendTextToUTF8File(const filename: TFileName;
  const text: UTF8String);

type
  TOpenFileOption = (ofoNoRaiseError);
  TOpenFileOptions = set of TOpenFileOption;

function OpenFileForSequentialReadOnly(const filename: TFileName;
  const options: TOpenFileOptions = []): THandle;
function OpenFileForSequentialWriteOnly(const filename: TFileName;
  const options: TOpenFileOptions = []): THandle;

function CloseFileHandle(hFile: THandle): Boolean;
function FileWrite(hFile: THandle; buffer: Pointer; byteCount: Int64): Int64;
function FileRead(hFile: THandle; buffer: Pointer; byteCount: Int64): Int64;
function FileFlushBuffers(hFile: THandle): Boolean;
function SetEndOfFile(hFile: THandle): Boolean;
function FileCopy(const existing, new: TFileName;
  failIfExists: Boolean): Boolean;
function FileMove(const existing, new: TFileName): Boolean;
function FileDelete(const filename: TFileName): Boolean;
function FileRename(const oldName, newName: TFileName): Boolean;
function FileSize(const name: TFileName): Int64;
function FileCreationTime(const name: TFileName): TdwsDateTime; overload;
function FileDateTime(const name: TFileName; lastAccess: Boolean = False)
  : TdwsDateTime; overload;
function FileDateTime(hFile: THandle; lastAccess: Boolean = False)
  : TdwsDateTime; overload;
procedure FileSetDateTime(hFile: THandle; const aDateTime: TdwsDateTime);
function DeleteDirectory(const path: String; recursive: Boolean = True)
  : Boolean;

type
  TdwsMemoryMappedFile = record
    FileHandle: THandle;
    MapHandle: THandle;
    Base: Pointer;
    size: Int64;

    function DetectEncoding(var encoding: TEncoding): Integer;
  end;

function FileMemoryMapReadOnly(const filename: TFileName): TdwsMemoryMappedFile;
procedure FileUnMap(var map: TdwsMemoryMappedFile);

function DirectSet8087CW(newValue: Word): Word; register;
function DirectSetMXCSR(newValue: Word): Word; register;

function SwapBytes(v: Cardinal): Cardinal;
procedure SwapBytesBlock(src, dest: PByte; nb: Integer);
procedure SwapBytesInt32(src, dest: PUInt32);
procedure SwapBytesInt64(src, dest: PInt64);

function RDTSC: UInt64;

function GetCurrentUserName: String;

{$IFNDEF FPC}
// Generics helper functions to handle Delphi 2009 issues - HV
function TtoObject(const t): TObject; inline;
function TtoPointer(const t): Pointer; inline;
procedure GetMemForT(var t; size: Integer); inline;
{$ENDIF}
procedure InitializeWithDefaultFormatSettings(var fmt: TFormatSettings);

type
  TTimerEvent = procedure of object;

  ITimer = interface
    procedure Cancel;
  end;

  TTimerTimeout = class(TInterfacedObject, ITimer)
  private
    FTimer: THandle;
    FOnTimer: TTimerEvent;

  public
    class function Create(delayMSec: Cardinal; onTimer: TTimerEvent): ITimer;
    destructor Destroy; override;

    procedure Cancel;
  end;

{$IFNDEF SRW_FALLBACK}

procedure AcquireSRWLockExclusive(var SRWLock: Pointer); stdcall;
  external 'kernel32.dll';
function TryAcquireSRWLockExclusive(var SRWLock: Pointer): ByteBool; stdcall;
  external 'kernel32.dll';
procedure ReleaseSRWLockExclusive(var SRWLock: Pointer); stdcall;
  external 'kernel32.dll';

procedure AcquireSRWLockShared(var SRWLock: Pointer); stdcall;
  external 'kernel32.dll';
function TryAcquireSRWLockShared(var SRWLock: Pointer): ByteBool; stdcall;
  external 'kernel32.dll';
procedure ReleaseSRWLockShared(var SRWLock: Pointer); stdcall;
  external 'kernel32.dll';
{$ENDIF}

type
  TModuleVersion = record
    Major, Minor: Word;
    Release, Build: Word;
    function GetAsString: String;
    procedure SetAsString(const s: String);
    property AsString: String read GetAsString write SetAsString;
  end;

  TApplicationVersionOption = (avoBitness
    // if set, mention 32bit or 64bit after version number
    );
  TApplicationVersionOptions = set of TApplicationVersionOption;

function GetModuleVersion(instance: THandle;
  var version: TModuleVersion): Boolean;
function GetApplicationVersion(var version: TModuleVersion): Boolean;
function ApplicationVersion(const options: TApplicationVersionOptions =
  [avoBitness]): String;

type
  TCPUFeature = (cpuFeaturesRead,
    // internal flag to indicate features were read
    cpuSSE41, // SSE4.1
    cpuFMA, // FMA3
    cpuAVX, // AVX
    cpuAVX2 // AVX2
    );
  TCPUFeatures = set of TCPUFeature;

function WIN64CPUFeatures: TCPUFeatures;
function Win64SSE41Supported: Boolean; inline;
function Win64FMASupported: Boolean; inline;
function Win64AVX2Supported: Boolean; inline;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  System.Masks, System.Variants, System.StrUtils;

// FileMemoryMapReadOnly
//
function FileMemoryMapReadOnly(const filename: TFileName): TdwsMemoryMappedFile;
{$IFDEF WINDOWS}
begin
  result.size := 0;
  result.Base := nil;
  result.MapHandle := 0;
  result.FileHandle := FileOpen(filename, fmOpenRead + fmShareDenyNone);
  if result.FileHandle = INVALID_HANDLE_VALUE then
  begin
    result.FileHandle := 0;
    RaiseLastOSError;
  end;
  try
    if not GetFileSizeEx(result.FileHandle, result.size) then
      RaiseLastOSError;
    if result.size > 0 then
    begin
      result.MapHandle := CreateFileMapping(result.FileHandle, nil,
        PAGE_READONLY, 0, 0, nil);
      if result.MapHandle = 0 then
        RaiseLastOSError;
      try
        result.Base := MapViewOfFile(result.MapHandle, FILE_MAP_READ, 0, 0,
          result.size);
        if result.Base = nil then
          RaiseLastOSError;
      except
        CloseHandle(result.MapHandle);
        result.MapHandle := 0;
        raise;
      end;
    end;
  except
    FileClose(result.FileHandle);
    result.FileHandle := 0;
    raise;
  end;
end;
{$ELSE}

begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$ENDIF}

// FileUnMap
//
procedure FileUnMap(var map: TdwsMemoryMappedFile);
{$IFDEF WINDOWS}
begin
  if map.Base <> nil then
  begin
    UnmapViewOfFile(map.Base);
    map.Base := nil;
  end;
  if map.MapHandle <> 0 then
  begin
    CloseHandle(map.MapHandle);
    map.MapHandle := 0;
  end;
  if map.FileHandle <> 0 then
  begin
    FileClose(map.FileHandle);
    map.FileHandle := 0;
  end;
end;
{$ELSE}

begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$ENDIF}

// DetectEncoding
//
function TdwsMemoryMappedFile.DetectEncoding(var encoding: TEncoding): Integer;

  function DetectComplexEncoding(var encoding: TEncoding): Integer;
  var
    buf: TBytes;
    n: Integer;
  begin
    n := size;
    if n > 64 then
      n := 64;
    SetLength(buf, n);
    System.Move(Base^, buf[0], n);
    result := TEncoding.GetBufferEncoding(buf, encoding);
  end;

begin
  if (size <= 0) or (Base = nil) then
    Exit(0);

  // shortcut for UTF-8 BOM
  if size >= 3 then
  begin
    if (PWord(Base)^ = $BBEF) and (Ord(PAnsiChar(Base)[2]) = $BF) then
    begin
      encoding := TEncoding.UTF8;
      Exit(3);
    end;
  end;
  result := DetectComplexEncoding(encoding);
end;

{$IFDEF FPC}

type
  TFindExInfoLevels = FINDEX_INFO_LEVELS;
{$ENDIF}

  // GetSystemTimeMilliseconds
  //
function GetSystemTimeMilliseconds: Int64; stdcall;
begin
  result := TdwsDateTime.Now.Value;
end;

// GetSystemMilliseconds
//
var
  vGetSystemMilliseconds: function: Int64; stdcall;

function GetSystemMilliseconds: Int64;
{$IFDEF WIN32_ASM}
asm
  jmp [vGetSystemMilliseconds]
  {$ELSE}
begin
  result := vGetSystemMilliseconds;
{$ENDIF}
end;

// InitializeGetSystemMilliseconds
//
procedure InitializeGetSystemMilliseconds;
{$IFDEF WINDOWS}
var
  h: THandle;
begin
  h := LoadLibrary('kernel32.dll');
  vGetSystemMilliseconds := GetProcAddress(h, 'GetTickCount64');
end;
{$ELSE}

begin
  if not Assigned(vGetSystemMilliseconds) then
    vGetSystemMilliseconds := @GetSystemTimeMilliseconds;
end;
{$ENDIF}

// UTCDateTime
//
function UTCDateTime: TDateTime;
{$IFDEF WINDOWS}
var
  systemTime: TSystemTime;
begin
  FillChar(systemTime, SizeOf(systemTime), 0);
  GetSystemTime(systemTime);
  with systemTime do
    result := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute,
      wSecond, wMilliseconds);
end;
{$ELSE}

begin
  result := TTimeZone.Local.ToUniversalTime(Now);
end;
{$ENDIF}

// UnixTime
//
function UnixTime: Int64;
{$IFDEF WINDOWS}
const
  cUNIX_TIME_START: Int64 = $019DB1DED53E8000;
  cTICKS_PER_SECOND: Int64 = 10000000; // 100ns
var
  ft: FILETIME;
  t: TdwsLargeInteger;
begin
  GetSystemTimeAsFileTime(ft);
  t.LowPart := ft.dwLowDateTime;
  t.HighPart := ft.dwHighDateTime;
  result := (t.QuadPart - cUNIX_TIME_START) div cTICKS_PER_SECOND;
end;
{$ELSE}

begin
  result := Trunc(UTCDateTime * 86400) - Int64(25569) * 86400;
end;
{$ENDIF}

// EpochTimeStamp
//
function EpochTimeStamp: Int64;
{$IFDEF WINDOWS}
const
  cUNIX_TIME_START: Int64 = $019DB1DED53E8000;
  cTICKS_PER_SECOND: Int64 = 10000000; // 100ns
  cEPOCH_TICKS_PER_SECOND: Int64 = 1000;
var
  ft: FILETIME;
  t: TdwsLargeInteger;
begin
  GetSystemTimeAsFileTime(ft);
  t.LowPart := ft.dwLowDateTime;
  t.HighPart := ft.dwHighDateTime;
  result := (t.QuadPart - cUNIX_TIME_START)
    div (cTICKS_PER_SECOND div cEPOCH_TICKS_PER_SECOND);
end;
{$ELSE}

begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$ENDIF}
{$IFNDEF LINUX}

type
  TDynamicTimeZoneInformation = record
    Bias: Longint;
    StandardName: array [0 .. 31] of WCHAR;
    StandardDate: TSystemTime;
    StandardBias: Longint;
    DaylightName: array [0 .. 31] of WCHAR;
    DaylightDate: TSystemTime;
    DaylightBias: Longint;
    TimeZoneKeyName: array [0 .. 127] of WCHAR;
    DynamicDaylightTimeDisabled: Boolean;
  end;

  PDynamicTimeZoneInformation = ^TDynamicTimeZoneInformation;

function GetDynamicTimeZoneInformation(var pTimeZoneInformation
  : TDynamicTimeZoneInformation): DWORD; stdcall; external 'kernel32'
{$IFNDEF FPC}delayed{$ENDIF};
function GetTimeZoneInformationForYear(wYear: USHORT;
  lpDynamicTimeZoneInformation: PDynamicTimeZoneInformation;
  var lpTimeZoneInformation: TTimeZoneInformation): BOOL; stdcall;
  external 'kernel32' {$IFNDEF FPC}delayed{$ENDIF};
function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation
  : pTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL;
  stdcall; external 'kernel32' {$IFNDEF FPC}delayed{$ENDIF};
{$ENDIF}

// LocalDateTimeToUTCDateTime
//
type
  TTimeZoneCracker = class(TTimeZone);

function LocalDateTimeToUTCDateTime(t: TDateTime): TDateTime;
{$IFDEF DELPHI_XE3_PLUS}
var
  utcOffsetInSeconds, dstOffset: Int64;
  timeType: TLocalTimeType;
begin
  // ideally this would be just this line, but DoGetOffsetsAndType has the undesirable behavior
  // of unconditionnally raising an exception in case of a time falling in DST "hole",
  // which is something that can happen in historic data when a clock did not get adjusted
  // we handle it the same way as the ambiguous one here: we ignore it and assume DST off

  // Result := TTimeZone.Local.ToUniversalTime(t, False);

  TTimeZoneCracker(TTimeZone.Local).DoGetOffsetsAndType(t, utcOffsetInSeconds,
    dstOffset, timeType);
  case timeType of
    lttDaylight, lttAmbiguous:
      utcOffsetInSeconds := utcOffsetInSeconds + dstOffset;
  end;

  if utcOffsetInSeconds <> 0 then
    result := IncMilliSecond(t, -utcOffsetInSeconds * 1000)
  else
    result := t;
end;
{$ELSE}{$IFDEF FPC}

begin
  result := LocalTimeToUniversal(t);
end;
{$ELSE}

var
  localSystemTime, universalSystemTime: TSystemTime;
  tzDynInfo: TDynamicTimeZoneInformation;
  tzInfo: TTimeZoneInformation;
  y, m, d: Word;
begin
  DateTimeToSystemTime(t, localSystemTime);
  if GetDynamicTimeZoneInformation(tzDynInfo) = TIME_ZONE_ID_INVALID then
    RaiseLastOSError;
  DecodeDate(t, y, m, d);
  if not GetTimeZoneInformationForYear(y, @tzDynInfo, tzInfo) then
    RaiseLastOSError;
  if not TzSpecificLocalTimeToSystemTime(@tzInfo, localSystemTime,
    universalSystemTime) then
    RaiseLastOSError;
  result := SystemTimeToDateTime(universalSystemTime);
end;
{$ENDIF}{$ENDIF}

// UTCDateTimeToLocalDateTime
//
function UTCDateTimeToLocalDateTime(t: TDateTime): TDateTime;
{$IFDEF DELPHI_XE3_PLUS}
begin
  result := TTimeZone.Local.ToLocalTime(t);
end;
{$ELSE}{$IFDEF FPC}

begin
  result := UniversalTimeToLocal(t);
end;
{$ELSE}

var
  tzDynInfo: TDynamicTimeZoneInformation;
  tzInfo: TTimeZoneInformation;
  localSystemTime, universalSystemTime: TSystemTime;
begin
  DateTimeToSystemTime(t, universalSystemTime);
  if GetDynamicTimeZoneInformation(tzDynInfo) = TIME_ZONE_ID_INVALID then
    RaiseLastOSError;
  if not GetTimeZoneInformationForYear(universalSystemTime.wYear, @tzDynInfo,
    tzInfo) then
    RaiseLastOSError;
  if not SystemTimeToTzSpecificLocalTime(@tzInfo, universalSystemTime,
    localSystemTime) then
    RaiseLastOSError;
  result := SystemTimeToDateTime(localSystemTime);
end;
{$ENDIF}{$ENDIF}

// SystemMillisecondsToUnixTime
//
function SystemMillisecondsToUnixTime(t: Int64): Int64;
begin
  result := UnixTime - (GetSystemTimeMilliseconds - t) div 1000;
end;

// UnixTimeToSystemMilliseconds
//
function UnixTimeToSystemMilliseconds(ut: Int64): Int64;
begin
  result := GetSystemTimeMilliseconds - (UnixTime - ut) * 1000;
end;

// SystemSleep
//
procedure SystemSleep(msec: Integer);
begin
  if msec >= 0 then
    Sleep(msec);
end;

// FirstWideCharOfString
//
function FirstWideCharOfString(const s: String; const default: WideChar = #0)
  : WideChar;
begin
{$IFDEF FPC}
  if s <> '' then
    result := PWideChar(String(s))^
  else
    result := default;
{$ELSE}
  if s <> '' then
    result := PWideChar(Pointer(s))^
  else
    result := default;
{$ENDIF}
end;

// CodePointToUnicodeString
//
procedure CodePointToUnicodeString(c: Integer; var result: UnicodeString);
begin
  case c of
    0 .. $FFFF:
      result := WideChar(c);
    $10000 .. $10FFFF:
      begin
        c := c - $10000;
        result := WideChar($D800 + (c shr 10)) + WideChar($DC00 + (c and $3FF));
      end;
  else
    raise EConvertError.CreateFmt('Invalid codepoint: %d', [c]);
  end;
end;

// CodePointToString
//
procedure CodePointToString(const c: Integer; var result: String); inline;
{$IFDEF FPC}
var
  buf: UnicodeString;
begin
  CodePointToUnicodeString(c, buf);
  result := String(buf);
{$ELSE}
begin
  CodePointToUnicodeString(c, result);
{$ENDIF}
end;

// UnicodeCompareStr
//
{$IFNDEF FPC}

function UnicodeCompareStr(const S1, S2: String): Integer;
begin
  result := CompareStr(S1, S2);
end;
{$ENDIF}

// UnicodeStringReplace
//
function UnicodeStringReplace(const s, oldPattern, newPattern: String;
  flags: TReplaceFlags): String;
begin
  result := System.SysUtils.StringReplace(s, oldPattern, newPattern, flags);
end;

{$IFDEF WINDOWS}

const
  CSTR_LESS_THAN = 1;
  CSTR_EQUAL = 2;
  CSTR_GREATER_THAN = 3;
function CompareStringEx(lpLocaleName: LPCWSTR; dwCmpFlags: DWORD;
  lpString1: LPCWSTR; cchCount1: Integer; lpString2: LPCWSTR;
  cchCount2: Integer; lpVersionInformation: Pointer; lpReserved: LPVOID;
  lParam: lParam): Integer; stdcall; external 'kernel32.dll';
{$ENDIF}

// UnicodeCompareP
//
function UnicodeCompareP(p1: PWideChar; n1: Integer; p2: PWideChar;
  n2: Integer): Integer;
{$IFDEF WINDOWS}
begin
  result := CompareStringEx(nil, NORM_IGNORECASE, p1, n1, p2, n2, nil, nil, 0);
  if result = 0 then
    RaiseLastOSError
  else
    Dec(result, CSTR_EQUAL);
end;
{$ELSE}

begin
  if IsICUAvailable then
    result := Integer(ucol_strcoll(GetCollator(UTF8CompareLocale,
      [coIgnoreCase]), p1, n1, p2, n2))
  else
    raise Exception.Create
      ('ICU not available (http://site.icu-project.org/home)');
end;
{$ENDIF}

// UnicodeCompareEx
//
function UnicodeCompareEx(const a, b: UnicodeString;
  const locale: UnicodeString; caseSensitive: Boolean): Integer;
{$IFDEF WINDOWS}
var
  flags: Integer;
begin
  if caseSensitive then
    flags := 0
  else
    flags := NORM_IGNORECASE;
  result := CompareStringEx(PChar(locale), flags, PChar(a), Length(a), PChar(b),
    Length(b), nil, nil, 0);
  if result = 0 then
    RaiseLastOSError
  else
    Dec(result, CSTR_EQUAL);
end;
{$ELSE}

begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$ENDIF}

// UnicodeCompareP
//
function UnicodeCompareP(p1, p2: PWideChar; n: Integer): Integer; overload;
{$IFDEF WINDOWS}
begin
  result := CompareStringEx(nil, NORM_IGNORECASE, p1, n, p2, n, nil, nil, 0);
  if result = 0 then
    RaiseLastOSError
  else
    Dec(result, CSTR_EQUAL);
end;
{$ELSE}

begin
  if IsICUAvailable then
    result := Integer(ucol_strcoll(GetCollator(UTF8CompareLocale,
      [coIgnoreCase]), p1, n, p2, n))
  else
    raise Exception.Create
      ('ICU not available (http://site.icu-project.org/home)');
end;
{$ENDIF}

// UnicodeLowerCase
//
procedure UnicodeLowerCase(const s: UnicodeString; var result: UnicodeString);
var
  n: Integer;
begin
  n := Length(s);
  if n > 0 then
  begin
{$IFDEF WINDOWS}
    SetLength(result, n);
    Winapi.Windows.LCMapStringEx(nil, LCMAP_LOWERCASE or
      LCMAP_LINGUISTIC_CASING, PWideChar(Pointer(s)), n,
      PWideChar(Pointer(result)), n, nil, nil, 0);
{$ELSE}
    result := s.ToLower;
{$ENDIF}
  end
  else
    result := '';
end;

// UnicodeLowerCase
//
function UnicodeLowerCase(const s: UnicodeString): UnicodeString;
begin
  UnicodeLowerCase(s, result);
end;

// UnicodeUpperCase
//
procedure UnicodeUpperCase(const s: UnicodeString; var result: UnicodeString);
var
  n: Integer;
begin
  n := Length(s);
  if n > 0 then
  begin
{$IFDEF WINDOWS}
    SetLength(result, n);
    Winapi.Windows.LCMapStringEx(nil, LCMAP_UPPERCASE or
      LCMAP_LINGUISTIC_CASING, PWideChar(Pointer(s)), n,
      PWideChar(Pointer(result)), n, nil, nil, 0);
{$ELSE}
    result := s.ToUpper;
{$ENDIF}
  end
  else
    result := '';
end;

// UnicodeUpperCase
//
function UnicodeUpperCase(const s: UnicodeString): UnicodeString;
begin
  UnicodeUpperCase(s, result);
end;

{$IFDEF FPC}

// UnicodeLowerCase
//
function UnicodeLowerCase(const s: String): String;
begin
  result := String(UnicodeLowerCase(UnicodeString(s)));
end;

// UnicodeUpperCase
//
function UnicodeUpperCase(const s: String): String;
begin
  result := String(UnicodeUpperCase(UnicodeString(s)));
end;
{$ENDIF}

// ASCIICompareText
//
function ASCIICompareText(const S1, S2: String): Integer; inline;
begin
{$IFDEF FPC}
  result := CompareText(UTF8Encode(S1), UTF8Encode(S2));
{$ELSE}
  result := CompareText(S1, S2);
{$ENDIF}
end;

// ASCIISameText
//
function ASCIISameText(const S1, S2: String): Boolean; inline;
begin
{$IFDEF FPC}
  result := (ASCIICompareText(S1, S2) = 0);
{$ELSE}
  result := SameText(S1, S2);
{$ENDIF}
end;

// NormalizeString
//
{$IFDEF WINDOWS}
function APINormalizeString(normForm: Integer; lpSrcString: LPCWSTR;
  cwSrcLength: Integer; lpDstString: LPWSTR; cwDstLength: Integer): Integer;
  stdcall; external 'Normaliz.dll' name 'NormalizeString'
{$IFNDEF FPC}delayed{$ENDIF};

function NormalizeString(const s, form: String): String;
var
  nf, len, n: Integer;
begin
  if s = '' then
    Exit('');
  if (form = '') or (form = 'NFC') then
    nf := 1
  else if form = 'NFD' then
    nf := 2
  else if form = 'NFKC' then
    nf := 5
  else if form = 'NFKD' then
    nf := 6
  else
    raise Exception.CreateFmt('Unsupported normalization form "%s"', [form]);
  n := 10;
  len := APINormalizeString(nf, Pointer(s), Length(s), nil, 0);
  repeat
    SetLength(result, len);
    len := APINormalizeString(nf, PWideChar(s), Length(s),
      Pointer(result), len);
    if len <= 0 then
    begin
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        RaiseLastOSError;
      Dec(n);
      if n <= 0 then
        RaiseLastOSError;
      len := -len;
      len := len + (len div 4); // extra margin since estimation failed
      continue;
    end;
  until True;
  SetLength(result, len);
end;
{$ELSE}

function NormalizeString(const s, form: String): String;
begin
  { TODO : Unicode character normalization for non Windows platforms }

  // See http://www.unicode.org/reports/tr15/
  // Possible solutions:
  // http://www.delphitop.com/html/danyuan/1472.html
  // https://github.com/graemeg/freepascal/blob/master/rtl/objpas/unicodedata.pas
  result := s; // TODO
end;
{$ENDIF}

// StripAccents
//
function StripAccents(const s: String): String;
var
  i: Integer;
  pSrc, pDest: PWideChar;
begin
  result := NormalizeString(s, 'NFD');
  pSrc := Pointer(result);
  pDest := pSrc;
  for i := 1 to Length(result) do
  begin
    case Ord(pSrc^) of
      $300 .. $36F:
        ; // diacritic range
    else
      pDest^ := pSrc^;
      Inc(pDest);
    end;
    Inc(pSrc);
  end;
  SetLength(result, (NativeUInt(pDest) - NativeUInt(Pointer(result))) div 2);
end;

// InterlockedIncrement
//
function InterlockedIncrement(var val: Integer): Integer;
{$IFNDEF WIN32_ASM}
begin
{$IFDEF WINDOWS}
  result := Winapi.Windows.InterlockedIncrement(val);
{$ELSE}
  result := TInterlocked.Increment(val);
{$ENDIF}
{$ELSE}
asm
  mov   ecx,  eax
  mov   eax,  1
  lock  xadd [ecx], eax
  inc   eax
  {$ENDIF}
end;

// InterlockedDecrement
//
function InterlockedDecrement(var val: Integer): Integer;
{$IFNDEF WIN32_ASM}
begin
{$IFDEF WINDOWS}
  result := Winapi.Windows.InterlockedDecrement(val);
{$ELSE}
  result := TInterlocked.Decrement(val);
{$ENDIF}
{$ELSE}
asm
  mov   ecx,  eax
  mov   eax,  -1
  lock  xadd [ecx], eax
  dec   eax
  {$ENDIF}
end;

// FastInterlockedIncrement
//
procedure FastInterlockedIncrement(var val: Integer);
{$IFNDEF WIN32_ASM}
begin
  AtomicIncrement(val);
{$ELSE}
asm
  lock  inc [eax]
  {$ENDIF}
end;

// FastInterlockedDecrement
//
procedure FastInterlockedDecrement(var val: Integer);
{$IFNDEF WIN32_ASM}
begin
  AtomicDecrement(val);
{$ELSE}
asm
  lock  dec [eax]
  {$ENDIF}
end;

// InterlockedExchangePointer
//
function InterlockedExchangePointer(var target: Pointer; val: Pointer): Pointer;
{$IFNDEF WIN32_ASM}
begin
{$IFDEF FPC}
  result := System.InterLockedExchange(target, val);
{$ELSE}
{$IFDEF WINDOWS}
  result := Winapi.Windows.InterlockedExchangePointer(target, val);
{$ELSE}
  result := TInterlocked.exchange(target, val);
{$ENDIF}
{$ENDIF}
{$ELSE}
asm
  lock  xchg dword ptr [eax], edx
  mov   eax, edx
  {$ENDIF}
end;

// InterlockedCompareExchangePointer
//
function InterlockedCompareExchangePointer(var destination: Pointer;
  exchange, comparand: Pointer): Pointer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
begin
{$IFDEF FPC}
{$IFDEF CPU64}
  result := Pointer(System.InterlockedCompareExchange64(QWord(destination),
    QWord(exchange), QWord(comparand)));
{$ELSE}
  result := System.InterLockedCompareExchange(destination, exchange, comparand);
{$ENDIF}
{$ELSE}
{$IFDEF WINDOWS}
  result := Winapi.Windows.InterlockedCompareExchangePointer(destination,
    exchange, comparand);
{$ELSE}
  result := TInterlocked.CompareExchange(destination, exchange, comparand);
{$ENDIF}
{$ENDIF}
end;

// SetThreadName
//
{$IFDEF WINDOWS}
function IsDebuggerPresent: BOOL; stdcall;
  external kernel32 name 'IsDebuggerPresent';

procedure SetThreadName(const threadName: PAnsiChar;
  threadID: Cardinal = Cardinal(-1));
// http://www.codeproject.com/Articles/8549/Name-your-threads-in-the-VC-debugger-thread-list
type
  TThreadNameInfo = record
    dwType: Cardinal; // must be 0x1000
    szName: PAnsiChar; // pointer to name (in user addr space)
    dwThreadID: Cardinal; // thread ID (-1=caller thread)
    dwFlags: Cardinal; // reserved for future use, must be zero
  end;
var
  info: TThreadNameInfo;
begin
  if not IsDebuggerPresent then
    Exit;

  info.dwType := $1000;
  info.szName := threadName;
  info.dwThreadID := threadID;
  info.dwFlags := 0;
{$IFNDEF FPC}
  try
    RaiseException($406D1388, 0, SizeOf(info) div SizeOf(Cardinal), @info);
  except
  end;
{$ENDIF}
end;
{$ELSE}

procedure SetThreadName(const threadName: PAnsiChar;
  threadID: Cardinal = Cardinal(-1));
begin
  // This one appears limited to Embarcadero debuggers
  TThread.NameThreadForDebugging(threadName, threadID);
end;
{$ENDIF}

// OutputDebugString
//
procedure OutputDebugString(const msg: String);
begin
{$IFDEF WINDOWS}
  Winapi.Windows.OutputDebugStringW(PWideChar(msg));
{$ELSE}
  { TODO : Check for Linux debugger functionalities }
{$ENDIF}
end;

// WriteToOSEventLog
//
procedure WriteToOSEventLog(const logName, logCaption, logDetails: String;
  const logRawData: RawByteString = '');
{$IFDEF WINDOWS}
var
  eventSource: THandle;
  detailsPtr: array [0 .. 1] of PWideChar;
begin
  if logName <> '' then
    eventSource := RegisterEventSourceW(nil, PWideChar(logName))
  else
    eventSource := RegisterEventSourceW(nil,
      PWideChar(ChangeFileExt(ExtractFileName(ParamStr(0)), '')));
  if eventSource > 0 then
  begin
    try
      detailsPtr[0] := PWideChar(logCaption);
      detailsPtr[1] := PWideChar(logDetails);
      ReportEventW(eventSource, EVENTLOG_INFORMATION_TYPE, 0, 0, nil, 2,
        Length(logRawData), @detailsPtr, Pointer(logRawData));
    finally
      DeregisterEventSource(eventSource);
    end;
  end;
end;
{$ELSE}

begin
{$IFDEF POSIXSYSLOG}
  Posix.Syslog.Syslog(LOG_INFO, logCaption + ': ' + logDetails + '(' +
    logRawData + ')');
{$ENDIF}
end;
{$ENDIF}

// SetDecimalSeparator
//
procedure SetDecimalSeparator(c: Char);
begin
{$IFDEF FPC}
  FormatSettings.DecimalSeparator := c;
{$ELSE}
{$IF CompilerVersion >= 22.0}
  FormatSettings.DecimalSeparator := c;
{$ELSE}
  DecimalSeparator := c;
{$IFEND}
{$ENDIF}
end;

// GetDecimalSeparator
//
function GetDecimalSeparator: Char;
begin
{$IFDEF FPC}
  result := FormatSettings.DecimalSeparator;
{$ELSE}
{$IF CompilerVersion >= 22.0}
  result := FormatSettings.DecimalSeparator;
{$ELSE}
  result := DecimalSeparator;
{$IFEND}
{$ENDIF}
end;

// CollectFiles
//
type
{$IFDEF WINDOWS}
  TFindDataRec = record
    Handle: THandle;
    data: TWin32FindDataW;
  end;
{$ENDIF}

  TMasks = array of TMask;

  // CollectFilesMasked
  //
procedure CollectFilesMasked(const directory: TFileName; const Masks: TMasks;
  list: TStrings; recurseSubdirectories: Boolean = False;
  onProgress: TCollectFileProgressEvent = nil);
{$IFDEF WINDOWS}
const
  // contant defined in Windows.pas is incorrect
  FindExInfoBasic = 1;
var
  searchRec: TFindDataRec;
  infoLevel: TFindExInfoLevels;
  filename: TFileName;
  skipScan, addToList: Boolean;
  i: Integer;
begin
  // 6.1 required for FindExInfoBasic (Win 2008 R2 or Win 7)
  if ((Win32MajorVersion shl 8) or Win32MinorVersion) >= $601 then
    infoLevel := TFindExInfoLevels(FindExInfoBasic)
  else
    infoLevel := FindExInfoStandard;

  if Assigned(onProgress) then
  begin
    skipScan := False;
    onProgress(directory, skipScan);
    if skipScan then
      Exit;
  end;

  filename := directory + '*';
  searchRec.Handle := FindFirstFileEx(PChar(filename), infoLevel,
    @searchRec.data, FINDEX_SEARCH_OPS.FindExSearchNameMatch, nil, 0);
  if searchRec.Handle <> INVALID_HANDLE_VALUE then
  begin
    repeat
      if (searchRec.data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        // check file against mask
        filename := searchRec.data.cFileName;
        addToList := True;
        for i := 0 to High(Masks) do
        begin
          addToList := Masks[i].Matches(filename);
          if addToList then
            Break;
        end;
        if addToList then
        begin
          filename := directory + filename;
          list.Add(filename);
        end;
      end
      else if recurseSubdirectories then
      begin
        // dive in subdirectory
        if searchRec.data.cFileName[0] = '.' then
        begin
          if searchRec.data.cFileName[1] = '.' then
          begin
            if searchRec.data.cFileName[2] = #0 then
              continue;
          end
          else if searchRec.data.cFileName[1] = #0 then
            continue;
        end;
        // decomposed cast and concatenation to avoid implicit string variable
        filename := searchRec.data.cFileName;
        filename := directory + filename + PathDelim;
        CollectFilesMasked(filename, Masks, list, True, onProgress);
      end;
    until not FindNextFileW(searchRec.Handle, searchRec.data);
    Winapi.Windows.FindClose(searchRec.Handle);
  end;
end;
{$ELSE}

var
  searchRec: TSearchRec;
  filename: TFileName;
  skipScan: Boolean;
  addToList: Boolean;
  i: Integer;
begin
  try
    if Assigned(onProgress) then
    begin
      skipScan := False;
      onProgress(directory, skipScan);
      if skipScan then
        Exit;
    end;

    filename := directory + '*';
    if System.SysUtils.FindFirst(filename, faAnyfile, searchRec) = 0 then
    begin
      repeat
        if (searchRec.Attr and faVolumeId) = 0 then
        begin
          if (searchRec.Attr and faDirectory) = 0 then
          begin
            filename := searchRec.name;
            addToList := True;
            for i := 0 to High(Masks) do
            begin
              addToList := Masks[i].Matches(filename);
              if addToList then
                Break;
            end;
            if addToList then
            begin
              filename := directory + filename;
              list.Add(filename);
            end;
          end
          else if recurseSubdirectories and (searchRec.name <> '.') and
            (searchRec.name <> '..') then
          begin
            filename := directory + searchRec.name + PathDelim;
            CollectFilesMasked(filename, Masks, list, recurseSubdirectories,
              onProgress);
          end;
        end;
      until System.SysUtils.FindNext(searchRec) <> 0;
    end;
  finally
    System.SysUtils.FindClose(searchRec);
  end;
end;
{$ENDIF}

// CollectFiles
//
procedure CollectFiles(const directory, fileMask: TFileName; list: TStrings;
  recurseSubdirectories: Boolean = False;
  onProgress: TCollectFileProgressEvent = nil);
var
  Masks: TMasks;
  p, pNext: Integer;
begin
  if fileMask <> '' then
  begin
    p := 1;
    repeat
      pNext := PosEx(';', fileMask, p);
      if pNext < p then
      begin
        SetLength(Masks, Length(Masks) + 1);
        Masks[High(Masks)] := TMask.Create(Copy(fileMask, p));
        Break;
      end;
      if pNext > p then
      begin
        SetLength(Masks, Length(Masks) + 1);
        Masks[High(Masks)] := TMask.Create(Copy(fileMask, p, pNext - p));
      end;
      p := pNext + 1;
    until p > Length(fileMask);
  end;
  // Windows can match 3 character filters with old DOS filenames
  // Mask confirmation is necessary
  try
    CollectFilesMasked(IncludeTrailingPathDelimiter(directory), Masks, list,
      recurseSubdirectories, onProgress);
  finally
    for p := 0 to High(Masks) do
      Masks[p].Free;
  end;
end;

// CollectSubDirs
//
procedure CollectSubDirs(const directory: TFileName; list: TStrings);
{$IFDEF WINDOWS}
const
  // contant defined in Windows.pas is incorrect
  FindExInfoBasic = 1;
var
  searchRec: TFindDataRec;
  infoLevel: TFindExInfoLevels;
  filename: TFileName;
begin
  // 6.1 required for FindExInfoBasic (Win 2008 R2 or Win 7)
  if ((Win32MajorVersion shl 8) or Win32MinorVersion) >= $601 then
    infoLevel := TFindExInfoLevels(FindExInfoBasic)
  else
    infoLevel := FindExInfoStandard;

  filename := directory + '*';
  searchRec.Handle := FindFirstFileEx(PChar(filename), infoLevel,
    @searchRec.data, FINDEX_SEARCH_OPS.FindExSearchLimitToDirectories, nil, 0);
  if searchRec.Handle <> INVALID_HANDLE_VALUE then
  begin
    repeat
      if (searchRec.data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0
      then
      begin
        if searchRec.data.cFileName[0] = '.' then
        begin
          if searchRec.data.cFileName[1] = '.' then
          begin
            if searchRec.data.cFileName[2] = #0 then
              continue;
          end
          else if searchRec.data.cFileName[1] = #0 then
            continue;
        end;
        // decomposed cast and concatenation to avoid implicit string variable
        filename := searchRec.data.cFileName;
        list.Add(filename);
      end;
    until not FindNextFileW(searchRec.Handle, searchRec.data);
    Winapi.Windows.FindClose(searchRec.Handle);
  end;
end;
{$ELSE}

var
  searchRec: TSearchRec;
begin
  try
    if System.SysUtils.FindFirst(directory + '*', faDirectory, searchRec) = 0
    then
    begin
      repeat
        if (searchRec.Attr and faDirectory > 0) and (searchRec.name <> '.') and
          (searchRec.name <> '..') then
          list.Add(searchRec.name);
      until System.SysUtils.FindNext(searchRec) <> 0;
    end;
  finally
    System.SysUtils.FindClose(searchRec);
  end;
end;
{$ENDIF}
{$IFDEF FPC}

// VarCopy
//
procedure VarCopy(out dest: Variant; const src: Variant);
begin
  dest := src;
end;
{$ELSE}

// VarToUnicodeStr
//
function VarToUnicodeStr(const v: Variant): String; inline;
begin
  result := VarToStr(v);
end;
{$ENDIF FPC}
{$IFDEF FPC}

// Utf8ToUnicodeString
//
function Utf8ToUnicodeString(const buf: RawByteString): UnicodeString; inline;
begin
  result := UTF8Decode(buf);
end;
{$ENDIF}

// RawByteStringToBytes
//
function RawByteStringToBytes(const buf: RawByteString): TBytes;
var
  n: Integer;
begin
  n := Length(buf);
  SetLength(result, n);
  if n > 0 then
    System.Move(buf[1], result[0], n);
end;

// BytesToRawByteString
//
function BytesToRawByteString(const buf: TBytes; startIndex: Integer = 0)
  : RawByteString;
var
  n: Integer;
begin
  n := Length(buf) - startIndex;
  if n <= 0 then
    result := ''
  else
  begin
    SetLength(result, n);
    System.Move(buf[startIndex], Pointer(result)^, n);
  end;
end;

// PosExA
//
function PosExA(const needle, haystack: RawByteString;
  hayStackOffset: Integer): Integer;
var
  lenNeedle: Integer;
  charToFind: AnsiChar;
  pHaystack, pEndSearch: PAnsiChar;
begin
  result := 0;
  if (haystack = '') or (needle = '') then
    Exit;
  if hayStackOffset <= 0 then
    hayStackOffset := 1;

  lenNeedle := Length(needle);

  charToFind := PAnsiChar(Pointer(needle))^;

  pHaystack := Pointer(haystack);
  Inc(pHaystack, hayStackOffset - 1);

  pEndSearch := Pointer(haystack);
  Inc(pEndSearch, Length(haystack) - lenNeedle + 1);

  while pHaystack <= pEndSearch do
  begin
    while pHaystack^ <> charToFind do
    begin
      Inc(pHaystack);
      if pHaystack > pEndSearch then
        Break;
    end;
    if CompareMem(pHaystack, Pointer(needle), lenNeedle) then
    begin
      result := NativeUInt(pHaystack) - NativeUInt(Pointer(haystack)) + 1;
      Break;
    end;
    Inc(pHaystack);
  end;
end;

// BytesToRawByteString
//
function BytesToRawByteString(p: Pointer; size: Integer): RawByteString;
begin
  SetLength(result, size);
  System.Move(p^, Pointer(result)^, size);
end;

// BytesToScriptString
//
procedure BytesToScriptString(const p: PByteArray; n: Integer;
  var result: UnicodeString);
begin
  SetLength(result, n);
  BytesToWords(p, PWordArray(Pointer(result)), n);
end;

// WordsToBytes
//
procedure WordsToBytes(src: PWordArray; dest: PByteArray; nbWords: Integer);
{$IFDEF WIN64_ASM}
asm  // src -> rcx     dest -> rdx      nbBytes -> r8
  cmp         r8, 16
  jb          @@tail8

  mov         eax, r8d
  shr         eax, 4
  and         r8, 15

@@loop16:
  movdqu      xmm1, [rcx]
  movdqu      xmm2, [rcx+16]
  packuswb    xmm1, xmm2
  movdqu      [rdx], xmm1
  add         rcx,  32
  add         rdx,  16
  dec         eax
  jnz         @@loop16

@@tail8:
  cmp         r8, 8
  jb          @@tail

  and         r8, 7
  movdqu      xmm1, [rcx]
  packuswb    xmm1, xmm1
  movq        [rdx], xmm1
  add         rcx,  16
  add         rdx,  8

@@tail:
  test        r8, r8
  jz          @@end

@@loop1:
  mov         ax, [rcx+r8*2-2]
  mov         [rdx+r8-1], al
  dec         r8
  jnz         @@loop1

@@end:
end;
{$ELSE}
begin
  while nbWords >= 4 do
  begin
    Dec(nbWords, 4);
    dest[0] := Byte(src[0]);
    dest[1] := Byte(src[1]);
    dest[2] := Byte(src[2]);
    dest[3] := Byte(src[3]);
    dest := @dest[4];
    src := @src[4];
  end;
  while nbWords > 0 do
  begin
    Dec(nbWords);
    dest[0] := Byte(src[0]);
    dest := @dest[1];
    src := @src[1];
  end;
end;
{$ENDIF}

// BytesToWords
//
procedure BytesToWords(src: PByteArray; dest: PWordArray; nbBytes: Integer);
{$IFDEF WIN64_ASM}
asm  // src -> rcx     dest -> rdx      nbBytes -> r8
  pxor        xmm0, xmm0

  cmp         r8, 16
  jb          @@tail8

  mov         eax, r8d
  shr         eax, 4
  and         r8, 15

@@loop16:
  movq        xmm1, [rcx]
  movq        xmm2, [rcx+8]
  punpcklbw   xmm1, xmm0
  punpcklbw   xmm2, xmm0
  movdqu      [rdx], xmm1
  movdqu      [rdx+16], xmm2
  add         rcx,  16
  add         rdx,  32
  dec         eax
  jnz         @@loop16

@@tail8:
  cmp         r8, 8
  jb          @@tail

  and         r8, 7
  movq        xmm1, [rcx]
  punpcklbw   xmm1, xmm0
  movdqu      [rdx], xmm1
  add         rcx,  8
  add         rdx,  16

@@tail:
  test        r8, r8
  jz          @@end

@@loop1:
  movzx       eax, [rcx + r8 - 1]
  mov         [rdx + r8*2 - 2], ax
  dec         r8
  jnz         @@loop1

@@end:
end;
{$ELSE}
begin
  while nbBytes >= 4 do
  begin
    Dec(nbBytes, 4);
    dest[0] := src[0];
    dest[1] := src[1];
    dest[2] := src[2];
    dest[3] := src[3];
    dest := @dest[4];
    src := @src[4];
  end;
  while nbBytes > 0 do
  begin
    Dec(nbBytes);
    dest[0] := src[0];
    dest := @dest[1];
    src := @src[1];
  end;
end;
{$ENDIF}

// TryTextToFloat
//
function TryTextToFloat(const s: PChar; var Value: Extended;
  const FormatSettings: TFormatSettings): Boolean;
{$IFDEF FPC}
var
  cw: Word;
begin
  cw := Get8087CW;
  Set8087CW($133F);
  if TryStrToFloat(s, Value, FormatSettings) then
    result := (Value > -1.7E308) and (Value < 1.7E308);
  if not result then
    Value := 0;
  asm fclex
  end;
  Set8087CW(cw);
{$ELSE}
begin
  // Result:=TextToFloat(s, value, fvExtended, formatSettings);
  result := TryStrToFloat(s, Value, FormatSettings);
  // Result := StrToFloat(s, formatSettings);
{$ENDIF}
end;

// TryTextToFloatW
//
function TryTextToFloatW(const s: PWideChar; var Value: Extended;
  const FormatSettings: TFormatSettings): Boolean;
{$IFDEF FPC}
var
  bufU: UnicodeString;
  buf: String;
begin
  bufU := s;
  buf := String(bufU);
  result := TryTextToFloat(PChar(buf), Value, FormatSettings);
{$ELSE}
begin
  result := TextToFloat(s, Value, fvExtended, FormatSettings)
{$ENDIF}
end;

// LoadTextFromBuffer
//
function LoadTextFromBuffer(const buf: TBytes): UnicodeString;
var
  n, sourceLen, len: Integer;
  encoding: TEncoding;
begin
  if buf = nil then
    result := ''
  else
  begin
    encoding := nil;
    n := TEncoding.GetBufferEncoding(buf, encoding);
    if n = 0 then
      encoding := TEncoding.UTF8;
    if encoding = TEncoding.UTF8 then
    begin
      // handle UTF-8 directly, encoding.GetString returns an empty string
      // whenever a non-utf-8 character is detected, the implementation below
      // will return a '?' for non-utf8 characters instead
      sourceLen := Length(buf) - n;
      SetLength(result, sourceLen);
      len := Utf8ToUnicode(Pointer(result), sourceLen + 1, PAnsiChar(buf) + n,
        sourceLen) - 1;
      if len > 0 then
      begin
        if len <> sourceLen then
          SetLength(result, len);
      end
      else
        result := ''
    end
    else
    begin
      result := encoding.GetString(buf, n, Length(buf) - n);
    end;
  end;
end;

// LoadTextFromRawBytes
//
function LoadTextFromRawBytes(const buf: RawByteString): UnicodeString;
var
  b: TBytes;
begin
  if buf = '' then
    Exit('');
  SetLength(b, Length(buf));
  System.Move(buf[1], b[0], Length(buf));
  result := LoadTextFromBuffer(b);
end;

// LoadTextFromStream
//
function LoadTextFromStream(aStream: TStream): UnicodeString;
var
  n: Integer;
  buf: TBytes;
begin
  n := aStream.size - aStream.Position;
  SetLength(buf, n);
  aStream.Read(buf[0], n);
  result := LoadTextFromBuffer(buf);
end;

// LoadTextFromFile
//
function LoadTextFromFile(const filename: TFileName): UnicodeString;
var
  buf: TBytes;
begin
  buf := LoadDataFromFile(filename);
  result := LoadTextFromBuffer(buf);
end;

// ReadFileChunked
//
{$IFDEF WINDOWS}

function ReadFileChunked(hFile: THandle; const buffer; size: Integer): Integer;
const
  CHUNK_SIZE = 16384;
var
  p: PByte;
  nRemaining: Integer;
  nRead: Cardinal;
begin
  p := @buffer;
  nRemaining := size;
  repeat
    if nRemaining > CHUNK_SIZE then
      nRead := CHUNK_SIZE
    else
      nRead := nRemaining;
    if not ReadFile(hFile, p^, nRead, nRead, nil) then
      RaiseLastOSError
    else if nRead = 0 then
    begin
      // file got trimmed while we were reading
      Exit(size - nRemaining);
    end;
    Dec(nRemaining, nRead);
    Inc(p, nRead);
  until nRemaining <= 0;
  result := size;
end;
{$ENDIF}

// LoadDataFromFile
//
function LoadDataFromFile(const filename: TFileName): TBytes;
{$IFDEF WINDOWS}
const
  INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
  hFile: THandle;
  n, nRead: Cardinal;
begin
  if filename = '' then
    Exit(nil);
  hFile := OpenFileForSequentialReadOnly(filename);
  if hFile = INVALID_HANDLE_VALUE then
    Exit(nil);
  try
    n := GetFileSize(hFile, nil);
    if n = INVALID_FILE_SIZE then
      RaiseLastOSError;
    if n > 0 then
    begin
      SetLength(result, n);
      nRead := ReadFileChunked(hFile, result[0], n);
      if nRead < n then
        SetLength(result, nRead);
    end
    else
      result := nil;
  finally
    CloseFileHandle(hFile);
  end;
end;
{$ELSE}

begin
  if filename = '' then
    result := nil
  else
    result := System.IOUtils.TFile.ReadAllBytes(filename);
end;
{$ENDIF}

// SaveDataToFile
//
procedure SaveDataToFile(const filename: TFileName; const data: TBytes);
{$IFDEF WINDOWS}
var
  hFile: THandle;
  n, nWrite: DWORD;
begin
  hFile := OpenFileForSequentialWriteOnly(filename);
  try
    n := Length(data);
    if n > 0 then
      if not WriteFile(hFile, data[0], n, nWrite, nil) then
        RaiseLastOSError;
  finally
    CloseFileHandle(hFile);
  end;
end;
{$ELSE}

begin
  System.IOUtils.TFile.WriteAllBytes(filename, data);
end;
{$ENDIF}

// LoadRawBytesFromFile
//
function LoadRawBytesFromFile(const filename: TFileName): RawByteString;
{$IFDEF WINDOWS}
const
  INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
  hFile: THandle;
  n, nRead: Cardinal;
begin
  if filename = '' then
    Exit;
  hFile := OpenFileForSequentialReadOnly(filename);
  if hFile = INVALID_HANDLE_VALUE then
    Exit;
  try
    n := GetFileSize(hFile, nil);
    if n = INVALID_FILE_SIZE then
      RaiseLastOSError;
    if n > 0 then
    begin
      SetLength(result, n);
      nRead := ReadFileChunked(hFile, Pointer(result)^, n);
      if nRead < n then
        SetLength(result, nRead);
    end;
  finally
    CloseFileHandle(hFile);
  end;
end;
{$ELSE}

var
  fs: TFileStream;
begin
  if filename = '' then
    Exit;
  fs := TFileStream.Create(filename, fmOpenRead);
  try
    SetLength(result, fs.size);
    if fs.Read(Pointer(result)^, fs.size) <> fs.size then
      raise Exception.Create('stream read exception - data size mismatch');
  finally
    fs.Free;
  end;
end;
{$ENDIF}

// SaveRawBytesToFile
//
function SaveRawBytesToFile(const filename: TFileName;
  const data: RawByteString): Integer;
{$IFDEF WINDOWS}
var
  hFile: THandle;
  nWrite: DWORD;
begin
  result := 0;
  hFile := OpenFileForSequentialWriteOnly(filename);
  try
    if data <> '' then
    begin
      result := Length(data);
      if not WriteFile(hFile, data[1], result, nWrite, nil) then
        RaiseLastOSError;
    end;
  finally
    CloseFileHandle(hFile);
  end;
end;
{$ELSE}

var
  fs: TFileStream;
  dataSize: Longint;
begin
  fs := TFileStream.Create(filename, fmCreate);
  fs.Seek(0, soEnd);
  try
    dataSize := Length(data);
    result := fs.Write(Pointer(data)^, dataSize);
    if result <> dataSize then
      raise Exception.Create('stream write exception - data size mismatch')
  finally
    fs.Free;
  end;
end;
{$ENDIF}

// LoadRawBytesAsScriptStringFromFile
//
procedure BytesToWordsInPlace(p: Pointer; n: NativeInt);
{$IFDEF WIN64_ASM}
asm  // p -> rcx     n -> rdx
  lea         r8,  [ rcx + rdx ]   // r8 -> src
  lea         rcx, [ rcx + rdx*2 ] // rcx -> dest

  cmp         rdx, 16
  jb          @@lessthan16

  mov         rax, rdx
  shr         rax, 4
  and         rdx, 15

  pxor        xmm0, xmm0

@@loop16:
  sub         r8,  16
  sub         rcx, 32

  movq        xmm1, [r8]
  movq        xmm2, [r8+8]
  punpcklbw   xmm1, xmm0
  punpcklbw   xmm2, xmm0
  movdqu      [rcx], xmm1
  movdqu      [rcx+16], xmm2

  sub         rax, 1
  jnz         @@loop16

@@lessthan16:
  test        rdx, rdx
  jz          @@end

@@loop1:
  sub         r8, 1
  sub         rcx, 2
  movzx       ax, [r8]
  mov         [rcx], ax
  sub         rdx, 1
  jnz         @@loop1

@@end:
end;
{$ELSE}
begin
  var
  pw := PWord(IntPtr(p) + (n - 1) * SizeOf(Word));
  var
  pb := PByte(IntPtr(p) + (n - 1) * SizeOf(Byte));
  while n > 0 do
  begin
    pw^ := pb^;
    Dec(pw);
    Dec(pb);
    Dec(n);
  end;
end;
{$ENDIF}

procedure LoadRawBytesAsScriptStringFromFile(const filename: TFileName;
  var result: String);
{$IFDEF WINDOWS}
const
  INVALID_FILE_SIZE = DWORD($FFFFFFFF);
var
  hFile: THandle;
  n: Int64;
  nRead: Cardinal;
  pDest: PWord;
begin
  if filename = '' then
    Exit;
  hFile := OpenFileForSequentialReadOnly(filename);
  if hFile = INVALID_HANDLE_VALUE then
    Exit;
  try
    if not GetFileSizeEx(hFile, n) then
      RaiseLastOSError;
    if n > 0 then
    begin
      SetLength(result, n);
      if Length(result) <> n then
        raise Exception.CreateFmt('File too large (%d)', [n]);
      pDest := Pointer(result);
      ReadFile(hFile, pDest^, n, nRead, nil);
      if nRead <> n then
      begin
        // file got trimmed while we were reading
        SetLength(result, nRead);
        n := nRead;
      end;
      if n > 0 then
        BytesToWordsInPlace(pDest, n);
    end;
  finally
    CloseFileHandle(hFile);
  end;
end;
{$ELSE}

var
  buf: RawByteString;
begin
  buf := LoadRawBytesFromFile(filename);
  if buf <> '' then
    BytesToScriptString(Pointer(buf), Length(buf), result)
  else
    result := '';
end;
{$ENDIF}

// SaveTextToUTF8File
//
procedure SaveTextToUTF8File(const filename: TFileName; const text: String);
begin
  SaveRawBytesToFile(filename, UTF8Encode(text));
end;

// AppendTextToUTF8File
//
procedure AppendTextToUTF8File(const filename: TFileName;
  const text: UTF8String);
var
  fs: TFileStream;
begin
  if text = '' then
    Exit;
  if FileExists(filename) then
    fs := TFileStream.Create(filename, fmOpenWrite or fmShareDenyNone)
  else
    fs := TFileStream.Create(filename, fmCreate);
  try
    fs.Seek(0, soFromEnd);
    fs.Write(text[1], Length(text));
  finally
    fs.Free;
  end;
end;

// OpenFileForSequentialReadOnly
//
function OpenFileForSequentialReadOnly(const filename: TFileName;
  const options: TOpenFileOptions = []): THandle;
begin
{$IFDEF WINDOWS}
  result := CreateFile(PChar(filename), GENERIC_READ,
    FILE_SHARE_READ + FILE_SHARE_WRITE, nil, OPEN_EXISTING,
    FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if (result = INVALID_HANDLE_VALUE) and not(ofoNoRaiseError in options) then
  begin
    if GetLastError <> ERROR_FILE_NOT_FOUND then
      RaiseLastOSError;
  end;
{$ELSE}
  result := System.SysUtils.FileCreate(filename, fmOpenRead, $007);
  if (result = INVALID_HANDLE_VALUE) and not(ofoNoRaiseError in options) then
    raise Exception.Create('invalid file handle');
{$ENDIF}
end;

// OpenFileForSequentialWriteOnly
//
function OpenFileForSequentialWriteOnly(const filename: TFileName;
  const options: TOpenFileOptions = []): THandle;
begin
{$IFDEF WINDOWS}
  var
    attributes: Cardinal;
  result := CreateFile(PChar(filename), GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if (result = INVALID_HANDLE_VALUE) and (GetLastError = ERROR_ACCESS_DENIED)
  then
  begin
    // could be a file with non-normal attributes, try again after reading attributes
    attributes := GetFileAttributes(PChar(filename));
    if attributes <> INVALID_FILE_ATTRIBUTES then
    begin
      result := CreateFile(PChar(filename), GENERIC_WRITE, 0, nil,
        CREATE_ALWAYS, attributes or FILE_FLAG_SEQUENTIAL_SCAN, 0);
    end;
  end;
{$ELSE}
  result := System.SysUtils.FileCreate(filename, fmOpenWrite, $007);
{$ENDIF}
  if (result = INVALID_HANDLE_VALUE) and not(ofoNoRaiseError in options) then
    RaiseLastOSError;
end;

// CloseFileHandle
//
function CloseFileHandle(hFile: THandle): Boolean;
begin
{$IFDEF WINDOWS}
  result := CloseHandle(hFile);
{$ELSE}
  System.SysUtils.FileClose(hFile);
  result := True; // assume success, as RTL does not say
{$ENDIF}
end;

// FileWrite
//
function FileWrite(hFile: THandle; buffer: Pointer; byteCount: Int64): Int64;
const
  cOneGigabyte = 1024 * 1024 * 1024;
var
  bytesToWrite: Integer;
begin
  result := 0;
  while byteCount > 0 do
  begin
    if byteCount > cOneGigabyte then
      bytesToWrite := cOneGigabyte
    else
      bytesToWrite := byteCount;
{$IFDEF WINDOWS}
    if not WriteFile(hFile, buffer^, bytesToWrite, Cardinal(bytesToWrite), nil)
    then
      RaiseLastOSError;
{$ELSE}
    bytesToWrite := System.SysUtils.FileWrite(hFile, buffer^, byteCount);
    if bytesToWrite = -1 then
      raise Exception.Create('file write exception');
{$ENDIF}
    Assert(bytesToWrite > 0);
    Dec(byteCount, bytesToWrite);
    Inc(IntPtr(buffer), bytesToWrite);
    Inc(result, bytesToWrite);
  end;
end;

// FileRead
//
function FileRead(hFile: THandle; buffer: Pointer; byteCount: Int64): Int64;
const
  cOneGigabyte = 1024 * 1024 * 1024;
var
  bytesToRead: Integer;
begin
  result := 0;
  while byteCount > 0 do
  begin
    if byteCount > cOneGigabyte then
      bytesToRead := cOneGigabyte
    else
      bytesToRead := byteCount;
{$IFDEF WINDOWS}
    if not ReadFile(hFile, buffer^, bytesToRead, Cardinal(bytesToRead), nil)
    then
      RaiseLastOSError;
{$ELSE}
    bytesToRead := System.SysUtils.FileRead(hFile, buffer^, byteCount);
    if bytesToRead = -1 then
      raise Exception.Create('file read exception');
{$ENDIF}
    if bytesToRead > 0 then
    begin
      Dec(byteCount, bytesToRead);
      Inc(IntPtr(buffer), bytesToRead);
      Inc(result, bytesToRead);
    end
    else
      Break;
  end;
end;

// FileFlushBuffers
//
{$IFDEF WINDOWS}
function FlushFileBuffers(hFile: THandle): BOOL; stdcall;
  external 'kernel32.dll';

function FileFlushBuffers(hFile: THandle): Boolean;
begin
  result := FlushFileBuffers(hFile);
end;
{$ELSE}

function FileFlushBuffers(hFile: THandle): Boolean;
begin
  // TODO
end;
{$ENDIF}

// SetEndOfFile
//
function SetEndOfFile(hFile: THandle): Boolean;
begin
{$IFDEF WINDOWS}
  result := Winapi.Windows.SetEndOfFile(hFile);
{$ELSE}
  result := (ftruncate(hFile, lseek(hFile, 0, SEEK_CUR)) = 0);
{$ENDIF}
end;

// FileCopy
//
function FileCopy(const existing, new: TFileName;
  failIfExists: Boolean): Boolean;
begin
{$IFDEF WINDOWS}
  result := Winapi.Windows.CopyFileW(PWideChar(existing), PWideChar(new),
    failIfExists);
{$ELSE}
  try
    System.IOUtils.TFile.Copy(existing, new, not failIfExists);
    result := True;
  except
    result := False;
  end;
{$ENDIF}
end;

// FileMove
//
function FileMove(const existing, new: TFileName): Boolean;
begin
{$IFDEF WINDOWS}
  result := Winapi.Windows.MoveFileW(PWideChar(existing), PWideChar(new));
{$ELSE}
  try
    System.IOUtils.TFile.Move(existing, new);
    result := True;
  except
    result := False;
  end;
{$ENDIF}
end;

// FileDelete
//
function FileDelete(const filename: TFileName): Boolean;
begin
  result := System.SysUtils.DeleteFile(filename);
end;

// FileRename
//
function FileRename(const oldName, newName: TFileName): Boolean;
begin
  result := RenameFile(oldName, newName);
end;

// FileSize
//
function FileSize(const name: TFileName): Int64;
{$IFDEF WINDOWS}
var
  info: TWin32FileAttributeData;
begin
  if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info)
  then
    result := info.nFileSizeLow or (Int64(info.nFileSizeHigh) shl 32)
  else
    result := -1;
end;
{$ELSE}

var
  searchRec: TSearchRec;
begin
  try
    if System.SysUtils.FindFirst(name, faAnyfile, searchRec) = 0 then
      result := searchRec.size
    else
      result := 0;
  finally
    System.SysUtils.FindClose(searchRec);
  end;
end;
{$ENDIF}

// FileCreationTime
//
function FileCreationTime(const name: TFileName): TdwsDateTime; overload;
{$IFDEF WINDOWS}
var
  info: TWin32FileAttributeData;
  buf: TdwsDateTime;
begin
  if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info)
  then
  begin
    buf.AsFileTime := info.ftCreationTime;
  end
  else
    buf.Clear;
  result := buf;
end;
{$ELSE}

begin
  result := FileDateTime(name);
end;
{$ENDIF}

// FileDateTime
//
function FileDateTime(hFile: THandle; lastAccess: Boolean = False)
  : TdwsDateTime; overload;
{$IFDEF WINDOWS}
var
  FILETIME: TFileTime;
begin
  FILETIME.dwLowDateTime := 0;
  FILETIME.dwHighDateTime := 0;
  if lastAccess then
    GetFileTime(hFile, nil, @FILETIME, nil)
  else
    GetFileTime(hFile, nil, nil, @FILETIME);
  result.AsFileTime := FILETIME;
end;
{$ELSE}

begin
  result.AsLocalDateTime := FileDateToDateTime(FileGetDate(hFile));
end;
{$ENDIF}

// FileDateTime
//
function FileDateTime(const name: TFileName; lastAccess: Boolean = False)
  : TdwsDateTime;
{$IFDEF WINDOWS}
var
  info: TWin32FileAttributeData;
  buf: TdwsDateTime;
begin
  if GetFileAttributesExW(PWideChar(Pointer(name)), GetFileExInfoStandard, @info)
  then
  begin
    if lastAccess then
      buf.AsFileTime := info.ftLastAccessTime
    else
      buf.AsFileTime := info.ftLastWriteTime;
  end
  else
    buf.Clear;
  result := buf;
end;
{$ELSE}

var
  searchRec: TSearchRec;
  buf: TdwsDateTime;
begin
  try
    if System.SysUtils.FindFirst(name, faAnyfile, searchRec) = 0 then
      buf.AsLocalDateTime := searchRec.TimeStamp
    else
      buf.Clear;
  finally
    System.SysUtils.FindClose(searchRec);
  end;
  result := buf;
end;
{$ENDIF}

// FileSetDateTime
//
procedure FileSetDateTime(hFile: THandle; const aDateTime: TdwsDateTime);
{$IFDEF WINDOWS}
var
  doNotChange, newTimeStamp: TFileTime;
begin
  newTimeStamp := aDateTime.AsFileTime;
  doNotChange.dwLowDateTime := Cardinal(-1);
  doNotChange.dwHighDateTime := Cardinal(-1);
  SetFileTime(hFile, @doNotChange, @newTimeStamp, @newTimeStamp);
end;
{$ELSE}

begin
  { TODO : Check unix implementation }
  raise Exception.Create('not implemented');
end;
{$ENDIF}

// DeleteDirectory
//
function DeleteDirectory(const path: String; recursive: Boolean = True)
  : Boolean;
begin
{$IFDEF FPC}
  result := RemoveDir(path);
{$ELSE}
  try
    TDirectory.Delete(path, recursive);
  except
    Exit(False);
  end;
  result := not TDirectory.Exists(path);
{$ENDIF}
end;

// DirectSet8087CW
//
function DirectSet8087CW(newValue: Word): Word; register;
{$IFNDEF WIN32_ASM}
begin
  result := newValue;
{$ELSE}
asm
  push    eax
  push    eax
  fnstcw  [esp]
  fnclex
  pop     eax
  fldcw   [esp]
  pop     edx
  {$ENDIF}
end;

// DirectSetMXCSR
//
function DirectSetMXCSR(newValue: Word): Word; register;
{$IFDEF WIN32_ASM}
asm
  and      eax, $FFC0
  push     eax
  push     eax
  stmxcsr  [esp+4]
  ldmxcsr  [esp]
  pop eax
  pop eax
  {$ELSE}
begin
  result := newValue;
{$ENDIF}
end;

// SwapBytes
//
function SwapBytes(v: Cardinal): Cardinal;
{$IFDEF WIN32_ASM}
asm
  bswap eax
end;
{$ELSE}{$IFDEF WIN64_ASM}
asm
  bswap ecx
  mov   eax, ecx
end;
{$ELSE}
type
  TCardinalBytes = array [0 .. 3] of Byte;
begin
  TCardinalBytes(result)[0] := TCardinalBytes(v)[3];
  TCardinalBytes(result)[1] := TCardinalBytes(v)[2];
  TCardinalBytes(result)[2] := TCardinalBytes(v)[1];
  TCardinalBytes(result)[3] := TCardinalBytes(v)[0];
end;
{$ENDIF}{$ENDIF}

// SwapBytesBlock
//
procedure SwapBytesBlock(src, dest: PByte; nb: Integer);
begin
  Inc(dest, nb - 1);
  while nb > 0 do
  begin
    dest^ := src^;
    Inc(src);
    Dec(dest);
    Dec(nb);
  end;
end;

// SwapBytesInt32
//
procedure SwapBytesInt32(src, dest: PUInt32);
{$IFDEF WIN64_ASM}
asm
  mov   eax, [rcx]
  bswap eax
  mov   [rdx], eax
end;
{$ELSE}{$IFDEF WIN32_ASM}
asm
  mov   ecx, [eax]
  bswap ecx
  mov   [edx], ecx
end;
{$ELSE}
begin
  PByteArray(dest)[0] := PByteArray(src)[3];
  PByteArray(dest)[1] := PByteArray(src)[2];
  PByteArray(dest)[2] := PByteArray(src)[1];
  PByteArray(dest)[3] := PByteArray(src)[0];
end;
{$ENDIF}{$ENDIF}

// SwapBytesInt64
//
procedure SwapBytesInt64(src, dest: PInt64);
{$IFDEF WIN64_ASM}
asm
  mov   rax, [rcx]
  bswap rax
  mov   [rdx], rax
end;
{$ELSE}{$IFDEF WIN32_ASM}
asm
  mov   ecx, [eax]
  mov   eax, [eax+4]
  bswap ecx
  bswap eax
  mov   [edx+4], ecx
  mov   [edx], eax
end;
{$ELSE}
begin
  PByteArray(dest)[0] := PByteArray(src)[7];
  PByteArray(dest)[1] := PByteArray(src)[6];
  PByteArray(dest)[2] := PByteArray(src)[5];
  PByteArray(dest)[3] := PByteArray(src)[4];
  PByteArray(dest)[4] := PByteArray(src)[3];
  PByteArray(dest)[5] := PByteArray(src)[2];
  PByteArray(dest)[6] := PByteArray(src)[1];
  PByteArray(dest)[7] := PByteArray(src)[0];
end;
{$ENDIF}{$ENDIF}
// RDTSC
//
{$IFDEF WINDOWS}

function RDTSC: UInt64;
asm
  RDTSC
  {$IFDEF WIN64}
  SHL   RDX, 32
  OR    RAX, RDX
  {$ENDIF}
end;
{$ELSE}
var
  vFakeRDTSC: Int64;

function RDTSC: UInt64;
begin
  // TODO : Implement true RDTSC function
  // if asm does not work we use a fake, monotonous, vaguely random ersatz
  result := Int64(AtomicIncrement(vFakeRDTSC, 1 + (GetSystemTimeMilliseconds and
    $FFFF) * 7919));
end;
{$ENDIF}

// GetCurrentUserName
//
function GetCurrentUserName: String;
{$IFDEF WINDOWS}
var
  len: Cardinal;
begin
  len := 255;
  SetLength(result, len);
  Winapi.Windows.GetUserNameW(PWideChar(result), len);
  SetLength(result, len - 1);
end;
{$ELSE}

begin
  result := Posix.Unistd.getlogin;
end;
{$ENDIF}
{$IFNDEF FPC}

// Delphi 2009 is not able to cast a generic T instance to TObject or Pointer
function TtoObject(const t): TObject;
begin
  // Manually inlining the code would require the IF-defs
  // {$IF Compilerversion >= 21}
  result := TObject(t);
  // {$ELSE}
  // Result := PObject(@T)^;
  // {$IFEND}
end;

function TtoPointer(const t): Pointer;
begin
  // Manually inlining the code would require the IF-defs
  // {$IF Compilerversion >= 21}
  result := Pointer(t);
  // {$ELSE}
  // Result := PPointer(@T)^;
  // {$IFEND}
end;

procedure GetMemForT(var t; size: Integer); inline;
begin
  GetMem(Pointer(t), size);
end;
{$ENDIF}

// InitializeWithDefaultFormatSettings
//
procedure InitializeWithDefaultFormatSettings(var fmt: TFormatSettings);
begin
{$IFDEF DELPHI_XE_PLUS}
  fmt := System.SysUtils.FormatSettings;
{$ELSE}
  fmt := System.SysUtils.TFormatSettings((@CurrencyString{%H-})^);
{$ENDIF}
end;

// GetAsString
//
function TModuleVersion.GetAsString: String;
begin
  result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

// SetAsString
//
procedure TModuleVersion.SetAsString(const s: String);
var
  parts: TStringDynArray;
begin
  Self := Default (TModuleVersion);
  parts := SplitString(s, '.');
  var
  len := Length(parts);
  if len > 0 then
    Major := StrToIntDef(parts[0], 0);
  if len > 1 then
    Minor := StrToIntDef(parts[1], 0);
  if len > 2 then
    Release := StrToIntDef(parts[2], 0);
  if len > 3 then
    Build := StrToIntDef(parts[3], 0);
end;

{$IFDEF WINDOWS}

// Adapted from Ian Boyd code published in
// http://stackoverflow.com/questions/10854958/how-to-get-version-of-running-executable
function GetModuleVersion(instance: THandle;
  var version: TModuleVersion): Boolean;
var
  fileInformation: PVSFIXEDFILEINFO;
  verlen: Cardinal;
  rs: TResourceStream;
  m: TMemoryStream;
  resource: HRSRC;
begin
  result := False;

  // Workaround bug in Delphi if resource doesn't exist
  resource := FindResource(instance, PChar(1), RT_VERSION);
  if resource = 0 then
    Exit;

  m := TMemoryStream.Create;
  try
    rs := TResourceStream.CreateFromID(instance, 1, RT_VERSION);
    try
      m.CopyFrom(rs, rs.size);
    finally
      rs.Free;
    end;

    m.Position := 0;
    if VerQueryValue(m.Memory, '\', Pointer(fileInformation), verlen) then
    begin
      version.Major := fileInformation.dwFileVersionMS shr 16;
      version.Minor := fileInformation.dwFileVersionMS and $FFFF;
      version.Release := fileInformation.dwFileVersionLS shr 16;
      version.Build := fileInformation.dwFileVersionLS and $FFFF;
      result := True;
    end;
  finally
    m.Free;
  end;
end;

// GetApplicationVersion
//
var
  vApplicationVersion: TModuleVersion;
  vApplicationVersionRetrieved: Integer;

function GetApplicationVersion(var version: TModuleVersion): Boolean;
begin
  if vApplicationVersionRetrieved = 0 then
  begin
    if GetModuleVersion(HInstance, vApplicationVersion) then
      vApplicationVersionRetrieved := 1
    else
      vApplicationVersionRetrieved := -1;
  end;
  result := (vApplicationVersionRetrieved = 1);
  if result then
    version := vApplicationVersion;
end;
{$ELSE}

function GetModuleVersion(instance: THandle;
  var version: TModuleVersion): Boolean;
begin
  result := False;
end;

function GetApplicationVersion(var version: TModuleVersion): Boolean;
begin
  result := False;
end;
{$ENDIF}

// ApplicationVersion
//
function ApplicationVersion(const options: TApplicationVersionOptions =
  [avoBitness]): String;
begin
{$IFDEF WINDOWS}
  var
    version: TModuleVersion;
{$IFDEF WIN64}
  if GetApplicationVersion(version) then
    result := version.AsString
  else
    result := '?.?.?.?';
{$ELSE}
  if GetApplicationVersion(version) then
    result := version.AsString
  else
    result := '?.?.?.?';
{$ENDIF}
{$ELSE}
  // No version information available under Linux
  result := 'linux build';
{$ENDIF}
{$IFDEF CPUX64}
  if avoBitness in options then
    result := result + ' 64bit';
{$ENDIF}
{$IFDEF CPUX86}
  if avoBitness in options then
    result := result + ' 32bit';
{$ENDIF}
end;

// Win64CPUFeatures
//
{$IF Defined(WIN64_ASM)}

function GetCPUID_ECX: Cardinal;
asm
  mov   r10, rbx
  mov   eax, 1
  cpuid
  mov   eax, ecx
  mov   rbx, r10
end;

function GetCPUID7_EBX: Cardinal;
asm
  mov   r10, rbx
  mov   eax, 7
  xor   ecx, ecx
  cpuid
  mov   eax, ebx
  mov   rbx, r10
end;

var
  vCPUFeatures: TCPUFeatures;

function WIN64CPUFeatures: TCPUFeatures;
begin
  if not(cpuFeaturesRead in vCPUFeatures) then
  begin
    var
    ecx := GetCPUID_ECX;
    if ((ecx shr 19) and 1) <> 0 then
      Include(vCPUFeatures, cpuSSE41);
    if ((ecx shr 12) and 1) <> 0 then
      Include(vCPUFeatures, cpuFMA);
    if ((ecx shr 28) and 1) <> 0 then
    begin
      Include(vCPUFeatures, cpuAVX);
      if (GetCPUID7_EBX shr 5) <> 0 then
        Include(vCPUFeatures, cpuAVX2);
    end;
    Include(vCPUFeatures, cpuFeaturesRead);
  end;
  result := vCPUFeatures;
end;
{$ELSE}

function WIN64CPUFeatures: TCPUFeatures;
begin
  result := [];
end;
{$ENDIF}

// Win64SSE41Supported
//
function Win64SSE41Supported: Boolean;
begin
  result := cpuSSE41 in WIN64CPUFeatures;
end;

// Win64FMASupported
//
function Win64FMASupported: Boolean;
begin
  result := cpuFMA in WIN64CPUFeatures;
end;

// Win64AVX2Supported
//
function Win64AVX2Supported: Boolean;
begin
  result := cpuAVX2 in WIN64CPUFeatures;
end;

// ------------------
// ------------------ TdwsCriticalSection ------------------
// ------------------

{$IFNDEF UNIX}

// Create
//
constructor TdwsCriticalSection.Create;
begin
  InitializeCriticalSection(FCS);
end;

// Destroy
//
destructor TdwsCriticalSection.Destroy;
begin
  DeleteCriticalSection(FCS);
end;

// Enter
//
procedure TdwsCriticalSection.Enter;
begin
  EnterCriticalSection(FCS);
end;

// Leave
//
procedure TdwsCriticalSection.Leave;
begin
  LeaveCriticalSection(FCS);
end;

// TryEnter
//
function TdwsCriticalSection.TryEnter: Boolean;
begin
  result := TryEnterCriticalSection(FCS);
end;
{$ENDIF}

// TryEnterOrTimeout
//
function TdwsCriticalSection.TryEnterOrTimeout(delayMSec: Integer): Boolean;

  function EnterWait: Boolean;
  var
    timeout: Int64;
  begin
    timeout := GetSystemMilliseconds + delayMSec;
    repeat
      Sleep(10);
      result := TryEnter;
    until result or (GetSystemMilliseconds > timeout);
  end;

begin
  result := TryEnter;
  if not result then
    result := EnterWait;
end;

// ------------------
// ------------------ TPath ------------------
// ------------------

// GetTempPath
//
class function TPath.GetTempPath: String;
{$IFDEF WINDOWS}
var
  tempPath: array [0 .. MAX_PATH] of WideChar; // Buf sizes are MAX_PATH+1
begin
  if Winapi.Windows.GetTempPath(MAX_PATH, @tempPath[0]) = 0 then
  begin
    tempPath[1] := '.'; // Current directory
    tempPath[2] := #0;
  end;
  result := tempPath;
{$ELSE}
begin
  result := System.IOUtils.TPath.GetTempPath;
{$ENDIF}
end;

// GetTempFileName
//
class function TPath.GetTempFileName: String;
{$IFDEF WINDOWS}
var
  tempPath, tempFileName: array [0 .. MAX_PATH] of WideChar;
  // Buf sizes are MAX_PATH+1
begin
  if Winapi.Windows.GetTempPath(MAX_PATH, @tempPath[0]) = 0 then
  begin
    tempPath[1] := '.'; // Current directory
    tempPath[2] := #0;
  end;
  if Winapi.Windows.GetTempFileNameW(@tempPath[0], 'DWS', 0, tempFileName) = 0
  then
    RaiseLastOSError; // should never happen
  result := tempFileName;
{$ELSE}
begin
  result := System.IOUtils.TPath.GetTempFileName;
{$ENDIF}
end;

// ------------------
// ------------------ TFile ------------------
// ------------------

// ReadAllBytes
//
class function TFile.ReadAllBytes(const filename: String): TBytes;
begin
  result := LoadDataFromFile(filename)
end;

// ------------------
// ------------------ TdwsThread ------------------
// ------------------

{$IFNDEF FPC}
{$IFDEF VER200}

// Start
//
procedure TdwsThread.Start;
begin
  Resume;
end;

{$ENDIF}
{$ENDIF}

// SetTimeCriticalPriority
//
procedure TdwsThread.SetTimeCriticalPriority;
begin
{$IFDEF WINDOWS}
  // only supported in Windows
  Priority := tpTimeCritical;
{$ENDIF}
end;

// ------------------
// ------------------ TMultiReadSingleWrite ------------------
// ------------------

{$IFNDEF SRW_FALLBACK}

procedure TMultiReadSingleWrite.BeginRead;
begin
  AcquireSRWLockShared(FSRWLock);
end;

function TMultiReadSingleWrite.TryBeginRead: Boolean;
begin
  result := TryAcquireSRWLockShared(FSRWLock);
end;

procedure TMultiReadSingleWrite.EndRead;
begin
  ReleaseSRWLockShared(FSRWLock)
end;

procedure TMultiReadSingleWrite.BeginWrite;
begin
  AcquireSRWLockExclusive(FSRWLock);
end;

function TMultiReadSingleWrite.TryBeginWrite: Boolean;
begin
  result := TryAcquireSRWLockExclusive(FSRWLock);
end;

procedure TMultiReadSingleWrite.EndWrite;
begin
  ReleaseSRWLockExclusive(FSRWLock)
end;

function TMultiReadSingleWrite.State: TMultiReadSingleWriteState;
begin
  // Attempt to guess the state of the lock without making assumptions
  // about implementation details
  // This is only for diagnosing locking issues
  if TryBeginWrite then
  begin
    EndWrite;
    result := mrswUnlocked;
  end
  else if TryBeginRead then
  begin
    EndRead;
    result := mrswReadLock;
  end
  else
  begin
    result := mrswWriteLock;
  end;
end;
{$ELSE}
// SRW_FALLBACK
constructor TMultiReadSingleWrite.Create;
begin
  FLock := TdwsCriticalSection.Create;
end;

destructor TMultiReadSingleWrite.Destroy;
begin
  FLock.Free;
end;

procedure TMultiReadSingleWrite.BeginRead;
begin
  FLock.Enter;
end;

function TMultiReadSingleWrite.TryBeginRead: Boolean;
begin
  result := FLock.TryEnter;
end;

procedure TMultiReadSingleWrite.EndRead;
begin
  FLock.Leave;
end;

procedure TMultiReadSingleWrite.BeginWrite;
begin
  FLock.Enter;
end;

function TMultiReadSingleWrite.TryBeginWrite: Boolean;
begin
  result := FLock.TryEnter;
end;

procedure TMultiReadSingleWrite.EndWrite;
begin
  FLock.Leave;
end;

function TMultiReadSingleWrite.State: TMultiReadSingleWriteState;
begin
  if FLock.TryEnter then
  begin
    FLock.Leave;
    result := mrswUnlocked;
  end
  else
    result := mrswWriteLock;
end;

{$ENDIF}
// ------------------
// ------------------ TTimerTimeout ------------------
// ------------------

{$IFDEF WINDOWS}
{$IFDEF FPC}

type
  TWaitOrTimerCallback = procedure(Context: Pointer; Success: Boolean); stdcall;
function CreateTimerQueueTimer(out phNewTimer: THandle; TimerQueue: THandle;
  CallBack: TWaitOrTimerCallback; Parameter: Pointer; DueTime: DWORD;
  Period: DWORD; flags: ULONG): BOOL; stdcall; external 'kernel32.dll';
function DeleteTimerQueueTimer(TimerQueue: THandle; Timer: THandle;
  CompletionEvent: THandle): BOOL; stdcall; external 'kernel32.dll';

const
  WT_EXECUTEDEFAULT = ULONG($00000000);
  WT_EXECUTEONLYONCE = ULONG($00000008);
  WT_EXECUTELONGFUNCTION = ULONG($00000010);
{$ENDIF}
{$ENDIF}

  // TTimerTimeoutCallBack
  //
procedure TTimerTimeoutCallBack(Context: Pointer;
  {%H-}Success: Boolean); stdcall;
{$IFDEF WINDOWS}
var
  tt: TTimerTimeout;
  event: TTimerEvent;
begin
  tt := TTimerTimeout(Context);
  tt._AddRef;
  try
    event := tt.FOnTimer;
    if Assigned(event) then
      event();
    DeleteTimerQueueTimer(0, tt.FTimer, 0);
    tt.FTimer := 0;
  finally
    tt._Release;
  end;
end;
{$ELSE}

var
  Timer: TdwsXTimer;
  TimerQueue: TdwsXTimerQueue;
begin
  Timer := TdwsXTimer(Context);
  if Assigned(Timer.event) then
    Timer.event();

  TimerQueue := TdwsXTimerQueue.Create(False);
  try
    TimerQueue.Release(Timer.Handle);
  finally
    TimerQueue.Free;
  end;
end;
{$ENDIF}

// Create
//
class function TTimerTimeout.Create(delayMSec: Cardinal;
  onTimer: TTimerEvent): ITimer;
var
  obj: TTimerTimeout;
{$IFDEF UNIX}
  TimerQueue: TdwsXTimerQueue;
{$ENDIF}
begin
  obj := TTimerTimeout(inherited Create);
  result := obj;
  obj.FOnTimer := onTimer;
{$IFDEF WINDOWS}
  CreateTimerQueueTimer(obj.FTimer, 0, TTimerTimeoutCallBack, obj, delayMSec, 0,
    WT_EXECUTEDEFAULT or WT_EXECUTELONGFUNCTION or WT_EXECUTEONLYONCE);
{$ELSE}
  TimerQueue := TdwsXTimerQueue.Create(False);
  try
    obj.FTimer := TimerQueue.Add(delayMSec, onTimer, TTimerTimeoutCallBack);
  finally
    TimerQueue.Free;
  end;
{$ENDIF}
end;

// Destroy
//
destructor TTimerTimeout.Destroy;
begin
  Cancel;
  inherited;
end;

// Cancel
//
procedure TTimerTimeout.Cancel;
{$IFDEF UNIX}
var
  TimerQueue: TdwsXTimerQueue;
{$ENDIF}
begin
  FOnTimer := nil;
  if FTimer = 0 then
    Exit;
{$IFDEF WINDOWS}
  DeleteTimerQueueTimer(0, FTimer, INVALID_HANDLE_VALUE);
{$ELSE}
  TimerQueue := TdwsXTimerQueue.Create(False);
  try
    TimerQueue.ReleaseAll;
  finally
    TimerQueue.Free;
  end;
{$ENDIF}
  FTimer := 0;
end;

// ------------------
// ------------------ TdwsDateTime ------------------
// ------------------

// Now
//
class function TdwsDateTime.Now: TdwsDateTime;
{$IFDEF WINDOWS}
var
  FILETIME: TFileTime;
begin
  GetSystemTimeAsFileTime(FILETIME);
  result.AsFileTime := FILETIME;
end;
{$ELSE}

begin
  result.AsLocalDateTime := System.SysUtils.Now;
end;
{$ENDIF}

// FromLocalDateTime
//
class function TdwsDateTime.FromLocalDateTime(const dt: TDateTime)
  : TdwsDateTime;
begin
  result.AsLocalDateTime := dt;
end;

// Clear
//
procedure TdwsDateTime.Clear;
begin
  FValue := 0;
end;

// IsZero
//
function TdwsDateTime.IsZero: Boolean;
begin
  result := FValue = 0;
end;

// Equal
//
class operator TdwsDateTime.Equal(const a, b: TdwsDateTime): Boolean;
begin
  result := a.FValue = b.FValue;
end;

// NotEqual
//
class operator TdwsDateTime.NotEqual(const a, b: TdwsDateTime): Boolean;
begin
  result := a.FValue <> b.FValue;
end;

// GreaterThan
//
class operator TdwsDateTime.GreaterThan(const a, b: TdwsDateTime): Boolean;
begin
  result := a.FValue > b.FValue;
end;

// GreaterThanOrEqual
//
class operator TdwsDateTime.GreaterThanOrEqual(const a,
  b: TdwsDateTime): Boolean;
begin
  result := a.FValue >= b.FValue;
end;

// LessThan
//
class operator TdwsDateTime.LessThan(const a, b: TdwsDateTime): Boolean;
begin
  result := a.FValue < b.FValue;
end;

// LessThanOrEqual
//
class operator TdwsDateTime.LessThanOrEqual(const a, b: TdwsDateTime): Boolean;
begin
  result := a.FValue <= b.FValue;
end;

// MillisecondsAheadOf
//
function TdwsDateTime.MillisecondsAheadOf(const d: TdwsDateTime): Int64;
begin
  result := FValue - d.FValue;
end;

// IncMilliseconds
//
procedure TdwsDateTime.IncMilliseconds(const msec: Int64);
begin
  Inc(FValue, msec);
end;

const
  cFileTime_UnixTimeStart: Int64 = $019DB1DED53E8000;
  // January 1, 1970 (start of Unix epoch) in "ticks"
  cFileTime_TicksPerMillisecond: Int64 = 10000; // a tick is 100ns

  // SetAsFileTime
  //
procedure TdwsDateTime.SetAsFileTime(const val: TFileTime);
var
  temp: TdwsLargeInteger;
begin
  temp.LowPart := val.dwLowDateTime;
  temp.HighPart := val.dwHighDateTime;
  FValue := (temp.QuadPart - cFileTime_UnixTimeStart)
    div cFileTime_TicksPerMillisecond;
end;

// GetAsDosDateTime
//
function TdwsDateTime.GetAsDosDateTime: Integer;
{$IFDEF WINDOWS}
var
  FILETIME: TFileTime;
  dosTime: LongRec;
begin
  FILETIME := AsFileTime;
  FileTimeToDosDateTime(FILETIME, dosTime.Hi, dosTime.Lo);
  result := Integer(dosTime);
end;
{$ELSE}

begin
  result := Integer(System.SysUtils.DateTimeToFileDate(Self.AsLocalDateTime));
end;
{$ENDIF}

// GetAsFileTime
//
function TdwsDateTime.GetAsFileTime: TFileTime;
var
  temp: TdwsLargeInteger;
begin
  temp.QuadPart := (FValue * cFileTime_TicksPerMillisecond) +
    cFileTime_UnixTimeStart;
  result.dwLowDateTime := temp.LowPart;
  result.dwHighDateTime := temp.HighPart;
end;

// GetAsUnixTime
//
function TdwsDateTime.GetAsUnixTime: Int64;
begin
  result := FValue div 1000;
end;

// SetAsUnixTime
//
procedure TdwsDateTime.SetAsUnixTime(const val: Int64);
begin
  FValue := val * 1000;
end;

// GetAsLocalDateTime
//
function TdwsDateTime.GetAsLocalDateTime: TDateTime;
begin
  result := UTCDateTimeToLocalDateTime(AsUTCDateTime);
end;

// SetAsLocalDateTime
//
procedure TdwsDateTime.SetAsLocalDateTime(const val: TDateTime);
begin
  AsUTCDateTime := LocalDateTimeToUTCDateTime(val);
end;

// GetAsUTCDateTime
//
function TdwsDateTime.GetAsUTCDateTime: TDateTime;
begin
  result := FValue / 864E5 + 25569;
end;

// SetAsUTCDateTime
//
procedure TdwsDateTime.SetAsUTCDateTime(const val: TDateTime);
begin
  FValue := Round((val - 25569) * 864E5);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

InitializeGetSystemMilliseconds;

{$IFDEF UNIX}
{$IFNDEF FPC}
{$IFDEF POSIXSYSLOG}
Posix.Syslog.openlog(nil, LOG_PID or LOG_NDELAY, LOG_DAEMON);
{$ENDIF}
{$ENDIF}
{$ENDIF}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$IFDEF UNIX}
{$IFNDEF FPC}
{$IFDEF POSIXSYSLOG}
  Posix.Syslog.closelog();
{$ENDIF}
{$ENDIF}
{$ENDIF}

end.
