{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsBackgroundWorkersLibModule;

interface

uses
  System.SysUtils, System.Classes,
  dwsExprs, dwsComp, dwsUtils, dwsXPlatform,
  dwsIOCPWorkerThreadPool, dwsWebEnvironment, dwsWebEnvironmentTypes,
  dwsJSON;

const
   cDefaultMaxWorkersPerQueue = 32;

type
   TBackgroundWorkEvent = procedure (const request : TWebRequest) of object;
   TBackgroundWorkLogEvent = procedure (const message : String) of object;

   TWorkQueues = TSimpleNameObjectHash<TIOCPWorkerThreadPool>;

   TdwsBackgroundWorkersLib = class;

   TWorkWebRequest = class (TWebRequest)
      private
         FHeaders : TStrings;
         FOwner : TdwsBackgroundWorkersLib;
         FPrev, FNext : TWorkWebRequest;

      protected
         function GetHeaders : TStrings; override;
         function GetUserAgent : String; override;

      public
         Task : String;
         Data : RawByteString;

         constructor Create(aModule : TdwsBackgroundWorkersLib);
         destructor Destroy; override;

         function RemoteIP : String; override;

         function RawURL : String; override;
         function URL : String; override;
         function FullURL : String; override;
         function Method : String; override;
         function MethodVerb : TWebRequestMethodVerb; override;

         function Security : String; override;
         function Secure : Boolean; override;

         function ContentLength : Integer; override;
         function ContentData : RawByteString; override;
         function ContentType : RawByteString; override;

         procedure Execute(workData : UInt32);
   end;

  TdwsBackgroundWorkersLib = class(TDataModule)
    dwsBackgroundWorkers: TdwsUnit;
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsCreateWorkQueueEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsDestroyWorkQueueEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueWorkEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueSizeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueDelayedWorkEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsGetWorkerCountEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsSetWorkerCountEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueStatusAsJSONEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
    FLibID : UInt32;
    FOnBackgroundWork : TBackgroundWorkEvent;
    FOnBackgroundLogEvent : TBackgroundWorkLogEvent;
    FPools : TWorkQueues;
    FPoolsCS : TMultiReadSingleWrite;
    FWorkUnitHead : TWorkWebRequest;
    FWorkUnitLock : TMultiReadSingleWrite;

    FMaxWorkersPerQueue : Integer;

  public
    { Public declarations }
    property OnBackgroundWork : TBackgroundWorkEvent read FOnBackgroundWork write FOnBackgroundWork;
    property OnBackgroundLogEvent : TBackgroundWorkLogEvent read FOnBackgroundLogEvent write FOnBackgroundLogEvent;
    property MaxWorkersPerQueue : Integer read FMaxWorkersPerQueue write FMaxWorkersPerQueue;
  end;

implementation

{$R *.dfm}

var
   vLibsMRSW : TMultiReadSingleWrite;
   vLibIDCounter : Integer;
   vBackgroundWorkerLibs : array of TdwsBackgroundWorkersLib;

// Create
//
constructor TWorkWebRequest.Create(aModule : TdwsBackgroundWorkersLib);
begin
   inherited Create;
   aModule.FWorkUnitLock.BeginWrite;
   try
      FOwner := aModule;
      FNext := aModule.FWorkUnitHead;
      if FNext <> nil then
         FNext.FPrev := Self;
      aModule.FWorkUnitHead := Self;
   finally
      aModule.FWorkUnitLock.EndWrite;
   end;
end;

// Destroy
//
destructor TWorkWebRequest.Destroy;
begin
   if FOwner <> nil then begin
      FOwner.FWorkUnitLock.BeginWrite;
      try
         if FPrev = nil then
            FOwner.FWorkUnitHead := FNext
         else FPrev.FNext := FNext;
         if FNext <> nil then
            FNext.FPrev := FPrev;
      finally
         FOwner.FWorkUnitLock.EndWrite;
      end;
   end;
   FHeaders.Free;
   inherited;
end;

// GetHeaders
//
function TWorkWebRequest.GetHeaders : TStrings;
begin
   if FHeaders=nil then
      FHeaders := TStringList.Create;
   Result := FHeaders;
end;

// GetUserAgent
//
function TWorkWebRequest.GetUserAgent : String;
begin
   Result := 'BackgroundWorker';
end;

// RemoteIP
//
function TWorkWebRequest.RemoteIP : String;
begin
   Result := '127.0.0.1';
end;

// RawURL
//
function TWorkWebRequest.RawURL : String;
begin
   Result := Task;
end;

// URL
//
function TWorkWebRequest.URL : String;
begin
   Result := Task;
end;

// FullURL
//
function TWorkWebRequest.FullURL : String;
begin
   Result := 'worker:' + Task;
end;

// Method
//
function TWorkWebRequest.Method : String;
begin
   Result := 'POST';
end;

// MethodVerb
//
function TWorkWebRequest.MethodVerb : TWebRequestMethodVerb;
begin
   Result := wrmvPOST
end;

// Security
//
function TWorkWebRequest.Security : String;
begin
   Result := '';
end;

// Secure
//
function TWorkWebRequest.Secure : Boolean;
begin
   Result := False;
end;

// ContentLength
//
function TWorkWebRequest.ContentLength : Integer;
begin
   Result := Length(Data);
end;

// ContentData
//
function TWorkWebRequest.ContentData : RawByteString;
begin
   Result := Data;
end;

// ContentType
//
function TWorkWebRequest.ContentType : RawByteString;
begin
   Result := 'application/octet-stream';
end;

// Execute
//
procedure TWorkWebRequest.Execute(workData : Uint32);

   procedure DoLog(lib : TdwsBackgroundWorkersLib; E : Exception);
   begin
      if Assigned(lib.FOnBackgroundLogEvent) then
         lib.FOnBackgroundLogEvent(E.ClassName + ': ' + E.Message);
   end;

var
   lib : TdwsBackgroundWorkersLib;
begin
   lib := FOwner;
   Assert(lib.FLibID = workData);
   try
      try
         if Assigned(lib.FOnBackgroundWork) then
            lib.FOnBackgroundWork(Self);
      except
         on E : Exception do DoLog(lib, E);
      end;
   finally
      Free;
   end;
end;

procedure TdwsBackgroundWorkersLib.DataModuleCreate(Sender: TObject);
begin
   FLibID := AtomicIncrement(vLibIDCounter);

   vLibsMRSW.BeginWrite;
   try
      var n := Length(vBackgroundWorkerLibs);
      Insert(Self, vBackgroundWorkerLibs, n);
   finally
      vLibsMRSW.EndWrite;
   end;

   FPoolsCS := TMultiReadSingleWrite.Create;
   FPools := TWorkQueues.Create;
   FMaxWorkersPerQueue := cDefaultMaxWorkersPerQueue;
   FWorkUnitLock := TMultiReadSingleWrite.Create;
end;

procedure TdwsBackgroundWorkersLib.DataModuleDestroy(Sender: TObject);
var
   i : Integer;
begin
   FOnBackgroundWork := nil;

   // remove all workers gracefully
   for i := 0 to FPools.HighIndex do
      if FPools.BucketObject[i] <> nil then
         FPools.BucketObject[i].WorkerCount := 0;
   // wait for completion
   for i := 0 to FPools.HighIndex do
      if FPools.BucketObject[i] <> nil then
         FPools.BucketObject[i].Shutdown;

   FPools.Clean;
   FreeAndNil(FPools);
   FreeAndNil(FPoolsCS);

   while FWorkUnitHead <> nil do
      FWorkUnitHead.Destroy;
   FreeAndNil(FWorkUnitLock);

   vLibsMRSW.BeginWrite;
   try
      for i := 0 to High(vBackgroundWorkerLibs) do begin
         if vBackgroundWorkerLibs[i] = Self then begin
            Delete(vBackgroundWorkerLibs, i, 1);
            Break;
         end;
      end;
   finally
      vLibsMRSW.EndWrite;
   end;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsCreateWorkQueueEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
begin
   if not Assigned(FOnBackgroundWork) then
      raise Exception.Create('Cannot create workers during shutdown');

   name:=Info.ParamAsString[0];
   FPoolsCS.BeginWrite;
   try
      pool:=FPools[name];
      Info.ResultAsBoolean:=(pool=nil);
      if pool=nil then begin
         pool:=TIOCPWorkerThreadPool.Create(1);
         FPools[name]:=pool;
      end;
   finally
      FPoolsCS.EndWrite;
   end;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsDestroyWorkQueueEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
begin
   if not Assigned(FOnBackgroundWork) then Exit;

   name:=Info.ParamAsString[0];
   FPoolsCS.BeginWrite;
   try
      pool:=FPools[name];
      if pool<>nil then
         FPools[name]:=nil;
   finally
      FPoolsCS.EndWrite;
   end;
   Info.ResultAsBoolean:=(pool<>nil);
   if pool<>nil then begin
      pool.Shutdown;
      pool.Free;
   end;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueSizeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
   n : Integer;
begin
   name := Info.ParamAsString[0];
   FPoolsCS.BeginRead;
   try
      pool:=FPools[name];
      if pool<>nil then
         n := pool.QueueSize
      else n := 0;
   finally
      FPoolsCS.EndRead;
   end;
   Info.ResultAsInteger:=n;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueWorkEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   workUnit : TWorkWebRequest;
   pool : TIOCPWorkerThreadPool;
begin
   if not Assigned(FOnBackgroundWork) then Exit;

   name:=Info.ParamAsString[0];

   workUnit:=TWorkWebRequest.Create(Self);
   workUnit.Task:=Info.ParamAsString[1];
   workUnit.Data:=Info.ParamAsDataString[2];

   FPoolsCS.BeginRead;
   try
      pool:=FPools[name];
      if pool<>nil then
         pool.QueueWork(workUnit.Execute, FLibID);
   finally
      FPoolsCS.EndRead;
   end;

   if pool=nil then begin
      workUnit.Free;
      raise Exception.CreateFmt('Unknown Work Queue "%s"', [name]);
   end;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueDelayedWorkEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   workUnit : TWorkWebRequest;
   pool : TIOCPWorkerThreadPool;
   delayMilliseconds : Integer;
begin
   if not Assigned(FOnBackgroundWork) then Exit;

   name := Info.ParamAsString[0];

   delayMilliseconds := Round(Info.ParamAsFloat[1] * 1000);

   workUnit := TWorkWebRequest.Create(Self);
   workUnit.Task := Info.ParamAsString[2];
   workUnit.Data := Info.ParamAsDataString[3];

   FPoolsCS.BeginRead;
   try
      pool := FPools[name];
      if pool <> nil then
         pool.QueueDelayedWork(delayMilliseconds, workUnit.Execute, FLibID);
   finally
      FPoolsCS.EndRead;
   end;

   if pool = nil then begin
      workUnit.Free;
      raise Exception.CreateFmt('Unknown Work Queue "%s"', [name]);
   end;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsGetWorkerCountEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
   n : Integer;
begin
   name := Info.ParamAsString[0];
   FPoolsCS.BeginRead;
   try
      pool := FPools[name];
      if pool <> nil then
         n := pool.WorkerCount
      else n := 0;
   finally
      FPoolsCS.EndRead;
   end;
   Info.ResultAsInteger := n;
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsSetWorkerCountEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
   n : Integer;
begin
   name := Info.ParamAsString[0];
   n := Info.ParamAsInteger[1];
   if n <= 0 then
      raise Exception.CreateFmt('WorkerCount value must be strictly positive (got %d)', [n]);
   if n > MaxWorkersPerQueue then
      raise Exception.CreateFmt('WorkerCount value too high (got %d, must be <= %d)', [n, MaxWorkersPerQueue]);
   if (n <> 0) and not Assigned(FOnBackgroundWork) then
      raise Exception.Create('Cannot create workers during shutdown');
   FPoolsCS.BeginRead;
   try
      pool := FPools[name];
      if pool <> nil then
         pool.WorkerCount := n;
   finally
      FPoolsCS.EndRead;
   end;
   if pool=nil then
      raise Exception.CreateFmt('Unknown Work Queue "%s"', [name]);
end;

procedure TdwsBackgroundWorkersLib.dwsBackgroundWorkersClassesBackgroundWorkersMethodsQueueStatusAsJSONEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   name : String;
   pool : TIOCPWorkerThreadPool;
   wr : TdwsJSONWriter;
   sizeInfo : TWorkerThreadQueueSizeInfo;
begin
   name := Info.ParamAsString[0];
   FPoolsCS.BeginRead;
   try
      pool := FPools[name];
   finally
      FPoolsCS.EndRead;
   end;
   wr := TdwsJSONWriter.Create;
   try
      wr.BeginObject;
         wr.WriteString('name', name);
         if pool <> nil then begin
            wr.BeginObject('workers');
               wr.WriteInteger('count', pool.WorkerCount);
               wr.WriteInteger('live', pool.LiveWorkerCount);
               wr.WriteInteger('active', pool.ActiveWorkerCount);
               wr.WriteInteger('peak', pool.PeakActiveWorkerCount);
            wr.EndObject;
            wr.BeginObject('queue');
               sizeInfo := pool.QueueSizeInfo;
               wr.WriteInteger('total', sizeInfo.Total);
               wr.WriteInteger('delayed', sizeInfo.Delayed);
               wr.WriteInteger('peak', sizeInfo.Peak);
            wr.EndObject;
         end;
      wr.EndObject;
      Info.ResultAsString := wr.ToString;
   finally
      wr.Free;
   end;
   if (pool <> nil) and Info.ParamAsBoolean[1] then
      pool.ResetPeakStats;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vLibsMRSW := TMultiReadSingleWrite.Create;

finalization

   FreeAndNil(vLibsMRSW);

end.
