program dws;
{$SetPEFlags $0001}
{$IFNDEF VER200} // delphi 2009
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
{$APPTYPE CONSOLE}
{$r *.dres}
uses
  Winapi.Windows, Winapi.ActiveX,
  System.Classes,
  System.SysUtils,
  dwsXPlatform,
  dwsComp,
  dwsCompiler,
  dwsExprs,
  dwsUtils,
  dwsFunctions,
  dwsUnitSymbols,
  SynZip,
  dwsMathFunctions,
  dwsStringFunctions,
  dwsTimeFunctions,
  dwsVariantFunctions,
  dwsFileFunctions,
  dwsBigIntegerFunctions.GMP,
  dwsMPIR.Bundle,
  dwsClassesLibModule,
  dwsZipLibModule,
  dwsEncodingLibModule,
  dwsCryptoLibModule,
  dwsWebLibModule,
  dwsDatabaseLibModule,
  dwsSystemInfoLibModule,
  dwsComConnector,
  dwsJSONConnector,
  dwsSynSQLiteDatabase,
  dwsSynODBCDatabase,
  dwsWMIDatabase,
  dwsGraphicLibrary,
  dwsTurboJPEG.Bundle,
  dwsErrors,
  dwsJIT,
  {$ifdef WIN32} dwsJITx86, {$endif}
  {$ifdef WIN64} dwsJITx86_64, {$endif}
  dwsRunnerProject in 'dwsRunnerProject.pas';

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE or IMAGE_FILE_RELOCS_STRIPPED}

type
   TUnitProvider = class
      function DoNeedUnitEx(const unitName : String; var unitSource, unitLocation : String) : IdwsUnit;
   end;

function TUnitProvider.DoNeedUnitEx(const unitName : String; var unitSource, unitLocation : String) : IdwsUnit;
begin
   unitLocation := unitName + '.pas';
   unitSource := LoadTextFromFile(unitLocation);
   if unitSource = '' then
      unitLocation := '';
end;

function CreateScript : TDelphiWebScript;
begin
   Result:=TDelphiWebScript.Create(nil);
   Result.OnNeedUnitEx := TUnitProvider(nil).DoNeedUnitEx;
   TdwsComConnector.Create(Result).Script:=Result;
   TdwsJSONLibModule.Create(Result).Script:=Result;
   TdwsClassesLib.Create(Result).dwsUnit.Script:=Result;
   TdwsEncodingLib.Create(Result).dwsEncoding.Script:=Result;
   TdwsCryptoLib.Create(Result).dwsCrypto.Script:=Result;
   TdwsZipLib.Create(Result).dwsZip.Script:=Result;
   TdwsWebLib.Create(Result).dwsWeb.Script:=Result;
   TdwsDatabaseLib.Create(Result).dwsDatabase.Script:=Result;
   TdwsSystemInfoLibModule.Create(Result).Script:=Result;
end;
procedure WriteHeader;
begin
   Writeln('dws Runner - sample code runner for DWScript');
   Writeln('');
end;
{$WARN SYMBOL_PLATFORM OFF}
// MakeExecutable
//
procedure MakeExecutable;
var
   zw : TZipWrite;
   sourceName, zipFileName, exeName : TFileName;
   buf : RawByteString;
   prog : IdwsProgram;
   script : TDelphiWebScript;
   searchRec : TSearchRec;
   found : Integer;
begin
   WriteHeader;
   if ParamCount<2 then begin
      Writeln('Missing zipfile name');
      Exit;
   end;
   sourceName:=ParamStr(2);
   WriteLn('...Starting make for "', sourceName, '"');
   if not StrEndsWith(sourceName, '.zip') then begin
      zipFileName:=ChangeFileExt(sourceName, '.zip');
      WriteLn('...Zipping to "', zipFileName, '"');
      zw:=TZipWrite.Create(zipFileName);
      try
         if DirectoryExists(sourceName) then begin
            sourceName:=IncludeTrailingPathDelimiter(sourceName);
            found:=FindFirst(sourceName+'*.*', faArchive or faReadOnly, searchRec);
            while found=0 do begin
               zw.AddDeflated(sourceName+searchRec.Name, True, 9);
               found:=FindNext(searchRec);
            end;
            FindClose(searchRec);
         end else zw.AddDeflated(sourceName, True, 9);
      finally
         zw.Free;
      end;
   end else begin
      zipFileName:=sourceName;
   end;
   if ParamCount>2 then
      exeName:=ParamStr(3)
   else exeName:=ChangeFileExt(zipFileName, '.exe');
   var zip := TZipProject.Create(zipFileName);
   script := CreateScript;
   try
      prog:=script.Compile(zip.Attach(script));
      try
         if prog.Msgs.Count>0 then begin
            WriteLn('...Compiled with ', prog.Msgs.Count, ' message(s):');
            WriteLn(prog.Msgs.AsInfo);
            if prog.Msgs.HasErrors then begin
               Write('...Generation aborted');
               Exit;
            end;
         end else begin
            WriteLn('...Compiled without errors.');
         end;
      finally
         prog:=nil;
      end;
   finally
      script.Free;
      FreeAndNil(zip);
   end;
   if not FileCopy(ParamStr(0), exeName, False) then begin
      Writeln('...Failed to create "', exeName, '"');
   end;
   var fs := TFileStream.Create(zipFileName, fmOpenRead or fmShareDenyNone);
   try
      SetLength(buf, fs.Size);
      if buf <> '' then
         fs.Read(buf[1], Length(buf));
   finally
      fs.Free;
   end;
   var hUpdate := BeginUpdateResource(Pointer(exeName), False);
   try
      UpdateResource(hUpdate, RT_RCDATA, 'SCRIPT', 0, Pointer(buf), Length(buf));
   finally
      EndUpdateResource(hUpdate, False);
   end;
   WriteLn('..."', exeName, '" generated successfully!');
end;
// Execute
//
procedure Execute(project : TRunnerProject; paramOffset : Integer; embedded, compileOnly : Boolean);
var
   script : TDelphiWebScript;
   source : String;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   params : array of Variant;
begin
   try
      script:=CreateScript;
      try
         source:=project.Attach(script);
         prog:=script.Compile(source);
         try
            if compileOnly then begin
               Writeln(prog.Msgs.AsInfo);
               Writeln('Compiled with ', prog.Msgs.Count, ' message(s)');
               Exit;
            end;
            if prog.Msgs.Count>0 then begin
               if prog.Msgs.HasErrors or not embedded then
                  Writeln(prog.Msgs.AsInfo);
               if prog.Msgs.HasErrors then Exit;
            end;

            var jit := {$ifdef WIN64}TdwsJITx86_64.Create{$else}TdwsJITx86.Create{$endif};
            jit.GreedyJIT(prog.ProgramObject);
            jit.Free;

            SetLength(params, ParamCount-paramOffset+2);
            params[0]:=ParamStr(0);
            for var i := paramOffset to ParamCount do
               params[i-paramOffset+1]:=ParamStr(i);
            exec:=prog.ExecuteParam(params);
            try
               Writeln(exec.Result.ToString);
               if exec.Msgs.Count>0 then
                  Writeln(exec.Msgs.AsInfo);
            finally
               exec := nil;
            end;
         finally
            prog := nil;
         end;
      finally
         project.Free;
         script.Free;
      end;
   except
      on E: Exception do
         Writeln(E.ClassName, ': ', E.Message);
   end;
end;

function RunCommandLine(var compileOnly : Boolean) : TRunnerProject;
var
   fileName : String;
begin
   if ParamCount<1 then begin
      WriteHeader;
      Writeln('Run a simple script with:');
      Writeln('   dws <sourcefile> [param1] [param2] ... [paramN]');
      Writeln('');
      Writeln('Run a zip project with (starts from "main.pas" in the zip):');
      Writeln('   dws <zipfile> [param1] [param2] ... [paramN]');
      Writeln('');
      Writeln('Bundle a zip project into an executable:');
      Writeln('   dws make <zipFile|sourcefile> [exeName]');
      Writeln('');
      Writeln('Compile but do not run a script:');
      Writeln('   dws compile <zipFile|sourcefile>');
      Exit(nil);
   end;
   fileName:=ParamStr(1);
   if fileName='make' then begin
      MakeExecutable;
      Exit(nil);
   end;
   if fileName = 'compile' then begin
      compileOnly := True;
      fileName := ParamStr(2);
   end;
   if FileExists(fileName) then
      if StrEndsWith(fileName, '.zip') then
         Result := TZipProject.Create(fileName)
      else Result := TFileProject.Create(fileName)
   else if DirectoryExists(fileName) then
      Result := TDirectoryProject.Create(fileName)
   else begin
      Writeln('File "', fileName, '" not found.');
      Exit(nil);
   end;
end;

var
   paramOffset : Integer;
   project : TRunnerProject;
   zr : TZipRead;
   compileOnly, embedded : Boolean;
begin
   zr:=TZipRead.Create(HInstance, 'SCRIPT', RT_RCDATA);
   if zr.Count=0 then begin
      FreeAndNil(zr);
      project:=nil;
      paramOffset:=2;
      embedded:=False;
   end else begin
      project:=TZipProject.Create(zr);
      paramOffset:=1;
      embedded:=True;
   end;
   compileOnly := False;
   if project=nil then
      project := RunCommandLine(compileOnly);
   if project <> nil then begin
      CoInitialize(nil);
      Execute(project, paramOffset, embedded, compileOnly);
   end;
end.
