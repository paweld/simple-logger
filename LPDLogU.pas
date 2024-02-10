{==============================================================================

MIT License

Copyright (c) 2023-2024 Paweł Dmitruk (paweld), https://github.com/paweld

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

==============================================================================}

{
Simple, thread-safe logger for fpc/lazarus.
Logs saved to file, support for log archiving and compression.
}

unit LPDLogU;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses
  {$ifdef unix}cmem, cthreads,{$endif}
  Classes, SysUtils, zipper, DateUtils, fgl;

type
  TLogType = (ltInfo, ltError, ltDebug, ltWarning, ltFatal, ltTrace, ltSuccess, ltCritical, ltException);

  TLogger = class;

  { TLogRec }

  TLogRec = record
    dt: TDateTime;
    typ: TLogType;
    msg: String;
    function ToString: String;
  end;
  PLogRec = ^TLogRec;

  { TLogList }

  TLogList = class(specialize TFPGList<PLogRec>)
  public
    destructor Destroy; override;
  end;

  { TLogArchList }

  TLogArchList = class(specialize TFPGMap<String, Integer>)
  public
    constructor Create;
    function GetArchSize: Int64;
  end;

  { TCCThread }

  TCCThread = class(TThread)
  private
    FFileName: String;
    FCompressLog: Boolean;
    FLogsCount: Integer;
    FArchList: TLogArchList;
    FMaxArchSize: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(aFileName: String; aCompressLog: Boolean; aArchList: TLogArchList; aLogsCount: Integer; aMaxArchSize: Int64);
  end;

  { TSaveLogThread }

  TSaveLogThread = class(TThread)
  private
  const
    MaxLogSize: Int64 = 50 * 1024 * 1024;       //the size above which it will archive the log, 0-don't archive
    LogsCount: Integer = 100;                   //number of previous logs retained - 0-don't check, 1-only current log
    CompressLog: Boolean = True;                //compress log after archive
    ArchDirName: String = 'logs';               //folder to move log archives to
    MaxArchSize: Int64 = 300 * 1024 * 1024;     //max size of logs in archive - 0-don't check
    LogExt: String = '.log';                    //log file ext
  private
    FTmpLogs: TLogList;
    FArchList: TLogArchList;
    FFileActive: Boolean;
    FFile: TextFile;
    FLogFileName: String;
    FArchPath: String;
    FCurrLogFileSize: Int64;
    FLastArchiveTime: TDateTime;
    FCountArchiveInTime: Integer;
    FParentLogger: TLogger;
    procedure ArchiveLog;
    procedure OpenLogFile;
    procedure CloseLogFile;
    procedure FlushLogFile;
    procedure GetLogs;
    function SaveToLogFile: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(aParent: TLogger);
    destructor Destroy; override;
  end;

  { TLogger }

  TLogger = class
  private
    FSaveThread: TSaveLogThread;
    FLogList: TLogList;
    Ffs: TFormatSettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddLog(msgFmt: String; params: array of const; logtype: TLogType = ltError);
    procedure AddLog(msg: String; logtype: TLogType = ltError);
    procedure AddLog(msgFmt: String; params: array of const; err: Boolean);
    procedure AddLog(msg: String; err: Boolean);
    procedure AddInfo(msgFmt: String; params: array of const);
    procedure AddInfo(msg: String);
    procedure AddError(msgFmt: String; params: array of const);
    procedure AddError(msg: String);
    procedure AddDebug(msgFmt: String; params: array of const);
    procedure AddDebug(msg: String);
    procedure AddWarn(msgFmt: String; params: array of const);
    procedure AddWarn(msg: String);
    procedure AddFatal(msgFmt: String; params: array of const);
    procedure AddFatal(msg: String);
    procedure AddTrace(msgFmt: String; params: array of const);
    procedure AddTrace(msg: String);
    procedure AddSucc(msgFmt: String; params: array of const);
    procedure AddSucc(msg: String);
    procedure AddCritical(msgFmt: String; params: array of const);
    procedure AddCritical(msg: String);
    procedure AddExcept(msgFmt: String; params: array of const);
    procedure AddExcept(msg: String);
  end;

const
  LogTypeArr: array [TLogType] of String = ('INFO', 'ERROR', 'DEBUG', 'WARN', 'FATAL', 'TRACE', 'SUCC', 'CRITICAL', 'EXCEPT');

var
  lpdlogCS: TRTLCriticalSection;
  archlistCS: TRTLCriticalSection;
  Logger: TLogger;

implementation

function GetFileSize(filename: String): Integer;
var
  fileinfo: TSearchRec;
begin
  Result := 0;
  if FindFirst(filename, faAnyFile, fileinfo) = 0 then
  begin
    if (fileinfo.Attr and faDirectory) <> faDirectory then
      Result := fileinfo.Size;
    FindClose(fileinfo);
  end;
end;

procedure FindArchLogs(mask: String; var aArchList: TLogArchList);
var
  fileinfo: TSearchRec;
  dir: String;
begin
  dir := ExtractFilePath(mask);
  if FindFirst(mask, faAnyFile, fileinfo) = 0 then
  begin
    repeat
      if ((fileinfo.Attr and faDirectory) <> faDirectory) and (aArchList.IndexOf(dir + fileinfo.Name) < 0) then
        aArchList.Add(dir + fileinfo.Name, fileinfo.Size);
    until FindNext(fileinfo) <> 0;
    FindClose(fileinfo);
  end;
end;

{ TLogRec }

function TLogRec.ToString: String;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', dt) + #9 + LogTypeArr[typ] + #9 + msg;
end;

{ TLogList }

destructor TLogList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(PLogRec(Items[i]));
  inherited Destroy;
end;

{ TLogArchList }

constructor TLogArchList.Create;
begin
  inherited Create;
  Duplicates := dupIgnore;
  Sorted := False;
end;

function TLogArchList.GetArchSize: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Inc(Result, Data[i]);
end;

{ TCCThread }

procedure TCCThread.Execute;
var
  zip: TZipper;
  fn: String = '';
begin
  if FCompressLog and FileExists(FFileName) then
  begin
    zip := TZipper.Create;
    zip.FileName := ChangeFileExt(FFileName, '.zip');
    try
      zip.Entries.AddFileEntry(FFileName, ExtractFileName(FFileName));
      zip.ZipAllFiles;
      DeleteFile(FFileName);
      if FLogsCount > 1 then
        fn := zip.FileName;
    except
      if FLogsCount > 1 then
        fn := FFileName;
    end;
    zip.Free;
  end;
  if fn <> '' then
  begin
    EnterCriticalSection(archlistCS);
    try
      FindArchLogs(fn, FArchList);
      while (FLogsCount > 1) and ((FArchList.Count > FLogsCount) or ((FMaxArchSize > 0) and (FMaxArchSize < FArchList.GetArchSize))) do
      begin
        DeleteFile(FArchList.Keys[0]);
        FArchList.Delete(0);
      end;
    finally
      LeaveCriticalSection(archlistCS);
    end;
  end;
end;

constructor TCCThread.Create(aFileName: String; aCompressLog: Boolean; aArchList: TLogArchList; aLogsCount: Integer; aMaxArchSize: Int64);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FFileName := aFileName;
  FCompressLog := aCompressLog;
  FArchList := aArchList;
  FLogsCount := aLogsCount;
  FMaxArchSize := aMaxArchSize;
  Start;
end;

{ TFileLogThread }

procedure TSaveLogThread.ArchiveLog;
var
  _now: TDateTime;
  fname, archsufix, archlogname: String;
begin
  if not FFileActive or (MaxLogSize = 0) or (MaxLogSize > FCurrLogFileSize) then
    exit;
  if LogsCount = 1 then
  begin
    CloseLogFile;
    DeleteFile(FLogFileName);
    OpenLogFile;
  end
  else
  begin
    _now := Now;
    if SecondsBetween(_now, FLastArchiveTime) = 0 then
      Inc(FCountArchiveInTime)
    else
    begin
      FLastArchiveTime := _now;
      FCountArchiveInTime := 0;
    end;
    fname := ChangeFileExt(ExtractFileName(FLogFileName), '');
    archsufix := '~llog_' + FormatDateTime('yyyymmdd_hhnnss', _now) + '_' + IntToHex(FCountArchiveInTime, 3) + LogExt;
    archlogname := FArchPath + fname + archsufix;
    if not DirectoryExists(FArchPath) then
      CreateDir(FArchPath);
    CloseLogFile;
    if RenameFile(FLogFileName, archlogname) then
    begin
      OpenLogFile;
      TCCThread.Create(archlogname, CompressLog, FArchList, LogsCount, MaxArchSize);
    end
    else
      OpenLogFile;
  end;
end;

procedure TSaveLogThread.OpenLogFile;
var
  i: Integer;
begin
  if FLogFileName = '' then
    FLogFileName := ChangeFileExt(ParamStr(0), LogExt);
  i := 0;
  while not FFileActive and (i <= 3) do
  begin
    Inc(i);
    if FileExists(FLogFileName) then
      FCurrLogFileSize := GetFileSize(FLogFileName)
    else
      FCurrLogFileSize := 0;
    AssignFile(FFile, FLogFileName);
    try
      if FileExists(FLogFileName) then
        Append(FFile)
      else
        Rewrite(FFile);
      FFileActive := True;
    except
      FLogFileName := ChangeFileExt(ParamStr(0), '_' + IntToStr(GetProcessID) + LogExt);
      if i > 1 then
        FLogFileName := ChangeFileExt(ParamStr(0), '@' + IntToStr(GetTickCount64) + LogExt);
    end;
  end;
  if not FFileActive then
    FLogFileName := '';
end;

procedure TSaveLogThread.CloseLogFile;
begin
  if FFileActive then
  begin
    CloseFile(FFile);
    FFileActive := False;
  end;
end;

procedure TSaveLogThread.FlushLogFile;
begin
  CloseLogFile;
  OpenLogFile;
end;

procedure TSaveLogThread.GetLogs;
begin
  if FFileActive then
  begin
    EnterCriticalSection(lpdlogCS);
    try
      FTmpLogs.Assign(FParentLogger.FLogList);
      FParentLogger.FLogList.Clear;
    finally
      LeaveCriticalSection(lpdlogCS);
    end;
  end;
end;

function TSaveLogThread.SaveToLogFile: Integer;
var
  i, l, c: Integer;
  s: String;
  r: PLogRec;
begin
  if (FTmpLogs.Count = 0) or (FTmpLogs.Count > 10) then
    Result := 250
  else
    Result := 100;
  if not FFileActive or (FTmpLogs.Count = 0) then
    exit;
  c := 0;
  for i := 0 to FTmpLogs.Count - 1 do
  begin
    ArchiveLog;
    r := FTmpLogs.Items[i];
    s := r^.ToString;
    Dispose(r);
    l := Length(s);
    while s[l] in [#10, #13] do
    begin
      Dec(l);
      s := Copy(s, 1, l);
    end;
    Inc(FCurrLogFileSize, l);
    Inc(c, l);
    WriteLn(FFile, s);
    if (c > 100 * 1024) and (Result > i) then
    begin
      c := 0;
      FlushLogFile;
    end;
  end;
  CloseLogFile;
  FTmpLogs.Clear;
end;

procedure TSaveLogThread.Execute;
var
  sleepms: Integer;
  cs: QWord;

  function GetSleepMs: Integer; inline;
  begin
    if FFileActive then
      Result := 0
    else
      Result := 1000;
  end;

begin
  sleepms := 0;
  cs := GetTickCount64;
  while not Terminated do
  begin
    if GetTickCount64 - cs >= sleepms then
    begin
      OpenLogFile;
      if FFileActive then
      begin
        GetLogs;
        sleepms := SaveToLogFile;
      end
      else
        sleepms := GetSleepMs;
      cs := GetTickCount64;
    end;
    sleep(1);
  end;
  CloseLogFile;
end;

constructor TSaveLogThread.Create(aParent: TLogger);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTmpLogs := TLogList.Create;
  FArchList := TLogArchList.Create;
  FFileActive := False;
  FLogFileName := '';
  FArchPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + ArchDirName);
  FLastArchiveTime := Now;
  FCountArchiveInTime := 0;
  FParentLogger := aParent;
  if LogsCount > 1 then
    FindArchLogs(FArchPath + '*~llog_*', FArchList);
  Start;
end;

destructor TSaveLogThread.Destroy;
begin
  FArchList.Free;
  FTmpLogs.Free;
  inherited Destroy;
end;

{ TLogger }

constructor TLogger.Create;
begin
  inherited Create;
  FLogList := TLogList.Create;
  //format settings
  Ffs.DecimalSeparator := '.';
  Ffs.ShortDateFormat := 'yyyy-mm-dd';
  Ffs.LongDateFormat := 'yyyy-mm-dd';
  Ffs.DateSeparator := '-';
  Ffs.ShortTimeFormat := 'hh:nn:ss';
  Ffs.LongTimeFormat := 'hh:nn:ss.zzz';
  Ffs.TimeSeparator := ':';
  FSaveThread := TSaveLogThread.Create(Self);
end;

destructor TLogger.Destroy;
begin
  while FLogList.Count > 0 do
    sleep(1); 
  if FSaveThread <> nil then
  begin
    FSaveThread.Terminate;
    FSaveThread.WaitFor;
    FSaveThread.Free;
    FSaveThread := nil;
  end;
  FLogList.Free;
  inherited Destroy;
end;

procedure TLogger.AddLog(msgFmt: String; params: array of const; logtype: TLogType);
var
  r: PLogRec;
begin
  New(r);
  r^.dt := Now;
  r^.typ := logtype;
  if Length(params) > 0 then
    r^.msg := Format(msgFmt, params, Ffs)
  else
    r^.msg := msgFmt;
  EnterCriticalSection(lpdlogCS);
  try
    FLogList.Add(r);
  finally
    LeaveCriticalSection(lpdlogCS);
  end;
end;

procedure TLogger.AddLog(msg: String; logtype: TLogType);
begin
  AddLog(msg, [], logtype);
end;

procedure TLogger.AddLog(msgFmt: String; params: array of const; err: Boolean);
begin
  AddLog(msgFmt, params, TLogType(Ord(err)));
end;

procedure TLogger.AddLog(msg: String; err: Boolean);
begin
  AddLog(msg, [], TLogType(Ord(err)));
end;

procedure TLogger.AddInfo(msgFmt: String; params: array of const);
begin
  AddLog(msgFmt, params, ltInfo);
end;

procedure TLogger.AddInfo(msg: String);
begin
  AddLog(msg, [], ltInfo);
end;

procedure TLogger.AddError(msgFmt: String; params: array of const);
begin
  AddLog(msgFmt, params, ltError);
end;

procedure TLogger.AddError(msg: String);
begin
  AddLog(msg, [], ltError);
end;

procedure TLogger.AddDebug(msgFmt: String; params: array of const);
begin
  AddLog(msgFmt, params, ltDebug);
end;

procedure TLogger.AddDebug(msg: String);
begin
  AddLog(msg, [], ltDebug);
end;

procedure TLogger.AddWarn(msgFmt: String; params: array of const);
begin
  AddLog(msgFmt, params, ltWarning);
end;

procedure TLogger.AddWarn(msg: String);
begin
  AddLog(msg, [], ltWarning);
end;

procedure TLogger.AddFatal(msgFmt: String; params: array of const);
begin
  AddLog(msgFmt, params, ltFatal);
end;

procedure TLogger.AddFatal(msg: String);
begin
  AddLog(msg, [], ltFatal);
end;

procedure TLogger.AddTrace(msgFmt: String; params: array of const);
begin
  AddLog(msgFmt, params, ltTrace);
end;

procedure TLogger.AddTrace(msg: String);
begin
  AddLog(msg, [], ltTrace);
end;

procedure TLogger.AddSucc(msgFmt: String; params: array of const);
begin
  AddLog(msgFmt, params, ltSuccess);
end;

procedure TLogger.AddSucc(msg: String);
begin
  AddLog(msg, [], ltSuccess);
end;

procedure TLogger.AddCritical(msgFmt: String; params: array of const);
begin
  AddLog(msgFmt, params, ltCritical);
end;

procedure TLogger.AddCritical(msg: String);
begin
  AddLog(msg, [], ltCritical);
end;

procedure TLogger.AddExcept(msgFmt: String; params: array of const);
begin
  AddLog(msgFmt, params, ltException);
end;

procedure TLogger.AddExcept(msg: String);
begin
  AddLog(msg, [], ltException);
end;

initialization
  InitCriticalSection(lpdlogCS);
  InitCriticalSection(archlistCS);
  Logger := TLogger.Create;

finalization
  Logger.Free;
  DoneCriticalSection(archlistCS);
  DoneCriticalSection(lpdlogCS);

end.
