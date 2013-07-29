unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LConvEncoding, Windows, JwaTlHelp32, dateutils;

type
  TMyLogFlag = (mlfError, mlfInfo);

type

  { TFreenetUpdaterForm }

  TFreenetUpdaterForm = class(TForm)
    Timer_MonitorPID: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer_MonitorPIDStartTimer(Sender: TObject);
    procedure Timer_MonitorPIDTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

type
  TManifestData = record
    sWrapperPid, sJavaPid: string;
    iWrapperPid, iJavaPid: integer;
    FilePath: array [0..2, 0..2] of string;  // [File1|File2|File3 , from|to|tobackup]
    LogFile: string;
  end;

procedure UMainProcess;

function UReadManifest(ManifestFilePath: string): boolean;
function UCheckManifestValues(): boolean;
function UBackupFiles(): boolean;
function UCopyFiles(): boolean;
function URestoreFiles(): boolean;
function FindProcessByID(const pPID: integer): boolean;
function simpleMyLog(LogFlag: TMyLogFlag; LogLine: string; AppendNewLine: boolean = True): boolean;

var
  FreenetUpdaterForm: TFreenetUpdaterForm;
  UpdManifest: TManifestData;
  LogFilePath: string;
  BackupDir: string;
  tMonitorPidStarted: TDateTime;

implementation

{$R *.lfm}

{ TFreenetUpdaterForm }

procedure TFreenetUpdaterForm.FormCreate(Sender: TObject);
begin
  // No need to see the GUI
  FreenetUpdaterForm.WindowState:= wsMinimized;
  FreenetUpdaterForm.ShowInTaskBar:= stNever;

  if UReadManifest('freenetupdater.ini') then
    if UCheckManifestValues() then
      Timer_MonitorPID.Enabled := True
    else
    begin
      simpleMyLog(mlfInfo, 'Something went wrong during Manifest checking');
      simpleMyLog(mlfInfo, 'Update process canceled');
    end;
end;

procedure TFreenetUpdaterForm.Timer_MonitorPIDStartTimer(Sender: TObject);
begin
  simpleMyLog(mlfInfo, 'Wait ' + UpdManifest.sWrapperPid + ' (WrapperPid) and ' +
    UpdManifest.sJavaPid + ' (JavaPid)' + ' shutdowns');

  tMonitorPidStarted := Now;
end;

procedure TFreenetUpdaterForm.Timer_MonitorPIDTimer(Sender: TObject);
begin
  if not FindProcessByID(UpdManifest.iWrapperPid) and not FindProcessByID(UpdManifest.iJavaPid) then
  begin
    simpleMyLog(mlfInfo, UpdManifest.sWrapperPid + ' and ' + UpdManifest.sJavaPid +
      ' no longer exist. Start Update process');
    UMainProcess;        // We call the main Update process
    Timer_MonitorPID.Enabled := False;
  end
  else
  begin
    if FindProcessByID(UpdManifest.iWrapperPid) then
      simpleMyLog(mlfInfo, UpdManifest.sWrapperPid + ' still running');

    if FindProcessByID(UpdManifest.iJavaPid) then
      simpleMyLog(mlfInfo, UpdManifest.sJavaPid + ' still running');

    if SecondsBetween(tMonitorPidStarted, Now) >= 5 * 60 then // Max waiting of 5 minutes
    begin
      simpleMyLog(mlfInfo, 'Take to long, update canceled');
      simpleMyLog(mlfInfo, 'FreenetUpdater stop');
      Application.Terminate;
    end;
  end;

end;

procedure UMainProcess;
begin
  if UBackupFiles() then
  begin
    if UCopyFiles() then
    begin
      simpleMyLog(mlfInfo, 'Successfull update');

      { TODO :
       Launch freenet
       Delete the manifest file
       and the backup dir ?}

      Application.Terminate;
    end
    else
    begin
      simpleMyLog(mlfInfo, 'Something went wrong during copy');
      simpleMyLog(mlfInfo, 'Update process canceled');
      URestoreFiles();
      { TODO : check result of URestoreFiles }

    end;
  end
  else
  begin
    simpleMyLog(mlfInfo, 'Something went wrong during backup');
    simpleMyLog(mlfInfo, 'Update process canceled');
  end;
end;


function UReadManifest(ManifestFilePath: string): boolean;
var
  slFileData: TStringList;
begin
  Result := True;

  LogFilePath := 'updater.log'; // Set a default logfile

  if not FileExistsUTF8(ManifestFilePath) then
  begin
    simpleMyLog(mlfError, 'Manifest file not found');
    Result := False;
    Application.Terminate;
  end
  else
  begin
    slFileData := TStringList.Create;

    slFileData.LoadFromFile(ManifestFilePath);

    // Read entries
    UpdManifest.sWrapperPid := slFileData.Values['wrapper.pid'];
    UpdManifest.sJavaPid := slFileData.Values['java.pid'];
    UpdManifest.FilePath[0][0] := slFileData.Values['file1.from'];
    UpdManifest.FilePath[0][1] := slFileData.Values['file1.to'];
    UpdManifest.FilePath[1][0] := slFileData.Values['file2.from'];
    UpdManifest.FilePath[1][1] := slFileData.Values['file2.to'];
    UpdManifest.FilePath[2][0] := slFileData.Values['file3.from'];
    UpdManifest.FilePath[2][1] := slFileData.Values['file3.to'];
    UpdManifest.LogFile := slFileData.Values['logfile'];

    slFileData.Free;

    Result := True;

  end;
end;

function UCheckManifestValues(): boolean;
var
  i: integer;
begin
  Result := True;

  if UpdManifest.LogFile <> '' then
    LogFilePath := UpdManifest.LogFile;

  simpleMyLog(mlfInfo, 'FreenetUpdater start');

  // Wrapper.pid
  if UpdManifest.sWrapperPid <> '' then
  begin
    simpleMyLog(mlfInfo, 'wrapper.pid is ' + UpdManifest.sWrapperPid);
    try
      UpdManifest.iWrapperPid := StrToInt(UpdManifest.sWrapperPid);
    except
      on E: EConvertError do
      begin
        Result := False;
        simpleMyLog(mlfError, UpdManifest.sWrapperPid + ' is not a valid integer');
      end;
    end;
  end
  else
  begin
    simpleMyLog(mlfError, 'wrapper.pid is empty');
    Result := False;
  end;

  // Java.pid
  if UpdManifest.sJavaPid <> '' then
  begin
    simpleMyLog(mlfInfo, 'java.pid is ' + UpdManifest.sJavaPid);
    try
      UpdManifest.iJavaPid := StrToInt(UpdManifest.sJavaPid);
    except
      on E: EConvertError do
      begin
        Result := False;
        simpleMyLog(mlfError, UpdManifest.sJavaPid + ' is not a valid integer');
      end;

    end;
  end
  else
  begin
    simpleMyLog(mlfError, 'java.pid is empty');
    Result := False;
  end;

  // Check Files
  for i := 0 to 2 do
  begin
    if UpdManifest.FilePath[i][0] <> '' then
    begin
      if FileExistsUTF8(UpdManifest.FilePath[i][0]) then
        simpleMyLog(mlfInfo, 'File' + IntToStr(i + 1) + '.from is ' + UpdManifest.FilePath[i][0])
      else
      begin
        simpleMyLog(mlfError, UpdManifest.FilePath[i][0] + ' not found');
        Result := False;
      end;
    end
    else
    begin
      simpleMyLog(mlfError, 'File' + IntToStr(i + 1) + '.from is empty');
    end;

    if UpdManifest.FilePath[i][1] = '' then
    begin
      simpleMyLog(mlfError, 'File' + IntToStr(i + 1) + '.to is empty');
      Result := False;
    end;
  end;

end;

function UBackupFiles(): boolean;
var
  BackupTimeStamp: string;
  i: integer;
begin

  Result := True;
  simpleMyLog(mlfInfo, '-- Backup files --');

  BackupTimeStamp := FormatDateTime('yyymmdd_hhnnss', Now);
  BackupDir := Application.Location + 'data_' + BackupTimeStamp;

  if CreateDirUTF8(BackupDir) then
  begin

    for i := 0 to 2 do
    begin
      UpdManifest.FilePath[i][2] := BackupDir + '\' + ExtractFileName(UpdManifest.FilePath[i][1]); // Set ToBackup path
      if RenameFileUTF8(UpdManifest.FilePath[i][1], UpdManifest.FilePath[i][2]) then
        simpleMyLog(mlfInfo, 'Backup: ' + UpdManifest.FilePath[i][1] + ' save to ' + UpdManifest.FilePath[i][2])
      else
      begin
        simpleMyLog(mlfError, 'Backup: Failed to create the backup of ' + UpdManifest.FilePath[i][1]);
        Result := False;
      end;

    end;

  end
  else
  begin
    simpleMyLog(mlfError, 'Backup: ' + 'Failed to create the BackupDir: ' + BackupDir);
    Result := False;
  end;
end;

function UCopyFiles(): boolean;
var
  i: integer;
begin
  Result := True;
  simpleMyLog(mlfInfo, '-- Copy files --');

  for i := 0 to 2 do
  begin
    if FileUtil.CopyFile(UpdManifest.FilePath[i][0], UpdManifest.FilePath[i][1]) then
      simpleMyLog(mlfInfo, 'Copy: ' + UpdManifest.FilePath[i][0] + ' copied to ' + UpdManifest.FilePath[i][1])
    else
    begin
      simpleMyLog(mlfError, 'Copy: ' + UpdManifest.FilePath[i][0] + ' not copied to ' + UpdManifest.FilePath[i][1]);
      Result := False;
    end;
  end;

end;

function URestoreFiles(): boolean;
var
  i: integer;
begin

  Result := True;
  simpleMyLog(mlfInfo, '-- Restore files --');

  for i := 0 to 2 do
  begin
    if FileUtil.CopyFile(UpdManifest.FilePath[i][2], UpdManifest.FilePath[i][1]) then
      simpleMyLog(mlfInfo, 'Restore: ' + UpdManifest.FilePath[i][2] + ' restored as ' + UpdManifest.FilePath[i][1])
    else
    begin
      simpleMyLog(mlfError, 'Restore: ' + UpdManifest.FilePath[i][2] + ' not restored');
    end;
  end;

end;

function FindProcessByID(const pPID: integer): boolean;
{
 source: http://wiki.lazarus.freepascal.org/Windows_Programming_Tips#Showing.2Ffinding_processes
}
var
  CPID: DWORD;
  S: HANDLE;
  PE: TProcessEntry32;
begin
  Result := False;
  S := CreateToolHelp32Snapshot(TH32CS_SNAPALL, 0); // Create snapshot
  PE.DWSize := SizeOf(PE); // Set size before use

  if Process32First(S, PE) then
    Result := False;
  repeat
    CPID := PE.th32ProcessID;

    if CPID = pPID then
    begin
      Result := True;
      Break;
    end;
  until not Process32Next(S, PE);
  CloseHandle(S);
end;

function simpleMyLog(LogFlag: TMyLogFlag; LogLine: string; AppendNewLine: boolean = True): boolean;
var
  logText: TextFile;
  flag: string;
begin
  Result := True;

  if LogFlag = mlfError then
    flag := 'ERROR  ';
  if LogFlag = mlfInfo then
    flag := 'INFO   ';
  LogLine := flag + '| ' + FormatDateTime('yyyy/mm/dd hh:nn:ss', Now) + ' | ' + LogLine;
  AssignFile(logText, LogFilePath);
  try
    if FileExistsUTF8(LogFilePath) then
      Append(logText)
    else
      Rewrite(logText);

    if AppendNewLine then
      WriteLn(logText, LogLine)
    else
      Write(logText, LogLine);

    CloseFile(logText);
  except
    on E: EInOutError do
    begin
      Result := False;
    end;
  end;
end;

end.
