unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, JwaTlHelp32,
  {$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LConvEncoding, dateutils, RegExpr;

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
    FilePath: array of array[0..2] of string; // 0:From ; 1:To ; 2:ToBackup
    FileBackupStatus: array of boolean;
  end;

procedure UMainProcess;

function UReadManifest(ManifestFilePath: string): boolean;
function UCheckManifestValues(): boolean;
function UBackupFiles(): boolean;
function UCopyFiles(): boolean;
function URestoreFiles(): boolean;
function FindProcessByID(const pPID: integer): boolean;
function simpleMyLog(LogFlag: TMyLogFlag; LogLine: string; AppendNewLine: boolean = True): boolean;

const
  MaxWaitingTime: integer = 300;

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
  FreenetUpdaterForm.WindowState := wsMinimized;
  FreenetUpdaterForm.ShowInTaskBar := stNever;

  LogFilePath := 'freenetupdater.log';

  if UReadManifest('freenetupdater.ini') then
    if UCheckManifestValues() then
      Timer_MonitorPID.Enabled := True
    else
    begin
      simpleMyLog(mlfInfo, 'Something went wrong during Manifest checking');
      simpleMyLog(mlfInfo, 'Update process cancelled');
      Application.Terminate;
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

    Timer_MonitorPID.Enabled := False;

    UMainProcess;        // We call the main Update process

  end
  else
  begin
  (*
   if FindProcessByID(UpdManifest.iWrapperPid) then
      simpleMyLog(mlfInfo, UpdManifest.sWrapperPid + ' still running');

    if FindProcessByID(UpdManifest.iJavaPid) then
      simpleMyLog(mlfInfo, UpdManifest.sJavaPid + ' still running');
  *)

    if SecondsBetween(tMonitorPidStarted, Now) >= MaxWaitingTime then
    begin
      simpleMyLog(mlfInfo, 'Take too long, update cancelled');
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
      simpleMyLog(mlfInfo, 'Successful update');

      { TODO :
       Launch freenet
       Delete the manifest file
       and the backup dir ?}

      Application.Terminate;
    end
    else
    begin
      simpleMyLog(mlfInfo, 'Something went wrong during copy');
      simpleMyLog(mlfInfo, 'Update process cancelled');
      URestoreFiles();
      { TODO : check result of URestoreFiles }

      Application.Terminate;
    end;
  end
  else
  begin
    simpleMyLog(mlfInfo, 'Something went wrong during backup');
    simpleMyLog(mlfInfo, 'Update process cancelled');
    URestoreFiles();

    Application.Terminate;
  end;
end;

function UReadManifest(ManifestFilePath: string): boolean;
var
  slFileData: TStringList;
  rgxFileID: TRegExpr;
  i: integer;
  jFileID: integer = 0;

begin
  Result := True;

  if not FileExistsUTF8(ManifestFilePath) then
  begin
    simpleMyLog(mlfError, 'Manifest file not found');
    Result := False;
    Application.Terminate;
  end
  else
  begin
    slFileData := TStringList.Create;
    rgxFileID := TRegExpr.Create;

    slFileData.LoadFromFile(ManifestFilePath);

    // Read entries
    UpdManifest.sWrapperPid := slFileData.Values['wrapper.pid'];
    UpdManifest.sJavaPid := slFileData.Values['java.pid'];

    for i := 0 to slFileData.Count - 1 do
    begin
      rgxFileID.Expression := 'file(\d+).from';
      if rgxFileID.Exec(slFileData.Names[i]) then
      begin
        try
          jFileID := StrToInt(rgxFileID.Match[1]);
          if jFileID <> 0 then
          begin
            if jFileID >= Length(UpdManifest.FilePath) then
            begin
              SetLength(UpdManifest.FilePath, jFileID);
              SetLength(UpdManifest.FileBackupStatus, jFileID);
            end;

            UpdManifest.FilePath[jFileID - 1][0] := slFileData.Values['file' + IntToStr(jFileID) + '.from'];
            UpdManifest.FilePath[jFileID - 1][1] := slFileData.Values['file' + IntToStr(jFileID) + '.to'];
            UpdManifest.FileBackupStatus[jFileID - 1] := False;

          end;

        except
          // WriteLn('jFileID is not a valid integer');
        end;

      end;
    end;
    slFileData.Free;
    rgxFileID.Free;
    Result := True;

  end;
end;

function UCheckManifestValues(): boolean;
var
  i: integer;
begin
  Result := True;

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
        simpleMyLog(mlfError, UpdManifest.sWrapperPid + ' is not a valid integer');
        Result := False;
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
        simpleMyLog(mlfError, UpdManifest.sJavaPid + ' is not a valid integer');
        Result := False;
      end;
    end;

  end
  else
  begin
    simpleMyLog(mlfError, 'java.pid is empty');
    Result := False;
  end;

  // Check Files
  for i := 0 to High(UpdManifest.FilePath) do
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

    for i := 0 to High(UpdManifest.FilePath) do
    begin
      UpdManifest.FilePath[i][2] := BackupDir + '\' + ExtractFileName(UpdManifest.FilePath[i][1]); // Set ToBackup path
      if RenameFileUTF8(UpdManifest.FilePath[i][1], UpdManifest.FilePath[i][2]) then
      begin
        simpleMyLog(mlfInfo, 'Backup: ' + UpdManifest.FilePath[i][1] + ' saved to ' + UpdManifest.FilePath[i][2]);
        UpdManifest.FileBackupStatus[i] := True;
      end
      else
      begin
        simpleMyLog(mlfError, 'Backup: Failed to create the backup of ' + UpdManifest.FilePath[i][1]);
        UpdManifest.FileBackupStatus[i] := False;

        Result := False;
        Exit;
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

  for i := 0 to High(UpdManifest.FilePath) do
  begin
    if FileUtil.CopyFile(UpdManifest.FilePath[i][0], UpdManifest.FilePath[i][1]) then
      simpleMyLog(mlfInfo, 'Copy: ' + UpdManifest.FilePath[i][0] + ' copied to ' + UpdManifest.FilePath[i][1])
    else
    begin
      simpleMyLog(mlfError, 'Copy: ' + UpdManifest.FilePath[i][0] + ' not copied to ' + UpdManifest.FilePath[i][1]);
      Result := False;
      Exit;
    end;
  end;

end;

function URestoreFiles(): boolean;
var
  i: integer;
begin

  Result := True;
  simpleMyLog(mlfInfo, '-- Restore files --');

  for i := 0 to High(UpdManifest.FilePath) do
  begin
    if UpdManifest.FileBackupStatus[i] then
    begin
      if FileUtil.CopyFile(UpdManifest.FilePath[i][2], UpdManifest.FilePath[i][1]) then
        simpleMyLog(mlfInfo, 'Restore: ' + UpdManifest.FilePath[i][2] + ' restored as ' + UpdManifest.FilePath[i][1])
      else
      begin
        simpleMyLog(mlfError, 'Restore: ' + UpdManifest.FilePath[i][2] + ' not restored');
      end;
    end;
  end;

end;

{$IFDEF UNIX}
function FindProcessByID(const pPID: integer): boolean;
begin
  Result := False;
end;

{$ENDIF}

{$IFDEF MSWINDOWS}
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

{$ENDIF}

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
