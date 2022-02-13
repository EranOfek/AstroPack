//----------------------------------------------------------------------------
// Project:  MATLAB & Database Utils
// Module:
// File:     util_datamod.pas
// Title:    Data Module
// Author:   Chen Tishler, 01/2022
//
//----------------------------------------------------------------------------

unit util_datamod;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, SysUtils, FileUtil, Process, IniFiles, PQConnection, SQLite3Conn, SQLDB,
  LogPanel;


type

  { TAppDataModule }

  TAppDataModule = class(TDataModule)
    PQConnection: TPQConnection;
    SQLite3Connection: TSQLite3Connection;
    SQLQueryMsgs: TSQLQuery;
    SQLQueryTasks: TSQLQuery;
    SQLTransactionSQLite: TSQLTransaction;
    SQLTransactionPG: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private

  public

    // Initialize the Data Module
    procedure Init();

    // Run script @Todo
    procedure RunScript(Path: AnsiString;  Script: AnsiString);

    // Load files list from folder with mask
    procedure LoadFilesList(Path: String;  Mask: String;  List: TStrings);

    // Load specified sectoin from YAML file
    procedure LoadYmlSection(YmlLines: TStrings;  Section: String;  Config: TStrings);

    // Load specified section from YAML file to
    procedure LoadYmlConfig(FileName: String;  Section: String;  Config: TStrings);

    // Load YAML file as INI file
    procedure LoadYmlToIni(FileName: String;  IniFile: TMemIniFile;  TrimValues: Boolean = true);

    // Generate new file name, used to send messages in files between processes
    function GetNewFileName(Path: String;  Prefix: String;  Ext: String) : String;

    // Send GUI message as YAML file
    procedure SendGuiMsg(AText: String;  Xml: String;  Prefix: String);

    // Poll folder for incoming message, return file name of next message to process
    function PollGuiMsg(Prefix: String) : String;

    // Log to file
    procedure Log(AText: String);

    // Run Python script
    function RunPy(Params: TStrings;  ALogPanel: TLogPanel;  var AProcess: TProcess) : Boolean;

    // Log output of Python script to specified LogPanel
    procedure LogProcessOutput(AProcess: TProcess;  ALogPanel: TLogPanel;  AColor: TColor = clBlack);


  public
    Initialized         : Boolean;       //
    LogPath             : String;        //
    LogFileName         : String;        //

    ConfigIni           : TMemIniFile;   //
    ConfigFileName      : String;        //

    IfcXmlFilePath      : String;        //
    SimXmlFilePath      : String;        //

    MsgGcsToSocPath     : String;        //
    MsgSocToGcsPath     : String;        //
    GuiToPyPath         : String;        //
    PyToGuiPath         : String;        //
    GuiInPath           : String;        //
    ScriptPath          : String;        //
    NewFileNameCounter  : Integer;       //

    UsePostgres         : Boolean;       //
    UseSQLite           : Boolean;       //
  end;

var
  AppDataModule: TAppDataModule;

implementation

{$R *.lfm}

procedure TAppDataModule.DataModuleCreate(Sender: TObject);
begin
  // ..
  Initialized := false;
end;


procedure TAppDataModule.DataModuleDestroy(Sender: TObject);
begin
  // ..
end;


procedure TAppDataModule.Init();
var
  DatabaseName, Host : String;
begin
  if Initialized then
     exit;

  Initialized := true;
  exit;

  UsePostgres := false;
  UseSQLite := true;

  {$IFDEF Linux}
  ConfigFileName := '../gcs_conf.yml';
  {$ELSE}
  ConfigFileName := '..\gcs_conf_win.yml';
  {$ENDIF}

  GuiInPath := 'InMsgFolder';

  ScriptPath := '/home/user/dev/opsci.git/src/scripts/';

  //
  ConfigIni := TMemIniFile.Create('');
  LoadYmlToIni(ConfigFileName, ConfigIni);
  //LoadYmlConfig(ConfigFileName, IniSection, Config);
  //Log(Config.Values['InMsgFolder']);

  LogPath := ConfigIni.ReadString('Log', 'LogPath', '');
  if LogPath = '' then
     LogPath := '..';

  LogFileName := LogPath + DirectorySeparator + 'gui.log';
  ForceDirectories(LogPath);

  //
  IfcXmlFilePath := '..' + DirectorySeparator + 'gui_ifc_xml';
  SimXmlFilePath := '..' + DirectorySeparator + 'gui_sim_xml';

  //D:\Ultrasat\AstroPack.git\python\gcs\gcs_msg_xml';

  MsgGcsToSocPath := ConfigIni.ReadString('Interface', 'MsgGcsToSocPath', '');
  MsgSocToGcsPath := ConfigIni.ReadString('Interface', 'MsgSocToGcsPath', '');


  PyToGuiPath   := ConfigIni.ReadString('Gui', 'PyToGuiPath', '');
  GuiToPyPath   := ConfigIni.ReadString('Gui', 'GuiToPyPath', '');

  ForceDirectories(PyToGuiPath);
  ForceDirectories(GuiToPyPath);

  NewFileNameCounter := 0;

  // Setup database
  if UseSQLite then
  begin
    DatabaseName := ConfigIni.ReadString('Database', 'SQLiteDatabase', '');
    SQLite3Connection.Connected := false;
    SQLite3Connection.DatabaseName := DatabaseName;
    //SQLite3Connection.Connected := true;

    //SQLQueryMsgs.Active := true;
  end
  else
  begin
    Host := ConfigIni.ReadString('Database', 'PostgresHost', '');
    PQConnection.Connected := false;
    PQConnection.HostName := Host;
    PQConnection.Connected := true;
  end;

  //
  //LoadFiles();

end;


procedure TAppDataModule.RunScript(Path: AnsiString;  Script: AnsiString);
var
  AProcess: TProcess;
  Cmd: AnsiString;
begin
  {$IFDEF Linux}

  //----------------------------------------------------------  Linux
  //Script := Path + '/' + Script;
  //Cmd := Script + ';sleep 2';
  AProcess := TProcess.Create(nil);
  AProcess.Executable := '/usr/bin/xterm';  //Cmd;  //'/bin/bash';
  AProcess.Parameters.Add('-e');
  AProcess.Parameters.Add(Cmd);
  //AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  //AProcess.Free;

  //if RunCommand('/bin/bash',['-c',Script], s, [{poUsePipes}{, poWaitOnExit}]) then
  //   writeln(s);
  {$ELSE}

  //----------------------------------------------------------  Windows

  //Script := Path + '/' + Script;
  //Cmd := Script + ';sleep 2';
  AProcess := TProcess.Create(nil);
  AProcess.Executable := '/usr/bin/xterm';  //Cmd;  //'/bin/bash';
  AProcess.Parameters.Add('-e');
  AProcess.Parameters.Add(Cmd);
  //AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  //AProcess.Free;

  //if RunCommand('/bin/bash',['-c',Script], s, [{poUsePipes}{, poWaitOnExit}]) then
  //   writeln(s);



  {$ENDIF}
end;


procedure TAppDataModule.LoadFilesList(Path: String;  Mask: String;  List: TStrings);
var
  ListOfFiles: TStringList;
begin
  ListOfFiles := TStringList.Create;
  try
    FileUtil.FindAllFiles(ListOfFiles, Path, Mask, False);
    List.Assign(ListOfFiles);
  finally
    ListOfFiles.Free;
  end;
end;


// Unused
procedure TAppDataModule.LoadYmlSection(YmlLines: TStrings;  Section: String;  Config: TStrings);
var
  i, p: Integer;
  FirstList: Integer;
  Line: String;
begin

  // Search Section
  FirstList := -1;
  for i:= 0 to YmlLines.Count-1 do
  begin
    Line := YmlLines[i];
    p := Pos(Line, Section);
    if p = 1 then
    begin
      FirstList := i;
      break;
    end
  end;

  if FirstList > -1 then
  begin

    // Replace first ':' with '='
    for i:= FirstList+1 to YmlLines.Count-1 do
    begin
      Line := YmlLines[i];

      // Break on end of section
      p := Pos('EndOfSection', Line);
      if p > 0 then
      begin
        break;
      end;

      p := Pos(':', Line);
      if p > 0 then
      begin
        Line := Trim(Copy(Line, 1, p-1)) + '=' + Trim(Copy(Line, p+1, Length(Line)));
        Config.Add(Line);
      end;
    end;
  end;

end;


// Unused
procedure TAppDataModule.LoadYmlConfig(FileName: String;  Section: String;  Config: TStrings);
var
  Lines: TStringList;
begin
  //MainForm.Log('LoadYmlConfig: ' + FileName);
  try
    Lines:= TStringList.Create;
    Lines.LoadFromFile(FileName);
    LoadYmlSection(Lines, Section, Config);
  finally
    Lines.Free;
  end;
  //MainForm.MemoLog.Lines.AddStrings(Config);
end;


procedure TAppDataModule.LoadYmlToIni(FileName: String;  IniFile: TMemIniFile;  TrimValues: Boolean);
var
  i, p: Integer;
  Line: String;
  Section, KeyValue: String;
  YmlLines: TStringList;
  IniLines: TStringList;
begin

  YmlLines := TStringList.Create;
  IniLines := TStringList.Create;

  try
    YmlLines.LoadFromFile(FileName);

    // Search Section
    for i:= 0 to YmlLines.Count-1 do
    begin
      Line := YmlLines[i];
      if (Trim(Line) = '') or (Pos('#', Trim(Line)) = 1) then
         continue;

      p := Pos(':', Line);
      if p > 0 then
      begin
        if (Line[1] <> ' ') and (Line[1] <> '\t') then
        begin
          Section := Trim(StringReplace(Line, ':', '', []));
          IniLines.Add('[' + Section + ']');
        end
        else
        begin
          if TrimValues then
            KeyValue :=  Trim(Copy(Line, 1, p-1)) + '=' + Trim(Copy(Line, p+1, Length(Line)))
          else
            KeyValue :=  Trim(Copy(Line, 1, p-1)) + '=' + Copy(Line, p+1, Length(Line));

          IniLines.Add(KeyValue);
        end;
      end;
    end;
  finally

  end;

  IniFile.SetStrings(IniLines);
  YmlLines.Free;
  IniLines.Free;
end;


function TAppDataModule.GetNewFileName(Path: String;  Prefix: String;  Ext: String) : String;
var
  Serial, FileName: String;
begin
  Inc(NewFileNameCounter);
  Serial := Format('_%.4d', [NewFileNameCounter]);
  FileName := IncludeTrailingPathDelimiter(Path) + Prefix + FormatDateTime('YYYY_MM_DD__HH_MM_SS_zzz', Now) + Serial + Ext;
  Result := FileName;
end;


procedure TAppDataModule.SendGuiMsg(AText: String;  Xml: String;  Prefix: String);
var
   FileName: String;
   Lines: TStringList;
begin
  Lines := TStringList.Create;

  // Generate file name for new message
  FileName := GetNewFileName(AppDataModule.GuiToPyPath, Prefix, '');

  // Save XML text to file
  if Xml <> '' then
  begin
    Lines.Text := Xml;
    Lines.SaveToFile(FileName + '.xml');
  end;

  Lines.Text := AText;
  Lines.SaveToFile(FileName + '.yml.tmp');
  RenameFile(FileName + '.yml.tmp', FileName + '.yml');

  Lines.Free;
end;


function TAppDataModule.PollGuiMsg(Prefix: String) : String;
var
  Path: String;
  FileName: String;
  FileList: TStringList;
begin
  Result := '';
  Path := PyToGuiPath;
  FileList := TStringList.Create;
  try
    LoadFilesList(Path, Prefix + '*.yml', FileList);
    FileList.Sort;
    if FileList.Count > 0 then
    begin
        FileName := FileList.Strings[0];
        Result := FileName;
        //ProcessGuiFile(FileName);
        //DeleteFile(FileName);
    end;

  finally
    FileList.Free;
  end;

end;


procedure TAppDataModule.Log(AText: String);
var
  F: Text;
begin
  if LogFileName <> '' then
  begin
    AssignFile(F, LogFileName);
    if FileExists(LogFileName) then
      Append(F)
    else
      Rewrite(F);

    Writeln(F, AText);
    Close(F);
  end;
end;


function TAppDataModule.RunPy(Params: TStrings;  ALogPanel: TLogPanel;  var AProcess: TProcess) : Boolean;
var
  S: String;
  OutputString: String;
  ReadCount, i: Integer;
  CharBuffer: array [0..511] of char;

begin
  AProcess:= TProcess.Create(self);
  AProcess.Executable := 'python3.exe';
  AProcess.Parameters.Assign(Params);
  AProcess.Options := [poUsePipes, poStderrToOutPut];

  S := AProcess.Executable;
  for i:= 1 to AProcess.Parameters.Count do
    S:= S + ' ' + AProcess.Parameters[i-1];

  if ALogPanel <> nil then
    ALogPanel.Add('RunPy: ' + S);

  AProcess.Execute();
end;


procedure TAppDataModule.LogProcessOutput(AProcess: TProcess;  ALogPanel: TLogPanel;  AColor: TColor);
var
  OutputString: String;
  ReadCount: Integer;
  CharBuffer: array [0..8191] of char;
begin
  while AProcess.Output.NumBytesAvailable > 0 do
  begin
    ReadCount := AProcess.Output.NumBytesAvailable;
    if ReadCount > Length(CharBuffer) then
      ReadCount:= Length(CharBuffer);

    AProcess.Output.Read(CharBuffer, ReadCount);
    OutputString += Copy(CharBuffer, 0, ReadCount);
    ALogPanel.AddLn(OutputString, AColor);
    OutputString := '';
  end;
end;


end.

