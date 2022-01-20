unit gcs_datamod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Process, IniFiles, PQConnection, SQLite3Conn, SQLDB;


type

  { TAppDataModule }

  TAppDataModule = class(TDataModule)
    PQConnection: TPQConnection;
    SQLite3Connection: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private

  public

    //
    procedure Init();

    //
    procedure RunScript(Path: AnsiString;  Script: AnsiString);

    //
    procedure LoadFilesList(Path: String;  Mask: String;  List: TStrings);

    //
    procedure LoadYmlSection(YmlLines: TStrings;  Section: String;  Config: TStrings);

    //
    procedure LoadYmlConfig(FileName: String;  Section: String;  Config: TStrings);

    //
    procedure LoadYmlToIni(FileName: String;  IniFile: TMemIniFile;  TrimValues: Boolean = true);

    //
    function GetNewFileName(Path: String;  Ext: String) : String;

    //procedure SendGuiMsg(AText: String);

  public

    ConfigIni           : TMemIniFile;
    ConfigFileName      : String;

    XmlFilePath         : String;

    MsgFromGcsPath      : String;
    MsgToGcsPath        : String;
    FromGuiPath         : String;
    ToGuiPath           : String;

    GuiInPath           : String;
    ScriptPath          : String;

  end;

var
  AppDataModule: TAppDataModule;

implementation

{$R *.lfm}


procedure TAppDataModule.DataModuleCreate(Sender: TObject);
begin
  // ..
end;

procedure TAppDataModule.DataModuleDestroy(Sender: TObject);
begin
  // ..
end;


procedure TAppDataModule.Init();
begin

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

  //
  XmlFilePath := '..' + DirectorySeparator + 'gcs_msg_xml';
  //D:\Ultrasat\AstroPack.git\python\gcs\gcs_msg_xml';

  MsgFromGcsPath := ConfigIni.ReadString('Interface', 'MsgFromGcsPath', '');
  MsgToGcsPath := ConfigIni.ReadString('Interface', 'MsgToGcsPath', '');


  ToGuiPath := ConfigIni.ReadString('Gui', 'ToGuiPath', '');
  FromGuiPath := ConfigIni.ReadString('Gui', 'FromGuiPath', '');

  ForceDirectories(ToGuiPath);
  ForceDirectories(FromGuiPath);

  //
  //LoadFiles();

end;

procedure TAppDataModule.RunScript(Path: AnsiString;  Script: AnsiString);
var
  AProcess: TProcess;
  Cmd: AnsiString;
begin
  {$IFDEF Linux}
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


function TAppDataModule.GetNewFileName(Path: String;  Ext: String) : String;
var
  FileName: String;
begin
  FileName := IncludeTrailingPathDelimiter(Path) + FormatDateTime('YYYY_MM_DD__HH_MM_SS_zzz', Now) + Ext;
  Result := FileName;
end;




end.

