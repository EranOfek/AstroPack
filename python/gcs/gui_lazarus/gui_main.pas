// Lazarus 2.2.0
// Windows installer: lazarus-2.2.0-fpc-3.2.2-win64.exe
//
// Install Lazarus on Ubutnu
// https://ubuntuhandbook.org/index.php/2021/11/install-lazarus-ide-ubuntu/

unit gui_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, Menus, Process,
  gcs_about;


type

  { TMainForm }

  TMainForm = class(TForm)
    BtnCloseAll1: TBitBtn;
    BtnLoadFiles: TBitBtn;
    BtnRecordLeft1: TBitBtn;
    BtnRunMain1: TBitBtn;
    BtnSendXml: TBitBtn;
    BtnSendYml: TBitBtn;
    ComboBoxXmlFileName: TComboBox;
    ComboBoxYmlFileName: TComboBox;
    Image1: TImage;
    Label4: TLabel;
    Label6: TLabel;
    MainMenu: TMainMenu;
    MemoLog: TMemo;
    MemoXml: TMemo;
    MemoYml: TMemo;
    MIAbout: TMenuItem;
    MIHelp: TMenuItem;
    MINewWindow: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelLog: TPanel;
    PanelLog1: TPanel;
    PanelLog2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TabSheet1: TTabSheet;
    TabSheetGcsInterface: TTabSheet;
    TabSheetSimulator: TTabSheet;
    TimerPollGuiMsg: TTimer;
    procedure BtnCloseAll1Click(Sender: TObject);
    procedure BtnLoadFilesClick(Sender: TObject);
    procedure BtnRunMain1Click(Sender: TObject);
    procedure BtnSendXmlClick(Sender: TObject);
    procedure BtnSendYmlClick(Sender: TObject);
    procedure ComboBoxXmlFileNameChange(Sender: TObject);
    procedure ComboBoxYmlFileNameChange(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MINewWindowClick(Sender: TObject);
    procedure TimerPollGuiMsgTimer(Sender: TObject);
  private

  public

    procedure SendGuiMsg(AText: String);
    procedure ProcessGuiFile(FileName: String);
  end;

var
  MainForm: TMainForm;

const
  ScriptPath = '/home/user/dev/opsci.git/src/scripts/';


implementation

{$R *.lfm}

{ TMainForm }

procedure RunScript(Script: AnsiString);
var
  AProcess: TProcess;
  Cmd: ansistring;
begin
  Script := ScriptPath + '/' + Script;
  Cmd := Script + ';sleep 2';
  AProcess := TProcess.Create(nil);
  AProcess.Executable := '/usr/bin/xterm';  //Cmd;  //'/bin/bash';
  AProcess.Parameters.Add('-e');
  AProcess.Parameters.Add(Cmd);
  //AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  //AProcess.Free;

  //if RunCommand('/bin/bash',['-c',Script], s, [{poUsePipes}{, poWaitOnExit}]) then
  //   writeln(s);


end;




procedure LoadFilesList(Path: String;  Mask: String;  List: TStrings);
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



procedure LoadYmlConfig(YmlLines: TStrings;  Config: TStrings);
var
  i, p: Integer;
  FirstList: Integer;
  Line: String;
  Section: String;
begin
  {$IFDEF Linux}
  Section := 'GuiLinux:';
  {$ELSE}
  Section := 'GuiWin:';
  {$ENDIF}

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
      p := Pos(Line, ':');
      if p > 0 then
      begin
        Line[p] := '=';
        Config.Add(Trim(Line));
      end;
    end;
  end;

end;




{ TMainForm }


const
  LineBreak: String = Char(#10);


function NewFileName: String;
var
  Path: String;
  FileName: String;
begin
  Path := 'c:\gcs\gui\';
  FileName := Path + FormatDateTime('YYYY_MM_DD__HH_MM_SS_zzz', Now);
  Result := FileName;
end;

procedure TMainForm.SendGuiMsg(AText: String);
var
   FileName: String;
   Lines: TStringList;
begin
  FileName := NewFileName();

  MemoXml.Lines.SaveToFile(FileName + '.xml');
  MemoYml.Lines.SaveToFile(FileName + '.yml');

  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    Lines.SaveToFile(FileName + '.msg.yml');
  finally
    Lines.Free;
  end;
end;


procedure TMainForm.BtnLoadFilesClick(Sender: TObject);
var
  Path: String;
begin
  Path := 'D:\Ultrasat\AstroPack.git\python\gcs\gcs_msg_xml';
  LoadFilesList(Path, '*.xml', ComboBoxXmlFileName.Items);
  LoadFilesList(Path, '*.yml', ComboBoxYmlFileName.Items);
end;

procedure TMainForm.BtnRunMain1Click(Sender: TObject);
begin
  //
end;

procedure TMainForm.BtnSendXmlClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.BtnSendYmlClick(Sender: TObject);
var
  Yml: String;
begin
    Yml :=
     'Msg:'                                + LineBreak +
     '  Cmd: SendFile'                     + LineBreak +
     LineBreak;
  SendGuiMsg(Yml);
end;

procedure TMainForm.BtnCloseAll1Click(Sender: TObject);
begin
  //
end;

procedure TMainForm.ComboBoxXmlFileNameChange(Sender: TObject);
var
  FileName: String;
begin
  FileName := ComboBoxXmlFileName.Items[ComboBoxXmlFileName.ItemIndex];
  MemoXml.Lines.LoadFromFile(FileName);
end;

procedure TMainForm.ComboBoxYmlFileNameChange(Sender: TObject);
var
  FileName: String;
begin
  FileName := ComboBoxXmlFileName.Items[ComboBoxYmlFileName.ItemIndex];
  MemoYml.Lines.LoadFromFile(FileName);
end;

procedure TMainForm.MIAboutClick(Sender: TObject);
begin
  //
  AboutForm.Show();
end;

procedure TMainForm.MIExitClick(Sender: TObject);
begin
  //
  Application.Terminate();
end;

procedure TMainForm.MINewWindowClick(Sender: TObject);
var
  Form: TMainForm;
begin
  Form := TMainForm.Create(Application);
  Form.Show();
end;

//function GetKey(Lines: TStrings:


function ExtractBetween(const Value, A, B: string): string;
var
  aPos, bPos: Integer;
begin
  result := '';
  aPos := Pos(A, Value);
  if aPos > 0 then begin
    aPos := aPos + Length(A);
    bPos := Pos(B, Value);  //@Todo @Chen , aPos);
    if bPos > 0 then begin
      Result := Trim(Copy(Value, aPos, bPos - aPos));
    end;
  end;
end;




procedure TMainForm.ProcessGuiFile(FileName: String);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);


  finally
    Lines.Free;
  end;
end;



procedure TMainForm.TimerPollGuiMsgTimer(Sender: TObject);
var
  Path: String;
  FileName: String;
  FileList: TStringList;
begin
  TimerPollGuiMsg.Enabled := false;

  Path := 'c:\gcs\gui_in';
  FileList := TStringList.Create;
  try
    LoadFilesList(Path, '*.yml', FileList);
    FileList.Sort;
    if FileList.Count > 0 then
    begin
        FileName := FileList.Strings[0];
        ProcessGuiFile(FileName);
    end;

  finally
    FileList.Free;
  end;

  TimerPollGuiMsg.Enabled := true;
end;

end.

