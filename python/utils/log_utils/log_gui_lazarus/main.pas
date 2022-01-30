// Lazarus 2.2.0
// Windows installer: lazarus-2.2.0-fpc-3.2.2-win64.exe
//
// Install Lazarus on Ubutnu
// https://ubuntuhandbook.org/index.php/2021/11/install-lazarus-ide-ubuntu/

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls, Menus, DBGrids,
  Process, IniFiles, DB,
  LogPanel,
  util_datamod, about;


type

  { TMainForm }

  TMainForm = class(TForm)
    BtnLoadFiles: TBitBtn;
    BtnRunScript: TBitBtn;
    BtnSendKeepAlive: TBitBtn;
    BtnSendXml: TBitBtn;
    CheckBoxAutoProcessIncomingMsgs: TCheckBox;
    CheckBoxAutoSendRandomMsg: TCheckBox;
    CheckBoxAutoSendRandomMsg1: TCheckBox;
    ComboBoxXmlFileName: TComboBox;
    ComboBoxYmlFileName: TComboBox;
    Image1: TImage;
    LabelTitle: TLabel;
    Label8: TLabel;
    ListBox1: TListBox;
    MainMenu: TMainMenu;
    MemoLog: TMemo;
    MemoXml: TMemo;
    MemoYml: TMemo;
    MISimulatorWindow: TMenuItem;
    MIAbout: TMenuItem;
    MIHelp: TMenuItem;
    MIInterfaceWindow: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    PageControlMain: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel13: TPanel;
    Panel15: TPanel;
    Panel17: TPanel;
    Panel19: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelLog: TPanel;
    PanelXml: TPanel;
    Splitter1: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    SynEditXml: TSynEdit;
    SynXMLSyn: TSynXMLSyn;
    TabSheetMain: TTabSheet;
    TabSheetYaml: TTabSheet;
    TabSheetTemp: TTabSheet;
    TimerUpdateStatus: TTimer;
    TimerAutoSend: TTimer;
    TimerPollGuiMsg: TTimer;
    procedure BtnLoadFilesClick(Sender: TObject);
    procedure BtnSendKeepAliveClick(Sender: TObject);
    procedure BtnRunScriptClick(Sender: TObject);
    procedure BtnSendXmlClick(Sender: TObject);
    procedure ComboBoxXmlFileNameChange(Sender: TObject);
    procedure ComboBoxYmlFileNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure TimerAutoSendTimer(Sender: TObject);
    procedure TimerPollGuiMsgTimer(Sender: TObject);
    procedure TimerUpdateStatusTimer(Sender: TObject);
  private

  //=========================================================================

  //=========================================================================
  public

    //
    procedure Init();

    // Set SOC / GCS
    procedure SetWindowType(WinType: String);

    //
    procedure ProcessGuiFile(FileName: String);


    // Load XML and YML file lists
    procedure LoadFiles();

    //
    procedure PollGuiMsg();

    //
    procedure SendGuiMsg(AText: String;  ATitle: String = '');

    //
    procedure Log(Line: String;  AColor: TColor = clBlack;  BColor: TColor = clWhite);

  //=========================================================================
  public
    LogPanel: TLogPanel;
    IsSim: Boolean;
  end;

//===========================================================================
//
//===========================================================================
var
  MainForm: TMainForm;


const

  LineBreak: String = Char(#10);

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Init();
begin
  //
  IsSim := false;
  AppDataModule.Init();

  //
  LoadFiles();
end;


procedure TMainForm.SetWindowType(WinType: String);
begin
  if WinType = 'SOC' then
  begin
    IsSim := false;
    Caption := 'ULTRASAT SOC';
    LabelTitle.Caption := 'ULTRASAT SOC';
  end
  else
  begin
    IsSim := true;
    Caption := 'GCS SIMULATOR';
    LabelTitle.Caption := 'GCS SIMULATOR';
  end;
  LoadFiles();
end;

procedure TMainForm.LoadFiles();
var
  Path: String;
begin
  if IsSim then
    Path := AppDataModule.SimXmlFilePath
  else
    Path := AppDataModule.IfcXmlFilePath;

  Log('LoadFiles: ' + Path);
  AppDataModule.LoadFilesList(Path, '*.xml', ComboBoxXmlFileName.Items);
  //AppDataModule.LoadFilesList(Path, '*.yml', ComboBoxYmlFileName.Items);
end;

procedure TMainForm.ProcessGuiFile(FileName: String);
var
  Lines: TStringList;
  Ini: TMemIniFile;
  Cmd, AText: String;
  AColor, BColor: TColor;
begin
  Lines := TStringList.Create;
  Ini := TMemIniFile.Create('');
  AppDataModule.LoadYmlToIni(FileName, Ini);
  try
     Cmd := Ini.ReadString('Msg', 'Cmd', '');

     if Cmd = 'Log' then
     begin
        AText := Ini.ReadString('Msg', 'Text', '');
        AColor := Ini.ReadInteger('Msg', 'Color', clBlack);
        BColor := Ini.ReadInteger('Msg', 'Bkg', clWhite);
        Log(AText, AColor, BColor);
     end;
  finally
    Lines.Free;
    Ini.Free;
  end;
end;


procedure TMainForm.PollGuiMsg();
var
  Prefix, FileName: String;
begin
  if IsSim then
     Prefix := 'sim'
  else
    Prefix := 'ifc';

  FileName := AppDataModule.PollGuiMsg(Prefix);
  if FileName <> '' then
  begin
    try
       ProcessGuiFile(FileName);
    except
    end;
    DeleteFile(FileName);
  end;
end;

procedure TMainForm.SendGuiMsg(AText: String;  ATitle: String);
var
  Prefix: String;
begin
  if IsSim then
     Prefix := 'sim_'
  else
    Prefix := 'ifc_';

  Log('SendGuiMsg: ' + ATitle, clBlue);
  AppDataModule.SendGuiMsg(AText, SynEditXml.Lines.Text, Prefix);
  //AppDataModule.SendGuiMsg(AText, MemoXml.Lines.Text, Prefix);
end;


procedure TMainForm.Log(Line: String;  AColor: TColor;  BColor: TColor);
begin
  //
  LogPanel.Add(Line, AColor, BColor);

  with MemoLog do
  begin
    //Lines.Add(Line);
    //SetRangeColor(Length(Lines.Text) - Length(Lines[Lines.Count - 1]) - Lines.Count - 1,
    //  Length(Lines[Lines.Count - 1]), AColor);
  end;

  if IsSim then
    Line := '[SIM] ' + Line
  else
    Line := '[IFC] ' + Line;

  AppDataModule.Log(Line);
end;

//===========================================================================
//                                 VCL Events
//===========================================================================
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create LogPanel
  LogPanel := TLogPanel.Create(self);
  PanelLog.InsertControl(LogPanel);
  LogPanel.Align := alClient;

  Init();
end;

procedure TMainForm.BtnLoadFilesClick(Sender: TObject);
begin
  LoadFiles();
end;

procedure TMainForm.BtnSendKeepAliveClick(Sender: TObject);
var
   Yml: String;
begin
  //
  Yml :=
     'Msg:'                                + LineBreak +
     '  Cmd: SendKeepAlive'                + LineBreak +
     LineBreak;

  SendGuiMsg(Yml);
end;


procedure TMainForm.BtnRunScriptClick(Sender: TObject);
begin
  //AppDataModule.RunScript();
end;


procedure TMainForm.BtnSendXmlClick(Sender: TObject);
var
  Yml: String;
begin
  // Prepare
  Yml :=
   'Msg:'                  + LineBreak +
   '  Cmd: SendXmlFile'    + LineBreak +
   LineBreak;

  SendGuiMsg(Yml, 'SendXmlFile');
end;


procedure TMainForm.ComboBoxXmlFileNameChange(Sender: TObject);
var
  FileName: String;
begin
  FileName := ComboBoxXmlFileName.Items[ComboBoxXmlFileName.ItemIndex];
  SynEditXml.Lines.LoadFromFile(FileName);
  //MemoXml.Lines.LoadFromFile(FileName);
end;


procedure TMainForm.ComboBoxYmlFileNameChange(Sender: TObject);
var
  FileName: String;
begin
  FileName := ComboBoxXmlFileName.Items[ComboBoxYmlFileName.ItemIndex];
  MemoYml.Lines.LoadFromFile(FileName);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false;
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


procedure TMainForm.TimerAutoSendTimer(Sender: TObject);
begin
  TimerAutoSend.Enabled := false;
  try
    if CheckBoxAutoSendRandomMsg.Checked then
    begin
      ComboBoxXmlFileName.ItemIndex := Random(ComboBoxXmlFileName.Items.Count);
      ComboBoxXmlFileNameChange(Sender);
      BtnSendXmlClick(Sender);
    end;
  finally
  end;
  TimerAutoSend.Enabled := true;
end;


procedure TMainForm.TimerPollGuiMsgTimer(Sender: TObject);
begin
  TimerPollGuiMsg.Enabled := false;
  try
    PollGuiMsg();
  finally
  end;
  TimerPollGuiMsg.Enabled := true;
end;

procedure TMainForm.TimerUpdateStatusTimer(Sender: TObject);
begin
  TimerUpdateStatus.Enabled := false;
  try
  finally
  end;
  TimerUpdateStatus.Enabled := true;
end;


end.

