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
  StdCtrls, Buttons, ComCtrls, Menus, Process, IniFiles,
  gcs_datamod, gcs_about;


type

  { TMainForm }

  TMainForm = class(TForm)
    BtnCloseAll1: TBitBtn;
    BtnLoadFiles: TBitBtn;
    BtnRecordLeft1: TBitBtn;
    BtnRunScript: TBitBtn;
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
    procedure BtnRunScriptClick(Sender: TObject);
    procedure BtnSendXmlClick(Sender: TObject);
    procedure BtnSendYmlClick(Sender: TObject);
    procedure ComboBoxXmlFileNameChange(Sender: TObject);
    procedure ComboBoxYmlFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MINewWindowClick(Sender: TObject);
    procedure TimerPollGuiMsgTimer(Sender: TObject);
  private

  //=========================================================================

  //=========================================================================
  public

    //
    procedure Init();

    //
    procedure ProcessGuiFile(FileName: String);


    // Load XML and YML file lists
    procedure LoadFiles();

    //
    procedure PollGuiMsg();

    procedure SendGuiMsg(AText: String);

    //
    procedure Log(Line: String);

  //=========================================================================
  public
  end;

//===========================================================================
//
//===========================================================================
var
  MainForm: TMainForm;
  FormCount: Integer = 1;

const

  LineBreak: String = Char(#10);

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Init();
begin
  //
  AppDataModule.Init();

  //
  LoadFiles();

end;


procedure TMainForm.LoadFiles();
var
  Path: String;
begin
  Path := AppDataModule.XmlFilePath;
  AppDataModule.LoadFilesList(Path, '*.xml', ComboBoxXmlFileName.Items);
  AppDataModule.LoadFilesList(Path, '*.yml', ComboBoxYmlFileName.Items);
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


procedure TMainForm.PollGuiMsg();
var
  Path: String;
  FileName: String;
  FileList: TStringList;
begin
  Path := 'c:\gcs\gui_in';
  FileList := TStringList.Create;
  try
    AppDataModule.LoadFilesList(Path, '*.yml', FileList);
    FileList.Sort;
    if FileList.Count > 0 then
    begin
        FileName := FileList.Strings[0];
        ProcessGuiFile(FileName);
    end;

  finally
    FileList.Free;
  end;

end;


procedure TMainForm.SendGuiMsg(AText: String);
var
   FileName: String;
   Lines: TStringList;
begin
  FileName := AppDataModule.GetNewFileName(AppDataModule.ConfigIni.ReadString('Gui', AppDataModule.GuiInPath, ''), '');

  MemoXml.Lines.SaveToFile(FileName + '.xml');
  MemoYml.Lines.SaveToFile(FileName + '.yml');

  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    Lines.SaveToFile(FileName + '.yml');
  finally
    Lines.Free;
  end;
end;



procedure TMainForm.Log(Line: String);
begin
  MemoLog.Lines.Add(Line);

end;

//===========================================================================
//                                 VCL Events
//===========================================================================
procedure TMainForm.FormCreate(Sender: TObject);
begin
  Init();
end;


procedure TMainForm.BtnLoadFilesClick(Sender: TObject);
begin
  LoadFiles();
end;


procedure TMainForm.BtnRunScriptClick(Sender: TObject);
begin
  //AppDataModule.RunScript();
end;


procedure TMainForm.BtnSendXmlClick(Sender: TObject);
begin
  Log('SendYml');

end;


procedure TMainForm.BtnSendYmlClick(Sender: TObject);
var
  Yml: String;
begin
  Log('SendYml');

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
  Inc(FormCount);
  Form.Caption := Form.Caption + ' #' + IntToStr(FormCount);
  Form.Show();
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


end.

