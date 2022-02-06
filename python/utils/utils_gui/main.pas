// Lazarus 2.2.0
// Windows installer: lazarus-2.2.0-fpc-3.2.2-win64.exe
//
// Install Lazarus on Ubutnu
// https://ubuntuhandbook.org/index.php/2021/11/install-lazarus-ide-ubuntu/

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, SynHighlighterSQL,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls,
  Menus, DBGrids, Process, IniFiles, DB, LogPanel, util_datamod, about;


type

  { TMainForm }

  TMainForm = class(TForm)
    BtnRunScript1: TBitBtn;
    BtnSendXml: TBitBtn;
    CheckBoxXlsSqlFile: TCheckBox;
    CheckBoxXlsSqlFolder: TCheckBox;
    CheckBoxXlsSqlPostgre: TCheckBox;
    CheckBoxXlsSqlSQLite: TCheckBox;
    CheckBoxXlsSqlFirebird: TCheckBox;
    CheckBoxXlsSqlMatlab: TCheckBox;
    CheckBoxXlsSqlPython: TCheckBox;
    CheckBoxXlsSqlCpp: TCheckBox;
    CheckBoxXlsSqlDelphi: TCheckBox;
    CheckBoxXlsSqlDart: TCheckBox;

    CheckBoxDocSubdirs: TCheckBox;
    CheckBoxDocTxt: TCheckBox;
    CheckBoxDocMlx: TCheckBox;
    CheckBoxDocTrim: TCheckBox;
    CheckBoxDocFuncList: TCheckBox;

    EditXlsFileName: TEdit;
    EditGenDorFolder: TEdit;
    EditXlsFolderName: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Image1: TImage;
    LabelTitle: TLabel;
    MainMenu: TMainMenu;
    MemoXml: TMemo;
    MISimulatorWindow: TMenuItem;
    MIAbout: TMenuItem;
    MIHelp: TMenuItem;
    MIInterfaceWindow: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    PageControlMain: TPageControl;
    Panel1: TPanel;
    Panel11: TPanel;
    Panel13: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel9: TPanel;
    PanelLog: TPanel;
    PanelLog1: TPanel;
    PanelXml: TPanel;
    Process: TProcess;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    SynEditSQL: TSynEdit;
    SynSQLSyn: TSynSQLSyn;
    TabSheetXls2sql: TTabSheet;
    TabSheetDocGen: TTabSheet;
    TimerUpdateProcessOutput: TTimer;
    procedure BtnLoadFilesClick(Sender: TObject);
    procedure BtnSendKeepAliveClick(Sender: TObject);
    procedure BtnRunScriptClick(Sender: TObject);
    procedure BtnSendXmlClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure TimerUpdateProcessOutputTimer(Sender: TObject);
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

    //
    procedure SendGuiMsg(AText: String;  ATitle: String = '');

    //
    procedure RunPy(Cmd: String;  Params: TStrings);

    //
    procedure Log(Line: String;  AColor: TColor = clBlack;  BColor: TColor = clWhite);

  //=========================================================================
  public
    LogPanel: TLogPanel;

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

  AppDataModule.Init();

  //
  //LoadFiles();
end;


procedure TMainForm.LoadFiles();
var
  Path: String;
begin
  //Log('LoadFiles: ' + Path);
  //AppDataModule.LoadFilesList(Path, '*.xml', ComboBoxXmlFileName.Items);
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
  Prefix := 'ifc_';

  Log('SendGuiMsg: ' + ATitle, clBlue);
  AppDataModule.SendGuiMsg(AText, SynEditSQL.Lines.Text, Prefix);
  //AppDataModule.SendGuiMsg(AText, MemoXml.Lines.Text, Prefix);
end;


procedure TMainForm.RunPy(Cmd: String;  Params: TStrings);
begin
  Process.Executable := 'python3.exe';
  Process.Parameters.Assign(Params);
  Process.Options := [poUsePipes];
  Process.Execute();
end;

procedure TMainForm.Log(Line: String;  AColor: TColor;  BColor: TColor);
begin
  //
  LogPanel.Add(Line, AColor, BColor);
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
var
  Cmd: String;
  Params: TStringList;
begin
  Params := TStringList.Create();

  Cmd := '..' + DirectorySeparator + 'matlab_docgen.py ';

  Cmd := Cmd + ' -d ' + EditGenDorFolder.Text;

  if CheckBoxDocSubdirs.Checked then
    Cmd := Cmd + '-subdirs';

  if CheckBoxDocTxt.Checked then
    Cmd := Cmd + '-txt';

  if CheckBoxDocMlx.Checked then
    Cmd := Cmd + '-mlx';

  if CheckBoxDocTrim.Checked then
    Cmd := Cmd + '-trim';

  if CheckBoxDocFuncList.Checked then
    Cmd := Cmd + '-funclist';

  RunPy(Cmd, Params);
  Params.Free();
end;


procedure TMainForm.BtnSendXmlClick(Sender: TObject);
var
  Cmd: String;
  Params: TStringList;
begin
  Params := TStringList.Create();

  Cmd := '..' + DirectorySeparator + 'xls2sql.py ';

  if CheckBoxXlsSqlFile.Checked then
  begin
     Params.Add('-f');
     Params.Add(EditXlsFileName.Text);
  end
  else if CheckBoxXlsSqlFolder.Checked then
  begin
    Params.Add('-d');
    Params.Add(EditXlsFolderName.Text);
  end;

  if CheckBoxXlsSqlPostgre.Checked then
    Params.Add('-postgres');

  if CheckBoxXlsSqlSQLite.Checked then
    Params.Add('-sqlite');

  if CheckBoxXlsSqlFirebird.Checked then
    Params.Add('-firebird');

  if CheckBoxXlsSqlMatlab.Checked then
    Params.Add('-matlab');

  if CheckBoxXlsSqlPython.Checked then
    Params.Add('-python');

  if CheckBoxXlsSqlCpp.Checked then
    Params.Add('-cpp');

  if CheckBoxXlsSqlDelphi.Checked then
    Params.Add('-delphi');

  if CheckBoxXlsSqlDart.Checked then
    Params.Add('-dart');

  RunPy(Cmd, Params);
  Params.Free();
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := true;  // false;
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

procedure TMainForm.TimerUpdateProcessOutputTimer(Sender: TObject);
begin
  TimerUpdateProcessOutput.Enabled := false;
  try
  finally
  end;
  TimerUpdateProcessOutput.Enabled := true;
end;

end.

