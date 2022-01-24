unit DbBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, PQConnection, SQLDB, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, DBGrids, StdCtrls, ComCtrls, Buttons, Menus,
  RunProc, DataModu, About;

type

  { TDbBrowserForm }

  TDbBrowserForm = class(TForm)
    BtnConvertXlsToSql: TBitBtn;
    BtnCreateDb: TBitBtn;
    BtnSelectOutputSqlFolder: TBitBtn;
    BtnSelectXlsxFile: TBitBtn;
    ComboBoxDbName: TComboBox;
    ComboOutputSqlFolder: TComboBox;
    ComboXlsFile: TComboBox;
    ComboXlsFolder: TComboBox;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ImageLogo: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ListBoxDbTables: TListBox;
    MainMenu: TMainMenu;
    Memo1: TMemo;
    MIAbout: TMenuItem;
    MIDatabase: TMenuItem;
    MIDbCreateDbFromSql: TMenuItem;
    MIDbXlsxToSql: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIHelp: TMenuItem;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel9: TPanel;
    PanelLogo: TPanel;
    StatusBar: TStatusBar;
    OpenDialogXls: TOpenDialog;
    PageControl1: TPageControl;
    PanelTop: TPanel;
    PanelLeft: TPanel;
    PanelBottom: TPanel;
    Panel4: TPanel;
    Panel8: TPanel;
    PQConnection1: TPQConnection;
    SelectDirectoryDialogOutputSql: TSelectDirectoryDialog;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    TabSheetDbBrowser: TTabSheet;
    TabSheetCreateDb: TTabSheet;
    TabSheetXlsxToSQL: TTabSheet;
    procedure BtnConvertXlsToSqlClick(Sender: TObject);
    procedure BtnCreateDbClick(Sender: TObject);
    procedure BtnSelectOutputSqlFolderClick(Sender: TObject);
    procedure BtnSelectXlsxFileClick(Sender: TObject);
    procedure ComboBoxDbNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxDbTablesClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
  private
    Proc: TRunProc;

  public
    DbName : String;

  end;

var
  DbBrowserForm: TDbBrowserForm;

implementation

{$R *.lfm}

{ TDbBrowserForm }

procedure TDbBrowserForm.FormCreate(Sender: TObject);
begin
  Proc := TRunProc.Create;
  Proc.Init;
  Proc.Memo1 := Memo1;
  Proc.StatusBar1 := StatusBar;

  ComboXlsFile.Items.Clear();
  ComboXlsFile.Items.Add('unittest__tables.xlsx');
  ComboXlsFile.Items.Add('lastdb__tables.xlsx');  //OpenDialogXls.InitialDir + PathDelim + OpenDialogXls.FileName;

  ComboXlsFolder.Text := DataMod.AstroFile('database' + PathDelim + 'xlsx');
  ComboXlsFile.ItemIndex := 0;
end;

procedure TDbBrowserForm.ListBoxDbTablesClick(Sender: TObject);
begin
  //
end;

procedure TDbBrowserForm.MIAboutClick(Sender: TObject);
begin
  AboutForm.Show;
end;

procedure TDbBrowserForm.BtnSelectXlsxFileClick(Sender: TObject);
begin
  //
  if OpenDialogXls.Execute then
  begin
    if FileExists(OpenDialogXls.Filename) then
      ComboXlsFile.Text := OpenDialogXls.Filename;
      ShowMessage(OpenDialogXls.Filename);
  end
  else
    ShowMessage('No file selected');

end;

procedure TDbBrowserForm.ComboBoxDbNameChange(Sender: TObject);
begin
  //
  DbName := ComboBoxDbName.Text;
  ListBoxDbTables.Items.Assign(DataMod.GetDbTablesList(DbName));
end;

procedure TDbBrowserForm.BtnSelectOutputSqlFolderClick(Sender: TObject);
begin
  //
  if SelectDirectoryDialogOutputSql.Execute then
  begin
    ComboOutputSqlFolder.Text := SelectDirectoryDialogOutputSql.Filename;

  end;
end;

procedure TDbBrowserForm.BtnConvertXlsToSqlClick(Sender: TObject);
var
  Lines: TStringList;
  ScriptFileName, FileName, Params, Cmd: String;
begin
  // Prepare python command
  ScriptFileName := DataMod.AstroFile('python/utils/database_utils/xlsx2sql.py');
  FileName := ComboXlsFolder.Text + PathDelim + ComboXlsFile.Text;
  Params := '-f ' + FileName;
  Cmd := DataMod.PythonExe + ' ' + ScriptFileName + ' ' + Params;

  // Run
  Memo1.Lines.Add(Cmd);
  Proc.Run(Cmd);

end;

procedure TDbBrowserForm.BtnCreateDbClick(Sender: TObject);
var
  ScriptFileName, Params, Cmd: String;
begin
  // Prepare python command (@Todo: psql does not have redirected output, why? also to python!)
  ScriptFileName := DataMod.AstroFile('python\utils\database_utils\create_db.py');
  Params := '-f ' + ComboXlsFile.Text;
  Cmd := DataMod.PythonExe + ' ' + ScriptFileName + ' ' + Params;
  //Cmd := 'psql -U postgres -f D:\Ultrasat\AstroPack.git\python\utils\database_utils\db\unittest\t2.sql';

  // Run
  Memo1.Lines.Add(Cmd);
  Proc.Run(Cmd);


  Proc.Run(Cmd);
end;


end.

