unit DbBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, PQConnection, SQLDB, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, DBGrids, StdCtrls, ComCtrls, Buttons,
  RunProc;

type

  { TDbBrowserForm }

  TDbBrowserForm = class(TForm)
    BtnConvertXlsToSql: TBitBtn;
    BtnCreateDb: TBitBtn;
    BtnSelectOutputSqlFolder: TBitBtn;
    BtnSelectXlsxFile: TBitBtn;
    ComboOutputSqlFolder: TComboBox;
    ComboXlsFile: TComboBox;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel9: TPanel;
    StatusBar1: TStatusBar;
    OpenDialogXls: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
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
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  DbBrowserForm: TDbBrowserForm;

implementation

{$R *.lfm}

var
    Proc: TRunProc;
    AstroPath: String;

function AstroFile(FileName: String) : String;
begin
  FileName := StringReplace(FileName, '/', PathDelim, [rfReplaceAll]);
  FileName := StringReplace(FileName, '\', PathDelim, [rfReplaceAll]);
  FileName := AstroPath + PathDelim + FileName;
  Result := FileName;
end;


{ TDbBrowserForm }

procedure TDbBrowserForm.FormCreate(Sender: TObject);
begin
  Proc := TRunProc.Create;
  Proc.Init;
  Proc.Memo1 := Memo1;
  Proc.StatusBar1 := StatusBar1;

  ComboXlsFile.Text := OpenDialogXls.InitialDir + PathDelim + OpenDialogXls.FileName;

  AstroPath := GetEnvironmentVariable('ASTROPACKPATH');
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
  ScriptFileName := AstroFile('python/utils/database_utils/xlsx2sql.py');
  //Params := '-f ' + ComboXlsFile.Text;

  FileName := AstroFile('python/utils/database_utils/db/lastdb__tables.xlsx');
  Params := '-f ' + FileName;
  Cmd := 'python3 ' + ScriptFileName + ' ' + Params;
  Proc.Run(Cmd);


  //Proc.Destroy();
  {
  Lines := TStringList.Create;
  try
    ScriptFileName := 'D:\Ultrasat\AstroPack.git\python\utils\database_utils\xlsx2sql_lang.py';
    Lines.LoadFromFile(ScriptFileName);
    PythonEngine1.ExecStrings(Lines);
  finally
    Lines.free;
  end;}

end;

procedure TDbBrowserForm.BtnCreateDbClick(Sender: TObject);
var
  ScriptFileName, Params, Cmd: String;
begin
    ScriptFileName := AstroPath + PathDelim + 'python\utils\database_utils\create_db.py';
    Params := '-f ' + ComboXlsFile.Text;
    Cmd := 'python3 ' + ScriptFileName + ' ' + Params;
    Proc.Run(Cmd);

  //Cmd := 'psql -U postgres -f D:\Ultrasat\AstroPack.git\python\utils\database_utils\db\unittest\t2.sql';
  Proc.Run(Cmd);
end;


end.

