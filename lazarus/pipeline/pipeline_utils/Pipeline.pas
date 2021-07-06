unit Pipeline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, ComCtrls, About, LogPanel;

type

  { TPipelineForm }

  TPipelineForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    EditInputImage: TEdit;
    EditStartTime: TEdit;
    EditEndTime: TEdit;
    EditCurProc: TEdit;
    EditStatus: TEdit;
    EditStatus1: TEdit;
    ImageLogo: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MainMenu: TMainMenu;
    Memo1: TMemo;
    MIDbCreateDbFromSql: TMenuItem;
    MIDbXlsxToSql: TMenuItem;
    MIDatabase: TMenuItem;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    MIAbout: TMenuItem;
    MIHelp: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel8: TPanel;
    PanelLogo: TPanel;
    TabSheet1: TTabSheet;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure MIAboutClick(Sender: TObject);
    procedure MIDbCreateDbFromSqlClick(Sender: TObject);
    procedure MIDbXlsxToSqlClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  PipelineForm: TPipelineForm;

implementation

{$R *.lfm}


var
  Log: TLogPanel;
  idx: Integer;

{ TPipelineForm }

procedure TPipelineForm.MIExitClick(Sender: TObject);
var
  r: trect;
begin
  r := rect(1,2,3,4);
  Application.Terminate();
end;

procedure TPipelineForm.Timer1Timer(Sender: TObject);
var
  i: Integer;
begin
  //
  for i := 1 to 100 do
  begin
    Log.Add(IntToStr(Idx) + ' aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ' + IntToStr(Idx));
    Inc(Idx);
  end;

end;

// Menu Events

procedure TPipelineForm.MIAboutClick(Sender: TObject);
begin
  if AboutForm = nil then
     AboutForm := TAboutForm.Create(Self);

  AboutForm.Show();
end;

procedure TPipelineForm.MIDbCreateDbFromSqlClick(Sender: TObject);
begin
  //
end;

procedure TPipelineForm.MIDbXlsxToSqlClick(Sender: TObject);
begin
  //
end;

procedure TPipelineForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
     CanClose := Application.Terminated;
end;

// Buttons Events

procedure TPipelineForm.Button1Click(Sender: TObject);
begin
  Log := TLogPanel.Create(Panel1);
  Panel1.InsertControl(Log);
  Log.Align := alClient;

  //Timer1.Enabled := true;
end;

procedure TPipelineForm.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  //
  for i := 1 to 100 do
  begin
    Log.Add(IntToStr(Idx) + ' aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ' + IntToStr(Idx));
    Inc(Idx);
  end;
end;

end.

