unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, About, LogPanel;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageLogo: TImage;
    Label7: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MIDbCreateDbFromSql: TMenuItem;
    MIDbXlsxToSql: TMenuItem;
    MIDatabase: TMenuItem;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    MIAbout: TMenuItem;
    MIHelp: TMenuItem;
    PanelLogo: TPanel;
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
  MainForm: TMainForm;

implementation

{$R *.lfm}


var
  Log: TLogPanel;
  idx: Integer;

{ TMainForm }

procedure TMainForm.MIExitClick(Sender: TObject);
var
  r: trect;
begin
  r := rect(1,2,3,4);
  Application.Terminate();
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
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

procedure TMainForm.MIAboutClick(Sender: TObject);
begin
  if AboutForm = nil then
     AboutForm := TAboutForm.Create(Self);

  AboutForm.Show();
end;

procedure TMainForm.MIDbCreateDbFromSqlClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.MIDbXlsxToSqlClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
     CanClose := Application.Terminated;
end;

// Buttons Events

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Log := TLogPanel.Create(Panel1);
  Panel1.InsertControl(Log);
  Log.Align := alClient;

  //Timer1.Enabled := true;
end;

procedure TMainForm.Button2Click(Sender: TObject);
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

