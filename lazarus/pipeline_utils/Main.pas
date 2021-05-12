unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  StdCtrls, About, LogPanel;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageLogo: TImage;
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    MIAbout: TMenuItem;
    MIHelp: TMenuItem;
    Panel1: TPanel;
    PanelLogo: TPanel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure MIAboutClick(Sender: TObject);
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
begin
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

procedure TMainForm.MIAboutClick(Sender: TObject);
begin
  if AboutForm = nil then
     AboutForm := TAboutForm.Create(Self);

  AboutForm.Show();
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
     CanClose := Application.Terminated;
end;

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

