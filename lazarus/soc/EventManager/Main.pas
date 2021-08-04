unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  About;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    MIAbout: TMenuItem;
    MIHelp: TMenuItem;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure MIAboutClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.MIExitClick(Sender: TObject);
begin
  Application.Terminate();
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

end.

