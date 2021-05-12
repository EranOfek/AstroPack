unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  About;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageLogo: TImage;
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    MIAbout: TMenuItem;
    MIHelp: TMenuItem;
    PanelLogo: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure MIAboutClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure PanelLogoClick(Sender: TObject);
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

procedure TMainForm.PanelLogoClick(Sender: TObject);
begin

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

