unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  About;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MIAbout: TMenuItem;
    MIFile: TMenuItem;
    MIHelp: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure MIHelpClick(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.MIHelpClick(Sender: TObject);
begin

end;

end.


