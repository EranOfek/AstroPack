unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BtnClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    procedure BtnCloseClick(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.BtnCloseClick(Sender: TObject);
begin
  Close();
end;

end.

