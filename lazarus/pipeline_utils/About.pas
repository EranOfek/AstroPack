unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BtnClose: TButton;
    ImageLogo: TImage;
    LabelVersion: TLabel;
    LabelTitle: TLabel;
    LabelCopyright: TLabel;
    LabelSubTitle: TLabel;
    Panel1: TPanel;
    procedure BtnCloseClick(Sender: TObject);
    procedure ImageLogoClick(Sender: TObject);
    procedure LabelVersionClick(Sender: TObject);
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

procedure TAboutForm.ImageLogoClick(Sender: TObject);
begin

end;

procedure TAboutForm.LabelVersionClick(Sender: TObject);
begin

end;

end.

