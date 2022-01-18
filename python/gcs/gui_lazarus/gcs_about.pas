unit gcs_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BtnClose: TButton;
    ImageLogo: TImage;
    Label4: TLabel;
    LabelVersion: TLabel;
    LabelTitle: TLabel;
    LabelCopyright: TLabel;
    LabelSubTitle: TLabel;
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

