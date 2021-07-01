unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BCTypes, uAtom;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    Atoms: TAtoms;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap
  );
begin
  Atoms.Draw(Bitmap);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Atoms := TAtoms.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Atoms.Free;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  Atoms.Step;
  BGRAVirtualScreen1.DiscardBitmap(Rect((Width div 2) - 190, (Height div 2) - 190, (Width div 2) + 190, (Height div 2) + 190));
end;

end.

