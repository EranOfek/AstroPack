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
var dpi: single;
begin
  dpi := Screen.PixelsPerInch;
  if not BGRAVirtualScreen1.BitmapAutoScale then
    dpi *= BGRAVirtualScreen1.GetCanvasScaleFactor;
  Atoms.Draw(Bitmap, dpi);
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
  BGRAVirtualScreen1.DiscardBitmap;
end;

end.

