unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes;

type

  { TfrmTurtle }

  TfrmTurtle = class(TForm)
    VScreen: TBGRAVirtualScreen;
    procedure VScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VScreenMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure IdleProc(Sender: TObject; var Done: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  public
    GAngle: Single;
    MouseIsDown: Boolean;
  end;

var
  frmTurtle: TfrmTurtle;

implementation

{$R *.lfm}

{ TfrmTurtle }

procedure TfrmTurtle.VScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown := True;
end;

procedure TfrmTurtle.VScreenMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown := False;
end;

procedure TfrmTurtle.VScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  I: Integer;
begin
  if MouseIsDown then
    GAngle += 0.003
  else
    GAngle += 0.05;
  with Bitmap.Canvas2D do
  begin
    Fill;
    SetTransform(1, 0, 0, 1, 0, 0);
    Translate(Width / 2, Height / 2);
    for I := 1 to 490 do
    begin
      BeginPath;
      MoveTo(0, 0);
      Translate(I, 0);
      LineTo(0, 0);
      Stroke;
      Rotate(GAngle * 0.01745329238);
    end;
  end;
end;

procedure TfrmTurtle.IdleProc(Sender: TObject; var Done: Boolean);
begin
  Done := False;
  VScreen.RedrawBitmap;
end;

procedure TfrmTurtle.FormShow(Sender: TObject);
begin
  GAngle := 0;
  MouseIsDown := False;
  VScreen.OnMouseDown := @VScreenMouseDown;
  VScreen.OnMouseUp := @VScreenMouseUp;
  VScreen.OnRedraw := @VScreenRedraw;
  VScreen.Bitmap.Canvas2D.StrokeStyle(clBlack);
  Application.OnIdle := @IdleProc;
end;

procedure TfrmTurtle.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Application.OnIdle := nil;
  VScreen.OnRedraw := nil;
  VScreen.OnMouseUp := nil;
  VScreen.OnMouseDown := nil;
  CanClose := True;
end;

end.
