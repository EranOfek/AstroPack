{
 Fractal Tree
 Based on https://www.rosettacode.org/wiki/Fractal_tree#JavaScript
 2018 by Lainz
}

unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BCTypes, BGRABitmapTypes, Types,
  Math;

const
  deg_to_rad = pi / 180;

type

  { TfrmFractalTree }

  TfrmFractalTree = class(TForm)
    Timer1: TTimer;
    vsCanvas: TBGRAVirtualScreen;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure vsCanvasMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure vsCanvasMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure vsCanvasRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    t: single;
    multiplier: single;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public

  end;

var
  frmFractalTree: TfrmFractalTree;

implementation

{$R *.lfm}

{ TfrmFractalTree }

procedure TfrmFractalTree.vsCanvasRedraw(Sender: TObject; Bitmap: TBGRABitmap);

procedure drawLine(x1, y1, x2, y2, depth: single);
begin
  Bitmap.DrawLineAntialias(x1, y1, x2, y2, BGRABlack, 1, False);
end;

procedure drawTree(x1, y1, angle, depth: single);
var
  x2, y2: single;
begin
  if (depth > 0) then
  begin
    x2 := x1 + (cos(angle * deg_to_rad) * depth * multiplier);
    y2 := y1 + (sin(angle * deg_to_rad) * depth * multiplier);
    drawLine(x1, y1, x2, y2, depth);
    // Use even values without randomness to get a 'real' fractal image
    drawTree(x2, y2, angle - t, depth - 1);
    drawTree(x2, y2, angle + t, depth - 1);
  end;
end;

begin
  //Bitmap.GradientFill(0, 0, Bitmap.Width, Bitmap.Height, clSkyBlue, BGRA(0,125,0), gtLinear, PointF(0,0), PointF(0, Bitmap.Height), dmSet);
  drawTree(Bitmap.Width div 2, Bitmap.Height div 2, -90, 6);
  drawTree(Bitmap.Width div 2, Bitmap.Height div 2, 90, 6);
  drawTree(Bitmap.Width div 2, Bitmap.Height div 2, -180, 6);
  drawTree(Bitmap.Width div 2, Bitmap.Height div 2, 360, 6);
  drawTree(Bitmap.Width div 2, Bitmap.Height div 2, -45, 6);
  drawTree(Bitmap.Width div 2, Bitmap.Height div 2, 45, 6);
  drawTree(Bitmap.Width div 2, Bitmap.Height div 2, -225, 6);
  drawTree(Bitmap.Width div 2, Bitmap.Height div 2, -135, 6);
end;

procedure TfrmFractalTree.OnIdle(Sender: TObject; var Done: Boolean);
begin
  vsCanvas.DiscardBitmap;
end;

procedure TfrmFractalTree.vsCanvasMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  // zoom out
  multiplier -= 0.25;
  if multiplier <= 0 then
    multiplier := 0.25;

  vsCanvas.DiscardBitmap;
end;

procedure TfrmFractalTree.FormCreate(Sender: TObject);
begin
  // default zoom
  t := 0;
  multiplier := 20;
  Application.OnIdle:=@OnIdle;
end;

procedure TfrmFractalTree.Timer1Timer(Sender: TObject);
begin
  t += 0.1;
  caption:=t.tostring;
end;

procedure TfrmFractalTree.vsCanvasMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  // zoom in
  multiplier += 0.25;

  vsCanvas.DiscardBitmap;
end;

end.

