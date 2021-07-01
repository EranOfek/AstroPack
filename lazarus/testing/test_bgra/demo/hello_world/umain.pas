unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Math, BGRABitmap, BGRABitmapTypes;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Timer1: TTimer;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    step: double;
    time: double;
    slide: integer;
    function easing(t: double): double;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormPaint(Sender: TObject);
var
  temp: double;
  x, y, x2, y2, w: integer;
  bmp: TBGRABitmap;
begin
  temp := easing(time);
  case slide of
    // top left, to bottom right (open)
    0:
    begin
      bmp := TBGRABitmap.Create(Width, Height, BGRAWhite);
      x := 10;
      y := 10;
      x2 := max(round(temp * (Width - 10)), 10);
      y2 := max(round(temp * (Height - 10)), 10);
      bmp.Rectangle(x, y, x2, y2, BGRABlack, BGRA(110, 130, 170), dmSet);
      // text
      bmp.ClipRect := Rect(x, y, x2, y2);
      bmp.FontHeight := (Height) div 6;
      bmp.TextOut((Width - bmp.TextSize('Hello World').cx) / 2, (y2 - bmp.TextSize('Hello World').cy) / 2, 'Hello World', BGRABlack);
      // fade in
      //bmp.AlphaFill(trunc(temp * 255));
      bmp.Draw(Canvas, 0, 0, False);
      bmp.Free;
    end;
    // top left, to bottom right (close)
    1:
    begin
      bmp := TBGRABitmap.Create(Width, Height, BGRAWhite);
      x := max(round(temp * (Width - 10)), 10);
      y := max(round(temp * (Height - 10)), 10);
      x2 := Width - 10;
      y2 := Height - 10;
      bmp.Rectangle(x, y, x2, y2, BGRABlack, BGRA(170, 130, 110), dmSet);
      // text
      bmp.ClipRect := Rect(x, y, x2, y2);
      bmp.FontHeight := (Height) div 6;
      bmp.TextOut((Width - bmp.TextSize('Hello World').cx) / 2, (y2 + y - bmp.TextSize('Hello World').cy) / 2, 'Hello World', BGRAWhite);
      // fade out
      //bmp.AlphaFill(255-trunc(temp * 255));
      bmp.Draw(Canvas, 0, 0, False);
      bmp.Free;
    end;
  end;
end;

procedure TfrmMain.Timer1StartTimer(Sender: TObject);
begin
  time := 0;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  step := 0.05;
  slide := 0;
end;

procedure TfrmMain.FormClick(Sender: TObject);
begin
  if not Timer1.Enabled then
  begin
    Timer1.Enabled := True;
    slide := slide + 1;
    if slide > 1 then
      slide := 0;
  end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  time := time + step;
  if time < 0 then
    time := 0
  else if time > 1 then
    time := 1;
  Invalidate;
  if Time = 1 then
    Timer1.Enabled := False;
end;

function TfrmMain.easing(t: double): double;
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
end;

end.
