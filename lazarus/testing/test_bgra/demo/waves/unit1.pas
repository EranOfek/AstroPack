unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRABitmap, BGRABitmapTypes, Math;

const
  STEP = 0.04;
  MAXCOUNT = 30;
  MINSIZE = 30;
  MAXSIZE = 100;
  BLURRY = TRUE;

type

  { TForm1 }

  { TCircle }

  TCircle = class(TObject)
  private
    FCX: double;
    FCY: double;
    FPercent: double;
    FSize: integer;
  public
    property Percent: double read FPercent write FPercent;
    property CX: double read FCX write FCX;
    property CY: double read FCY write FCY;
    property Size: integer read FSize write FSize;
  end;

  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    pos: integer;
    Circles: array [0..MAXCOUNT - 1] of TCircle;
    bmp: TBGRABitmap;
    r, g, b: byte;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClick(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  bmp := TBGRABitmap.Create(Width, Height, BGRA(50, 50, 200));
  pos := 0;
  for i := 0 to MAXCOUNT - 1 do
  begin
    Circles[i] := TCircle.Create;
    Circles[i].Percent := 1;
  end;
  r := 255;
  g := 255;
  b := 255;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  Timer1.Enabled := False;
  for i := 0 to MAXCOUNT - 1 do
  begin
    Circles[i].Free;
  end;
  bmp.Free;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: integer;
begin
  for i := 0 to 7 do
  begin
    with Circles[(pos mod MAXCOUNT)] do
    begin
      Percent := 0;
      Size := MINSIZE * i;
      CX := X;
      CY := Y;
    end;
    pos += 1;
  end;
  r := RandomRange(0, 256);
  g := RandomRange(0, 256);
  b := RandomRange(0, 256);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  with Circles[(pos mod MAXCOUNT)] do
  begin
    Percent := 0;
    Size := RandomRange(MINSIZE, MAXSIZE);
    CX := X;
    CY := Y;
  end;
  pos += 1;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  i: integer;
  alpha: byte;
begin
  for i := 0 to MAXCOUNT - 1 do
  begin
    if Circles[i].Percent <= 1 then
    begin
      alpha := 255 - round(Circles[i].Percent * 255);
      bmp.EllipseAntialias(Circles[i].CX, Circles[i].CY, Circles[i].Size *
        Circles[i].Percent, Circles[i].Size * Circles[i].Percent,
        BGRA(r, g, b, alpha),
        2);
    end;
    if BLURRY and (i = (MAXCOUNT div 2)) then
      BGRAReplace(bmp, bmp.FilterBlurRadial(2, rbBox));
  end;
  bmp.Draw(Canvas, 0, 0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to MAXCOUNT - 1 do
  begin
    Circles[i].Percent := Circles[i].Percent + STEP;
  end;
  Invalidate;
end;

end.
