unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BCTypes, BGRABitmapTypes, Math;

type

  { TfrmSuperFormula }

  TfrmSuperFormula = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    t: double;
    procedure OnIdle(Sender: TObject; var Done: boolean);
    function mouseX: integer;
    function mouseY: integer;
  public

  end;

var
  frmSuperFormula: TfrmSuperFormula;
  MAX_LOG: Double = 0.0;
  MIN_LOG: Double = 0.0;
  MAX_R: Double = 100.0;

implementation

function SafePower(a, b: Double; out c: Double): Boolean;
var
  tmp: Double;
begin
  Result := true;
  if (a < 0) then
    raise Exception.Create('1st argument of Power() must not be negative');
  if (a = 0) then begin
    if (b = 0) then
      raise Exception.Create('Both arguments of Power() must not be zero.');
    c := 0;
    exit;
  end;

  if MAX_LOG = 0.0 then
    MAX_LOG := ln(MaxDouble);
  if MIN_LOG = 0.0 then
    MIN_LOG := ln(MinDouble);

  // ln(a^b) = b ln(a)
  tmp := b * ln(a);
  if tmp > MAX_LOG then
    Result := false
  else
  if tmp < MIN_LOG then
    c := 0.0
  else
    c := exp(tmp);
end;

function r(theta, a, b, m, n1, n2, n3: double): double;
const
  EPS = 1E-9;
var
  c, pc, s, ps: Double;
begin
  if (a = 0) then
    raise Exception.Create('a must not be zero.');
  if (b = 0) then
    raise Exception.Create('b must not be zero');
  if (m = 0) then
    raise Exception.Create('m must not be zero');
  if (n1 = 0) then
    raise Exception.Create('n1 must not be zero');
  if (n2 = 0) then
    raise Exception.Create('n2 must not be zero');
  if (n3 = 0) then
    raise Exception.Create('n3 must not be zero');

  c := abs(cos(m * theta / 4) / a);
  if c < EPS then
    pc := 0
  else
  if not SafePower(c, n2, pc) then begin
    Result := MAX_R;
    exit;
  end;

  s := abs(sin(m * theta / 4) / b);
  if s < EPS then
    ps := 0
  else
  if not SafePower(s, n3, ps) then begin
    Result := MAX_R;
    exit;
  end;

  if pc + ps < EPS then
    Result := 0
  else
  if not SafePower(pc + ps, -1/n1, Result) then
    Result := MAX_R;

  if Result > MAX_R then Result := MAX_R;
end;

{$R *.lfm}

{ TfrmSuperFormula }

procedure TfrmSuperFormula.Timer1Timer(Sender: TObject);
begin
  t += 0.1;
end;

procedure TfrmSuperFormula.OnIdle(Sender: TObject; var Done: boolean);
begin
  Done := False;
  BGRAVirtualScreen1.DiscardBitmap;
end;

function TfrmSuperFormula.mouseX: integer;
begin
  { If you want to control the formula with mouse positions }
  Result := ScreenToClient(Mouse.CursorPos).x;
end;

function TfrmSuperFormula.mouseY: integer;
begin
  { If you want to control the formula with mouse positions }
  Result := ScreenToClient(Mouse.CursorPos).y;
end;

procedure TfrmSuperFormula.BGRAVirtualScreen1Redraw(Sender: TObject;
  Bitmap: TBGRABitmap);
var
  theta: double;
  rad: double;
  x: double = 0;
  y: double = 0;
begin
  theta := 0.01; // prevent starting line
  Bitmap.Canvas2D.resetTransform;
  Bitmap.Canvas2D.translate(Width div 2, Height div 2);
  Bitmap.Canvas2D.lineWidth := 1;
  Bitmap.Canvas2D.strokeStyle(BGRABlack);
  Bitmap.Canvas2D.beginPath;
  while (theta <= 2 * pi) do
  begin
    {
      If you want to control the formula with mouse positions replace with
      mouseX / 100
      mouseY / 100
    }
    rad := r(theta, 1, // a - size and control spikes
      1, // b - size and control spikes
      6, // m - number of spikes
      0.5, // n1 - roundness
      tan(t) * 0.5 + 0.5, // n2 - shape
      tan(t) * 0.5 + 0.5 // n3 - shape
      );
    x := rad * cos(theta) * (Width div 4);
    y := rad * sin(theta) * (Height div 4);
    try
      Bitmap.Canvas2D.lineTo(x, y);
    except
      on e: exception do
      begin
        // prevent floating point overflow
        exit;
      end;
    end;
    theta += 0.01; // resolution of the drawing
  end;
  Bitmap.Canvas2D.closePath; // prevent holes
  Bitmap.Canvas2D.stroke;
end;

procedure TfrmSuperFormula.FormCreate(Sender: TObject);
begin
  t := 0;
  Application.OnIdle := @OnIdle;
  {$ifdef Windows}
  DoubleBuffered := True;
  {$endif}
end;

end.
