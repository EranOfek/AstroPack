unit BGRASuperformulaOriginal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRALayerOriginal, BGRABitmap, BGRABitmapTypes, Math;

type

  { TBGRALayerSuperformulaOriginal }

  TBGRALayerSuperformulaOriginal = class(TBGRALayerCustomOriginal)
  private
    Fa: double;
    Fb: double;
    FFillColor: TBGRAPixel;
    FLineColor: TBGRAPixel;
    FLineWidth: double;
    Fm: double;
    FMultiplier: double;
    Fn1: double;
    Fn2: double;
    Fn3: double;
    procedure SetFa(AValue: double);
    procedure SetFb(AValue: double);
    procedure SetFFillColor(AValue: TBGRAPixel);
    procedure SetFLineColor(AValue: TBGRAPixel);
    procedure SetFLineWidth(AValue: double);
    procedure SetFm(AValue: double);
    procedure SetFMultiplier(AValue: double);
    procedure SetFn1(AValue: double);
    procedure SetFn2(AValue: double);
    procedure SetFn3(AValue: double);
  public
    constructor Create; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
      ADraft: boolean); override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    property a: double read Fa write SetFa;
    property b: double read Fb write SetFb;
    property m: double read Fm write SetFm;
    property n1: double read Fn1 write SetFn1;
    property n2: double read Fn2 write SetFn2;
    property n3: double read Fn3 write SetFn3;
    property LineWidth: double read FLineWidth write SetFLineWidth;
    property LineColor: TBGRAPixel read FLineColor write SetFLineColor;
    property FillColor: TBGRAPixel read FFillColor write SetFFillColor;
    property Multiplier: double read FMultiplier write SetFMultiplier;
  end;

implementation

var
  MAX_LOG: double = 0.0;
  MIN_LOG: double = 0.0;
  MAX_R: double = 100.0;

function SafePower(a, b: double; out c: double): boolean;
var
  tmp: double;
begin
  Result := True;
  if (a < 0) then
    raise Exception.Create('1st argument of Power() must not be negative');
  if (a = 0) then
  begin
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
    Result := False
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
  c, pc, s, ps: double;
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
  if not SafePower(c, n2, pc) then
  begin
    Result := MAX_R;
    exit;
  end;

  s := abs(sin(m * theta / 4) / b);
  if s < EPS then
    ps := 0
  else
  if not SafePower(s, n3, ps) then
  begin
    Result := MAX_R;
    exit;
  end;

  if pc + ps < EPS then
    Result := 0
  else
  if not SafePower(pc + ps, -1 / n1, Result) then
    Result := MAX_R;

  if Result > MAX_R then
    Result := MAX_R;
end;

{ TBGRALayerSuperformulaOriginal }

procedure TBGRALayerSuperformulaOriginal.SetFa(AValue: double);
begin
  if Fa = AValue then
    Exit;
  Fa := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFb(AValue: double);
begin
  if Fb = AValue then
    Exit;
  Fb := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFFillColor(AValue: TBGRAPixel);
begin
  if FFillColor = AValue then
    Exit;
  FFillColor := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFLineColor(AValue: TBGRAPixel);
begin
  if FLineColor = AValue then
    Exit;
  FLineColor := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFLineWidth(AValue: double);
begin
  if FLineWidth = AValue then
    Exit;
  FLineWidth := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFm(AValue: double);
begin
  if Fm = AValue then
    Exit;
  Fm := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFMultiplier(AValue: double);
begin
  if FMultiplier = AValue then
    Exit;
  FMultiplier := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFn1(AValue: double);
begin
  if Fn1 = AValue then
    Exit;
  Fn1 := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFn2(AValue: double);
begin
  if Fn2 = AValue then
    Exit;
  Fn2 := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFn3(AValue: double);
begin
  if Fn3 = AValue then
    Exit;
  Fn3 := AValue;
  NotifyChange;
end;

constructor TBGRALayerSuperformulaOriginal.Create;
begin
  inherited Create;
  Fa := 1;
  Fb := 1;
  Fm := 24;
  Fn1 := 2;
  Fn2 := 1;
  Fn3 := 2;
  FLineWidth := 0;
  FLineColor := $00804000;
  FFillColor := $00C08000;
  FMultiplier := 200;
end;

procedure TBGRALayerSuperformulaOriginal.Render(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean);
var
  theta: double;
  rad: double;
  x: double = 0;
  y: double = 0;
begin
  try
    theta := 0.001;
    ADest.Canvas2D.antialiasing := not ADraft;
    ADest.Canvas2D.resetTransform;
    ADest.Canvas2D.translate(ADest.Width div 2, ADest.Height div 2);
    ADest.Canvas2D.lineWidth := FLineWidth;
    ADest.Canvas2D.strokeStyle(FLineColor);
    ADest.Canvas2D.fillStyle(FFillColor);
    ADest.Canvas2D.beginPath;
    while (theta <= 2 * pi) do
    begin
      rad := r(theta, Fa, Fb, Fm, Fn1, Fn2,
        Fn3);
      x := rad * cos(theta) * FMultiplier;
      y := rad * sin(theta) * FMultiplier;
      ADest.Canvas2D.lineTo(x, y);
      theta += 0.001;
    end;
    ADest.Canvas2D.closePath;
    ADest.Canvas2D.fill;
    if FLineWidth > 0 then
      ADest.Canvas2D.stroke;
  except
    on e: Exception do ;
  end;
end;

function TBGRALayerSuperformulaOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
begin
  result := ADestRect;
end;

procedure TBGRALayerSuperformulaOriginal.LoadFromStorage(
  AStorage: TBGRACustomOriginalStorage);
var
  colorArray: ArrayOfTBGRAPixel;
begin
  Fa := AStorage.Float['a'];
  Fb := AStorage.Float['b'];
  Fm := AStorage.Float['m'];
  Fn1 := AStorage.Float['n1'];
  Fn2 := AStorage.Float['n2'];
  Fn3 := AStorage.Float['n3'];
  FMultiplier := AStorage.Float['multiplier'];
  FLineWidth := AStorage.Float['linewidth'];

  colorArray := AStorage.ColorArray['colors'];
  FLineColor := colorArray[0];
  FFillColor := colorArray[1];
end;

procedure TBGRALayerSuperformulaOriginal.SaveToStorage(
  AStorage: TBGRACustomOriginalStorage);
var
  colorArray: ArrayOfTBGRAPixel;
begin
  AStorage.Float['a'] := Fa;
  AStorage.Float['b'] := Fb;
  AStorage.Float['m'] := Fm;
  AStorage.Float['n1'] := Fn1;
  AStorage.Float['n2'] := Fn2;
  AStorage.Float['n3'] := Fn3;
  AStorage.Float['multiplier'] := FMultiplier;
  AStorage.Float['linewidth'] := FLineWidth;

  setlength(colorArray, 2);
  colorArray[0] := FLineColor;
  colorArray[1] := FFillColor;
  AStorage.ColorArray['colors'] := colorArray;
end;

class function TBGRALayerSuperformulaOriginal.StorageClassName: RawByteString;
begin
  Result := 'superformula';
end;

initialization
  RegisterLayerOriginal(TBGRALayerSuperformulaOriginal);

end.
