unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BCTypes, LCLType, Math;

const
  SIZE = 32;
  MAX_STEPS = 100;
  SHADOW_STEP = 255 div MAX_STEPS;

type

  { TGame }

  TGame = class(TForm)
    gameLoop: TTimer;
    gameCanvas: TBGRAVirtualScreen;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure gameCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure gameCanvasMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure gameCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure gameLoopStartTimer(Sender: TObject);
    procedure gameLoopTimer(Sender: TObject);
    procedure gameCanvasRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    cur: TPoint;
    steps: array[0..MAX_STEPS - 1] of TPoint;
    current_step: integer;
    last_color: TBGRAPixel;
    down: boolean;
    circles: integer;
    // http://jsfiddle.net/kBsdW/29/
    function isCircle(error: double): boolean;
    function getDistance(p1, p2: TPoint): double;
    function getCenter(p1, p2: TPoint): TPoint;
  public

  end;

var
  Game: TGame;

implementation

{$R *.lfm}

{ TGame }

procedure TGame.gameCanvasRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  x, y, col, row, num_row, num_col, i: integer;
  y2, x2: longint;
  //start_time, delta_time: TTime;
begin
  //start_time := Time();
  num_row := Height div size;
  num_col := Width div size;
  for row := 0 to num_row do
  begin
    for col := 0 to num_col do
    begin
      y := row * size;
      x := col * size;
      Bitmap.Rectangle(x, y, x + size, y + size, BGRA(20, 20, 30), BGRABlack, dmSet);
    end;
  end;
  for i := 0 to MAX_STEPS - 2 do
  begin
    y := steps[(current_step + i) mod MAX_STEPS].y * size;
    x := steps[(current_step + i) mod MAX_STEPS].x * size;
    if not down then
    begin
      Bitmap.Rectangle(x, y, x + size, y + size, BGRA(last_color.red,
        last_color.green, last_color.blue, SHADOW_STEP * i),
        BGRA(last_color.red, last_color.green, last_color.blue, SHADOW_STEP * i),
        dmDrawWithTransparency);
    end
    else
    begin
      y2 := steps[(current_step + i + 1) mod MAX_STEPS].y * size;
      x2 := steps[(current_step + i + 1) mod MAX_STEPS].x * size;
      Bitmap.DrawLineAntialias(x, y, x2, y2, BGRA(last_color.red,
        last_color.green, last_color.blue, SHADOW_STEP * i), 2);
    end;
  end;
  last_color := BGRA(RandomRange(200, 250), RandomRange(200, 250),
    RandomRange(200, 250));
  //delta_time := Time() - start_time;
  Bitmap.FontHeight := 15;
  //Bitmap.TextOut(10,10, FloatToStr(delta_time), BGRAWhite);
  if down and isCircle(0.3) then
    Inc(circles);
  Bitmap.TextOut(10, 10, IntToStr(circles), BGRAWhite);
end;

function TGame.isCircle(error: double): boolean;
var
  weights: array of double;
  maxDistance, sumDistance, avgDistance, errorConstraint, distance, d: double;
  i, j: integer;
begin
  SetLength(weights, 0);
  maxDistance := 0;
  sumDistance := 0;
  avgDistance := 0;
  errorConstraint := 0;
  for i := 0 to MAX_STEPS - 1 do
  begin
    distance := 0;
    for j := 0 to MAX_STEPS - 1 do
    begin
      d := getDistance(steps[i], steps[j]);
      if (d > distance) then
        distance := d;
    end;
    if (distance > 0) then
    begin
      if (distance > maxDistance) then
        maxDistance := distance;
      sumDistance += distance;
      SetLength(weights, Length(weights) + 1);
      weights[Length(weights) - 1] := distance;
    end;
  end;
  if (Length(weights) > 0) then
    avgDistance := sumDistance / Length(weights)
  else
    exit(False);
  errorConstraint := error * avgDistance;
  for i := 0 to Length(weights) - 1 do
  begin
    if (abs(avgDistance - weights[i]) > errorConstraint) then
      exit(False);
  end;
  exit(True);
end;

function TGame.getDistance(p1, p2: TPoint): double;
begin
  exit(sqrt((p2.x - p1.x) * (p2.x - p1.x) + (p2.y - p1.y) * (p2.y - p1.y)));
end;

function TGame.getCenter(p1, p2: TPoint): TPoint;
begin
  exit(Point((p2.x + p1.x) div 2, (p2.y + p1.y) div 2));
end;

procedure TGame.gameLoopTimer(Sender: TObject);
begin
  gameCanvas.DiscardBitmap;
  //if (steps[current_step mod MAX_STEPS] <> cur) then
  //begin
    steps[current_step mod MAX_STEPS] := cur;
    Inc(current_step);
  //end;
end;

procedure TGame.gameCanvasMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  down := True;
end;

procedure TGame.FormCreate(Sender: TObject);
begin
  //Self.WindowState := wsMaximized;
  //Self.BorderStyle := bsNone;
end;

procedure TGame.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Application.Terminate;
end;

procedure TGame.gameCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  cur := Point(trunc(x div size), trunc(y div size));
end;

procedure TGame.gameCanvasMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  down := False;
end;

procedure TGame.gameLoopStartTimer(Sender: TObject);
var
  i: integer;
begin
  current_step := 0;
  last_color := BGRAWhite;
  circles := 0;
  for i := 0 to MAX_STEPS - 1 do
  begin
    steps[i] := Point(0, 0);
  end;
end;

end.
