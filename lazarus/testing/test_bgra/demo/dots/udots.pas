unit udots;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRAGraphics, Math;

type
  TDot = record
    Point: TPoint;
    Size: integer;
  end;

  TDotArray2D = array of array of TDot;

  { TDotDrawer }

  TDotDrawer = class(TComponent)
  private
    FDots: TDotArray2D;
    FDotsColor: TBGRAPixel;
    FDotsX: integer;
    FDotsY: integer;
    FLinesColor: TBGRAPixel;
    FSeed: integer;
    procedure SetFDotsX(AValue: integer);
    procedure SetFDotsY(AValue: integer);
    procedure SetFLinesColor(AValue: TBGRAPixel);
    procedure SetFSeed(AValue: integer);
  private
    FDotsRandPos: integer;
    FDotsSizeMax: integer;
    FDotsSizeMin: integer;
    FDotsSpacing: integer;
    FDrawCrossLines: boolean;
    FDrawHorizLines: boolean;
    FDrawVertlines: boolean;
    FInverseCrossLines: boolean;
    FLinesSize: single;
    FOffSetX: integer;
    FOffSetY: integer;
    procedure SetFDotsColor(AValue: TBGRAPixel);
    procedure SetFDotsRandPos(AValue: integer);
    procedure SetFDotsSizeMax(AValue: integer);
    procedure SetFDotsSizeMin(AValue: integer);
    procedure SetFDotsSpacing(AValue: integer);
    procedure SetFDrawCrossLines(AValue: boolean);
    procedure SetFDrawHorizLines(AValue: boolean);
    procedure SetFDrawVertLines(AValue: boolean);
    procedure SetFInverseCrossLines(AValue: boolean);
    procedure SetFLinesSize(AValue: single);
    procedure SetFOffSetX(AValue: integer);
    procedure SetFOffSetY(AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareData;
    procedure Draw(Bitmap: TBGRABitmap);
  public
    property DotsColor: TBGRAPixel read FDotsColor write SetFDotsColor;
    property LinesColor: TBGRAPixel read FLinesColor write SetFLinesColor;
  published
    property DotsX: integer read FDotsX write SetFDotsX;
    property DotsY: integer read FDotsY write SetFDotsY;
    property DotsSizeMin: integer read FDotsSizeMin write SetFDotsSizeMin;
    property DotsSizeMax: integer read FDotsSizeMax write SetFDotsSizeMax;
    property DotsRandPos: integer read FDotsRandPos write SetFDotsRandPos;
    property DotsSpacing: integer read FDotsSpacing write SetFDotsSpacing;
    property Seed: integer read FSeed write SetFSeed;
    property DrawHorizLines: boolean read FDrawHorizLines write SetFDrawHorizLines;
    property DrawVertLines: boolean read FDrawVertlines write SetFDrawVertLines;
    property DrawCrossLines: boolean read FDrawCrossLines write SetFDrawCrossLines;
    property InverseCrossLines: boolean read FInverseCrossLines
      write SetFInverseCrossLines;
    property LinesSize: single read FLinesSize write SetFLinesSize;
    property OffSetX: integer read FOffSetX write SetFOffSetX;
    property OffSetY: integer read FOffSetY write SetFOffSetY;
  end;

implementation

{ TDotDrawer }

procedure TDotDrawer.SetFDotsColor(AValue: TBGRAPixel);
begin
  if FDotsColor = AValue then
    Exit;
  FDotsColor := AValue;
end;

procedure TDotDrawer.SetFDotsRandPos(AValue: integer);
begin
  if FDotsRandPos = AValue then
    Exit;
  FDotsRandPos := AValue;
end;

procedure TDotDrawer.SetFDotsX(AValue: integer);
begin
  if FDotsX = AValue then
    Exit;
  FDotsX := AValue;
end;

procedure TDotDrawer.SetFDotsY(AValue: integer);
begin
  if FDotsY = AValue then
    Exit;
  FDotsY := AValue;
end;

procedure TDotDrawer.SetFLinesColor(AValue: TBGRAPixel);
begin
  if FLinesColor = AValue then
    Exit;
  FLinesColor := AValue;
end;

procedure TDotDrawer.SetFSeed(AValue: integer);
begin
  if FSeed = AValue then
    Exit;
  FSeed := AValue;
end;

procedure TDotDrawer.PrepareData;
var
  y, x: integer;
begin
  RandSeed := FSeed;
  SetLength(FDots, FDotsY, FDotsX);

  for y := 0 to FDotsY - 1 do
  begin
    for x := 0 to FDotsX - 1 do
    begin
      FDots[y, x].Point.x := FDotsSpacing * x + FOffSetX;
      FDots[y, x].Point.y := FDotsSpacing * y + FOffSetY;
      FDots[y, x].Point.x := RandomRange(FDots[y, x].Point.x - DotsRandPos,
        FDots[y, x].Point.x + DotsRandPos);
      FDots[y, x].Point.y := RandomRange(FDots[y, x].Point.y - DotsRandPos,
        FDots[y, x].Point.y + DotsRandPos);
      FDots[y, x].Size := RandomRange(FDotsSizeMin, FDotsSizeMax + 1);
    end;
  end;
end;

procedure TDotDrawer.SetFDotsSizeMax(AValue: integer);
begin
  if FDotsSizeMax = AValue then
    Exit;
  FDotsSizeMax := AValue;
end;

procedure TDotDrawer.SetFDotsSizeMin(AValue: integer);
begin
  if FDotsSizeMin = AValue then
    Exit;
  FDotsSizeMin := AValue;
end;

procedure TDotDrawer.SetFDotsSpacing(AValue: integer);
begin
  if FDotsSpacing = AValue then
    Exit;
  FDotsSpacing := AValue;
end;

procedure TDotDrawer.SetFDrawCrossLines(AValue: boolean);
begin
  if FDrawCrossLines = AValue then
    Exit;
  FDrawCrossLines := AValue;
end;

procedure TDotDrawer.SetFDrawHorizLines(AValue: boolean);
begin
  if FDrawHorizLines = AValue then
    Exit;
  FDrawHorizLines := AValue;
end;

procedure TDotDrawer.SetFDrawVertLines(AValue: boolean);
begin
  if FDrawVertlines = AValue then
    Exit;
  FDrawVertlines := AValue;
end;

procedure TDotDrawer.SetFInverseCrossLines(AValue: boolean);
begin
  if FInverseCrossLines = AValue then
    Exit;
  FInverseCrossLines := AValue;
end;

procedure TDotDrawer.SetFLinesSize(AValue: single);
begin
  if FLinesSize = AValue then
    Exit;
  FLinesSize := AValue;
end;

procedure TDotDrawer.SetFOffSetX(AValue: integer);
begin
  if FOffSetX=AValue then Exit;
  FOffSetX:=AValue;
end;

procedure TDotDrawer.SetFOffSetY(AValue: integer);
begin
  if FOffSetY=AValue then Exit;
  FOffSetY:=AValue;
end;

constructor TDotDrawer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOffSetX := 0;
  FOffSetY := 0;
  FSeed := 1;
  FDotsX := 10;
  FDotsY := 10;
  { Dots }
  FDotsColor := BGRABlack;
  FDotsSizeMin := 4;
  FDotsSizeMax := 10;
  FDotsSpacing := 40;
  FDotsRandPos := 5;
  { Lines }
  FLinesColor := BGRABlack;
  FLinesSize := 1.5;
  FDrawCrossLines := True;
  FDrawHorizLines := True;
  FDrawVertLines := True;
  FInverseCrossLines := False;
  { Initialize }
  PrepareData;
end;

destructor TDotDrawer.Destroy;
begin
  inherited Destroy;
end;

procedure TDotDrawer.Draw(Bitmap: TBGRABitmap);
var
  y, x: integer;
begin
  { Horizontal Lines }
  if DrawHorizLines then
    for y := 0 to FDotsY - 1 do
    begin
      for x := 0 to FDotsX - 2 do
      begin
        Bitmap.DrawLineAntialias(
          FDots[y, x].Point.x, FDots[y, x].Point.y,
          FDots[y, x + 1].Point.x, FDots[y, x + 1].Point.y, FLinesColor, FLinesSize);
      end;
    end;

  { Vertical Lines }
  if DrawVertLines then
    for y := 0 to FDotsY - 2 do
    begin
      for x := 0 to FDotsX - 1 do
      begin
        Bitmap.DrawLineAntialias(
          FDots[y, x].Point.x, FDots[y, x].Point.y,
          FDots[y + 1, x].Point.x, FDots[y + 1, x].Point.y, FLinesColor, FLinesSize);
      end;
    end;

  { Diagonal Lines 1 }
  if not FInverseCrossLines then
  begin
    for y := 0 to FDotsY - 2 do
    begin
      for x := 0 to FDotsX - 2 do
      begin
        Bitmap.DrawLineAntialias(
          FDots[y, x].Point.x, FDots[y, x].Point.y,
          FDots[y + 1, x + 1].Point.x, FDots[y + 1, x + 1].Point.y,
          FLinesColor, FLinesSize);
      end;
    end;
  end
  { Diagonal Lines 2 }
  else
  begin
    for y := 0 to FDotsY - 2 do
    begin
      for x := 0 to FDotsX - 2 do
      begin
        Bitmap.DrawLineAntialias(
          FDots[y, x + 1].Point.x, FDots[y, x + 1].Point.y,
          FDots[y + 1, x].Point.x, FDots[y + 1, x].Point.y, FLinesColor, FLinesSize);
      end;
    end;
  end;

  { Dots }
  for y := 0 to FDotsY - 1 do
  begin
    for x := 0 to FDotsX - 1 do
    begin
      Bitmap.EllipseAntialias(FDots[y, x].Point.x, FDots[y, x].Point.y,
        0, 0, FDotsColor, FDots[y, x].Size, FDotsColor);
    end;
  end;
end;

end.
