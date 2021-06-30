unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BGRAGradients, uvoxel;

const
  StandingHeight = 16;
  PixelSize = 2;
  PlayerRadius = 20;

type

  { TFMain }

  TFMain = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    FVoxelMap: TVoxelMap;
    FViewPoint: TPoint3D;
    dy: single;
    FViewDir: TPoint3D;
    FOrig,FUnit: TPoint3D;
    FMap: TBGRABitmap;
    FKeys: array[0..255] of boolean;
    FLastIdle: TDateTime;
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    procedure TryMove(ADir: TPoint3D);
    function GetGroundLevelArea(APos: TPoint3D): single;
    function GetGroundLevel(APos: TPoint3D): single;
  public
    { public declarations }
  end;

var
  FMain: TFMain;

implementation

uses BGRAMatrix3D, LCLType;

{$R *.lfm}

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
var
  resPath: string;
begin
  resPath := {$IFDEF DARWIN}ExtractFilePath(Application.ExeName)+'../../../'{$ELSE}''{$ENDIF};
  FMap := TBGRABitmap.Create(resPath+'map.png');
  FUnit := Point3D(1,1/3,1);
  FOrig := Point3D((-FMap.Width div 2)*FUnit.x,-128*FUnit.y,(-FMap.Height div 2)*FUnit.z);
  FVoxelMap := TVoxelMap.Create(FMap,
                    TBGRABitmap.Create(resPath+'marble.png'), TBGRABitmap.Create(resPath+'wooden_floor.png'), TBGRABitmap.Create(resPath+'stone.png'), TBGRABitmap.Create(resPath+'water.png'),
                    TBGRABitmap.Create(resPath+'wall.png'),  TBGRABitmap.Create(resPath+'wooden_wall.png'),  TBGRABitmap.Create(resPath+'stone.png'), TBGRABitmap.Create(resPath+'marble.png'),
                    FOrig, FUnit);
  FViewPoint := Point3D(0,0,0);
  FViewPoint.y := GetGroundLevel(FViewPoint)+ StandingHeight;
  FViewDir := Point3D(0,0,1);
  fillchar(FKeys, sizeof(FKeys), 0);
  Application.OnIdle := @AppOnIdle;
  FLastIdle := Now;
  dy := 0;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  FVoxelMap.Free;
end;

procedure TFMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <= high(FKeys) then
  begin
    FKeys[Key] := true;
    Key := 0;
  end;
end;

procedure TFMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <= high(FKeys) then
  begin
    FKeys[Key] := false;
    Key := 0;
  end;
end;

procedure TFMain.AppOnIdle(Sender: TObject; var Done: Boolean);
const millisecPerDay = 24*60*60*1000;
  timeGrain = 10;

var newIdle: TDateTime;
  elapsedMs: double;
  groundY, headY: single;
  jump,crouch: boolean;
begin
  newIdle := Now;
  elapsedMs := (newIdle - FLastIdle)*millisecPerDay;
  if elapsedMs > 1000 then
  begin
    elapsedMs := 1000;
    FLastIdle := newIdle-elapsedMs/millisecPerDay;
  end;
  while elapsedMs >= timeGrain do
  begin
    jump := false;
    crouch := false;
    if FKeys[VK_RIGHT] then FViewDir := MatrixRotateY(1*Pi/180)*FViewDir;
    if FKeys[VK_LEFT] then FViewDir := MatrixRotateY(-1*Pi/180)*FViewDir;
    if FKeys[VK_UP] then TryMove(FViewDir*2);
    if FKeys[VK_DOWN] then TryMove(FViewDir*(-2));
    if FKeys[VK_PRIOR] then jump := true;
    if FKeys[VK_NEXT] then crouch := true;

    groundY := GetGroundLevelArea(FViewPoint);
    if groundY <> EmptySingle then
    begin
      if crouch then headY := StandingHeight/2 else headY := StandingHeight;
      dy := dy - 0.2;
      FViewPoint.y += dy;
      if FViewPoint.y < groundY + headY then
      begin
        dy := 0;
        FViewPoint.y := groundY + headY;
        if jump then dy := 3;
      end;
    end;

    elapsedMs -= timeGrain;
    FLastIdle += timeGrain/millisecPerDay;
  end;

  BGRAVirtualScreen1.RedrawBitmap;
  Done := false;
end;

procedure TFMain.TryMove(ADir: TPoint3D);
var
  curY, compY: Single;
begin
  curY := FViewPoint.y-StandingHeight;
  compY := GetGroundLevelArea(FViewPoint+ADir);
  if (curY = EmptySingle) or (compY = EmptySingle) or (compY < curY+StandingHeight) then
    FViewPoint += ADir;
end;

function TFMain.GetGroundLevelArea(APos: TPoint3D): single;
var r,angle: integer;
  compY: Single;
begin
  result := GetGroundLevel(APos);
  for r := 1 to PlayerRadius do
    for angle := 1 to 8 do
    begin
      compY := GetGroundLevel(APos+Point3D(cos(angle*Pi/4)*r,0,sin(angle*Pi/4)*r));
      if (compY <> EmptySingle) and ((result = EmptySingle) or (compY > result)) then result := compY;
    end;
end;

function TFMain.GetGroundLevel(APos: TPoint3D): single;
var
  groundLevel: TBGRAPixel;
begin
  with (APos - FOrig) do
  groundLevel := FMap.GetPixel(x,z);
  if groundLevel.alpha = 255 then
    result := groundLevel.green*FUnit.y + FOrig.y
  else
    result := EmptySingle;
end;

procedure TFMain.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var reduced: TBGRABitmap;
begin
  if (Bitmap.Width > PixelSize) and (Bitmap.Height > PixelSize) then
  begin
    reduced := TBGRABitmap.Create(round(Bitmap.Width/PixelSize), round(Bitmap.Height/PixelSize));
    reduced.GradientFill(0,0,reduced.Width,reduced.Height, CSSSkyBlue,CSSBlue, gtLinear, PointF(0,0),PointF(0,reduced.Height),dmSet);
    FVoxelMap.Render(reduced, FViewPoint, FViewDir, 90);
    Bitmap.StretchPutImage(rect(0,0,round(reduced.Width*PixelSize), round(reduced.Height*PixelSize)),reduced,dmSet);
    reduced.Free;
  end;
end;

end.

