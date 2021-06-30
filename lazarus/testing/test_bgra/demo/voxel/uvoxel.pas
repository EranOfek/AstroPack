unit uvoxel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes;

type

  { TVoxelMap }

  TVoxelMap = class
  protected
    FHeightMap: TBGRABitmap;
    FColorMaps, FWallTexs, FColorMapsDownsample: array[1..4] of TBGRABitmap;
    FOrigin: TPoint3D;
    FUnit: TPoint3D;
    FFlatRadius: array of array of integer;
    procedure ComputeFlatRadius;
    procedure WallVertLine(ADest: TBGRABitmap; x, y1, y2: integer; texX,texY1,texY2: integer; tex: TBGRABitmap);
    function FindInterWithRect(AStart: TPointF; ADir: TPointF; ARectOrigin: TPointF; ARectAxis1, ARectAxis2: TPointF): TPointF;

  public
    constructor Create(AHeightMap, AColorMap1,AColorMap2,AColorMap3,AColorMap4,
                 AWallTex1,AWallTex2,AWallTex3,AWallTex4: TBGRABitmap; AOrigin: TPoint3D; AUnit: TPoint3D);
    procedure Render(ADest: TBGRABitmap; AViewPoint: TPoint3D; AViewDir: TPoint3D; AViewAngleXDeg: Single);
    destructor Destroy; override;
  end;

implementation

uses BGRAMatrix3D, Math;

{ TVoxelMap }

procedure TVoxelMap.ComputeFlatRadius;
const maxFlatRadius = 4;
var
  row: array of integer;
  y, x, xb, yb, i: Integer;
  curFlatRadius: integer;
  centerValue: byte;
  curDist: single;
  curRadius: integer;
  otherPix: TBGRAPixel;
begin
  setlength(FFlatRadius, FHeightMap.Height, FHeightMap.Width);
  for y := 0 to FHeightMap.Height-1 do
  begin
    row := FFlatRadius[y];
    for x := 0 to FHeightMap.Width-1 do
    begin
      curFlatRadius := maxFlatRadius;
      centerValue := FHeightMap.GetPixel(x,y).green;
      for xb := -maxFlatRadius to maxFlatRadius do
        for yb := -maxFlatRadius to maxFlatRadius do
        begin
          curDist := abs(xb*FUnit.x)+abs(yb*FUnit.z);
          if curDist = 0 then continue;
          otherPix := FHeightMap.GetPixel(x+xb,y+yb);
          if (otherPix.alpha < 255) or ((otherPix.green - centerValue)*FUnit.y / curDist > 0.25) then
          begin
            curRadius := max(abs(xb),abs(yb));
            if curRadius < curFlatRadius then curFlatRadius:= curRadius;
          end;
        end;
      row[x] := curFlatRadius;
    end;
  end;
end;

procedure TVoxelMap.WallVertLine(ADest: TBGRABitmap; x, y1, y2: integer; texX,
  texY1, texY2: integer; tex: TBGRABitmap);
var yb, texY, texDY, texYStep, texYAcc, texYAccAdd, texYMod, dy: integer;
  pdest: PBGRAPixel; stride: PtrInt;
begin
  if (y2 < y1) or (x < 0) or (x > ADest.Width) then exit;
  texDY := texY2-texY1+1;
  dy := y2-y1+1;
  texYStep := texDY div dy;
  texYAccAdd := texDY - (texYStep*dy);
  if texYAccAdd < 0 then
  begin
    texYAccAdd += dy;
    texYStep -= 1;
  end;
  texYStep := PositiveMod(texYStep, tex.Height);
  texYMod := dy;
  texY := texY1;
  texYAcc := 0;
  if y1 < 0 then
  begin
    texY += texYStep*(0-y1);
    texYAcc += texYAccAdd*(0-y1);
    texY += texYAcc div texYMod;
    texYAcc := texYAcc mod texYMod;
    y1 := 0;
  end;
  if y2 > ADest.Height-1 then y2 := ADest.Height-1;
  texX := PositiveMod(texX, tex.Width);
  texY := PositiveMod(texY, tex.Height);

  pdest := ADest.ScanLine[y1]+x;
  if ADest.LineOrder = riloTopToBottom then
    stride := ADest.Width*4 else stride := -ADest.Width*4;
  for yb := y1 to y2 do
  begin
    pdest^ := tex.GetPixel(texX, texY);
    inc(PByte(pdest),stride);

    texY += texYStep;
    texYAcc += texYAccAdd;
    if texYAcc > texYMod then
    begin
      texYAcc -= texYMod;
      texY += 1;
    end;
    if texY >= tex.Height then texY -= tex.Height;
  end;
  ADest.InvalidateBitmap;
end;

function TVoxelMap.FindInterWithRect(AStart: TPointF; ADir: TPointF;
  ARectOrigin: TPointF; ARectAxis1, ARectAxis2: TPointF): TPointF;
var
  side: array[1..4] of TLineDef;
  inter: array[1..4] of TPointF;
  source: TLineDef;
  i: Integer;
  sideLen,posInSeg: Single;
  closest: integer;
begin
  source.origin := AStart;
  source.dir := ADir;
  source.dir := (1/VectLen(source.dir))*source.dir;
  side[1].origin := ARectOrigin;
  side[3].origin := ARectAxis1+ (ARectAxis2-ARectOrigin);
  side[2].origin := ARectAxis1;
  side[4].origin := ARectAxis2;
  closest := -1;
  for i := 1 to 4 do
  begin
    side[i].dir := side[(i mod 4)+1].origin-side[i].origin;
    sideLen := VectLen(side[i].dir);
    if sideLen <> 0 then side[i].dir := (1/sideLen)*side[i].dir;
    inter[i] := IntersectLine(source, side[i]);
    if not isEmptyPointF(inter[i]) and ((inter[i]-source.origin)*source.dir > 0) then
    begin
      posInSeg := side[i].dir*(inter[i]-side[i].origin);
      if (posInSeg >= 0) and (posInSeg <= sideLen) then
      begin
        if (closest = -1) or (VectLen(inter[i]-AStart) < VectLen(inter[closest]-AStart)) then
          closest := i;
      end;
    end;
  end;

  if closest = -1 then
    exit(EmptyPointF)
  else
    exit(inter[closest]);
end;

constructor TVoxelMap.Create(AHeightMap, AColorMap1,AColorMap2,AColorMap3,AColorMap4, AWallTex1,AWallTex2,AWallTex3,AWallTex4: TBGRABitmap; AOrigin: TPoint3D;
  AUnit: TPoint3D);
var
  i: Integer;
begin
  FHeightMap := AHeightMap;
  FColorMaps[1] := AColorMap1;
  FColorMaps[2] := AColorMap2;
  FColorMaps[3] := AColorMap3;
  FColorMaps[4] := AColorMap4;
  FWallTexs[1] := AWallTex1;
  FWallTexs[2] := AWallTex2;
  FWallTexs[3] := AWallTex3;
  FWallTexs[4] := AWallTex4;
  for i := 1 to 4 do
    FColorMapsDownsample[i] := FColorMaps[i].Resample(FColorMaps[i].Width div 4, FColorMaps[i].Height div 4) as TBGRABitmap;
  FOrigin := AOrigin;
  FUnit:= AUnit;
  ComputeFlatRadius;
end;

procedure TVoxelMap.Render(ADest: TBGRABitmap; AViewPoint: TPoint3D;
  AViewDir: TPoint3D; AViewAngleXDeg: Single);
const
  precisionShift = 16;
  precision = 1 shl precisionShift;
var
  centerPos3D: TPoint3D;
  centerPos2D: TPoint;
  curViewDir: TPoint3D;
  angleOfs: single;
  curAngle, h, zoomY, curAltitude: single;
  curPos3D: TPoint3D;
  iteration,x, y, prevY: integer;
  dist: single;
  heightPix, color, prevHeightPix: TBGRAPixel;
  minY,maxY,y1,y2: integer;

  mapX,mapY, mapXFrac, mapYFrac: integer;
  mapStepX,mapStepY, mapStepXFrac, mapStepYFrac: integer;

  speed, maxSpeed, prevH: single;
  flatRadius, i: integer;
  height2, iMapX, iMapY: integer;
  colorMap,wallTex,colorMapDownSample: TBGRABitmap;
  curViewDirLen,curU: single;
  curPos,u, inter, start2D: TPointF;
  minMaxInit: Boolean;

  procedure GoForward;
  begin
    mapX += mapStepX;
    mapXFrac += mapStepXFrac;
    if mapXFrac >= precision then
    begin
      mapXFrac -= precision;
      inc(mapX);
    end;

    mapY += mapStepY;
    mapYFrac += mapStepYFrac;
    if mapYFrac >= precision then
    begin
      mapYFrac -= precision;
      inc(mapY);
    end;

    curAltitude += curViewDir.y;
    dist += speed;
  end;

  procedure GoBackwards;
  begin
    mapX -= mapStepX;
    mapXFrac -= mapStepXFrac;
    if mapXFrac < 0 then
    begin
      mapXFrac += precision;
      dec(mapX);
    end;

    mapY -= mapStepY;
    mapYFrac -= mapStepYFrac;
    if mapYFrac < 0 then
    begin
      mapYFrac += precision;
      dec(mapY);
    end;

    curAltitude -= curViewDir.y;
    dist -= speed;
  end;

begin
  if (ADest.Width = 0) or (ADest.Height = 0) then exit;

  centerPos3D := AViewPoint - FOrigin;
  centerPos2D := Point(ADest.Width div 2, ADest.Height div 2);
  angleOfs := AViewAngleXDeg*Pi/180 / ADest.Width;
  curAngle := -angleOfs*centerPos2D.x;
  zoomY := ADest.Width/sin(AViewAngleXDeg*Pi/180/2);
  maxSpeed := min(abs(FUnit.x),abs(FUnit.z));

  for x := 0 to ADest.Width-1 do
  begin
//    curViewDir := MatrixRotateY(curAngle)*AViewDir;
    curViewDir := AViewDir + MatrixRotateY(Pi/2)*AViewDir*sin(curAngle);
    curViewDir.x *= FUnit.x;
    curViewDir.y *= FUnit.y;
    curViewDir.z *= FUnit.z;
    curViewDirLen := VectLen3D(curViewDir);
    u := (1/curViewDirLen)*PointF(curViewDir.x,curViewDir.z);
    speed := 1;

    dist := 0;
    flatRadius := 1;
    curPos3D := centerPos3D;
    if (curPos3D.x < 0) or (curPos3D.z < 0) or (CurPos3D.x > FHeightMap.Width)
      or (CurPos3D.z > FHeightMap.Height) then
    begin
      start2D := PointF(curPos3D.x,curPos3D.z);
      inter := FindInterWithRect(start2D, PointF(curViewDir.x,curViewDir.z),
                        PointF(0,0), PointF(FHeightMap.Width-1,0), PointF(0,FHeightMap.Height-1) );
      if not isEmptyPointF(inter) then
      begin
        curPos3D.x:= inter.x;
        curPos3D.z := inter.y;
        dist := VectLen(inter-start2D)/VectLen(PointF(curViewDir.x,curViewDir.z));
        flatRadius := 0;
      end;
    end;

    mapX := floor(curPos3D.x);
    mapXFrac := trunc((curPos3D.x-mapX)*precision);

    mapY := floor(curPos3D.z);
    mapYFrac := trunc((curPos3D.z-mapY)*precision);

    mapStepX := floor(curViewDir.x);
    mapStepXFrac := trunc((curViewDir.x-mapStepX)*precision);
    mapStepY := floor(curViewDir.z);
    mapStepYFrac := trunc((curViewDir.z-mapStepY)*precision);

    curAltitude := curPos3D.y;
    h := EmptySingle;

    repeat
      prevH := h;

      for i := 1 to flatRadius do GoForward;

      heightPix := FHeightMap.GetPixel(mapX+integer(mapXFrac>precision shr 1),
                                       mapY+integer(mapYFrac>precision shr 1));
      if heightPix.alpha < 255 then break;

      h := (heightPix.green*FUnit.y-curAltitude)/dist*zoomY;
      if h > - (ADest.Height shr 1) then break;

      flatRadius := FFlatRadius[mapY+integer(mapYFrac>precision shr 1),
                               mapX+integer(mapXFrac>precision shr 1)];
    until false;

    if dist > speed then for i := 1 to flatRadius do GoBackwards;

    if prevH <> EmptySingle then
    begin
      y := round(centerPos2D.y - prevH);
      minY := y;
      maxY := y;
      minMaxInit := true;
    end else
      minMaxInit := false;
    iteration := 1;

    repeat
      prevY := y;

      prevHeightPix := heightPix;
      iMapX := mapX+integer(mapXFrac>precision shr 1);
      iMapY := mapY+integer(mapYFrac>precision shr 1);
      heightPix := FHeightMap.GetPixel(iMapX,iMapY);
      if heightPix.alpha < 255 then break;

      flatRadius := FFlatRadius[iMapY,iMapX];

      colorMap := FColorMaps[(heightPix.blue shr 6) + 1];
      colorMapDownsample := FColorMapsDownsample[(heightPix.blue shr 6) + 1];
      wallTex := FWallTexs[(heightPix.red shr 6) + 1];

      h := (heightPix.green*FUnit.y-curAltitude)/dist*zoomY;
      y := round(centerPos2D.y - h);

      if (iteration = 1) or (y <> prevY) then
      begin
        if not minMaxInit then
        begin
          minY := y;
          maxY := y;
          y1 := y;
          y2 := y;
          minMaxInit:= true;
        end else
        if prevY < y then
        begin
          y1 := prevY+1;
          y2 := y;
        end else
        begin
          y1 := y;
          y2 := prevY-1;
        end;

        if (y2 >= 0) and (y1 < ADest.Height) then
        begin
          if y1 < minY then
          begin
            if (abs(prevHeightPix.green - heightPix.green) > 5) then
            begin
              height2 := round(((centerPos2D.y - (minY-0.5))*dist/zoomY + curAltitude)/FUnit.y*10*FUnit.y);
              curPos := PointF(mapX + mapXFrac/precision, mapY + mapYFrac/precision);
              curU := PointF(curPos.x-iMapX, curPos.y-iMapY)*u;
              curPos -= curU * u;

              WallVertLine(ADest, x, y1, minY-1, round(curPos.x*4)+round(curPos.y*4),
                                  round(heightPix.green*10*FUnit.y), height2, wallTex);
              minY := y1;
            end else
            begin
              if iteration > 170 then
                color := colorMapDownSample.GetPixelCycle((mapX+integer(mapStepX>precision shr 1)) shr 2,
                                         (mapY+integer(mapStepX>precision shr 1)) shr 2)
              else
              if (iteration < 80) or (iteration > 140) then
                color := colorMap.GetPixelCycle256(mapX,mapY,
                            mapXFrac shr (precisionShift - 8),mapYFrac shr (precisionShift - 8),
                            rfLinear)
              else
                color := colorMap.GetPixelCycle(mapX+integer(mapStepX>precision shr 1),
                                           mapY+integer(mapStepX>precision shr 1));

              ADest.VertLine(x, y1, minY-1, color, dmSet);
              minY := y1;
              { else
              if y2 > maxY then
              begin
                ADest.VertLine(x, maxY+1, y2, color, dmSet);
                maxY := y2;
              end};
            end;
          end;
        end;
      end;

      if iteration > 140 then
      begin
        for i := 1 to flatRadius do GoForward;
      end else
        GoForward;

      inc(iteration);

{      if (iteration = 30) or (iteration = 50) then
      begin
        if speed*2 < maxSpeed*1.001 then
        begin
          speed *= 2;
          mapStepX *= 2;
          mapStepY *= 2;
          mapStepXFrac *= 2;
          mapStepYFrac *= 2;

          if mapStepXFrac > precision then
          begin
            mapStepXFrac -= precision;
            mapStepX += 1;
          end;
          if mapStepYFrac > precision then
          begin
            mapStepYFrac -= precision;
            mapStepY += 1;
          end;
        end;
      end;}
    until minY <= 0;

    curAngle += angleOfs;
  end;

end;

destructor TVoxelMap.Destroy;
var
  i: Integer;
begin
  FHeightMap.Free;
  for i := low(FColorMaps) to high(FColorMaps) do
    FColorMaps[i].Free;
  for i := low(FColorMapsDownsample) to high(FColorMapsDownsample) do
    FColorMapsDownsample[i].Free;
  for i := low(FWallTexs) to high(FWallTexs) do
    FWallTexs[i].Free;
  inherited Destroy;
end;

end.

