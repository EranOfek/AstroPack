unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRABitmap, Math, BGRABitmapTypes, LCLType, BGRAOpenGL, BGLVirtualScreen;

const
  MAX_REFRESH_RATE = 80;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    procedure FormMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure FormMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    vs: TBGLVirtualScreen;
    sand: TBGLBitmap;
    size: integer;
    md: boolean;
    m: TPoint;
    hsla: double;
    h: integer;
    w: integer;
    scaleX: double;
    scaleY: double;
    elapseAcc: integer;
    procedure putSand(const x: integer; const y: integer);
    procedure updateSand;
    procedure updateScale;
    procedure VSElapse(Sender: TObject; BGLContext: TBGLContext;
      ElapsedMs: integer);
    procedure VSRedraw(Sender: TObject; BGLContext: TBGLContext);
    procedure VSUnload(Sender: TObject; BGLContext: TBGLContext);
  public

  end;

var
  frmMain: TfrmMain;

implementation

var
  randomBits: longint;
  randomBitCount: integer;

function nextRandomBit: boolean; inline;
begin
  if randomBitCount = 0 then
  begin
    randomBits := Random(1 shl 30);
    randomBitCount := 30;
  end;
  result := (randomBits and 1) <> 0;
  dec(randomBitCount);
  randomBits := randomBits shr 1;
end;

function isSand(pix : TBGRAPixel): boolean; inline;
begin
  result := (PDWord(@pix)^ <> PDWord(@BGRAWhite)^) and
    (PDWord(@pix)^ <> PDWord(@BGRABlack)^);
end;

function isSpace(pix : TBGRAPixel): boolean; inline;
begin
  result := (PDWord(@pix)^ = PDWord(@BGRAWhite)^);
end;

procedure movePix(psrc, pdest: PBGRAPixel); inline;
begin
  pdest^ := psrc^;
  psrc^ := BGRAWhite;
end;

procedure sandMovement(ppix: PBGRAPixel; rowDelta: PtrInt);
var
  bottomLeft, bottomRight, bottom: PBGRAPixel;
begin
  bottomLeft := ppix + rowDelta - 1;
  bottomRight := ppix + rowDelta + 1;
  if isSpace(bottomLeft^) then
  begin
    if isSpace(bottomRight^) then
    begin
      if nextRandomBit then
        movePix(ppix, bottomRight)
      else
        movePix(ppix, bottomLeft);
    end
      else
        movePix(ppix, bottomLeft);
  end
  else if isSpace(bottomRight^) then
    movePix(ppix, bottomRight)
  else
  begin
    bottom := ppix + rowDelta;
    if isSpace(bottom^) then
       movePix(ppix, bottom);
  end;
end;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  m := Point(round(X * scaleX), round(Y * scaleY));
end;

procedure TfrmMain.FormMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  md := False;
end;

procedure TfrmMain.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  md := True;
  m := Point(round(X * scaleX), round(Y * scaleY));
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  WindowState := wsFullScreen;
  md := False;
  m := Point(0, 0);
  hsla := randomRange(0, MaxWord);
  vs := TBGLVirtualScreen.Create(self);
  vs.Align := alClient;
  InsertControl(vs);
  vs.OnMouseMove:= @FormMouseMove;
  vs.OnMouseDown:= @FormMouseDown;
  vs.OnMouseUp := @FormMouseUp;
  vs.OnRedraw:=@VSRedraw;
  vs.OnUnloadTextures:=@VSUnload;
  vs.RedrawOnIdle:= true;
  vs.OnKeyDown:= @FormKeyDown;
  vs.OnElapse:=@VSElapse;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    vs.RedrawOnIdle:= false;
    Close;
    Key := 0;
  end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  UpdateScale;
end;

procedure TfrmMain.putSand(const x: integer; const y: integer);
var
  p: THSLAPixel;
  underneath: TBGRAPixel;
begin
  underneath := sand.GetPixel(x, y);
  if isSpace(underneath) then
  begin
    p.hue := round(hsla);
    p.saturation := randomRange(32768, 65535 - (32768 div 2));
    p.lightness := 32768;
    p.alpha := 65535;
    sand.SetPixel(x, y, p.ToBGRAPixel);
    hsla := hsla + 1 / size;
    if (hsla > 65535) then
      hsla := 0;
  end;
end;

procedure TfrmMain.updateSand;
var
  y: integer = 0;
  x: integer = 0;
  pscan: PBGRAPixel;
  rowDelta: PtrInt;
  hsize: integer;
begin
  if sand = nil then
  begin
    sand := TBGLBitmap.Create(ClientWidth div 2, ClientHeight div 2, BGRAWhite);
    sand.Rectangle(sand.ClipRect, BGRABlack);
    size := round(sand.Width / 24);
    h := sand.Height;
    w := sand.Width;
    updateScale;
  end;

  if (md) then
    for y := m.y - size to m.y + size do
    begin
      hsize := round(sqrt(sqr(size) - sqr(y - m.y)));
      for x := m.x - hsize to m.x + hsize do
        if (y > 0) and (y < h) and (x > 0) and (x < w) then
          if nextRandomBit then
            putSand(x, y);
    end;

  rowDelta := sand.AllocatedWidth;
  if sand.LineOrder = riloBottomToTop then rowDelta := -rowDelta;

  for y := h - 1 downto 0 do
  begin
    pscan := sand.ScanLine[y];
    if odd(y) then
    begin
      for x := 0 to w - 1 do
      begin
        if isSand(pscan^) then
           sandMovement(pscan, rowDelta);
        inc(pscan);
      end;
    end else
    begin
      pscan += w;
      for x := w - 1 downto 0 do
      begin
        dec(pscan);
        if isSand(pscan^) then
           sandMovement(pscan, rowDelta);
      end;
    end;
  end;
  sand.InvalidateBitmap;
end;

procedure TfrmMain.updateScale;
begin
  scaleX := w / ClientWidth;
  scaleY := h / ClientHeight;
end;

procedure TfrmMain.VSElapse(Sender: TObject; BGLContext: TBGLContext;
  ElapsedMs: integer);
var maxTime: TDateTime;
begin
  inc(elapseAcc, ElapsedMs);
  maxTime := Now + (30/(24*60*60*1000));
  while elapseAcc > 1000 div MAX_REFRESH_RATE do
  begin
    if Now >= maxTime then // not enough computing power
    begin
      elapseAcc := 0;
      break;
    end;
    updateSand;
    dec(elapseAcc, 10);
  end;
end;

procedure TfrmMain.VSRedraw(Sender: TObject; BGLContext: TBGLContext);
var
  sandTex: IBGLTexture;
begin
  if sand = nil then
    updateSand;
  sandTex := sand.Texture;
  sandTex.ResampleFilter:= orfBox;
  BGLContext.Canvas.StretchPutImage(0,0,BGLContext.Width,BGLContext.Height, sandTex);
end;

procedure TfrmMain.VSUnload(Sender: TObject; BGLContext: TBGLContext);
begin
  FreeAndNil(sand);
end;

end.
