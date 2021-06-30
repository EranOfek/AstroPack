unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BGRAGraphicControl,
  BGRABitmap, BGRABitmapTypes, BCTypes;

type

  { TfrmSoftUI }

  TfrmSoftUI = class(TForm)
    BGRADropShadowControl: TBGRAGraphicControl;
    BGRAInnerShadowControl: TBGRAGraphicControl;
    BGRAMixedShadow: TBGRAGraphicControl;
    procedure BGRADropShadowControlRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAInnerShadowControlRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAMixedShadowRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
  private
    BGRABackgroundColor: TBGRAPixel;
    BGRADarkShadowColor: TBGRAPixel;
    BGRALightShadowColor: TBGRAPixel;
    BGRATextColor: TBGRAPixel;
    procedure DropShadow(Bitmap: TBGRABitmap; BackgroundColor, DarkShadowColor, LightShadowColor: TBGRAPixel);
    procedure InnerShadow(Bitmap: TBGRABitmap; BackgroundColor, DarkShadowColor, LightShadowColor: TBGRAPixel);
  public

  end;

var
  frmSoftUI: TfrmSoftUI;

implementation

{$R *.lfm}

{ TfrmSoftUI }

procedure TfrmSoftUI.BGRADropShadowControlRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.FillTransparent;
  DropShadow(Bitmap, BGRABackgroundColor, BGRADarkShadowColor, BGRALightShadowColor);

  // TEXT
  Bitmap.TextRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), TControl(Sender).Caption,
    taCenter, tlCenter, BGRATextColor);
end;

procedure TfrmSoftUI.BGRAInnerShadowControlRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.FillTransparent;
  InnerShadow(Bitmap, BGRABackgroundColor, BGRADarkShadowColor, BGRALightShadowColor);

  // TEXT
  Bitmap.TextRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), TControl(Sender).Caption,
    taCenter, tlCenter, BGRATextColor);
end;

procedure TfrmSoftUI.BGRAMixedShadowRedraw(Sender: TObject; Bitmap: TBGRABitmap
  );
begin
  Bitmap.FillTransparent;
  DropShadow(Bitmap, BGRABackgroundColor, BGRADarkShadowColor, BGRALightShadowColor);
  InnerShadow(Bitmap, BGRABackgroundColor, BGRADarkShadowColor, BGRALightShadowColor);

  // TEXT
  Bitmap.TextRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), TControl(Sender).Caption,
    taCenter, tlCenter, BGRATextColor);
end;

procedure TfrmSoftUI.FormCreate(Sender: TObject);
begin
  BGRABackgroundColor := BGRA(236, 240, 243);
  BGRADarkShadowColor := BGRA(209, 217, 230);
  BGRALightShadowColor := BGRAWhite;
  BGRATextColor := BGRA(179, 182, 184);
end;

procedure TfrmSoftUI.DropShadow(Bitmap: TBGRABitmap; BackgroundColor,
  DarkShadowColor, LightShadowColor: TBGRAPixel);
const
  ROUNDING = 20;
  BLUR = 30;
  SHADOW_OFFSET = 16;
  OBJECT_SPACING = SHADOW_OFFSET * 2;
var
  shadow: TBGRABitmap;
begin
  // DARK SHADOW
  shadow := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height, BGRAPixelTransparent);
  // - SHAPE
  shadow.RoundRectAntialias(OBJECT_SPACING, OBJECT_SPACING, Bitmap.Width -
    OBJECT_SPACING, Bitmap.Height - OBJECT_SPACING, ROUNDING, ROUNDING,
    DarkShadowColor, 1, DarkShadowColor);
  // - BLUR
  BGRAReplace(shadow, shadow.FilterBlurRadial(BLUR, rbFast));
  // - DRAW
  Bitmap.PutImage(SHADOW_OFFSET, SHADOW_OFFSET, shadow, dmDrawWithTransparency);
  shadow.Free;

  // LIGHT SHADOW
  shadow := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height, BGRAPixelTransparent);
  // - SHAPE
  shadow.RoundRectAntialias(OBJECT_SPACING, OBJECT_SPACING, Bitmap.Width -
    OBJECT_SPACING, Bitmap.Height - OBJECT_SPACING, ROUNDING, ROUNDING,
    LightShadowColor, 1, LightShadowColor);
  // - BLUR
  BGRAReplace(shadow, shadow.FilterBlurRadial(BLUR, rbFast));
  // - DRAW
  Bitmap.PutImage(-SHADOW_OFFSET, -SHADOW_OFFSET, shadow, dmDrawWithTransparency);
  shadow.Free;

  // SHAPE
  Bitmap.RoundRectAntialias(OBJECT_SPACING, OBJECT_SPACING, Bitmap.Width -
    OBJECT_SPACING, Bitmap.Height - OBJECT_SPACING, ROUNDING, ROUNDING,
    BackgroundColor, 1, BackgroundColor);
end;

procedure TfrmSoftUI.InnerShadow(Bitmap: TBGRABitmap; BackgroundColor,
  DarkShadowColor, LightShadowColor: TBGRAPixel);
const
  ROUNDING = 20;
  BLUR = 30;
  SHADOW_OFFSET = 16;
var
  shadow, shadows, mask: TBGRABitmap;
begin
  shadows := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height, BGRAPixelTransparent);

  // LIGHT SHADOW
  shadow := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height, LightShadowColor);
  mask := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height, BGRABlack);
  // - SHAPE
  mask.RoundRectAntialias(0, 0, Bitmap.Width, Bitmap.Height, ROUNDING, ROUNDING, BGRAWhite, 1, BGRAWhite);
  // - ERASE SHAPE
  shadow.EraseMask(-SHADOW_OFFSET,-SHADOW_OFFSET,mask);
  mask.Free;
  // - DRAW
  shadows.PutImage(0, 0, shadow, dmDrawWithTransparency);
  shadow.Free;

  // DARK SHADOW
  shadow := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height, DarkShadowColor);
  mask := TBGRABitmap.Create(Bitmap.Width, Bitmap.Height, BGRABlack);
  // - SHAPE
  mask.RoundRectAntialias(0, 0, Bitmap.Width, Bitmap.Height, ROUNDING, ROUNDING, BGRAWhite, 1, BGRAWhite);
  // - ERASE SHAPE
  shadow.EraseMask(SHADOW_OFFSET,SHADOW_OFFSET,mask);
  mask.Free;
  // - DRAW
  shadows.PutImage(0, 0, shadow, dmDrawWithTransparency);
  shadow.Free;

  // BLUR
  BGRAReplace(shadows, shadows.FilterBlurRadial(BLUR, rbFast));

  // SHAPE
  Bitmap.RoundRectAntialias(0, 0, Bitmap.Width, Bitmap.Height, ROUNDING, ROUNDING,
    shadows, 1, shadows);

  shadows.Free;
end;

end.
