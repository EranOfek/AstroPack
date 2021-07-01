unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  BGRAVirtualScreen, BGRABitmap, BCTypes, BGRABitmapTypes,
  BGRAGradients;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    BGRAVirtualScreen2: TBGRAVirtualScreen;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAVirtualScreen2Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    bmp: TBGRABitmap;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.PutImage(0,0,bmp,dmSet);
end;

procedure TForm1.BGRAVirtualScreen2Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  blur: TBGRABitmap;
  mask: TBGRABitmap;
  phong: TPhongShading;
begin
  mask := bmp.FilterGrayscale as TBGRABitmap;

  blur := mask.FilterBlurRadial(5,5,rbFast) as TBGRABitmap;

  blur.FillMask(0,0,mask,BGRAPixelTransparent,dmSet);

  phong := TPhongShading.Create;
  phong.Draw(Bitmap, blur, 2, 0,0,bmp);

  phong.Free;
  blur.Free;
  mask.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bmp := TBGRABitmap.Create(Application.Location + 'image.png');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
end;

end.

