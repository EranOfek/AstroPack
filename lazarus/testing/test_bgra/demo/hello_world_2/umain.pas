unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRAGraphicControl, BGRABitmap, BCTypes, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1StartTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    step: double;
    time: double;
    image: TBGRABitmap;
    function easing(t: double): double;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  time := time + step;
  if time < 0 then
    time := 0
  else if time > 1 then
    time := 1;
  BGRAVirtualScreen1.DiscardBitmap;
  if Time = 1 then
  begin
    Timer1.Enabled := False;
  end;
end;

function TForm1.easing(t: double): double;
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
end;

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  temp: double;
  w, h: single;
  cx, cy: single;
  alpha: byte;
begin
  temp := easing(time);
  alpha := Round(255 * temp);
  cx := Bitmap.Width div 2;
  cy := Bitmap.Height div 2;
  w := cx * temp;
  h := cy * temp;
  //Bitmap.Rectangle(Round(cx - w), Round(cy - h), Round(cx + w), Round(cy + h), BGRA(255, 255, 255, alpha), BGRA(255, 255, 255, alpha), dmDrawWithTransparency);
  Bitmap.StretchPutImage(Rect(Round(cx - w), Round(cy - h), Round(cx + w), Round(cy + h)), image, dmDrawWithTransparency, alpha);
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  time := 0;
  Timer1.Enabled := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  step := 0.08;
  image := TBGRABitmap.Create(Application.Location + 'sunflower_800x600.jpg');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.Free;
end;

procedure TForm1.Timer1StartTimer(Sender: TObject);
begin
  time := 0;
end;

end.

