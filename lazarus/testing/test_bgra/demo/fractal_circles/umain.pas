unit umain;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes;

const
  ColorStep = 40;
  FractalDepth = 7;
  ColorGamma = 0.7;

type

  { TForm1 }

  TProp = record
    thecolor: integer;
    x, y: single;
  end;

  { TData }

  TData = record
    data: array of TProp;
    procedure Push(aData: TProp);
    function Pop: TProp;
  end;

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    zoom: single;
  public

  end;

var
  Form1: TForm1;

implementation

uses math;

{$R *.lfm}

{ TData }

procedure TData.Push(aData: TProp);
begin
  SetLength(data, Length(data)+1);
  data[Length(data)-1] := aData;
end;

function TData.Pop: TProp;
begin
  Result := data[Length(data)-1];
  SetLength(data, Length(data)-1);
end;

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  thecolor, thecolor0: integer;
  x, y: single;
  myData: TData;

procedure Translate(toX, toY: single);
begin
  x += toX;
  y += toY;
end;

procedure push();
var
  prop: TProp;
begin
  prop.thecolor:=thecolor;
  prop.x := x;
  prop.y := y;
  mydata.Push(prop);
end;

procedure pop();
var
  prop: TProp;
begin
  prop := mydata.Pop;
  x := prop.x;
  y := prop.y;
  thecolor := prop.thecolor;
end;

procedure Ellipse(ax, ay, aw, ah: single);
begin
  ax += x;
  ay += y;
  Bitmap.FillEllipseAntialias(ax, ay, aw / 2, ah / 2, TByteMask.New(round(power(thecolor, ColorGamma))));
end;

procedure Circles(w: single; depth: integer);
begin
  if (depth > 0) then
  begin
    thecolor += ColorStep;
    Ellipse(w / 2, 0, w, w);
    Circles(w / 2, depth - 1);
    push();
    thecolor -= theColor0;
    Translate(w, 0);
    Ellipse(w / 2, 0, w, w);
    Circles(w / 2, depth - 1);
    pop();
  end;
end;

begin
  thecolor0 := -round(ln(zoom/2)/ln(2)*ColorStep) - ColorStep;
  thecolor := thecolor0;
  x := 0;
  y := 0;
  Translate(0, Bitmap.Height / 2);

  circles(Bitmap.Width * zoom, FractalDepth);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  zoom := 1;
  BGRAVirtualScreen1.BitmapAutoScale:= false;
  BGRAVirtualScreen1.Color := clBlack;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  zoom *= 1.015;
  if (zoom >= 2) then
    zoom := zoom / 2;
    BGRAVirtualScreen1.DiscardBitmap;
end;

end.

