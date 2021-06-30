unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BCTypes, contnrs, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    points: TObjectList;
  end;

  { TXY }

  TXY = class(TObject)
  private
    Fx: integer;
    Fy: integer;
    public
      constructor Create(x, y: integer);
      property x: integer read Fx write Fx;
      property y: integer read Fy write Fy;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TXY }

constructor TXY.Create(x, y: integer);
begin
  Self.x := x;
  Self.y := y;
end;

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  i: integer;
  p1, p2: TXY;
  temp: TBGRABitmap;
begin
  for i:=0 to points.Count-2 do
  begin
    p1 := TXY(points.Items[i]);
    p2 := TXY(points.Items[i+1]);
    if i = points.Count-2 then
      Bitmap.DrawLineAntialias(p1.X, p1.Y, p2.x, p2.y, BGRA(255,255,255,i*8), i div 2, True)
    else
      Bitmap.DrawLineAntialias(p1.X, p1.Y, p2.x, p2.y, BGRA(255,255,255,i*8), i div 2, False);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  p: TPoint;
begin
  points := TObjectList.Create;
  p := ScreenToClient(Mouse.CursorPos);
  for i:=0 to 31 do
    points.Add(TXY.Create(p.x, p.y));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  p: TPoint;
begin
  p := ScreenToClient(Mouse.CursorPos);
  points.Delete(0);
  points.Add(TXY.Create(p.x, p.y));
  BGRAVirtualScreen1.DiscardBitmap;
end;

end.
