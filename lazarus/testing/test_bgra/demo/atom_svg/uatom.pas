unit uatom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, FGL, BGRASVG, Math;

const
  ATOMSPEED: array [0..10] of Double = (0, 2, 1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2);

type

  { TAtom }

  TAtom = class(TObject)
  private
    FAngle: double;
    FImage: TBGRASVG;
    FSpeed: double;
  public
    constructor Create(const aResource: string; const aSpeed: double);
    destructor Destroy; override;
    property Image: TBGRASVG read FImage;
    property Speed: double read FSpeed;
    property Angle: double read FAngle;
    procedure Step;
  end;

  TAtomList = specialize TFPGObjectList<TAtom>;

  { TAtoms }

  TAtoms = class(TObject)
  private
    FAtoms: TAtomList;
    procedure LoadAtoms;
  public
    constructor Create;
    destructor Destroy; override;
    property Atoms: TAtomList read FAtoms;
    procedure Step;
    procedure Draw(const aBitmap: TBGRABitmap; const ADPI: single);
  end;

implementation

{ TAtoms }

procedure TAtoms.LoadAtoms;
var
  i: integer;
begin
  for i := 0 to 10 do
    {%H-}FAtoms.Add(TAtom.Create(i.ToString, ATOMSPEED[i]));
end;

constructor TAtoms.Create;
begin
  FAtoms := TAtomList.Create(True);
  LoadAtoms;
end;

destructor TAtoms.Destroy;
begin
  FAtoms.Free;
  inherited Destroy;
end;

procedure TAtoms.Step;
var
  i: integer;
begin
  for i := 0 to FAtoms.Count-1 do
    FAtoms[i]{%H-}.Step;
end;

procedure TAtoms.Draw(const aBitmap: TBGRABitmap; const ADPI: single);
var
  i: integer;
begin
  aBitmap.Canvas2D.save;
  for i := 0 to FAtoms.Count-1 do
    try
      aBitmap.Canvas2D.resetTransform;
      aBitmap.Canvas2D.translate(aBitmap.Width/2, aBitmap.Height/2);
      aBitmap.Canvas2D.rotate(FAtoms[i].Angle*Pi/180);
      FAtoms[i].Image.Draw(aBitmap.Canvas2D,taCenter, tlCenter, 0, 0, ADPI);
    except
    end;
  aBitmap.Canvas2D.restore;     
end;

{ TAtom }

constructor TAtom.Create(const aResource: string; const aSpeed: double);
begin
  FImage := TBGRASVG.Create();
  FImage.LoadFromResource(aResource);
  FSpeed := aSpeed;
end;

destructor TAtom.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TAtom.Step;
begin
  FAngle += FSpeed;
end;

end.

