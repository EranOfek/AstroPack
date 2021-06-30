unit uatom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, FGL;

const
  ATOMSPEED: array [0..10] of Double = (0, 2, 1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2);

type

  { TAtom }

  TAtom = class(TObject)
  private
    FAngle: double;
    FImage: TBGRABitmap;
    FSpeed: double;
  public
    constructor Create(const aResource: string; const aSpeed: double);
    destructor Destroy; override;
    property Image: TBGRABitmap read FImage;
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
    procedure Draw(const aBitmap: TBGRABitmap);
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

procedure TAtoms.Draw(const aBitmap: TBGRABitmap);
var
  i: integer;
begin
  for i := 0 to FAtoms.Count-1 do
    aBitmap.PutImageAngle(aBitmap.Width / 2, aBitmap.Height / 2,
    FAtoms[i]{%H-}.Image, FAtoms[i]{%H-}.Angle,
    Rect(0, 0, aBitmap.Width, aBitmap.Height),
    FAtoms[i]{%H-}.Image.Width / 2, FAtoms[i]{%H-}.Image.Height / 2);
end;

{ TAtom }

constructor TAtom.Create(const aResource: string; const aSpeed: double);
begin
  FImage := TBGRABitmap.Create();
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

