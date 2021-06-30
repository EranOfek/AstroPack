unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, BGRAGraphics, Dialogs, Spin,
  StdCtrls, BGRAVirtualScreen, BGRABitmap, BCTypes, BCPanel, udots, BGRABitmapTypes;

type

  { TfrmDots }

  TfrmDots = class(TForm)
    editDotsX: TSpinEdit;
    editMaxSize: TSpinEdit;
    editMinSize: TSpinEdit;
    editRandomPos: TSpinEdit;
    editSeed: TSpinEdit;
    editSpacing: TSpinEdit;
    SettingsPanel: TBCPanel;
    DrawingArea: TBGRAVirtualScreen;
    editColor: TColorButton;
    editLineSize: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    editDotsY: TSpinEdit;
    procedure DrawingAreaRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure ChangeSettings(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Dots: TDotDrawer;
    procedure UpdateSettings;
  end;

var
  frmDots: TfrmDots;

implementation

{$R *.lfm}

{ TfrmDots }

procedure TfrmDots.DrawingAreaRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Dots.Draw(Bitmap);
end;

procedure TfrmDots.FormCreate(Sender: TObject);
begin
  Dots := TDotDrawer.Create(Self);
  UpdateSettings;
end;

procedure TfrmDots.ChangeSettings(Sender: TObject);
begin
  UpdateSettings;
end;

procedure TfrmDots.UpdateSettings;
begin
  Dots.Seed := editSeed.Value;
  Dots.DotsX := editDotsX.Value;
  Dots.DotsY := editDotsY.Value;
  Dots.DotsColor := ColorToBGRA(editColor.ButtonColor);
  Dots.DotsSizeMax := editMinSize.Value;
  Dots.DotsSizeMin := editMaxSize.Value;
  Dots.DotsSpacing := editSpacing.Value;
  Dots.DotsRandPos := editRandomPos.Value;
  Dots.LinesSize := editLineSize.Value;
  Dots.LinesColor := Dots.DotsColor;
  Dots.OffSetX := editSpacing.Value;
  Dots.OffSetY := editSpacing.Value;
  Dots.PrepareData;
  DrawingArea.DiscardBitmap;
end;

end.

