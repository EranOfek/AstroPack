unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, BGRAVirtualScreen, BGRABitmap, BCTypes, BGRABitmapTypes, Math, Types,
  BGRALayerOriginal, BGRAStreamLayers, BGRALayers, BGRASuperformulaOriginal;

type

  { TfrmSuperFormula }

  TfrmSuperFormula = class(TForm)
    cbLineColor: TColorButton;
    cbFillColor: TColorButton;
    edValueA: TFloatSpinEdit;
    edValueB: TFloatSpinEdit;
    edValueM: TFloatSpinEdit;
    edValueN1: TFloatSpinEdit;
    edValueN2: TFloatSpinEdit;
    edValueN3: TFloatSpinEdit;
    edMultiplier: TFloatSpinEdit;
    edLineWidth: TFloatSpinEdit;
    lblFillColor: TLabel;
    lblLineColor: TLabel;
    lblLineWidth: TLabel;
    lblMultiplier: TLabel;
    lblValueN3: TLabel;
    lblValueN2: TLabel;
    lblValueN1: TLabel;
    lblValueM: TLabel;
    lblValueB: TLabel;
    lblValueA: TLabel;
    pnlControls: TPanel;
    vsPreview: TBGRAVirtualScreen;
    procedure cbFillColorColorChanged(Sender: TObject);
    procedure cbLineColorColorChanged(Sender: TObject);
    procedure edLineWidthChange(Sender: TObject);
    procedure edMultiplierChange(Sender: TObject);
    procedure edValueAChange(Sender: TObject);
    procedure edValueBChange(Sender: TObject);
    procedure edValueMChange(Sender: TObject);
    procedure edValueN1Change(Sender: TObject);
    procedure edValueN2Change(Sender: TObject);
    procedure edValueN3Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure vsPreviewClick(Sender: TObject);
    procedure vsPreviewMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure vsPreviewMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
  private
    procedure OriginalChange(Sender: TObject; {%H-}AOriginal: TBGRALayerCustomOriginal);
  public
    FLayers: TBGRALayeredBitmap;
  end;

var
  frmSuperFormula: TfrmSuperFormula;

implementation

{$R *.lfm}

{ TfrmSuperFormula }

procedure TfrmSuperFormula.vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  FLayers.Draw(Bitmap, 0, 0);
end;

procedure TfrmSuperFormula.edValueAChange(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.a := edValueA.Value;
end;

procedure TfrmSuperFormula.edMultiplierChange(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.Multiplier := edMultiplier.Value;
end;

procedure TfrmSuperFormula.edLineWidthChange(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.LineWidth := edLineWidth.Value;
end;

procedure TfrmSuperFormula.cbLineColorColorChanged(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.LineColor := cbLineColor.ButtonColor;
end;

procedure TfrmSuperFormula.cbFillColorColorChanged(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.FillColor := cbFillColor.ButtonColor;
end;

procedure TfrmSuperFormula.edValueBChange(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.b := edValueB.Value;
end;

procedure TfrmSuperFormula.edValueMChange(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.m := edValueM.Value;
end;

procedure TfrmSuperFormula.edValueN1Change(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.n1 := edValueN1.Value;
end;

procedure TfrmSuperFormula.edValueN2Change(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.n2 := edValueN2.Value;
end;

procedure TfrmSuperFormula.edValueN3Change(Sender: TObject);
var
  orig: TBGRALayerSuperformulaOriginal;
begin
  orig := FLayers.Original[0] as TBGRALayerSuperformulaOriginal;
  orig.n3 := edValueN3.Value;
end;

procedure TfrmSuperFormula.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FLayers.Original[0].SaveToFile(Application.Location + 'superformula.data');
end;

procedure TfrmSuperFormula.FormDestroy(Sender: TObject);
begin
  FLayers.Free;
end;

procedure TfrmSuperFormula.vsPreviewClick(Sender: TObject);
begin
  cbFillColor.Click;
end;

procedure TfrmSuperFormula.vsPreviewMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  edMultiplier.Value := edMultiplier.Value - 25;
end;

procedure TfrmSuperFormula.vsPreviewMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  edMultiplier.Value := edMultiplier.Value + 25;
end;

procedure TfrmSuperFormula.FormCreate(Sender: TObject);
var
  superformula: TBGRALayerSuperformulaOriginal;
begin
  {$ifdef Windows}
  DoubleBuffered := True;
  {$endif}
  cbLineColor.Constraints.MinHeight := edValueA.Height;
  cbFillColor.Constraints.MinHeight := edValueA.Height;

  FLayers := TBGRALayeredBitmap.Create(vsPreview.Width, vsPreview.Height);
  Flayers.OnOriginalChange := @OriginalChange;

  superformula := TBGRALayerSuperformulaOriginal.Create;
  FLayers.AddLayerFromOwnedOriginal(superformula);

  if FileExists(Application.Location + 'superformula.data') then
  begin
    superformula.LoadFromFile(Application.Location + 'superformula.data');
    cbLineColor.ButtonColor := superformula.LineColor;
    cbFillColor.ButtonColor := superformula.FillColor;
    edValueA.Value := superformula.a;
    edValueB.Value := superformula.b;
    edValueM.Value := superformula.m;
    edValueN1.Value := superformula.n1;
    edValueN2.Value := superformula.n2;
    edValueN3.Value := superformula.n3;
    edMultiplier.Value := superformula.Multiplier;
    edLineWidth.Value := superformula.LineWidth;
  end;
end;

procedure TfrmSuperFormula.OriginalChange(Sender: TObject;
  AOriginal: TBGRALayerCustomOriginal);
begin
  vsPreview.DiscardBitmap;
end;

end.
