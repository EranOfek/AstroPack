unit SignalToNoiseCalc;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIFrame, uniRadioButton, uniGroupBox, uniGUIBaseClasses,
  uniPanel;

type
  TUniFrameSignalToNoise = class(TUniFrame)
    UniPanel1: TUniPanel;
    UniPanel2: TUniPanel;
    UniGroupBox1: TUniGroupBox;
    RadioFilterWide: TUniRadioButton;
    RadioFilterMedium: TUniRadioButton;
    RadioFilterNarrow: TUniRadioButton;
    RadioFilterQuad: TUniRadioButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}



end.
