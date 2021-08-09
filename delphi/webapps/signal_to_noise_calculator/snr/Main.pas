unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIRegClasses, uniGUIForm, uniMultiItem, uniComboBox,
  uniLabel, uniEdit, uniGroupBox, Vcl.Imaging.pngimage, uniImage,
  uniGUIBaseClasses, uniPanel, uniPageControl, uniSyntaxEditorBase,
  uniSyntaxEditor;

type
  TMainForm = class(TUniForm)
    UniPanel1: TUniPanel;
    UniImage1: TUniImage;
    UniPanel2: TUniPanel;
    UniPanel4: TUniPanel;
    UniPanel5: TUniPanel;
    UniPanel7: TUniPanel;
    UniLabel6: TUniLabel;
    UniLabel7: TUniLabel;
    UniPanel6: TUniPanel;
    UniPanel3: TUniPanel;
    UniLabel8: TUniLabel;
    UniLabel9: TUniLabel;
    UniPanel8: TUniPanel;
    UniLabel10: TUniLabel;
    UniLabel11: TUniLabel;
    UniPanel9: TUniPanel;
    UniLabel12: TUniLabel;
    UniPageControl1: TUniPageControl;
    UniTabSheet1: TUniTabSheet;
    UniTabSheet2: TUniTabSheet;
    UniTabSheet3: TUniTabSheet;
    UniTabSheet4: TUniTabSheet;
    UniGroupBox1: TUniGroupBox;
    UniEdit1: TUniEdit;
    UniEdit2: TUniEdit;
    UniEdit3: TUniEdit;
    UniEdit4: TUniEdit;
    UniLabel1: TUniLabel;
    UniLabel2: TUniLabel;
    UniLabel3: TUniLabel;
    UniLabel4: TUniLabel;
    UniLabel5: TUniLabel;
    UniComboBox1: TUniComboBox;
    UniComboBox2: TUniComboBox;
    UniLabel13: TUniLabel;
    UniLabel14: TUniLabel;
    UniLabel15: TUniLabel;
    UniLabel16: TUniLabel;
    UniLabel17: TUniLabel;
    UniLabel18: TUniLabel;
    UniLabel19: TUniLabel;
    UniLabel20: TUniLabel;
    UniLabel21: TUniLabel;
    UniLabel22: TUniLabel;
    UniLabel23: TUniLabel;
    UniLabel24: TUniLabel;
    UniLabel25: TUniLabel;
    UniLabel26: TUniLabel;
    UniLabel27: TUniLabel;
    UniLabel28: TUniLabel;
    UniLabel29: TUniLabel;
    UniLabel30: TUniLabel;
    UniTabSheet5: TUniTabSheet;
    UniFormattedNumberEdit1: TUniFormattedNumberEdit;
    UniNumberEdit1: TUniNumberEdit;
    UniTabSheet6: TUniTabSheet;
    UniTabSheet7: TUniTabSheet;
    UniLabel31: TUniLabel;
    UniLabel32: TUniLabel;
    UniLabel33: TUniLabel;
    UniLabel34: TUniLabel;
    UniLabel35: TUniLabel;
    UniLabel36: TUniLabel;
    UniLabel37: TUniLabel;
    UniLabel38: TUniLabel;
    UniLabel39: TUniLabel;
    UniLabel40: TUniLabel;
    UniLabel41: TUniLabel;
    UniLabel42: TUniLabel;
    UniLabel43: TUniLabel;
    UniSyntaxEdit1: TUniSyntaxEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uniGUIVars, MainModule, uniGUIApplication;

function MainForm: TMainForm;
begin
  Result := TMainForm(UniMainModule.GetFormInstance(TMainForm));
end;

initialization
  RegisterAppFormClass(TMainForm);

end.
