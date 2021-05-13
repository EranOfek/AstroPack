unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIRegClasses, uniGUIForm, Vcl.Menus, uniMainMenu,
  Vcl.Imaging.pngimage, uniGUIBaseClasses, uniImage,
  SignalToNoiseCalc;

type
  TMainForm = class(TUniForm)
    UniMainMenu1: TUniMainMenu;
    MIAbout: TUniMenuItem;
    MIAboutUltrasatProject: TUniMenuItem;
    MIAstroTools: TUniMenuItem;
    MISignalToNoiseCalculator: TUniMenuItem;
    MIDatabases: TUniMenuItem;
    MIDbSources: TUniMenuItem;
    MIDbRawImages: TUniMenuItem;
    UniImageLogo: TUniImage;
    MainMenu1: TMainMenu;
    procedure MISignalToNoiseCalculatorClick(Sender: TObject);
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

procedure TMainForm.MISignalToNoiseCalculatorClick(Sender: TObject);
begin
  //


end;

initialization
  RegisterAppFormClass(TMainForm);

end.
