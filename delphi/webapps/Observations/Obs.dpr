program Obs;

uses
  Forms,
  ServerModule in 'ServerModule.pas' {UniServerModule: TUniGUIServerModule},
  MainModule in 'MainModule.pas' {UniMainModule: TUniGUIMainModule},
  Main in 'Main.pas' {MainForm: TUniForm},
  ObsCalendar in 'Frames\ObsCalendar.pas' {UniFrame1: TUniFrame},
  ObsBasic in 'Frames\ObsBasic.pas' {UniFrame2: TUniFrame},
  ObsSeq in 'Frames\ObsSeq.pas' {UniFrame3: TUniFrame},
  ObsScheduler in 'Frames\ObsScheduler.pas' {UniFrame4: TUniFrame},
  ObsTask in 'Frames\ObsTask.pas' {UniFrame5: TUniFrame},
  ObsTargets in 'Frames\ObsTargets.pas' {UniFrame6: TUniFrame},
  ObsVisibility in 'Frames\ObsVisibility.pas' {UniFrame7: TUniFrame},
  ObsAstroMaps in 'Frames\ObsAstroMaps.pas' {UniFrame8: TUniFrame},
  ObsProgram in 'Frames\ObsProgram.pas' {UniFrame9: TUniFrame},
  ObsValidation in 'Frames\ObsValidation.pas' {UniFrame10: TUniFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  TUniServerModule.Create(Application);
  Application.Run;
end.
