program Soc;

uses
  Forms,
  ServerModule in 'ServerModule.pas' {UniServerModule: TUniGUIServerModule},
  MainModule in 'MainModule.pas' {UniMainModule: TUniGUIMainModule},
  Main in 'Main.pas' {MainForm: TUniForm},
  PipelineStatus in 'Frames\PipelineStatus.pas' {UniFrame1: TUniFrame},
  IncomingAlerts in 'Frames\IncomingAlerts.pas' {UniFrame2: TUniFrame},
  OutgoingAlerts in 'Frames\OutgoingAlerts.pas' {UniFrame3: TUniFrame},
  EventsTable in 'Frames\EventsTable.pas' {UniFrame4: TUniFrame},
  MainDashboard in 'Frames\MainDashboard.pas' {UniFrame5: TUniFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  TUniServerModule.Create(Application);
  Application.Run;
end.
