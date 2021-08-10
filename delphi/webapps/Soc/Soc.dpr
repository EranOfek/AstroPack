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
  MainDashboard in 'Frames\MainDashboard.pas' {UniFrame5: TUniFrame},
  GcsStatus in 'Frames\GcsStatus.pas' {UniFrame6: TUniFrame},
  GcsDashboard in 'Frames\GcsDashboard.pas' {UniFrame7: TUniFrame},
  GcsSchedule in 'Frames\GcsSchedule.pas' {UniFrame8: TUniFrame},
  GcsLog in 'Frames\GcsLog.pas' {UniFrame9: TUniFrame},
  SysMonitor in 'Frames\SysMonitor.pas' {UniFrame10: TUniFrame},
  SowNotifications in 'Frames\SowNotifications.pas' {UniFrame11: TUniFrame},
  Desy in 'Frames\Desy.pas' {UniFrame12: TUniFrame},
  Nasa in 'Frames\Nasa.pas' {UniFrame13: TUniFrame},
  Lsst in 'Frames\Lsst.pas' {UniFrame14: TUniFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  TUniServerModule.Create(Application);
  Application.Run;
end.
