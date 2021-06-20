program PipelineUtils;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, About, Main, LogPanel, DbBrowser, DataModu;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDataMod, DataMod);
  Application.CreateForm(TDbBrowserForm, DbBrowserForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

