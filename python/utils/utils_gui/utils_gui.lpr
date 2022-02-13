//----------------------------------------------------------------------------
// Project:  MATLAB & Database Utils
// Module:
// File:     utils_gui.lpr
// Title:    Project file
// Author:   Chen Tishler, 01/2022
//
//----------------------------------------------------------------------------

program utils_gui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, util_datamod, about, LogPanel
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TAppDataModule, AppDataModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

