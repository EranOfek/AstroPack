program evm;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  EvmFrameInstruction in 'EvmFrameInstruction.pas' {Frame1: TFrame},
  EmvFrameUserDef in 'EmvFrameUserDef.pas' {Frame2: TFrame},
  EvmFrameChangePassword in 'EvmFrameChangePassword.pas' {Frame3: TFrame},
  EvmFrameLogin in 'EvmFrameLogin.pas' {Frame4: TFrame},
  EvmEmptyMasterDetailFrame in 'EvmEmptyMasterDetailFrame.pas' {Form2},
  ApsObjects in 'ApsObjects.pas' {ApsFrameObjects},
  ApsEventTypes in 'ApsEventTypes.pas' {Form4},
  EvmFrameOkCancel in 'EvmFrameOkCancel.pas' {FrameOkCancel: TFrame},
  EvmTest1 in 'EvmTest1.pas' {Frame6: TFrame},
  ApsFrameEventTypes in 'ApsFrameEventTypes.pas' {Frame5: TFrame},
  ApsFrameInstructions in 'ApsFrameInstructions.pas' {Frame7: TFrame},
  ApsFrameMaps in 'ApsFrameMaps.pas' {Frame8: TFrame},
  Unit9 in 'Unit9.pas' {Frame9: TFrame},
  EvmFrameEventTable in 'EvmFrameEventTable.pas' {Frame10: TFrame},
  EvmFrameUserList in 'EvmFrameUserList.pas' {Frame11: TFrame},
  RepReportDef in 'RepReportDef.pas' {Frame12: TFrame},
  RepReportResult in 'RepReportResult.pas' {Frame13: TFrame},
  RepReportList in 'RepReportList.pas' {Frame14: TFrame},
  EvmFrameEevntDetails in 'EvmFrameEevntDetails.pas' {Frame15: TFrame},
  EvmFrameEventFilter in 'EvmFrameEventFilter.pas' {Frame16: TFrame},
  EvmFrameMap in 'EvmFrameMap.pas' {Frame17: TFrame},
  EvmFrameMapList in 'EvmFrameMapList.pas' {Frame18: TFrame},
  KernelFrameMain in 'KernelFrameMain.pas' {Frame19: TFrame},
  EvmFrameBrowser in 'EvmFrameBrowser.pas' {Frame20: TFrame},
  FrameAbout in 'FrameAbout.pas' {Frame21: TFrame},
  FrameDbGrid in 'FrameDbGrid.pas' {Frame22: TFrame},
  FrameLogPanel in 'FrameLogPanel.pas' {Frame23: TFrame},
  EvmFramePopupMessage in 'EvmFramePopupMessage.pas' {Frame24: TFrame},
  FrameSearch in 'FrameSearch.pas' {Frame25: TFrame},
  Unit26 in 'Unit26.pas' {Frame26: TFrame},
  Unit1 in 'Unit1.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TApsFrameObjects, ApsFrameObjects);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
