program evm;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  EvmFrame1 in 'EvmFrame1.pas' {Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
