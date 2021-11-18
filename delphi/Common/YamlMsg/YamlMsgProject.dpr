program YamlMsgProject;

uses
  Vcl.Forms,
  YamlMsgTest in 'YamlMsgTest.pas' {Form1},
  MsgYaml in 'MsgYaml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
