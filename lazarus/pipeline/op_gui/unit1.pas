unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Process;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnRunMain: TBitBtn;
    BtnStartup: TBitBtn;
    BtnCloseAll: TBitBtn;
    BtnLiveLeft: TBitBtn;
    BtnLiveRight: TBitBtn;
    BtnRecordLeft: TBitBtn;
    BtnRecordRight: TBitBtn;
    BtnSdCard: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure BtnCloseAllClick(Sender: TObject);
    procedure BtnLiveLeftClick(Sender: TObject);
    procedure BtnLiveRightClick(Sender: TObject);
    procedure BtnRecordLeftClick(Sender: TObject);
    procedure BtnRecordRightClick(Sender: TObject);
    procedure BtnRunMainClick(Sender: TObject);
    procedure BtnStartupClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

const
  ScriptPath = '/home/user/dev/opsci.git/src/scripts/';


implementation

{$R *.lfm}

{ TForm1 }

procedure RunScript(Script: AnsiString);
var
  AProcess: TProcess;
  Cmd, s: ansistring;
begin
  Script := ScriptPath + '/' + Script;
  Cmd := Script + ';sleep 2';
  AProcess := TProcess.Create(nil);
  AProcess.Executable := '/usr/bin/xterm';  //Cmd;  //'/bin/bash';
  AProcess.Parameters.Add('-e');
  AProcess.Parameters.Add(Cmd);
  //AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  //AProcess.Free;

  //if RunCommand('/bin/bash',['-c',Script], s, [{poUsePipes}{, poWaitOnExit}]) then
  //   writeln(s);


end;

procedure TForm1.BtnLiveLeftClick(Sender: TObject);
begin
  RunScript('pre1.sh');
end;

procedure TForm1.BtnCloseAllClick(Sender: TObject);
begin
  RunScript('kill_all.sh');
end;

procedure TForm1.BtnLiveRightClick(Sender: TObject);
begin
  RunScript('pre2.sh');
end;

procedure TForm1.BtnRecordLeftClick(Sender: TObject);
begin
  RunScript('rec1.sh');
end;

procedure TForm1.BtnRecordRightClick(Sender: TObject);
begin
  RunScript('rec2.sh');
end;

procedure TForm1.BtnRunMainClick(Sender: TObject);
begin
  RunScript('run.sh');
end;

procedure TForm1.BtnStartupClick(Sender: TObject);
begin
  RunScript('startup.sh');
end;


end.

