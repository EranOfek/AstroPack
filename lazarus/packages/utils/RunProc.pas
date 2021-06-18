unit RunProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ComCtrls, UnTerminal;

type

TRunProc = class(TComponent)
public

  Proc: TConsoleProc;
  LinPartial: boolean;
  Memo1: TMemo;
  StatusBar1: TStatusBar;

  constructor Create; virtual;
  destructor Destroy; override;

  procedure Init;
  procedure Run(Cmd: String);

  // Events
  procedure procChangeState(State: string; pFinal: TPoint);
  procedure procLineCompleted(const lin: string);
  procedure procReadData(nDat: integer; const lastLin: string);


end;

implementation


constructor TRunProc.Create;
begin
    LinPartial := false;
end;


destructor TRunProc.Destroy;
begin
end;


procedure TRunProc.Init;
begin
  proc:= TConsoleProc.Create(nil);
  proc.LineDelimSend := LDS_CRLF;
  proc.OnLineCompleted:=@procLineCompleted;
  proc.OnReadData:=@procReadData;
  proc.OnChangeState:=@procChangeState;
end;

procedure TRunProc.Run(Cmd: String);
begin
  Proc.Open(Cmd, '');
end;

procedure TRunProc.procChangeState(State: string; pFinal: TPoint);
begin
  StatusBar1.Panels[0].Text:=State;
end;

procedure TRunProc.procLineCompleted(const lin: string);
begin
  if LinPartial then begin
    //Estamos en la línea del prompt
    Memo1.Lines[Memo1.Lines.Count-1] := lin;  //reemplaza última línea
    LinPartial := false;
  end else begin  //caso común
    Memo1.Lines.Add(lin);
  end;
end;

procedure TRunProc.procReadData(nDat: integer; const lastLin: string);
begin
  LinPartial := true;   //marca bandera
  Memo1.Lines.Add(lastLin);   //agrega la línea que contiene al prompt
end;




end.


