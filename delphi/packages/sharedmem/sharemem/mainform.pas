unit mainform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  sharedmm, StdCtrls,  ExtCtrls;

type
  TForm1 = class(TForm)
    SharedMemory1: TSharedMemory;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Timer1: TTimer;
    Label4: TLabel;
    Label5: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


procedure TForm1.Timer1Timer(Sender: TObject);
begin
      if label5.caption='Server' then
         memo1.GetTextBuf(sharedmemory1.memory,sharedmemory1.blocksize)
      else
         memo1.SetTextBuf(sharedmemory1.memory);
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  if NOT sharedmemory1.BlockExists then label5.caption:='Server' else label5.caption:='Client';
end;

end.
 