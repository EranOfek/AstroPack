unit EvmFrameInstruction;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFrame1 = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    LabelObjectName: TLabel;
    LabelDescription: TLabel;
    EditObjectName: TEdit;
    EditDescription: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
