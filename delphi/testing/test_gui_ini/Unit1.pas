unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

type
  class TGuiMsg
    public

  end;


  class TGiuHandler
    public

      constructor Init();

      // Send message
      procedure SendMsg(var GuiMsg);

    function TGiuHandler.GetNextMsg(var GuiMsg) : Boolean;

    Path: String;
    IniFile: TIniFile;

  end;



implementation


constructor TGiuHandler.Init();
begin

end;


procedure TGiuHandler.SendMsg(var GuiMsg);
var
  Ini: TIniFile;
  FileName: String;
begin
  IniFile = TIniFile.Create(FileName);
  try

  finally
     Ini.Free;
  end;
end;


function TGiuHandler.GetNextMsg(var GuiMsg) : Boolean;
var
  Ini: TIniFile;
  FileName: String;
begin
  IniFile = TIniFile.Create(FileName);
  try

  finally
    Ini.Free;
  end;

end;




{$R *.dfm}

end.
