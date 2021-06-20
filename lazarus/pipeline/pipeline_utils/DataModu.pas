unit DataModu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDataMod }

  TDataMod = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private

  public
     AstroPackPath: String;

    //
    procedure Init;

    //
    function AstroFile(FileName: String) : String;



  end;

var
  DataMod: TDataMod;

implementation

{$R *.lfm}

procedure TDataMod.DataModuleCreate(Sender: TObject);
begin
  Init;
end;

procedure TDataMod.Init;
begin
      AstroPackPath := GetEnvironmentVariable('ASTROPACKPATH');
end;


function TDataMod.AstroFile(FileName: String) : String;
begin
  FileName := StringReplace(FileName, '/', PathDelim, [rfReplaceAll]);
  FileName := StringReplace(FileName, '\', PathDelim, [rfReplaceAll]);
  FileName := AstroPackPath + PathDelim + FileName;
  Result := FileName;
end;


end.

