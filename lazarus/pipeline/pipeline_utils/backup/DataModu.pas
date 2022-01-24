unit DataModu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, PQConnection, SQLDB, DB;

type

  { TDataMod }

  TDataMod = class(TDataModule)
    DataSource1: TDataSource;
    PQConnection1: TPQConnection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DataModuleCreate(Sender: TObject);

  private

  public
    // Data
    AstroPackPath: String;                     // AstroPack folder
    IniFileName : String;
    IniFile : TIniFile;

    PythonExe : String;

    // Initialize
    procedure Init;

    // Get full path to AstroPack file/folder
    function AstroFile(FileName: String) : String;

    // Get list of databases
    function GetDbList : TStringList;

    // Get list of databases
    function GetDbTablesList(DbName: String) : TStringList;

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
      AstroPackPath := GetEnvironmentVariable('ASTROPACK_PATH');

      if AstroPackPath = '' then
      begin
        {$IFDEF UNIX}
        AstroPackPath := '/home/user/eran/matlab/AstroPack';
        {$ENDIF}
      end;

      IniFileName := AstroPackPath + PathDelim + 'config.ini';
      IniFile := TIniFile.Create(IniFileName);

      {$IFDEF WIN32}
      PythonExe := IniFile.ReadString('Python', 'Python3', 'C:\Python38\python.exe');
      {$ENDIF}

      {$IFDEF UNIX}
      PythonExe := IniFile.ReadString('Python', 'Python3', 'python3');
      {$ENDIF}
end;

function TDataMod.AstroFile(FileName: String) : String;
begin
  FileName := StringReplace(FileName, '/', PathDelim, [rfReplaceAll]);
  FileName := StringReplace(FileName, '\', PathDelim, [rfReplaceAll]);
  FileName := AstroPackPath + PathDelim + FileName;
  Result := FileName;
end;

function TDataMod.GetDbList : TStringList;
begin
  // https://stackoverflow.com/questions/19175615/function-with-result-tstringlist-best-practice
  Result := TStringList.Create;

  // https://kb.objectrocket.com/postgresql/postgres-list-tables-with-python-1023
end;

// Get list of databases
function TDataMod.GetDbTablesList(DbName: String) : TStringList;
begin
  Result := TStringList.Create;

  // https://kb.objectrocket.com/postgresql/postgres-list-tables-with-python-1023
end;


end.

