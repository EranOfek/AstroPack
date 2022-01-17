unit datamod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PQConnection, SQLite3Conn, SQLDB;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    PQConnection: TPQConnection;
    SQLite3Connection: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
  private

  public

  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.lfm}

end.

