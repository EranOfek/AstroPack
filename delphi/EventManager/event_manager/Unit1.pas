unit Unit1;

interface

uses
  System.SysUtils, System.Classes, Data.DB;

type
  TDataModule1 = class(TDataModule)
    DataSource1: TDataSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
