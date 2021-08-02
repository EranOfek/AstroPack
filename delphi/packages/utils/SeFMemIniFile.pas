{*******************************************************}
{                                                       }
{       TFMemIniFile - Improved TMemIniFile class       }
{                                                       }
{       Written by Chen Tishler, 11/05/02               }
{                                                       }
{*******************************************************}

unit SeFMemIniFile;

{$R-,T-,H+,X+}

interface

uses Windows, SysUtils, Classes, IniFiles;

type
  { TFMemIniFile - loads and entire ini file into memory and allows all
    operations to be performed on the memory image.  The image can then
    be written out to the disk file }

  TFMemIniFile = class(TCustomIniFile)
  private
    FSections: TStringList;
    FFileName: string;  // Chen
    function AddSection(const Section: string): TStrings;
    procedure LoadValues;
  public
    constructor Create(const FileName: string = '');    // Default value added (25/11/2015)
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure EraseSection(const Section: string); override;
    procedure GetStrings(List: TStrings);
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure Rename(const FileName: string; Reload: Boolean);
    procedure SetStrings(List: TStrings);
    procedure UpdateFile; override;
    procedure WriteString(const Section, Ident, Value: String); override;

    // Chen
    procedure ReadSectionLines(const Section: string; Strings: TStrings);
    procedure WriteSectionLines(const Section: string; Strings: TStrings);
    procedure RenameKey(const Section, Ident, NewIdent: string);
    procedure RenameKeyValue(const Section, Ident, NewIdent, Value: string);

    // Chen, 26/02/13)
    function ReadIntegerRange(const Section, Ident: string;  Default, Min, Max: Integer) : Integer;

    // Chen, (25/11/2015)
    function GetText : String;
    procedure SetText(const AText: string);
    function GetCommaText : String;
    procedure SetCommaText(const AText: string);

    //
    property FileName: string read FFileName;

    // Get/Set the entire file as text (25/11/2015)
    property Text: string read GetText write SetText;
    property CommaText: string read GetCommaText write SetCommaText;
  end;

implementation

uses Consts;


{ TFMemIniFile }

constructor TFMemIniFile.Create(const FileName: string);
begin
  inherited Create(FileName);
  FFileName := FileName;
  FSections := TStringList.Create;
  LoadValues;
end;

destructor TFMemIniFile.Destroy;
begin
  if FSections <> nil then Clear;
  FSections.Free;
  inherited;
end;

function TFMemIniFile.AddSection(const Section: string): TStrings;
begin
  Result := TStringList.Create;
  try
    FSections.AddObject(Section, Result);
  except
    Result.Free;
  end;
end;

procedure TFMemIniFile.Clear;
var
  I: Integer;
begin
  for I := 0 to FSections.Count - 1 do
    TStrings(FSections.Objects[I]).Free;
  FSections.Clear;
end;

procedure TFMemIniFile.DeleteKey(const Section, Ident: String);
var
  I, J: Integer;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(FSections.Objects[I]);
    J := Strings.IndexOfName(Ident);
    if J >= 0 then Strings.Delete(J);
  end;
end;

procedure TFMemIniFile.EraseSection(const Section: string);
var
  I: Integer;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    TStrings(FSections.Objects[I]).Free;
    FSections.Delete(I);
  end;
end;

procedure TFMemIniFile.GetStrings(List: TStrings);
var
  I, J: Integer;
  Strings: TStrings;
begin
  List.BeginUpdate;
  try
    for I := 0 to FSections.Count - 1 do
    begin
      List.Add('[' + FSections[I] + ']');
      Strings := TStrings(FSections.Objects[I]);
      for J := 0 to Strings.Count - 1 do List.Add(Strings[J]);
      List.Add('');
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TFMemIniFile.LoadValues;
var
  List: TStringList;
begin
  if (FileName <> '') and FileExists(FileName) then
  begin
    List := TStringList.Create;
    try
      List.LoadFromFile(FileName);
      SetStrings(List);
    finally
      List.Free;
    end;
  end else Clear;
end;

procedure TFMemIniFile.ReadSection(const Section: string;
  Strings: TStrings);
var
  I, J: Integer;
  SectionStrings: TStrings;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
    begin
      SectionStrings := TStrings(FSections.Objects[I]);
      for J := 0 to SectionStrings.Count - 1 do
        Strings.Add(SectionStrings.Names[J]);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TFMemIniFile.ReadSections(Strings: TStrings);
begin
  Strings.Assign(FSections);
end;

procedure TFMemIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then Strings.Assign(TStrings(FSections.Objects[I]));
  finally
    Strings.EndUpdate;
  end;
end;

function TFMemIniFile.ReadString(const Section, Ident,
  Default: string): string;
var
  I: Integer;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
  begin
    Strings := TStrings(FSections.Objects[I]);
    I := Strings.IndexOfName(Ident);
    if I >= 0 then
    begin
      Result := Copy(Strings[I], Length(Ident) + 2, Maxint);
      Exit;
    end;
  end;
  Result := Default;
end;

procedure TFMemIniFile.Rename(const FileName: string; Reload: Boolean);
begin
  FFileName := FileName;
  if Reload then LoadValues;
end;

procedure TFMemIniFile.SetStrings(List: TStrings);
var
  I: Integer;
  S: string;
  Strings: TStrings;
begin
  Clear;
  Strings := nil;
  for I := 0 to List.Count - 1 do
  begin
    S := List[I];
    if (S <> '') and (S[1] <> ';') then
      if (S[1] = '[') and (S[Length(S)] = ']') then
        Strings := AddSection(Copy(S, 2, Length(S) - 2))
      else
        if Strings <> nil then Strings.Add(S);
  end;
end;

procedure TFMemIniFile.UpdateFile;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    GetStrings(List);
    List.SaveToFile(FFileName);
  finally
    List.Free;
  end;
end;

procedure TFMemIniFile.WriteString(const Section, Ident, Value: String);
var
  I: Integer;
  S: string;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := TStrings(FSections.Objects[I]) else
    Strings := AddSection(Section);
  S := Ident + '=' + Value;
  I := Strings.IndexOfName(Ident);
  if I >= 0 then Strings[I] := S else Strings.Add(S);
end;

// Chen
procedure TFMemIniFile.ReadSectionLines(const Section: string; Strings: TStrings);
var
  I: Integer;
  SectionStrings: TStrings;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf(Section);
    if I >= 0 then
    begin
      SectionStrings := TStrings(FSections.Objects[I]);
      Strings.AddStrings(SectionStrings);
    end;
  finally
    Strings.EndUpdate;
  end;
end;


procedure TFMemIniFile.WriteSectionLines(const Section: string; Strings: TStrings);
var
  I: Integer;
  SectionStrings: TStrings;
begin
  try
    I := FSections.IndexOf(Section);
    if I >= 0 then
      SectionStrings := TStrings(FSections.Objects[I])
    else SectionStrings := AddSection(Section);

    SectionStrings.Assign(Strings);
  finally
  end;
end;


procedure TFMemIniFile.RenameKey(const Section, Ident, NewIdent: string);
var
  I: Integer;
  S: string;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := TStrings(FSections.Objects[I]) else
    Strings := AddSection(Section);
  I := Strings.IndexOfName(Ident);
  if I >= 0 then begin
    S := Copy(Strings[I], Length(Ident) + 2, Maxint);
    Strings[I] := NewIdent + '=' + S;
  end;
end;


procedure TFMemIniFile.RenameKeyValue(const Section, Ident, NewIdent, Value: string);
var
  I, J: Integer;
  S: string;
  Strings: TStrings;
begin
  I := FSections.IndexOf(Section);
  if I >= 0 then
    Strings := TStrings(FSections.Objects[I]) else
    Strings := AddSection(Section);
  S := NewIdent + '=' + Value;
  I := Strings.IndexOfName(Ident);
  if I >= 0 then Strings[I] := S else
  begin
    J := Strings.IndexOfName(NewIdent);
    if (J < 0) then
      Strings.Add(S);
  end;
end;

// 26/02/13
function TFMemIniFile.ReadIntegerRange(const Section, Ident: string;  Default, Min, Max: Integer) : Integer;
begin
  Result := ReadInteger(Section, Ident, Default);

  if (Result < Min) then Result := Min;
  if (Result > Max) then Result := Max;
end;


// Chen, (25/11/2015)
function TFMemIniFile.GetText : String;
var
  AStrings: TStringList;
begin
  AStrings := TStringList.Create;
  GetStrings(AStrings);
  Result := AStrings.Text;
  AStrings.Free;
end;

// Chen, (25/11/2015)
procedure TFMemIniFile.SetText(const AText: string);
var
  AStrings: TStringList;
begin
  AStrings := TStringList.Create;
  AStrings.Text := AText;
  SetStrings(AStrings);
  AStrings.Free;
end;


// Chen, (25/11/2015)
function TFMemIniFile.GetCommaText : String;
var
  AStrings: TStringList;
begin
  AStrings := TStringList.Create;
  GetStrings(AStrings);
  Result := AStrings.CommaText;
  AStrings.Free;
end;

// Chen, (25/11/2015)
procedure TFMemIniFile.SetCommaText(const AText: string);
var
  AStrings: TStringList;
begin
  AStrings := TStringList.Create;
  AStrings.CommaText := AText;
  SetStrings(AStrings);
  AStrings.Free;
end;


end.

