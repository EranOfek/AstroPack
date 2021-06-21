unit fisSharedMemory;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type

  TfisSharedMemory = class(TComponent)
  private
    { Private declarations }
    FShareName: String;
    FSize: integer;
    FHandle, FMutex: THandle;
    FReadOnly: boolean;
    FTimeout: integer;

  protected
      procedure SetName(const aValue: TComponentName );override;
    { Protected declarations }
  public
      constructor Create(AOwner: TComponent);override;
      destructor Destroy;override;
      function MapMemory: pointer;    { Public declarations }
      function UnMapMemory(aMapPtr: pointer):boolean;
      function CreateMemory: boolean;
      function CloseMemory: boolean;
      function OpenMemory: boolean;
      function RequestOwnership: boolean;
      function ReleaseOwnership: boolean;
      property Handle: THandle read FHandle;
      property Mutex: THandle read FMutex;

  published
    { Published declarations }
      property ReadOnly: boolean read FReadOnly write FReadOnly default false;
      property ShareName: String read FShareName write FShareName;
      property Size: integer read FSize write FSize;
      property Timeout: integer read FTimeout write FTimeout default -1;

  end;

  const
    MUTEX_NAME = '_SMMutex';

procedure Register;

implementation

procedure TfisSharedMemory.SetName(const aValue: TComponentName );
var
    lChange: boolean;
begin
    lChange := (csDesigning in ComponentState) and
        ((Name = FShareName) or (Length(FShareName) = 0));
    inherited;
    if lChange then
    begin
        FShareName := Name;
    end;
end;
//---------------------------------------------------------------------------
function TfisSharedMemory.MapMemory:pointer;
var
    lMapping: DWord;
begin
    if FHandle = 0 then
    begin
      Result := nil;
      exit;
    end;

    if(FReadOnly)then
    begin
        lMapping := FILE_MAP_READ;
    end
    else
    begin
        lMapping := FILE_MAP_WRITE;
    end;
    Result := MapViewOfFile(FHandle, lMapping, 0, 0, FSize);
    if(Result = nil)then
    begin
        ReleaseMutex(FMutex);
    end;
end;
//---------------------------------------------------------------------------
function TfisSharedMemory.UnMapMemory(aMapPtr: pointer): boolean;
begin
    if FHandle <> 0 then
    begin
        UnmapViewOfFile(aMapPtr);
        result := true;
    end
    else
    begin
        result := false;
    end;
end;
//---------------------------------------------------------------------------
function TfisSharedMemory.CreateMemory: boolean;
var
    lMutexName: string;
begin
    Result := true;
    if FHandle <> 0 then CreateMemory := false;
    FHandle := CreateFileMapping(THANDLE($FFFFFFFF), nil, PAGE_READWRITE, 0,
        FSize, pchar(FShareName));
    if (FHandle = 0) or ((FHandle <> 0) and (GetLastError = ERROR_ALREADY_EXISTS)) then
    begin
        CloseMemory;
        Result := false;
    end;
    lMutexName := FShareName + MUTEX_NAME;
    FMutex := CreateMutex(nil, false, pchar(lMutexName));
    if(FMutex = 0) then
    begin
        CloseMemory;
        Result := false;
    end;
end;
//---------------------------------------------------------------------------
function TfisSharedMemory.CloseMemory: boolean;
begin
    if(FHandle <> 0) then
    begin
        CloseHandle(FHandle);
        FHandle := 0;
    end;
    if(FMutex <> 0) then
    begin
        CloseHandle(FMutex);
        FMutex := 0;
    end;
    Result := true;
end;
//---------------------------------------------------------------------------
function TfisSharedMemory.OpenMemory: boolean;
var
    lMutexName: string;
begin
    Result := false;
    if(FHandle = 0) then
    begin
        FHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, true, pchar(FShareName));
        if(FHandle <> 0) then
        begin
            lMutexName := FShareName + MUTEX_NAME;
            FMutex := OpenMutex(MUTEX_ALL_ACCESS, true, pchar(lMutexName));
            if(FMutex <> 0 ) then
            begin
                Result := true;
            end
            else
            begin
                CloseMemory;
            end;
        end;
    end;
end;
//---------------------------------------------------------------------------
function TfisSharedMemory.RequestOwnership: boolean;
var
    lTimeout: DWord;
begin
    Result := false;
    if(FHandle <> 0) then
    begin
        if(FTimeout < 0) then
        begin
            lTimeout := INFINITE;
        end
        else
        begin
            lTimeout := FTimeout;
        end;
        Result := WaitForSingleObject(FMutex, lTimeout) = WAIT_OBJECT_0;
    end;
end;
//---------------------------------------------------------------------------
function TfisSharedMemory.ReleaseOwnership: boolean;
begin
    Result := false;
    if(FHandle <> 0) then
    begin
        Result := ReleaseMutex(FMutex);
    end;
end;
//---------------------------------------------------------------------------
constructor TfisSharedMemory.Create(AOwner: TComponent);
begin
    inherited;
    FShareName := '';
    FTimeout := -1;
    FSize := 0;
    FReadOnly := false;
    FHandle := 0;
    FMutex := 0;
end;
//---------------------------------------------------------------------------
destructor TfisSharedMemory.Destroy;
begin
    CloseMemory;
    inherited;
end;
//---------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('FISH', [TfisSharedMemory]);
end;
//---------------------------------------------------------------------------
end.
