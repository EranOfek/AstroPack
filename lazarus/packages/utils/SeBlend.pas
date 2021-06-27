{*********************************************************************
 *****                                                           *****
 *****                TBlend Component. Version 1.0              *****
 *****                   (c) 2000, Oleg Tjagunov.                *****
 *****                   E-mail: oleg_tg@mail.ru                 *****
 *****                                                           *****
 *****         This  source   unit  is  freeware,  you  can      *****
 *****         edit  and  modify the source code, use it as      *****
 *****         a part of  application, but you can not sale      *****
 *****         and/or  receive  money  benefit of the given      *****
 *****         component source file.                            *****
 *****                                                           *****
 *****         See ReadMe.txt for information  about  using      *****
 *****         this component, it's properties, methods and      *****
 *****         functions overview.                               *****
 *****                                                           *****
 *********************************************************************}

unit SeBlend;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Graphics, ExtCtrls, Controls;

type
  TBlendMode = (bmLWA_COLORKEY, bmLWA_ALPHA, bmBoth);

  TBlend = class(TComponent)
  private
    fAlpha: Byte;
    fAlphaFirst: Byte;
    fActive: Boolean;
    fBlendMode: TBlendMode;
    fColorKey: TColor;
    fDesignActive: Boolean;
    fShow: Boolean;
    fStep: Byte;
    fTimer: TTimer;
    function IsLayeredWindow(hWnd: HWND): Boolean;
    procedure SetLayeredWindow(hWnd: HWND; bIsLayered: Boolean);
    procedure Activate;
    procedure SetAlpha(Value: Byte);
    procedure SetActive(Value: Boolean);
    procedure SetBlendMode(Value: TBlendMode);
    procedure SetColorkey(Value: TColor);
    procedure SetDesignActive(Value: Boolean);
    procedure ProcTimer(Sender: TObject);
    { Private declarations }
  protected
    { Protected declarations }
  public
    procedure ShowWindow(Show: Boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  published
    property Alpha: Byte read FAlpha write SetAlpha;
    property Active: Boolean read FActive write SetActive;
    property BlendMode: TBlendMode read FBlendMode write SetBlendMode;
    property DesignActive: Boolean read FDesignActive write SetDesignActive;
    property TransparentColor: TColor read FColorKey write SetColorKey;
    { Published declarations }
  end;

//Import Windows2K Function
function SetLayeredWindowAttributes(hWnd: HWND; crKey: TColor; bAlpha: Byte; dwFlags: Integer): Integer; stdcall; external user32;

procedure Register;

implementation

const
 WS_EX_LAYERED = $80000;
 LWA_COLORKEY = $01;
 LWA_ALPHA = $02;

 
procedure TBlend.Activate;
var
 FormHandle: HWND;
 IBlendMode: Integer;
begin
 FormHandle := (Owner as TWinControl).Handle;
  If (Active and not (csDesigning in ComponentState)) or
     ((csDesigning in ComponentState) and DesignActive) Then begin
   If not IsLayeredWindow(FormHandle) Then
   SetLayeredWindow(FormHandle, True);
    case FBlendMode of
     bmLWA_COLORKEY: IBlendMode := LWA_COLORKEY;
     bmLWA_ALPHA: IBlendMode := LWA_ALPHA;
     bmBoth: IBlendMode := LWA_ALPHA + LWA_COLORKEY;
    end;
      SetLayeredWindowAttributes(FormHandle, FColorKey, FAlpha, IBlendMode);
  end;
  If (not Active and not (csDesigning in ComponentState)) or
   ((csDesigning in ComponentState) and not DesignActive) Then begin
 SetLayeredWindowAttributes(FormHandle, 0, 255, LWA_ALPHA);
 SetLayeredWindow(FormHandle, False);
 end;
end;


procedure TBlend.SetLayeredWindow(hWnd: hWnd; bIsLayered: Boolean);
var l: Integer;
begin
 l := GetWindowLong(hWnd, GWL_EXSTYLE);
   If bIsLayered = True Then
     l := l Or WS_EX_LAYERED
   else
     l := l And Not WS_EX_LAYERED;
  SetWindowLong(hWnd, GWL_EXSTYLE, l);
end;

function TBlend.IsLayeredWindow(hWnd: hWnd): Boolean;
var l: Integer;
begin
 l := GetWindowLong(hWnd, GWL_EXSTYLE);
    If (l and WS_EX_LAYERED) = WS_EX_LAYERED Then
        IsLayeredWindow := True
    else
        IsLayeredWindow := False;
end;

procedure TBlend.SetAlpha(Value: Byte);
begin
 If FAlpha <> Value Then begin
  FAlpha := Value;
  Activate;
 end;
end;

procedure TBlend.SetActive(Value: Boolean);
begin
 If FActive <> Value Then begin
  FActive := Value;
  Activate;
 end;
end;

procedure TBlend.SetBlendMode(Value: TBlendMode);
begin
 If FBlendMode <> Value Then begin
  FBlendMode := Value;
  Activate;
 end;
end;

procedure TBlend.SetColorKey(Value: TColor);
begin
 If FColorKey <> Value Then begin
  FColorKey := Value;
  Activate;
 end;
end;

procedure TBlend.SetDesignActive(Value: Boolean);
begin
 If (FDesignActive <> Value) and (csDesigning in ComponentState) Then begin
  FDesignActive := Value;
  Activate;
 end;
end;

procedure TBlend.ProcTimer(Sender: TObject);
begin
 If FShow Then
   If (FAlphaFirst - FAlpha) <= FStep Then begin
    FTimer.Enabled := False;
    Alpha := FAlphaFirst;
   end else Alpha := FAlpha + FStep
 else
   If (FAlphaFirst - FAlpha) >= FStep*16 Then begin
    (Owner as TForm).Close;
    FTimer.Enabled := False;
    Active := False;
    FAlpha := FAlphaFirst;
  end else Alpha := FAlpha - FStep;
end;

procedure TBlend.ShowWindow(Show: Boolean);
begin
 FAlphaFirst := FAlpha;
 FStep := FAlphaFirst div 16;
 FShow := Show;
 FActive := True;
 If FShow Then begin
  Alpha := 0;
  (Owner as TForm).Show;
 end;
 FTimer.Enabled := True;
end;

constructor TBlend.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FTimer := TTimer.Create(Self);
 FTimer.Interval := 10;
 FTimer.OnTimer := ProcTimer;
 FTimer.Enabled := False;
end;

destructor TBlend.Destroy;
begin
 If csDesigning in ComponentState Then
 Active := False;
 DesignActive := False;
 FTimer.Free;
 inherited Destroy;
end;

procedure Register;
begin
  RegisterComponents('SESAM', [TBlend]);
end;

end.
