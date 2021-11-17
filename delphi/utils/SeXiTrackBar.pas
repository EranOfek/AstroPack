{================================================================

    XiTrackBar 1.00
    Written by Eugene Genev

=================================================================}


unit SeXiTrackBar;

interface

uses
  Windows, Classes, Controls, Graphics, Messages, //Chen Types,
  Forms, Dialogs,
  Math, SysUtils, ExtCtrls, ComCtrls;

type
  TColorScheme = (csCustom, csDesert, csGrass, csSilver, csSky, csRose, csSun);
  TBtnState = (bsUp, bsOver, bsDown, bsOut);
  TFillDirection = (fdHorizontal, fdVertical, fdDiagonal);
  TXiTrackBar = class(TCustomControl)
  private
    FBackColor: TColor;
    FTickColor: TColor;
    FDisabledTickColor: TColor;
    FSlideBorderColor: TColor;
    FSlideFaceColor: TColor;
    FSlideGradColor: TColor;
    FDisabledSlideBorderColor: TColor;
    FDisabledSlideFaceColor: TColor;
    FDisabledSlideGradColor: TColor;
    FThumbBorderColor: TColor;
    FThumbFaceColor: TColor;
    FThumbGradColor: TColor;
    FOverThumbBorderColor: TColor;
    FOverThumbFaceColor: TColor;
    FOverThumbGradColor: TColor;
    FDownThumbBorderColor: TColor;
    FDownThumbFaceColor: TColor;
    FDownThumbGradColor: TColor;
    FDisabledThumbBorderColor: TColor;
    FDisabledThumbFaceColor: TColor;
    FDisabledThumbGradColor: TColor;

    FColorScheme: TColorScheme;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FFrequency: Integer;
    FOrientation: TTrackBarOrientation;
    FTickMarks: TTickMark;
    FTickStyle: TTickStyle;
    FSmoothCorners: Boolean;
    FOnChange: TNotifyEvent;

    //Chen
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;

    FThumbState: TBtnState;
    FSlideRect: TRect;
    FThumbRect: TRect;
    FAbsLength: Integer;
    FAbsPos: Integer;
    FThumbWidth: Integer;
    FThumbLength: Integer;

    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetPosition(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetFrequency(Value: Integer);
    procedure SetThumbLength(Value: Integer);
    procedure SetThumbWidth(Value: Integer);
    procedure SetTickStyle(Value: TTickStyle);
    procedure SetTickMarks(Value: TTickMark);
    procedure SetOrientation(Value: TTrackBarOrientation);
    procedure SetSmoothCorners(Value: Boolean);
    procedure SetColorScheme(Value: TColorScheme);
    function PointInRect(X, Y: Integer; R: TRect): Boolean;
  protected
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure MouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove (Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure CMEnabledChanged (var msg: TMessage); message CM_ENABLEDCHANGED;
  public
    procedure GradientFillRect(Canvas: TCanvas; Rect: TRect;
                StartColor, EndColor: TColor; Direction: TFillDirection);
    function GetColorScheme: TStringList;
  published
    property BackColor: TColor index 0 read FBackColor write SetColors;
    property TickColor: TColor index 1 read FTickColor write SetColors;
    property DisabledTickColor: TColor index 2 read FDisabledTickColor write SetColors;
    property SlideBorderColor: TColor index 10 read FSlideBorderColor write SetColors;
    property SlideFaceColor: TColor index 11 read FSlideFaceColor write SetColors;
    property SlideGradColor: TColor index 12 read FSlideGradColor write SetColors;
    property DisabledSlideBorderColor: TColor index 13 read FDisabledSlideBorderColor write SetColors;
    property DisabledSlideFaceColor: TColor index 14 read FDisabledSlideFaceColor write SetColors;
    property DisabledSlideGradColor: TColor index 15 read FDisabledSlideGradColor write SetColors;
    property DisabledThumbBorderColor: TColor index 16 read FDisabledThumbBorderColor write SetColors;
    property DisabledThumbFaceColor: TColor index 17 read FDisabledThumbFaceColor write SetColors;
    property DisabledThumbGradColor: TColor index 18 read FDisabledThumbGradColor write SetColors;
    property ThumbBorderColor: TColor index 20 read FThumbBorderColor write SetColors;
    property ThumbFaceColor: TColor index 21 read FThumbFaceColor write SetColors;
    property ThumbGradColor: TColor index 22 read FThumbGradColor write SetColors;
    property OverThumbBorderColor: TColor index 30 read FOverThumbBorderColor write SetColors;
    property OverThumbFaceColor: TColor index 31 read FOverThumbFaceColor write SetColors;
    property OverThumbGradColor: TColor index 32 read FOverThumbGradColor write SetColors;
    property DownThumbBorderColor: TColor index 40 read FDownThumbBorderColor write SetColors;
    property DownThumbFaceColor: TColor index 41 read FDownThumbFaceColor write SetColors;
    property DownThumbGradColor: TColor index 42 read FDownThumbGradColor write SetColors;
    property SmoothCorners: Boolean read FSmoothCorners write SeTSmoothCorners;
    property ColorScheme: TColorScheme read FColorScheme write SetColorScheme;
    property Position: Integer read FPosition write SetPosition;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Frequency: Integer read FFrequency write SetFrequency;
    property TickStyle: TTickStyle read FTickStyle write SetTickStyle;
    property TickMarks: TTickMark read FTickMarks write SetTickMarks;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation;

    property Align;
    property Anchors;
    property BorderWidth;
    property Constraints;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HelpContext;
    // Chen property HelpKeyword;
    // Chen property HelpType;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    // property TabOrder;
    // property TabStop;
    property Tag;
    property Visible;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    // Chen
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
end;


procedure Register;

{$R SeXiTrackBar.res}

implementation

procedure Register;
begin
  RegisterComponents('SESAM', [TXiTrackBar]);
end;

constructor TXiTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  Width:= 100;
  Height:= 28;
  FThumbLength:= 20;
  FThumbWidth:= 10;
  FMin:= 0;
  FMax:= 10;
  FFrequency:= 1;
  FSmoothCorners:= true;
  FBackColor:= clBtnFace;
  ColorScheme:= csDesert;
  
  FThumbState:= bsOut;
end;

procedure TXiTrackBar.Paint;
var
  SlideBorderColor, SlideFaceColor, SlideGradColor, TickColor: TColor;
  ThumbBorderColor, ThumbFaceColor, ThumbGradColor: TColor;
  ScrBmp: TBitmap;
  i: integer;
begin
  SlideBorderColor:= FSlideBorderColor;
  SlideFaceColor:= FSlideFaceColor;
  SlideGradColor:= FSlideGradColor;
  TickColor:= FTickColor;
  Color:= FBackColor;

  case FThumbState of
    bsOut:    begin
                ThumbBorderColor:= FThumbBorderColor;
                ThumbFaceColor:= FThumbFaceColor;
                ThumbGradColor:= FThumbGradColor;
              end;
    bsOver:   begin
                ThumbBorderColor:= FOverThumbBorderColor;
                ThumbFaceColor:= FOverThumbFaceColor;
                ThumbGradColor:= FOverThumbGradColor;
              end;
    bsDown:   begin
                ThumbBorderColor:= FDownThumbBorderColor;
                ThumbFaceColor:= FDownThumbFaceColor;
                ThumbGradColor:= FDownThumbGradColor;
              end;
  end;

  if not Enabled then begin
    SlideBorderColor:= FDisabledSlideBorderColor;
    SlideFaceColor:= FDisabledSlideFaceColor;
    SlideGradColor:= FDisabledSlideGradColor;
    ThumbBorderColor:= FDisabledThumbBorderColor;
    ThumbFaceColor:= FDisabledThumbFaceColor;
    ThumbGradColor:= FDisabledThumbGradColor;
    TickColor:= FDisabledTickColor;
  end;

  ScrBmp:= TBitmap.Create;
  ScrBmp.Width:= ClientWidth;
  ScrBmp.Height:= ClientHeight;

  ScrBmp.Canvas.Brush.Style:= bsSolid;
  ScrBmp.Canvas.Brush.Color:= Color;
  ScrBmp.Canvas.Rectangle(-1, -1, ScrBmp.Width+1, ScrBmp.Height+1);

  if FOrientation = trHorizontal then begin
    FThumbLength:= ClientHeight - 8;
    FThumbWidth:= FThumbLength div 2;
    FAbsLength:= ClientWidth - FThumbWidth;

    FThumbRect.Top:= 4;
    FThumbRect.Bottom:= FThumbRect.Top + FThumbLength;
    FThumbRect.Left:= FAbsPos;
    FThumbRect.Right:= FThumbRect.Left + (FThumbRect.Bottom - FThumbRect.Top) div 2;

    FSlideRect.Left:= 0;
    FSlideRect.Right:= ClientWidth;
    FSlideRect.Top:= ClientHeight div 3 + 1;
    FSlideRect.Bottom:= ClientHeight - FSlideRect.Top;
  end else begin
    FThumbLength:= ClientWidth - 8;
    FThumbWidth:= FThumbLength div 2;
    FAbsLength:= ClientHeight - FThumbWidth;

    FPosition:= Round(FAbsPos * (FMax - FMin) / FAbsLength) + FMin;
    FAbsPos:= Round((FAbsLength / (FMax - FMin)) * (FPosition - FMin));

    FThumbRect.Left:= 4;
    FThumbRect.Right:= FThumbRect.Left + FThumbLength;
    FThumbRect.Top:= FAbsPos;
    FThumbRect.Bottom:= FThumbRect.Top + FThumbWidth;

    FSlideRect.Left:= ClientWidth div 3 + 1;
    FSlideRect.Right:= ClientWidth - FSlideRect.Left;
    FSlideRect.Top:= 0;
    FSlideRect.Bottom:= ClientHeight;
  end;


  with ScrBmp.Canvas do begin
    Brush.Style:= bsClear;

    if FOrientation = trHorizontal then GradientFillRect(ScrBmp.Canvas, FSlideRect, SlideFaceColor, SlideGradColor, fdVertical)
    else GradientFillRect(ScrBmp.Canvas, FSlideRect, SlideFaceColor, SlideGradColor, fdHorizontal);

    Pen.Color:= SlideBorderColor;
    Rectangle(FSlideRect.Left, FSlideRect.Top, FSlideRect.Right, FSlideRect.Bottom);

    if FSmoothCorners then begin
      Pixels[FSlideRect.Left, FSlideRect.Top]:= FBackColor;
      Pixels[FSlideRect.Left, FSlideRect.Bottom-1]:= FBackColor;
      Pixels[FSlideRect.Right-1, FSlideRect.Top]:= FBackColor;
      Pixels[FSlideRect.Right-1, FSlideRect.Bottom-1]:= FBackColor;
    end;

    if FOrientation = trHorizontal then GradientFillRect(ScrBmp.Canvas, FThumbRect, ThumbFaceColor, ThumbGradColor, fdHorizontal)
    else GradientFillRect(ScrBmp.Canvas, FThumbRect, ThumbFaceColor, ThumbGradColor, fdVertical);

    Pen.Color:= ThumbBorderColor;
    Rectangle(FThumbRect.Left, FThumbRect.Top, FThumbRect.Right, FThumbRect.Bottom);

    if FSmoothCorners then begin
      Pixels[FThumbRect.Left, FThumbRect.Top]:= FBackColor;
      Pixels[FThumbRect.Left, FThumbRect.Bottom-1]:= FBackColor;
      Pixels[FThumbRect.Right-1, FThumbRect.Top]:= FBackColor;
      Pixels[FThumbRect.Right-1, FThumbRect.Bottom-1]:= FBackColor;
    end;

    for i:= 0 to (FMax-FMin) do begin
      if FTickStyle = tsNone then break;
      if FTickStyle = tsManual then if not ((i = 0) or (i = FMax-FMin)) then continue;
      if not ((i = FMax-FMin)) then if FFrequency <> 0 then if i div FFrequency * FFrequency <> i then continue;

      Pen.Color:= TickColor;
      if FOrientation = trHorizontal then begin
        if (FTickMarks = tmTopLeft) or (FTickMarks = tmBoth) then begin
          MoveTo(Round(FAbsLength * i / (FMax - FMin) + FThumbWidth / 2), 0);
          LineTo(Round(FAbsLength * i / (FMax - FMin) + FThumbWidth / 2), 3);
        end;
        if (FTickMarks = tmBottomRight) or (FTickMarks = tmBoth) then begin
          MoveTo(Round(FAbsLength * i / (FMax - FMin)) + FThumbWidth div 2, ScrBmp.Height-3);
          LineTo(Round(FAbsLength * i / (FMax - FMin)) + FThumbWidth div 2, ScrBmp.Height);
        end;
      end else begin
        if (FTickMarks = tmTopLeft) or (FTickMarks = tmBoth) then begin
          MoveTo(0, Round(FAbsLength * i / (FMax - FMin)) + (FThumbRect.Bottom - FThumbRect.Top) div 2);
          LineTo(3, Round(FAbsLength * i / (FMax - FMin)) + (FThumbRect.Bottom - FThumbRect.Top) div 2);
        end;
        if (FTickMarks = tmBottomRight) or (FTickMarks = tmBoth) then begin
          MoveTo(ScrBmp.Width-3, Round(FAbsLength * i / (FMax - FMin)) + (FThumbRect.Bottom - FThumbRect.Top) div 2);
          LineTo(ScrBmp.Width, Round(FAbsLength * i / (FMax - FMin)) + (FThumbRect.Bottom - FThumbRect.Top) div 2);
        end;
      end;
    end;
  end;

  Canvas.Draw(0, 0, ScrBmp);
  ScrBmp.Free;
end;

procedure TXiTrackBar.MouseEnter(var msg: TMessage);
begin
  Paint;
end;

procedure TXiTrackBar.MouseLeave(var msg: TMessage);
begin
  if not Enabled then Exit;
  if FThumbState <> bsDown then FThumbState:= bsOut;
  Paint;
end;

procedure TXiTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then Exit;
  if PointInRect(X, Y, FThumbRect) then FThumbState:= bsDown;
  Paint;

  //Chen
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);

end;

procedure TXiTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then Exit;
  if PointInRect(X, Y, FThumbRect) then FThumbState:= bsOver
  else FThumbState:= bsOut;
  Paint;

  //Chen
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TXiTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then Exit;
  if FThumbState <> bsDown then
    if PointInRect(X, Y, FThumbRect) then FThumbState:= bsOver
    else FThumbState:= bsOut;

  if FThumbState = bsDown then begin
    if FOrientation = trHorizontal then begin
      FAbsPos:= X - FThumbWidth div 2;
      if FAbsPos < 0 then FAbsPos:= 0;
      if FAbsPos > FAbsLength then FAbsPos:= FAbsLength;
    end else begin
      FAbsPos:= Y - 5;
      if FAbsPos < 0 then FAbsPos:= 0;
      if FAbsPos > FAbsLength then FAbsPos:= FAbsLength;
    end;

    FPosition:= Round(FAbsPos * (FMax - FMin) / FAbsLength) + FMin;
    FAbsPos:= Round((FAbsLength / (FMax - FMin)) * (FPosition - FMin));
   if Assigned(FOnChange) then FOnChange(self);
  end;

  Paint;

  //Chen
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TXiTrackBar.Resize;
begin
  Paint;
end;

procedure TXiTrackBar.CMEnabledChanged(var msg: TMessage);
begin
  inherited;
  Paint;
end;

procedure TXiTrackBar.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0:  FBackColor:= Value;
    1:  FTickColor:= Value;
    2:  FDisabledTickColor:= Value;
    10: FSlideBorderColor:= Value;
    11: FSlideFaceColor:= Value;
    12: FSlideGradColor:= Value;
    13: FDisabledSlideBorderColor:= Value;
    14: FDisabledSlideFaceColor:= Value;
    15: FDisabledSlideGradColor:= Value;
    16: FDisabledThumbBorderColor:= Value;
    17: FDisabledThumbFaceColor:= Value;
    18: FDisabledThumbGradColor:= Value;
    20: FThumbBorderColor:= Value;
    21: FThumbFaceColor:= Value;
    22: FThumbGradColor:= Value;
    30: FOverThumbBorderColor:= Value;
    31: FOverThumbFaceColor:= Value;
    32: FOverThumbGradColor:= Value;
    40: FDownThumbBorderColor:= Value;
    41: FDownThumbFaceColor:= Value;
    42: FDownThumbGradColor:= Value;
  end;
  FColorScheme:= csCustom;
  Paint;
end;

procedure TXiTrackBar.SetPosition(Value: Integer);
begin
  if (Value < FMin) then Value:= FMin;
  if (Value > FMax) then Value:= FMax;
  FPosition:= Value;

  //Chen
  if FMax > FMin then
      FAbsPos:= Round((FAbsLength / (FMax - FMin)) * (FPosition - FMin))
  else
      FAbsPos:= FMin;

  Paint;
  if Assigned(FOnChange) then FOnChange(self);
end;

procedure TXiTrackBar.SetMin(Value: Integer);
begin
  FMin:= Value;
  if FPosition < FMin then FPosition:= FMin;

  if FMax > FMin then
      FAbsPos:= Round((FAbsLength / (FMax - FMin)) * (FPosition - FMin))
  else
      FAbsPos:= FMin;

  Paint;
end;

procedure TXiTrackBar.SetMax(Value: Integer);
begin
  FMax:= Value;
  if FPosition > FMax then FPosition:= FMax;

  if FMax > FMin then
      FAbsPos:= Round((FAbsLength / (FMax - FMin)) * (FPosition - FMin))
  else
      FAbsPos:= FMin;
  
  Paint;
end;

procedure TXiTrackBar.SetFrequency(Value: Integer);
begin
  FFrequency:= Value;
  Paint;
end;

procedure TXiTrackBar.SetThumbLength(Value: Integer);
begin
  FThumbLength:= Value;
  Paint;
end;

procedure TXiTrackBar.SetThumbWidth(Value: Integer);
begin
  FThumbWidth:= Value;
  Paint;
end;


procedure TXiTrackBar.SetTickStyle(Value: TTickStyle);
begin
  FTickStyle:= Value;
  Paint;
end;

procedure TXiTrackBar.SetTickMarks(Value: TTickMark);
begin
  FTickMarks:= Value;
  Paint;
end;

procedure TXiTrackBar.SetOrientation(Value: TTrackBarOrientation);
begin
  FOrientation:= Value;
  Paint;
end;

procedure TXiTrackBar.SetSmoothCorners(Value: Boolean);
begin
  FSmoothCorners:= Value;
  Paint;
end;

procedure TXiTrackBar.SetColorScheme(Value: TColorScheme);
begin
  FColorScheme:= Value;
  case FColorScheme of
    csDesert: begin
                FBackColor:=clBtnFace;
                FTickColor:=$00006A9D;
                FSlideBorderColor:=$000082BF;
                FSlideFaceColor:=$0028B9FF;
                FSlideGradColor:=$00BBE9FF;
                FThumbBorderColor:=$00005B88;
                FThumbFaceColor:=$008CDAFF;
                FThumbGradColor:=$000093D9;
                FOverThumbBorderColor:=$00005680;
                FOverThumbFaceColor:=$005ECBFF;
                FOverThumbGradColor:=$00007BB7;
                FDownThumbBorderColor:=$00005680;
                FDownThumbFaceColor:=$000083C1;
                FDownThumbGradColor:=$001AB5FF;
                FDisabledTickColor:=clSilver;
                FDisabledSlideBorderColor:=$00BEBEBE;
                FDisabledSlideFaceColor:=$00D8D8D8;
                FDisabledSlideGradColor:=$00E8E8E8;
                FDisabledThumbBorderColor:=$00B5B5B5;
                FDisabledThumbFaceColor:=$00EAEAEA;
                FDisabledThumbGradColor:=$00CFCFCF;
              end;
    csGrass:  begin
                FBackColor:=clBtnFace;
                FTickColor:=$001D9A4B;
                FSlideBorderColor:=$0020A452;
                FSlideFaceColor:=$003ED978;
                FSlideGradColor:=$00C1F4D6;
                FThumbBorderColor:=$00126732;
                FThumbFaceColor:=$0082E8AA;
                FThumbGradColor:=$0021AB55;
                FOverThumbBorderColor:=$00156F37;
                FOverThumbFaceColor:=$007CE7A7;
                FOverThumbGradColor:=$001E954A;
                FDownThumbBorderColor:=$00156F37;
                FDownThumbFaceColor:=$0020A251;
                FDownThumbGradColor:=$005EE193;
                FDisabledTickColor:=clSilver;
                FDisabledSlideBorderColor:=$00BEBEBE;
                FDisabledSlideFaceColor:=$00D8D8D8;
                FDisabledSlideGradColor:=$00E8E8E8;
                FDisabledThumbBorderColor:=$00B5B5B5;
                FDisabledThumbFaceColor:=$00EAEAEA;
                FDisabledThumbGradColor:=$00CFCFCF;
              end;
    csSky:    begin
                FBackColor:=clBtnFace;
                FTickColor:=$00C88D2D;
                FSlideBorderColor:=$00F47A00;
                FSlideFaceColor:=$00FFBA75;
                FSlideGradColor:=$00FFE9D2;
                FThumbBorderColor:=$00C66300;
                FThumbFaceColor:=$00FFD9B3;
                FThumbGradColor:=$00FF9224;
                FOverThumbBorderColor:=$00B35900;
                FOverThumbFaceColor:=$00FFCF9F;
                FOverThumbGradColor:=$00F97C00;
                FDownThumbBorderColor:=$00B35900;
                FDownThumbFaceColor:=$00FF8C1A;
                FDownThumbGradColor:=$00FFBF80;
                FDisabledTickColor:=clSilver;
                FDisabledSlideBorderColor:=$00BEBEBE;
                FDisabledSlideFaceColor:=$00D8D8D8;
                FDisabledSlideGradColor:=$00E8E8E8;
                FDisabledThumbBorderColor:=$00B5B5B5;
                FDisabledThumbFaceColor:=$00EAEAEA;
                FDisabledThumbGradColor:=$00CFCFCF;
              end;
    csSun:    begin
                FBackColor:=clBtnFace;
                FTickColor:=$0000A4A4;
                FSlideBorderColor:=$000077AA;
                FSlideFaceColor:=$001AD9FB;
                FSlideGradColor:=$00DAFAFE;
                FThumbBorderColor:=$00004F84;
                FThumbFaceColor:=$00C6FFFF;
                FThumbGradColor:=$0000B8E6;
                FOverThumbBorderColor:=$00004F84;
                FOverThumbFaceColor:=$006AFFFF;
                FOverThumbGradColor:=$00009CC4;
                FDownThumbBorderColor:=$00004F84;
                FDownThumbFaceColor:=$0000B8E6;
                FDownThumbGradColor:=$0091E9FF;
                FDisabledTickColor:=clSilver;
                FDisabledSlideBorderColor:=$00BEBEBE;
                FDisabledSlideFaceColor:=$00D8D8D8;
                FDisabledSlideGradColor:=$00E8E8E8;
                FDisabledThumbBorderColor:=$00B5B5B5;
                FDisabledThumbFaceColor:=$00EAEAEA;
                FDisabledThumbGradColor:=$00CFCFCF;
              end;
    csRose:   begin
                FBackColor:=clBtnFace;
                FTickColor:=$005B5BFF;
                FSlideBorderColor:=$004242FF;
                FSlideFaceColor:=$008282FF;
                FSlideGradColor:=$00C6DBFF;
                FThumbBorderColor:=$000000D7;
                FThumbFaceColor:=$009DC2FF;
                FThumbGradColor:=$006666FF;
                FOverThumbBorderColor:=$000000D7;
                FOverThumbFaceColor:=$008CB8FF;
                FOverThumbGradColor:=$005151FF;
                FDownThumbBorderColor:=$000000D7;
                FDownThumbFaceColor:=$006666FF;
                FDownThumbGradColor:=$008CB8FF;
                FDisabledTickColor:=clSilver;
                FDisabledSlideBorderColor:=$00BEBEBE;
                FDisabledSlideFaceColor:=$00D8D8D8;
                FDisabledSlideGradColor:=$00E8E8E8;
                FDisabledThumbBorderColor:=$00B5B5B5;
                FDisabledThumbFaceColor:=$00EAEAEA;
                FDisabledThumbGradColor:=$00CFCFCF;
              end;
    csSilver: begin
                FBackColor:=clBtnFace;
                FTickColor:=$00888888;
                FSlideBorderColor:=$00727272;
                FSlideFaceColor:=clSilver;
                FSlideGradColor:=$00EAEAEA;
                FThumbBorderColor:=$00616161;
                FThumbFaceColor:=$00E1E1E1;
                FThumbGradColor:=$00A3A3A3;
                FOverThumbBorderColor:=$00747474;
                FOverThumbFaceColor:=$00D1D1D1;
                FOverThumbGradColor:=$00959595;
                FDownThumbBorderColor:=$00747474;
                FDownThumbFaceColor:=$00999999;
                FDownThumbGradColor:=$00C1C1C1;
                FDisabledTickColor:=clSilver;
                FDisabledSlideBorderColor:=$00BEBEBE;
                FDisabledSlideFaceColor:=$00D8D8D8;
                FDisabledSlideGradColor:=$00E8E8E8;
                FDisabledThumbBorderColor:=$00B5B5B5;
                FDisabledThumbFaceColor:=$00EAEAEA;
                FDisabledThumbGradColor:=$00CFCFCF;
              end;
  end;
  Invalidate;
end;

function TXiTrackBar.GetColorScheme: TStringList;
const
  t = '                ';
begin
  Result:= TStringList.Create;
  with Result do begin
    Add(t+'FBackColor:='+ColorToString(FBackColor)+';');
    Add(t+'FTickColor:='+ColorToString(FTickColor)+';');
    Add(t+'FSlideBorderColor:='+ColorToString(FSlideBorderColor)+';');
    Add(t+'FSlideFaceColor:='+ColorToString(FSlideFaceColor)+';');
    Add(t+'FSlideGradColor:='+ColorToString(FSlideGradColor)+';');
    Add(t+'FThumbBorderColor:='+ColorToString(FThumbBorderColor)+';');
    Add(t+'FThumbFaceColor:='+ColorToString(FThumbFaceColor)+';');
    Add(t+'FThumbGradColor:='+ColorToString(FThumbGradColor)+';');
    Add(t+'FOverThumbBorderColor:='+ColorToString(FOverThumbBorderColor)+';');
    Add(t+'FOverThumbFaceColor:='+ColorToString(FOverThumbFaceColor)+';');
    Add(t+'FOverThumbGradColor:='+ColorToString(FOverThumbGradColor)+';');
    Add(t+'FDownThumbBorderColor:='+ColorToString(FDownThumbBorderColor)+';');
    Add(t+'FDownThumbFaceColor:='+ColorToString(FDownThumbFaceColor)+';');
    Add(t+'FDownThumbGradColor:='+ColorToString(FDownThumbGradColor)+';');

    Add(t+'FDisabledTickColor:='+ColorToString(FDisabledTickColor)+';');
    Add(t+'FDisabledSlideBorderColor:='+ColorToString(FDisabledSlideBorderColor)+';');
    Add(t+'FDisabledSlideFaceColor:='+ColorToString(FDisabledSlideFaceColor)+';');
    Add(t+'FDisabledSlideGradColor:='+ColorToString(FDisabledSlideGradColor)+';');
    Add(t+'FDisabledThumbBorderColor:='+ColorToString(FDisabledThumbBorderColor)+';');
    Add(t+'FDisabledThumbFaceColor:='+ColorToString(FDisabledThumbFaceColor)+';');
    Add(t+'FDisabledThumbGradColor:='+ColorToString(FDisabledThumbGradColor)+';');
  end;
end;

procedure TXiTrackBar.GradientFillRect(Canvas: TCanvas; Rect: TRect;
                StartColor, EndColor: TColor; Direction: TFillDirection);
var
  Steps: Integer;
  StartR, StartG, StartB, EndR, EndG, EndB: Byte;
  CrrR, CrrG, CrrB: Double;
  IncR, IncG, incB: Double;
  i: integer;
begin
  case Direction of
    fdVertical:   Steps:= Rect.Bottom - Rect.Top;
    fdHorizontal: Steps:= Rect.Right - Rect.Left;
    fdDiagonal:   Steps:= Rect.Bottom - Rect.Top + Rect.Right - Rect.Left;
  end;

  StartR:= GetRValue(StartColor);  EndR:= GetRValue(EndColor);
  StartG:= GetGValue(StartColor);  EndG:= GetGValue(EndColor);
  StartB:= GetBValue(StartColor);  EndB:= GetBValue(EndColor);
                 
  IncR:= (EndR - StartR) / steps;
  IncG:= (EndG - StartG) / steps;
  IncB:= (EndB - StartB) / steps;

  CrrR:= StartR;
  CrrG:= StartG;
  CrrB:= StartB;

  for i:= 0 to Steps-1 do begin
    Canvas.Pen.Color:= RGB(Round(CrrR), Round(CrrG), Round(CrrB));
    case Direction of
      fdVertical:   begin
                      Canvas.MoveTo(Rect.Left, Rect.Top + i);
                      Canvas.LineTo(Rect.Right, Rect.Top + i);
                    end;
      fdHorizontal: begin
                      Canvas.MoveTo(Rect.Left+i, Rect.Top);
                      Canvas.LineTo(Rect.Left+i, Rect.Bottom);
                    end;
      fdDiagonal:   begin
                      Canvas.MoveTo(i, Rect.Top);
                      Canvas.LineTo(Rect.Left, i);
                    end;
    end;
    CrrR:= CrrR + IncR;
    CrrG:= CrrG + IncG;
    CrrB:= CrrB + IncB;
  end;
end;

function TXiTrackBar.PointInRect(X, Y: Integer; R: TRect): Boolean;
begin
  if (X > R.Left) and (X < R.Right) and (Y > R.Top) and (Y < R.Bottom) then
    Result:= true
  else
    Result:= false;
end;


end.
