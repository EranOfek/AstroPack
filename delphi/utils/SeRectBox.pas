unit SeRectBox;
{
	As a thank you for all those magnificent Freeware components I learned from.

	TRectBox is mainly a Canvas that can be dragged and resized during runtime.
	The appearance of the Canvas can be modified using the OnPaint event.
	Use TRectBox whenever you need to let the user define a rectangular area at
	runtime.

	TRectBox is pretty much a standard Graphic control with the exception of the
	simulated focusing mechanism. Focus shift will work automatically between
	TRectBoxes leaving the windows focus were it is. Unfocusing must be done in
	code with the	RemoveAllFocus procedure.

	Paul van Dinther  dinther@geocities.com 02/01/2000
}

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls, CommCtrl;

type
	TFrameType = (ftOuter,ftInner,ftMiddle);
	TDragBox = (dbNone,dbDrag,dbTopLeft,dbTopMid,dbTopRight,dbMidLeft,dbMidRight,dbBottomLeft,dbBottomMid,dbBottomRight);
	TOnPaintEvent = procedure (Sender: TObject; Canvas: TCanvas; Rect: TRect) of object;

	TRectBox = class(TGraphicControl)
//	TRectBox = class(TGraphicControl)
	private
		{ Private declarations }
		FAutoForeGround: Boolean;
		FReadOnly: Boolean;
		FOnPaint: TOnPaintEvent;
		FFrameType: TFrameType;
		FAutoSize: Boolean;
		FCaption: String;
		FHideFrame: Boolean;
		FFrameColor: TColor;
		FBoxSize: Integer;
		FBoxColor: TColor;
		FActiveFrameColor: TColor;
		FPassiveFrameColor: TColor;
		FTextColor: TColor;
		FDrawFrame: Boolean;
		FDrawBoxes: Boolean;
		FShowPos: Boolean;
		FShowSize: Boolean;
		FDragBox: TDragBox;
		FPosX: Integer;
		FPosY: Integer;
		FHasFocus: Boolean;
		procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
		procedure SetFocus(Value: Boolean);
		procedure SetAutoSize(Value: Boolean);
		procedure SetHideFrame(Value: Boolean);
		procedure SetCaption(Value: String);
    procedure SetFrameBox(PLeft,PTop,PWidth,PHeight: Integer);
		procedure SetFrameType(Value: TFrameType);
		procedure SetBoxSize(Value: Integer);
		procedure SetBoxColor(Value: TColor);
		procedure SetActiveFrameColor(Value: TColor);
		procedure SetPassiveFrameColor(Value: TColor);
		procedure SetTextColor(Value: TColor);
		procedure SetDrawFrame(Value: Boolean);
		procedure SetDrawBoxes(Value: Boolean);
		procedure SetShowPos(Value: boolean);
		procedure SetShowSize(Value: Boolean);
		procedure WMLMouseDown(var Message: TMessage); message WM_LBUTTONDOWN;
		procedure WMLMouseUp(var Message: TMessage); message WM_LBUTTONUP;
		procedure MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
		procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
	protected
		procedure paint; override;
	public
		procedure RemoveAllFocus;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		property AutoForeGround: Boolean read FAutoForeGround write FAutoForeGround;
		property AutoSize: Boolean read FAutoSize write SetAutoSize;
		property FrameType: TFrameType read FFrameType write SetFrameType;
		property HidePassiveFrame: Boolean read FHideFrame write SetHideFrame;
		property Caption: String read FCaption write SetCaption;
		property BoxSize: Integer read FBoxSize write SetBoxSize;
		property BoxColor: TColor read FBoxColor write SetBoxColor;
		property ActiveFrameColor: TColor read FActiveFrameColor write SetActiveFrameColor;
		property PassiveFrameColor: TColor read FPassiveFrameColor write SetPassiveFrameColor;
		property DrawFrame: Boolean read FDrawFrame write SetDrawFrame;
		property DrawBoxes: Boolean read FDrawBoxes write SetDrawBoxes;
		property ShowPos: Boolean read FShowPos write SetShowPos;
		property ShowSize: Boolean read FShowSize write SetShowSize;
		property InfoTextColor: TColor read FTextColor write SetTextColor;
		property Focus: Boolean read FHasFocus write SetFocus;
		property Font;
		property OnClick;
		property PopupMenu;
		property OnMouseDown;
		property OnMouseMove;
		property OnMouseUp;
		property OnDblClick;
		property OnStartDrag;
		property OnDragOver;
		property OnDragDrop;
		property OnEndDrag;
		property OnPaint: TOnPaintEvent read FOnPaint write FOnPaint;
	end;

procedure Register;

implementation

procedure Register;
begin
	RegisterComponents('SESAM', [TRectBox]);
end;

function between(PVal1,PVal2,PVal3: Integer):Boolean;
begin
	if (PVal1 > PVal2) and (PVal1 < PVal3) then Result := True
	else if (PVal1 > PVal3) and (PVal1 < PVal2) then result := True
	else Result := False;
end;

constructor TRectBox.Create(AOwner: TComponent);
begin
	inherited create(AOwner);
	FBoxColor := clBlack;
	FActiveFrameColor := clBlack;
	FPassiveFrameColor := clGray;
	FBoxSize := 5;
	FDrawBoxes := True;
	FDrawFrame := True;
	FFrameType := ftOuter;
	FAutoForeGround := True;
	OnMouseMove := MouseMove;
end;

destructor TRectBox.Destroy;
begin
	inherited Destroy;
end;

procedure TRectBox.Paint;
var
	LHalfBox: Integer;
	LRect: TRect;
	LClient: TRect;
begin
	if FAutoSize then SetBounds(Left,Top,Canvas.Textwidth(FCaption),Canvas.TextHeight(FCaption));
	LRect := ClientRect;
	if FFrameType = ftOuter then LHalfBox := 0
	else if FFrameType = ftInner then LHalfBox := FBoxSize
	else LHalfBox := (FBoxSize div 2);

	//Calculate Client area that does not intercept the drag boxes;
	LRect.Left := LRect.Left + LHalfBox + 1;
	LRect.Right := LRect.Right - LHalfBox - 1;
	LRect.Top := LRect.Top - LHalfBox;
	LRect.Bottom := LRect.Bottom - LHalfBox - 1;

	//client Area as defined by the frame drawn
	LClient.Left := LHalfBox;
	LClient.Top := LHalfBox;
	LClient.right := width-1 - LHalfBox;
	LClient.Bottom :=	Height - 1 - LHalfBox;

	if assigned(FOnPaint) then FOnPaint(Self,Canvas,LClient);

	//draw the frame
	if (FHasFocus and DrawFrame) or
		 (not FHasFocus and not FHideFrame) then begin
		if FDrawFrame then begin
			canvas.pen.color := FFrameColor;
			Canvas.MoveTo(LClient.Left,LClient.Top);
			Canvas.LineTo(LClient.Right,LClient.Top);
			Canvas.LineTo(LClient.Right,LClient.Bottom);
			Canvas.LineTo(LClient.Left,LClient.Bottom);
			Canvas.LineTo(LClient.Left,LClient.Top);
		end;
	end;
	//Draw the drag boxes
	LHalfBox := (FBoxSize div 2);
	if FDrawBoxes and (FHasFocus or (csDesigning in ComponentState)) then begin
		Canvas.brush.color := FBoxColor;
		Canvas.FillRect(rect(0,0,FBoxSize,FBoxSize));
		Canvas.FillRect(rect((width div 2) - LHalfBox,0,(width div 2) - LHalfBox + FBoxSize,FBoxSize));
		Canvas.FillRect(rect(width - FBoxSize,0,width,FBoxSize));
		Canvas.FillRect(rect(width - FBoxSize,Height div 2 - LHalfBox,width,Height div 2 - LHalfBox + FBoxSize));
		Canvas.FillRect(rect(width - FBoxSize,Height - FBoxSize,width,Height));
		Canvas.FillRect(rect((width div 2) - LHalfBox,Height - FBoxSize,(width div 2) - LHalfBox + FBoxSize,Height));
		Canvas.FillRect(rect(0,Height - FBoxSize,FBoxSize,Height));
		Canvas.FillRect(rect(0,Height div 2 - LHalfBox,FBoxSize,Height div 2 - LHalfBox + FBoxSize));
	end;

	//Set the fixed font for the optional position information
	Canvas.Font.Color := FTextColor;
	Canvas.Brush.style := bsClear;
	canvas.font.Name := 'MS Sans Serif';
	canvas.font.Style := [];
	canvas.font.height := 10;

	if FShowPos and FHasFocus then begin
		Canvas.Brush.style := bsClear;
		Canvas.TextRect(LRect,FBoxSize+ 2,FBoxSize,'L:' + IntToStr(Left) + ' T:' + IntToStr(Top));
	end;
	if FShowSize and FHasFocus then begin
		Canvas.TextRect(LRect,FBoxSize + 2,FBoxSize + (Canvas.font.Height * 1) + 3,'W:' + IntToStr(Width) + ' H:' + IntToStr(Height));
	end;
	Canvas.Font := Font;
	Canvas.TextRect(LRect,(Width - Canvas.Textwidth(FCaption)) div 2,(Height - Canvas.TextHeight(FCaption)) div 2,FCaption);
end;

procedure TRectBox.SetFrameType(Value: TFrameType);
begin
	if FFrameType = Value then exit;
	FFrameType := Value;
	Invalidate;
end;

procedure TRectBox.SetBoxSize(Value: Integer);
begin
	if Value = FBoxSize then exit;
	if value < 2 then exit;
	if value > 50 then exit;
	FBoxSize := ((Value div 2) * 2) + 1;
	invalidate;
end;

procedure TRectBox.SetBoxColor(Value: Tcolor);
begin
	if Value = FBoxColor then exit;
	FBoxColor := Value;
	invalidate;
end;

procedure TRectBox.SetActiveFrameColor(Value: Tcolor);
begin
	if Value = FActiveFrameColor then exit;
	FActiveFrameColor := Value;
	invalidate;
end;

procedure TRectBox.SetPassiveFrameColor(Value: Tcolor);
begin
	if Value = FPassiveFrameColor then exit;
	FPassiveFrameColor := Value;
	invalidate;
end;

procedure TRectBox.SetTextColor(Value: Tcolor);
begin
	if Value = FTextColor then exit;
	FTextColor := Value;
	invalidate;
end;

procedure TRectBox.SetDrawFrame(Value: Boolean);
begin
	if Value = FDrawFrame then exit;
	FDrawFrame := Value;
	invalidate;
end;


procedure TRectBox.SetDrawBoxes(Value: Boolean);
begin
	if Value = FDrawBoxes then exit;
	FDrawBoxes := Value;
	invalidate;
end;

procedure TRectBox.SetShowPos(Value: boolean);
begin
	if Value = FShowPos then exit;
	FShowPos := Value;
	invalidate;
end;

procedure TRectBox.SetShowSize(Value: Boolean);
begin
	if Value = FShowSize then exit;
	FShowSize := Value;
	invalidate;
end;

procedure TRectBox.WMLMouseDown(var Message: TMessage);
begin
	FPosX := Message.LParamLo;
	FposY := Message.LParamHi;
	if FAutoForeGround then	BringToFront;
	RemoveAllFocus;
	FHasFocus := True;
	FFrameColor := FActiveFrameColor;
	SetCapture(TForm(Owner).handle);
	TForm(Owner).OnMouseMove := FormMouseMove;
	invalidate;
end;

procedure TRectBox.WMLMouseUp(var Message: TMessage);
begin
	releaseCapture;
	FPosX := -1;
	FPosY := -1;
	FDragBox := dbNone;
	invalidate
end;

procedure TRectBox.FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
	LPoint: TPoint;
begin
	//Translate coodinates to object
	LPoint := ScreenToClient( Tform(Owner).ClientToScreen(point(X,Y)));
	MouseMove(Sender,Shift,LPoint.X,Lpoint.Y);
end;

procedure TRectBox.MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
	MidHeightStart,MidHeightEnd: Integer;
	MidWidthStart,MidWidthEnd: Integer;
	SX,SY: Integer;
  StartPoint,EndPoint,MousePoint: TPoint;
begin
	//Prepare some variables
	MidHeightStart := (Height - FBoxSize) div 2;
	MidHeightEnd := MidHeightStart + FBoxSize;
	MidWidthStart := (Width - FBoxSize) div 2;
	MidWidthEnd := MidWidthStart + FBoxSize;
	SX := Left + X;
	SY := Top + Y;
	if not (ssLeft in Shift) then begin
		FDragBox := dbNone;
		//Check if cursor is over any of the most left boxes
		if between(X,0,FBoxSize) then begin
			if between(Y,0,FBoxSize) then begin cursor := crSizeNWSE; FDragBox := dbTopLeft; end
			else if between(Y,MidHeightStart,MidHeightEnd) then begin Cursor := crSizeWE; FDragBox := dbMidLeft; end
			else if between(Y,Height - FBoxSize,Height) then begin cursor := crSizeNESW; FDragBox := dbBottomLeft; end
			else begin Cursor := crDefault; FDragBox := dbNone; end;
		end else if between(X,Width - FBoxSize,Width) then begin
			if between(Y,0,FBoxSize) then begin
				cursor := crSizeNESW;
				FDragBox := dbTopRight;
			end
			else if between(Y,MidHeightStart,MidHeightEnd) then begin Cursor := crSizeWE; FDragBox := dbMidRight; end
			else if between(Y,Height - FBoxSize,Height) then begin cursor := crSizeNWSE; FDragBox := dbBottomRight; end
			else begin Cursor := crDefault; FDragBox := dbNone; end;
		end else if between(X,MidWidthStart,MidWidthEnd) then begin
			if between(Y,0,FBoxSize) then begin cursor := crSizeNS; FDragBox := dbTopMid; end
			else if between(Y,Height - FBoxSize,Height) then begin cursor := crSizeNS; FDragBox := dbBottomMid; end
			else begin Cursor := crDefault; FDragBox := dbNone; end;
		end else if between(X,0,width) and between(Y,0,Height) then begin Cursor := crDefault; FDragBox := dbDrag; end;
	end else begin
		StartPoint := clientToScreen(point(Left,Top));
		EndPoint := clientToScreen(point(Left + Width,Top + Height));
		MousePoint := ClientToScreen(point(X,Y));
		//Make size changes if clicked
		if (FPosX >= 0) and (FPosY >= 0 ) then begin
			if FDragBox = dbTopLeft then begin
				SetFrameBox(Left + X,Top + Y,Width - X,Height - Y);
			end else if FDragBox = dbTopRight then begin
				SetFrameBox(Left,Top + Y,X,Height - Y);
			end else if FDragBox = dbTopMid then begin
				SetFrameBox(Left,Top + Y,Width,Height - Y);
			end else if FDragBox =  dbMidLeft then begin
				SetFrameBox(Left + X,Top,Width -X,Height);
			end else if FDragBox = dbBottomright then begin
				SetFrameBox(Left,Top,X,Y);
			end else if FDragBox = dbBottomLeft then begin
				SetFrameBox(Left + X,Top,Width -X,Y);
			end else if FDragBox = dbBottomMid then begin
				SetFrameBox(Left,Top,Width,Y);
			end else if FDragBox =  dbMidRight then begin
				SetFrameBox(Left,Top,X,Height);
			end else if FDragBox = dbDrag then begin
				SetFrameBox(Left - (FPosX - X),Top - (FPosY - Y),Width,Height);
				invalidate;
			end;
		end;
	end;
end;

procedure TRectBox.SetFrameBox(PLeft,PTop,PWidth,PHeight: Integer);
begin
	if (PWidth < (3 * FBoxSize)) or (PHeight < (3 * FBoxSize)) then exit;
	SetBounds(PLeft,PTop,PWidth,PHeight);
end;

procedure TRectBox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
	inherited;
	Message.Result := DLGC_WANTARROWS;
end;

procedure TRectBox.SetCaption(Value: String);
begin
  if Value = FCaption then exit;
	FCaption := Value;
  invalidate;
end;

procedure TRectBox.SetHideFrame(Value: Boolean);
begin
	if Value = FHideFrame then exit;
  FHideFrame := Value;
  Invalidate;
end;

procedure TRectBox.SetAutoSize(Value: Boolean);
begin
	if Value = FAutoSize then exit;
	FAutoSize := Value;
	invalidate;
end;

procedure TRectBox.RemoveAllFocus;
var
	i: Integer;
	LRectBox: TRectBox;
begin
	for i := 0 to Owner.ComponentCount - 1 do begin
		if Owner.Components[i] is TRectBox then begin
			LRectBox := TRectBox(Owner.Components[i]);
			if LRectBox.FHasFocus then begin
				LRectBox.FHasFocus := False;
				LRectBox.Invalidate;
			end;
		end;
	end;
end;

procedure TRectBox.SetFocus(Value: Boolean);
begin
	if Value = FHasFocus then exit;
	FHasFocus := Value;
	invalidate;
end;


end.
