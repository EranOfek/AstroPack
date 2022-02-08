//
// TLogPanel, by Chen Tishler, 1998-2022
//

unit LogPanel;

interface

uses
    Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, Forms;
  
type
  // TLogPanelMsg
  TLogPanelMsg = class
  public
    constructor Create(APrompt: String; AText: String;
      ATextColor: TColor = clBlack; ABackColor: TColor = clWhite);

  public
    Prompt : String;
    Text : String;
    TextColor : TColor;
    BackColor : TColor;
  end;


  // TLogPanel
  TLogPanel = class(TPanel)
  private
    FThreadList : TThreadList;         // List of lines
    FMaxLines : Integer;               // Max number of lines
    FMaxLength : Integer;              // Max line length (of lines in List)
    FUpdateInterval : Integer;
    FAddAtEnd : Boolean;               // true to Add add end of list, otherwise top of list
    FAutoScroll : Boolean;
    FPaused : Boolean;
    FDefColor : TColor;                // Default text color
    FDefBack : TColor;                 // Default text background
    FTimeFormat : String;              // DateTime format string
    FTimeMS : Boolean;                 // True to display milliseconds
    FTimeTickCount : Boolean;          // True to display TickCount instead of DateTime
    FTimeDiff : Boolean;               // True to display elapsed time since last log
    FLastTickCount : DWord;            // TickCount of last log message
    FScrollLines : Integer;
    FScrollLength : Integer;
    FPaintBox : TPaintBox;
    FHScrollBar : TScrollBar;
    FVScrollBar : TScrollBar;
    FUpdateTimer : TTimer;
    FLineBuf: String;
    FTimePrompt: Boolean;

  protected
    procedure ClearList;
    procedure Paint; override;
    procedure SetScrollBars;

  private
    procedure PaintBoxPaint(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure UpdateTimerEvent(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X: Integer; Y: Integer);

    function TimeStr : String;

  public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Add message to log
      procedure Add(MsgText: String; TextColor: TColor = $7FFFFFFF;
        BackColor: TColor = $7FFFFFFF);

      // Add message to log
      procedure AddLn(MsgText: String; TextColor: TColor = $7FFFFFFF;
        BackColor: TColor = $7FFFFFFF);

      procedure Clear;


  published
    property MaxLines: Integer read FMaxLines  write FMaxLines;
    property UpdateInterval: Integer read FUpdateInterval write FUpdateInterval;
    property AddAtEnd: Boolean read FAddAtEnd write FAddAtEnd;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
    property DefColor: TColor read FDefColor write FDefColor;
    property DefBack: TColor read FDefBack write FDefBack;
    property TimeFormat: String read FTimeFormat write FTimeFormat;
    property TimeMS: Boolean read FTimeMS write FTimeMS;
    property TimeTickCount: Boolean read FTimeTickCount write FTimeTickCount;
    property TimeDiff: Boolean read FTimeDiff write FTimeDiff;
    property Paused: Boolean read FPaused write FPaused;
    property TimePrompt: Boolean read FTimePrompt write FTimePrompt;
  end;


implementation

// TLogPanelMsg
constructor TLogPanelMsg.Create(APrompt: String; AText: String;
      ATextColor: TColor; ABackColor: TColor);
begin
  Prompt := APrompt;
  Text := AText;
  TextColor := ATextColor;
  BackColor := ABackColor;
end;


// TLogPanel
constructor TLogPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDefColor := clBlack;
  FDefBack  := clWhite;

  FThreadList := TThreadList.Create;

  MaxLines        := 2500;             // (28/12/11) changed from 500 to 2500
  FMaxLength      := 0;
  FUpdateInterval := 50;               // (28/12/11) changed from 0 to 50, to be thread-safe
  AddAtEnd        := true;
  AutoScroll      := true;
  TimeFormat      := 'hh:nn:ss';
  TimeMS          := false;
  TimeTickCount   := false;
  TimeDiff        := false;
  Paused          := false;
  TimePrompt      := true;

  FLastTickCount  := 0;
  FScrollLines    := 0;
  FScrollLength   := 0;

  Caption         := '';
  Color           := clWhite;

  // Create Horizonatal scroll bar
  FHScrollBar := TScrollBar.Create(Self);
  FHScrollBar.Kind       := sbHorizontal;
  FHScrollBar.Align      := alBottom;
  FHScrollBar.Height     := 12;
  FHScrollBar.OnChange   := @ScrollBarChange;
  FHScrollBar.Visible    := false;


  // Create Vertical scroll bar
  FVScrollBar := TScrollBar.Create(Self);
  FVScrollBar.Kind       := sbVertical;
  FVScrollBar.Align      := alRight;
  FVScrollBar.Width      := 12;
  FVScrollBar.OnChange   := @ScrollBarChange;
  FVScrollBar.Visible    := true;

  // Create PaintBox
  FPaintBox := TPaintBox.Create(Self);
  FPaintBox.Align        := alClient;
  FPaintBox.Color        := clWhite;
  FPaintBox.OnPaint      := @PaintBoxPaint;
  FPaintBox.OnMouseDown  := @PaintBoxMouseDown;
  FPaintBox.Visible      := true;

  InsertControl(FHScrollBar);
  InsertControl(FVScrollBar);
  InsertControl(FPaintBox);

  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Enabled := false;
  FUpdateTimer.OnTimer := @UpdateTimerEvent;
end;


destructor TLogPanel.Destroy;
begin
    ClearList;
    FThreadList.Destroy;
end;


procedure TLogPanel.Add(MsgText: String; TextColor: TColor = $7FFFFFFF;
    BackColor: TColor = $7FFFFFFF);
var
  List: TList;
  Msg, OldMsg: TLogPanelMsg;
  Prompt, AText: String;

begin
  try
    if FTimePrompt then
      Prompt := TimeStr;

    AText   := Prompt + MsgText;

    if (FMaxLength < Length(AText)) then
      FMaxLength := Length(AText);

    if (TextColor = $7FFFFFFF) then
      TextColor := FDefColor;

    if (BackColor = $7FFFFFFF) then
      BackColor := FDefBack;

    Msg := TLogPanelMsg.Create(Prompt, MsgText, TextColor, BackColor);

    // Add to list
    List := FThreadList.LockList;

    if FAddAtEnd then
    begin
      // Remove first line if list is full
      if (List.Count >= FMaxLines) then
      begin
        OldMsg := TObject(List.Items[0]) as TLogPanelMsg;
        OldMsg.Destroy;
        List.Delete(0);
      end;

      // Add at end
      List.Add(Msg);
    end
    else
    begin
      // Remove last line if list is full
      if (List.Count >= FMaxLines) then
      begin
        OldMsg := TObject(List.Items[List.Count-1]) as TLogPanelMsg;
        OldMsg.Destroy;
        List.Delete(List.Count-1);
      end;

      // Insert at top
      List.Insert(0, Msg);
    end;

    FThreadList.UnlockList;

    //
    if FAutoScroll and not FPaused then
    begin
      // Trigger timer or update now
      if (FUpdateInterval > 0) then
      begin
        FUpdateTimer.Interval := FUpdateInterval;
        FUpdateTimer.Enabled  := true;
      end
      else
      begin
        SetScrollBars;
        FPaintBox.Invalidate;
      end;
    end;

  finally
  end;

end;


procedure TLogPanel.AddLn(MsgText: String; TextColor: TColor = $7FFFFFFF;
    BackColor: TColor = $7FFFFFFF);
var
  p: Integer;
begin
  MsgText := StringReplace(MsgText, #13, '', [rfReplaceAll]);
  while Length(MsgText) > 0 do
  begin
    p:= Pos(#10, MsgText);
    if p > 0 then
    begin
      FLineBuf := FLineBuf + Copy(MsgText, 1, p-1);
      Add(FLineBuf, TextColor, BackColor);
      FLineBuf := '';
      MsgText := Copy(MsgText, p+1, Length(MsgText) - p);
    end
    else
    begin
      FLineBuf := FLineBuf + MsgText;
      break;
    end;
  end;
end;


procedure TLogPanel.ClearList;
var
  List: TList;
  Msg: TLogPanelMsg;

begin
  List := FThreadList.LockList;

  while (List.Count > 0) do
  begin
    Msg := TObject(List.Items[0]) as TLogPanelMsg;
    Msg.Destroy;
    List.Delete(0);
  end;
  List.Clear;
  FThreadList.UnlockList;

  FMaxLength := 0;
end;


procedure TLogPanel.Clear;
begin
  ClearList;
  SetScrollBars;
  Invalidate;
end;


procedure TLogPanel.Paint;
begin
end;

procedure TLogPanel.SetScrollBars;
var
  List: TList;
  ListCount, LetterWidth, LetterHeight,
  Letters, Lines, MaxLine: Integer;

begin
  // Get number of lines in list
  List := FThreadList.LockList;
  ListCount := List.Count;
  FThreadList.UnlockList;

  // Nothing to do
  if ((ListCount = FScrollLines) and (FMaxLength = FScrollLength)) then
    exit;

  LetterWidth  := Canvas.TextWidth('X');
  LetterHeight := Canvas.TextHeight('X');

  // Sometimes in multi-threading app, we get zero (28/12/11)
  if ((LetterWidth < 1) or (LetterHeight < 1)) then
    exit;

  Letters := (ClientWidth - FVScrollBar.Width) div LetterWidth;
  Lines   := (ClientHeight - FHScrollBar.Height) div LetterHeight;
  MaxLine := ListCount - Lines;
  if (MaxLine < 0) then
      MaxLine := 0;

  // Set Horizontal scroll bar
  if (Letters < FMaxLength) then
  begin
    FHScrollBar.Min := 0;
    FHScrollBar.Max := FMaxLength - Letters;
    FHScrollBar.Visible := true;
  end
  else
  begin
    FHScrollBar.Visible := false;
  end;

  // Vertical
  FVScrollBar.Min := 0;
  FVScrollBar.Max := MaxLine + 2;

  FScrollLines  := ListCount;
  FScrollLength := FMaxLength;

  if FAutoScroll then
  begin
    if FAddAtEnd then
      FVScrollBar.Position := MaxLine + 2
    else
      FVScrollBar.Position := 1;
  end;
end;


procedure TLogPanel.PaintBoxPaint(Sender: TObject);
var
  List: TList;
  i, Y, First, Last, hPos, PromptWidth, TextWidth, TextHeight: Integer;
  ARect: TRect;
  Msg: TLogPanelMsg;
  AText: String;

begin
   List := FThreadList.LockList;
   Y := 0;

    if (List.Count > 0) then
    begin

        TextHeight := Canvas.TextHeight('A') + 2;

        // (28/12/11)
        if (TextHeight < 4) then
            TextHeight := 4;

        //
        First := FVScrollBar.Position;
        Last := First + (ClientHeight - FHScrollBar.Height) div TextHeight;

        if (Last >= List.Count) then
            Last := List.Count - 1;

        HPos := FHScrollBar.Position;

        for i := First to Last do
        begin

            // Get pointer to message
            Msg := TObject(List.Items[i]) as TLogPanelMsg;

            PromptWidth  := Canvas.TextWidth(Msg.Prompt);
            TextWidth    := FPaintBox.ClientWidth - PromptWidth;

            // Write propmpt
            if (PromptWidth > 0) then
            begin
              FPaintBox.Canvas.Brush.Color := Color;
              FPaintBox.Canvas.Font.Color  := Font.Color;

              // Fill background
              // @Todo: replace .Create with Rect() !!!!
              ARect := Rect(0, Y, PromptWidth - 1, Y+TextHeight);
              FPaintBox.Canvas.FillRect(ARect);

              // Write text
              FPaintBox.Canvas.TextOut(2, Y + 2, Msg.Prompt);
            end;

            // Set colors
            FPaintBox.Canvas.Brush.Color := Msg.BackColor;
            FPaintBox.Canvas.Font.Color  := Msg.TextColor;

            // Fill background
            ARect := Rect(PromptWidth, Y, PromptWidth + TextWidth, Y+TextHeight);
            FPaintBox.Canvas.FillRect(ARect);

            // Write text
            //int Len = Msg.Text.Length();  // - HPos - Msg.Prompt.Length();
            //if (Len < 0) Len = 0;
            AText := Msg.Text.SubString(HPos, Length(Msg.Text));
            FPaintBox.Canvas.TextOut(PromptWidth + 2, Y + 2, AText);

            Y := Y + TextHeight;
        end;
    end;

    FThreadList.UnlockList;

    // Fill bottom of window
    ARect := Rect(0, Y, FPaintBox.ClientWidth, FPaintBox.Height);
    FPaintBox.Canvas.Brush.Color := Color;
    FPaintBox.Canvas.FillRect(ARect);
end;


procedure TLogPanel.ScrollBarChange(Sender: TObject);
begin
    Invalidate;
end;


procedure TLogPanel.UpdateTimerEvent(Sender: TObject);
begin
    FUpdateTimer.Enabled := false;

    SetScrollBars;
    Invalidate;
end;


procedure TLogPanel.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
    if (Button = mbRight) then
    begin
      Paused := true;
      FPaintBox.Cursor := crHandPoint;
    end
    else
    begin
      Paused := false;
      FPaintBox.Cursor := crDefault;
    end;
end;


function TLogPanel.TimeStr : String;
var
  Str: String;
  Tick: DWord;
  DT: TDateTime;
  Hour, Minute, Second, MS: Word;
begin
    Tick := GetTickCount();
    if (FLastTickCount = 0) then
        FLastTickCount := Tick;

    if FTimeTickCount then
    begin
      Str := Format('%10u', [Tick]);
    end
    else if (FTimeFormat <> '') then
    begin
        DT := Now();
        Str := FormatDateTime(FTimeFormat, DT);

        // Add Miliseconds
        if FTimeMS then
        begin
            DecodeTime(DT, Hour, Minute, Second, MS);
            Str := Str + Format('.%02d', [MS div 10]);
        end;
    end;

    if FTimeDiff then
    begin
		  if (Str = '') then
        Str := Str + Format('[%6u]', [Tick - FLastTickCount])
      else
        Str := Str + Format(' [%6u]', [Tick - FLastTickCount]);
    end;

    FLastTickCount := Tick;

    if (Str <> '') then
        Str := Str + ' > ';

    Result := Str;
end;


end.

