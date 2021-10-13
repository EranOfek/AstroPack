object SoundManager: TSoundManager
  Left = 170
  Top = 128
  Caption = 'SoundManager'
  ClientHeight = 209
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = HEBREW_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 58
    Height = 13
    Caption = 'Wave File'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 69
    Height = 13
    Caption = 'Background'
  end
  object Label3: TLabel
    Left = 8
    Top = 104
    Width = 26
    Height = 13
    Caption = 'Text'
  end
  object Label4: TLabel
    Left = 272
    Top = 128
    Width = 42
    Height = 13
    Caption = 'Repeat'
  end
  object Button1: TButton
    Left = 8
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Back'
    TabOrder = 0
    OnClick = Button1Click
  end
  object EditWave: TEdit
    Left = 8
    Top = 72
    Width = 369
    Height = 21
    TabOrder = 1
    Text = 'c:\sesam\bin\alarm.wav'
  end
  object Button2: TButton
    Left = 96
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Wave'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 184
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Speak'
    TabOrder = 3
    OnClick = Button3Click
  end
  object EditBack: TEdit
    Left = 8
    Top = 24
    Width = 369
    Height = 21
    TabOrder = 4
    Text = 'c:\sesam\bin\day.wav'
  end
  object EditText: TEdit
    Left = 8
    Top = 120
    Width = 249
    Height = 21
    TabOrder = 5
    Text = 'Call police officer to room 1.'
  end
  object EditRepCount: TEdit
    Left = 328
    Top = 120
    Width = 49
    Height = 21
    TabOrder = 6
    Text = '1'
  end
  object MediaPlayer: TMediaPlayer
    Left = 8
    Top = 192
    Width = 253
    Height = 30
    TabOrder = 7
  end
  object Button4: TButton
    Left = 272
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 8
    OnClick = Button4Click
  end
  object MngTimer: TTimer
    Interval = 250
    OnTimer = MngTimerTimer
    Left = 280
    Top = 192
  end
end
