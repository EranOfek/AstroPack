object ZDBUtil: TZDBUtil
  Left = 51
  Top = 108
  Caption = 'ZDBUtil'
  ClientHeight = 163
  ClientWidth = 437
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 225
    Height = 113
    Caption = 'Panel1'
    TabOrder = 0
    object StrAll: TLabel
      Left = 16
      Top = 16
      Width = 11
      Height = 13
      Caption = 'All'
    end
  end
  object DBGrid1: TDBGrid
    Left = 240
    Top = 8
    Width = 193
    Height = 113
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object ZConnection1: TZConnection
    ControlsCodePage = cCP_UTF16
    ReadOnly = True
    Port = 0
    Left = 32
    Top = 136
  end
  object ZQuery1: TZQuery
    Params = <>
    Left = 64
    Top = 136
  end
  object Query1: TQuery
    DatabaseName = 'SESAM'
    Left = 96
    Top = 136
  end
end
