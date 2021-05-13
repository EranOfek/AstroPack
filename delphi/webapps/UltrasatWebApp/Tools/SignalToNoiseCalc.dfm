object UniFrameSignalToNoise: TUniFrameSignalToNoise
  Left = 0
  Top = 0
  Width = 604
  Height = 516
  TabOrder = 0
  object UniPanel1: TUniPanel
    Left = 3
    Top = 0
    Width = 398
    Height = 105
    Hint = ''
    TabOrder = 0
    Caption = 'UniPanel1'
  end
  object UniPanel2: TUniPanel
    Left = 3
    Top = 111
    Width = 414
    Height = 386
    Hint = ''
    ParentFont = False
    Font.Height = -16
    Font.Style = [fsBold]
    TabOrder = 1
    Caption = ''
    object UniGroupBox1: TUniGroupBox
      Left = 16
      Top = 32
      Width = 361
      Height = 225
      Hint = ''
      Caption = 'Select Filter'
      TabOrder = 1
      object RadioFilterWide: TUniRadioButton
        Left = 32
        Top = 48
        Width = 113
        Height = 17
        Hint = ''
        Caption = 'Wide'
        TabOrder = 1
      end
      object RadioFilterMedium: TUniRadioButton
        Left = 32
        Top = 88
        Width = 113
        Height = 17
        Hint = ''
        Caption = 'Medium'
        TabOrder = 2
      end
      object RadioFilterNarrow: TUniRadioButton
        Left = 32
        Top = 128
        Width = 113
        Height = 17
        Hint = ''
        Caption = 'Narrow'
        TabOrder = 3
      end
      object RadioFilterQuad: TUniRadioButton
        Left = 32
        Top = 160
        Width = 113
        Height = 17
        Hint = ''
        Caption = 'Quad'
        TabOrder = 4
      end
    end
  end
end
