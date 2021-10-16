object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 447
  Height = 391
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 81
    Align = alTop
    TabOrder = 0
    object LabelObjectName: TLabel
      Left = 16
      Top = 16
      Width = 61
      Height = 13
      Caption = 'Object name'
    end
    object LabelDescription: TLabel
      Left = 16
      Top = 48
      Width = 53
      Height = 13
      Caption = 'Description'
    end
    object EditObjectName: TEdit
      Left = 112
      Top = 13
      Width = 233
      Height = 21
      ReadOnly = True
      TabOrder = 0
      Text = 'EditObjectName'
    end
    object EditDescription: TEdit
      Left = 112
      Top = 40
      Width = 233
      Height = 21
      ReadOnly = True
      TabOrder = 1
      Text = 'EditObjectName'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 81
    Width = 447
    Height = 310
    Align = alClient
    TabOrder = 1
  end
end
