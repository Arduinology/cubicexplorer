inherited CEOptionsPage_General: TCEOptionsPage_General
  object check_singleinstance: TTntCheckBox
    Left = 16
    Top = 156
    Width = 401
    Height = 17
    Caption = 'Single Instance Only'
    TabOrder = 0
    OnClick = HandleChange
  end
  object group_startup: TTntGroupBox
    Left = 16
    Top = 12
    Width = 409
    Height = 129
    Caption = 'On Startup'
    TabOrder = 1
    object radio_default: TTntRadioButton
      Left = 11
      Top = 24
      Width = 386
      Height = 17
      Caption = 'Open default tab'
      TabOrder = 0
      OnClick = radioClick
    end
    object radio_lasttime: TTntRadioButton
      Left = 11
      Top = 47
      Width = 386
      Height = 17
      Caption = 'Continue from last time'
      TabOrder = 1
      OnClick = radioClick
    end
    object radio_session: TTntRadioButton
      Left = 11
      Top = 70
      Width = 386
      Height = 17
      Caption = 'Load session'
      TabOrder = 2
      OnClick = radioClick
    end
    object combo_sessions: TTntComboBox
      Left = 27
      Top = 93
      Width = 162
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = HandleChange
    end
  end
end
