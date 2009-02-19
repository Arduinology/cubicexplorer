inherited CEOptionsPage_Display: TCEOptionsPage_Display
  object TntLabel1: TTntLabel
    Left = 16
    Top = 16
    Width = 32
    Height = 13
    Caption = 'Theme'
  end
  object combo_theme: TComboBox
    Left = 16
    Top = 35
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnChange = HandleChange
  end
  object check_path_in_title: TTntCheckBox
    Left = 16
    Top = 71
    Width = 409
    Height = 17
    Caption = 'Show Path in Title bar'
    TabOrder = 1
    OnClick = HandleChange
  end
end
