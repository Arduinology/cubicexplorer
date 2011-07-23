inherited CE_OptionsPage_General_Updates: TCE_OptionsPage_General_Updates
  object check_autoupdates: TTntCheckBox
    Left = 16
    Top = 16
    Width = 413
    Height = 17
    Caption = 'Check for updates on startup'
    TabOrder = 0
    OnClick = HandleChange
  end
  object list_update_types: TTntCheckListBox
    Left = 32
    Top = 62
    Width = 197
    Height = 87
    ItemHeight = 13
    TabOrder = 1
    OnClick = HandleChange
  end
  object check_buildtypes: TTntCheckBox
    Left = 16
    Top = 39
    Width = 413
    Height = 17
    Caption = 'Notify only about following update types'
    TabOrder = 2
    OnClick = HandleChange
  end
  object group_proxy: TTntGroupBox
    Left = 16
    Top = 171
    Width = 409
    Height = 130
    Caption = 'Proxy Settings'
    TabOrder = 3
    DesignSize = (
      409
      130)
    object label_proxy_address: TTntLabel
      Left = 29
      Top = 52
      Width = 39
      Height = 13
      Caption = 'Address'
    end
    object label_proxy_port: TTntLabel
      Left = 327
      Top = 52
      Width = 20
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Port'
      ExplicitLeft = 331
    end
    object edit_proxy_address: TTntEdit
      Left = 29
      Top = 71
      Width = 292
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 296
    end
    object edit_proxy_port: TTntEdit
      Left = 327
      Top = 71
      Width = 66
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      ExplicitLeft = 331
    end
    object check_proxy_system: TTntCheckBox
      Left = 29
      Top = 98
      Width = 392
      Height = 17
      Caption = 'Use system settings'
      TabOrder = 2
      OnClick = HandleChange
    end
    object check_proxy: TTntCheckBox
      Left = 13
      Top = 25
      Width = 97
      Height = 17
      Caption = 'Enable'
      TabOrder = 3
      OnClick = HandleChange
    end
  end
end
