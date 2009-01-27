object CESessionEditDlg: TCESessionEditDlg
  Left = 0
  Top = 0
  Caption = 'New Session'
  ClientHeight = 251
  ClientWidth = 217
  Color = clBtnFace
  Constraints.MinHeight = 280
  Constraints.MinWidth = 225
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    217
    251)
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 8
    Top = 8
    Width = 66
    Height = 13
    Caption = 'Session Name'
  end
  object TntGroupBox1: TTntGroupBox
    Left = 8
    Top = 56
    Width = 201
    Height = 102
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Save/Load Settings'
    TabOrder = 0
    DesignSize = (
      201
      102)
    object check_bookmarks: TTntCheckBox
      Left = 16
      Top = 48
      Width = 169
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Bookmarks'
      Enabled = False
      TabOrder = 0
    end
    object check_tabs: TTntCheckBox
      Left = 16
      Top = 25
      Width = 169
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Tabs'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object check_layouts: TTntCheckBox
      Left = 16
      Top = 72
      Width = 169
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Layouts'
      Enabled = False
      TabOrder = 2
    end
  end
  object check_autosave: TTntCheckBox
    Left = 8
    Top = 168
    Width = 201
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Save settings automatically.'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object TntButton1: TTntButton
    Left = 56
    Top = 218
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object TntButton2: TTntButton
    Left = 136
    Top = 218
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object combo_sessionname: TTntComboBox
    Left = 8
    Top = 24
    Width = 201
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 4
    OnChange = combo_sessionnameChange
  end
  object check_onstartup: TTntCheckBox
    Left = 8
    Top = 191
    Width = 201
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Load this session on startup.'
    TabOrder = 5
  end
end
