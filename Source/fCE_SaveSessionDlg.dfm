object CESaveSessionDlg: TCESaveSessionDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Save Session'
  ClientHeight = 97
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    249
    97)
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 8
    Top = 8
    Width = 66
    Height = 13
    Caption = 'Session Name'
  end
  object SessionCombo: TTntComboBox
    Left = 8
    Top = 27
    Width = 233
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 0
    TabOrder = 0
    OnChange = SessionComboChange
  end
  object but_save: TTntButton
    Left = 47
    Top = 64
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
    OnClick = but_saveClick
  end
  object but_cancel: TTntButton
    Left = 128
    Top = 64
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
