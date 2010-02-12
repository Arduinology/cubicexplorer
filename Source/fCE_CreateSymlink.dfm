object CreateSymlinkDlg: TCreateSymlinkDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Create Symbolic Link'
  ClientHeight = 145
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    329
    145)
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 13
    Caption = 'Link Name'
  end
  object TntLabel2: TTntLabel
    Left = 8
    Top = 56
    Width = 65
    Height = 13
    Caption = 'Target Folder'
  end
  object but_cancel: TSpTBXButton
    Left = 246
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Cancel'
    Anchors = [akRight, akBottom]
    TabOrder = 0
    ModalResult = 2
  end
  object but_create: TSpTBXButton
    Left = 165
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Create'
    Anchors = [akRight, akBottom]
    TabOrder = 1
    OnClick = but_createClick
  end
  object edit_linkname: TSpTBXEdit
    Left = 8
    Top = 27
    Width = 313
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edit_targetpath: TSpTBXButtonEdit
    Left = 8
    Top = 75
    Width = 313
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    EditButton.Left = 290
    EditButton.Top = 0
    EditButton.Width = 19
    EditButton.Height = 17
    EditButton.Caption = '...'
    EditButton.Align = alRight
    EditButton.OnClick = edit_targetpathSubEditButton0Click
  end
end
