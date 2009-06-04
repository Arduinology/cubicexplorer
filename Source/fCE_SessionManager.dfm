object CESessionManager: TCESessionManager
  Left = 0
  Top = 0
  Caption = 'Manage Sessions'
  ClientHeight = 291
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 192
    Top = 0
    Width = 203
    Height = 256
    Align = alRight
    BevelEdges = [beLeft]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      201
      256)
    object TntLabel1: TTntLabel
      Left = 6
      Top = 8
      Width = 27
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Name'
    end
    object check_autosave: TTntCheckBox
      Left = 6
      Top = 168
      Width = 189
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Save settings automatically.'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = check_autosaveClick
    end
    object check_onstartup: TTntCheckBox
      Left = 6
      Top = 191
      Width = 189
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Load this session on startup.'
      TabOrder = 1
      OnClick = check_onstartupClick
    end
    object edit_name: TTntEdit
      Left = 6
      Top = 23
      Width = 189
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 2
      OnChange = edit_nameChange
    end
    object TntGroupBox1: TTntGroupBox
      Left = 6
      Top = 54
      Width = 189
      Height = 102
      Anchors = [akTop, akRight]
      Caption = 'Save/Load Settings'
      TabOrder = 3
      DesignSize = (
        189
        102)
      object check_bookmarks: TTntCheckBox
        Left = 16
        Top = 48
        Width = 157
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Bookmarks'
        Enabled = False
        TabOrder = 0
        ExplicitWidth = 155
      end
      object check_tabs: TTntCheckBox
        Left = 16
        Top = 25
        Width = 157
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Tabs'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 1
        ExplicitWidth = 155
      end
      object check_layout: TTntCheckBox
        Left = 16
        Top = 72
        Width = 157
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Layout'
        Enabled = False
        TabOrder = 2
        ExplicitWidth = 155
      end
    end
    object but_delete: TTntButton
      Left = 6
      Top = 225
      Width = 82
      Height = 25
      Caption = 'Delete'
      TabOrder = 4
      OnClick = but_deleteClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 256
    Width = 395
    Height = 35
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 248
    DesignSize = (
      395
      33)
    object TntButton1: TTntButton
      Left = 307
      Top = 4
      Width = 82
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 0
    end
  end
  object SessionList: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 192
    Height = 256
    Align = alClient
    BorderStyle = bsNone
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnFocusChanged = SessionListFocusChanged
    OnGetText = SessionListGetText
    OnPaintText = SessionListPaintText
    OnKeyDown = SessionListKeyDown
    OnNewText = SessionListNewText
    Columns = <>
  end
end
