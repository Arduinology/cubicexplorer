object CESessionManager: TCESessionManager
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Manage Sessions'
  ClientHeight = 293
  ClientWidth = 375
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
    Left = 156
    Top = 0
    Width = 219
    Height = 258
    Align = alRight
    BevelEdges = [beLeft]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      217
      258)
    object TntLabel1: TTntLabel
      Left = 14
      Top = 8
      Width = 27
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Name'
      ExplicitLeft = 10
    end
    object check_autosave: TTntCheckBox
      Left = 14
      Top = 168
      Width = 189
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Save settings automatically.'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = check_autosaveClick
      ExplicitLeft = 10
    end
    object check_onstartup: TTntCheckBox
      Left = 14
      Top = 191
      Width = 189
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Load this session on startup.'
      TabOrder = 1
      OnClick = check_onstartupClick
      ExplicitLeft = 10
    end
    object edit_name: TTntEdit
      Left = 14
      Top = 23
      Width = 189
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 2
      OnChange = edit_nameChange
      ExplicitLeft = 10
    end
    object TntGroupBox1: TTntGroupBox
      Left = 14
      Top = 54
      Width = 189
      Height = 102
      Anchors = [akTop, akRight]
      Caption = 'Save/Load Settings'
      TabOrder = 3
      ExplicitLeft = 10
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
      end
    end
    object but_delete: TTntButton
      Left = 67
      Top = 219
      Width = 82
      Height = 25
      Caption = 'Delete'
      TabOrder = 4
      OnClick = but_deleteClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 258
    Width = 375
    Height = 35
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 256
    ExplicitWidth = 395
    DesignSize = (
      375
      33)
    object TntButton1: TTntButton
      Left = 287
      Top = 4
      Width = 82
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 307
    end
  end
  object SessionList: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 156
    Height = 258
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
    ExplicitWidth = 192
    ExplicitHeight = 256
    Columns = <>
  end
end
