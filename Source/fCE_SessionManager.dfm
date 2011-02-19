object CESessionManager: TCESessionManager
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Manage Sessions'
  ClientHeight = 292
  ClientWidth = 403
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
  object SpTBXPanel2: TSpTBXPanel
    Left = 0
    Top = 249
    Width = 403
    Height = 43
    Caption = 'SpTBXPanel2'
    Align = alBottom
    TabOrder = 0
    Borders = False
    BorderType = pbrRaised
    TBXStyleBackground = True
    ExplicitTop = 250
    ExplicitWidth = 375
    DesignSize = (
      403
      43)
    object SpTBXButton1: TSpTBXButton
      Left = 302
      Top = 6
      Width = 91
      Height = 31
      Caption = 'Close'
      Anchors = [akTop, akRight]
      TabOrder = 0
      ModalResult = 1
      ExplicitLeft = 274
    end
  end
  object SpTBXTabControl1: TSpTBXTabControl
    Left = 0
    Top = 0
    Width = 403
    Height = 249
    Align = alClient
    ActiveTabIndex = 0
    TabVisible = False
    HiddenItems = <>
    object SpTBXTabItem1: TSpTBXTabItem
      Checked = True
    end
    object SpTBXTabSheet1: TSpTBXTabSheet
      Left = 0
      Top = 10
      Width = 403
      Height = 239
      ImageIndex = -1
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        403
        239)
      TabItem = 'SpTBXTabItem1'
      object but_delete: TSpTBXButton
        Left = 202
        Top = 201
        Width = 69
        Height = 25
        Caption = 'Delete'
        Anchors = [akRight, akBottom]
        TabOrder = 0
        OnClick = but_deleteClick
        ExplicitLeft = 170
        ExplicitTop = 192
      end
      object check_autosave: TSpTBXCheckBox
        Left = 202
        Top = 160
        Width = 185
        Height = 31
        Caption = 'Save settings automatically.'
        Anchors = [akTop, akRight]
        AutoSize = False
        TabOrder = 1
        Wrapping = twWrap
        OnClick = check_autosaveClick
        ExplicitLeft = 190
      end
      object edit_name: TSpTBXEdit
        Left = 202
        Top = 32
        Width = 185
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 2
        OnClick = edit_nameChange
        ExplicitLeft = 190
      end
      object group_loadsave: TSpTBXGroupBox
        Left = 202
        Top = 59
        Width = 185
        Height = 95
        Caption = 'Save/Load Settings'
        Anchors = [akTop, akRight]
        Enabled = False
        TabOrder = 3
        ExplicitLeft = 190
        DesignSize = (
          185
          95)
        object check_tabs: TSpTBXCheckBox
          Left = 12
          Top = 18
          Width = 170
          Height = 21
          Caption = 'Tabs'
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Enabled = False
          TabOrder = 0
          Wrapping = twEndEllipsis
          Checked = True
          State = cbChecked
        end
        object check_bookmarks: TSpTBXCheckBox
          Left = 12
          Top = 41
          Width = 170
          Height = 21
          Caption = 'Bookmarks'
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Enabled = False
          TabOrder = 1
          Wrapping = twEndEllipsis
        end
        object check_layout: TSpTBXCheckBox
          Left = 12
          Top = 64
          Width = 170
          Height = 21
          Caption = 'Layouts'
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Enabled = False
          TabOrder = 2
          Wrapping = twEndEllipsis
        end
      end
      object SpTBXLabel1: TSpTBXLabel
        Left = 202
        Top = 12
        Width = 33
        Height = 19
        Caption = 'Name'
        Anchors = [akTop, akRight]
        ExplicitLeft = 190
      end
      object list_sessions: TSpTBXListBox
        Left = 12
        Top = 12
        Width = 177
        Height = 214
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 16
        TabOrder = 5
        OnClick = list_sessionsClick
      end
    end
  end
end
