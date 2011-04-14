object CEFormColumnSettings: TCEFormColumnSettings
  Left = 364
  Top = 252
  BorderIcons = [biSystemMenu]
  Caption = 'Column Settings'
  ClientHeight = 332
  ClientWidth = 287
  Color = clBtnFace
  Constraints.MinHeight = 370
  Constraints.MinWidth = 295
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SpTBXPanel1: TSpTBXPanel
    Left = 0
    Top = 294
    Width = 287
    Height = 38
    Caption = 'SpTBXPanel1'
    Align = alBottom
    TabOrder = 0
    TBXStyleBackground = True
    ExplicitTop = 296
    DesignSize = (
      287
      38)
    object CheckBoxLiveUpdate: TSpTBXCheckBox
      Left = 8
      Top = 8
      Width = 81
      Height = 21
      Caption = 'Live Update'
      Anchors = [akLeft, akBottom]
      TabOrder = 0
      OnClick = CheckBoxLiveUpdateClick
      ExplicitTop = 72
    end
    object ButtonOk: TSpTBXButton
      Left = 123
      Top = 6
      Width = 75
      Height = 25
      Caption = '&OK'
      Anchors = [akRight, akBottom]
      TabOrder = 1
      ModalResult = 1
    end
    object ButtonCancel: TSpTBXButton
      Left = 204
      Top = 6
      Width = 75
      Height = 25
      Caption = '&Cancel'
      Anchors = [akRight, akBottom]
      TabOrder = 2
      ModalResult = 2
    end
  end
  object SpTBXTabControl1: TSpTBXTabControl
    Left = 0
    Top = 0
    Width = 287
    Height = 294
    Align = alClient
    ActiveTabIndex = 0
    TabVisible = False
    ExplicitLeft = -2
    ExplicitTop = 8
    ExplicitWidth = 289
    ExplicitHeight = 281
    HiddenItems = <>
    object SpTBXTabItem1: TSpTBXTabItem
      Checked = True
    end
    object SpTBXTabSheet1: TSpTBXTabSheet
      Left = 0
      Top = 10
      Width = 287
      Height = 284
      ImageIndex = -1
      ExplicitTop = 8
      ExplicitHeight = 254
      DesignSize = (
        287
        284)
      TabItem = 'SpTBXTabItem1'
      object EditPixelWidth: TSpTBXEdit
        Left = 8
        Top = 251
        Width = 35
        Height = 21
        Anchors = [akLeft, akBottom]
        TabOrder = 0
        OnExit = EditPixelWidthExit
        OnKeyPress = EditPixelWidthKeyPress
      end
      object Label1: TSpTBXLabel
        Left = 2
        Top = 0
        Width = 281
        Height = 27
        Caption = 'Drag and Drop to reorder the columns. '
        Align = alTop
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        Wrapping = twWrap
        Alignment = taCenter
        ExplicitTop = 2
        ExplicitWidth = 283
      end
      object Label2: TSpTBXLabel
        Left = 49
        Top = 251
        Width = 230
        Height = 19
        Caption = 'Width of the selected column'
        Anchors = [akLeft, akRight, akBottom]
        AutoSize = False
        OnClick = FormCreate
      end
      object VSTColumnNames: TVirtualStringTree
        Left = 8
        Top = 27
        Width = 271
        Height = 214
        Anchors = [akLeft, akTop, akRight, akBottom]
        CheckImageKind = ckXP
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'MS Sans Serif'
        Header.Font.Style = []
        Header.MainColumn = -1
        Header.Options = [hoColumnResize, hoDrag]
        HintAnimation = hatNone
        TabOrder = 3
        TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toInitOnSave, toToggleOnDblClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
        OnChecking = VSTColumnNamesChecking
        OnDragAllowed = VSTColumnNamesDragAllowed
        OnDragOver = VSTColumnNamesDragOver
        OnDragDrop = VSTColumnNamesDragDrop
        OnFocusChanging = VSTColumnNamesFocusChanging
        OnFreeNode = VSTColumnNamesFreeNode
        OnGetText = VSTColumnNamesGetText
        OnInitNode = VSTColumnNamesInitNode
        Columns = <>
      end
    end
  end
end
