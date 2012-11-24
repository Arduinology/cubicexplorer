object CEPluginSettingsForm: TCEPluginSettingsForm
  Left = 0
  Top = 0
  ClientHeight = 346
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object panel_bottom: TPanel
    Left = 0
    Top = 306
    Width = 464
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      464
      40)
    object but_apply: TTntButton
      Left = 378
      Top = 6
      Width = 80
      Height = 27
      Anchors = [akTop, akRight]
      Caption = 'Apply'
      TabOrder = 2
      OnClick = but_applyClick
    end
    object but_cancel: TTntButton
      Left = 292
      Top = 6
      Width = 80
      Height = 27
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = but_cancelClick
    end
    object but_ok: TTntButton
      Left = 206
      Top = 6
      Width = 80
      Height = 27
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = but_okClick
    end
  end
  object panel_content: TPanel
    Left = 0
    Top = 0
    Width = 464
    Height = 306
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object tree_settings: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 464
      Height = 306
      Align = alClient
      Header.AutoSizeIndex = 1
      Header.DefaultHeight = 17
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      LineStyle = lsSolid
      RootNodeCount = 5
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
      OnEditing = tree_settingsEditing
      OnFreeNode = tree_settingsFreeNode
      OnGetText = tree_settingsGetText
      OnNewText = tree_settingsNewText
      Columns = <
        item
          Position = 0
          Width = 150
          WideText = 'Name'
        end
        item
          Position = 1
          Width = 310
          WideText = 'Value'
        end>
    end
  end
end
