object CEPoEditor: TCEPoEditor
  Left = 0
  Top = 0
  Width = 419
  Height = 501
  TabOrder = 0
  TabStop = True
  object SpTBXDock1: TSpTBXDock
    Left = 0
    Top = 0
    Width = 419
    Height = 25
    AllowDrag = False
    ExplicitWidth = 420
    object SpTBXToolbar1: TSpTBXToolbar
      Left = 0
      Top = 0
      FullSize = True
      ShrinkMode = tbsmNone
      Stretch = True
      TabOrder = 0
      Caption = 'SpTBXToolbar1'
      object TBControlItem1: TTBControlItem
        Control = LanguagesCombo
      end
      object SpTBXItem1: TSpTBXItem
        Action = act_save
      end
      object SpTBXItem3: TSpTBXItem
        Action = act_apply
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object SpTBXItem2: TSpTBXItem
        Action = act_new
      end
      object SpTBXItem4: TSpTBXItem
        Caption = 'Test1'
        Visible = False
        OnClick = SpTBXItem4Click
      end
      object LanguagesCombo: TSpTBXComboBox
        Left = 0
        Top = 0
        Width = 145
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        OnClick = LanguagesComboItemClick
      end
    end
  end
  object TabControl: TSpTBXTabControl
    Left = 0
    Top = 25
    Width = 419
    Height = 476
    Align = alClient
    ActiveTabIndex = 0
    ExplicitWidth = 420
    ExplicitHeight = 475
    HiddenItems = <>
    object SpTBXTabItem2: TSpTBXTabItem
      Caption = 'Translation'
      Checked = True
    end
    object SpTBXTabItem1: TSpTBXTabItem
      Caption = 'Word List'
    end
    object WordsSheet: TSpTBXTabSheet
      Left = 0
      Top = 23
      Width = 419
      Height = 453
      Caption = 'Word List'
      ImageIndex = -1
      ExplicitWidth = 420
      ExplicitHeight = 452
      TabItem = 'SpTBXTabItem1'
      object horz_splitter: TSpTBXSplitter
        Left = 2
        Top = 373
        Width = 413
        Height = 5
        Cursor = crSizeNS
        Align = alBottom
        ResizeStyle = rsPattern
        ExplicitTop = 372
        ExplicitWidth = 414
      end
      object Translation_panel: TPanel
        Left = 2
        Top = 378
        Width = 413
        Height = 71
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 377
        ExplicitWidth = 414
        object vert_splitter: TSpTBXSplitter
          Left = 201
          Top = 0
          Height = 71
          Cursor = crSizeWE
          ResizeStyle = rsPattern
        end
        object PoItemStrID_panel: TSpTBXPanel
          Left = 0
          Top = 0
          Width = 201
          Height = 71
          Align = alLeft
          TabOrder = 1
          OnResize = PoItemStrMsg_panelResize
          TBXStyleBackground = True
          object TntLabel6: TTntLabel
            Left = 6
            Top = 6
            Width = 36
            Height = 13
            Caption = 'Original'
            Transparent = True
          end
          object PoItemStrID: TTntMemo
            Left = 5
            Top = 24
            Width = 190
            Height = 40
            TabStop = False
            BevelInner = bvNone
            BevelKind = bkTile
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGrayText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object PoItemStrMsg_panel: TSpTBXPanel
          Left = 206
          Top = 0
          Width = 207
          Height = 71
          Align = alClient
          TabOrder = 0
          OnResize = PoItemStrMsg_panelResize
          TBXStyleBackground = True
          ExplicitWidth = 208
          object TntLabel5: TTntLabel
            Left = 6
            Top = 6
            Width = 53
            Height = 13
            Caption = 'Translation'
            Transparent = True
          end
          object PoItemStrMsg: TTntMemo
            Left = 5
            Top = 24
            Width = 200
            Height = 40
            BevelInner = bvNone
            BevelKind = bkTile
            BorderStyle = bsNone
            ScrollBars = ssVertical
            TabOrder = 0
            WantReturns = False
            OnChange = PoItemStrMsgChange
            OnKeyDown = PoItemStrMsgKeyDown
          end
        end
      end
      object PoList_panel: TSpTBXPanel
        Left = 2
        Top = 0
        Width = 413
        Height = 373
        Align = alClient
        TabOrder = 1
        ExplicitWidth = 414
        ExplicitHeight = 372
        object PoList: TVirtualStringTree
          Left = 2
          Top = 2
          Width = 409
          Height = 369
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Header.AutoSizeIndex = 1
          Header.DefaultHeight = 17
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'Tahoma'
          Header.Font.Style = []
          Header.MainColumn = 1
          Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
          Header.SortColumn = 1
          Header.Style = hsPlates
          LineStyle = lsSolid
          TabOrder = 0
          TreeOptions.MiscOptions = [toEditable, toFullRepaintOnResize, toInitOnSave, toWheelPanning]
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toFullRowSelect, toSimpleDrawSelection]
          OnChange = PoListChange
          OnCompareNodes = PoListCompareNodes
          OnEdited = PoListEdited
          OnGetText = PoListGetText
          OnHeaderClick = PoListHeaderClick
          OnKeyAction = PoListKeyAction
          OnMouseDown = PoListMouseDown
          OnMouseUp = PoListMouseUp
          OnNewText = PoListNewText
          ExplicitWidth = 410
          ExplicitHeight = 368
          Columns = <
            item
              Position = 0
              Width = 200
              WideText = 'Original'
            end
            item
              Position = 1
              Width = 209
              WideText = 'Translation'
            end>
        end
      end
    end
    object SettingsSheet: TSpTBXTabSheet
      Left = 0
      Top = 23
      Width = 419
      Height = 453
      Caption = 'Translation'
      ImageIndex = -1
      ExplicitWidth = 420
      ExplicitHeight = 452
      TabItem = 'SpTBXTabItem2'
      object SpTBXPanel1: TSpTBXPanel
        Left = 12
        Top = 12
        Width = 349
        Height = 165
        TabOrder = 0
        object TntLabel1: TTntLabel
          Left = 16
          Top = 16
          Width = 86
          Height = 13
          Caption = 'Translator'#39's Name'
          Transparent = True
        end
        object TntLabel2: TTntLabel
          Left = 16
          Top = 60
          Width = 83
          Height = 13
          Caption = 'Translator'#39's Email'
          Transparent = True
        end
        object TntLabel3: TTntLabel
          Left = 16
          Top = 104
          Width = 47
          Height = 13
          Caption = 'Language'
          Transparent = True
        end
        object TntLabel4: TTntLabel
          Left = 176
          Top = 16
          Width = 91
          Height = 13
          Caption = 'Translation Version'
          Transparent = True
        end
        object label1: TTntLabel
          Left = 192
          Top = 95
          Width = 99
          Height = 13
          Alignment = taRightJustify
          Caption = 'Application Revision:'
          Transparent = True
        end
        object label2: TTntLabel
          Left = 191
          Top = 76
          Width = 100
          Height = 13
          Alignment = taRightJustify
          Caption = 'Translation Revision:'
          Transparent = True
        end
        object label_pot_rev: TTntLabel
          Left = 297
          Top = 95
          Width = 3
          Height = 13
          Transparent = True
        end
        object label_translation_rev: TTntLabel
          Left = 297
          Top = 76
          Width = 3
          Height = 13
          Transparent = True
          WordWrap = True
        end
        object Bevel1: TBevel
          Left = 176
          Top = 72
          Width = 153
          Height = 49
          Shape = bsFrame
          Visible = False
        end
        object LanguageList: TComboBox
          Left = 16
          Top = 120
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = LanguageListChange
        end
        object edit_translators_name: TTntEdit
          Left = 16
          Top = 32
          Width = 145
          Height = 21
          BevelKind = bkTile
          BorderStyle = bsNone
          TabOrder = 1
          OnChange = edit_translators_Change
        end
        object edit_translators_email: TTntEdit
          Left = 16
          Top = 76
          Width = 145
          Height = 21
          BevelKind = bkTile
          BorderStyle = bsNone
          TabOrder = 2
          OnChange = edit_translators_Change
        end
        object edit_translation_version: TTntEdit
          Left = 176
          Top = 32
          Width = 153
          Height = 21
          BevelKind = bkTile
          BorderStyle = bsNone
          TabOrder = 3
          OnChange = edit_translation_versionChange
        end
      end
      object SpTBXButton1: TSpTBXButton
        Left = 12
        Top = 183
        Width = 115
        Height = 25
        Caption = 'Publish Translation'
        Enabled = False
        TabOrder = 1
      end
    end
  end
  object ActionList: TTntActionList
    Left = 528
    Top = 32
    object act_save: TTntAction
      Caption = 'Save'
      Hint = 'Save changes.'
      OnExecute = act_saveExecute
      OnUpdate = act_Update
    end
    object act_new: TTntAction
      Caption = 'New'
      Hint = 'Create new translation.'
      OnExecute = act_newExecute
      OnUpdate = act_Update
    end
    object act_apply: TTntAction
      Caption = 'Apply'
      Hint = 'Apply changes.'
      OnExecute = act_applyExecute
      OnUpdate = act_Update
    end
  end
end
