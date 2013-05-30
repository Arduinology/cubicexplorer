inherited CEOptionsPage_Plugins: TCEOptionsPage_Plugins
  object TntPageControl1: TTntPageControl
    Left = 3
    Top = 3
    Width = 435
    Height = 330
    ActivePage = sheet_libraries
    TabOrder = 0
    object TntTabSheet1: TTntTabSheet
      Caption = 'Active Plugins'
      DesignSize = (
        427
        302)
      object label_plugin_version: TTntLabel
        Left = 232
        Top = 39
        Width = 185
        Height = 19
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsItalic]
        ParentFont = False
        Visible = False
      end
      object tree_plugins: TVirtualStringTree
        Left = 8
        Top = 8
        Width = 213
        Height = 285
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        HintMode = hmHint
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
        OnFocusChanged = tree_pluginsFocusChanged
        OnFreeNode = tree_plug_FreeNode
        OnGetText = tree_plug_GetText
        OnPaintText = tree_plugPaintText
        OnGetHint = tree_plug_GetHint
        Columns = <>
      end
      object label_plugin_name: TSpTBXLabel
        Left = 232
        Top = 8
        Width = 185
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Visible = False
        Wrapping = twEndEllipsis
        Images = CE_Images.SmallIcons
        Underline = True
      end
      object but_plugin_settings: TTntButton
        Left = 248
        Top = 264
        Width = 153
        Height = 29
        Caption = 'Settings'
        TabOrder = 2
        Visible = False
        OnClick = but_plugin_settingsClick
      end
      object memo_plugin_desc: TTntMemo
        Left = 232
        Top = 64
        Width = 185
        Height = 194
        ReadOnly = True
        TabOrder = 3
        Visible = False
      end
    end
    object sheet_formats: TTntTabSheet
      Caption = 'Formats'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object VirtualStringTree2: TVirtualStringTree
        Left = 8
        Top = 8
        Width = 213
        Height = 285
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        HintMode = hmHint
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Columns = <>
      end
    end
    object sheet_libraries: TTntTabSheet
      Caption = 'Libraries'
      object tree_libraries: TVirtualStringTree
        Left = 8
        Top = 8
        Width = 213
        Height = 285
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        HintMode = hmHint
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
        OnFreeNode = tree_plug_FreeNode
        OnGetText = tree_plug_GetText
        OnPaintText = tree_plugPaintText
        OnGetHint = tree_plug_GetHint
        Columns = <>
      end
      object label_library_name: TSpTBXLabel
        Left = 232
        Top = 8
        Width = 185
        Height = 25
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Wrapping = twEndEllipsis
        Images = CE_Images.SmallIcons
      end
    end
  end
end
