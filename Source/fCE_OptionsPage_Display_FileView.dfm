inherited CE_OptionsPage_Display_FileView: TCE_OptionsPage_Display_FileView
  object TntPageControl1: TTntPageControl
    Left = 3
    Top = 3
    Width = 435
    Height = 330
    ActivePage = sheet_display
    TabOrder = 0
    object sheet_options: TTntTabSheet
      Caption = 'Options'
      ExplicitHeight = 305
      object check_fullrowselect: TTntCheckBox
        Left = 16
        Top = 16
        Width = 401
        Height = 17
        Caption = 'Highlight row completely'
        TabOrder = 0
        OnClick = HandleChange
      end
      object check_selectprev: TTntCheckBox
        Left = 16
        Top = 35
        Width = 401
        Height = 17
        Caption = 'Select previous folder'
        TabOrder = 1
        OnClick = HandleChange
      end
      object check_autoselect: TTntCheckBox
        Left = 16
        Top = 54
        Width = 401
        Height = 17
        Caption = 'Select first item automatically'
        TabOrder = 2
        OnClick = HandleChange
      end
      object check_autosize_liststyle: TTntCheckBox
        Left = 16
        Top = 73
        Width = 401
        Height = 17
        Caption = 'Auto size cells in list view style.'
        TabOrder = 3
        OnClick = HandleChange
      end
      object check_sortfoldersfirst: TTntCheckBox
        Left = 16
        Top = 92
        Width = 401
        Height = 17
        Caption = 'Always sort folders first.'
        TabOrder = 4
        OnClick = HandleChange
      end
      object check_infotips: TTntCheckBox
        Left = 16
        Top = 111
        Width = 401
        Height = 17
        Caption = 'Show InfoTips'
        TabOrder = 5
        OnClick = HandleChange
      end
      object check_singleclick: TTntCheckBox
        Left = 16
        Top = 130
        Width = 401
        Height = 17
        Caption = 'Single click browsing'
        TabOrder = 6
        OnClick = HandleChange
      end
      object check_perfolder: TTntCheckBox
        Left = 16
        Top = 149
        Width = 401
        Height = 17
        Caption = 'Use per folder settings'
        TabOrder = 7
        OnClick = HandleChange
      end
      object check_gridlines: TTntCheckBox
        Left = 16
        Top = 168
        Width = 401
        Height = 17
        Caption = 'Show gridlines'
        TabOrder = 8
        OnClick = HandleChange
      end
      object check_browse_zip: TTntCheckBox
        Left = 16
        Top = 187
        Width = 401
        Height = 17
        Caption = 'Browse Zip Files'
        TabOrder = 9
        OnClick = HandleChange
      end
    end
    object sheet_display: TTntTabSheet
      Caption = 'Display'
      ExplicitLeft = 0
      object TntLabel1: TTntLabel
        Left = 200
        Top = 14
        Width = 76
        Height = 13
        Caption = 'File size format:'
      end
      object TntLabel3: TTntLabel
        Left = 16
        Top = 212
        Width = 82
        Height = 13
        Caption = 'Background color'
      end
      object combo_sizeformat: TTntComboBox
        Left = 200
        Top = 33
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = HandleChange
      end
      object color_background: TColorBox
        Left = 16
        Top = 231
        Width = 145
        Height = 22
        DefaultColorColor = clWindow
        Selected = clDefault
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 1
        OnSelect = color_backgroundSelect
      end
      object TntGroupBox1: TTntGroupBox
        Left = 16
        Top = 14
        Width = 159
        Height = 178
        Caption = 'Font'
        TabOrder = 2
        DesignSize = (
          159
          178)
        object TntLabel14: TTntLabel
          Left = 16
          Top = 118
          Width = 29
          Height = 13
          Caption = 'Color:'
        end
        object but_font: TTntButton
          Left = 16
          Top = 87
          Width = 127
          Height = 25
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Font'
          TabOrder = 0
          WordWrap = True
          OnClick = but_fontClick
        end
        object color_font: TColorBox
          Left = 24
          Top = 137
          Width = 119
          Height = 22
          DefaultColorColor = clWindowText
          NoneColorColor = clNone
          Selected = clDefault
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 16
          TabOrder = 1
          OnSelect = color_fontSelect
        end
        object panel_font: TTntPanel
          Left = 8
          Top = 24
          Width = 143
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          BevelOuter = bvNone
          Color = clWindow
          ParentBackground = False
          TabOrder = 2
        end
      end
    end
    object sheet_colors: TTntTabSheet
      Caption = 'Colors'
      ExplicitLeft = 0
      object TntLabel2: TTntLabel
        Left = 16
        Top = 278
        Width = 408
        Height = 17
        AutoSize = False
        Caption = 'Use comma separated list of extensions. For example: txt,doc,rtf'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsItalic]
        ParentFont = False
        WordWrap = True
      end
      object but_add: TTntButton
        Left = 16
        Top = 247
        Width = 81
        Height = 25
        Caption = 'Add'
        TabOrder = 0
        OnClick = but_addClick
      end
      object but_delete: TTntButton
        Left = 136
        Top = 247
        Width = 81
        Height = 25
        Caption = 'Delete'
        TabOrder = 1
        OnClick = but_deleteClick
      end
      object color_extension: TColorBox
        Left = 231
        Top = 113
        Width = 154
        Height = 22
        DefaultColorColor = clWindowText
        Selected = clDefault
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 2
        OnSelect = HandleExtColorChange
      end
      object list_exts: TVirtualStringTree
        Left = 16
        Top = 40
        Width = 201
        Height = 201
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
        LineStyle = lsSolid
        TabOrder = 3
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect]
        OnEditCancelled = list_extsEditCancelled
        OnEdited = list_extsEdited
        OnFocusChanged = list_extsFocusChanged
        OnFreeNode = list_extsFreeNode
        OnGetText = list_extsGetText
        OnNewText = list_extsNewText
        Columns = <
          item
            Position = 0
            Width = 197
          end>
      end
      object check_ext_bold: TTntCheckBox
        Left = 231
        Top = 52
        Width = 193
        Height = 17
        Caption = 'Bold'
        TabOrder = 4
        OnClick = HandleExtColorChange
      end
      object check_ext_italic: TTntCheckBox
        Left = 231
        Top = 71
        Width = 193
        Height = 17
        Caption = 'Italic'
        TabOrder = 5
        OnClick = HandleExtColorChange
      end
      object check_ext_underline: TTntCheckBox
        Left = 231
        Top = 90
        Width = 193
        Height = 17
        Caption = 'Underline'
        TabOrder = 6
        OnClick = HandleExtColorChange
      end
      object check_extension_colors: TTntCheckBox
        Left = 16
        Top = 17
        Width = 408
        Height = 17
        Caption = 'Use extension colors'
        TabOrder = 7
        OnClick = HandleChange
      end
    end
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdApplyButton]
    OnApply = FontDlgApply
    Left = 400
    Top = 34
  end
end
