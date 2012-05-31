inherited CE_OptionsPage_Display_FileView: TCE_OptionsPage_Display_FileView
  object TntPageControl1: TTntPageControl
    Left = 3
    Top = 3
    Width = 435
    Height = 330
    ActivePage = sheet_options
    TabOrder = 0
    object sheet_options: TTntTabSheet
      Caption = 'Options'
      ExplicitHeight = 305
      object TntLabel1: TTntLabel
        Left = 16
        Top = 230
        Width = 76
        Height = 13
        Caption = 'File size format:'
      end
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
      object combo_sizeformat: TTntComboBox
        Left = 16
        Top = 249
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 7
        OnChange = HandleChange
      end
      object check_perfolder: TTntCheckBox
        Left = 16
        Top = 149
        Width = 401
        Height = 17
        Caption = 'Use per folder settings'
        TabOrder = 8
        OnClick = HandleChange
      end
      object check_gridlines: TTntCheckBox
        Left = 16
        Top = 168
        Width = 401
        Height = 17
        Caption = 'Show gridlines'
        TabOrder = 9
        OnClick = HandleChange
      end
      object check_browse_zip: TTntCheckBox
        Left = 16
        Top = 187
        Width = 401
        Height = 17
        Caption = 'Browse Zip Files'
        TabOrder = 10
        OnClick = HandleChange
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
        Selected = clWindowText
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
end
