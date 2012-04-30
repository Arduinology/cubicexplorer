object CETextEditor: TCETextEditor
  Left = 0
  Top = 0
  ClientHeight = 419
  ClientWidth = 684
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TopDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 684
    Height = 25
    object toolbar_main: TCEToolbar
      Left = 0
      Top = 0
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      DragHandleStyle = dhNone
      Stretch = True
      TabOrder = 0
      Caption = 'MainToolbar'
      object SpTBXSubmenuItem1: TSpTBXSubmenuItem
        Tag = 100
        Caption = 'File'
        object but_new: TSpTBXItem
          Tag = 1
          Action = act_new
        end
        object but_open: TSpTBXItem
          Tag = 2
          Action = act_open
        end
        object but_reload: TSpTBXItem
          Tag = 3
          Action = act_reload
        end
        object SpTBXSeparatorItem1: TSpTBXSeparatorItem
        end
        object but_save: TSpTBXItem
          Tag = 4
          Action = act_save
        end
        object but_saveas: TSpTBXItem
          Tag = 5
          Action = act_saveas
        end
        object SpTBXSeparatorItem12: TSpTBXSeparatorItem
        end
        object SpTBXItem7: TSpTBXItem
          Action = act_export_html
        end
        object SpTBXSeparatorItem2: TSpTBXSeparatorItem
        end
        object but_close: TSpTBXItem
          Tag = 6
          Action = act_close
        end
      end
      object SpTBXSubmenuItem2: TSpTBXSubmenuItem
        Tag = 200
        Caption = 'Edit'
        object but_undo: TSpTBXItem
          Tag = 21
          Action = act_undo
        end
        object but_redo: TSpTBXItem
          Tag = 22
          Action = act_redo
        end
        object SpTBXSeparatorItem4: TSpTBXSeparatorItem
        end
        object but_copy: TSpTBXItem
          Tag = 23
          Action = act_copy
        end
        object but_cut: TSpTBXItem
          Tag = 24
          Action = act_cut
        end
        object but_paste: TSpTBXItem
          Tag = 25
          Action = act_paste
        end
        object but_delete: TSpTBXItem
          Tag = 26
          Action = act_delete
        end
        object SpTBXSeparatorItem13: TSpTBXSeparatorItem
        end
        object SpTBXItem8: TSpTBXItem
          Action = act_copy_as_html
        end
        object SpTBXSeparatorItem5: TSpTBXSeparatorItem
        end
        object but_select_all: TSpTBXItem
          Tag = 27
          Action = act_select_all
        end
      end
      object SpTBXSubmenuItem3: TSpTBXSubmenuItem
        Tag = 300
        Caption = 'Search'
        object SpTBXItem5: TSpTBXItem
          Action = act_find
        end
        object SpTBXItem4: TSpTBXItem
          Action = act_find_next
        end
        object SpTBXItem3: TSpTBXItem
          Action = act_find_previous
        end
        object SpTBXSeparatorItem6: TSpTBXSeparatorItem
        end
        object SpTBXItem2: TSpTBXItem
          Action = act_replace
        end
      end
      object SpTBXSubmenuItem4: TSpTBXSubmenuItem
        Caption = 'View'
        object SpTBXItem6: TSpTBXItem
          Action = act_wordwrap
        end
        object SpTBXItem10: TSpTBXItem
          Action = act_special_chars
        end
        object SpTBXSeparatorItem17: TSpTBXSeparatorItem
        end
        object SpTBXSubmenuItem6: TSpTBXSubmenuItem
          Caption = 'Toolbars'
          object SpTBXItem35: TSpTBXItem
            Action = act_show_bookmark_toolbar
          end
          object SpTBXItem36: TSpTBXItem
            Action = act_show_statusbar
          end
        end
        object SpTBXSeparatorItem18: TSpTBXSeparatorItem
        end
        object SpTBXItem25: TSpTBXItem
          Action = act_playback_enabled
        end
        object SpTBXSeparatorItem14: TSpTBXSeparatorItem
        end
        object SpTBXItem9: TSpTBXItem
          Action = act_options
        end
      end
      object sub_highlighter: TSpTBXSubmenuItem
        Caption = 'Highlighter'
      end
    end
    object toolbar_bookmarks: TSpTBXToolbar
      Left = 218
      Top = 0
      DockMode = dmCannotFloat
      DockPos = 218
      Options = [tboNoRotation]
      Stretch = True
      TabOrder = 1
      Visible = False
      Caption = 'Bookmarks'
      object SpTBXItem34: TSpTBXItem
        Caption = '1'
        Action = act_bookmark_1
      end
      object SpTBXItem33: TSpTBXItem
        Caption = '2'
        Action = act_bookmark_2
      end
      object SpTBXItem32: TSpTBXItem
        Caption = '3'
        Action = act_bookmark_3
      end
      object SpTBXItem31: TSpTBXItem
        Caption = '4'
        Action = act_bookmark_4
      end
      object SpTBXItem30: TSpTBXItem
        Caption = '5'
        Action = act_bookmark_5
      end
      object SpTBXItem29: TSpTBXItem
        Caption = '6'
        Action = act_bookmark_6
      end
      object SpTBXItem28: TSpTBXItem
        Caption = '7'
        Action = act_bookmark_7
      end
      object SpTBXItem27: TSpTBXItem
        Caption = '8'
        Action = act_bookmark_8
      end
      object SpTBXItem26: TSpTBXItem
        Caption = '9'
        Action = act_bookmark_9
      end
    end
  end
  object StatusBar: TSpTBXStatusBar
    Left = 0
    Top = 394
    Width = 684
    Height = 25
    SizeGrip = False
    ExplicitTop = 409
    object label_status: TSpTBXLabelItem
      Wrapping = twPathEllipsis
    end
    object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
      CustomWidth = 529
    end
    object sep_modified: TSpTBXSeparatorItem
    end
    object label_modified: TSpTBXLabelItem
      Caption = 'Modified'
    end
    object SpTBXSeparatorItem3: TSpTBXSeparatorItem
    end
    object label_position: TSpTBXLabelItem
    end
    object SpTBXSeparatorItem8: TSpTBXSeparatorItem
    end
    object but_insertmode: TSpTBXItem
      Tag = 1001
      Caption = 'Insert'
      OnClick = ActionExecute
    end
    object SpTBXSeparatorItem7: TSpTBXSeparatorItem
    end
    object label_stats: TSpTBXLabelItem
    end
    object SpTBXSeparatorItem11: TSpTBXSeparatorItem
    end
    object label_format: TSpTBXLabelItem
    end
  end
  object SynMemo: TSynMemo
    Left = 9
    Top = 25
    Width = 666
    Height = 315
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = EditorPopupMenu
    TabOrder = 2
    BorderStyle = bsNone
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Options = [eoAutoIndent, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
    WantTabs = True
    OnChange = SynMemoChange
    OnStatusChange = SynMemoStatusChange
  end
  object LeftDock: TSpTBXDock
    Left = 0
    Top = 25
    Width = 9
    Height = 315
    Position = dpLeft
  end
  object RightDock: TSpTBXDock
    Left = 675
    Top = 25
    Width = 9
    Height = 315
    Position = dpRight
  end
  object DockBottom: TSpTBXDock
    Left = 0
    Top = 340
    Width = 684
    Height = 54
    Position = dpBottom
    object toolbar_find: TCEToolbar
      Left = 0
      Top = 0
      BorderStyle = bsNone
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      DragHandleStyle = dhNone
      FullSize = True
      Images = PngImageList1
      TabOrder = 0
      Caption = 'Find'
      object but_close_search: TSpTBXItem
        Tag = 301
        Caption = 'Close'
        ImageIndex = 0
        OnClick = ActionExecute
      end
      object SpTBXSeparatorItem9: TSpTBXSeparatorItem
      end
      object label_find: TSpTBXLabelItem
        Caption = 'Find:'
        Alignment = taRightJustify
      end
      object edit_search: TCEToolbarEditItem
        CustomWidth = 128
        OnAcceptText = edit_searchAcceptText
        OnChange = edit_searchChange
        AutoShowClearButton = True
        DefaultWidth = 128
        ShowClearButton = False
      end
      object CEToolbarFixedSpacerItem2: TCEToolbarFixedSpacerItem
        CustomHeight = 16
        CustomWidth = 3
      end
      object but_search_next: TSpTBXItem
        Tag = 302
        Caption = 'Next'
        DisplayMode = nbdmImageAndText
        ImageIndex = 1
        OnClick = ActionExecute
      end
      object but_search_prev: TSpTBXItem
        Tag = 303
        Caption = 'Previous'
        DisplayMode = nbdmImageAndText
        ImageIndex = 2
        OnClick = ActionExecute
      end
      object CEToolbarFixedSpacerItem1: TCEToolbarFixedSpacerItem
        CustomHeight = 16
        CustomWidth = 6
      end
      object but_search_options: TSpTBXSubmenuItem
        Caption = 'Options'
        ImageIndex = 3
        Options = [tboDropdownArrow]
        object check_case_sensitive: TSpTBXItem
          Caption = 'Case sensitive'
          AutoCheck = True
        end
        object check_whole_word: TSpTBXItem
          Caption = 'Whole words only'
          AutoCheck = True
        end
        object check_selected_text_only: TSpTBXItem
          Caption = 'Selected text only'
          AutoCheck = True
        end
        object check_regex: TSpTBXItem
          Caption = 'Regular expression'
          AutoCheck = True
        end
        object check_wrap_around: TSpTBXItem
          Caption = 'Wrap around'
          AutoCheck = True
        end
      end
    end
    object toolbar_replace: TCEToolbar
      Left = 0
      Top = 27
      BorderStyle = bsNone
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      DockRow = 1
      DragHandleStyle = dhNone
      FullSize = True
      Images = PngImageList1
      TabOrder = 1
      Caption = 'Replace'
      object SpTBXItem1: TSpTBXItem
        Tag = 304
        Caption = 'Close'
        ImageIndex = 0
        OnClick = ActionExecute
      end
      object SpTBXSeparatorItem10: TSpTBXSeparatorItem
      end
      object label_replace: TSpTBXLabelItem
        Caption = 'Replace:'
        Alignment = taRightJustify
      end
      object edit_replace: TCEToolbarEditItem
        CustomWidth = 128
        OnChange = edit_searchChange
        AutoShowClearButton = True
        DefaultWidth = 128
        ShowClearButton = False
      end
      object CEToolbarFixedSpacerItem3: TCEToolbarFixedSpacerItem
        CustomHeight = 16
        CustomWidth = 3
      end
      object but_replace: TSpTBXItem
        Action = act_replace_selected
      end
      object but_replace_all: TSpTBXItem
        Action = act_replace_all
      end
    end
  end
  object PngImageList1: TPngImageList
    PngImages = <
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000000D54944415478DAAD92B10A83301086EF0621109076EB66
          C7BAB5EFFF10764BC7BA756B118480C33589462F314A6B7B1012F2DF7DFE9713
          89087E09FC1B0011ED2686A587C563A6D95A0EB0E291F25C61D394E67C6790A4
          16037624E573B4D6B63E115CB1948A697BB3BDD20E849812B5BE38AB4254EC6E
          D1C16435CB4648F0605D17B4960278C8C9DC564131807573E38FBB0A68110380
          24FA08E05A7820265B3810ADB6E08A6B565CF45F859AB9291864364685388EB1
          9C12DD1815031B6D798C5793788EACFAD6626DE90D36FFCAF06D0480ADF1065F
          C3B4E1D5CAF70B0000000049454E44AE426082}
        Name = 'PngImage0'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D4948445200000010000000100803000000282D0F
          530000000373424954080808DBE14FE00000001974455874536F667477617265
          007777772E696E6B73636170652E6F72679BEE3C1A00000135504C5445FFFFFF
          3A73043A73043A73043C76043B74043C76043B74043B75043B75043A74043B75
          043B76043C76043A74044B821852891E568B24578C263F780A40780D427A0E42
          7B0E5689273B7604659A347AA54F72A83F7AAA4C84B7558DBF5D92C55F99BF75
          3A73044E9A0653A20858A90C59AA0A59AB0B5AAB0C5EB30D5FB40D62B90F63AA
          1F65BC0F65BD1067C01169B61E69C4126AC5126ECB146FAD3471AE3773C91F78
          B24179C3317BC5327CB4477DC23B7EBC4282B85082BA4D85B95486BA5587C051
          87C34D88BD5689BC5989C25289C54E8AC0588CBD5D8DBE5D8DBE5F8DD14C90D5
          4E97C76898CC659FC7789FCE71A0D66DA2D174A3CA7EA3D375A4CC7EA4CD7EA5
          CE7FA5E06CA8DB78A9D183B1D291B1D390B7DB94B8D69BBFDAA6C2DBAAC2DCAA
          C3DCABC5DDADC6DEAFC7DEB0C7DFB1C8DFB18E2900140000002174524E53000C
          2D659DABABACACC5E1E1E1E1E8F5F5F5F5F6F6F6F6F6F7F8F8F9FAFEFEFEFE14
          382A6E000000CD4944415478DA4DCECF0BC1601807F0E77DB7428A32E1E22029
          0707C545B98D687FB1AB2C37B93828073F0FB4EDA5612F9B778F8DD19EC3B7A7
          4F3D3F08049551C204CB0E82845D4515E2F580F9FA0FFDBD7367CD18F40E0E37
          E3A0EEB9778A205502E0DAF6C9CFCD690AE048A4FC50467FE7B1470BD11DDF08
          2414D5F05DCF44C4FACCF4483054E81882B980E5257330584A7303DBBA50922E
          EA67FF7345CA6A2B2EC9D5D155446793F9F686D674C3FDFD41D2B90659B01BFE
          00A8D2A513D3873F8094015B400C4086D7B7790315B4591D9F64987400000000
          49454E44AE426082}
        Name = 'PngImage1'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000000473424954080808087C0864880000001974455874536F6674776172
          65007777772E696E6B73636170652E6F72679BEE3C1A0000025C4944415478DA
          8D934B4C53411486FFB9F49636B221211489C5C474872C5CF808A00B302A2418
          35694C949DA21BC552445998685D48427994850B54A22ED09034A888B25349A8
          A48A42158C5544A446A8406BA5EDED7DF48ED3128826543A9B4926E77CE7FCFF
          394328A548754A2FF05D897BA8593E912A86A402949CE76B8CB92607858AEF3F
          A72CAE16F966DA00965C94A9DDE03E75B8512FC705DCEE7308A214DDC920EFD7
          05B0E42C104C5457D41A353A10391E432814A2FD833D3E50143248F8BF80D206
          DEB97B5BE5C15D45E5FC9B990188AC83CDD98598F8E491C63EBAFB5C76D99C12
          90D46D30759CAC6AD47B7E3C47580C40628099E007ECD972144F5E38A3F3C1D9
          7FFC580524746B79BDFB6275BBDE1F99C27C641AB22A20109BC5D7E018366699
          50985386FBFDB704459156FD480256749F3E74C998936D20DF7EBD830C01921A
          C5E7C5612C290130FDD89A5B06BAA4A303830F7C545DF6230948E8DEBBFD48D5
          811DC7B4DE85614834028501E685697CF9FD0A844312009543E9A6E31879FB5A
          1CF78E3E66FB6126C5F59A9A823C5387D5DCAAD7F2996CEA0AE2ACFEC8DC4378
          169F42E1A20C405059508F443155052449C4BD475DD14070C1424A1A342ED64E
          F18A291C8F587B6DAFEE99AF13936117345A024E43B03FDF8A964E5B4C11A96E
          D5400E2FD7DA03EAB03AD13B6543303E0D5EC781CFE4509E6BC1B5B6CB60BAC9
          7A8B44DBEA7AD0EDB542E442AC030E19AC837DF97568725C490F603FD78D3BE3
          67A16AC46432C920A8305AD17CDD961EA0E9CC5DF8A39349F7130612B23C85D6
          1B57D30034F0732CD8B0E6CF23F00FD9E5BCBFDFFE006E9E3B8507D9688D0000
          000049454E44AE426082}
        Name = 'PngImage2'
        Background = clWindow
      end
      item
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000027C4944415478DA95934D4C134114C7FF33BBB32DDB965A
          1A5A5B08250142B4128D31410F62C43344459410A3C68B4609214444919B1C4C
          2442022A8A315E8C201CB0C2D9931EE068C06888180EF265680B746BDBFD70B6
          4A62ADF8F1929979BB33EFF7DEBCF786E00FD2F1E0E52B0A94775DA8F16F7586
          64180C8E1F1629ED5135E38661E85312A51F044A9C1BC9D46E4337AA4542DAE2
          C9E4D1DE96FAA9DF023A07274E30818E805B1B7C1002D130C03FF96C2A04642D
          9E3CD6D35C379601E878385E492961FC8CC76D9787DC4E99396C5668BA014DD3
          A1F3351253105E538CD5A87235A569939CAFF6B59E7C43DA0742B956267EA220
          AE5C9B05051E1737D2B0128921B2A1E8E0BE6D3992205B2CE920BE44A250E249
          13AEDE6E3ECE48DBBD1792240A039240CE057C6E223111F34B613D1A4B3CE2B6
          5D1CA072CB7EAB28D43A1D56319150D39128A9E493BED653E7D35768BF1FF2C8
          4C580C9616109FCCCC8B697C43FF393FDCBBC067BAB8B20C6FBE074B7CF579BC
          29726D2034C228D92F08B47047B11F850E869D6505F8177937FB394578AD9798
          483DA6BFF2800F816D5216601D0CC31F23A8F352B86C39990053B9D23F56C3F3
          142A2BF2A2244FCE02DC9D5EC73EFB575406F2B323309596DED11AD9C2C67CF9
          2EBAB7C8CD017E2C8415789D564CCCAE6166218CEBD5A5D07975B20097BB8725
          AB459CB330E6CF610CD515456940EDE3B7A8DFB31DA1F7ABB875280F25FE4CEF
          191134DD797E51D7A10AC4686838183C625E617E258A33A373381DB4E1EC8162
          300EDF12B02997BA9F353656553C0DA67340B01C8DC12E5138ECB67473FD15D0
          D43DD4D950B5EB66F07FCAF8EBCFD793D32A7F44FAF7E63144F3095142523F9A
          896EEE6DCA372D3F0FE0A65E02A00000000049454E44AE426082}
        Name = 'PngImage3'
        Background = clWindow
      end>
    Left = 104
    Top = 288
    Bitmap = {}
  end
  object ActionList: TTntActionList
    Images = PngImageList1
    Left = 40
    Top = 288
    object act_new: TTntAction
      Tag = 101
      Category = 'File'
      Caption = 'New'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_open: TTntAction
      Tag = 102
      Category = 'File'
      Caption = 'Open...'
      ShortCut = 16463
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_reload: TTntAction
      Tag = 103
      Category = 'File'
      Caption = 'Reload from Disk'
      ShortCut = 16466
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_save: TTntAction
      Tag = 104
      Category = 'File'
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_saveas: TTntAction
      Tag = 105
      Category = 'File'
      Caption = 'Save As...'
      ShortCut = 49235
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_close: TTntAction
      Tag = 106
      Category = 'File'
      Caption = 'Close'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_undo: TTntAction
      Tag = 201
      Category = 'Edit'
      Caption = 'Undo'
      ShortCut = 16474
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_redo: TTntAction
      Tag = 202
      Category = 'Edit'
      Caption = 'Redo'
      ShortCut = 16473
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_copy: TTntAction
      Tag = 203
      Category = 'Edit'
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_cut: TTntAction
      Tag = 204
      Category = 'Edit'
      Caption = 'Cut'
      ShortCut = 16472
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_paste: TTntAction
      Tag = 205
      Category = 'Edit'
      Caption = 'Paste'
      ShortCut = 16470
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_delete: TTntAction
      Tag = 206
      Category = 'Edit'
      Caption = 'Delete'
      ShortCut = 46
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_select_all: TTntAction
      Tag = 207
      Category = 'Edit'
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_find: TTntAction
      Tag = 301
      Category = 'Search'
      Caption = 'Find'
      ShortCut = 16454
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_find_next: TTntAction
      Tag = 302
      Category = 'Search'
      Caption = 'Find Next'
      ShortCut = 114
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_find_previous: TTntAction
      Tag = 303
      Category = 'Search'
      Caption = 'Find Previous'
      ShortCut = 8306
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_replace: TTntAction
      Tag = 304
      Category = 'Search'
      Caption = 'Replace'
      ShortCut = 16456
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_wordwrap: TTntAction
      Tag = 401
      Category = 'View'
      Caption = 'Word Wrap'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_export_html: TTntAction
      Tag = 107
      Category = 'File'
      Caption = 'Export as HTML...'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_copy_as_html: TTntAction
      Tag = 208
      Category = 'Edit'
      Caption = 'Copy as HTML'
      ShortCut = 49219
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_options: TTntAction
      Tag = 402
      Category = 'View'
      Caption = 'Options...'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_special_chars: TTntAction
      Tag = 403
      Category = 'View'
      Caption = 'Show Special Characters'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_bookmark_1: TTntAction
      Tag = 501
      Category = 'Bookmarks'
      Caption = 'Bookmark 1'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_bookmark_2: TTntAction
      Tag = 502
      Category = 'Bookmarks'
      Caption = 'Bookmark 2'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_bookmark_3: TTntAction
      Tag = 503
      Category = 'Bookmarks'
      Caption = 'Bookmark 3'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_bookmark_4: TTntAction
      Tag = 504
      Category = 'Bookmarks'
      Caption = 'Bookmark 4'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_bookmark_5: TTntAction
      Tag = 505
      Category = 'Bookmarks'
      Caption = 'Bookmark 5'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_bookmark_6: TTntAction
      Tag = 506
      Category = 'Bookmarks'
      Caption = 'Bookmark 6'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_bookmark_7: TTntAction
      Tag = 507
      Category = 'Bookmarks'
      Caption = 'Bookmark 7'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_bookmark_8: TTntAction
      Tag = 508
      Category = 'Bookmarks'
      Caption = 'Bookmark 8'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_bookmark_9: TTntAction
      Tag = 509
      Category = 'Bookmarks'
      Caption = 'Bookmark 9'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_playback_enabled: TTntAction
      Tag = 404
      Category = 'View'
      Caption = 'Enable Playback'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_show_bookmark_toolbar: TTntAction
      Tag = 405
      Category = 'View'
      Caption = 'Bookmarks'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_show_statusbar: TTntAction
      Tag = 406
      Category = 'View'
      Caption = 'Status Bar'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_replace_selected: TTntAction
      Tag = 305
      Category = 'Search'
      Caption = 'Replace'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_replace_all: TTntAction
      Tag = 306
      Category = 'Search'
      Caption = 'Replace All'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
  end
  object StatusTimer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = StatusTimerTimer
    Left = 72
    Top = 288
  end
  object NormalSearch: TSynEditSearch
    Left = 592
    Top = 288
  end
  object RegexSearch: TSynEditRegexSearch
    Left = 624
    Top = 288
  end
  object SynCppSyn1: TSynCppSyn
    DefaultFilter = 'C++ files (*.cpp,*.h,*.hpp)|*.cpp;*.h;*.hpp'
    AsmAttri.Background = clBlack
    AsmAttri.Foreground = clLime
    CommentAttri.Foreground = clGreen
    DirecAttri.Foreground = clMaroon
    InvalidAttri.Foreground = clRed
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    FloatAttri.Foreground = clBlue
    HexAttri.Foreground = clBlue
    OctalAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    CharAttri.Foreground = clBlue
    Left = 40
    Top = 60
  end
  object SynPasSyn1: TSynPasSyn
    AsmAttri.Background = clBlack
    AsmAttri.Foreground = clLime
    CommentAttri.Foreground = clGreen
    DirectiveAttri.Foreground = clTeal
    DirectiveAttri.Style = []
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    FloatAttri.Foreground = clBlue
    HexAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    CharAttri.Foreground = clBlue
    Left = 72
    Top = 60
  end
  object SynSQLSyn1: TSynSQLSyn
    DefaultFilter = 'SQL files (*.sql)|*.sql'
    CommentAttri.Foreground = clGreen
    IdentifierAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    SQLDialect = sqlSybase
    Left = 104
    Top = 60
  end
  object SynVBSyn1: TSynVBSyn
    CommentAttri.Foreground = clGreen
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 136
    Top = 60
  end
  object SynJavaSyn1: TSynJavaSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 168
    Top = 60
  end
  object SynCSSyn1: TSynCSSyn
    AsmAttri.Background = clBlack
    AsmAttri.Foreground = clLime
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 200
    Top = 60
  end
  object SynCssSyn1: TSynCssSyn
    CommentAttri.Foreground = clGreen
    CommentAttri.Style = [fsItalic]
    ColorAttri.Foreground = clBlue
    NumberAttri.Foreground = clBlue
    KeyAttri.Foreground = clNavy
    StringAttri.Foreground = clBlue
    TextAttri.Foreground = clBlue
    ValueAttri.Foreground = clBlue
    Left = 232
    Top = 60
  end
  object SynHTMLSyn1: TSynHTMLSyn
    CommentAttri.Foreground = clGreen
    CommentAttri.Style = [fsItalic]
    KeyAttri.Foreground = clNavy
    ValueAttri.Foreground = clBlue
    Left = 264
    Top = 60
  end
  object SynJScriptSyn1: TSynJScriptSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 40
    Top = 128
  end
  object SynPHPSyn1: TSynPHPSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    VariableAttri.Style = [fsBold]
    Left = 72
    Top = 128
  end
  object SynVBScriptSyn1: TSynVBScriptSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 104
    Top = 128
  end
  object SynXMLSyn1: TSynXMLSyn
    ElementAttri.Foreground = clNavy
    AttributeAttri.Foreground = clNone
    AttributeAttri.Style = [fsBold]
    NamespaceAttributeAttri.Foreground = clNone
    AttributeValueAttri.Foreground = clBlue
    AttributeValueAttri.Style = []
    NamespaceAttributeValueAttri.Foreground = clNone
    TextAttri.Foreground = clNone
    TextAttri.Style = []
    CDATAAttri.Foreground = clNone
    CDATAAttri.Style = []
    EntityRefAttri.Foreground = clNone
    ProcessingInstructionAttri.Foreground = clNone
    CommentAttri.Background = clNone
    CommentAttri.Foreground = clGreen
    CommentAttri.Style = [fsItalic]
    DocTypeAttri.Foreground = clTeal
    SymbolAttri.Foreground = clNone
    WantBracesParsed = False
    Left = 136
    Top = 128
  end
  object SynBatSyn1: TSynBatSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    VariableAttri.Foreground = clNone
    VariableAttri.Style = [fsBold]
    Left = 168
    Top = 128
  end
  object SynPerlSyn1: TSynPerlSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 200
    Top = 128
  end
  object SynPythonSyn1: TSynPythonSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    Left = 232
    Top = 128
  end
  object SynTclTkSyn1: TSynTclTkSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    OptionsAttri.Background = clNone
    OptionsAttri.Foreground = clNone
    PathAttri.Background = clNone
    PathAttri.Foreground = clNone
    VariableAttri.Background = clNone
    VariableAttri.Foreground = clNone
    VariableAttri.Style = [fsBold]
    Left = 264
    Top = 128
  end
  object SynRubySyn1: TSynRubySyn
    CommentAttri.Foreground = clGreen
    CommentAttri.Style = [fsItalic]
    KeyAttri.Foreground = clNavy
    KeyAttri.Style = [fsBold]
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    SymbolAttri.Foreground = clNone
    Left = 40
    Top = 160
  end
  object SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn
    CommentAttri.Style = [fsItalic]
    StringAttri.Foreground = clBlue
    SymbolAttri.Foreground = clNone
    VarAttri.Foreground = clNone
    VarAttri.Style = [fsBold]
    Left = 72
    Top = 160
  end
  object SynAsmSyn1: TSynAsmSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 40
    Top = 92
  end
  object SynDfmSyn1: TSynDfmSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 72
    Top = 92
  end
  object SynIniSyn1: TSynIniSyn
    TextAttri.Foreground = clNavy
    SectionAttri.Foreground = clNavy
    KeyAttri.Style = [fsBold]
    NumberAttri.Foreground = clNavy
    StringAttri.Foreground = clNavy
    Left = 104
    Top = 92
  end
  object SynInnoSyn1: TSynInnoSyn
    ConstantAttri.Style = [fsBold]
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNone
    KeyAttri.Style = []
    NumberAttri.Foreground = clBlue
    ParameterAttri.Foreground = clNone
    ParameterAttri.Style = []
    SectionAttri.Foreground = clNavy
    Left = 136
    Top = 92
  end
  object SynRCSyn1: TSynRCSyn
    CommentAttri.Foreground = clGreen
    CommentAttri.Style = [fsItalic]
    DirecAttri.Foreground = clTeal
    DirecAttri.Style = [fsBold]
    IdentifierAttri.Foreground = clNavy
    IdentifierAttri.Style = [fsBold]
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 168
    Top = 92
  end
  object SynURISyn1: TSynURISyn
    Left = 200
    Top = 92
  end
  object SynFortranSyn1: TSynFortranSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 232
    Top = 92
  end
  object SynCobolSyn1: TSynCobolSyn
    CommentAttri.Foreground = clGreen
    AreaAStartPos = 7
    AreaBStartPos = 11
    CodeEndPos = 71
    Left = 264
    Top = 92
  end
  object SynMultiHighlighter: TSynMultiSyn
    DefaultFilter = 
      'HTML and PHP (*.htm;*.html;*.php;*.php3;*.phtml)|*.htm;*.html;*.' +
      'php;*.php3;*.phtml'
    Schemes = <
      item
        CaseSensitive = False
        StartExpr = '<[ \t]*style\b[^>]*>'
        EndExpr = '</[ \t]*style[ \t]*>'
        Highlighter = SynCssSyn1
        MarkerAttri.Background = 11253980
        MarkerAttri.Foreground = clBlack
        SchemeName = 'CSS'
        StartExprW = '<[ \t]*style\b[^>]*>'
        EndExprW = '</[ \t]*style[ \t]*>'
      end
      item
        CaseSensitive = False
        StartExpr = '<[ \t]*script\b[^>]*>'
        EndExpr = '</[ \t]*script[ \t]*>'
        Highlighter = SynJScriptSyn1
        MarkerAttri.Background = 9883283
        MarkerAttri.Foreground = clBlack
        SchemeName = 'JavaScript'
        StartExprW = '<[ \t]*script\b[^>]*>'
        EndExprW = '</[ \t]*script[ \t]*>'
      end
      item
        StartExpr = '<\?[ \t]*php\b'
        EndExpr = '\?>'
        Highlighter = SynPHPSyn1
        MarkerAttri.Background = 14728122
        MarkerAttri.Foreground = clBlack
        SchemeName = 'PHP'
        StartExprW = '<\?[ \t]*php\b'
        EndExprW = '\?>'
      end>
    DefaultHighlighter = SynHTMLSyn1
    DefaultLanguageName = 'HTML'
    Left = 40
    Top = 200
  end
  object EditorPopupMenu: TSpTBXPopupMenu
    Left = 136
    Top = 288
    object SpTBXItem11: TSpTBXItem
      Action = act_copy
    end
    object SpTBXItem12: TSpTBXItem
      Action = act_cut
    end
    object SpTBXItem13: TSpTBXItem
      Action = act_paste
    end
    object SpTBXItem14: TSpTBXItem
      Action = act_delete
    end
    object SpTBXSeparatorItem15: TSpTBXSeparatorItem
    end
    object SpTBXItem15: TSpTBXItem
      Action = act_select_all
    end
    object SpTBXSeparatorItem16: TSpTBXSeparatorItem
    end
    object SpTBXSubmenuItem5: TSpTBXSubmenuItem
      Caption = 'Bookmarks'
      object SpTBXItem24: TSpTBXItem
        Action = act_bookmark_1
      end
      object SpTBXItem23: TSpTBXItem
        Action = act_bookmark_2
      end
      object SpTBXItem22: TSpTBXItem
        Action = act_bookmark_3
      end
      object SpTBXItem21: TSpTBXItem
        Action = act_bookmark_4
      end
      object SpTBXItem20: TSpTBXItem
        Action = act_bookmark_5
      end
      object SpTBXItem19: TSpTBXItem
        Action = act_bookmark_6
      end
      object SpTBXItem18: TSpTBXItem
        Action = act_bookmark_7
      end
      object SpTBXItem17: TSpTBXItem
        Action = act_bookmark_8
      end
      object SpTBXItem16: TSpTBXItem
        Action = act_bookmark_9
      end
    end
  end
  object URIOpener: TSynURIOpener
    Editor = SynMemo
    URIHighlighter = SynURISyn1
    Left = 120
    Top = 200
  end
end
