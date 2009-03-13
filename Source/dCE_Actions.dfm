object CEActions: TCEActions
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 305
  Width = 385
  object ActionList: TTntActionList
    Images = CE_Images.SmallIcons
    Left = 24
    Top = 12
    object act_tabs_closetab: TTntAction
      Tag = 661
      Category = 'Tabs'
      Caption = 'Close Tab'
      Hint = 'Close Tab'
      ImageIndex = 10
      ShortCut = 16471
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = ActionExecute
    end
    object act_gen_exit: TTntAction
      Tag = 100
      Category = 'General'
      Caption = 'Exit'
      ImageIndex = 0
      ShortCut = 32883
      OnExecute = ActionExecute
    end
    object act_view_dropstack: TTntAction
      Tag = 305
      Category = 'View'
      Caption = 'Drop Stack'
      ShortCut = 16437
      OnExecute = ActionExecute
    end
    object act_navi_forward: TCEToolbarAction
      Tag = 604
      Category = 'Navigation'
      Caption = 'Forward'
      ImageIndex = 6
      ShortCut = 32807
      OnExecute = ActionExecute
    end
    object act_navi_back: TCEToolbarAction
      Tag = 603
      Category = 'Navigation'
      Caption = 'Back'
      ImageIndex = 5
      ShortCut = 32805
      OnExecute = ActionExecute
    end
    object act_navi_folderup: TTntAction
      Tag = 605
      Category = 'Navigation'
      Caption = 'Folder Up'
      ImageIndex = 7
      OnExecute = ActionExecute
    end
    object act_navi_refresh: TTntAction
      Tag = 606
      Category = 'Navigation'
      Caption = 'Refresh'
      ImageIndex = 8
      ShortCut = 116
      OnExecute = ActionExecute
    end
    object act_view_folders: TTntAction
      Tag = 301
      Category = 'View'
      Caption = 'Folders'
      ImageIndex = 28
      ShortCut = 16433
      OnExecute = ActionExecute
    end
    object act_view_bookmark: TTntAction
      Tag = 302
      Category = 'View'
      Caption = 'Bookmarks'
      ImageIndex = 18
      ShortCut = 16434
      OnExecute = ActionExecute
    end
    object act_view_large: TTntAction
      Tag = 351
      Category = 'View'
      Caption = 'Large Icons'
      ImageIndex = 11
      OnExecute = ActionExecute
    end
    object act_view_small: TTntAction
      Tag = 352
      Category = 'View'
      Caption = 'Small Icons'
      ImageIndex = 12
      OnExecute = ActionExecute
    end
    object act_view_list: TTntAction
      Tag = 353
      Category = 'View'
      Caption = 'List'
      ImageIndex = 13
      OnExecute = ActionExecute
    end
    object act_view_showheaderalways: TTntAction
      Tag = 333
      Category = 'View'
      Caption = 'Always Show Sort Columns'
      OnExecute = ActionExecute
    end
    object act_view_filters: TTntAction
      Tag = 304
      Category = 'View'
      Caption = 'Filters'
      ImageIndex = 29
      ShortCut = 16436
      OnExecute = ActionExecute
    end
    object act_view_details: TTntAction
      Tag = 354
      Category = 'View'
      Caption = 'Details'
      ImageIndex = 14
      OnExecute = ActionExecute
    end
    object act_view_thumbs: TTntAction
      Tag = 355
      Category = 'View'
      Caption = 'Thumbnails'
      ImageIndex = 16
      OnExecute = ActionExecute
    end
    object act_view_loadskin: TTntAction
      Tag = 371
      Category = 'View'
      Caption = 'Load Skin...'
      Hint = 'Load skin from file'
      OnExecute = ActionExecute
    end
    object act_view_tiles: TTntAction
      Tag = 356
      Category = 'View'
      Caption = 'Tiles'
      ImageIndex = 15
      OnExecute = ActionExecute
    end
    object act_view_alwaysontop: TTntAction
      Tag = 335
      Category = 'View'
      Caption = 'Always On Top'
      OnExecute = ActionExecute
    end
    object act_view_groupby: TCEToolbarAction
      Tag = 374
      Category = 'View'
      Caption = 'Group By'
      ImageIndex = 31
      OnExecute = ActionExecute
    end
    object act_view_viewstyle: TCEToolbarAction
      Tag = 373
      Category = 'View'
      Caption = 'View Style'
      ImageIndex = 16
      OnExecute = ActionExecute
    end
    object act_view_filmstrip: TTntAction
      Tag = 357
      Category = 'View'
      Caption = 'Filmstrip'
      ImageIndex = 17
      OnExecute = ActionExecute
    end
    object act_edit_copy: TTntAction
      Tag = 201
      Category = 'Edit'
      Caption = 'Copy'
      ImageIndex = 1
      ShortCut = 16451
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = ActionExecute
    end
    object act_edit_cut: TTntAction
      Tag = 202
      Category = 'Edit'
      Caption = 'Cut'
      ImageIndex = 2
      ShortCut = 16472
      OnExecute = ActionExecute
    end
    object act_edit_paste: TTntAction
      Tag = 203
      Category = 'Edit'
      Caption = 'Paste'
      ImageIndex = 3
      ShortCut = 16470
      SecondaryShortCuts.Strings = (
        'Shift+Ins')
      OnExecute = ActionExecute
    end
    object act_edit_delete: TTntAction
      Tag = 204
      Category = 'Edit'
      Caption = 'Delete'
      ImageIndex = 4
      ShortCut = 46
      OnExecute = ActionExecute
    end
    object act_edit_selall: TTntAction
      Tag = 205
      Category = 'Edit'
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = ActionExecute
    end
    object act_edit_invertsel: TTntAction
      Tag = 206
      Category = 'Edit'
      Caption = 'Invert Selection'
      ShortCut = 24641
      OnExecute = ActionExecute
    end
    object act_edit_properties: TTntAction
      Tag = 207
      Category = 'Edit'
      Caption = 'Properties...'
      ImageIndex = 26
      ShortCut = 32781
      OnExecute = ActionExecute
    end
    object act_edit_rename: TTntAction
      Tag = 208
      Category = 'Edit'
      Caption = 'Rename'
      ShortCut = 113
      OnExecute = ActionExecute
    end
    object act_quick_none: TTntAction
      Tag = 701
      Category = 'Quickview'
      Caption = 'None'
      OnExecute = ActionExecute
    end
    object act_quick_auto: TTntAction
      Tag = 702
      Category = 'Quickview'
      Caption = 'Auto'
      OnExecute = ActionExecute
    end
    object act_quick_text: TTntAction
      Tag = 703
      Category = 'Quickview'
      Caption = 'Text'
      OnExecute = ActionExecute
    end
    object act_quick_image: TTntAction
      Tag = 704
      Category = 'Quickview'
      Caption = 'Image'
      OnExecute = ActionExecute
    end
    object act_quick_hex: TTntAction
      Tag = 705
      Category = 'Quickview'
      Caption = 'Hex'
      OnExecute = ActionExecute
    end
    object act_help_home: TTntAction
      Tag = 501
      Category = 'Help'
      Caption = 'CubicExplore Home Page'
      OnExecute = ActionExecute
    end
    object act_help_forum: TTntAction
      Tag = 502
      Category = 'Help'
      Caption = 'Support Forum'
      OnExecute = ActionExecute
    end
    object act_help_about: TTntAction
      Tag = 503
      Category = 'Help'
      Caption = 'About...'
      OnExecute = ActionExecute
    end
    object act_view_quickview: TTntAction
      Tag = 303
      Category = 'View'
      Caption = 'Quickview'
      ImageIndex = 20
      ShortCut = 16435
      OnExecute = ActionExecute
    end
    object act_edit_duplicate: TTntAction
      Tag = 209
      Category = 'Edit'
      Caption = 'Duplicate'
      ShortCut = 16452
      OnExecute = ActionExecute
    end
    object act_navi_texteditor: TTntAction
      Tag = 650
      Category = 'Navigation'
      Caption = 'Text Editor'
      ImageIndex = 21
      ShortCut = 115
      OnExecute = ActionExecute
    end
    object act_navi_filesearch: TTntAction
      Tag = 651
      Category = 'Navigation'
      Caption = 'File Search'
      ImageIndex = 22
      OnExecute = ActionExecute
    end
    object act_tools_mapdrive: TTntAction
      Tag = 451
      Category = 'Tools'
      Caption = 'Map Network Drive...'
      OnExecute = ActionExecute
    end
    object act_tools_disconnectdrive: TTntAction
      Tag = 452
      Category = 'Tools'
      Caption = 'Disconnect Network Drive...'
      OnExecute = ActionExecute
    end
    object act_edit_newfile: TCEToolbarAction
      Tag = 210
      Category = 'Edit'
      Caption = 'New'
      OnExecute = ActionExecute
    end
    object act_edit_copypath: TCEToolbarAction
      Tag = 211
      Category = 'Edit'
      Caption = 'Copy Path'
      ShortCut = 16466
      OnExecute = ActionExecute
    end
    object act_view_showhints: TTntAction
      Tag = 330
      Category = 'View'
      Caption = 'Show Hints'
      OnExecute = ActionExecute
    end
    object act_tools_showcustomizer: TTntAction
      Tag = 401
      Category = 'Tools'
      Caption = 'Customizer...'
      OnExecute = ActionExecute
    end
    object act_view_hiddenfiles: TTntAction
      Tag = 332
      Category = 'View'
      Caption = 'Show Hidden Files'
      OnExecute = ActionExecute
    end
    object act_edit_newfolder: TTntAction
      Tag = 212
      Category = 'Edit'
      Caption = 'New Folder'
      ImageIndex = 25
      ShortCut = 16462
      OnExecute = ActionExecute
    end
    object act_edit_paste_shortcut: TTntAction
      Tag = 213
      Category = 'Edit'
      Caption = 'Paste Shortcut'
      SecondaryShortCuts.Strings = (
        'Shift+Ins')
      OnExecute = ActionExecute
    end
    object act_tools_emptytrash: TCEToolbarAction
      Tag = 453
      Category = 'Tools'
      Caption = 'Empty Recycle Bin'
      ImageIndex = 24
      OnExecute = ActionExecute
    end
    object act_gen_remembertabs: TTntAction
      Tag = 102
      Category = 'General'
      Caption = 'Remember Tabs'
      OnExecute = ActionExecute
    end
    object act_tools_cmd: TTntAction
      Tag = 454
      Category = 'Tools'
      Caption = 'Open Command Prompt'
      ImageIndex = 27
      OnExecute = ActionExecute
    end
    object act_view_statusbar: TTntAction
      Tag = 300
      Category = 'View'
      Caption = 'Status Bar'
      OnExecute = ActionExecute
    end
    object act_view_fullscreen: TTntAction
      Tag = 370
      Category = 'View'
      Caption = 'Fullscreen'
      ShortCut = 122
      OnExecute = ActionExecute
    end
    object act_view_showextensions: TTntAction
      Tag = 334
      Category = 'View'
      Caption = 'Show Extensions'
      OnExecute = ActionExecute
    end
    object act_help_poedit_form: TTntAction
      Tag = 504
      Category = 'Help'
      Caption = 'Translate CubicExplorer'
      OnExecute = ActionExecute
    end
    object act_tools_showoptions: TTntAction
      Tag = 402
      Category = 'Tools'
      Caption = 'Options...'
      OnExecute = ActionExecute
    end
    object act_sessions_save: TTntAction
      Tag = 851
      Category = 'Sessions'
      Caption = 'Save'
      Hint = 'Save active session.'
      OnExecute = ActionExecute
    end
    object act_sessions_saveas: TTntAction
      Tag = 852
      Category = 'Sessions'
      Caption = 'Save As...'
      Hint = 'Save active session with new name.'
      OnExecute = ActionExecute
    end
    object act_sessions_properties: TTntAction
      Tag = 853
      Category = 'Sessions'
      Caption = 'Properties...'
      Hint = 'Edit session properties.'
      OnExecute = ActionExecute
    end
    object act_sessions_delete: TTntAction
      Tag = 854
      Category = 'Sessions'
      Caption = 'Delete'
      Hint = 'Delete active session.'
      OnExecute = ActionExecute
    end
    object act_view_arrangeby: TCEToolbarAction
      Tag = 372
      Category = 'View'
      Caption = 'Arrange By'
      ImageIndex = 30
      OnExecute = ActionExecute
    end
    object act_navi_scrollleft: TTntAction
      Tag = 608
      Category = 'Navigation'
      Caption = 'Scroll Left'
      OnExecute = ActionExecute
    end
    object act_navi_scrollright: TTntAction
      Tag = 609
      Category = 'Navigation'
      Caption = 'Scroll Right'
      OnExecute = ActionExecute
    end
    object act_tabs_closeothertabs: TTntAction
      Tag = 662
      Category = 'Tabs'
      Caption = 'Close Other Tabs'
      Hint = 'Close Other Tabs'
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = ActionExecute
    end
    object act_tabs_addtab: TTntAction
      Tag = 663
      Category = 'Tabs'
      Caption = 'Add Tab'
      Hint = 'Add Tab'
      ImageIndex = 9
      ShortCut = 16468
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = ActionExecute
    end
    object act_tabs_duplicatetab: TTntAction
      Tag = 664
      Category = 'Tabs'
      Caption = 'Duplicate Tab'
      Hint = 'Duplicate Tab'
      ImageIndex = 32
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = ActionExecute
    end
    object act_tabs_closeonleft: TTntAction
      Tag = 665
      Category = 'Tabs'
      Caption = 'Close Tabs on Left'
      Hint = 'Close Tabs on Left'
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = ActionExecute
    end
    object act_tabs_closeonright: TTntAction
      Tag = 666
      Category = 'Tabs'
      Caption = 'Close Tabs on Right'
      Hint = 'Close Tabs on Right'
      SecondaryShortCuts.Strings = (
        'Ctrl+Ins')
      OnExecute = ActionExecute
    end
    object act_gen_menu: TCEToolbarAction
      Tag = 103
      Category = 'General'
      Caption = 'Menu'
      OnExecute = ActionExecute
    end
    object act_navi_quickview: TTntAction
      Tag = 652
      Category = 'Navigation'
      Caption = 'QuickView'
      ImageIndex = 20
      ShortCut = 119
      OnExecute = ActionExecute
    end
  end
  object UpdateTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = UpdateTimerTimer
    Left = 92
    Top = 12
  end
  object ApplicationEvents: TApplicationEvents
    OnActivate = ApplicationEventsActivate
    OnMessage = ApplicationEventsMessage
    Left = 180
    Top = 12
  end
  object BackgroundCMItems_up: TTntPopupMenu
    OnPopup = BackgroundCMItems_upPopup
    Left = 64
    Top = 104
    object View1: TTntMenuItem
      Caption = 'View'
      object LargeIcons1: TTntMenuItem
        Action = act_view_large
        RadioItem = True
      end
      object SmallIcons1: TTntMenuItem
        Action = act_view_small
        RadioItem = True
      end
      object List1: TTntMenuItem
        Action = act_view_list
        RadioItem = True
      end
      object Details1: TTntMenuItem
        Action = act_view_details
        RadioItem = True
      end
      object iles1: TTntMenuItem
        Action = act_view_tiles
        RadioItem = True
      end
      object humbnails1: TTntMenuItem
        Action = act_view_thumbs
        RadioItem = True
      end
      object Filmstrip1: TTntMenuItem
        Action = act_view_filmstrip
        RadioItem = True
      end
    end
    object N1: TTntMenuItem
      Caption = '-'
    end
    object MenuItem_ArragneBy: TTntMenuItem
      Caption = 'Arrange By'
    end
    object MenuItem_GroupBy: TTntMenuItem
      Caption = 'Group By'
    end
    object Refresh1: TTntMenuItem
      Action = act_navi_refresh
    end
    object N2: TTntMenuItem
      Caption = '-'
    end
    object Paste1: TTntMenuItem
      Action = act_edit_paste
    end
    object PasteShortcut1: TTntMenuItem
      Action = act_edit_paste_shortcut
    end
    object CopyPath1: TTntMenuItem
      Action = act_edit_copypath
    end
    object N4: TTntMenuItem
      Caption = '-'
    end
  end
  object BackgroundCMItems_down: TTntPopupMenu
    Left = 64
    Top = 168
    object N3: TTntMenuItem
      Caption = '-'
    end
    object Properties1: TTntMenuItem
      Action = act_edit_properties
    end
  end
end
