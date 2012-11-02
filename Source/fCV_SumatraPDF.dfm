object SumatraPDF: TSumatraPDF
  Left = 0
  Top = 0
  ClientHeight = 321
  ClientWidth = 544
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object label_status: TTntLabel
    Left = 0
    Top = 25
    Width = 544
    Height = 296
    Align = alClient
    Alignment = taCenter
    Layout = tlCenter
    ExplicitWidth = 3
    ExplicitHeight = 13
  end
  object TopDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 544
    Height = 25
    object toolbar_menu: TCEToolbar
      Left = 0
      Top = 0
      DockMode = dmCannotFloatOrChangeDocks
      DragHandleStyle = dhNone
      Stretch = True
      TabOrder = 0
      object sub_file: TSpTBXSubmenuItem
        Caption = 'File'
        object SpTBXItem3: TSpTBXItem
          Action = act_file_open
        end
        object SpTBXSeparatorItem10: TSpTBXSeparatorItem
        end
        object SpTBXItem2: TSpTBXItem
          Action = act_file_properties
        end
        object SpTBXSeparatorItem1: TSpTBXSeparatorItem
        end
        object SpTBXItem1: TSpTBXItem
          Action = act_file_close
        end
      end
      object sub_view: TSpTBXSubmenuItem
        Caption = 'View'
        object SpTBXItem11: TSpTBXItem
          Action = act_view_single_page
        end
        object SpTBXItem10: TSpTBXItem
          Action = act_view_facing
        end
        object SpTBXItem9: TSpTBXItem
          Action = act_view_book_view
        end
        object SpTBXItem8: TSpTBXItem
          Action = act_view_continuosly
        end
        object SpTBXSeparatorItem2: TSpTBXSeparatorItem
        end
        object SpTBXItem7: TSpTBXItem
          Action = act_view_rotate_left
        end
        object SpTBXItem6: TSpTBXItem
          Action = act_view_rotate_right
        end
        object SpTBXSeparatorItem3: TSpTBXSeparatorItem
        end
        object SpTBXItem12: TSpTBXItem
          Action = act_view_bookmarks
        end
        object SpTBXSeparatorItem5: TSpTBXSeparatorItem
        end
        object SpTBXItem13: TSpTBXItem
          Action = act_view_select_all
        end
        object SpTBXItem14: TSpTBXItem
          Action = act_view_copy_selection
        end
      end
      object sub_goto: TSpTBXSubmenuItem
        Caption = 'Go To'
        object SpTBXItem25: TSpTBXItem
          Action = act_goto_next_page
        end
        object SpTBXItem24: TSpTBXItem
          Action = act_goto_prev_page
        end
        object SpTBXItem23: TSpTBXItem
          Action = act_goto_first_page
        end
        object SpTBXItem22: TSpTBXItem
          Action = act_goto_last_page
        end
        object SpTBXItem21: TSpTBXItem
          Action = act_goto_page
        end
        object SpTBXSeparatorItem7: TSpTBXSeparatorItem
        end
        object SpTBXItem20: TSpTBXItem
          Action = act_goto_back
        end
        object SpTBXItem19: TSpTBXItem
          Action = act_goto_forward
        end
        object SpTBXSeparatorItem8: TSpTBXSeparatorItem
        end
        object SpTBXItem18: TSpTBXItem
          Action = act_goto_find
        end
      end
      object sub_zoom: TSpTBXSubmenuItem
        Caption = 'Zoom'
        object SpTBXItem40: TSpTBXItem
          Action = act_zoom_fit_page
        end
        object SpTBXItem39: TSpTBXItem
          Action = act_zoom_actual_size
        end
        object SpTBXItem38: TSpTBXItem
          Action = act_zoom_fit_width
        end
        object SpTBXItem37: TSpTBXItem
          Action = act_zoom_fit_content
        end
        object SpTBXSeparatorItem9: TSpTBXSeparatorItem
        end
        object SpTBXItem35: TSpTBXItem
          Action = act_zoom_6400
        end
        object SpTBXItem34: TSpTBXItem
          Action = act_zoom_3200
        end
        object SpTBXItem33: TSpTBXItem
          Action = act_zoom_1600
        end
        object SpTBXItem32: TSpTBXItem
          Action = act_zoom_800
        end
        object SpTBXItem31: TSpTBXItem
          Action = act_zoom_400
        end
        object SpTBXItem30: TSpTBXItem
          Action = act_zoom_200
        end
        object SpTBXItem29: TSpTBXItem
          Action = act_zoom_150
        end
        object SpTBXItem28: TSpTBXItem
          Action = act_zoom_125
        end
        object SpTBXItem27: TSpTBXItem
          Action = act_zoom_100
        end
        object SpTBXItem26: TSpTBXItem
          Action = act_zoom_50
        end
        object SpTBXItem17: TSpTBXItem
          Action = act_zoom_25
        end
        object SpTBXItem16: TSpTBXItem
          Action = act_zoom_12_5
        end
        object SpTBXItem41: TSpTBXItem
          Action = act_zoom_8_33
        end
      end
    end
  end
  object ActionList: TTntActionList
    Left = 24
    Top = 48
    object act_file_open: TTntAction
      Tag = 400
      Category = 'File'
      Caption = 'Open...'
      ShortCut = 16463
      OnExecute = ActionExecute
    end
    object act_file_save_as: TTntAction
      Tag = 402
      Category = 'File'
      Caption = 'Save As...'
      ShortCut = 16467
      Visible = False
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_file_close: TTntAction
      Tag = 401
      Category = 'File'
      Caption = 'Close'
      ShortCut = 16471
      OnExecute = ActionExecute
    end
    object act_file_properties: TTntAction
      Tag = 409
      Category = 'File'
      Caption = 'Properties...'
      ShortCut = 16452
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_single_page: TTntAction
      Tag = 410
      Category = 'View'
      Caption = 'Single Page'
      ShortCut = 16438
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_facing: TTntAction
      Tag = 411
      Category = 'View'
      Caption = 'Facing'
      ShortCut = 16439
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_book_view: TTntAction
      Tag = 412
      Category = 'View'
      Caption = 'Book View'
      ShortCut = 16440
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_continuosly: TTntAction
      Tag = 413
      Category = 'View'
      Caption = 'Show pages continuously'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_rotate_left: TTntAction
      Tag = 415
      Category = 'View'
      Caption = 'Rotate Left'
      ShortCut = 24687
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_rotate_right: TTntAction
      Tag = 416
      Category = 'View'
      Caption = 'Rotate Right'
      ShortCut = 24763
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_presentation: TTntAction
      Tag = 418
      Category = 'View'
      Caption = 'Presentation'
      Visible = False
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_fullscreen: TTntAction
      Tag = 421
      Category = 'View'
      Caption = 'Fullscreen'
      Visible = False
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_bookmarks: TTntAction
      Tag = 417
      Category = 'View'
      Caption = 'Bookmarks'
      ShortCut = 123
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_select_all: TTntAction
      Tag = 422
      Category = 'View'
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_copy_selection: TTntAction
      Tag = 420
      Category = 'View'
      Caption = 'Copy Selection'
      ShortCut = 16451
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_view_settings: TTntAction
      Tag = 552
      Category = 'View'
      Caption = 'Options...'
      Visible = False
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_goto_next_page: TTntAction
      Tag = 430
      Category = 'GoTo'
      Caption = 'Next Page'
      ShortCut = 39
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_goto_prev_page: TTntAction
      Tag = 431
      Category = 'GoTo'
      Caption = 'Previous Page'
      ShortCut = 37
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_goto_first_page: TTntAction
      Tag = 432
      Category = 'GoTo'
      Caption = 'First Page'
      ShortCut = 36
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_goto_last_page: TTntAction
      Tag = 433
      Category = 'GoTo'
      Caption = 'Last Page'
      ShortCut = 35
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_goto_page: TTntAction
      Tag = 434
      Category = 'GoTo'
      Caption = 'Page...'
      ShortCut = 16455
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_goto_back: TTntAction
      Tag = 558
      Category = 'GoTo'
      Caption = 'Back'
      ShortCut = 32805
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_goto_forward: TTntAction
      Tag = 559
      Category = 'GoTo'
      Caption = 'Forward'
      ShortCut = 32807
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_goto_find: TTntAction
      Tag = 435
      Category = 'GoTo'
      Caption = 'Find...'
      ShortCut = 16454
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_fit_page: TTntAction
      Tag = 440
      Category = 'Zoom'
      Caption = 'Fit Page'
      ShortCut = 16432
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_actual_size: TTntAction
      Tag = 441
      Category = 'Zoom'
      Caption = 'Actual Size'
      ShortCut = 16433
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_fit_width: TTntAction
      Tag = 442
      Category = 'Zoom'
      Caption = 'Fit Width'
      ShortCut = 16434
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_fit_content: TTntAction
      Tag = 456
      Category = 'Zoom'
      Caption = 'Fit Content'
      ShortCut = 16435
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_custom: TTntAction
      Tag = 457
      Category = 'Zoom'
      Caption = 'Custom Zoom...'
      Visible = False
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_6400: TTntAction
      Tag = 443
      Category = 'Zoom'
      Caption = '6400%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_3200: TTntAction
      Tag = 444
      Category = 'Zoom'
      Caption = '3200%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_1600: TTntAction
      Tag = 445
      Category = 'Zoom'
      Caption = '1600%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_800: TTntAction
      Tag = 446
      Category = 'Zoom'
      Caption = '800%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_400: TTntAction
      Tag = 447
      Category = 'Zoom'
      Caption = '400%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_200: TTntAction
      Tag = 448
      Category = 'Zoom'
      Caption = '200%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_150: TTntAction
      Tag = 449
      Category = 'Zoom'
      Caption = '150%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_125: TTntAction
      Tag = 450
      Category = 'Zoom'
      Caption = '125%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_100: TTntAction
      Tag = 451
      Category = 'Zoom'
      Caption = '100%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_50: TTntAction
      Tag = 452
      Category = 'Zoom'
      Caption = '50%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_25: TTntAction
      Tag = 453
      Category = 'Zoom'
      Caption = '25%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_12_5: TTntAction
      Tag = 454
      Category = 'Zoom'
      Caption = '12.5%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object act_zoom_8_33: TTntAction
      Tag = 455
      Category = 'Zoom'
      Caption = '8.33%'
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
  end
end
