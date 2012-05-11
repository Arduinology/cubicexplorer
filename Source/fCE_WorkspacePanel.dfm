inherited CEWorkspacePanel: TCEWorkspacePanel
  Caption = 'Workspace'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited TopDock: TSpTBXDock
    Height = 27
    ExplicitHeight = 27
    object WorkspaceToolbar: TCEToolbar
      Left = 0
      Top = 0
      BorderStyle = bsNone
      DragHandleStyle = dhNone
      Images = CE_Images.SmallIcons
      Stretch = True
      TabOrder = 0
      Caption = 'Workspace'
      object item_path_combo: TTBControlItem
        Control = combo_path
      end
      object CEToolbarStretcherItem1: TCEToolbarStretcherItem
        CustomHeight = 16
        CustomWidth = 0
      end
      object combo_path: TVirtualExplorerCombobox
        Left = 0
        Top = 0
        Width = 421
        Options = [vcboThreadedImages, vcboThemeAware, vcboSelectPathOnDropDown]
        TabOrder = 0
        Path = 'combo_path'
        PopupExplorerOptions.ComboBoxStyle = cbsVETEnhanced
        PopupExplorerOptions.Indent = 16
        PopupExplorerOptions.Options = [poAnimated, poEnabled, poPersistentSizing, poSizeable, poRespectSysAnimationFlag, poThemeAware]
        TextType = ecbtFullPath
        OnPathChange = combo_pathPathChange
      end
    end
  end
  object BackgroundCMItems_up: TTntPopupMenu
    OnPopup = BackgroundCMItems_upPopup
    Left = 64
    Top = 104
    object View1: TTntMenuItem
      Caption = 'View'
      object item_large_icons: TTntMenuItem
        Tag = 100
        Caption = 'Large Icons'
        RadioItem = True
        OnClick = ContextMenuClick
      end
      object item_small_icons: TTntMenuItem
        Tag = 101
        Caption = 'Small Icons'
        RadioItem = True
        OnClick = ContextMenuClick
      end
      object item_list: TTntMenuItem
        Tag = 102
        Caption = 'List'
        RadioItem = True
        OnClick = ContextMenuClick
      end
      object item_details: TTntMenuItem
        Tag = 103
        Caption = 'Details'
        RadioItem = True
        OnClick = ContextMenuClick
      end
      object item_tiles: TTntMenuItem
        Tag = 105
        Caption = 'Tiles'
        RadioItem = True
        OnClick = ContextMenuClick
      end
      object item_thumbnails: TTntMenuItem
        Tag = 104
        Caption = 'Thumbnails'
        RadioItem = True
        OnClick = ContextMenuClick
      end
      object item_filmstrip: TTntMenuItem
        Tag = 107
        Caption = 'Filmstrip'
        RadioItem = True
        OnClick = ContextMenuClick
      end
      object N3: TTntMenuItem
        Caption = '-'
      end
      object item_per_folder: TTntMenuItem
        Tag = 110
        Caption = 'Use per folder settings'
        OnClick = ContextMenuClick
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
    object item_refresh: TTntMenuItem
      Tag = 606
      Caption = 'Refresh'
      OnClick = ContextMenuClick
    end
    object N2: TTntMenuItem
      Caption = '-'
    end
    object Paste1: TTntMenuItem
      Action = CEActions.act_edit_paste
    end
    object PasteShortcut1: TTntMenuItem
      Action = CEActions.act_edit_paste_shortcut
    end
    object CopyPath1: TTntMenuItem
      Action = CEActions.act_edit_copypath
    end
    object N4: TTntMenuItem
      Caption = '-'
    end
    object CreateSymbolicLink1: TTntMenuItem
      Action = CEActions.act_edit_create_symlink
    end
    object N5: TTntMenuItem
      Caption = '-'
    end
  end
end
