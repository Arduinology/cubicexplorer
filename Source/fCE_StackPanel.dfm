inherited CEStackPanel: TCEStackPanel
  Caption = 'Stack'
  ClientWidth = 422
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 438
  ExplicitHeight = 330
  PixelsPerInch = 96
  TextHeight = 13
  inherited TopDock: TSpTBXDock
    Width = 422
    Height = 26
    ExplicitWidth = 422
    ExplicitHeight = 26
    object StackToolbar: TCEToolbar
      Left = 0
      Top = 0
      BorderStyle = bsNone
      DockMode = dmCannotFloatOrChangeDocks
      DragHandleStyle = dhNone
      Images = CE_Images.MiscImages
      Stretch = True
      TabOrder = 0
      object sub_load: TSpTBXSubmenuItem
        Caption = 'Open'
        ImageIndex = 4
        Options = [tboDropdownArrow]
        OnPopup = sub_loadPopup
      end
      object sub_save: TSpTBXSubmenuItem
        Caption = 'Save'
        ImageIndex = 5
        Options = [tboDropdownArrow]
        OnPopup = sub_savePopup
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object item_clear_list: TSpTBXItem
        Caption = 'Clear Filters'
        ImageIndex = 44
        Images = CE_Images.SmallIcons
        OnClick = item_clear_listClick
      end
      object item_safe_operations: TSpTBXItem
        Caption = 'Allow Move'
        ImageIndex = 7
        Images = CE_Images.MiscImages
        OnClick = item_safe_operationsClick
      end
    end
  end
  inherited BottomDock: TSpTBXDock
    Width = 422
    ExplicitWidth = 422
  end
  object DropStackPopup: TSpTBXPopupMenu
    OnPopup = DropStackPopupPopup
    Left = 368
    Top = 48
    object but_clearlist: TSpTBXItem
      Caption = 'Clear List'
      ImageIndex = 44
      Images = CE_Images.SmallIcons
      OnClick = item_clear_listClick
    end
  end
end
