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
      Caption = 'StackToolbar'
      object sub_load: TSpTBXSubmenuItem
        Caption = 'Load'
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
    object but_safetyswitch: TSpTBXItem
      Caption = 'Safe Operations Only'
      OnClick = but_safetyswitchClick
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object but_clearlist: TSpTBXItem
      Caption = 'Clear List'
      OnClick = but_clearlistClick
    end
  end
end
