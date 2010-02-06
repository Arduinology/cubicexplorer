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
    ExplicitWidth = 422
  end
  inherited BottomDock: TSpTBXDock
    Width = 422
    ExplicitWidth = 422
  end
  object DropStackPopup: TSpTBXPopupMenu
    OnPopup = DropStackPopupPopup
    Left = 360
    Top = 48
    object SpTBXSubmenuItem1: TSpTBXSubmenuItem
      Caption = 'View Style'
      object viewstyle_1: TSpTBXItem
        Caption = 'Large Icons'
        GroupIndex = 1
        OnClick = ViewStyleClick
      end
      object viewstyle_2: TSpTBXItem
        Tag = 1
        Caption = 'Small Icons'
        GroupIndex = 1
        OnClick = ViewStyleClick
      end
      object viewstyle_3: TSpTBXItem
        Tag = 2
        Caption = 'List'
        GroupIndex = 1
        OnClick = ViewStyleClick
      end
      object viewstyle_4: TSpTBXItem
        Tag = 3
        Caption = 'Details'
        GroupIndex = 1
        OnClick = ViewStyleClick
      end
      object viewstyle_5: TSpTBXItem
        Tag = 4
        Caption = 'Tiles'
        GroupIndex = 1
        OnClick = ViewStyleClick
      end
      object viewstyle_6: TSpTBXItem
        Tag = 5
        Caption = 'Thumbnails'
        GroupIndex = 1
        OnClick = ViewStyleClick
      end
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object but_clearlist: TSpTBXItem
      Caption = 'Clear List'
      OnClick = but_clearlistClick
    end
  end
end
