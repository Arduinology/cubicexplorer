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
