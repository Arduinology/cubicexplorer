inherited CEFolderPanel: TCEFolderPanel
  Caption = 'Folders'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited TopDock: TSpTBXDock
    ExplicitWidth = 425
  end
  inherited BottomDock: TSpTBXDock
    ExplicitWidth = 425
  end
end
