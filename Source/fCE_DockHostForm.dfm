object CEDockHostForm: TCEDockHostForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'CEDockHostForm'
  ClientHeight = 320
  ClientWidth = 434
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object CenterPanel: TPanel
    Left = 0
    Top = 0
    Width = 434
    Height = 320
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PageHostPanel: TPanel
      Left = 0
      Top = 0
      Width = 434
      Height = 320
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object TopPageToolDock: TSpTBXDock
        Left = 0
        Top = 0
        Width = 434
        Height = 9
      end
      object BottomPageToolDock: TSpTBXDock
        Left = 0
        Top = 311
        Width = 434
        Height = 9
        Position = dpBottom
      end
      object LeftPageToolDock: TSpTBXDock
        Left = 0
        Top = 9
        Width = 9
        Height = 302
        Position = dpLeft
      end
      object RightPageToolDock: TSpTBXDock
        Left = 425
        Top = 9
        Width = 9
        Height = 302
        Position = dpRight
      end
    end
  end
end
