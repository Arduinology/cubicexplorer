object CECustomDockableForm: TCECustomDockableForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'CECustomDockableForm'
  ClientHeight = 296
  ClientWidth = 426
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object TopDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 426
    Height = 9
  end
  object BottomDock: TSpTBXDock
    Left = 0
    Top = 287
    Width = 426
    Height = 9
    Position = dpBottom
  end
end
