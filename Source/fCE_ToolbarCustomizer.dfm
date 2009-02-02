object CEToolbarCustomizer: TCEToolbarCustomizer
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Customize'
  ClientHeight = 293
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    259
    293)
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 4
    Top = 4
    Width = 35
    Height = 13
    Caption = 'Actions'
  end
  object ActionTree: TVirtualStringTree
    Left = 4
    Top = 20
    Width = 251
    Height = 269
    Anchors = [akLeft, akTop, akRight, akBottom]
    DragType = dtVCL
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
    OnDragAllowed = ActionTreeDragAllowed
    OnDragOver = ActionTreeDragOver
    OnDragDrop = ActionTreeDragDrop
    OnGetText = ActionTreeGetText
    OnPaintText = ActionTreePaintText
    OnGetImageIndexEx = ActionTreeGetImageIndexEx
    OnStartDrag = ActionTreeStartDrag
    Columns = <
      item
        Position = 0
        Width = 247
        WideText = 'Name'
      end>
  end
end
