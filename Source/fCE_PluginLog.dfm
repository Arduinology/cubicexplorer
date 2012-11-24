object CEPluginLog: TCEPluginLog
  Left = 0
  Top = 0
  Caption = 'Plugin Log'
  ClientHeight = 343
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object memo_log: TMemo
    Left = 0
    Top = 0
    Width = 532
    Height = 301
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitLeft = 76
    ExplicitTop = 32
    ExplicitWidth = 473
    ExplicitHeight = 305
  end
  object panel_bottom: TPanel
    Left = 0
    Top = 301
    Width = 532
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 428
    ExplicitWidth = 650
    DesignSize = (
      532
      42)
    object check_enable: TTntCheckBox
      Left = 12
      Top = 13
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Enable'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = check_enableClick
    end
    object but_close: TTntButton
      Left = 429
      Top = 6
      Width = 95
      Height = 28
      Anchors = [akRight, akBottom]
      Caption = 'Close'
      TabOrder = 1
      OnClick = but_closeClick
      ExplicitLeft = 547
      ExplicitTop = 5
    end
  end
end
