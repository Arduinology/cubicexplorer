object SamplePluginSettings_form: TSamplePluginSettings_form
  Left = 0
  Top = 0
  BorderStyle = bsNone
  ClientHeight = 240
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    320
    240)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = 'My Message'
  end
  object edit_msg: TEdit
    Left = 8
    Top = 27
    Width = 301
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'Hello World!'
    OnChange = edit_msgChange
    ExplicitWidth = 285
  end
end
