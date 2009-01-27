unit fCE_OptionsPage_Display;

interface

uses
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, CE_SettingsIntf, CE_LanguageEngine,
  // SpTBX
  SpTBXSkins,
  // Tnt
  TntStdCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCEOptionsPage_Display = class(TCEOptionsCustomPage)
    TntLabel1: TTntLabel;
    combo_theme: TComboBox;
    check_path_in_title: TTntCheckBox;
    procedure HandleChange(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure LoadFromStorage(Storage: ICESettingsStorage); override; stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); override; stdcall;
    { Public declarations }
  end;

var
  CEOptionsPage_Display: TCEOptionsPage_Display;

implementation

uses
  Main;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Display
-------------------------------------------------------------------------------}
constructor TCEOptionsPage_Display.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  PageName:= _('Display');
  PageTitle:= _('Display Settings');
  PagePath:= 'Display';
  ImageIndex:= 2;
  // Fill themes combo
  SkinManager.SkinsList.GetSkinNames(combo_theme.Items);
  SkinManager.SkinsList.Sort;
  I:= SkinManager.SkinsList.IndexOf('Default');
  if I > -1 then
  SkinManager.SkinsList.Move(I, 0);
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Display.ApplySettings;
begin
  // theme
  if SkinManager.CurrentSkinName <> combo_theme.Text then
  SkinManager.SetSkin(combo_theme.Text);
  MainForm.PathInTitle:= check_path_in_title.Checked;
end;

{-------------------------------------------------------------------------------
  Handle changes
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Display.HandleChange(Sender: TObject);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Display.LoadFromStorage(Storage: ICESettingsStorage);
var
  ws: WideString;
begin
  // Skin
  ws:= Storage.ReadString('/MainForm/Skin', SkinManager.CurrentSkinName);
  combo_theme.ItemIndex:= combo_theme.Items.IndexOf(ws);
  check_path_in_title.Checked:= Storage.ReadBoolean('/MainForm/PathInTitle', MainForm.PathInTitle);
end;

{-------------------------------------------------------------------------------
  Save To Storage
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Display.SaveToStorage(Storage: ICESettingsStorage);
begin
  // Skin
  Storage.WriteString('/MainForm/Skin', combo_theme.Items.Strings[combo_theme.ItemIndex]);
  Storage.WriteBoolean('/MainForm/PathInTitle', check_path_in_title.Checked);
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCEOptionsPage_Display);

finalization

end.
