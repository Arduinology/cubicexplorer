unit fCE_OptionsPage_Display_FileView;

interface

uses
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, CE_SettingsIntf, CE_LanguageEngine,
  // Tnt
  TntStdCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCE_OptionsPage_Display_FileView = class(TCEOptionsCustomPage)
    check_fullrowselect: TTntCheckBox;
    check_selectprev: TTntCheckBox;
    check_autoselect: TTntCheckBox;
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
  CE_OptionsPage_Display_FileView: TCE_OptionsPage_Display_FileView;

implementation

uses
  fCE_FileView;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Display
-------------------------------------------------------------------------------}
constructor TCE_OptionsPage_Display_FileView.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('Fileview');
  PageTitle:= _('Fileview Settings');
  PagePath:= 'Display/Fileview';
  ImageIndex:= 5;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.ApplySettings;
begin
  GlobalFileViewSettings.FullRowSelect:= check_fullrowselect.Checked;
  GlobalFileViewSettings.SelectPreviousFolder:= check_selectprev.Checked;
  GlobalFileViewSettings.AutoSelectFirstItem:= check_autoselect.Checked;
end;

{-------------------------------------------------------------------------------
  HandleChange
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.HandleChange(Sender: TObject);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.LoadFromStorage(Storage:
    ICESettingsStorage);
begin
  Storage.OpenPath('/FileView');
  try
    // Toggles
    check_fullrowselect.Checked:= Storage.ReadBoolean('FullRowSelect', GlobalFileViewSettings.FullRowSelect);
    check_selectprev.Checked:= Storage.ReadBoolean('SelectPreviousFolder', GlobalFileViewSettings.SelectPreviousFolder);
    check_autoselect.Checked:= Storage.ReadBoolean('AutoSelectFirstItem', GlobalFileViewSettings.AutoSelectFirstItem);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Save To Storage
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.SaveToStorage(Storage:
    ICESettingsStorage);
begin
  Storage.OpenPath('/FileView');
  try
    // Toggles
    Storage.ReadBoolean('FullRowSelect', check_fullrowselect.Checked);
    Storage.ReadBoolean('SelectPreviousFolder', check_selectprev.Checked);
    Storage.ReadBoolean('AutoSelectFirstItem', check_autoselect.Checked);
  finally
    Storage.ClosePath;
  end;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCE_OptionsPage_Display_FileView);

finalization

end.
