unit fCE_OptionsPage_General;

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
  TCEOptionsPage_General = class(TCEOptionsCustomPage)
    check_singleinstance: TTntCheckBox;
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

implementation

{$R *.dfm}

uses
  Main;

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_General
-------------------------------------------------------------------------------}
constructor TCEOptionsPage_General.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('General');
  PageTitle:= _('General Settings');
  PagePath:= 'General';
  ImageIndex:= 0;
end;

{-------------------------------------------------------------------------------
  HandleChange
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.HandleChange(Sender: TObject);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.ApplySettings;
begin
  MainForm.SingleInstance:= check_singleinstance.Checked;
end;

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.LoadFromStorage(Storage: ICESettingsStorage);
begin
  // Toggles
  check_singleinstance.Checked:= Storage.ReadBoolean('/MainForm/SingleInstance', MainForm.SingleInstance);
end;

{-------------------------------------------------------------------------------
  Save To Storage
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_General.SaveToStorage(Storage: ICESettingsStorage);
begin
  // Toggles
  Storage.WriteBoolean('/MainForm/SingleInstance', check_singleinstance.Checked);
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCEOptionsPage_General);

finalization

end.
