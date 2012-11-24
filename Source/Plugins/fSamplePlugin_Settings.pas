unit fSamplePlugin_Settings;

interface

uses
  CE_PluginsIntf,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls;

type
  TSamplePluginSettings_form = class(TForm)
    Label1: TLabel;
    edit_msg: TEdit;
    procedure edit_msgChange(Sender: TObject);
  private
  protected
    fEditor: ICEPluginSettingsEditor;
    fHost: ICEPluginSettingsEditorHost;
    fUpdateCount: Integer;
    procedure DoChanged;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property Editor: ICEPluginSettingsEditor read fEditor write fEditor;
    property Host: ICEPluginSettingsEditorHost read fHost write fHost;
  end;

  TSamplePluginSettings = class(TInterfacedObject, ICEPluginSettingsEditor)
  protected
    fForm: TSamplePluginSettings_form;
  public
    // Finalize
    // - Gets called before the editor instance will be destroyed.
    // - AHost should be released here.
    procedure Finalize; virtual; stdcall;
    // Initialize
    // - Gets called right after the editor instance has been created.
    // - AHost is the host that manages editors. Editors should store the AHost
    //   refrence so that they can call it's ValueHasChanged when value has changed.
    // - APluginID is the PluginID whose settings are being edited.
    procedure Initialize(const AHost: ICEPluginSettingsEditorHost; const APluginID:
        TGUID); virtual; stdcall;
    // Load Settings
    // - Load values to editors.
    // - Gets called after Initialize -> SetParentWindow -> SetBounds
    procedure LoadSettings(const ASettings: ICEPluginSettings); virtual; stdcall;
    // - Save editor values to settings.
    // - Gets called when Apply or OK is pressed.
    // - It's recommended that editors save settings only in this method and
    //   not directly when editor value is changed.
    procedure SaveSettings(const ASettings: ICEPluginSettings); virtual; stdcall;
    // SetBounds
    // - Set window boundaries, relative to the parent window.
    // - Gets called after SetParentWindow and when the parent window is resized.
    procedure SetBounds(const ARect: TRect); virtual; stdcall;
    // SetParentWindow
    // - Set Parent Window.
    // - Gets called after Initialize.
    procedure SetParentWindow(AParentWindow: HWND); virtual; stdcall;
  end;

implementation

{$R *.dfm}

{##############################################################################}
// TSamplePluginSettings

{-------------------------------------------------------------------------------
  Finalize
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings.Finalize;
begin
  fForm.Editor:= nil;
  fForm.Host:= nil;
  fForm.Free;
end;

{-------------------------------------------------------------------------------
  Initialize
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings.Initialize(const AHost:
    ICEPluginSettingsEditorHost; const APluginID: TGUID);
begin
  fForm:= TSamplePluginSettings_form.Create(nil);
  fForm.Host:= AHost;
  fForm.Editor:= Self;
end;

{-------------------------------------------------------------------------------
  LoadSettings
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings.LoadSettings(const ASettings:
    ICEPluginSettings);
begin
  fForm.BeginUpdate;
  fForm.edit_msg.Text:= ASettings.ReadString('my_message', 'Empty');
  fForm.EndUpdate;
end;

{-------------------------------------------------------------------------------
  SaveSettings
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings.SaveSettings(const ASettings:
    ICEPluginSettings);
begin
  ASettings.WriteString('my_message', fForm.edit_msg.Text);
end;

{-------------------------------------------------------------------------------
  SetBounds
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings.SetBounds(const ARect: TRect);
begin
  fForm.BoundsRect:= ARect;
end;

{-------------------------------------------------------------------------------
  SetParentWindow
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings.SetParentWindow(AParentWindow: HWND);
begin
  fForm.ParentWindow:= AParentWindow;
  fForm.Show;
end;

{##############################################################################}
// TSamplePluginSettings_form

{-------------------------------------------------------------------------------
  BeginUpdate
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings_form.BeginUpdate;
begin
  fUpdateCount:= fUpdateCount + 1;
end;

{-------------------------------------------------------------------------------
  EndUpdate
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings_form.EndUpdate;
begin
  fUpdateCount:= fUpdateCount - 1;
  if fUpdateCount < 0 then
  fUpdateCount:= 0;
end;

{-------------------------------------------------------------------------------
  DoChanged
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings_form.DoChanged;
begin
  if assigned(Host) and assigned(Editor) then
  Host.ValueHasChanged(Editor);
end;

{-------------------------------------------------------------------------------
  On edit_msg.Change
-------------------------------------------------------------------------------}
procedure TSamplePluginSettings_form.edit_msgChange(Sender: TObject);
begin
  if fUpdateCount = 0 then
  DoChanged;
end;

end.
