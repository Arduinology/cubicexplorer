unit fCE_PluginSettingsForm;

interface

uses
  // CC
  ccStrings,
  // CE
  CE_PluginsIntf, CE_LanguageEngine, CE_Plugins,
  // Tnt
  TntForms,
  // VSTools
  MPCommonUtilities,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, VirtualTrees;

type
{-------------------------------------------------------------------------------
  TCEPluginSettingsForm
-------------------------------------------------------------------------------}
  TCEPluginSettingsForm = class(TTntForm, ICEPluginSettingsEditorHost)
    panel_bottom: TPanel;
    panel_content: TPanel;
    but_apply: TTntButton;
    but_cancel: TTntButton;
    but_ok: TTntButton;
    tree_settings: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure but_applyClick(Sender: TObject);
    procedure but_cancelClick(Sender: TObject);
    procedure but_okClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tree_settingsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; var Allowed: Boolean);
    procedure tree_settingsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tree_settingsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tree_settingsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: WideString);
  private
  protected
    fEditor: ICEPluginSettingsEditor;
    fHost: ICEPluginHost;
    fSettings: ICEPluginSettings;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PopulateSettings; virtual;
    procedure ValueHasChanged(const AEditor: ICEPluginSettingsEditor); virtual;
        stdcall;
  public
    procedure Clear; virtual;
    procedure OpenSettings(const AHost: ICEPluginHost; const APluginID: TGUID);
        virtual;
  end;

{-------------------------------------------------------------------------------
  Public methods
-------------------------------------------------------------------------------}
function ShowPluginSettings(const AHost: ICEPluginHost; const APluginID: TGUID;
    AModalForm: Boolean = false; AOwner: TComponent = nil): TTntForm;

implementation

{$R *.dfm}

{##############################################################################}
// Public methods

{-------------------------------------------------------------------------------
  Show PluginSettings
-------------------------------------------------------------------------------}
function ShowPluginSettings(const AHost: ICEPluginHost; const APluginID: TGUID;
    AModalForm: Boolean = false; AOwner: TComponent = nil): TTntForm;
var
  form: TCEPluginSettingsForm;
begin
  if AOwner = nil then
  AOwner:= Application.MainForm;
  // create form
  form:= TCEPluginSettingsForm.Create(AOwner);
  // init
  form.OpenSettings(AHost, APluginID);
  // show
  if AModalForm then
  begin
    form.ShowModal;
    form.Release;
    Result:= nil;
  end
  else
  begin
    Result:= form;
    form.Show;
  end;
end;

{##############################################################################}
// TCEPluginSettingsForm

{-------------------------------------------------------------------------------
  On FormCreate
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.FormCreate(Sender: TObject);
begin
  tree_settings.NodeDataSize:= SizeOf(TCESettingItem);
end;

{-------------------------------------------------------------------------------
  On FormDestroy
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.FormDestroy(Sender: TObject);
begin
  Clear;
end;

{-------------------------------------------------------------------------------
  CreateParams
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent:= Application.MainFormHandle;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.Clear;
begin
  // release previous instances
  if assigned(fEditor) then
  begin
    fEditor.Finalize;
    fEditor:= nil;
  end;

  if assigned(fSettings) then
  fSettings:= nil;

  if assigned(fHost) then
  fHost:= nil;

  but_apply.Enabled:= false;
end;

{-------------------------------------------------------------------------------
  On FormClose
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.FormClose(Sender: TObject; var Action:
    TCloseAction);
begin
  Action:= caFree;
end;

{-------------------------------------------------------------------------------
  OpenSettings
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.OpenSettings(const AHost: ICEPluginHost; const
    APluginID: TGUID);
var
  data: TCEPluginData;
begin
  if not assigned(AHost) then
  Exit;

  Clear;

  fHost:= AHost;
  
  if AHost.GetPluginDataByID(APluginID, data) and assigned(data.Settings) then
  begin
    Caption:= data.Info.GetPluginName + ' - ' + _('Settings');
    fSettings:= data.Settings;

    // create settings editor
    fEditor:= data.Factory.CreateSettingsEditor(APluginID);
    if assigned(fEditor) then
    begin
      // initialize editor
      fEditor.Initialize(Self, APluginID);
      fEditor.SetParentWindow(panel_content.Handle);
      fEditor.SetBounds(panel_content.ClientRect);
      fEditor.LoadSettings(fSettings);
      tree_settings.Visible:= false;
    end
    // use default editor
    else
    begin
      tree_settings.Visible:= true;
      PopulateSettings;
    end;
    
    // release data
    data.Info:= nil;
    data.Factory:= nil;
    data.Settings:= nil;
    data.PluginPath:= '';
  end
  else
  Caption:= 'Invalid Plugin ID';
end;

{-------------------------------------------------------------------------------
  ValueHasChanged
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.ValueHasChanged(const AEditor:
    ICEPluginSettingsEditor);
begin
  if AEditor = fEditor then
  but_apply.Enabled:= true;
end;

{-------------------------------------------------------------------------------
  On FormCloseQuery
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.FormCloseQuery(Sender: TObject; var CanClose:
    Boolean);
var
  hr: Integer;
  ws,ws2: WideString;
begin
  if but_apply.Enabled then
  begin
    ws2:= _('Apply changes now?')+#13#10+
        #13+#10+
        _('If not, all changes will be lost.');
    ws:= _('Apply Changes');
    hr:= WideMessageBox(Self.Handle, ws, ws2 , MB_ICONQUESTION or MB_YESNOCANCEL);
    if hr = IDCANCEL then
    begin
      CanClose:= false;
      Exit;
    end
    else if hr = IDYES then
    but_apply.Click;
  end;

  CanClose:= true;
end;

{-------------------------------------------------------------------------------
  On but_ok.Click
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.but_okClick(Sender: TObject);
begin
  if but_apply.Enabled then                                                            
  but_apply.Click;

  Self.Close;
end;

{-------------------------------------------------------------------------------
  On but_cancel.Click
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.but_cancelClick(Sender: TObject);
begin
  but_apply.Enabled:= false;
  Self.Close;
end;

{-------------------------------------------------------------------------------
  On but_apply.Click
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.but_applyClick(Sender: TObject);
var
  node: PVirtualNode;
  data: PCESettingItem;
begin
  if not assigned(fSettings) then
  Exit;

  if assigned(fEditor) then
  fEditor.SaveSettings(fSettings)
  else
  begin
    node:= tree_settings.GetFirst;
    while assigned(node) do
    begin
      data:= tree_settings.GetNodeData(node);
      fSettings.WriteString(data.AName, data.AValue);
      node:= tree_settings.GetNextSibling(node);
    end;
  end;
  // write settings to disk
  if fHost.WriteSettingsToDisk(fSettings.GetPluginID, false) then
  fSettings.SendChangedNotify;

  but_apply.Enabled:= false;
end;

{-------------------------------------------------------------------------------
  PopulateSettings
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.PopulateSettings;
var
  i: Integer;
  data: PCESettingItem;
begin
  tree_settings.Clear;

  if not assigned(fSettings) then
  Exit;

  tree_settings.BeginUpdate;
  try
    for i:= 0 to fSettings.GetItemCount-1 do
    begin
      data:= tree_settings.GetNodeData(tree_settings.AddChild(nil));
      fSettings.GetItem(i, data^);
    end;
  finally
    tree_settings.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  On tree_settings.FreeNode
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.tree_settingsFreeNode(Sender: TBaseVirtualTree;
    Node: PVirtualNode);
var
  data: PSettingItem;
begin
  data:= Sender.GetNodeData(Node);
  data.AName:= '';
  data.AValue:= '';
end;

{-------------------------------------------------------------------------------
  On tree_settings.GetText
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.tree_settingsGetText(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var
    CellText: WideString);
var
  data: PSettingItem;
begin
  data:= Sender.GetNodeData(Node);
  case Column of
    0: CellText:= data.AName;
    1: CellText:= data.AValue;  
  end;
end;

{-------------------------------------------------------------------------------
  On tree_settings.NewText
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.tree_settingsNewText(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  data: PSettingItem;
begin
  if Column = 1 then
  begin
    data:= Sender.GetNodeData(Node);
    if not WideIsSameStr(data.AValue, NewText) then
    begin
      data.AValue:= NewText;
      but_apply.Enabled:= true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On tree_settings.Editing
-------------------------------------------------------------------------------}
procedure TCEPluginSettingsForm.tree_settingsEditing(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed:= Column = 1;
end;

end.
