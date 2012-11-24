unit SamplePlugin;

interface

uses
  // CE
  CE_PluginsIntf, CE_Plugins,
  // System Units
  Classes, SysUtils;

const
  IID_SamplePlugin: TGUID = '{DF751E44-45A6-4AA9-B797-CB0EB0DF3AEA}';
  IID_SimplePlugin: TGUID = '{1FA55403-D0D0-42F9-9E7A-53A74BD36EDB}';

type
{-------------------------------------------------------------------------------
  TSamplePluginFactory
-------------------------------------------------------------------------------}
  TSamplePluginFactory = class(TCECustomPluginFactory)
  protected
    fSamplePluginInfo: TCEPluginInfo;
    fSimplePluginInfo: TCEPluginInfo;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // CreatePlugin
    // - Return new instance of ICEPlugin based on APluginID.
    // - If APluginID is not supported, should return nil.
    function CreatePlugin(const APluginID: TGUID): ICEPlugin; override; stdcall;
    // CreateSettingsEditor
    // - Return new instance of ICEPluginSettingsEditor based on APluginID.
    // - If APluginID is not supported, should return nil.
    // - This is an optional feature, factories are not required to provide
    //   SettingsEditors.
    function CreateSettingsEditor(const APluginID: TGUID): ICEPluginSettingsEditor;
        override; stdcall;
    // GetPluginCount
    // - Return the number of plugins supported.
    function GetPluginCount: Integer; override; stdcall;
    // GetPluginInfo
    // - Return an intance of ICEPluginInfo based on AIndex.
    // - AIndex range is from 0 (zero) to GetPluginCount-1.
    function GetPluginInfo(AIndex: Integer): ICEPluginInfo; override; stdcall;
    // InitializeSettings
    // - Get's called when factory is registered in plugin host.
    // - Factories can setup initial values here or do modifications if
    //   settings are from older version.
    procedure InitializeSettings(const APluginID: TGUID; const ASettings:
        ICEPluginSettings); override; stdcall;
  end;

{-------------------------------------------------------------------------------
  TSamplePlugin
-------------------------------------------------------------------------------}
  TSamplePlugin = class(TCECustomPlugin)
  public
    // Finalize
    // - Get's called once, just before the plugin is destroyed.
    // - Plugin should release AHost and ASettings here.
    procedure Finalize; override; stdcall;
    // GetPluginID
    // - Returns the PluginID.
    function GetPluginID: TGUID; override; stdcall;
    // Initialize
    // - Get's called once, right after the plugin is created.
    // - AHost is the PluginHost that created the Plugin.
    function Initialize(const AHost: ICEPluginHost): Boolean; override; stdcall;
    // LoadSettings
    // - Get's called after Initialize or when ever someone calls
    //   ICEPluginHost.LoadSettings with this plugin.
    // - ASettings holds settings for the plugin.
    procedure LoadSettings(const ASettings: ICEPluginSettings); override; stdcall;
    // Notify
    // - Get's called on PluginHost.SendNotify.
    // - ASender is the instance that called Host's SendNotify. Warning, can be NIL!
    // - ANotify is the notify event (PNOTIFY).
    // - AParam1 and AParam2 have different parameters depending on the Notify event.
    procedure Notify(const ASender: IInterface; ANotify: Integer; AParam1, AParam2:
        Integer); override; stdcall;
    // SaveSettings
    // - Get's called before Finalize or when ever someone calls
    //   ICEPluginHost.SaveSettings with this plugin.
    // - ASettings holds settings for the plugin.
    procedure SaveSettings(const ASettings: ICEPluginSettings); override; stdcall;
  end;

  TSimplePlugin = class(TCECustomPlugin)
  public
    // GetPluginID
    // - Returns the PluginID.
    function GetPluginID: TGUID; override; stdcall;
    // LoadSettings
    // - Get's called after Initialize or when ever someone calls
    //   ICEPluginHost.LoadSettings with this plugin.
    // - ASettings holds settings for the plugin.
    procedure LoadSettings(const ASettings: ICEPluginSettings); override; stdcall;
    // Notify
    // - Get's called on PluginHost.SendNotify.
    // - ASender is the instance that called Host's SendNotify. Warning, can be NIL!
    // - ANotify is the notify event (PNOTIFY).
    // - AParam1 and AParam2 have different parameters depending on the Notify event.
    procedure Notify(const ASender: IInterface; ANotify: Integer; AParam1, AParam2:
        Integer); override; stdcall;
    // SaveSettings
    // - Get's called before Finalize or when ever someone calls
    //   ICEPluginHost.SaveSettings with this plugin.
    // - ASettings holds settings for the plugin.
    procedure SaveSettings(const ASettings: ICEPluginSettings); override; stdcall;
  end;

implementation

uses
  fSamplePlugin_Settings;

{##############################################################################}
// TSamplePluginFactory

{-------------------------------------------------------------------------------
  Create an intance of TSamplePluginFactory
-------------------------------------------------------------------------------}
constructor TSamplePluginFactory.Create;
begin
  inherited Create;
  // sample plugin info                                           
  fSamplePluginInfo:= TCEPluginInfo.Create;
  fSamplePluginInfo.PluginID:= IID_SamplePlugin;
  fSamplePluginInfo.PluginType:= PTYPE_QuickViewViewer;
  fSamplePluginInfo.PluginName:= 'Sample Plugin';
  fSamplePluginInfo.PluginDescription:= 'Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry''s standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.' +
                                        'It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with ' +
                                        'desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.';
  fSamplePluginInfo.PluginVersion:= 1;
  fSamplePluginInfo.SupportedFormats:= 'txt';

  // simple plugin info
  fSimplePluginInfo:= TCEPluginInfo.Create;
  fSimplePluginInfo.PluginID:= IID_SimplePlugin;
  fSimplePluginInfo.PluginType:= PTYPE_Service;
  fSimplePluginInfo.PluginName:= 'Simple Plugin';
  fSimplePluginInfo.PluginDescription:= 'Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry''s standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book.' +
                                        'It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with ' +
                                        'desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.';
  fSimplePluginInfo.PluginVersion:= 123;
  fSimplePluginInfo.SupportedFormats:= 'txt';
end;

{-------------------------------------------------------------------------------
  Destroy TSamplePluginFactory
-------------------------------------------------------------------------------}
destructor TSamplePluginFactory.Destroy;
begin
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  CreatePlugin
-------------------------------------------------------------------------------}
function TSamplePluginFactory.CreatePlugin(const APluginID: TGUID): ICEPlugin;
begin
  if IsEqualGUID(APluginID, IID_SamplePlugin) then
  Result:= TSamplePlugin.Create
  else if IsEqualGUID(APluginID, IID_SimplePlugin) then
  Result:= TSimplePlugin.Create
  else
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  CreateSettingsEditor
-------------------------------------------------------------------------------}
function TSamplePluginFactory.CreateSettingsEditor(const APluginID: TGUID):
    ICEPluginSettingsEditor;
begin
  if IsEqualGUID(APluginID, IID_SamplePlugin) then
  Result:= TSamplePluginSettings.Create
  else
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  GetPluginCount
-------------------------------------------------------------------------------}
function TSamplePluginFactory.GetPluginCount: Integer;
begin
  Result:= 2;
end;

{-------------------------------------------------------------------------------
  GetPluginInfo
-------------------------------------------------------------------------------}
function TSamplePluginFactory.GetPluginInfo(AIndex: Integer): ICEPluginInfo;
begin
  case AIndex of
    0: Result:= fSamplePluginInfo;
    1: Result:= fSimplePluginInfo;
    else
    Result:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  Initialize Settings
-------------------------------------------------------------------------------}
procedure TSamplePluginFactory.InitializeSettings(const APluginID: TGUID; const
    ASettings: ICEPluginSettings);
begin
  // Simple Plugin
  if IsEqualGUID(APluginID, IID_SimplePlugin) then
  begin
    if ASettings.GetVersion < 1 then
    begin
      ASettings.WriteString('Name', 'Homer Simpson');
      ASettings.SetVersion(1);
      ASettings.SendChangedNotify;
    end;
  end;
end;

{##############################################################################}
// TSamplePlugin

{-------------------------------------------------------------------------------
  Initialize
-------------------------------------------------------------------------------}
function TSamplePlugin.Initialize(const AHost: ICEPluginHost): Boolean;
begin
  Result:= inherited Initialize(AHost);
end;

{-------------------------------------------------------------------------------
  Finalize
-------------------------------------------------------------------------------}
procedure TSamplePlugin.Finalize;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  GetPluginID
-------------------------------------------------------------------------------}
function TSamplePlugin.GetPluginID: TGUID;
begin
  Result:= IID_SamplePlugin;
end;


{-------------------------------------------------------------------------------
  LoadSettings
-------------------------------------------------------------------------------}
procedure TSamplePlugin.LoadSettings(const ASettings: ICEPluginSettings);
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  Notify
-------------------------------------------------------------------------------}
procedure TSamplePlugin.Notify(const ASender: IInterface; ANotify: Integer;
    AParam1, AParam2: Integer);
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  SaveSettings
-------------------------------------------------------------------------------}
procedure TSamplePlugin.SaveSettings(const ASettings: ICEPluginSettings);
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  GetPluginID
-------------------------------------------------------------------------------}
function TSimplePlugin.GetPluginID: TGUID;
begin
  Result:= IID_SimplePlugin;
end;

{##############################################################################}
// TSimplePlugin

{-------------------------------------------------------------------------------
  LoadSettings
-------------------------------------------------------------------------------}
procedure TSimplePlugin.LoadSettings(const ASettings: ICEPluginSettings);
begin
  //
end;

{-------------------------------------------------------------------------------
  Notify
-------------------------------------------------------------------------------}
procedure TSimplePlugin.Notify(const ASender: IInterface; ANotify: Integer;
    AParam1, AParam2: Integer);
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  SaveSettings
-------------------------------------------------------------------------------}
procedure TSimplePlugin.SaveSettings(const ASettings: ICEPluginSettings);
begin
  // override from descendant
end;

end.
