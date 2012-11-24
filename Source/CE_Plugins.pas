unit CE_Plugins;

{$DEFINE LOG}
{$DEFINE LOG_SETTINGS}

interface

uses
  // CubicCore
  ccContainers, ccClasses, ccStrings, ccFileUtils, ccConsts, ccInterface, ccLog,
  ccPlugins,
  // CubicExplorer
  CE_PluginsIntf,
  // Fundamentals
  cTimers,
  // System Units
  Windows, Classes, SysUtils;

const
  IID_ICEPluginHostAccess: TGUID = '{3AA5E5F9-DE75-4E33-9826-AF934314CBF5}';

type
  TCEPluginFactoryLibraryHost = class;
  TCEPluginFactoryLibrary = class;

{-------------------------------------------------------------------------------
  ICEPluginHostAccess
-------------------------------------------------------------------------------}
  ICEPluginHostAccess = interface(IInterface)
  ['{3AA5E5F9-DE75-4E33-9826-AF934314CBF5}']
    // GetLogger
    // - Returns currently assigned log instance.
    // - Returns NIL if no log has been set.
    function GetLogger: ICCLog; stdcall;

    // SetLogger
    // - Assign log instance.
    // - Previous instance will be released.
    procedure SetLogger(ALog: ICCLog); stdcall;
  end;

{-------------------------------------------------------------------------------
  TCEPluginHost
-------------------------------------------------------------------------------}
  PPluginData = ^APluginData;
  APluginData = record
    fSettingsFileTick: Cardinal; // tick taken when settings were loaded from or saved to file. Initial value is 0.
    Info: ICEPluginInfo;
    Factory: ICEPluginFactory;
    Settings: ICEPluginSettings;
    fLibrary: TCEPluginFactoryLibrary;
  end;

  TCEPluginHost = class(TCCThInterfacedObject, ICEPluginHost, ICEPluginHostAccess)
  protected
    fDefaults: TCCList;
    fPlugins: TCCList;
    fInstances: TInterfaceList;
    fLogger: ICCLog;
    fLibraries: TCEPluginFactoryLibraryHost;
    fSettingsFolder: WideString;
    procedure AddPluginItem(const AFactory: ICEPluginFactory; const AInfo:
        ICEPluginInfo; ALibrary: TCEPluginFactoryLibrary = nil); virtual;
    function FindDefaultItemByFormat(APluginType: Integer; AFormat: WideString):
        PCCListItem; virtual;
    function FindPluginItem(APluginType: Integer; AFormat: WideString):
        PCCListItem; virtual;
    function FindPluginItemByID(const APluginID: TGUID): PCCListItem; virtual;
    function FindDefaultItemByID(const APluginID: TGUID): PCCListItem; virtual;
    function FindPluginItemByLibrary(ALibrary: TCEPluginFactoryLibrary):
        PCCListItem; virtual;
    function GetLogger: ICCLog; virtual; stdcall;
    procedure HandlePluginItemFree(ASender: TCCList; AItem: PCCListItem); virtual;
    procedure HandleDefaultItemFree(ASender: TCCList; AItem: PCCListItem); virtual;
    procedure InitSettings(AData: PPluginData; AForce: Boolean = false); virtual;
    procedure SetLogger(ALog: ICCLog); virtual; stdcall;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreatePlugin(const APluginID: TGUID): ICEPlugin; virtual; stdcall;
    procedure DestroyPlugin(var APlugin: ICEPlugin); virtual; stdcall;
    procedure Finalize; override;
    function FindPluginID(APluginType: Integer; const AFormat: WideString; out
        APluginID: TGUID): Boolean; virtual; stdcall;
    function GetDefaultItem(AIndex: Integer; var AOutput: TCEDefaultItem): Boolean;
        virtual; stdcall;
    function GetDefaultItemCount: Integer; virtual; stdcall;
    function GetPluginData(AIndex: Integer; var AOutput: TCEPluginData): Boolean;
        virtual; stdcall;
    function GetPluginDataByID(const APluginID: TGUID; var AOutput: TCEPluginData):
        Boolean; virtual; stdcall;
    function GetPluginDataCount: Integer; virtual; stdcall;
    function GetPluginSettings(const APluginID: TGUID): ICEPluginSettings; virtual;
        stdcall;
    function GetSettingsFolder: WideString; virtual; stdcall;
    function GetTick: Cardinal; virtual; stdcall;
    function LoadDefaults(const AFilePath: WideString): Boolean; virtual; stdcall;
    function LoadPlugins(const AFolderPath: WideString; const APluginExtension:
        WideString; AIncludeSubfolders: Boolean): Integer; virtual; stdcall;
    function ReadSettingsFromDisk(const APluginID: TGUID; Force: Boolean; Append:
        Boolean): Boolean; virtual; stdcall;
    procedure LoadSettings(const APlugin: ICEPlugin; Append: Boolean); virtual;
        stdcall;
    function Log(const AMessage: WideString; AMsgType: Integer; const ASender:
        IInterface): Int64; virtual; stdcall;
    procedure RegisterFactory(const AFactory: ICEPluginFactory); virtual; stdcall;
    function SaveDefaults(const AFilePath: WideString): Boolean; virtual; stdcall;
    function WriteSettingsToDisk(const APluginID: TGUID; Force: Boolean): Boolean;
        virtual; stdcall;
    procedure SaveSettings(const APlugin: ICEPlugin); virtual; stdcall;
    procedure SendNotify(const ASender: IInterface; const APluginID: TGUID;
        ANotify: Integer; AParam1, AParam2: Integer); virtual; stdcall;
    function SetAsDefault(const APluginID: TGUID): Boolean; virtual; stdcall;
    function SetAsDefaultFor(const APluginID: TGUID; APluginType: Integer; const
        AFormats: WideString): Boolean; virtual; stdcall;
    procedure SetSettingsFolder(const APath: WideString); virtual; stdcall;
    procedure UnRegisterFactory(const AFactory: ICEPluginFactory); virtual; stdcall; 
    property Logger: ICCLog read fLogger write fLogger;
    property SettingsFolder: WideString read GetSettingsFolder write
        SetSettingsFolder;
  end;

{-------------------------------------------------------------------------------
  TCEPluginInfo
-------------------------------------------------------------------------------}
  TCEPluginInfo = class(TInterfacedObject, ICEPluginInfo)
  protected
    fPluginDescription: WideString;
    fPluginID: TGUID;
    fPluginName: WideString;
    fPluginType: Integer;
    fPluginVersion: Integer;
    fSupportedFormats: WideString;
    function GetPluginDescription: WideString; virtual; stdcall;
    function GetPluginID: TGUID; virtual; stdcall;
    function GetPluginName: WideString; virtual; stdcall;
    function GetPluginType: Integer; virtual; stdcall;
    function GetPluginVersion: Integer; virtual; stdcall;
    function GetSupportedFormats: WideString; virtual; stdcall;
  public
    property PluginDescription: WideString read fPluginDescription write
        fPluginDescription;
    property PluginID: TGUID read fPluginID write fPluginID;
    property PluginName: WideString read fPluginName write fPluginName;
    property PluginType: Integer read fPluginType write fPluginType;
    property PluginVersion: Integer read fPluginVersion write fPluginVersion;
    property SupportedFormats: WideString read fSupportedFormats write
        fSupportedFormats;
  end;

{-------------------------------------------------------------------------------
  TCEPluginSettings
-------------------------------------------------------------------------------}
  PSettingItem = ^ASettingItem;
  ASettingItem = record
    AName: WideString;
    AValue: WideString;
  end;

  TCEPluginSettings = class(TCCThInterfacedObject, ICEPluginSettings)
  protected
    fAlwaysSetLastChanged: Boolean;
    fPluginHost: TCEPluginHost;
    fPluginID: TGUID;
    fItems: TList;
    fLastChange: Cardinal;
    fUpdateCount: Integer;
    fVersion: Integer;
    function AllocItem: PSettingItem; virtual;
    function FindItemIndex(const AName: WideString; var AIndex: Integer): Boolean;
        virtual;
    procedure FreeItem(AItem: PSettingItem); virtual;
    function InternalGetItem(const AName: WideString; AutoCreate: Boolean = false):
        PSettingItem; virtual; stdcall;
    procedure SetLastChanged(AValue: Integer); virtual;
    procedure UpdateLastChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure Clear; virtual; stdcall;
    function DeleteItem(AIndex: Integer): Boolean; virtual; stdcall;
    function DeleteValue(const AName: WideString): Boolean; virtual; stdcall;
    procedure EndUpdate; virtual;
    procedure Finalize; override;
    function GetPluginID: TGUID; virtual; stdcall;
    function GetItem(AIndex: Integer; var AOutput: TCESettingItem): Boolean;
        virtual; stdcall;
    function GetItemCount: Integer; virtual; stdcall;
    function GetLastChanged: Cardinal; virtual; stdcall;
    function GetSettingsFile: WideString; virtual; stdcall;
    function GetSettingsFolder: WideString; virtual; stdcall;
    function GetVersion: Integer; virtual; stdcall;
    function LoadFromFile(const AFilePath: WideString; Append: Boolean): Boolean;
        virtual; stdcall;
    function ReadBoolean(const AName: WideString; const ADefault: Boolean):
        Boolean; virtual; stdcall;
    function ReadInteger(const AName: WideString; const ADefault: Integer):
        Integer; virtual; stdcall;
    function ReadString(const AName: WideString; const ADefault: WideString):
        WideString; virtual; stdcall;
    function RenameValue(const AOldName: WideString; const ANewName: WideString):
        Boolean; virtual; stdcall;
    function SaveToFile(const AFilePath: WideString): Boolean; virtual; stdcall;
    procedure SendChangedNotify; virtual; stdcall;
    function SetItem(AIndex: Integer; const AItem: TCESettingItem): Integer;
        virtual; stdcall;
    procedure SetVersion(AVersion: Integer); virtual; stdcall;
    function SupportsFormat(const AName: WideString; AFormat: TCESettingFormat):
        Boolean; virtual; stdcall;
    function ValueExists(const AName: WideString): Boolean; virtual; stdcall;
    procedure WriteBoolean(const AName: WideString; const AValue: Boolean);
        virtual; stdcall;
    procedure WriteInteger(const AName: WideString; const AValue: Integer);
        virtual; stdcall;
    procedure WriteString(const AName: WideString; const AValue: WideString);
        virtual; stdcall; 
    property AlwaysSetLastChanged: Boolean read fAlwaysSetLastChanged write
        fAlwaysSetLastChanged;
    property PluginHost: TCEPluginHost read fPluginHost write fPluginHost;
    property PluginID: TGUID read fPluginID write fPluginID;
    property Version: Integer read fVersion write fVersion;
  end;

{-------------------------------------------------------------------------------
  TCEPluginFactoryLibraryHost
-------------------------------------------------------------------------------}
  TCEPluginFactoryLibraryHost = class(TCCPluginHost)
  protected
    function GetPluginClass: TCCPluginClass; override;
  end;

{-------------------------------------------------------------------------------
  TCEPluginFactoryLibrary
-------------------------------------------------------------------------------}
  TCEPluginFactoryLibrary = class(TCCPlugin)
  protected
    _CreatePluginFactory: TCECreatePluginFactory;
    function InitializePlugin(AHost: TCCPluginHost): Boolean; override;
  public
    constructor Create; override;
    function CreatePluginFactory: ICEPluginFactory; virtual; stdcall;
  end;

{-------------------------------------------------------------------------------
  TCECustomPluginFactory
-------------------------------------------------------------------------------}
  TCECustomPluginFactory = class(TInterfacedObject, ICEPluginFactory)
  public
    // CreatePlugin
    // - Return new instance of ICEPlugin based on APluginID.
    // - If APluginID is not supported, should return nil.
    function CreatePlugin(const APluginID: TGUID): ICEPlugin; virtual; stdcall;
    // CreateSettingsEditor
    // - Return new instance of ICEPluginSettingsEditor based on APluginID.
    // - If APluginID is not supported, should return nil.
    // - This is an optional feature, factories are not required to provide
    //   SettingsEditors.
    function CreateSettingsEditor(const APluginID: TGUID): ICEPluginSettingsEditor;
        virtual; stdcall;
    // GetPluginCount
    // - Return the number of plugins supported.
    function GetPluginCount: Integer; virtual; stdcall;
    // GetPluginInfo
    // - Return an intance of ICEPluginInfo based on AIndex.
    // - AIndex range is from 0 (zero) to GetPluginCount-1.
    function GetPluginInfo(AIndex: Integer): ICEPluginInfo; virtual; stdcall;
    // InitializeSettings
    // - Get's called when factory is registered in plugin host.
    // - Factories can setup initial values here or do modifications if
    //   settings are from older version.
    procedure InitializeSettings(const APluginID: TGUID; const ASettings:
        ICEPluginSettings); virtual; stdcall;
  end;

{-------------------------------------------------------------------------------
  TCECustomPlugin
-------------------------------------------------------------------------------}
  TCECustomPlugin = class(TInterfacedObject, ICEPlugin)
  protected
    fHost: ICEPluginHost;
  public
    // Finalize
    // - Get's called once, just before the plugin is destroyed.
    // - Plugin should release AHost and ASettings here.
    procedure Finalize; virtual; stdcall;
    // GetPluginID
    // - Returns the PluginID.
    function GetPluginID: TGUID; virtual; stdcall;
    // Initialize
    // - Get's called once, right after the plugin is created.
    // - AHost is the PluginHost that created the Plugin.
    function Initialize(const AHost: ICEPluginHost): Boolean; virtual; stdcall;
    // LoadSettings
    // - Get's called after Initialize or when ever someone calls
    //   ICEPluginHost.LoadSettings with this plugin.
    // - ASettings holds settings for the plugin.
    procedure LoadSettings(const ASettings: ICEPluginSettings); virtual; stdcall;
    // Notify
    // - Get's called on PluginHost.SendNotify.
    // - ASender is the instance that called Host's SendNotify. Warning, can be NIL!
    // - ANotify is the notify event (PNOTIFY).
    // - AParam1 and AParam2 have different parameters depending on the Notify event.
    procedure Notify(const ASender: IInterface; ANotify: Integer; AParam1, AParam2:
        Integer); virtual; stdcall;
    // SaveSettings
    // - Get's called before Finalize or when ever someone calls
    //   ICEPluginHost.SaveSettings with this plugin.
    // - ASettings holds settings for the plugin.
    procedure SaveSettings(const ASettings: ICEPluginSettings); virtual; stdcall;
    property Host: ICEPluginHost read fHost;
  end;

{-------------------------------------------------------------------------------
  Public methods
-------------------------------------------------------------------------------}
  function GlobalPluginHost: ICEPluginHost;
  function PluginTypeToStr(APluginType: Integer): WideString;

implementation

const
  BoolTrueStr: WideString = 'True';
  BoolFalseStr: WideString = 'False';
  BoolTrueIntStr: WideString = '1';
  BoolFalseIntStr: WideString = '0';
  BoolTrueInt: Integer = 1;
  BoolFalseInt: Integer = 0;
  VersionPrefix = WideChar('#');

{##############################################################################}
// Public methods

{-------------------------------------------------------------------------------
  GlobalPluginHost (not thread safe!!!)
-------------------------------------------------------------------------------}
var
  fGlobalPluginHost: ICEPluginHost;

function GlobalPluginHost: ICEPluginHost;
begin
  if not assigned(fGlobalPluginHost) then
  begin
    fGlobalPluginHost:= TCEPluginHost.Create;
  end;
  Result:= fGlobalPluginHost;
end;

{-------------------------------------------------------------------------------
  PluginTypeToStr
-------------------------------------------------------------------------------}
function PluginTypeToStr(APluginType: Integer): WideString;
begin
  case APluginType of
    PTYPE_Unknown: Result:= 'Unknown';
    PTYPE_Service: Result:= 'Service';
    PTYPE_Button: Result:= 'Button';
    PTYPE_QuickViewViewer: Result:= 'QuickView Viewer';
    PTYPE_Tab: Result:= 'Tab';
    PTYPE_Panel: Result:= 'Panel';
    else
    Result:= 'Unknown Type (' + IntToStr(APluginType) + ')';
  end;
end;

{##############################################################################}
// TCEPluginHost

{-------------------------------------------------------------------------------
  Create an instance of TCEPluginHost
-------------------------------------------------------------------------------}
constructor TCEPluginHost.Create;
begin
  inherited Create;
  // create instances
  fPlugins:= TCCList.Create;
  fPlugins.ItemDataSize:= SizeOf(APluginData);
  fPlugins.OnFreeItem:= HandlePluginItemFree;

  fDefaults:= TCCList.Create;
  fDefaults.ItemDataSize:= SizeOf(TCEDefaultItem);
  fDefaults.OnFreeItem:= HandleDefaultItemFree;

  fInstances:= TInterfaceList.Create;

  fLibraries:= TCEPluginFactoryLibraryHost.Create;

  // initialize values
  fSettingsFolder:= AppDirPath + 'Plugins\.settings\';
end;

{-------------------------------------------------------------------------------
  Destroy TCEPluginHost
-------------------------------------------------------------------------------}
destructor TCEPluginHost.Destroy;
begin
  if IsAlive then
  Finalize;

  // release instances
  if assigned(fLogger) then
  fLogger:= nil;

  // free instances
  FreeAndNil(fInstances);
  FreeAndNil(fPlugins);
  FreeAndNil(fDefaults);
  FreeAndNil(fLibraries);

  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  AddPluginItem
-------------------------------------------------------------------------------}
procedure TCEPluginHost.AddPluginItem(const AFactory: ICEPluginFactory; const
    AInfo: ICEPluginInfo; ALibrary: TCEPluginFactoryLibrary = nil);
var
  s: String;
  data: PPluginData;
  settings: TCEPluginSettings;
begin
  if assigned(AInfo) then
  begin
    {$IFDEF LOG}Log('Host.AddPluginItem: ' + GuidToString(AInfo.GetPluginID) +
                    ' | type: ' + IntToStr(AInfo.GetPluginType) +
                    ' | formats: ' + AInfo.GetSupportedFormats +
                    ' | name: ' + AInfo.GetPluginName +
                    ' | desc: ' + AInfo.GetPluginDescription,
                    0, Self);{$ENDIF}                               

    // add plugin item
    data:= @fPlugins.AddItem.Data;
    data.fSettingsFileTick:= 0;
    data.Info:= AInfo;
    data.Factory:= AFactory;
    data.fLibrary:= ALibrary;

    // create settings
    settings:= TCEPluginSettings.Create;
    settings.PluginHost:= Self;
    settings.PluginID:= AInfo.GetPluginID;
    data.Settings:= settings;
  end;
end;

{-------------------------------------------------------------------------------
  Handle PluginItem Free
-------------------------------------------------------------------------------}
procedure TCEPluginHost.HandlePluginItemFree(ASender: TCCList; AItem:
    PCCListItem);
var
  data: PPluginData;
begin
  data:= @AItem.Data;
  // write settings to file if needed
  if data.fSettingsFileTick < data.Settings.GetLastChanged then
  data.Settings.SaveToFile(data.Settings.GetSettingsFile);
  // release instances
  data.Info:= nil;
  data.Factory:= nil;
  data.Settings:= nil;
end;

{-------------------------------------------------------------------------------
  Find PluginItem
-------------------------------------------------------------------------------}
function TCEPluginHost.FindPluginItem(APluginType: Integer; AFormat:
    WideString): PCCListItem;
var
  list: TCCStringList;
  data: PPluginData;
  i: Integer;
begin
  Result:= nil;

  // TODO: optimize
  list:= TCCStringList.Create;
  list.Delimiter:= ';';
  try
    Result:= fPlugins.GetFirst;
    while assigned(Result) do
    begin
      data:= @Result.Data;
      if data.Info.GetPluginType = APluginType then
      begin
        list.DelimitedText:= data.Info.GetSupportedFormats;
        for i:= 0 to list.Count - 1 do
        begin
          if (list.Strings[i] = '*') or WideIsSameText(list.Strings[i], AFormat) then
          Exit; // match found, exit -->
        end;
      end;
      Result:= Result.Next;
    end;
    Result:= nil;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Find PluginItem By ID
-------------------------------------------------------------------------------}
function TCEPluginHost.FindPluginItemByID(const APluginID: TGUID): PCCListItem;
var
  data: PPluginData;
begin
  Result:= fPlugins.GetFirst;
  while assigned(Result) do
  begin
    data:= @Result.Data;
    if IsEqualGUID(APluginID, data.Info.GetPluginID) then
    Exit; // match found, exit -->
    Result:= Result.Next;
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Find PluginItem By Library
-------------------------------------------------------------------------------}
function TCEPluginHost.FindPluginItemByLibrary(ALibrary:
    TCEPluginFactoryLibrary): PCCListItem;
var
  data: PPluginData;
begin
  if assigned(ALibrary) then
  begin
    Result:= fPlugins.GetFirst;
    while assigned(Result) do
    begin
      data:= @Result.Data;
      if data.fLibrary = ALibrary then
      Exit; // match found, exit -->
      Result:= Result.Next;
    end;
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Handle DefaultItem Free
-------------------------------------------------------------------------------}
procedure TCEPluginHost.HandleDefaultItemFree(ASender: TCCList; AItem:
    PCCListItem);
var
  data: PCEDefaultItem;
begin
  data:= @AItem.Data;
  // free data
  data.Formats:= '';
end;

{-------------------------------------------------------------------------------
  Find DefaultItem By Format
-------------------------------------------------------------------------------}
function TCEPluginHost.FindDefaultItemByFormat(APluginType: Integer; AFormat:
    WideString): PCCListItem;
var
  list: TCCStringList;
  data: PCEDefaultItem;
  i: Integer;
begin
  Result:= nil;

  if AFormat = '' then
  Exit;

  // remove dot from extension
  if (AFormat[1] = '.') then
  Delete(AFormat, 1, 1);

  // TODO: optimize
  list:= TCCStringList.Create;
  list.Delimiter:= ';';
  try
    Result:= fDefaults.GetFirst;
    while assigned(Result) do
    begin
      data:= @Result.Data;
      if data.PluginType = APluginType then
      begin
        list.DelimitedText:= data.Formats;
        for i:= 0 to list.Count - 1 do
        begin
          if WideIsSameText(list.Strings[i], AFormat) then
          Exit; // match found, exit -->
        end;
      end;
      Result:= Result.Next;
    end;
    Result:= nil;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Find DefaultItem By ID
-------------------------------------------------------------------------------}
function TCEPluginHost.FindDefaultItemByID(const APluginID: TGUID): PCCListItem;
var
  data: PCEDefaultItem;
begin
  Result:= fDefaults.GetFirst;
  while assigned(Result) do
  begin
    data:= @Result.Data;
    if IsEqualGUID(APluginID, data.PluginID) then
    Exit; // match found, exit -->
    Result:= Result.Next;
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Create Plugin (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.CreatePlugin(const APluginID: TGUID): ICEPlugin;
var
  item: PCCListItem;
  data: PPluginData;
begin
  Result:= nil;
  if IsAlive then
  begin
    Lock;
    try
      item:= FindPluginItemByID(APluginID);
      if assigned(item) then
      begin
        data:= @item.Data;
        
        InitSettings(data);

        {$IFDEF LOG}Log('Host.CreatePlugin: ' + GuidToString(data.Info.GetPluginID), 0, Self);{$ENDIF}

        // create plugin
        Result:= data.Factory.CreatePlugin(data.Info.GetPluginID);
        fInstances.Add(Result);

        // initialize plugin
        if assigned(Result) then
        Result.Initialize(Self);
      end;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Destroy Plugin (ICEPluginHost)
-------------------------------------------------------------------------------}
procedure TCEPluginHost.DestroyPlugin(var APlugin: ICEPlugin);
begin
  if IsAlive and assigned(APlugin) then
  begin
    Lock;
    try
      {$IFDEF LOG}Log('Host.DestroyPlugin: ' + GuidToString(APlugin.GetPluginID), 0, Self);{$ENDIF}

      // remove from instance list
      fInstances.Remove(APlugin);
      // finalize and release
      APlugin.Finalize;
      APlugin:= nil;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Finalize (ICCInterface)
-------------------------------------------------------------------------------}
procedure TCEPluginHost.Finalize;
begin
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    fInstances.Clear;
    fPlugins.Clear;
    fDefaults.Clear;
    fLibraries.UnloadAll;
  finally
    fIsAlive:= false;
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  FindPluginID (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.FindPluginID(APluginType: Integer; const AFormat:
    WideString; out APluginID: TGUID): Boolean;
var
  item: PCCListItem;
  defaultData: PCEDefaultItem;
  pluginData: PPluginData;
begin
  Result:= false;
  if IsAlive then
  begin
    Lock;
    try
      // try to find default plugin
      item:= FindDefaultItemByFormat(APluginType, AFormat);
      if not assigned(item) then
      item:= FindDefaultItemByFormat(APluginType, '*');
      if assigned(item) then
      begin
        defaultData:= @item.Data;
        if FindPluginItemByID(defaultData.PluginID) <> nil then
        begin
          APluginID:= defaultData.PluginID;
          Result:= true;
        end;
      end;

      // find supporting plugin
      if not Result then
      begin
        item:= FindPluginItem(APluginType, AFormat);
        if assigned(item) then
        begin
          pluginData:= @item.Data;
          APluginID:= pluginData.Info.GetPluginID;
          Result:= true;
        end;
      end;
    finally
      if not Result then
      APluginID:= NULL_GUID;
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Default Item (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.GetDefaultItem(AIndex: Integer; var AOutput:
    TCEDefaultItem): Boolean;
var
  data: PCEDefaultItem;
begin
  Result:= false;
  if IsAlive then
  begin
    if (AIndex >= 0) and (AIndex < fDefaults.Count) then
    begin
      data:= @fDefaults.Item[AIndex].Data;
      AOutput.PluginID:= data.PluginID;
      AOutput.PluginType:= data.PluginType;
      AOutput.Formats:= data.Formats;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Default Item Count (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.GetDefaultItemCount: Integer;
begin
  if IsAlive then
  Result:= fDefaults.Count
  else
  Result:= -1;
end;

{-------------------------------------------------------------------------------
  GetLogger (ICEPluginHostAccess)
-------------------------------------------------------------------------------}
function TCEPluginHost.GetLogger: ICCLog;
begin
  Result:= fLogger;
end;

{-------------------------------------------------------------------------------
  GetPluginData (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.GetPluginData(AIndex: Integer; var AOutput:
    TCEPluginData): Boolean;
var
  data: PPluginData;
begin
  if (AIndex >= 0) and (AIndex < fPlugins.Count) then
  begin
    data:= @fPlugins.Item[AIndex].Data;
    AOutput.Info:= data.Info;
    AOutput.Factory:= data.Factory;
    AOutput.Settings:= data.Settings;

    InitSettings(data);

    if assigned(data.fLibrary) then
    AOutput.PluginPath:= data.fLibrary.LibraryPath
    else
    AOutput.PluginPath:= '';
    Result:= True;
  end
  else
  Result:= False;
end;

{-------------------------------------------------------------------------------
  GetPluginDataByID (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.GetPluginDataByID(const APluginID: TGUID; var AOutput:
    TCEPluginData): Boolean;
var
  data: PPluginData;
  item: PCCListItem;
begin
  item:= FindPluginItemByID(APluginID);
  if assigned(item) then
  begin
    data:= @item.Data;
    AOutput.Info:= data.Info;
    AOutput.Factory:= data.Factory;
    AOutput.Settings:= data.Settings;

    InitSettings(data);
        
    if assigned(data.fLibrary) then
    AOutput.PluginPath:= data.fLibrary.LibraryPath
    else
    AOutput.PluginPath:= '';
    Result:= True;
  end
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  GetPluginDataCount (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.GetPluginDataCount: Integer;
begin
  Result:= fPlugins.Count;
end;

{-------------------------------------------------------------------------------
  GetPluginSettings (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.GetPluginSettings(const APluginID: TGUID):
    ICEPluginSettings;
var
  item: PCCListItem;
begin
  Result:= nil;
  if IsAlive then
  begin
    Lock;
    try
      item:= FindPluginItemByID(APluginID);
      if assigned(item) then
      Result:= PPluginData(@item.Data).Settings;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  GetSettingsFolder (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.GetSettingsFolder: WideString;
begin
  if IsAlive then
  begin
    Lock;
    try
      Result:= fSettingsFolder;
    finally
      UnLock;
    end;
  end
  else
  Result:= '';
end;

{-------------------------------------------------------------------------------
  Get Tick (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.GetTick: Cardinal;
begin
  Result:= GetTickCount;
end;

{-------------------------------------------------------------------------------
  InitSettings
-------------------------------------------------------------------------------}
procedure TCEPluginHost.InitSettings(AData: PPluginData; AForce: Boolean =
    false);
begin
  if assigned(AData) and ((AData.fSettingsFileTick = 0) or AForce) then
  begin
    AData.Settings.LoadFromFile(AData.Settings.GetSettingsFile, true);
    AData.fSettingsFileTick:= GetTick;
    AData.Factory.InitializeSettings(AData.Info.GetPluginID, AData.Settings);
  end;
end;

{-------------------------------------------------------------------------------
  LoadDefaults (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.LoadDefaults(const AFilePath: WideString): Boolean;
var
  data: PCEDefaultItem;
  list: TCCStrings;
  i: Integer;
  g: TGUID;
  ws: WideString;
begin
  Result:= false;
  if IsAlive then
  begin
    {$IFDEF LOG}Log('Host.LoadDefaults: ' + AFilePath, 0, Self);{$ENDIF}

    // clear previous entries
    fDefaults.Clear;                                                        
    list:= TCCStringList.Create;
    try
      try
        list.LoadFromFile(AFilePath, [foUnicodeLB]);
        for i:= 0 to list.Count - 1 do
        begin
          try
            g:= StringToGUID(list.Names[i]);
            data:= @fDefaults.AddItem.Data;
            data.PluginID:= g;
            ws:= list.ValueFromIndex[i];
            data.PluginType:= StrToIntDef(WideGetNextItem(ws, ';'), 0);
            data.Formats:= ws;
          except
          end;
        end;
        Result:= true;
      except
        exit;
      end;
    finally
      list.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  LoadPlugins (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.LoadPlugins(const AFolderPath: WideString; const
    APluginExtension: WideString; AIncludeSubfolders: Boolean): Integer;
var
  i, j, c: Integer;
  lib: TCEPluginFactoryLibrary;
  factory: ICEPluginFactory;
  ext: WideString;
begin
  Result:= -1;
  if IsAlive then
  begin
    Lock;
    try
      if (Length(APluginExtension) > 0) and (APluginExtension[1] <> '.') then
      ext:= '.' + APluginExtension
      else
      ext:= APluginExtension;

      {$IFDEF LOG}Log('Host.LoadPlugins: ' + WideIncludeTrailingPathDelimiter(AFolderPath) + '*' + ext, 0, Self);{$ENDIF}


      // load libraries
      Result:= fLibraries.LoadPluginsFromFolder(AFolderPath, AIncludeSubfolders, ext);

      // enumerate plugins
      for i:= 0 to fLibraries.Count - 1 do
      begin
        lib:= TCEPluginFactoryLibrary(fLibraries.Plugins[i]);
        if FindPluginItemByLibrary(lib) = nil then
        begin
          // create factory
          factory:= lib.CreatePluginFactory;
          if assigned(factory) then
          begin
            // unregister factory just in case
            UnRegisterFactory(factory);
            
            // add plugins
            c:= factory.GetPluginCount-1;
            for j:= 0 to c do
            begin
              AddPluginItem(factory, factory.GetPluginInfo(j), lib);
            end;
          end;
        end;
      end;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  RegisterFactory (ICEPluginHost)
-------------------------------------------------------------------------------}
procedure TCEPluginHost.RegisterFactory(const AFactory: ICEPluginFactory);
var
  i, c: Integer;
begin
  if not IsAlive or not assigned(AFactory) then
  Exit;

  {$IFDEF LOG}Log('Host.RegisterFactory', 0, Self);{$ENDIF}

  Lock;
  try
    // unregister first
    UnRegisterFactory(AFactory);

    // add plugins
    c:= AFactory.GetPluginCount;
    for i:= 0 to c - 1 do
    begin
      AddPluginItem(AFactory, AFactory.GetPluginInfo(i));
    end;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Set Settings Path (ICEPluginHost)
-------------------------------------------------------------------------------}
procedure TCEPluginHost.SetSettingsFolder(const APath: WideString);
begin
  if IsAlive then
  begin
    {$IFDEF LOG}Log('Host.SetSettingsFolder: ' + APath, 0, Self);{$ENDIF}

    Lock;
    try
      fSettingsFolder:= WideIncludeTrailingPathDelimiter(APath);
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Read Settings From Disk (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.ReadSettingsFromDisk(const APluginID: TGUID; Force:
    Boolean; Append: Boolean): Boolean;
var
  item: PCCListItem;
  data: PPluginData;
begin
  Result:= false;
  if IsAlive then
  begin
    {$IFDEF LOG}Log('Host.ReadSettingsFromDisk: ' + GUIDToString(APluginID), 0, Self);{$ENDIF}

    Lock;
    try
      // read all plugins
      if IsEqualGUID(APluginID, NULL_GUID) then
      begin
        item:= fPlugins.GetFirst;
        while assigned(item) do
        begin
          InitSettings(@item.Data, Force);
          item:= item.Next;
        end;
      end
      // read only one plugin
      else
      begin
        item:= FindPluginItemByID(APluginID);
        if assigned(item) then
        InitSettings(@item.Data, Force);
      end;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Write Settings To Disk (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.WriteSettingsToDisk(const APluginID: TGUID; Force:
    Boolean): Boolean;

  // write to disk
  procedure Write(PluginData: PPluginData);
  begin
    if Force or (PluginData.fSettingsFileTick < PluginData.Settings.GetLastChanged) then
    begin
      PluginData.Settings.SaveToFile(PluginData.Settings.GetSettingsFile);
      PluginData.fSettingsFileTick:= GetTick;
      Result:= true;
    end;
  end;

var
  item: PCCListItem;
  data: PPluginData;
begin
  Result:= false;
  if IsAlive then
  begin
    {$IFDEF LOG}Log('Host.WriteSettingsToDisk: ' + GUIDToString(APluginID), 0, Self);{$ENDIF}

    Lock;
    try
      // read all plugins
      if IsEqualGUID(APluginID, NULL_GUID) then
      begin
        item:= fPlugins.GetFirst;
        while assigned(item) do
        begin
          Write(@item.Data);
          item:= item.Next;
        end;
      end
      // read only one plugin
      else
      begin
        item:= FindPluginItemByID(APluginID);
        if assigned(item) then
        Write(@item.Data);
      end;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Load Settings (ICEPluginHost)
-------------------------------------------------------------------------------}
procedure TCEPluginHost.LoadSettings(const APlugin: ICEPlugin; Append: Boolean);
var
  item: PCCListItem;
  data: PPluginData;
begin
  if IsAlive and assigned(APlugin) then
  begin
    {$IFDEF LOG}Log('Host.LoadSettings: ' + GUIDToString(APlugin.GetPluginID), 0, Self);{$ENDIF}

    Lock;
    try
      item:= fPlugins.GetFirst;
      while assigned(item) do
      begin
        data:= @item.Data;
        if IsEqualGUID(data.Info.GetPluginID, APlugin.GetPluginID) then
        begin
          InitSettings(data);

          // load settings
          APlugin.LoadSettings(data.Settings);

          break;
        end;
        item:= item.Next;
      end;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Log (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.Log(const AMessage: WideString; AMsgType: Integer; const
    ASender: IInterface): Int64;
begin
  if AMsgType = MTYPE_TICK then
  begin
    StartTimer(Result);
    Exit;
  end;

  if IsAlive then
  begin
    if assigned(fLogger) then
    fLogger.Log(AMessage, AMsgType, Pointer(ASender));
    
    StartTimer(Result);
  end
  else
  Result:= -1;
end;

{-------------------------------------------------------------------------------
  SaveDefaults (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.SaveDefaults(const AFilePath: WideString): Boolean;
var
  item: PCCListItem;
  data: PCEDefaultItem;
  list: TCCStrings;
begin
  Result:= false;
  if IsAlive then
  begin
    {$IFDEF LOG}Log('Host.SaveDefaults: ' + AFilePath, 0, Self);{$ENDIF}

    Lock;
    list:= TCCStringList.Create;
    try
      item:= fDefaults.GetFirst;
      while assigned(item) do
      begin
        data:= @item.Data;
        list.Add(GUIDToString(data.PluginID) + '=' + IntToStr(data.PluginType) + ';' + data.Formats);
        item:= item.Next;
      end;

      try
        list.SaveToFile(AFilePath, [foUnicodeLB]);
        Result:= true;
      except
      end;
    finally
      list.Free;
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save Settings (ICEPluginHost)
-------------------------------------------------------------------------------}
procedure TCEPluginHost.SaveSettings(const APlugin: ICEPlugin);
var
  item: PCCListItem;
  data: PPluginData;
begin
  if IsAlive and assigned(APlugin) then
  begin
    {$IFDEF LOG}Log('Host.SaveSettings: ' + GUIDToString(APlugin.GetPluginID), 0, Self);{$ENDIF}

    Lock;
    try
      item:= fPlugins.GetFirst;
      while assigned(item) do
      begin
        data:= @item.Data;
        if IsEqualGUID(data.Info.GetPluginID, APlugin.GetPluginID) then
        begin
          // save settings
          APlugin.SaveSettings(data.Settings);

          // write to disk if needed
          if data.fSettingsFileTick < data.Settings.GetLastChanged then
          begin
            data.Settings.SaveToFile(data.Settings.GetSettingsFile);
            data.fSettingsFileTick:= GetTick;
          end;

          break;
        end;
        item:= item.Next;
      end;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Send Notify (ICEPluginHost)
-------------------------------------------------------------------------------}
procedure TCEPluginHost.SendNotify(const ASender: IInterface; const APluginID:
    TGUID; ANotify: Integer; AParam1, AParam2: Integer);
var
  i: Integer;
  setChanged: Boolean;
  settings: ICEPluginSettings;
  plugin: ICEPlugin;
begin
  {$IFDEF LOG}Log('Host.SendNotify: to ' + GUIDToString(APluginID) + ', notify=' + IntToStr(ANotify) +
                  ' param1=' + IntToStr(AParam1) + ' param2=' + IntToStr(AParam2), 0, Self);{$ENDIF}

  setChanged:= ANotify = PNOTIFY_SettingsChanged;

  // Send to all plugins
  if IsEqualGUID(APluginID, NULL_GUID) then
  begin
    for i:= 0 to fInstances.Count - 1 do
    begin
      plugin:= (fInstances.Items[i] as ICEPlugin);
      plugin.Notify(ASender, ANotify, AParam1, AParam2);
    end;
  end
  // Send to single plugin type
  else
  begin
    for i:= 0 to fInstances.Count - 1 do
    begin
      plugin:= (fInstances.Items[i] as ICEPlugin);
      if IsEqualGUID(APluginID, plugin.GetPluginID) then
      begin
        plugin.Notify(ASender, ANotify, AParam1, AParam2);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  UnRegisterFactory (ICEPluginHost)
-------------------------------------------------------------------------------}
procedure TCEPluginHost.UnRegisterFactory(const AFactory: ICEPluginFactory);
var
  item, tmp: PCCListItem;
  data: PPluginData;
begin
  if IsAlive then
  begin
    Lock;
    try
      item:= fPlugins.GetFirst;
      while assigned(item) do
      begin
        tmp:= item;
        item:= item.Next;

        data:= @tmp.Data;
        if data.Factory = AFactory then
        begin
          fPlugins.DeleteItem(tmp);
          {$IFDEF LOG}Log('Host.UnRegisterFactory', 0, Self);{$ENDIF}
        end;
      end;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set As Default (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.SetAsDefault(const APluginID: TGUID): Boolean;
var
  item: PCCListItem;
  pluginData: PPluginData;
  defaultData: PCEDefaultItem;
  formats, list: TCCStrings;
  found: Boolean;
  i, index: Integer;
begin
  Result:= false;
  if IsAlive then
  begin
    {$IFDEF LOG}Log('Host.SetAsDefault: ' + GuidToString(APluginID), 0, Self);{$ENDIF}
    
    Lock;
    try
      // get plugin data
      item:= FindPluginItemByID(APluginID);
      Result:= assigned(item);
      if Result then
      begin
        pluginData:= @item.Data;
    
        // TODO: optimize
    
        formats:= TCCStringList.Create;
        formats.Delimiter:= ';';
        try
          formats.DelimitedText:= pluginData.Info.GetSupportedFormats;

          // remove all existing extensions
          list:= TCCStringList.Create;
          list.Delimiter:= ';';
          try
            found:= false;
            item:= fDefaults.GetFirst;
            while assigned(item) do
            begin
              defaultData:= @item.Data;
              if pluginData.Info.GetPluginType = defaultData.PluginType then
              begin
                // add format(s) to existing item
                if IsEqualGUID(pluginData.Info.GetPluginID, defaultData.PluginID) then
                begin
                  defaultData.PluginType:= pluginData.Info.GetPluginType;
                  defaultData.Formats:= pluginData.Info.GetSupportedFormats;
                  found:= true;
                end
                // remove format(s) from other items
                else
                begin
                  list.DelimitedText:= defaultData.Formats;
                  list.Tag:= 0;
                  for i:= 0 to formats.Count - 1 do
                  begin
                    index:= list.IndexOf(formats.Strings[i]);
                    if index > -1 then
                    begin
                      list.Delete(index);
                      list.Tag:= 1;
                    end;
                  end;

                  if list.Tag = 1 then
                  defaultData.Formats:= list.DelimitedText;
                end;
              end;
              item:= item.Next;
            end;

            // add new default item
            if not found then
            begin
              defaultData:= @fDefaults.AddItem.Data;
              defaultData.PluginID:= pluginData.Info.GetPluginID;
              defaultData.PluginType:= pluginData.Info.GetPluginType;
              defaultData.Formats:= pluginData.Info.GetSupportedFormats;
            end;
          finally
            list.Free;
          end;

        finally
          formats.Free;
        end;
      end;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set As Default For (ICEPluginHost)
-------------------------------------------------------------------------------}
function TCEPluginHost.SetAsDefaultFor(const APluginID: TGUID; APluginType:
    Integer; const AFormats: WideString): Boolean;
var
  item: PCCListItem;
  defaultData: PCEDefaultItem;
  formats, list: TCCStrings;
  found: Boolean;
  i, index: Integer;
begin
  Result:= false;
  if IsAlive then
  begin
    {$IFDEF LOG}Log('Host.SetAsDefaultFor: ' + GuidToString(APluginID) + '=' + IntToStr(APluginType) +';'+ AFormats, 0, Self);{$ENDIF}

    Lock;
    try
      // TODO: optimize

      formats:= TCCStringList.Create;
      formats.Delimiter:= ';';
      try
        formats.DelimitedText:= AFormats;

        // remove all existing extensions
        list:= TCCStringList.Create;
        list.Delimiter:= ';';
        try
          found:= false;
          item:= fDefaults.GetFirst;
          while assigned(item) do
          begin
            defaultData:= @item.Data;
            list.DelimitedText:= defaultData.Formats;

            if defaultData.PluginType = APluginType then
            begin
              // add format(s) to existing item
              if IsEqualGUID(APluginID, defaultData.PluginID) then
              begin
                for i:= 0 to formats.Count - 1 do
                begin
                  index:= list.IndexOf(formats.Strings[i]);
                  if index = -1 then
                  list.Add(formats.Strings[i]);
                end;
                found:= true;
              end
              // remove format(s) from other items
              else
              begin
                list.Tag:= 0;
                for i:= 0 to formats.Count - 1 do
                begin
                  index:= list.IndexOf(formats.Strings[i]);
                  if index > -1 then
                  begin
                    list.Delete(index);
                    list.Tag:= 1;
                  end;
                end;
                if list.Tag = 1 then
                defaultData.Formats:= list.DelimitedText;
              end;
            end;
            item:= item.Next;
          end;

          // add new default item
          if not found then
          begin
            defaultData:= @fDefaults.AddItem.Data;
            defaultData.PluginID:= APluginID;
            defaultData.PluginType:= APluginType;
            defaultData.Formats:= AFormats;
          end;

          Result:= true;
        finally
          list.Free;
        end;

      finally
        formats.Free;
      end;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  SetLogger (ICEPluginHostAccess)
-------------------------------------------------------------------------------}
procedure TCEPluginHost.SetLogger(ALog: ICCLog);
begin
  fLogger:= ALog;
end;

{##############################################################################}
// TCEPluginInfo

{-------------------------------------------------------------------------------
  Get Plugin Description (ICEPluginInfo)
-------------------------------------------------------------------------------}
function TCEPluginInfo.GetPluginDescription: WideString;
begin
  Result:= fPluginDescription;
end;

{-------------------------------------------------------------------------------
  Get Plugin ID (ICEPluginInfo)
-------------------------------------------------------------------------------}
function TCEPluginInfo.GetPluginID: TGUID;
begin
  Result:= fPluginID;
end;

{-------------------------------------------------------------------------------
  Get Plugin Name (ICEPluginInfo)
-------------------------------------------------------------------------------}
function TCEPluginInfo.GetPluginName: WideString;
begin
  Result:= fPluginName;
end;

{-------------------------------------------------------------------------------
  Get Plugin Type (ICEPluginInfo)
-------------------------------------------------------------------------------}
function TCEPluginInfo.GetPluginType: Integer;
begin
  Result:= fPluginType;
end;

{-------------------------------------------------------------------------------
  Get Plugin Version (ICEPluginInfo)
-------------------------------------------------------------------------------}
function TCEPluginInfo.GetPluginVersion: Integer;
begin
  Result:= fPluginVersion;
end;

{-------------------------------------------------------------------------------
  Get Supported Formats (ICEPluginInfo)
-------------------------------------------------------------------------------}
function TCEPluginInfo.GetSupportedFormats: WideString;
begin
  Result:= fSupportedFormats;
end;

{##############################################################################}
// TCEPluginSettings

{-------------------------------------------------------------------------------
  Create an instance of TCEPluginSettings
-------------------------------------------------------------------------------}
constructor TCEPluginSettings.Create;
begin
  inherited Create;
  // init values
  fPluginID:= NULL_GUID;
  fAlwaysSetLastChanged:= false;
  // create instances
  fItems:= TList.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCEPluginSettings
-------------------------------------------------------------------------------}
destructor TCEPluginSettings.Destroy;
begin
  // finalize if needed
  if IsAlive then Finalize;
  
  // free instances
  fItems.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Finalize (ICCInterface)
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.Finalize;
begin
  // exit if we are dead
  if not IsAlive then Exit;

  Clear;
  inherited;
end;

{-------------------------------------------------------------------------------
  AllocItem
-------------------------------------------------------------------------------}
function TCEPluginSettings.AllocItem: PSettingItem;
begin
  Result:= AllocMem(SizeOf(ASettingItem));
end;

{-------------------------------------------------------------------------------
  Do FreeItem
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.FreeItem(AItem: PSettingItem);
begin
  try
    AItem.AName:= '';
    AItem.AValue:= '';
  finally
    FreeMem(AItem);
  end;
end;

{-------------------------------------------------------------------------------
  FindItemIndex (Binary search)
  - Find item by it's name.
  - If item is found, it's index is returned in AIndex.
  - If item doesn't exist, AIndex will contain the index which the item should have.
  - If found returns TRUE, else FALSE.
-------------------------------------------------------------------------------}
function TCEPluginSettings.FindItemIndex(const AName: WideString; var
    AIndex: Integer): Boolean;
var
  loI, hiI: Integer;
  i: Integer;
  item: PSettingItem;
begin
  Result:= false;
  AIndex:= 0;

  loI:= 0;
  hiI:= fItems.Count-1;

  while (loI <= hiI) and not Result do
  begin
    AIndex:= (loI + hiI) div 2;
    item:= fItems.Items[AIndex];

    // compare name
    i:= ccStrings.WideCompareText(item.AName, AName);

    if i < 0 then
    loI:= AIndex+1
    else if i > 0 then
    hiI:= AIndex-1
    else // found
    Result:= true
  end;

  if not Result and (i < 0) then
  AIndex:= AIndex + 1;
end;

{-------------------------------------------------------------------------------
  InternalGetItem
  - creates property item if it doesn't exist.
-------------------------------------------------------------------------------}
function TCEPluginSettings.InternalGetItem(const AName: WideString; AutoCreate:
    Boolean = false): PSettingItem;
var
  index: Integer;
begin
  if FindItemIndex(AName, index) then
  begin
    Result:= fItems.Items[index];
  end
  else if AutoCreate then
  begin
    // create new item
    Result:= AllocItem;
    fItems.Insert(index, Result);
    Result.AName:= AName;
  end
  else
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Set Last Changed
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.SetLastChanged(AValue: Integer);
begin
  fLastChange:= AValue;
end;

{-------------------------------------------------------------------------------
  UpdateLastChanged
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.UpdateLastChanged;
begin
  if assigned(fPluginHost) then
  fLastChange:= fPluginHost.GetTick
  else
  fLastChange:= GetTickCount;
end;

{-------------------------------------------------------------------------------
  Begin Update
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.BeginUpdate;
begin
  Lock;
  try
    fUpdateCount:= fUpdateCount + 1;    
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  End Update
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.EndUpdate;
begin
  Lock;
  try
    if fUpdateCount > 0 then
    begin
      fUpdateCount:= fUpdateCount + 1;
      if fUpdateCount = 0 then
      UpdateLastChanged;
    end;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Get PluginID (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.GetPluginID: TGUID;
begin
  Result:= fPluginID;
end;

{-------------------------------------------------------------------------------
  Delete Item (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.DeleteItem(AIndex: Integer): Boolean;
begin
  Result:= false;
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    if (AIndex >= 0) and (AIndex < fItems.Count) then
    begin
      FreeItem(fItems.Items[AIndex]);
      fItems.Delete(AIndex);
      Result:= true;
    end;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Clear (ICEPluginSettings)
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.Clear;
var
  i: Integer;
begin
  // exit if we are dead
  if not IsAlive then Exit;
  
  Lock;
  try
    for i:= 0 to fItems.Count - 1 do
    FreeItem(fItems.Items[i]);
    fItems.Clear;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Delete Value (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.DeleteValue(const AName: WideString): Boolean;
var
  index: Integer;
begin
  Result:= false;
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    if FindItemIndex(AName, index) then
    Result:= DeleteItem(index);
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Get Item (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.GetItem(AIndex: Integer; var AOutput:
    TCESettingItem): Boolean;
var
  item: PSettingItem;
begin
  Result:= false;

  // exit if we are dead
  if not IsAlive then Exit;

  if (AIndex >= 0) and (AIndex < fItems.Count) then
  begin
    Lock;
    try
      item:= PSettingItem(fItems.Items[AIndex]);
      AOutput.AName:= item.AName;
      AOutput.AValue:= item.AValue;
      Result:= true;
    finally
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Item Count (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.GetItemCount: Integer;
begin
  if IsAlive then
  Result:= fItems.Count
  else
  Result:= 0;
end;

{-------------------------------------------------------------------------------
  Get Last Changed (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.GetLastChanged: Cardinal;
begin
  if IsAlive then
  try
    Lock;
    Result:= fLastChange;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Get Settings File (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.GetSettingsFile: WideString;
var
  s: String;
begin
  // exit if we are dead
  if not IsAlive then
  begin
    Result:= '';
    Exit;
  end;
  
  Assert(assigned(fPluginHost), 'PluginHost must be assigned!');
  
  s:= GUIDToString(fPluginID);
  Delete(s, 1, 1); // remove begin brackets
  Delete(s, Length(s), 1); // remove end bracket
  Result:= fPluginHost.SettingsFolder + s + '.conf';
end;

{-------------------------------------------------------------------------------
  Get Settings Folder (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.GetSettingsFolder: WideString;
begin
  // exit if we are dead
  if not IsAlive then
  begin
    Result:= '';
    Exit;
  end;

  Assert(assigned(fPluginHost), 'PluginHost must be assigned!');
  
  Result:= fPluginHost.SettingsFolder;
end;

{-------------------------------------------------------------------------------
  Get Version (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.GetVersion: Integer;
begin
  // exit if we are dead
  if not IsAlive then
  begin
    Result:= -1;
    Exit;
  end;

  Lock;
  try
    Result:= fVersion;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Load From File (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.LoadFromFile(const AFilePath: WideString; Append:
    Boolean): Boolean;
var
  list: TCCStrings;
  i, index: Integer;
  ws: WideString;
begin
  Result:= false;
  if not IsAlive then Exit;

  {$IFDEF LOG}PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.LoadFromFile: ' + AFilePath, 0, Self);{$ENDIF}
  
  if WideFileExists(AFilePath) then
  begin
    Lock;
    list:= TCCStringList.Create;
    try
      list.NameValueSeparator:= '=';
      // load file
      try
        if AFilePath = '' then
        list.LoadFromFile(GetSettingsFile, [foUnicodeLB])
        else
        list.LoadFromFile(AFilePath, [foUnicodeLB]);
      except
        Exit;
      end;
      // clear previous values if not appending
      if not Append then Clear;
      // write values
      for i:= 0 to list.Count - 1 do
      begin
        ws:= list.PStrings[i]^;
        if Length(ws) > 0 then
        begin
          if ws[1] = VersionPrefix then
          fVersion:= StrToIntDef(Copy(ws, 2, 11), 0)
          else
          WriteString(list.Names[i], WideDecodeLineBreaks(list.ValueFromIndex[i]));
        end;
      end;

      Result:= true;
    finally
      list.Free;
      UnLock;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save To File (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.SaveToFile(const AFilePath: WideString): Boolean;
var
  list: TCCStrings;
  i: Integer;
  item: PSettingItem;
begin
  Result:= false;
  if not IsAlive or (AFilePath = '') then Exit;

  {$IFDEF LOG}PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.SaveToFile: ' + AFilePath, 0, Self);{$ENDIF}

  Lock;
  list:= TCCStringList.Create;
  try
    // add version
    list.Add(VersionPrefix + IntToStr(fVersion));
    // add items
    for i:= 0 to fItems.Count - 1 do
    begin
      item:= fItems.Items[i];
      list.Add(item.AName + '=' + WideEncodeLineBreaks(item.AValue));
    end;

    // save to file
    try
      if WideForceDirectories(WideExtractFilePath(AFilePath)) then
      begin
        if AFilePath = '' then
        list.SaveToFile(GetSettingsFile, [foUnicodeLB])
        else
        list.SaveToFile(AFilePath, [foUnicodeLB]);
      end;
    except
    end;

    Result:= true;
  finally
    list.Free;
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Read Boolean (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.ReadBoolean(const AName: WideString; const ADefault:
    Boolean): Boolean;
var
  item: PSettingItem;
  l: Integer;
begin
  Result:= ADefault;

  // exit if we are dead
  if not IsAlive then exit;
  
  Lock;
  try
    item:= InternalGetItem(AName);
    if assigned(item) then
    begin
      l:= Length(item.AValue);
      if l = 0 then
      Result:= false
      else if l <= 5 then
      begin
        Result:= not ((item.AValue = BoolFalseIntStr) or WideIsSameText(item.AValue, BoolFalseStr));
      end
      else
      Result:= true;
    end;
  finally
    {$IFDEF LOG_SETTINGS}
      PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.ReadBoolean: ' + AName + '=' + BoolToStr(Result, true), 0, Self);
    {$ENDIF}

    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Read Integer (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.ReadInteger(const AName: WideString; const ADefault:
    Integer): Integer;
var
  item: PSettingItem;
begin
  // exit if we are dead
  if not IsAlive then
  begin
    Result:= ADefault;
    exit;
  end;
  
  Lock;
  try
    item:= InternalGetItem(AName);
    if assigned(item) then
    begin
      // try to convert string to integer
      if not TryStrToInt(item.AValue, Result) then
      begin
        // try to convert boolean strings
        if Length(item.AValue) <= 5 then
        begin
          if WideIsSameText(item.AValue, BoolTrueStr) then
          begin
            Result:= BoolTrueInt;
            Exit;
          end
          else if WideIsSameText(item.AValue, BoolFalseStr) then
          begin
            Result:= BoolFalseInt;
            Exit;
          end;
        end;
      end
      else
      Exit;
    end;
    Result:= ADefault;
  finally
    {$IFDEF LOG_SETTINGS}
      PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.ReadInteger: ' + AName + '=' + IntToStr(Result), 0, Self);
    {$ENDIF}

    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Read String (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.ReadString(const AName: WideString; const ADefault:
    WideString): WideString;
var
  item: PSettingItem;
begin
  // exit if we are dead
  if not IsAlive then
  begin
    Result:= ADefault;
    exit;
  end;

  Lock;
  try
    item:= InternalGetItem(AName);
    if assigned(item) then
    Result:= item.AValue
    else
    Result:= ADefault;
  finally
    {$IFDEF LOG_SETTINGS}
      PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.ReadString: ' + AName + '=' + Result, 0, Self);
    {$ENDIF}
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Rename Value (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.RenameValue(const AOldName: WideString; const
    ANewName: WideString): Boolean;
var
  oldI, newI: Integer;
  item: PSettingItem;
begin
  Result:= false;
  
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    if FindItemIndex(AOldName, oldI) and not FindItemIndex(ANewName, newI) then
    begin
      // rename
      item:= fItems.Items[oldI];
      item.AName:= ANewName;
      // move
      if newI > oldI then
      newI:= newI - 1;
      fItems.Move(oldI, newI);

      Result:= true;
    end;
  finally
    {$IFDEF LOG_SETTINGS}
      PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.RenameValue: ' + AOldName + '->' + ANewName + ' = ' + BoolToStr(Result, true), 0, Self);
    {$ENDIF}
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Send Changed Notify (ICEPluginSettings)
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.SendChangedNotify;
begin
  Assert(assigned(fPluginHost), 'PluginHost must be assigned!');

  PluginHost.SendNotify(Self, GetPluginID, PNOTIFY_SettingsChanged, 0, 0);
end;

{-------------------------------------------------------------------------------
  Set Item (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.SetItem(AIndex: Integer; const AItem:
    TCESettingItem): Integer;
var
  item: PSettingItem;
begin
  Result:= -1;
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    if (AIndex >= 0) and (AIndex < fItems.Count) then
    begin
      item:= fItems.Items[AIndex];
      // set value
      item.AValue:= AItem.AValue;
      // rename
      if item.AName <> AItem.AName then
      begin
        // move to new index
        FindItemIndex(AItem.AName, Result);
        if Result > AIndex then
        Result:= Result - 1;
        fItems.Move(AIndex, Result);

        item.AName:= AItem.AName;
      end
      else
      Result:= AIndex;
    end;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Set Version (ICEPluginSettings)
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.SetVersion(AVersion: Integer);
begin
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    fVersion:= AVersion;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Supports Format (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.SupportsFormat(const AName: WideString; AFormat:
    TCESettingFormat): Boolean;
var
  item: PSettingItem;
  b: Boolean;
  i: Integer;
begin
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    item:= InternalGetItem(AName);
    if assigned(item) then
    begin
      // test convertion
      case AFormat of
        sfString, sfBoolean: Result:= true;
        sfInteger: Result:= TryStrToInt(item.AValue, i);
      end;
    end
    else
    Result:= false;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Value Exists (ICEPluginSettings)
-------------------------------------------------------------------------------}
function TCEPluginSettings.ValueExists(const AName: WideString): Boolean;
var
  i: Integer;
begin
  // exit if we are dead
  if not IsAlive then
  begin
    Result:= false;
    Exit;
  end;

  Lock;
  try
    Result:= FindItemIndex(AName, i);
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Write Boolean (ICEPluginSettings)
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.WriteBoolean(const AName: WideString; const AValue:
    Boolean);
var
  item: PSettingItem;
  ws: WideString;
begin
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    item:= InternalGetItem(AName, true);
    // always change the value
    if fAlwaysSetLastChanged then
    begin
      if AValue then
      item.AValue:= BoolTrueIntStr
      else
      item.AValue:= BoolFalseIntStr;
      UpdateLastChanged;

      {$IFDEF LOG_SETTINGS}
        PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.WriteBoolean: ' + AName + '=' + item.AValue, 0, Self);
      {$ENDIF}
    end
    // change only when needed
    else
    begin
      if AValue then
      ws:= BoolTrueIntStr
      else
      ws:= BoolFalseIntStr;

      if not WideIsSameStr(item.AValue, ws) then
      begin
        item.AValue:= ws;
        UpdateLastChanged;
        
        {$IFDEF LOG_SETTINGS}
          PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.WriteBoolean: ' + AName + '=' + item.AValue, 0, Self);
        {$ENDIF}
      end;
    end;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Write Integer (ICEPluginSettings)
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.WriteInteger(const AName: WideString; const AValue:
    Integer);
var
  item: PSettingItem;
  ws: WideString;
begin
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    item:= InternalGetItem(AName, true);
    // always change the value
    if fAlwaysSetLastChanged then
    begin
      item.AValue:= IntToStr(AValue);
      UpdateLastChanged;

      {$IFDEF LOG_SETTINGS}
        PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.WriteInteger: ' + AName + '=' + item.AValue, 0, Self);
      {$ENDIF}
    end
    // change only when needed
    else
    begin
      ws:= IntToStr(AValue);
      if not WideIsSameStr(item.AValue, ws) then
      begin
        item.AValue:= ws;
        UpdateLastChanged;

        {$IFDEF LOG_SETTINGS}
          PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.WriteInteger: ' + AName + '=' + item.AValue, 0, Self);
        {$ENDIF}
      end;
    end;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Write String (ICEPluginSettings)
-------------------------------------------------------------------------------}
procedure TCEPluginSettings.WriteString(const AName: WideString; const AValue:
    WideString);
var
  item: PSettingItem;
begin
  // exit if we are dead
  if not IsAlive then Exit;

  Lock;
  try
    item:= InternalGetItem(AName, true);
    // always change the value
    if fAlwaysSetLastChanged then
    begin
      item.AValue:= AValue;
      UpdateLastChanged;

      {$IFDEF LOG_SETTINGS}
        PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.WriteString: ' + AName + '=' + item.AValue, 0, Self);
      {$ENDIF}
    end
    // change only when needed
    else
    begin
      if not WideIsSameStr(item.AValue, AValue) then
      begin
        item.AValue:= AValue;
        UpdateLastChanged;

        {$IFDEF LOG_SETTINGS}
          PluginHost.Log(GuidToString(GetPluginID) +  '.Settings.WriteString: ' + AName + '=' + item.AValue, 0, Self);
        {$ENDIF}
      end;
    end;
  finally
    UnLock;
  end;
end;

{##############################################################################}
// TCEPluginFactoryLibraryHost

{-------------------------------------------------------------------------------
  Get Plugin Class
-------------------------------------------------------------------------------}
function TCEPluginFactoryLibraryHost.GetPluginClass: TCCPluginClass;
begin
  Result:= TCEPluginFactoryLibrary;
end;

{##############################################################################}
// TCEPluginFactoryLibrary

{-------------------------------------------------------------------------------
  Create an instance of TCEPluginFactoryLibrary
-------------------------------------------------------------------------------}
constructor TCEPluginFactoryLibrary.Create;
begin
  inherited Create;
  _CreatePluginFactory:= nil;
end;

{-------------------------------------------------------------------------------
  CreatePluginFactory
-------------------------------------------------------------------------------}
function TCEPluginFactoryLibrary.CreatePluginFactory: ICEPluginFactory;
begin
  if assigned(_CreatePluginFactory) then
  begin
    if not _CreatePluginFactory(CE_PluginInterfaceVersion, Result) then
    Result:= nil;
  end
  else
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Initialize Plugin
  - Return true if plugin is correctly initialized.
  - Return false if the library is not the right kind of plugin or otherwise
    not supported.
  - This method is called before the plugin is added to the PluginHost list. If
    false is returned, it won't get added to the list.
-------------------------------------------------------------------------------}
function TCEPluginFactoryLibrary.InitializePlugin(AHost: TCCPluginHost): Boolean;
begin
  Result:= false;
  try
  @_CreatePluginFactory:= GetProcAddress(Self.LibraryHandle, 'CreatePluginFactory');
  except
  end;
  Result:= assigned(_CreatePluginFactory);
end;

{##############################################################################}
// TCECustomPluginFactory

{-------------------------------------------------------------------------------
  CreatePlugin
-------------------------------------------------------------------------------}
function TCECustomPluginFactory.CreatePlugin(const APluginID: TGUID): ICEPlugin;
begin
  Result:= nil;
  // override from descendant
end;

{-------------------------------------------------------------------------------
  CreateSettingsEditor
-------------------------------------------------------------------------------}
function TCECustomPluginFactory.CreateSettingsEditor(const APluginID: TGUID):
    ICEPluginSettingsEditor;
begin
  Result:= nil;
  // override from descendant
end;

{-------------------------------------------------------------------------------
  GetPluginCount
-------------------------------------------------------------------------------}
function TCECustomPluginFactory.GetPluginCount: Integer;
begin
  Result:= 0;
  // override from descendant
end;

{-------------------------------------------------------------------------------
  GetPluginInfo
-------------------------------------------------------------------------------}
function TCECustomPluginFactory.GetPluginInfo(AIndex: Integer): ICEPluginInfo;
begin
  Result:= nil;
  // override from descendant
end;

{-------------------------------------------------------------------------------
  Initialize Settings
-------------------------------------------------------------------------------}
procedure TCECustomPluginFactory.InitializeSettings(const APluginID: TGUID;
    const ASettings: ICEPluginSettings);
begin
  // override from descendant
end;

{##############################################################################}
// TCECustomPlugin

{-------------------------------------------------------------------------------
  Initialize
-------------------------------------------------------------------------------}
function TCECustomPlugin.Initialize(const AHost: ICEPluginHost): Boolean;
begin
  Result:= true;
  fHost:= AHost;
end;

{-------------------------------------------------------------------------------
  Finalize
-------------------------------------------------------------------------------}
procedure TCECustomPlugin.Finalize;
begin
  fHost:= nil;
end;

{-------------------------------------------------------------------------------
  GetPluginID
-------------------------------------------------------------------------------}
function TCECustomPlugin.GetPluginID: TGUID;
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  Notify
-------------------------------------------------------------------------------}
procedure TCECustomPlugin.Notify(const ASender: IInterface; ANotify: Integer;
    AParam1, AParam2: Integer);
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  LoadSettings
-------------------------------------------------------------------------------}
procedure TCECustomPlugin.LoadSettings(const ASettings: ICEPluginSettings);
begin
  // override from descendant
end;

{-------------------------------------------------------------------------------
  SaveSettings
-------------------------------------------------------------------------------}
procedure TCECustomPlugin.SaveSettings(const ASettings: ICEPluginSettings);
begin
  // override from descendant
end;

{##############################################################################}

initialization

finalization
  // free instances
  if assigned(fGlobalPluginHost) then
  begin
    fGlobalPluginHost.Finalize;
    fGlobalPluginHost:= nil;
  end;

end.
