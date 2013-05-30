unit CE_PluginsIntf;

interface

uses
  // CubicCore
  ccInterface,
  // System Units
  Windows;

const
  CE_PluginInterfaceVersion = 1;

  IID_ICEPlugin: TGUID            = '{4C44B9D5-3856-4798-A258-F056F9741B16}';
  IID_ICEPluginInfo: TGUID        = '{6BB64D5B-8FF7-4062-A81E-341F761A56D9}';
  IID_ICEPluginSettings: TGUID    = '{CFBC9470-78EA-4CEE-BDFC-CDBDBB67A242}';
  IID_ICEPluginWindow: TGUID      = '{767D7694-BD62-4022-B485-840E1A7905EA}';
  IID_ICEPluginFactory: TGUID     = '{85E0FF21-684B-49A5-987D-7BEF4AC5682B}';
  IID_ICEPluginHost: TGUID        = '{2E65FE65-9FC3-4792-BF73-8E463DF808B6}';
  IID_ICEPluginSettingsEditor     = '{6CF81877-4406-4BF2-AB40-608FB2729DFF}';
  IID_ICEPluginSettingsEditorHost = '{A7912987-AE76-4FB4-A81E-595A43DB5685}';
  IID_ICEPluginPermissions        = '{D682C654-EB96-42CE-96EE-425C22346093}';

  // Log Msg type (MTYPE)
  MTYPE_TICK    = -1;
  MTYPE_NORMAL  = 0;
  MTYPE_HINT    = 1;
  MTYPE_WARNING = 2;
  MTYPE_ERROR   = 3;

  // Plugin Notify (PNOTIFY)
  PNOTIFY_SettingsChanged = 1;

  // Plugin Type (PTYPE)
  PTYPE_Unknown         = 0;
  PTYPE_Service         = 1;
  PTYPE_Button          = 2;
  PTYPE_QuickViewViewer = 3;
  PTYPE_Tab             = 4;
  PTYPE_Panel           = 5;
  
type
  ICEPluginSettings           = interface;
  ICEPluginInfo               = interface;
  ICEPluginHost               = interface;
  ICEPluginSettingsEditor     = interface;
  ICEPluginSettingsEditorHost = interface;
  ICEPluginPermissions        = interface;
  
{-------------------------------------------------------------------------------
  ICEPlugin
  - Base class for plugins
-------------------------------------------------------------------------------}
  ICEPlugin = interface(IInterface)
  ['{4C44B9D5-3856-4798-A258-F056F9741B16}']
    // Initialize
    // - Get's called once, right after the plugin is created.
    // - AHost is the PluginHost that created the Plugin.
    function Initialize(const AHost: ICEPluginHost): Boolean; stdcall;

    // Finalize
    // - Get's called once, just before the plugin is destroyed.
    // - Plugin should release AHost and ASettings here.
    procedure Finalize; stdcall;

    // GetPluginID
    // - Returns the PluginID.
    function GetPluginID: TGUID; stdcall;

    // LoadSettings
    // - Get's called after Initialize or when ever someone calls
    //   ICEPluginHost.LoadSettings with this plugin.
    // - ASettings holds settings for the plugin.
    procedure LoadSettings(const ASettings: ICEPluginSettings); stdcall;

    // Notify
    // - Get's called on PluginHost.SendNotify.
    // - ASender is the instance that called Host's SendNotify. Warning, can be NIL!
    // - ANotify is the notify event (PNOTIFY).
    // - AParam1 and AParam2 have different parameters depending on the Notify event.
    procedure Notify(const ASender: IInterface; ANotify: Integer; AParam1, AParam2:
        Integer); stdcall;

    // SaveSettings
    // - Get's called before Finalize or when ever someone calls
    //   ICEPluginHost.SaveSettings with this plugin.
    // - ASettings holds settings for the plugin.
    procedure SaveSettings(const ASettings: ICEPluginSettings); stdcall;

  end;

{-------------------------------------------------------------------------------
  ICEPluginInfo
  - Provides information about a plugin.
-------------------------------------------------------------------------------}
  ICEPluginInfo = interface(IInterface)
  ['{6BB64D5B-8FF7-4062-A81E-341F761A56D9}']
    // GetPluginName
    // - Return plugin's name.
    function GetPluginName: WideString; stdcall;

    // GetPluginDescription
    // - Return plugin's description.
    function GetPluginDescription: WideString; stdcall;

    // GetPluginID
    // - Return plugin's ID (has to be unique).
    // - This ID is used to create plugins from ICEPluginFactory.
    function GetPluginID: TGUID; stdcall;

    // GetPluginVersion
    // - Return plugin's version number.
    // - PluginSettings store the plugin version. When new plugin is released,
    //   plugin can compare the settings version and make modification accordingly.
    // - It is up to the plugin developer to decide what the version number scheme is.
    //   Recommended scheme is to start with 1 and increment the version by 1 with each release.
    function GetPluginVersion: Integer; stdcall;

    // GetPluginType
    // - Return a plugin type.
    function GetPluginType: Integer; stdcall;

    // GetSupportedFormats
    // - Return semicolon separated list of formats that are supported by the plugin.
    function GetSupportedFormats: WideString; stdcall;
  end;

{-------------------------------------------------------------------------------
  ICEPluginSettings
  - Plugins can use this to read/write settings.
  - Settings are stored as strings when saved.
  - Names are case insensitive!
    Allowed characters for names are: a-z, A-Z, 0-9, _ and . and -
-------------------------------------------------------------------------------}
  TCESettingFormat = (sfString, sfInteger, sfBoolean);

  PCESettingItem = ^TCESettingItem;
  TCESettingItem = record
    AName: WideString;
    AValue: WideString;
  end;

  ICEPluginSettings = interface(ICCThreadedInterface)
  ['{CFBC9470-78EA-4CEE-BDFC-CDBDBB67A242}']
    // GetVersion
    // - Return the settings version.
    function GetVersion: Integer; stdcall;

    // SetVersion
    // - Set the settings version.
    procedure SetVersion(AVersion: Integer); stdcall;

    // GetPluginID
    // - Returns the PluginID to which this settings instance is assigned with.
    function GetPluginID: TGUID; stdcall;

    // Clear
    // - Delete all values.
    procedure Clear; stdcall;

    // DeleteItem
    // - Deletes item from specified index.
    /// - AIndex range is from 0 (zero) to GetItemCount-1.
    // - Returns FALSE if AIndex is invalid, else TRUE
    function DeleteItem(AIndex: Integer): Boolean; stdcall;

    // DeleteValue
    // - Deletes value. AName is the name of the value.
    // - Returns TRUE if item was deleted, else FALSE.
    function DeleteValue(const AName: WideString): Boolean; stdcall;

    // GetItem
    // - Returns item from specified index.
    // - AIndex range is from 0 (zero) to GetItemCount-1.
    // - Returns FALSE if AIndex is invalid, else TRUE
    function GetItem(AIndex: Integer; var AOutput: TCESettingItem): Boolean; stdcall;

    // SetItem
    // - Set item value and/or name for specified index.
    // - AIndex range is from 0 (zero) to GetItemCount-1.
    // - If item name is changed, the item will be moved to another index
    //   if needed to keep the list sorted. The new index is returned.
    // - If AIndex is invalid, returns -1. Else item's index is returned.
    function SetItem(AIndex: Integer; const AItem: TCESettingItem): Integer;
        stdcall;

    // GetItemCount
    // - Returns the item count.
    function GetItemCount: Integer; stdcall;

    // GetLastChanged
    // - Returns a tick count when last value change was made.
    function GetLastChanged: Cardinal; stdcall;
    
    // RenameValue
    // - Rename value.
    // - AOldName is the name of the value that will be renamed.
    // - ANewName is the new name.
    // - If value named AOldName does not exits, nothing is done.
    // - If value named ANewName exists, nothing is done.
    // - Returns TRUE if renamed successfully, else FALSE.
    function RenameValue(const AOldName: WideString; const ANewName: WideString):
        Boolean; stdcall;

    // ValueExists
    // - Returns TRUE if value exists, else FALSE.
    function ValueExists(const AName: WideString): Boolean; stdcall;

    // SupportsFormat
    // - Check to see if value can be converted in to a specific format.
    // - AName is the name of the value. AFormat is the format to check.
    // - If value does not exist, or it can't be converted, returns FALSE, else TRUE.
    function SupportsFormat(const AName: WideString; AFormat: TCESettingFormat):
        Boolean; stdcall;

    // ReadString
    // - Return string value. AName is the name of the value.
    // - If value is not found, ADefault is returned.
    function ReadString(const AName: WideString; const ADefault: WideString):
        WideString; stdcall;

    // ReadInteger
    // - Return integer value. AName is the name of the value.
    // - If value is not found, or it can't be converted into an integer,
    //   ADefault is returned.
    // - Boolean strings will be converted to integer:
    //   'false' = 0, 'true' = 1
    function ReadInteger(const AName: WideString; const ADefault: Integer):
        Integer; stdcall;

    // ReadBoolean
    // - Return boolean value. AName is the name of the value.
    // - If value is not found ADefault is returned.
    // - Returns FALSE If value is empty text, '0' or 'false'.
    //   Else TRUE is returned.
    function ReadBoolean(const AName: WideString; const ADefault: Boolean):
        Boolean; stdcall;

    // WriteString
    // - Write string value. AName is the name of the value.
    // - If value does not exist, it will be created.
    procedure WriteString(const AName: WideString; const AValue: WideString);
        stdcall;

    // WriteInteger
    // - Write integer value. AName is the name of the value.
    // - If value does not exist, it will be created.
    procedure WriteInteger(const AName: WideString; const AValue: Integer);
        stdcall;

    // WriteBoolean
    // - Write boolean value. AName is the name of the value.
    // - If value does not exist, it will be created.
    // - When reading the value as string/integer, TRUE = 1 and FALSE = 0
    procedure WriteBoolean(const AName: WideString; const AValue: Boolean);
        stdcall;

    // GetSettingsFolder
    // - Returns a folder path where plugin should save files if it needs to (cache etc.).
    // - NOTICE, the plugin is responsible for file naming! It should not override
    //   files created by other plugins.
    // - If plugin creates multiple files, it is recommended that it puts them
    //   into a subfolder.
    // - It is recommended that file/subfolder names are human readable and
    //   include the plugin/plugin name or id. That way it's easier for the user to
    //   identify which file belongs to which plugin/plugin.
    // - Returned path WILL include trailing path delimiter.
    function GetSettingsFolder: WideString; stdcall;

    // GetSettingsFile
    // - Returns a path to the settings file that is used to store settings.
    // - The returned path is: GetSettingsFolder + PluginID (without brackets) + .conf
    function GetSettingsFile: WideString; stdcall;

    // LoadFromFile
    // - Load settings from file.
    // - AFilePath is a path to the file where the settings are loaded from.
    //   If AFilePath = '', then GetSettingsFile is used as file path.
    // - If Append is TRUE, settings will be added to existing values.
    // - If Append is FALSE, all previous settings will be cleared.
    // - Returns TRUE if settings were loaded successfully, else FALSE.
    function LoadFromFile(const AFilePath: WideString; Append: Boolean): Boolean;
        stdcall;

    // SaveToFile
    // - Save settings to file.
    // - AFilePath is a path to the file where the settings should be saved.
    //   If AFilePath = '', then GetSettingsFile is used as file path.
    // - Returns TRUE if settings were saved successfully, else FALSE.
    function SaveToFile(const AFilePath: WideString): Boolean; stdcall;

    // SendChangedNotify
    // - Call this to send PNOTIFY_SettingChanged notification to all open
    //   plugin instances (who have the same PluginID as the settings).
    procedure SendChangedNotify; stdcall;
  end;

{-------------------------------------------------------------------------------
  ICEPluginWindow
  - Plugin can use this interface if it wants to use a window.
-------------------------------------------------------------------------------}
  ICEPluginWindow = interface(IInterface)
  ['{767D7694-BD62-4022-B485-840E1A7905EA}']
    // SetBounds
    // - Set window boundaries, relative to the parent window.
    procedure SetBounds(const ARect: TRect); stdcall;

    // SetFocus
    // - Set focus to the window.
    procedure SetFocus; stdcall;

    // SetParentWindow
    // - Set Parent Window.
    procedure SetParentWindow(AParentWindow: HWND); stdcall;
  end;

{-------------------------------------------------------------------------------
  ICEPluginFactory
  - This interface is used to create plugins.
-------------------------------------------------------------------------------}
  ICEPluginFactory = interface(IInterface)
  ['{85E0FF21-684B-49A5-987D-7BEF4AC5682B}']
    // CreatePlugin
    // - Return new instance of ICEPlugin based on APluginID.
    // - If APluginID is not supported, should return nil.
    function CreatePlugin(const APluginID: TGUID): ICEPlugin; stdcall;

    // CreateSettingsEditor
    // - Return new instance of ICEPluginSettingsEditor based on APluginID.
    // - If APluginID is not supported, should return nil.
    // - This is an optional feature, factories are not required to provide
    //   SettingsEditors.
    function CreateSettingsEditor(const APluginID: TGUID): ICEPluginSettingsEditor;
        stdcall;

    // GetPluginCount
    // - Return the number of plugins supported.
    function GetPluginCount: Integer; stdcall;

    // GetPluginInfo
    // - Return an intance of ICEPluginInfo based on AIndex.
    // - AIndex range is from 0 (zero) to GetPluginCount-1.
    function GetPluginInfo(AIndex: Integer): ICEPluginInfo; stdcall;

    // InitializeSettings
    // - Get's called when settings are loaded for the first time since
    //   application start.
    // - Factories can setup initial values here or do modifications if
    //   settings are from older version.
    procedure InitializeSettings(const APluginID: TGUID; const ASettings:
        ICEPluginSettings); stdcall;
  end;

  // TCECreatePluginFactory
  // - This is the method that dlls should export as: "CreatePluginFactory" (without quotes).
  //   It's used to create a plugin factory.
  // - AInterfaceVersion is the version number for the interfaces that the
  //   calling application uses. Plugins can use AInterfaceVersion to make sure
  //   that they are compatible with the calling app.
  // - Result should be TRUE if factory is compatible with the interface version
  //   and it's instance was returned in APluginFactory. Else FALSE.
  TCECreatePluginFactory = function(AInterfaceVersion: Integer; out APluginFactory: ICEPluginFactory): boolean; stdcall;

{-------------------------------------------------------------------------------
  ICEPluginHost
  - PluginHost manages plugins, settings and factories.
-------------------------------------------------------------------------------}
  PCEDefaultItem = ^TCEDefaultItem;
  TCEDefaultItem = record
    PluginID: TGUID;
    PluginType: Integer;
    Formats: WideString;
  end;

  PCEPluginData = ^TCEPluginData;
  TCEPluginData = record
    Info: ICEPluginInfo;
    Factory: ICEPluginFactory;
    Settings: ICEPluginSettings;
    PluginPath: WideString;
  end;

  ICEPluginHost = interface(ICCThreadedInterface)
  ['{2E65FE65-9FC3-4792-BF73-8E463DF808B6}']
    // CreatePlugin
    // - Create an instance of plugin by it's ID.
    // - Plugin's Initialize is called in this method.
    // - If plugin is not found, returns nil.
    // - NOTICE, plugins created by this method HAS TO be destroyed with
    //   DestroyPlugin. Plugins are added to an instance list, so they wont get
    //   released if DestroyPlugin is not called!
    function CreatePlugin(const APluginID: TGUID): ICEPlugin; stdcall;

    // DestroyPlugin
    // - Destroys plugin.
    // - This method removes the APlugin from host's instance list. Then it calls
    //   APlugin's Finalize and sets APlugin to nil.
    procedure DestroyPlugin(var APlugin: ICEPlugin); stdcall;

    // FindPluginID
    // - Finds PluginID that is associated with the specified plugin type and format.
    // - If ID is found, returns TRUE, else FALSE.
    function FindPluginID(APluginType: Integer; const AFormat: WideString; out
        APluginID: TGUID): Boolean; stdcall;

    // GetDefaultItemCount
    // - Returns the number of Default items.
    function GetDefaultItemCount: Integer; stdcall;

    // GetDefaultItem
    // - Get Default item from specified index.
    // - AIndex range is from 0 (zero) to GetDefaultItemCount-1;
    // - Returns FALSE if AIndex is invalid, else TRUE.
    // - NOTICE, caller is responsible for freeing the resulted data!
    function GetDefaultItem(AIndex: Integer; var AOutput: TCEDefaultItem): Boolean;
        stdcall;

    // GetPluginDataCount
    // - Returns the number of PluginData items (registered plugins).
    function GetPluginDataCount: Integer; stdcall;

    // GetPluginData
    // - Returns PluginData from specified index.
    // - AIndex range is from 0 (zero) to GetPluginDataCount-1;
    // - Returns FALSE if AIndex is invalid, else TRUE.
    // - NOTICE, caller is responsible for freeing the resulted data!
    function GetPluginData(AIndex: Integer; var AOutput: TCEPluginData): Boolean; stdcall;

    // GetPluginDataByID
    // - Returns PluginData for specified plugin id.
    // - Returns FALSE if plugin is not found, else TRUE.
    // - NOTICE, caller is responsible for freeing the resulted data!
    function GetPluginDataByID(const APluginID: TGUID; var AOutput: TCEPluginData):
        Boolean; stdcall;

    // GetPluginSettings
    // - Returns PluginSettings for specified PluginID.
    // - If Plugin is not registered, returns nil.
    function GetPluginSettings(const APluginID: TGUID): ICEPluginSettings; stdcall;

    // GetSettingsFolder
    // - Return settings path.
    function GetSettingsFolder: WideString; stdcall;

    // SetSettingsFolder
    // - Assign a folder path where all plugin settings should be saved.
    // - Plugins can use this path to save their own files too.
    // - If the folder does not exist, it will be created.
    procedure SetSettingsFolder(const APath: WideString); stdcall;

    // RegisterFactory
    // - Register factory.
    // - Notice, UnRegisterFactory is called here to make sure there are no dublicate entries!
    procedure RegisterFactory(const AFactory: ICEPluginFactory); stdcall;

    // ReadSettingsFromDisk
    // - Read all plugin settings from disk to memory.
    // - APluginID is the plugin whose settings should be read.
    //   If APluginID = NULL_GUID, all plugins will be read.
    // - If Force = TRUE, settings are always read. If FALSE, settings are only
    //   read if they have not been read/written before.
    // - If Append = TRUE, settings are appended to existing ones If FALSE,
    //   all previous settings are cleared.
    // - Settings will be read from GetSettingsFolder path.
    // - Returns TRUE if settings were read, else FALSE.
    function ReadSettingsFromDisk(const APluginID: TGUID; Force: Boolean; Append:
        Boolean): Boolean; stdcall;

    // WriteSettingsToDisk
    // - Write plugin settings from memory to disk if needed.
    // - APluginID is the plugin whose settings should be written.
    //   If APluginID = NULL_GUID, all plugins will be written.
    // - If Force = TRUE, settings are always written.
    //   If Force = FALSE, settings are only written if they are changed.
    // - Settings will be written to GetSettingsFolder path.
    // - Returns TRUE if settings were written, else FALSE.
    function WriteSettingsToDisk(const APluginID: TGUID; Force: Boolean): Boolean;
        stdcall;

    // LoadSettings
    // - Load settings for specified plugin.
    // - If Append = TRUE, settings will be appended to existing settings.
    //   Else all previous settings will be cleared.
    procedure LoadSettings(const APlugin: ICEPlugin; Append: Boolean); stdcall;

    // SaveSettings
    // - Save settings for specified plugin.
    procedure SaveSettings(const APlugin: ICEPlugin); stdcall;

    // UnRegisterFactory
    // - Unregister factory.
    procedure UnRegisterFactory(const AFactory: ICEPluginFactory); stdcall;

    // Log
    // - Add message to log.
    // - AMessage is the message to be added to the log.
    // - AMsgType (MTYPE) is the type of message that will be added.
    // - Returns tick count. The tick count is retreived just before this method
    //   returns.
    // - If AMsgType = MTYPE_TICK, only tick is returned, no message will be
    //   added to the log.
    // - ASender is the sender of the log message, can be nil.
    // - Returns -1 if the host is not Alive anymore.
    function Log(const AMessage: WideString; AMsgType: Integer; const ASender:
        IInterface): Int64; stdcall;

    // LoadDefaults
    // - Load defaults from file.
    // - Returns TRUE if successful, else FALSE.
    function LoadDefaults(const AFilePath: WideString): Boolean; stdcall;

    // SaveDefaults
    // - Save defaults to file.
    // - Returns TRUE if successful, else FALSE.
    function SaveDefaults(const AFilePath: WideString): Boolean; stdcall;

    // SetAsDefault
    // - Set specified plugin as default for all formats it supports.
    // - If specified plugin is not registered, nothing is done and return is FALSE.
    // - Returns TRUE if successfull, else FALSE.
    function SetAsDefault(const APluginID: TGUID): Boolean; stdcall;

    // SetAsDefaultFor
    // - Set specified plugin as default for extensions listed in AExtensions.
    // - NOTICE, the plugin does NOT have to be registered!
    // - APluginType is the plugin type.
    // - AFormats is a semicolon separated list of formats.
    //   Use * to to make default for all formats.
    // - Returns TRUE if successfull, else FALSE.
    function SetAsDefaultFor(const APluginID: TGUID; APluginType: Integer;
        const AFormats: WideString): Boolean; stdcall;

    // LoadPlugins
    // - Load plugins from specified folder.
    // - If APluginExtension <> '', then only files that have the specified
    //   extension are loaded.
    // - If APluginExtension = '', then .dll files are loaded.
    // - If AIncludeSubfolders = TRUE, sub folders are also searched for plugins.
    // - Returns the number of plugins loaded.
    // - If the Host is not Alive or other error occurs, returns -1.
    // - All found plugin factories are automatically registered.
    // - New plugins are appended to the list. If plugin is already loaded,
    //   nothing is done.
    function LoadPlugins(const AFolderPath: WideString; const APluginExtension:
        WideString; AIncludeSubfolders: Boolean): Integer; stdcall;

    // GetTick
    // - Returns tick count.
    function GetTick: Cardinal; stdcall;

    // SendNotify
    // - Send notification to all open plugin instances with specific PluginID.
    // - ASender should be the sender of the notify message, can be nil.
    // - If APluginID = NULL_GUID, the notify event will be sent to all plugins.
    // - If ANotify = ENOTIFY_SettingsChanged, plugin's SettingsChanged method
    //   is called.
    procedure SendNotify(const ASender: IInterface; const APluginID: TGUID;
        ANotify: Integer; AParam1, AParam2: Integer); stdcall;

    // Permissions
    // - Returns an instance of ICEPluginPermissions.
    function Permissions: ICEPluginPermissions; stdcall;
  end;

{-------------------------------------------------------------------------------
  ICEPluginSettingsEditor
-------------------------------------------------------------------------------}
  ICEPluginSettingsEditor = interface(IInterface)
  ['{6CF81877-4406-4BF2-AB40-608FB2729DFF}']
    // Initialize
    // - Gets called right after the editor instance has been created.
    // - AHost is the host that manages editors. Editors should store the AHost
    //   refrence so that they can call it's ValueHasChanged when value has changed.
    // - APluginID is the PluginID whose settings are being edited.
    procedure Initialize(const AHost: ICEPluginSettingsEditorHost; const APluginID:
        TGUID); stdcall;

    // Finalize
    // - Gets called before the editor instance will be destroyed.
    // - AHost should be released here.
    procedure Finalize; stdcall;

    // Load Settings
    // - Load values to editors.
    // - Gets called after Initialize -> SetParentWindow -> SetBounds
    procedure LoadSettings(const ASettings: ICEPluginSettings); stdcall;

    // - Save editor values to settings.
    // - Gets called when Apply or OK is pressed.
    // - It's recommended that editors save settings only in this method and
    //   not directly when editor value is changed.
    procedure SaveSettings(const ASettings: ICEPluginSettings); stdcall;

    // SetBounds
    // - Set window boundaries, relative to the parent window.
    // - Gets called after SetParentWindow and when the parent window is resized.
    procedure SetBounds(const ARect: TRect); stdcall;

    // SetParentWindow
    // - Set Parent Window.
    // - Gets called after Initialize.
    procedure SetParentWindow(AParentWindow: HWND); stdcall;
  end;  

{-------------------------------------------------------------------------------
  ICEPluginSettingsEditorHost
  - A host that manages SettingsEditors.
-------------------------------------------------------------------------------}
  ICEPluginSettingsEditorHost = interface(IInterface)
  ['{A7912987-AE76-4FB4-A81E-595A43DB5685}']
    // ValueHasChanged
    // - Editors should call this method when user changes some value.
    // - This method enables the Apply button and makes sure editor's
    //   SaveSettings is called before destroying it.
    procedure ValueHasChanged(const AEditor: ICEPluginSettingsEditor); stdcall;
  end;

{-------------------------------------------------------------------------------
  ICEPluginPermissions
-------------------------------------------------------------------------------}
  ICEPluginPermissions = interface(IInterface)
  ['{D682C654-EB96-42CE-96EE-425C22346093}']
    // GetAllowedCount
    // - Returns the number of Allowed items.
    function GetAllowedCount: Integer; stdcall;

    // GetAllowedItem
    // - Returns an Allowed item by it's index.
    // - Valid Index range is from 0 to GetAllowedCount-1.
    // - If Index is not in valid range and ALibrary is not empty, the item
    //   is searched by ALibrary.
    // - Returns the index of item if it's found, else -1.
    function GetAllowedItem(Index: Integer; var ALibrary: WideString; out
        APluginIDs: WideString): Integer; stdcall;

    // SetAllowedItem
    // - Add allowed item.
    // - ALibrary is an absolute or relative (to exe) path of the plugin library.
    //   If ALibrary = *, all libraries will be included.
    // - If ALibrary already exists on the list, APluginIDs will be appended to it.
    // - APluginIDs should be a semicolon separated list of plugin IDs that
    //   should be allowed.
    // - Returns the index of the item added.
    function SetAllowedItem(const ALibrary: WideString; const APluginIDs:
        WideString): Integer; stdcall;

    // DeleteAllowedItem
    // - Delete Allowed item by it's index.
    // - Valid Index range is from 0 to GetAllowedCount-1.
    // - Returns TRUE if item was deleted, else FALSE.
    function DeleteAllowedItem(Index: Integer): Boolean; stdcall;

    // ClearAllowedItems
    // - Clear all Allowed items.
    // - Returns the number of items deleted.
    function ClearAllowedItems: Integer; stdcall;

    // GetBlockedCount
    // - Returns the number of Blocked items.
    function GetBlockedCount: Integer; stdcall;

    // GetBlockedItem
    // - Returns an Blocked item by it's index.
    // - Valid Index range is from 0 to GetBlockedCount-1.
    // - If Index is not in valid range and ALibrary is not empty, the item
    //   is searched by ALibrary.
    // - Returns the index of item if it's found, else -1.
    function GetBlockedItem(Index: Integer; out ALibrary, APluginIDs: WideString):
        Integer; stdcall;

    // SetBlockedItem
    // - Add Blocked item.
    // - ALibrary is an absolute or relative (to exe) path of the plugin library.
    //   If ALibrary = *, all libraries will be included.
    // - If ALibrary already exists on the list, APluginIDs will be appended to it.
    // - APluginIDs should be a semicolon separated list of plugin IDs that
    //   should be allowed.
    // - Returns the index of the item added.
    function SetBlockedItem(const ALibrary: WideString; const APluginIDs:
        WideString): Integer; stdcall;

    // DeleteBlockedItem
    // - Delete Blocked item by it's index.
    // - Valid Index range is from 0 to GetBlockedCount-1.
    // - Returns TRUE if item was deleted, else FALSE.
    function DeleteBlockedItem(Index: Integer): Boolean; stdcall;

    // ClearBlockedItems
    // - Clear all Blocked items.
    // - Returns the number of items deleted.
    function ClearBlockedItems: Integer; stdcall;

    // GetBlockByDefault
    // - Returns TRUE if non allowed plugins are blocked by default, else TRUE.
    function GetBlockByDefault: Boolean; stdcall;

    // SetBlockByDefault
    // - If Value=TRUE, non allowed plugins are blocked by default.
    // - If Value=FALSE, non allowed plugins will be registered by default.
    procedure SetBlockByDefault(Value: Boolean); stdcall;

    // IsAllowed
    // - Returns FALSE if specified item is in Blocked list.
    // - Returns FALSE if BlockByDefault=TRUE and the specified item is not in the
    //   Allowed list.
    // - Returns TRUE if specified item is in the Allowed list (even if it's in
    //   the Blocked list also).
    // - Returns TRUE if BlockByDefault=FALSE and the specified item is not in the
    //   Blocked list.
    function IsAllowed(const ALibrary: WideString; const APluginID: WideString):
        Boolean; stdcall;
  end;

implementation

end.
