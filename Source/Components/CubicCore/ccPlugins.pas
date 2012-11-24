//******************************************************************************
//  CubicCore
//  Version: 1.00
//
//  The contents of this file are subject to the Mozilla Public License
//  Version 1.1 (the "License"); you may not use this file except in
//  compliance with the License. You may obtain a copy of the License at
//  http://www.mozilla.org/MPL/
//
//  Software distributed under the License is distributed on an "AS IS"
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
//  License for the specific language governing rights and limitations
//  under the License.
//
//  The Original Code is ccPlugins.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccPlugins;

interface

uses
  // CubicCore
  ccFileUtils,
  ccStrings,
  // System Units
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  dynlibs,
  {$ENDIF}  
  SysUtils, Contnrs;

type
  TCCPluginHost = class;

{-------------------------------------------------------------------------------
  TCCPlugin
-------------------------------------------------------------------------------}
  TCCPluginClass = class of TCCPlugin;

  TCCPlugin = class(TObject)
  protected
    fLibraryHandle: Cardinal;
    fLibraryPath: WideString;
    procedure FinalizePlugin(AHost: TCCPluginHost); virtual;
    function InitializePlugin(AHost: TCCPluginHost): Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property LibraryHandle: Cardinal read fLibraryHandle;
    property LibraryPath: WideString read fLibraryPath;
  end;

{-------------------------------------------------------------------------------
  TCCPluginHost
-------------------------------------------------------------------------------}
  TCCPluginHost = class(TObject)
  protected
    fPlugins: TObjectList;
    function GetCount: Integer; virtual;
    function GetPlugins(Index: Integer): TCCPlugin; virtual;
    function GetPluginClass: TCCPluginClass; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IndexOf(APlugin: TCCPlugin): Integer; virtual;
    function LoadPlugin(APluginPath: WideString): Integer; virtual;
    function LoadPluginsFromFolder(AFolderPath: WideString; AIncludeSubFolders:
        Boolean = false; APluginExtension: WideString = ''): Integer; virtual;
    procedure UnloadAll; virtual;
    procedure UnloadPlugin(Index: Integer); virtual;
    property Count: Integer read GetCount;
    property Plugins[Index: Integer]: TCCPlugin read GetPlugins;
  end;

implementation

{##############################################################################}
// TCCPlugin

{-------------------------------------------------------------------------------
  Create an instance of TCCPlugin
-------------------------------------------------------------------------------}
constructor TCCPlugin.Create;
begin
  inherited Create;
  // initialize values
  fLibraryHandle:= 0;
  fLibraryPath:= '';
end;

{-------------------------------------------------------------------------------
  Destroy TCCPlugin
-------------------------------------------------------------------------------}
destructor TCCPlugin.Destroy;
begin
  // free library
  if fLibraryHandle <> 0 then
  FreeLibrary(fLibraryHandle);
  
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Initialize Plugin
  - Return true if plugin is correctly initialized.
  - Return false if the library is not the right kind of plugin or otherwise
    not supported.
  - This method is called before the plugin is added to the PluginHost list. If
    false is returned, it won't get added to the list.
-------------------------------------------------------------------------------}
function TCCPlugin.InitializePlugin(AHost: TCCPluginHost): Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  Finalize Plugin
  - This method will be called before the plugin is freed from memory.
-------------------------------------------------------------------------------}
procedure TCCPlugin.FinalizePlugin(AHost: TCCPluginHost);
begin

end;

{##############################################################################}
// TCCPluginHost

{-------------------------------------------------------------------------------
  Create an instance of TCCPluginHost
-------------------------------------------------------------------------------}
constructor TCCPluginHost.Create;
begin
  inherited Create;
  fPlugins:= TObjectList.Create(false);
end;

{-------------------------------------------------------------------------------
  Destroy TCCPluginHost
-------------------------------------------------------------------------------}
destructor TCCPluginHost.Destroy;
begin
  UnloadAll;
  fPlugins.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Load Library
  - Loads library into memory.
  - Returns library index if successful, otherwise -1 is returned.
-------------------------------------------------------------------------------}
function TCCPluginHost.LoadPlugin(APluginPath: WideString): Integer;
var
  h: Cardinal;
  i: Integer;
  plug: TCCPlugin;
begin
  Result:= -1;
  try
    {$IFDEF MSWINDOWS}
    h:= LoadLibraryW(PWideChar(APluginPath));
    {$ELSE}
    h:= LoadLibrary(UTF8Encode(APluginPath));
    {$ENDIF}
  except
    // catch exceptions
    Exit;
  end;
  
  if h <> 0 then
  begin
    // check if library is already in the list
    for i:= 0 to fPlugins.Count - 1 do
    begin
      plug:= TCCPlugin(fPlugins.Items[i]);
      if plug.fLibraryHandle = h then
      Exit;
    end;

    // add to list if valid plugin
    plug:= GetPluginClass.Create;
    plug.fLibraryHandle:= h;
    plug.fLibraryPath:= APluginPath;
    if plug.InitializePlugin(Self) then
    Result:= fPlugins.Add(plug)
    else
    begin
      plug.FinalizePlugin(Self);
      plug.Free;
      FreeLibrary(h);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Load Plugins From Folder
  - Returns the number of plugins loaded.
  - If APluginExtension = '' then default extensions are used: .dll in Windows
    and .so in Linux.
  - APluginExtension is case insensitive.
  - NOTICE, the dot before extension should be included in APluginExtension!
-------------------------------------------------------------------------------}
function TCCPluginHost.LoadPluginsFromFolder(AFolderPath: WideString;
    AIncludeSubFolders: Boolean = false; APluginExtension: WideString = ''):
    Integer;
var
  attr: Integer;

  procedure EnumFolder(APath: WideString);
  var
    sr: TCCSearchRec;
  begin
    if WideFindFirst(APath + '*', attr, sr) = S_OK then
    begin
      try
        repeat
          if (sr.Name <> '.') and (sr.Name <> '..') then
          begin
            // enum sub folder
            if AIncludeSubFolders and ((sr.Attr and ccFileUtils.faDirectory) = ccFileUtils.faDirectory) then
            EnumFolder(APath + sr.Name + PathDelim)
            // add plugin
            else
            begin
              if ccStrings.WideIsSameText(WideExtractFileExt(sr.Name), APluginExtension) then
              begin
                if LoadPlugin(APath + sr.Name) > -1 then
                Result:= Result + 1;
              end;
            end;
          end;
        until WideFindNext(sr) <> S_OK;        
      finally
        WideFindClose(sr);
      end;
    end;
  end;

begin
  // init values
  Result:= 0;
  AFolderPath:= WideIncludeTrailingPathDelimiter(AFolderPath);
  if APluginExtension = '' then
  begin
    {$IFDEF MSWINDOWS}
    APluginExtension:= '.dll';
    {$ELSE}
    APluginExtension:= '.so';
    {$ENDIF}
  end;
  
  if AIncludeSubFolders then
  attr:= faAnyFile and faDirectory
  else
  attr:= faAnyFile;

  EnumFolder(AFolderPath);
end;

{-------------------------------------------------------------------------------
  UnloadAll
  - Unloads's all plugins.
-------------------------------------------------------------------------------}
procedure TCCPluginHost.UnloadAll;
var
  i: Integer;
begin
  for i:= 0 to fPlugins.Count - 1 do
  begin
    TCCPlugin(fPlugins.Items[i]).FinalizePlugin(Self);
    fPlugins.Items[i].Free;
  end;
  fPlugins.Clear;
end;

{-------------------------------------------------------------------------------
  Get Count
-------------------------------------------------------------------------------}
function TCCPluginHost.GetCount: Integer;
begin
  Result:= fPlugins.Count;
end;

{-------------------------------------------------------------------------------
  Get Plugins
-------------------------------------------------------------------------------}
function TCCPluginHost.GetPlugins(Index: Integer): TCCPlugin;
begin
  Result:= TCCPlugin(fPlugins.Items[Index]);
end;

{-------------------------------------------------------------------------------
  Get Plugin Class
-------------------------------------------------------------------------------}
function TCCPluginHost.GetPluginClass: TCCPluginClass;
begin
  Result:= TCCPlugin;
end;

{-------------------------------------------------------------------------------
  Index Of
-------------------------------------------------------------------------------}
function TCCPluginHost.IndexOf(APlugin: TCCPlugin): Integer;
begin
  Result:= fPlugins.IndexOf(APlugin);
end;

{-------------------------------------------------------------------------------
  Unload Plugin
-------------------------------------------------------------------------------}
procedure TCCPluginHost.UnloadPlugin(Index: Integer);
begin
  if (Index >= 0) and (Index < fPlugins.Count) then
  begin
    TCCPlugin(fPlugins.Items[Index]).FinalizePlugin(Self);
    fPlugins.Items[Index].Free;
    fPlugins.Delete(Index);
  end;
end;

end.
