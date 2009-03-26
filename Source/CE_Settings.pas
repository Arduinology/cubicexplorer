//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
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
//  The Original Code is CE_Settings.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Settings;

interface

uses
  // CE Units
  CE_SettingsStorage, CE_SettingsIntf,
  // Tnt
  TntSysUtils,
  // System Units
  SysUtils, Classes, Contnrs;

type
  TCEGlobalSettings = class(TObject)
  private
    fGlobalSettings: TInterfaceList;
    fStorage: TCESettingsStorage;
    fStorageIntf: ICESettingsStorage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFilePath: WideString);
    procedure ReadGlobalSettings;
    procedure WriteSettingTo(Handler: ICESettingsHandler);
    procedure WriteGlobalSettings;
    procedure RegisterHandler(Obj: IInterface);
    procedure UnRegisterHandler(Obj: IInterface);
    procedure SaveToFile(AFilePath: WideString);
    procedure ReadSettingsFrom(Handler: ICESettingsHandler);
    property Storage: TCESettingsStorage read fStorage;
    property StorageIntf: ICESettingsStorage read fStorageIntf;
  end;

var
  GlobalSettings: TCEGlobalSettings;  

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEGlobalSettings
-------------------------------------------------------------------------------}
constructor TCEGlobalSettings.Create;
begin
  fStorage:= TCESettingsStorage.Create;
  fStorageIntf:= fStorage as ICESettingsStorage;
  fGlobalSettings:= TInterfaceList.Create;
end;

{*------------------------------------------------------------------------------
  Destroy TCEGlobalSettings
-------------------------------------------------------------------------------}
destructor TCEGlobalSettings.Destroy;
begin
  fGlobalSettings.Free;
  fStorageIntf:= nil;
  inherited;
end;

{-------------------------------------------------------------------------------
  Load from file
-------------------------------------------------------------------------------}
procedure TCEGlobalSettings.LoadFromFile(AFilePath: WideString);
begin
  if WideFileExists(AFilePath) then
  fStorage.LoadFromFile(AFilePath);
end;

{-------------------------------------------------------------------------------
  Read Global Settings
-------------------------------------------------------------------------------}
procedure TCEGlobalSettings.ReadGlobalSettings;
var
  i: Integer;
begin
  for i:= 0 to fGlobalSettings.Count - 1 do
  begin
    (fGlobalSettings.Items[i] as ICESettingsHandler).SaveToStorage(StorageIntf);
  end;
end;

{-------------------------------------------------------------------------------
  Read settings to single handler
-------------------------------------------------------------------------------}
procedure TCEGlobalSettings.WriteSettingTo(Handler: ICESettingsHandler);
begin
  if assigned(Handler) then
  Handler.LoadFromStorage(StorageIntf);
end;

{-------------------------------------------------------------------------------
  Write Global settings
-------------------------------------------------------------------------------}
procedure TCEGlobalSettings.WriteGlobalSettings;
var
  i: Integer;
begin
  for i:= 0 to fGlobalSettings.Count - 1 do
  begin
    (fGlobalSettings.Items[i] as ICESettingsHandler).LoadFromStorage(StorageIntf);
  end;
end;

{*------------------------------------------------------------------------------
  Register Global setting
-------------------------------------------------------------------------------}
procedure TCEGlobalSettings.RegisterHandler(Obj: IInterface);
begin
  fGlobalSettings.Add(Obj);
end;

{*------------------------------------------------------------------------------
  UnRegister Global setting
-------------------------------------------------------------------------------}
procedure TCEGlobalSettings.UnRegisterHandler(Obj: IInterface);
begin
  fGlobalSettings.Remove(Obj);
end;

{-------------------------------------------------------------------------------
  Save to file
-------------------------------------------------------------------------------}
procedure TCEGlobalSettings.SaveToFile(AFilePath: WideString);
begin
  fStorage.SaveToFile(AFilePath);
end;

{-------------------------------------------------------------------------------
  Write settings from single handler
-------------------------------------------------------------------------------}
procedure TCEGlobalSettings.ReadSettingsFrom(Handler:
    ICESettingsHandler);
begin
  if assigned(Handler) then
  Handler.SaveToStorage(StorageIntf);
end;

{##############################################################################}

initialization
  GlobalSettings:= TCEGlobalSettings.Create;

finalization
  if assigned(GlobalSettings) then
  FreeAndNil(GlobalSettings);
  
end.
