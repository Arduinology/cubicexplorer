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
//  The Original Code is Main.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit Main;

interface

uses
  // CE Frames   
  fCE_ExtAppPage, fCE_FiltersPanel, fCE_BookmarkPropDlg, 
  fCE_DockHostForm, fCE_DockableForm, fCE_TabPage, fCE_FileView,
  fCE_FolderPanel, fCE_QuickViewPanel, fCE_BookmarkPanel, fCE_ToolbarCustomizer,
  fCE_PoEditor,
  // CE Data Modules
  dCE_Actions, dCE_Images, dCE_Input,
  // CE Units
  CE_Layout, CE_Utils, CE_GlobalCtrl, CE_AddressToolbar,
  CE_DriveBar, CE_BookmarkBar, CE_StatusBar, CE_VistaFuncs, CE_Breadcrumb,
  CE_ToolbarButtons, CE_TBActions, CE_LanguageCodes, CE_LanguageEngine,
  CE_LanguageUtils, CE_SettingsIntf, CE_Settings, CE_Sessions, CE_SpTabBar,
  // Toolbar2000
  TB2Dock, TB2Item, TB2Toolbar, TB2ToolWindow, TB2ExtItems,
  // SpTBX
  SpTBXItem, SpTBXEditors, SpTBXTabs, SpTBXControls, SpTBXSkins,
  // VSTools
  VirtualShellNewMenu, EasyListview, VirtualExplorerEasyListview,
  MPCommonObjects, VirtualShellNotifier, VirtualResources, MPShellTypes,
  VirtualTrees, MPShellUtilities,
  // Tnt Controls
  TntSystem, TntActnList, TntClasses, TntMenus, TntStdCtrls, TntSysUtils,
  TntDialogs, TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ShellAPI, Menus, ShlObj, XPMan, ActiveX,
  ImgList, Registry, AppEvnts;

type
  TMainForm = class(TTntForm, ICESettingsHandler)
    TopToolDock: TSpTBXDock;
    RightToolDock: TSpTBXDock;
    BottomToolDock: TSpTBXDock;
    LeftToolDock: TSpTBXDock;
    MainToolbar: TSpTBXToolbar;
    MainPanel: TPanel;
    fileMenuItem: TSpTBXSubmenuItem;
    editMenuItem: TSpTBXSubmenuItem;
    viewMenuItem: TSpTBXSubmenuItem;
    toolsMenuItem: TSpTBXSubmenuItem;
    helpMenuItem: TSpTBXSubmenuItem;
    ViewToolbar: TSpTBXToolbar;
    SpTBXItem3: TSpTBXItem;
    SpTBXSubmenuItem6: TSpTBXSubmenuItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXItem6: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SpTBXItem7: TSpTBXItem;
    SpTBXItem8: TSpTBXItem;
    SpTBXItem9: TSpTBXItem;
    SpTBXItem17: TSpTBXItem;
    SpTBXItem19: TSpTBXItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXItem22: TSpTBXItem;
    SpTBXItem23: TSpTBXItem;
    SpTBXItem24: TSpTBXItem;
    SpTBXItem25: TSpTBXItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    toolbarsMenuItem: TSpTBXSubmenuItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    SpTBXSubmenuItem9: TSpTBXSubmenuItem;
    SpTBXItem18: TSpTBXItem;
    SpTBXItem26: TSpTBXItem;
    SpTBXItem27: TSpTBXItem;
    SpTBXItem28: TSpTBXItem;
    SpTBXItem29: TSpTBXItem;
    SpTBXItem30: TSpTBXItem;
    SpTBXItem31: TSpTBXItem;
    SpTBXItem32: TSpTBXItem;
    SpTBXItem33: TSpTBXItem;
    SpTBXItem34: TSpTBXItem;
    NavigationToolbar: TSpTBXToolbar;
    SpTBXItem36: TSpTBXItem;
    SpTBXItem37: TSpTBXItem;
    SpTBXItem38: TSpTBXItem;
    SpTBXItem39: TSpTBXItem;
    SpTBXItem40: TSpTBXItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    SpTBXItem41: TSpTBXItem;
    SpTBXItem42: TSpTBXItem;
    SpTBXItem43: TSpTBXItem;
    SpTBXSeparatorItem12: TSpTBXSeparatorItem;
    SpTBXSeparatorItem13: TSpTBXSeparatorItem;
    SpTBXSeparatorItem14: TSpTBXSeparatorItem;
    SpTBXItem48: TSpTBXItem;
    SpTBXItem49: TSpTBXItem;
    bookmarkMenuItem: TSpTBXSubmenuItem;
    SpTBXItem54: TSpTBXItem;
    SpTBXItem55: TSpTBXItem;
    SpTBXSeparatorItem16: TSpTBXSeparatorItem;
    AlphaSubmenuItem: TSpTBXSubmenuItem;
    SpTBXItem57: TSpTBXItem;
    SpTBXItem58: TSpTBXItem;
    SpTBXItem59: TSpTBXItem;
    SpTBXItem60: TSpTBXItem;
    SpTBXItem61: TSpTBXItem;
    SpTBXItem62: TSpTBXItem;
    SpTBXItem63: TSpTBXItem;
    SpTBXItem64: TSpTBXItem;
    SpTBXItem65: TSpTBXItem;
    SpTBXItem66: TSpTBXItem;
    EditToolbar: TSpTBXToolbar;
    SpTBXItem67: TSpTBXItem;
    SpTBXItem68: TSpTBXItem;
    SpTBXItem69: TSpTBXItem;
    SpTBXItem70: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SpTBXItem71: TSpTBXItem;
    SpTBXItem44: TSpTBXItem;
    SpTBXSeparatorItem11: TSpTBXSeparatorItem;
    SpTBXItem35: TSpTBXItem;
    SpTBXItem50: TSpTBXItem;
    SpTBXItem45: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXItem46: TSpTBXItem;
    SpTBXItem53: TSpTBXItem;
    ToolbarPopupMenu: TSpTBXPopupMenu;
    SpTBXItem52: TSpTBXItem;
    SpTBXSeparatorItem17: TSpTBXSeparatorItem;
    SpTBXItem56: TSpTBXItem;
    SpTBXItem74: TSpTBXItem;
    test_act1: TSpTBXItem;
    StartUpTimer: TTimer;
    SpTBXItem75: TSpTBXItem;
    test_sep1: TSpTBXSeparatorItem;
    SpTBXItem79: TSpTBXItem;
    SpTBXItem80: TSpTBXItem;
    SpTBXSeparatorItem19: TSpTBXSeparatorItem;
    SpTBXItem81: TSpTBXItem;
    LanguageMenuItem: TSpTBXSubmenuItem;
    SpTBXSeparatorItem20: TSpTBXSeparatorItem;
    SpTBXItem83: TSpTBXItem;
    SkinGroupItem: TSpTBXSkinGroupItem;
    SpTBXSeparatorItem21: TSpTBXSeparatorItem;
    SpTBXItem82: TSpTBXItem;
    SpTBXItem84: TSpTBXItem;
    sessionsMenuItem: TSpTBXSubmenuItem;
    SpTBXItem47: TSpTBXItem;
    SpTBXItem85: TSpTBXItem;
    SpTBXSeparatorItem22: TSpTBXSeparatorItem;
    SpTBXItem87: TSpTBXItem;
    SpTBXItem88: TSpTBXItem;
    SpTBXSeparatorItem23: TSpTBXSeparatorItem;
    SpTBXItem51: TSpTBXItem;
    SpTBXSeparatorItem15: TSpTBXSeparatorItem;
    SpTBXSeparatorItem18: TSpTBXSeparatorItem;
    SpTBXItem72: TSpTBXItem;
    SpTBXItem73: TSpTBXItem;
    SpTBXItem76: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    SpTBXItem77: TSpTBXItem;
    SpTBXItem78: TSpTBXItem;
    SpTBXItem86: TSpTBXItem;
    SpTBXItem10: TSpTBXItem;
    SpTBXItem11: TSpTBXItem;
    TabPopupMenu: TSpTBXPopupMenu;
    SpTBXItem12: TSpTBXItem;
    SpTBXItem13: TSpTBXItem;
    SpTBXItem14: TSpTBXItem;
    SpTBXSeparatorItem24: TSpTBXSeparatorItem;
    SpTBXItem15: TSpTBXItem;
    SpTBXItem16: TSpTBXItem;
    SpTBXItem89: TSpTBXItem;
    MainMenuPopupMenu: TSpTBXPopupMenu;
    SpTBXItem90: TSpTBXItem;
    ApplicationEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TransparencyClick(Sender: TObject);
    procedure TransparencyPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure LanguageMenuItemPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure SpTBXItem45Click(Sender: TObject);
    procedure SpTBXItem46Click(Sender: TObject);
    procedure StartUpTimerTimer(Sender: TObject);
    procedure SpTBXItem75Click(Sender: TObject);
    procedure SpTBXItem80Click(Sender: TObject);
    procedure SpTBXItem85Click(Sender: TObject);
    procedure test_act1Click(Sender: TObject);
  private
    fFullscreen: Boolean;
    fActiveLanguage: WideString;
    fLanguageList: TTntStrings;
    fOldWindowState: TWindowState;
    fPathInTitle: Boolean;
    fSingleInstance: Boolean;
    fUpdatingCount: Integer;
    procedure SetFullscreen(const Value: Boolean);
    procedure SetActiveLanguage(const Value: WideString);
    procedure SetPathInTitle(const Value: Boolean);
    procedure SetSingleInstance(const Value: Boolean);
  protected
    fIsReady: Boolean;
    procedure ForceIconCachRebuild;
    procedure GetLanguageList;
    procedure GetSkinsFromFolder(AFolderPath: WideString);
    procedure ConvertCustomActions(Root: TTBCustomItem; Recursive: Boolean = true);
    procedure InitLanguage;
    procedure LanguageItemClick(Sender: TObject);
    procedure WMDeviceChange(var Message: TMessage); message WM_DEVICECHANGE;
    procedure WMPowerBroadcast(var Message: TMessage); message WM_POWERBROADCAST;
    procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
  public
    DockHostForm: TCEDockHostForm;
    Layouts: TCELayoutController;
    TabSet: TCESpTabSet;
    AddressBarToolbar: TCEAddressBarToolbar;
    BreadcrumbBar: TCEBreadcrumbBar;
    DriveToolbar: TCEDriveToolbar;
    BookmarkToolbar: TCEBookmarkToolbar;
    SessionsToolbar: TCESessionsToolbar;
    StatusBar: TCEStatusBar;
    procedure BeginUIUpdate;
    procedure EndUIUpdate;
    procedure InitializeUI;
    procedure FinalizeUI;
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure MakeVisible;
    procedure MenuItemTranslateHandler(Obj:TObject; var IsIgnored: Boolean);
    procedure OpenSkin;
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
    procedure Shutdown;
    procedure StartUp;
    procedure TranslateUI(Sender: TObject);
    property Fullscreen: Boolean read fFullscreen write SetFullscreen;
    property ActiveLanguage: WideString read fActiveLanguage write
        SetActiveLanguage;
    property PathInTitle: Boolean read fPathInTitle write SetPathInTitle;
    property SingleInstance: Boolean read fSingleInstance write SetSingleInstance;
  end;

var
  MainForm: TMainForm;

implementation

uses
  madExcept, CE_QuickView, Clipbrd;

{$R *.dfm}

{*------------------------------------------------------------------------------
  BeginUIUpdate
-------------------------------------------------------------------------------}
procedure TMainForm.BeginUIUpdate;
begin
  fUpdatingCount:= fUpdatingCount + 1;
  if fUpdatingCount > 1 then
  Exit;
  LockWindowUpdate(GetDesktopWindow);
  SendMessage(MainForm.DockHostForm.Handle, WM_SETREDRAW, 0,0);
  Self.TopToolDock.BeginUpdate;
  Self.RightToolDock.BeginUpdate;
  Self.BottomToolDock.BeginUpdate;
  Self.LeftToolDock.BeginUpdate;
  Self.MainToolbar.BeginUpdate;
  DockHostForm.TopPageToolDock.BeginUpdate;
  DockHostForm.BottomPageToolDock.BeginUpdate;
  DockHostForm.LeftPageToolDock.BeginUpdate;
  DockHostForm.RightPageToolDock.BeginUpdate;
end;

{*------------------------------------------------------------------------------
  EndUIUpdate
-------------------------------------------------------------------------------}
procedure TMainForm.EndUIUpdate;
begin
  if fUpdatingCount = 0 then
  Exit;
  fUpdatingCount:= fUpdatingCount - 1;
  if fUpdatingCount > 0 then
  Exit;

  DockHostForm.DockServer.LeftDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.TopDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.BottomDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.RightDockPanel.DockManager.ResetBounds(true);

  Self.TopToolDock.EndUpdate;
  Self.RightToolDock.EndUpdate;
  Self.BottomToolDock.EndUpdate;
  Self.LeftToolDock.EndUpdate;
  Self.MainToolbar.EndUpdate;
  DockHostForm.TopPageToolDock.EndUpdate;
  DockHostForm.BottomPageToolDock.EndUpdate;
  DockHostForm.LeftPageToolDock.EndUpdate;
  DockHostForm.RightPageToolDock.EndUpdate;
  LockWindowUpdate(0);
  SendMessage(MainForm.DockHostForm.Handle, WM_SETREDRAW, 1,0);
  RedrawWindow(MainForm.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

{*------------------------------------------------------------------------------
  Get's called when MainForm is created.
-------------------------------------------------------------------------------}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  ChangeNotifier.RegisterShellChangeNotify(Self);
  fLanguageList:= TTntStringList.Create;
  fLanguageList.NameValueSeparator:= '=';
  fIsReady:= false;
  fUpdatingCount:= 0;
  fPathInTitle:= false;
  SetVistaFont(Self.Font);
  GlobalSettings.RegisterHandler(Self);
end;

{*------------------------------------------------------------------------------
  Get's called when MainForm is destroyed.
-------------------------------------------------------------------------------}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ChangeNotifier.UnRegisterShellChangeNotify(Self);
  fIsReady:= false;
  CEActions.UpdateTimer.Enabled:= false;
  FinalizeUI;
  fLanguageList.Free;
end;

{*------------------------------------------------------------------------------
  Initialize MainForm UI.
-------------------------------------------------------------------------------}
procedure TMainForm.InitializeUI;
begin
  SetDesktopIconFonts(Font);
  // Create Data Modules
  CEActions:= TCEActions.Create(self);
  CE_Images:= TCE_Images.Create(self);
  CEInput:= TCEInput.Create(self);

  // Create/Init layout controller
  Layouts:= TCELayoutController.Create(self);
  // Create/Init DockingFrame
  DockHostForm:= TCEDockHostForm.Create(self);
  DockHostForm.Name:= 'DockHost';
  DockHostForm.Parent:= MainPanel;
  DockHostForm.Align:= alClient;
  DockHostForm.Show;
  //BeginUIUpdate;

  // Setup MainMenuPopup
  MainMenuPopupMenu.LinkSubitems:= MainToolbar.Items;

  // Create TabSet
  TabSet:= TCESpTabSet.Create(nil);
  TabSet.Parent:= MainPanel;
  TabSet.Align:= alTop;
  TabSet.Toolbar.Name:= 'TabBar';
  TabSet.Toolbar.Caption:= _('Tabs');
  TabSet.TabDragReorder:= true;
  TabSet.TabPageHost:= DockHostForm.PageHostPanel;
  TabSet.LayoutController:= Layouts;
  TabSet.Toolbar.Customizable:= true;
  TabSet.Images:= CE_Images.SmallIcons;
  TabSet.TabPopupMenu:= TabPopupMenu;
  GlobalFocusCtrl.CtrlList.Add(TabSet.Toolbar);
  //TCESpTabToolbar(TabSet.Toolbar).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;

  // Create FolderCombo Toolbar
  AddressBarToolbar:= TCEAddressBarToolbar.Create(self);
  AddressBarToolbar.Name:= 'AddressBarToolbar';
  AddressBarToolbar.Caption:= _('Address Bar');
  AddressBarToolbar.DockableTo:= [TB2Dock.dpTop, TB2Dock.dpBottom];
  AddressBarToolbar.Visible:= false;
  AddressBarToolbar.CurrentDock:= DockHostForm.TopPageToolDock;
  AddressBarToolbar.Tag:= 1;
  // Create DriveToolbar
  DriveToolbar:= TCEDriveToolbar.Create(self);
  DriveToolbar.Name:= 'DriveToolbar';
  DriveToolbar.Caption:= _('Drives');
  DriveToolbar.Stretch:= true;
  DriveToolbar.ChevronMoveItems:= false;
  DriveToolbar.Visible:= false;
  DriveToolbar.CurrentDock:= TopToolDock;
  DriveToolbar.Populate;
  DriveToolbar.PopupMenu:= ToolbarPopupMenu;
  DriveToolbar.Tag:= 1;
  // Create BookmarkBar
  BookmarkToolbar:= TCEBookmarkToolbar.Create(self);
  BookmarkToolbar.Name:= 'BookmarkToolbar';
  BookmarkToolbar.Caption:= _('Bookmarks');
  BookmarkToolbar.Stretch:= true;
  BookmarkToolbar.ChevronMoveItems:= false;
  BookmarkToolbar.Visible:= false;
  BookmarkToolbar.CurrentDock:= TopToolDock;
  BookmarkToolbar.PopupMenu:= ToolbarPopupMenu;
  BookmarkToolbar.Tag:= 1;
  // Create SessionsBar
  SessionsToolbar:= TCESessionsToolbar.Create(self);
  SessionsToolbar.Name:= 'SessionsToolbar';
  SessionsToolbar.Caption:= _('Sessions');
  SessionsToolbar.Stretch:= true;
  SessionsToolbar.ChevronMoveItems:= false;
  SessionsToolbar.Visible:= false;
  SessionsToolbar.CurrentDock:= TopToolDock;
  SessionsToolbar.PopupMenu:= ToolbarPopupMenu;
  SessionsToolbar.Tag:= 1;
  // Create BreadcrumbBar
  BreadcrumbBar:= TCEBreadcrumbBar.Create(self);
  BreadcrumbBar.Name:= 'BreadcrumbBar';
  BreadcrumbBar.Caption:= _('Breadcrumb Navigation');
  BreadcrumbBar.Stretch:= true;
  BreadcrumbBar.Visible:= false;
  BreadcrumbBar.DockableTo:= [TB2Dock.dpTop, TB2Dock.dpBottom];
  BreadcrumbBar.CurrentDock:= DockHostForm.TopPageToolDock;
  BreadcrumbBar.Tag:= 1;
  // Create Status bar
  StatusBar:= TCEStatusBar.Create(Self);
  StatusBar.Parent:= Self;
  StatusBar.Initialize;
  StatusBar.PopupMenu:= MainMenuPopupMenu;
  GlobalPathCtrl.RegisterNotify(StatusBar);
  // Add Layout items
  CELayoutItems.Add(MainToolbar);
  CELayoutItems.Add(NavigationToolbar);
  CELayoutItems.Add(ViewToolbar);
  CELayoutItems.Add(AddressBarToolbar);
  CELayoutItems.Add(DriveToolbar);
  CELayoutItems.Add(BookmarkToolbar);
  CELayoutItems.Add(SessionsToolbar);
  CELayoutItems.Add(EditToolbar);
  CELayoutItems.Add(BreadcrumbBar);
  CELayoutItems.Add(TabSet);
  // Add Toolbar Docks
  CEToolbarDocks.Add(LeftToolDock);
  CEToolbarDocks.Add(TopToolDock);
  CEToolbarDocks.Add(RightToolDock);
  CEToolbarDocks.Add(BottomToolDock);
  CEToolbarDocks.Add(DockHostForm.LeftPageToolDock);
  CEToolbarDocks.Add(DockHostForm.TopPageToolDock);
  CEToolbarDocks.Add(DockHostForm.RightPageToolDock);
  CEToolbarDocks.Add(DockHostForm.BottomPageToolDock);
  // Populate menu items
  CELayoutItems.PopulateMenuItem(toolbarsMenuItem);
  CELayoutItems.PopulateMenuItem(ToolbarPopupMenu.Items);

  sessionsMenuItem.Add(TCESessionsMenuItem.Create(self));


  // Add custom menu items
  MainToolbar.BeginUpdate;
  try
    ConvertCustomActions(MainToolbar.Items);
  finally
    MainToolbar.EndUpdate;
  end;  

  // Enable transparency control?
  case GetWinVersion of
    wvWin2000, wvWinXP, wvWin2003, wvWinVista: AlphaSubmenuItem.Visible:= true;
  end;

  InitLanguage;
end;

{*------------------------------------------------------------------------------
  Finalize MainForm UI.
-------------------------------------------------------------------------------}
procedure TMainForm.FinalizeUI;
begin
  TabSet.Free;
  Layouts.Free;
  DockHostForm.Free;
end;

{-------------------------------------------------------------------------------
  ForceIconCachRebuild
-------------------------------------------------------------------------------}
procedure TMainForm.ForceIconCachRebuild;
var
  Reg: TRegistry;
  LargeIconSize: integer;
begin
  Reg := TRegistry.Create;
  try
    try
      Reg.Access:= KEY_READ or KEY_WRITE;
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Control Panel\Desktop\WindowMetrics', False) then
      begin
        FlushImageLists;
        { Flush the Icon Cache by changing the size of the icons }
        if Reg.ValueExists('Shell Icon Size') then
          LargeIconSize := StrToInt(Reg.ReadString('Shell Icon Size'))
        else
          LargeIconSize := GetSystemMetrics(SM_CXICON);
        Reg.WriteString('Shell Icon Size', IntToStr(LargeIconSize + 1));
        SendMessage(Handle, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS, Integer(PChar('WindowMetrics')));
        FileIconInit(True); // Flush the cached Icons
        Reg.WriteString('Shell Icon Size', IntToStr(LargeIconSize));
        SendMessage(Handle, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS, Integer(PChar('WindowMetrics')));
        FileIconInit(True); // Flush the cached Icons
      end;
    except // Quiet failure
    end
  finally
    Reg.Free;
  end
end;

{*------------------------------------------------------------------------------
  Load default startup settings.
-------------------------------------------------------------------------------}
procedure TMainForm.StartUp;

  procedure enumToolbar(item: TSpTBXItem);
  var
    i2: Integer;
    chItem: TSpTBXItem;
  begin
    for i2:= 0 to item.Count - 1 do
    begin
      if item.Items[i2] is TSpTBXItem then
      begin
        chItem:= TSpTBXItem(item.Items[i2]);
        chItem.Caption:= '&' + chItem.Caption;
        if chItem.ClassType = TSpTBXSubmenuItem then
        enumToolbar(chItem);
      end;
    end;
  end;

var
  i: Integer;
  TabsOpened: Boolean;
  ws: WideString;
begin
  // Load skins
  GetSkinsFromFolder(ExePath + 'Skins\');
  SkinGroupItem.Recreate;

  // Load Sessions
  GlobalSessions.LoadFromFile(exePath + 'sessions.xml', '/');

  // Load Settings
  GlobalSettings.LoadFromFile(exePath + 'settings.xml');
  GlobalSettings.WriteGlobalSettings;

  // Load Bookmarks
  CEBookmarkPanel.BookmarksPath:= ExePath + 'bookmarks.xml';
  CEBookmarkPanel.BookmarkMenuItems.Add(BookmarkToolbar.Items);
  CEBookmarkPanel.BookmarkMenuItems.Add(bookmarkMenuItem);
  CEBookmarkPanel.LoadBookmarks;

  // Load Layouts
  Layouts.AutoSave:= false;
  Layouts.LoadFromFile(ExePath + 'layout.xml');
  Layouts.LoadToolbarLayout;

  TabsOpened:= false;
  if WideParamCount > 0 then
  begin
    for i:= 1 to WideParamCount do
    begin
      ws:= ws + WideParamStr(i);
      if i < WideParamCount then
      ws:= ws + ',';
    end;
    TabsOpened:= HandleCmdParams(ws);
  end;

  if not TabsOpened then
  begin
    
    GlobalSessions.LoadActiveSession;
  end;

  // Atleast one tab has to be open
  if TabSet.TabCount = 0 then
  begin
    CEActions.act_tabs_addtab.Execute;
  end;


  if Application.MainForm.AlphaBlendValue < 255 then
  Application.MainForm.AlphaBlend:= true;

  // Start Update timer
  CEActions.UpdateTimer.Enabled:= true;

  fIsReady:= true;



 //test_act1Click(self);



  // Testing stuff!!!
  if DebugHook <> 0 then
  begin
    SpTBXSeparatorItem2.Visible:= true;
    SpTBXItem75.Visible:= true;
    SpTBXItem45.Visible:= true;
    test_sep1.Visible:= true;
    test_act1.Visible:= true;
  end;
  //test_act1.Visible:= true;
end;

{*------------------------------------------------------------------------------
  A delayed Startup code.
-------------------------------------------------------------------------------}
procedure TMainForm.StartUpTimerTimer(Sender: TObject);
begin
  StartUpTimer.Enabled:= false;
  EndUIUpdate;

  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.SetFocus;
end;

{*------------------------------------------------------------------------------
  Get's called before shutdown
-------------------------------------------------------------------------------}
procedure TMainForm.Shutdown;
begin
  GlobalSessions.SaveToFile(exePath + 'sessions.xml');
  
  GlobalSettings.ReadGlobalSettings;
  GlobalSettings.SaveToFile(exePath + 'settings.xml');
  

  Layouts.SaveToolbarLayout;
  Layouts.SaveLayout(Layouts.CurrentLayout);
  Layouts.SaveToFile(ExePath + 'layout.xml');
end;

{*------------------------------------------------------------------------------
  Get's called on MainForm Close.
-------------------------------------------------------------------------------}
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Shutdown;
end;

{*------------------------------------------------------------------------------
  Get's called on MainForm CloseQuery.
-------------------------------------------------------------------------------}
procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  s: TCESession;
begin
  CEActions.UpdateTimer.Enabled:= false;

  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  GlobalFileViewSettings.AssignColumnSettingsFrom(TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView);  

  s:= GlobalSessions.GetActiveSession;
  if assigned(s) then
  begin
    if s.AutoSave then
    GlobalSessions.SaveToSession(s);
  end;

  CanClose:= TabSet.CloseAllTabs;

  if not CanClose then
  CEActions.UpdateTimer.Enabled:= true;
end;

{*------------------------------------------------------------------------------
  On Transparency popup
-------------------------------------------------------------------------------}
procedure TMainForm.TransparencyPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
begin
  if not Self.AlphaBlend then
  Sender.Items[0].Checked:= true
  else
  begin
    case Self.AlphaBlendValue of
      255: Sender.Items[0].Checked:= true;
      229..254: Sender.Items[1].Checked:= true;
      204..228: Sender.Items[2].Checked:= true;
      178..203: Sender.Items[3].Checked:= true;
      153..177: Sender.Items[4].Checked:= true;
      127..152: Sender.Items[5].Checked:= true;
      102..126: Sender.Items[6].Checked:= true;
      76..101: Sender.Items[7].Checked:= true;
      51..75: Sender.Items[8].Checked:= true;
      0..50: Sender.Items[9].Checked:= true;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Set transparency
-------------------------------------------------------------------------------}
procedure TMainForm.TransparencyClick(Sender: TObject);
var
  i: Integer;
begin
  i:= TSpTBXItem(Sender).Tag;
  case i of
    0: begin
         Self.AlphaBlend:= false;
         Self.AlphaBlendValue:= 255;
       end;
    1..9: begin
            Self.AlphaBlendValue:= 255 - Round(25.5 * i);
            Self.AlphaBlend:= true;
          end;
  end;
end;

{*------------------------------------------------------------------------------
  Set Fullscreen
-------------------------------------------------------------------------------}
procedure TMainForm.SetFullscreen(const Value: Boolean);
begin
  fFullscreen:= Value;
  if fFullscreen then
  begin
    fOldWindowState:= Self.WindowState;
    Self.BorderStyle:= bsNone;
    Self.WindowState:= wsMaximized;
  end
  else
  begin
    Self.BorderStyle:= bsSingle;
    Self.WindowState:= fOldWindowState;
  end;
end;

{*------------------------------------------------------------------------------
  Form KeyUp
-------------------------------------------------------------------------------}
procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_TAB) then
  begin
    if Shift = [ssCtrl] then
    TabSet.SelectNextTab(true)
    else if Shift = [ssCtrl,ssShift] then
    TabSet.SelectNextTab(false);
  end
//  else if Shift = [ssAlt] then
//  begin
//    if Key = Ord('D') then
//    begin
//      if AddressBarToolbar.Visible then
//      AddressBarToolbar.AddressBar.SetFocus;
//    end
//  end
  else if Key = VK_F6 then
  begin
    if AddressBarToolbar.Visible then
      if AddressBarToolbar.AddressBar.TextEditor.Visible then
      AddressBarToolbar.AddressBar.TextEditor.SetFocus;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on Form show
-------------------------------------------------------------------------------}
procedure TMainForm.FormShow(Sender: TObject);
begin
  if fUpdatingCount > 1 then
  Exit;
  
  DockHostForm.DockServer.LeftDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.TopDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.BottomDockPanel.DockManager.ResetBounds(true);
  DockHostForm.DockServer.RightDockPanel.DockManager.ResetBounds(true);
end;

{*------------------------------------------------------------------------------
  Get Language List
-------------------------------------------------------------------------------}
procedure TMainForm.GetLanguageList;
var
  i: Integer;
  item: TSpTBXItem;
  sep: TSpTBXSeparatorItem;
begin
  fLanguageList.Clear;
  LanguageMenuItem.Clear;
  // Get Language List
  GetPOLanguageList(exePath + 'Locale\', 'default', fLanguageList);

  // Disable Language menu if there is no translations
  if fLanguageList.Count = 0 then
  begin
    LanguageMenuItem.Enabled:= false;
  end;
  // Add default menu items
  item:= TSpTBXItem.Create(self);
  item.Caption:= _('English (default)');
  item.OnClick:= LanguageItemClick;
  item.Tag:= -1;
  LanguageMenuItem.Add(item);
  sep:= TSpTBXSeparatorItem.Create(self);
  LanguageMenuItem.Add(sep);
  // Loop through language list
  for i:= 0 to fLanguageList.Count - 1 do
  begin
    // Add language menu items
    item:= TSpTBXItem.Create(self);
    item.Caption:= fLanguageList.Names[i];
    item.OnClick:= LanguageItemClick;
    item.Tag:= i;
    LanguageMenuItem.Add(item);
  end;
end;

{*------------------------------------------------------------------------------
  Get Skins From folder
-------------------------------------------------------------------------------}
procedure TMainForm.GetSkinsFromFolder(AFolderPath: WideString);
var
  SearchRec: TSearchRecW;
  list: TTntStrings;
  path: WideString;
  i: Integer;
begin
  AFolderPath:= WideIncludeTrailingPathDelimiter(AFolderPath);
  if WideDirectoryExists(AFolderPath) then
  begin
    list:= TTntStringList.Create;
    try
      path:= AFolderPath + '*.skn';
      if WideFindFirst(Path, faAnyFile, SearchRec) = 0 then
      begin
        try
          repeat
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            list.Add(SearchRec.Name);
          until WideFindNext(SearchRec) <> 0;
        finally
          WideFindClose(SearchRec);
        end;
      end;
      for i:= 0 to List.Count - 1 do
      begin
        path:= AFolderPath + list.Strings[i];
        if WideFileExists(path) then
        SkinManager.SkinsList.AddSkinFromFile(path);
      end;
    finally
      list.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Convert custom toolbar actions
-------------------------------------------------------------------------------}
procedure TMainForm.ConvertCustomActions(Root: TTBCustomItem; Recursive:
    Boolean = true);

  procedure EnumItem(Item: TTBCustomItem);
  var
    chItem, newItem: TTBCustomItem;
    i, index: Integer;
  begin
    if assigned(Item) then
    begin
      for i:= 0 to Item.Count - 1 do
      begin
        chItem:= Item.Items[i];
        if chItem.Action is TCEToolbarAction then
        begin
          newItem:= TCEToolbarAction(chItem.Action).ItemClass.Create(Item);
          newItem.Action:= chItem.Action;
          index:= Item.IndexOf(chItem);
          FreeAndNil(chItem);
          Item.Insert(index, newItem);
        end;
        if Recursive then
        EnumItem(chItem);
      end;
    end;
  end;

begin
  EnumItem(Root);
end;

{*------------------------------------------------------------------------------
  Initialize Language stuff
-------------------------------------------------------------------------------}
procedure TMainForm.InitLanguage;
begin
  GetLanguageList;
  // Add Ignore items
  CEGlobalTranslator.RegisterIgnoredClass(TFont);
  CEGlobalTranslator.UseIncludeList:= true;
  CEGlobalTranslator.IncludeInheritedAlso:= true;
  CEGlobalTranslator.IncludeClasses.Add(TTntAction);
  CEGlobalTranslator.IncludeClasses.Add(TSpTBXItem);
  CEGlobalTranslator.IncludeClasses.Add(TTntMenuItem);
  CEGlobalTranslator.IncludeClasses.Add(TSpTBXTabItem);
  CEGlobalTranslator.IncludeClasses.Add(TTntLabel);
  CEGlobalTranslator.IncludeClasses.Add(TTntButton);
  CEGlobalTranslator.IncludeClasses.Add(TTntCheckBox);
  CEGlobalTranslator.IncludeClasses.Add(TTntGroupBox);
  CEGlobalTranslator.IncludeClasses.Add(TSpTBXButton);
  CEGlobalTranslator.IncludeClasses.Add(TForm);
  CEGlobalTranslator.IncludeClasses.Add(TTBCustomDockableWindow);
  CEGlobalTranslator.IncludeClasses.Add(TTntRadioButton);

  CEGlobalTranslator.IgnoredProperties.Add('HelpKeyword');
  CEGlobalTranslator.IgnoredProperties.Add('ImeName');
  CEGlobalTranslator.IgnoredProperties.Add('Name');
  CEGlobalTranslator.RegisterIgnoredClassHandler(TSpTBXItem, MenuItemTranslateHandler);
end;

{*------------------------------------------------------------------------------
  Get's called when language menu is opened
-------------------------------------------------------------------------------}
procedure TMainForm.LanguageMenuItemPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  i: Integer;
  id: Integer;
begin
  GEtLanguageList;
  id:= fLanguageList.IndexOfName(ActiveLanguage);
  for i:= 0 to LanguageMenuItem.Count - 1 do
  begin
    if id = LanguageMenuItem.Items[i].Tag then
    LanguageMenuItem.Items[i].Checked:= true
    else
    LanguageMenuItem.Items[i].Checked:= false;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when language menu item is clicked
-------------------------------------------------------------------------------}
procedure TMainForm.LanguageItemClick(Sender: TObject);
var
  i: Integer;
begin
  i:= TTBItem(Sender).Tag;
  if (i = -1) or (i >= fLanguageList.Count) then
  begin
    ActiveLanguage:= '';
  end
  else
  begin
    ActiveLanguage:= fLanguageList.Names[i];
  end;
end;

{*------------------------------------------------------------------------------
  MenuItem Translate Handler
-------------------------------------------------------------------------------}
procedure TMainForm.MenuItemTranslateHandler(Obj:TObject; var IsIgnored:
    Boolean);
begin
  IsIgnored:= TSpTBXItem(Obj).Action <> nil;  
end;

procedure TMainForm.OpenSkin;
var
  open: TTntOpenDialog;
  i: Integer;
begin
  open:= TTntOpenDialog.Create(nil);
  open.InitialDir:= exePath;
  open.Filter:= 'Skin File|*.skn|All Files|*.*';
  try
    if open.Execute then
    begin
      i:= SkinManager.SkinsList.AddSkinFromFile(open.FileName);
      if I > -1 then
      begin
        SkinManager.SetSkin(SkinManager.SkinsList[i]);
        SkinGroupItem.Recreate;
      end;
    end;
  finally
    open.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Set Language
-------------------------------------------------------------------------------}
procedure TMainForm.SetActiveLanguage(const Value: WideString);
var
  ws: WideString;
begin
  ws:= fLanguageList.Values[Value];
  if ws = '' then
  begin
    CEGlobalTranslator.ResetToOld(false);
    fActiveLanguage:= '';
  end
  else
  begin
    CEGlobalTranslator.LoadPOFromFile(ws);
    TranslateUI(self);
    fActiveLanguage:= Value;
  end;
end;

{*------------------------------------------------------------------------------
  Handle suspend events
-------------------------------------------------------------------------------}
procedure TMainForm.WMPowerBroadcast(var Message: TMessage);
begin
  case Message.WParam of
    PBT_APMSUSPEND: begin
      Layouts.SaveToolbarLayout;
      Layouts.SaveLayout(Layouts.CurrentLayout);
      Layouts.SaveToFile(ExePath + 'layout.xml');
    end;
  end;
  Message.Result:= 1;
  inherited;
end;

{*------------------------------------------------------------------------------
  On Device Change (CD Insert/Eject)
-------------------------------------------------------------------------------}
procedure TMainForm.WMDeviceChange(var Message: TMessage);
begin
  inherited;

  if not fIsReady then
  Exit;

  if (Message.wParam = $8000) or (Message.wParam = $8004) then
  begin
    //CEActions.act_navi_refresh.Execute;
  end;
end;

{-------------------------------------------------------------------------------
  Translate UI
--------------------------------------------------------------------------------}
procedure TMainForm.TranslateUI(Sender: TObject);
begin
  BeginUIUpdate;
  CEGlobalTranslator.TranslatedCount:= 0;

  if Sender is TCEPoEditor then
  begin
    if fActiveLanguage <> '' then
    CEGlobalTranslator.LoadPOFromFile(fLanguageList.Values[fActiveLanguage]);
  end;

  if CEGlobalTranslator.IsTranslated then
  CEGlobalTranslator.ReTranslateAll
  else
  begin
    CEGlobalTranslator.TranslateComponent(Self);
  end;

  EndUIUpdate;
end;

{-------------------------------------------------------------------------------
  Handle Load settings call
--------------------------------------------------------------------------------}
procedure TMainForm.LoadFromStorage(Storage: ICESettingsStorage);
var
  Placement: TWindowPlacement;
  r: TRect;
  s: TCESession;
begin
  Storage.OpenPath('/MainForm');
  try
    // Position
    r.Left:= Storage.ReadInteger('Left',50);
    r.Top:= Storage.ReadInteger('Top',50);
    r.Right:= r.Left + Storage.ReadInteger('Width',640);
    r.Bottom:= r.Top + Storage.ReadInteger('Height',480);
    Placement.Length := SizeOf(TWindowPlacement);
    GetWindowPlacement(MainForm.Handle, @Placement);
    Placement.rcNormalPosition:= r;
    Placement.showCmd:= Storage.ReadInteger('ShowCmd', 1);
    if Placement.showCmd = SW_SHOWMINIMIZED then
    Placement.showCmd:= SW_SHOWNORMAL;
    SetWindowPlacement(MainForm.Handle, @Placement);
    // Alpha
    AlphaBlendValue:= Storage.ReadInteger('AlphaBlend',255);
    // Skin
    SkinManager.SetSkin(Storage.ReadString('Skin','Default'));
    // Language
    ActiveLanguage:= Storage.ReadString('Language', '');
    // Toggles
    fSingleInstance:= Storage.ReadBoolean('SingleInstance', true);
    ShowHint:= Storage.ReadBoolean('ShowHints', true);
    PathInTitle:= Storage.ReadBoolean('PathInTitle', false);
    if Storage.ReadBoolean('AlwaysOnTop', false) then
    MainForm.FormStyle:= fsStayOnTop
    else
    MainForm.FormStyle:= fsNormal;
    // Session to load
    s:= GlobalSessions.FindSession(Storage.ReadString('StartupSession', ''));
    if s = nil then
    s:= GlobalSessions.FindSession(Storage.ReadString('LastSession', ''))
    else
    GlobalSessions.StartupSession:= s.SessionName;

    if s = nil then
    GlobalSessions.ActiveSession:= 'Default'
    else
    GlobalSessions.ActiveSession:= s.SessionName;

  finally
    Storage.ClosePath;
  end;

end;

{-------------------------------------------------------------------------------
  Make MainForm visible
-------------------------------------------------------------------------------}
procedure TMainForm.MakeVisible;
begin
  if IsIconic(Application.Handle) then
  Application.Restore
  else
  Application.BringToFront;
end;

{-------------------------------------------------------------------------------
  Handle Save settings call
--------------------------------------------------------------------------------}
procedure TMainForm.SaveToStorage(Storage: ICESettingsStorage);
var
  Placement: TWindowPlacement;
  r: TRect;
begin
  Storage.OpenPath('/MainForm');
  try
    // Position
    Placement.Length := SizeOf(TWindowPlacement);
    GetWindowPlacement(MainForm.Handle, @Placement);
    r:= Placement.rcNormalPosition;
    Storage.WriteInteger('Left', r.Left);
    Storage.WriteInteger('Top', r.Top);
    Storage.WriteInteger('Width', r.Right - r.Left);
    Storage.WriteInteger('Height', r.Bottom - r.Top);
    Storage.WriteInteger('ShowCmd', Placement.showCmd);
    // Alpha
    Storage.WriteInteger('AlphaBlend', AlphaBlendValue);
    // Skin
    Storage.WriteString('Skin', SkinManager.CurrentSkinName);
    // Language
    Storage.WriteString('Language', ActiveLanguage);
    // Toggles
    Storage.WriteBoolean('SingleInstance', SingleInstance);
    Storage.WriteBoolean('ShowHints', ShowHint);
    Storage.WriteBoolean('PathInTitle', PathInTitle);
    Storage.WriteBoolean('AlwaysOnTop', MainForm.FormStyle = fsStayOnTop);
    // Session
    if GlobalSessions.StartupSession <> '' then
    Storage.WriteString('StartupSession', GlobalSessions.StartupSession)
    else
    Storage.DeletePath('StartupSession');
    Storage.WriteString('LastSession', GlobalSessions.ActiveSession);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Set Path In Title
-------------------------------------------------------------------------------}
procedure TMainForm.SetPathInTitle(const Value: Boolean);
begin
  if fPathInTitle <> value then
  begin
    fPathInTitle:= Value;
    if fPathInTitle then
    begin
      Caption:= GlobalPathCtrl.GlobalPathCaption;
      TntApplication.Title:= Caption;
    end
    else
    begin
      Caption:= 'CubicExplorer';
      TntApplication.Title:= Caption;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set Single Instance
-------------------------------------------------------------------------------}
procedure TMainForm.SetSingleInstance(const Value: Boolean);
begin
  fSingleInstance:= Value;
end;

{*------------------------------------------------------------------------------
  Handle WMShellNotify messages
-------------------------------------------------------------------------------}
procedure TMainForm.WMShellNotify(var Msg: TMessage);
var
  ShellEventList: TVirtualShellEventList;
  ShellEvent: TVirtualShellEvent;
  List: TList;
  i, Count, i2: Integer;
  item: TCESpTabItem;
begin
  ShellEventList:= TVirtualShellEventList(Msg.wParam);
  List:= ShellEventList.LockList;
  try
    Count:= List.Count;
    for i:= 0 to Count - 1 do
    begin
      ShellEvent:= TVirtualShellEvent(List.Items[i]);
      case ShellEvent.ShellNotifyEvent of
        vsneAssoccChanged: begin
          ForceIconCachRebuild;
          Application.ProcessMessages;
          // Rebuild all fileview tabs
          for i2:= 0 to TabSet.Items.Count - 1 do
          begin
            if TabSet.Items.Items[i2] is TCESpTabItem then
            begin
              item:= TCESpTabItem(MainForm.TabSet.Items.Items[i2]);
              if item.Page is TCEFileViewPage then
              begin
                TCEFileViewPage(item.Page).FileView.Rebuild(true);
              end;
            end;
          end;
          // refresh foldertree
          CEFolderPanel.FolderTree.Refresh;
        end;

        vsneDriveRemoved, vsneMediaRemoved: begin
          // Clear all tabs using removed drive
          i2:= 0;
          while i2 < TabSet.Items.Count do
          begin
            if TabSet.Items.Items[i2] is TCESpTabItem then
            begin
              item:= TCESpTabItem(MainForm.TabSet.Items.Items[i2]);
              if item.Page is TCEFileViewPage then
              begin
                if TCEFileViewPage(item.Page).FileView.RootFolderNamespace.IsParentByPIDL(ShellEvent.PIDL1, false) then
                begin
                  if item.CloseTab then
                  begin
                    item.Free;
                    i2:= i2 - 1;
                  end;
                end;
              end;
            end;            
            i2:= i2 + 1;
          end;
        end;
      end;
    end;
  finally
    ShellEventList.UnlockList;
    ShellEventList.Release;
  end;
end;

{##############################################################################}

{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
// Testing stuff below!!!!

procedure TMainForm.SpTBXItem45Click(Sender: TObject);
begin
  raise Exception.Create('Demo crash');
end;

procedure TMainForm.SpTBXItem46Click(Sender: TObject);
var
  page: TCEExtAppTabPage;
begin
  page:= TCEExtAppTabPage(TabSet.AddTab(TCEExtAppTabPage,true).Page);
  page.ExtApp.AppWndClass:= 'Solitaire';//'FileZilla Main Window';
  page.ExtApp.CommandLine:= 'sol.exe';//'"C:\Program Files\FileZilla\FileZilla.exe"';
  page.ExtApp.AppWndStyle:= WS_POPUP;//WS_CHILD;
  page.ExtApp.Run;
  page.UpdateCaption;
end;

procedure TMainForm.SpTBXItem75Click(Sender: TObject);
var
  shellinfo: TSHFileOpStructA;
  fromFile, toFile: String;
begin
  fromFile:= exePath + 'CubicExplorer.exe';
  toFile:= 'D:\Projektit\CubicExplorer\InTest\CubicExplorer.exe';

  ZeroMemory(@shellinfo, SizeOf(shellinfo));
  shellinfo.wnd   := Application.Handle;
  shellinfo.wFunc := FO_COPY;
  shellinfo.pFrom := PChar(fromFile);
  shellinfo.pTo   := PChar(toFile);
  SHFileOperation(shellinfo);
end;

procedure TMainForm.SpTBXItem80Click(Sender: TObject);
var
  page: TCEExtAppTabPage;
begin
  page:= TCEExtAppTabPage(TabSet.AddTab(TCEExtAppTabPage,true).Page);
  page.ExtApp.AppWndClass:= 'MMCMainFrame';//'FileZilla Main Window';
  page.ExtApp.CommandLine:= 'mmc.exe compmgmt.msc';//'"C:\Program Files\FileZilla\FileZilla.exe"';
  page.ExtApp.AppWndStyle:= WS_CHILD;
  page.ExtApp.Run;
  page.UpdateCaption;
end;

procedure TMainForm.SpTBXItem85Click(Sender: TObject);
begin
  if TSpTBXItem(Sender).Tag = 1 then
  begin
    GlobalSessions.ActiveSession:= TSpTBXItem(Sender).Caption;
  end;
end;

procedure TMainForm.test_act1Click(Sender: TObject);
begin
  //
  Raise Exception.CreateFmt('Test exception', [name]);
end;

// <---- fix try end


{##############################################################################}

{$IFDEF madExcept}
procedure LayoutExceptHandler(const exceptIntf : IMEException; var handled : boolean);
begin
  exceptIntf.BugReportHeader['user name']:= '';          // save privacy
  exceptIntf.BugReportHeader['registered owner']:= '';   // save privacy
  exceptIntf.BugReportHeader['computer name']:= '';      // save privacy
  exceptIntf.BugReportHeader['system up time']:= '';      // save privacy
  exceptIntf.BugReportHeader['compiled with']:= '';
end;

initialization
  RegisterExceptionHandler(LayoutExceptHandler, stDontSync);
{$ENDIF}

end.
