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
  fCE_FolderPanel, fCE_QuickViewPanel, fCE_BookmarkPanel, fCE_Customizer,
  fCE_PoEditor, fCE_MultiViewPage,
  // CE Data Modules
  dCE_Actions, dCE_Images, dCE_Input,
  // CE Units
  CE_Layout, CE_Utils, CE_GlobalCtrl, CE_AddressToolbar,
  CE_DriveBar, CE_BookmarkBar, CE_StatusBar, CE_VistaFuncs, CE_Breadcrumb,
  CE_ToolbarButtons, CE_TBActions, CE_LanguageCodes, CE_LanguageEngine,
  CE_LanguageUtils, CE_Sessions, CE_SpTabBar,
  CE_AppSettings, CE_Toolbar,
  // Toolbar2000
  TB2Dock, TB2Item, TB2Toolbar, TB2ToolWindow, TB2ExtItems,
  // SpTBX
  SpTBXItem, SpTBXEditors, SpTBXTabs, SpTBXControls, SpTBXSkins,
  // VSTools
  VirtualShellNewMenu, EasyListview, VirtualExplorerEasyListview,
  MPCommonObjects, VirtualShellNotifier, VirtualResources, MPShellTypes,
  VirtualTrees, MPShellUtilities, MPCommonUtilities,
  // Tnt Controls
  TntSystem, TntActnList, TntClasses, TntMenus, TntStdCtrls, TntSysUtils,
  TntDialogs, TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ShellAPI, Menus, ShlObj, XPMan, ActiveX,
  ImgList, Registry, AppEvnts, ActnList, Math, JvComponentBase, JvTrayIcon;

type
  TMainFormSettings = class;

  TMainForm = class(TTntForm)
    TopToolDock: TSpTBXDock;
    RightToolDock: TSpTBXDock;
    BottomToolDock: TSpTBXDock;
    LeftToolDock: TSpTBXDock;
    MainToolbar: TCEToolbar;
    MainPanel: TPanel;
    fileMenuItem: TSpTBXSubmenuItem;
    editMenuItem: TSpTBXSubmenuItem;
    viewMenuItem: TSpTBXSubmenuItem;
    toolsMenuItem: TSpTBXSubmenuItem;
    helpMenuItem: TSpTBXSubmenuItem;
    ViewToolbar: TCEToolbar;
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
    NavigationToolbar: TCEToolbar;
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
    EditToolbar: TCEToolbar;
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
    SpTBXItem53: TSpTBXItem;
    ToolbarPopupMenu: TSpTBXPopupMenu;
    SpTBXItem52: TSpTBXItem;
    SpTBXSeparatorItem17: TSpTBXSeparatorItem;
    SpTBXItem56: TSpTBXItem;
    SpTBXItem74: TSpTBXItem;
    StartUpTimer: TTimer;
    SpTBXItem79: TSpTBXItem;
    SpTBXItem81: TSpTBXItem;
    LanguageMenuItem: TSpTBXSubmenuItem;
    SpTBXSeparatorItem20: TSpTBXSeparatorItem;
    SpTBXItem83: TSpTBXItem;
    SkinGroupItem: TSpTBXSkinGroupItem;
    SpTBXSeparatorItem21: TSpTBXSeparatorItem;
    SpTBXItem82: TSpTBXItem;
    SpTBXItem84: TSpTBXItem;
    sessionsMenuItem: TSpTBXSubmenuItem;
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
    SpTBXItem91: TSpTBXItem;
    SpTBXItem94: TSpTBXItem;
    SpTBXItem95: TSpTBXItem;
    SpTBXSeparatorItem26: TSpTBXSeparatorItem;
    sessionHistoryMenuItem: TSpTBXSubmenuItem;
    SpTBXSeparatorItem27: TSpTBXSeparatorItem;
    SpTBXItem47: TSpTBXItem;
    SpTBXItem85: TSpTBXItem;
    SpTBXSeparatorItem22: TSpTBXSeparatorItem;
    SpTBXItem87: TSpTBXItem;
    SpTBXItem88: TSpTBXItem;
    SpTBXSeparatorItem23: TSpTBXSeparatorItem;
    TrayIcon: TJvTrayIcon;
    TrayPopupMenu: TSpTBXPopupMenu;
    SpTBXItem92: TSpTBXItem;
    SpTBXSeparatorItem25: TSpTBXSeparatorItem;
    SpTBXItem93: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    sub_closed_tab_list: TSpTBXSubmenuItem;
    SpTBXSeparatorItem19: TSpTBXSeparatorItem;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TransparencyClick(Sender: TObject);
    procedure TransparencyPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure LanguageMenuItemPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure StartUpTimerTimer(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure sub_closed_tab_listPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure TabPopupMenuPopup(Sender: TObject);
    procedure TrayIconMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
  private
    fFullscreen: Boolean;
    fActiveLanguage: WideString;
    fCEIsClosing: Boolean;
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
    procedure WMSyscommand(var Message: TWmSysCommand); message WM_SYSCOMMAND;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMHotkey(var Message: TMessage); message WM_HOTKEY;
  public
    DockHostForm: TCEDockHostForm;
    Layouts: TCELayoutController;
    TabSet: TCESpTabSet;
    AddressBarToolbar: TCEAddressBarToolbar;
    BreadcrumbBar: TCEBreadcrumbBar;
    DriveToolbar: TCEDriveToolbar;
    BookmarkToolbar: TCEBookmarkToolbar;
    SessionsToolbar: TCESessionsToolbar;
    Settings: TMainFormSettings;
    StatusBar: TCEStatusBar;
    procedure BeginUIUpdate;
    procedure DoCustomTranslate;
    procedure EndUIUpdate;
    procedure InitializeUI;
    procedure FinalizeUI;
    procedure MakeVisible(AForceToTop: Boolean = false);
    procedure MenuItemTranslateHandler(Obj:TObject; var IsIgnored: Boolean);
    procedure OpenSkin;
    procedure Shutdown;
    procedure StartUp;
    procedure ToggleVisibility;
    procedure TranslateUI(Sender: TObject);
    property Fullscreen: Boolean read fFullscreen write SetFullscreen;
    property ActiveLanguage: WideString read fActiveLanguage write
        SetActiveLanguage;
    property PathInTitle: Boolean read fPathInTitle write SetPathInTitle;
    property SingleInstance: Boolean read fSingleInstance write SetSingleInstance;
  published
    property CEIsClosing: Boolean read fCEIsClosing;
  end;

  TCEStartupType = (stNormal, stSession, stLastSession);

  TMainFormSettings = class(TPersistent)
  private
    fCloseToTray: Boolean;
    fExitOnLastTabClose: Boolean;
    fHeight: Integer;
    fLeft: Integer;
    fMinimizeToTray: Boolean;
    fShowCmd: Integer;
    fStartInTray: Boolean;
    fStartupType: TCEStartupType;
    fTop: Integer;
    fWidth: Integer;
    function GetAlphaBlend: Integer;
    function GetAlwaysOnTop: Boolean;
    function GetLanguage: WideString;
    function GetPathInTitle: Boolean;
    function GetShowHint: Boolean;
    function GetSingleInstance: Boolean;
    function GetSkin: string;
    function GetAutoLoadSession: WideString;
    function GetShowTray: Boolean;
    procedure SetAlphaBlend(const Value: Integer);
    procedure SetAlwaysOnTop(const Value: Boolean);
    procedure SetLanguage(const Value: WideString);
    procedure SetPathInTitle(const Value: Boolean);
    procedure SetShowHint(const Value: Boolean);
    procedure SetSingleInstance(const Value: Boolean);
    procedure SetSkin(const Value: string);
    procedure SetAutoLoadSession(const Value: WideString);
    procedure SetShowTray(const Value: Boolean);
  public
    Form: TMainForm;
    constructor Create;
    procedure UpdatePositionInfo;
    procedure ApplyPositionInfo(AHideForm: Boolean = false);
  published
    property AlphaBlend: Integer read GetAlphaBlend write SetAlphaBlend;
    property AlwaysOnTop: Boolean read GetAlwaysOnTop write SetAlwaysOnTop;
    property Left: Integer read fLeft write fLeft;
    property Top: Integer read fTop write fTop;
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
    property Language: WideString read GetLanguage write SetLanguage;
    property PathInTitle: Boolean read GetPathInTitle write SetPathInTitle;
    property ShowCmd: Integer read fShowCmd write fShowCmd;
    property ShowHint: Boolean read GetShowHint write SetShowHint;
    property SingleInstance: Boolean read GetSingleInstance write SetSingleInstance;
    property Skin: string read GetSkin write SetSkin;
    property AutoLoadSession: WideString read GetAutoLoadSession write
        SetAutoLoadSession;
    property CloseToTray: Boolean read fCloseToTray write fCloseToTray;
    property ExitOnLastTabClose: Boolean read fExitOnLastTabClose write
        fExitOnLastTabClose;
    property MinimizeToTray: Boolean read fMinimizeToTray write fMinimizeToTray;
    property ShowTray: Boolean read GetShowTray write SetShowTray;
    property StartInTray: Boolean read fStartInTray write fStartInTray;
    property StartupType: TCEStartupType read fStartupType write fStartupType;
  end;

var
  MainForm: TMainForm;
  
implementation

uses
  madExcept, CE_QuickView, Clipbrd, CE_PaneHost, CE_Stacks, MPResources;

{$R *.dfm}

{*------------------------------------------------------------------------------
  BeginUIUpdate
-------------------------------------------------------------------------------}
procedure TMainForm.BeginUIUpdate;
begin
  fUpdatingCount:= fUpdatingCount + 1;
  if fUpdatingCount > 1 then
  Exit;
  LockWindowUpdate(GetDesktopWindow); // TODO: this should not be used
  SendMessage(MainForm.DockHostForm.Handle, WM_SETREDRAW, 0,0);
  Self.TopToolDock.BeginUpdate;
  Self.RightToolDock.BeginUpdate;
  Self.BottomToolDock.BeginUpdate;
  Self.LeftToolDock.BeginUpdate;
  Self.MainToolbar.BeginUpdate;
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
  LockWindowUpdate(0);
  SendMessage(MainForm.DockHostForm.Handle, WM_SETREDRAW, 1,0);
  RedrawWindow(MainForm.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

{*------------------------------------------------------------------------------
  Get's called when MainForm is created.
-------------------------------------------------------------------------------}
procedure TMainForm.FormCreate(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_HIDE);
  SetWindowLong(Application.Handle, GWL_EXSTYLE,
    GetWindowLong(Application.Handle, GWL_EXSTYLE) and not WS_EX_APPWINDOW
    or WS_EX_TOOLWINDOW);
  ShowWindow(Application.Handle, SW_SHOW);

  ChangeNotifier.RegisterShellChangeNotify(Self);
  fLanguageList:= TTntStringList.Create;
  fLanguageList.NameValueSeparator:= '=';
  fIsReady:= false;
  fUpdatingCount:= 0;
  fCEIsClosing:= false;
  fPathInTitle:= false;
  SetVistaFont(Self.Font);
  Settings:= TMainFormSettings.Create;
  Settings.Form:= Self;
  GlobalAppSettings.AddItem('MainForm', Settings, true, true);

  StackDirPath:= ExePath + 'Stacks\';
end;

{-------------------------------------------------------------------------------
  Create Params
-------------------------------------------------------------------------------}
procedure TMainForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or
    WS_EX_APPWINDOW;
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
  Settings.Free;
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
  TabSet.Name:= 'TabBar';
  TabSet.Toolbar.Caption:= _('Tabs');
  TabSet.Toolbar.Name:= 'TabToolbar';
  TabSet.TabDragReorder:= true;
  TabSet.TabPageHost:= DockHostForm.PaneGroupHost.GetPane;
  TabSet.LayoutController:= Layouts;
  TabSet.Toolbar.Customizable:= true;
  TabSet.Images:= CE_Images.SmallIcons;
  TabSet.TabPopupMenu:= TabPopupMenu;
  GlobalFocusCtrl.CtrlList.Add(TabSet.Toolbar);

    // Add panes
  //Tabset.Panes.Add(DockHostForm.DualViewHost.MainPane);
  //Tabset.Panes.Add(DockHostForm.DualViewHost.DualPane);

  //TCESpTabToolbar(TabSet.Toolbar).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;

  // Create FolderCombo Toolbar
  AddressBarToolbar:= TCEAddressBarToolbar.Create(self);
  AddressBarToolbar.Name:= 'AddressBarToolbar';
  AddressBarToolbar.Caption:= _('Address Bar');
  AddressBarToolbar.DockableTo:= [TB2Dock.dpTop, TB2Dock.dpBottom];
  AddressBarToolbar.Visible:= false;
  AddressBarToolbar.CurrentDock:= TopToolDock;
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
  BreadcrumbBar.CurrentDock:= TopToolDock;
  BreadcrumbBar.PopupMenu:= ToolbarPopupMenu;
  BreadcrumbBar.Tag:= 1;
  // Create Status bar
  StatusBar:= TCEStatusBar.Create(Self);
  StatusBar.Name:= 'StatusBar';
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
  CELayoutItems.Add(StatusBar);
  //CELayoutItems.Add(DockHostForm.DualViewHost.Toolbar);
  // Add Toolbar Docks
  CEToolbarDocks.Add(LeftToolDock);
  CEToolbarDocks.Add(TopToolDock);
  CEToolbarDocks.Add(RightToolDock);
  CEToolbarDocks.Add(BottomToolDock);
  CEToolbarDocks.Add(DockHostForm.PaneGroupHost.ActivePaneGroup.LeftGroupToolDock, true);
  CEToolbarDocks.Add(DockHostForm.PaneGroupHost.ActivePaneGroup.TopGroupToolDock, true);
  CEToolbarDocks.Add(DockHostForm.PaneGroupHost.ActivePaneGroup.RightGroupToolDock, true);
  CEToolbarDocks.Add(DockHostForm.PaneGroupHost.ActivePaneGroup.BottomGroupToolDock, true);
  // Populate menu items
  CELayoutItems.PopulateMenuItem(toolbarsMenuItem);
  CELayoutItems.PopulateMenuItem(ToolbarPopupMenu.Items);
  // Add Session menu items
  sessionsMenuItem.Add(TCESessionsMenuItem.Create(self));
  sessionHistoryMenuItem.Add(TCESessionHistoryMenuItem.Create(self));

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
  //Wow64Enabled:= IsWindows64;

  // Load skins
  GetSkinsFromFolder(ExePath + 'Skins\');
  SkinGroupItem.Recreate;

  // Load Sessions
  GlobalSessions.LoadFromFile(exePath + 'sessions.xml');
  SessionsToolbar.Recreate;
  // Load Settings
  GlobalAppSettings.LoadFromFile(exePath + 'settings.xml');
  Settings.ApplyPositionInfo(Settings.StartInTray);
  if not Settings.StartInTray then
  MainForm.Show;

  // Load Bookmarks
  CEBookmarkPanel.BookmarksPath:= ExePath + 'bookmarks.xml';
  CEBookmarkPanel.BookmarkMenuItems.Add(BookmarkToolbar.Items);
  CEBookmarkPanel.BookmarkMenuItems.Add(bookmarkMenuItem);
  CEBookmarkPanel.LoadBookmarks;

  // Load Layouts
  Layouts.AutoSave:= false;
  Layouts.LoadFromFile(ExePath + 'layout.xml');
  Layouts.LoadSettingsForToolbars;

  // Load Stacks
  //GlobalStacks.LoadFromDir(GlobalStacks.StackDirPath);

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
    case Settings.StartupType of
      stNormal: CEActions.act_tabs_addtab.Execute;
      stSession: GlobalSessions.LoadAutoSession;
      stLastSession: GlobalSessions.LoadLatestHistorySession;
    end;
  end;

  // Atleast one tab has to be open
  if TabSet.TabCount = 0 then
  begin
    CEActions.act_tabs_addtab.Execute;
  end;

  if Application.MainForm.AlphaBlendValue < 255 then
  Application.MainForm.AlphaBlend:= true;

  fIsReady:= true;

  // Start Update timer
  CEActions.UpdateTimer.Enabled:= true;

  


  // Testing stuff!!!
  if DebugHook <> 0 then
  begin

  end;
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

  if TrayIcon.Active and Settings.StartInTray then
  TrayIcon.HideApplication;
end;

{*------------------------------------------------------------------------------
  Get's called before shutdown
-------------------------------------------------------------------------------}
procedure TMainForm.Shutdown;
begin
  //GlobalStacks.SaveToDir(GlobalStacks.StackDirPath);

  GlobalSessions.SaveToFile(exePath + 'sessions.xml');

  Settings.UpdatePositionInfo;
  GlobalAppSettings.SaveToFile(exePath + 'settings.xml');

  Layouts.SaveSettingsForToolbars;
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
begin
  fCEIsClosing:= true;
  CEActions.UpdateTimer.Enabled:= false;

  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  GlobalFileViewSettings.AssignColumnSettingsFrom(TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView);  

  if assigned(GlobalSessions.ActiveSession) then
  begin
    if GlobalSessions.ActiveSession.AutoSave then
    GlobalSessions.SaveActiveSession;
  end;
  
  if GlobalSessions.AutoSaveHistory then
  GlobalSessions.AddHistorySession;

  if GlobalPathCtrl.ActivePage is TCECustomTabPage then
  Layouts.SaveCurrentLayout;

  CanClose:= TabSet.CloseAllTabs;
  if not CanClose then
  begin
    CEActions.UpdateTimer.Enabled:= true;
    fCEIsClosing:= false;
  end;
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

  // TODO: SpTabSet is visible even when hidden bug. Make proper fix.
  if not TabSet.Visible then
  begin
    TabSet.Visible:= true;
    TabSet.Visible:= false;
  end;
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

{-------------------------------------------------------------------------------
  DoCustomTranslate
-------------------------------------------------------------------------------}
procedure TMainForm.DoCustomTranslate;
begin
  // TabBar Left/Right arrow hints
  TCESpTabToolbar(TabSet.Toolbar).LeftArrow.Hint:= _('Show more tabs');
  TCESpTabToolbar(TabSet.Toolbar).RightArrow.Hint:= TCESpTabToolbar(TabSet.Toolbar).LeftArrow.Hint;

  // VSTools strings
  S_WARNING:= _('Warning');
  S_OPEN:= _('Open');

  STR_GROUPMODIFIEDHOUR:= _('Last hour');
  STR_GROUPMODIFIEDTODAY:= _('Last twenty-four hours');
  STR_GROUPMODIFIEDTHISWEEK:= _('This week');
  STR_GROUPMODIFIEDTWOWEEKS:= _('Two weeks ago');
  STR_GROUPMODIFIEDTHREEWEEKS:= _('Three weeks ago');
  STR_GROUPMODIFIEDMONTH:= _('A month ago');
  STR_GROUPMODIFIEDTWOMONTHS:= _('Two months ago');
  STR_GROUPMODIFIEDTHREEMONTHS:= _('Three months ago');
  STR_GROUPMODIFIEDFOURMONTHS:= _('Four months ago');
  STR_GROUPMODIFIEDFIVEMONTHS:= _('Five months ago');
  STR_GROUPMODIFIEDSIXMONTHS:= _('Six months ago');
  STR_GROUPMODIFIEDEARLIERTHISYEAR:= _('Earlier this year');
  STR_GROUPMODIFIEDLONGTIMEAGO:= _('A long time ago');

  STR_GROUPSIZEZERO:= _('Zero');
  STR_GROUPSIZETINY:= _('Tiny');
  STR_GROUPSIZESMALL:= _('Small');
  STR_GROUPSIZEMEDIUM:= _('Medium');
  STR_GROUPSIZELARGE:= _('Large');
  STR_GROUPSIZEGIGANTIC:= _('Gigantic');
  STR_GROUPSIZESYSFOLDER:= _('System Folders');
  STR_GROUPSIZEFOLDER:= _('Folders');

  STR_FILE_SIZE_IN_KB:= _('KB');
  STR_FILE_SIZE_IN_MB:= _('MB');
  STR_FILE_SIZE_IN_TB:= _('TB');
  STR_ZERO_KB:= '0 ' + STR_FILE_SIZE_IN_KB;
  STR_ONE_KB:= '1 ' + STR_FILE_SIZE_IN_KB;

  StatusBar.UpdateLabels(false);
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
  CEGlobalTranslator.IncludeClasses.Add(TSpTBXCheckBox);
  CEGlobalTranslator.IncludeClasses.Add(TSpTBXGroupBox);
  CEGlobalTranslator.IncludeClasses.Add(TSpTBXRadioGroup);
  CEGlobalTranslator.IncludeClasses.Add(TSpTBXRadioButton);
  CEGlobalTranslator.IncludeClasses.Add(TSpTBXLabel);

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
    DoCustomTranslate;
  end
  else
  begin
    CEGlobalTranslator.LoadPOFromFile(ws);
    TranslateUI(self);
    fActiveLanguage:= Value;
    DoCustomTranslate;
  end;
end;

{*------------------------------------------------------------------------------
  Handle suspend events
-------------------------------------------------------------------------------}
procedure TMainForm.WMPowerBroadcast(var Message: TMessage);
begin
  case Message.WParam of
    PBT_APMSUSPEND: begin
      Layouts.SaveSettingsForToolbars;
      Layouts.SaveCurrentLayout;
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
  Make MainForm visible
-------------------------------------------------------------------------------}
procedure TMainForm.MakeVisible(AForceToTop: Boolean = false);
begin
  if TrayIcon.Active and not TrayIcon.ApplicationVisible then
  TrayIcon.ShowApplication
  else if (GetForegroundWindow <> Handle) and AForceToTop then
  begin
    // Trick Windows to force CE on top.
    Application.Minimize;
    ShowWindow(Application.Handle, SW_HIDE);
    ShowWindow(Application.Handle, SW_SHOW);
    Application.Restore;
  end;

  if IsIconic(Handle) then
  begin
    ShowWindow(Handle, SW_RESTORE);
  end;

  Application.BringToFront;
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

{-------------------------------------------------------------------------------
  Handle WM_SysCommand (fix Vista issues)
-------------------------------------------------------------------------------}
procedure TMainForm.WMSyscommand(var Message: TWmSysCommand);
begin
  case (Message.CmdType and $FFF0) of
    SC_MINIMIZE:
    begin
      ShowWindow(Handle, SW_MINIMIZE);
      if TrayIcon.Active and Settings.MinimizeToTray then
      TrayIcon.HideApplication;
      Message.Result:= 0;
    end;
    SC_RESTORE:
    begin
      if TrayIcon.Active and not TrayIcon.ApplicationVisible then
      TrayIcon.ShowApplication;
      ShowWindow(Handle, SW_RESTORE);
      Message.Result:= 0;
    end;
    SC_CLOSE:
    begin
      if TrayIcon.Active and Settings.CloseToTray then
      TrayIcon.HideApplication
      else
      Self.Close;
      Message.Result:= 0;
    end
  else
    inherited;  
  end;
end;

{-------------------------------------------------------------------------------
  On Shortcut
-------------------------------------------------------------------------------}
procedure TMainForm.FormShortCut(var Msg: TWMKey; var Handled: Boolean);

  function DoExecuteAction(AShortcut: TShortcut; AActionList: TActionList): Boolean;
  var
    i,i2: Integer;
    action: TAction;
  begin
    Result:= false;
    if assigned(AActionList) then
    begin
      for i:= 0 to AActionList.ActionCount-1 do
      begin
        action:= TAction(AActionList.Actions[i]);
        if action.ShortCut = AShortcut then
        begin
          if ExecuteShortcut(action) then
          begin
            Result:= true;
            break;
          end;
        end
        else
        begin
          for i2:= 0 to action.SecondaryShortCuts.Count - 1 do
          begin
            if TShortCut(action.SecondaryShortCuts.Objects[i2]) = AShortcut then
            begin
              if ExecuteShortcut(action) then
              begin
                Result:= true;
                break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

var
  AShortcut: TShortcut;
begin
  Handled:= false;
  AShortcut:= Shortcut(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));

  if assigned(GlobalPathCtrl.ActivePage) then
  Handled:= DoExecuteAction(AShortcut, TCECustomTabPage(GlobalPathCtrl.ActivePage).PageActionList);
  if not Handled then
  Handled:= DoExecuteAction(AShortcut, CEActions.ActionList);
end;

{-------------------------------------------------------------------------------
  Toggle Visibility (Restore/Minimize)
-------------------------------------------------------------------------------}
procedure TMainForm.ToggleVisibility;
begin
  if IsIconic(Handle) then
  begin
    if TrayIcon.Active and not TrayIcon.ApplicationVisible then
    TrayIcon.ShowApplication;

    ShowWindow(Handle, SW_RESTORE);

    Application.BringToFront;
  end
  else
  begin
    if TrayIcon.Active and Settings.MinimizeToTray then
    begin
      if not TrayIcon.ApplicationVisible then
      TrayIcon.ShowApplication
      else
      TrayIcon.HideApplication;
    end
    else
    ShowWindow(Handle, SW_MINIMIZE);
  end;
end;

{-------------------------------------------------------------------------------
  On TrayIcon.MouseUp
-------------------------------------------------------------------------------}
procedure TMainForm.TrayIconMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  ToggleVisibility;
end;

{-------------------------------------------------------------------------------
  On WM_Hotkey Message
-------------------------------------------------------------------------------}
procedure TMainForm.WMHotkey(var Message: TMessage);
begin
  CEActions.GlobalHotkeys.ExecuteHotkey(Message.WParam);
  inherited;
end;

{-------------------------------------------------------------------------------
  On sub_closed_tab_list.Popup
-------------------------------------------------------------------------------}
procedure TMainForm.sub_closed_tab_listPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
begin
  TabSet.PopulateUndoList(Sender);
end;

{-------------------------------------------------------------------------------
  On TabPopupMenu.Popup
-------------------------------------------------------------------------------}
procedure TMainForm.TabPopupMenuPopup(Sender: TObject);
begin
  sub_closed_tab_list.Enabled:= TabSet.CanUndoTabClose;
end;


{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TMainFormSettings
-------------------------------------------------------------------------------}
constructor TMainFormSettings.Create;
begin
  inherited;
  fLeft:= 50;
  fTop:= 50;
  fHeight:= 480;
  fWidth:= 640;
  fShowCmd:= 1;
  fStartupType:= stNormal;
  fMinimizeToTray:= false;
  fCloseToTray:= false;
  fStartInTray:= false;
  fExitOnLastTabClose:= false;
end;
{-------------------------------------------------------------------------------
  Get PositionInfo
-------------------------------------------------------------------------------}
procedure TMainFormSettings.UpdatePositionInfo;
var
  Placement: TWindowPlacement;
  r: TRect;
begin
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(MainForm.Handle, @Placement);
  r:= Placement.rcNormalPosition;
  Left:= r.Left;
  Top:= r.Top;
  Width:= Max(r.Right - r.Left, 50);
  Height:= Max(r.Bottom - r.Top, 50);
  ShowCmd:= Placement.showCmd;
end;

{-------------------------------------------------------------------------------
  SetP ositionInfo
-------------------------------------------------------------------------------}
procedure TMainFormSettings.ApplyPositionInfo(AHideForm: Boolean = false);
var
  Placement: TWindowPlacement;
  r: TRect;
begin
  r.Left:= Left;
  r.Top:= Top;
  r.Right:= Left + Max(Width, 50);
  r.Bottom:= Top + Max(Height, 50);
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(MainForm.Handle, @Placement);
  Placement.rcNormalPosition:= r;
  if AHideForm then
  Placement.showCmd:= SW_HIDE
  else
  Placement.showCmd:= ShowCmd;
  if Placement.showCmd = SW_SHOWMINIMIZED then
  Placement.showCmd:= SW_SHOWNORMAL;
  SetWindowPlacement(MainForm.Handle, @Placement);
end;

{-------------------------------------------------------------------------------
  Get/Set AlphaBlend value
-------------------------------------------------------------------------------}
function TMainFormSettings.GetAlphaBlend: Integer;
begin
  Result:= MainForm.AlphaBlendValue;
end;
procedure TMainFormSettings.SetAlphaBlend(const Value: Integer);
begin
  MainForm.AlphaBlendValue:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set Language
-------------------------------------------------------------------------------}
function TMainFormSettings.GetLanguage: WideString;
begin
  Result:= MainForm.ActiveLanguage;
end;
procedure TMainFormSettings.SetLanguage(const Value: WideString);
begin
  MainForm.ActiveLanguage:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set Skin
-------------------------------------------------------------------------------}
function TMainFormSettings.GetSkin: string;
begin
  Result:= SkinManager.CurrentSkinName;
end;
procedure TMainFormSettings.SetSkin(const Value: string);
begin
  SkinManager.SetSkin(Value);
end;

{-------------------------------------------------------------------------------
  Get/Set ShowHint
-------------------------------------------------------------------------------}
function TMainFormSettings.GetShowHint: Boolean;
begin
  Result:= MainForm.ShowHint;
end;
procedure TMainFormSettings.SetShowHint(const Value: Boolean);
begin
  MainForm.ShowHint:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set SingleInstance
-------------------------------------------------------------------------------}
function TMainFormSettings.GetSingleInstance: Boolean;
begin
  Result:= MainForm.SingleInstance;
end;
procedure TMainFormSettings.SetSingleInstance(const Value: Boolean);
begin
  MainForm.SingleInstance:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set PathInTitle
-------------------------------------------------------------------------------}
function TMainFormSettings.GetPathInTitle: Boolean;
begin
  Result:= MainForm.PathInTitle;
end;
procedure TMainFormSettings.SetPathInTitle(const Value: Boolean);
begin
  MainForm.PathInTitle:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AlwaysOnTop
-------------------------------------------------------------------------------}
function TMainFormSettings.GetAlwaysOnTop: Boolean;
begin
  Result:= MainForm.FormStyle = fsStayOnTop;
end;
procedure TMainFormSettings.SetAlwaysOnTop(const Value: Boolean);
begin
  if Value then
  MainForm.FormStyle:= fsStayOnTop
  else
  MainForm.FormStyle:= fsNormal;
end;

{-------------------------------------------------------------------------------
  Get/Set StartupSession
-------------------------------------------------------------------------------}
function TMainFormSettings.GetAutoLoadSession: WideString;
begin
  if assigned(GlobalSessions.AutoLoadSession) then
  Result:= GlobalSessions.AutoLoadSession.Name
  else
  Result:= '';
end;
procedure TMainFormSettings.SetAutoLoadSession(const Value: WideString);
begin
  GlobalSessions.AutoLoadSession:= GlobalSessions.Sessions.FindSession(Value);
end;

{-------------------------------------------------------------------------------
  Get/Set ShowTray
-------------------------------------------------------------------------------}
function TMainFormSettings.GetShowTray: Boolean;
begin
  Result:= MainForm.TrayIcon.Active;
end;
procedure TMainFormSettings.SetShowTray(const Value: Boolean);
begin
  if not Value and not MainForm.TrayIcon.ApplicationVisible then
  MainForm.TrayIcon.ShowApplication;
  
  MainForm.TrayIcon.Active:= Value;
end;


{##############################################################################}

{$IFDEF madExcept}
procedure LayoutExceptHandler(const exceptIntf : IMEException; var handled : boolean);
begin
  exceptIntf.BugReportHeader['user name']:= '';          // for privacy
  exceptIntf.BugReportHeader['registered owner']:= '';   // for privacy
  exceptIntf.BugReportHeader['computer name']:= '';      // for privacy
  exceptIntf.BugReportHeader['system up time']:= '';     // for privacy
  exceptIntf.BugReportHeader['compiled with']:= '';
end;

initialization
  RegisterExceptionHandler(LayoutExceptHandler, stDontSync);
{$ENDIF}

end.
