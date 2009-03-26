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
//  The Original Code is CE_Sessions.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Sessions;

interface

uses
  // CE Units
  CE_XMLStorage, CE_SettingsStorage, CE_LanguageEngine, CE_SpTabBar,
  // fcl-xml
  DOM, XmlRead, XmlWrite,
  // SpTBX
  SpTBXItem, TB2Item,
  // System Units
  Classes, Windows, SysUtils, Contnrs, CE_SettingsIntf;

type
  TCESpTabSetAccess = class(TCESpTabSet);

  TCESessionItemType = (sitTabs, sitBookmarks, sitLayouts);
  TCESessionItemTypes = set of TCESessionItemType;
  
  TCESession = class(TObject)
  private
    fAutoSave: Boolean;
    fSessionItemTypes: TCESessionItemTypes;
    fSessionName: WideString;
    fSessionNode: TDOMNode;
    fStorage: TCESettingsStorage;
  public
    constructor Create;
    procedure LoadSettings(OnlySessionProperties: Boolean = true);
    procedure SaveSettings(OnlySessionProperties: Boolean = true);
    property AutoSave: Boolean read fAutoSave write fAutoSave;
    property SessionItemTypes: TCESessionItemTypes read fSessionItemTypes write
        fSessionItemTypes;
    property SessionName: WideString read fSessionName write fSessionName;
    property SessionNode: TDOMNode read fSessionNode write fSessionNode;
    property Storage: TCESettingsStorage read fStorage write fStorage;
  end;

  TCESessionList = class(TObject)
  private
    fActiveSession: WideString;
    fIsDefaultSession: Boolean;
    fSessions: TObjectList;
    fStartupSession: WideString;
    function GetSession(Index: Integer): TCESession;
    function GetSessionCount: Integer;
    procedure SetActiveSession(const Value: WideString);
  protected
    Storage: TCESettingsStorage;
    StorageIntf: ICESettingsStorage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
    procedure Clear;
    function CreateSession(SessionName: WideString): TCESession;
    function DeleteSession(SessionName: WideString): Boolean;
    function FindSession(SessionName: WideString): TCESession;
    function GetActiveSession: TCESession;
    procedure LoadActiveSession;
    procedure LoadFromDoc(Storage: TCESettingsStorage; RootPath: WideString = '/');
    procedure LoadFromFile(AFilePath: WideString; RootPath: WideString);
    procedure SaveToFile(AFilePath: WideString; RootPath: WideString = '/');
    procedure SaveToSession(SessionName: WideString); overload;
    procedure SaveToSession(Session: TCESession); overload;
    property ActiveSession: WideString read fActiveSession write SetActiveSession;
    property IsDefaultSession: Boolean read fIsDefaultSession write
        fIsDefaultSession;
    property Session[Index: Integer]: TCESession read GetSession;
    property SessionCount: Integer read GetSessionCount;
    property StartupSession: WideString read fStartupSession write fStartupSession;
  end;

  TCESessionsMenuItem = class(TTBGroupItem)
  protected
    procedure FillList; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitiateAction; override;
    procedure OnSessionClick(Sender: TObject);
    procedure Recreate; virtual;
  end;

type
  TCESessionsToolbar = class(TSpTBXToolbar)
  private
  protected
    procedure OnSessionClick(Sender: TObject); virtual;
    procedure Populate; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Recreate;
  end;

function GlobalSessions: TCESessionList;

var
  fGlobalSessions: TCESessionList;

implementation

uses
  fCE_TabPage, Main, fCE_FileView;

function GlobalSessions: TCESessionList;
begin
  if fGlobalSessions = nil then
  fGlobalSessions:= TCESessionList.Create;
  Result:= fGlobalSessions;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCESessionList
-------------------------------------------------------------------------------}
constructor TCESessionList.Create;
begin
  inherited;
  fSessions:= TObjectList.Create(true);
  Storage:= TCESettingsStorage.Create;
  Storage.AppStorage.DocumentRootName:= 'Sessions';
  StorageIntf:= Storage as ICESettingsStorage;  
end;

{-------------------------------------------------------------------------------
  Free TCESessionList
-------------------------------------------------------------------------------}
destructor TCESessionList.Destroy;
begin
  fSessions.Clear;
  fSessions.Free;
  StorageIntf:= nil;
  inherited;
end;

{-------------------------------------------------------------------------------
  Changed
-------------------------------------------------------------------------------}
procedure TCESessionList.Changed;
begin
  MainForm.SessionsToolbar.Recreate;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCESessionList.Clear;
begin
  fSessions.Clear;
  ActiveSession:= '';
end;

{-------------------------------------------------------------------------------
  Create new session (returns nil if same named already exists)
-------------------------------------------------------------------------------}
function TCESessionList.CreateSession(SessionName: WideString): TCESession;
var
  node, chNode: TDOMNode;
begin
  Result:= nil;
  if FindSession(SessionName) <> nil then
  Exit;

  node:= GetElement(Storage.AppStorage.XmlDoc, '/');
  if node <> nil then
  begin
    chNode:= node.OwnerDocument.CreateElement('Session');
    TDOMElement(chNode).AttribStrings['name']:= SessionName;
    node.AppendChild(chNode);
    Result:= TCESession.Create;
    Result.Storage:= Storage;
    Result.SessionName:= SessionName;
    Result.SessionNode:= chNode;
    fSessions.Add(Result);
    Changed;
  end;
end;

{-------------------------------------------------------------------------------
  Delete Session (returns TRUE if session was found)
-------------------------------------------------------------------------------}
function TCESessionList.DeleteSession(SessionName: WideString): Boolean;
var
  s: TCESession;
begin
  s:= FindSession(SessionName);
  Result:= assigned(s);
  if Result then
  begin
    if WideCompareText(SessionName, ActiveSession) = 0 then
    ActiveSession:= 'Default';

    s.SessionNode.ParentNode.RemoveChild(s.SessionNode);
    fSessions.Remove(s);
    Changed;
  end;
end;

{-------------------------------------------------------------------------------
  Find Session
-------------------------------------------------------------------------------}
function TCESessionList.FindSession(SessionName: WideString): TCESession;
var
  i: Integer;
  s: TCESession;
begin
  Result:= nil;
  for i:= 0 to SessionCount - 1 do
  begin
    s:= Session[i];
    if WideSameText(s.SessionName, SessionName) then
    begin
      Result:= s;
      Break;
    end;    
  end;
end;

{-------------------------------------------------------------------------------
  GetActiveSession
-------------------------------------------------------------------------------}
function TCESessionList.GetActiveSession: TCESession;
begin
  Result:= FindSession(ActiveSession);
end;

{-------------------------------------------------------------------------------
  Get Session
-------------------------------------------------------------------------------}
function TCESessionList.GetSession(Index: Integer): TCESession;
begin
  Result:= TCESession(fSessions.Items[Index]);
end;

{-------------------------------------------------------------------------------
  Get Session Count
-------------------------------------------------------------------------------}
function TCESessionList.GetSessionCount: Integer;
begin
  Result:= fSessions.Count;
end;

{-------------------------------------------------------------------------------
  Load Active session
-------------------------------------------------------------------------------}
procedure TCESessionList.LoadActiveSession;
var
  s: TCESession;
begin
  s:= FindSession(ActiveSession);
  if s <> nil then // Session found
  begin
    s.LoadSettings(false);
  end;
end;

{-------------------------------------------------------------------------------
  Load From Doc
-------------------------------------------------------------------------------}
procedure TCESessionList.LoadFromDoc(Storage: TCESettingsStorage; RootPath:
    WideString = '/');
var
  node: TDOMNode;
  chnode,oldnode: TDOMNode;
  session: TCESession;
  ws: WideString;
  d: Boolean;
begin
  Clear;
  node:= GetElement(Storage.AppStorage.XmlDoc, RootPath);
  if node <> nil then
  begin
    chnode:= node.FirstChild;
    while chnode <> nil do
    begin
      d:= false;
      if WideSameText(chnode.NodeName, 'session') and (chnode is TDOMElement) then
      begin
        ws:= TDOMElement(chnode).AttribStrings['name'];
        if (ws <> '') and (FindSession(ws) = nil) then
        begin
          session:= TCESession.Create;
          session.Storage:= Storage;
          session.SessionName:= ws;
          session.SessionNode:= chnode;
          fSessions.Add(session);
          session.AutoSave:= WideCompareText(TDOMElement(chnode).AttribStrings['autosave'], 'true') = 0;
        end
        else
        begin
          d:= true;
        end;
      end;
      
      if d then
      begin
        oldnode:= chnode;
        chnode:= chnode.NextSibling;
        node.RemoveChild(oldnode);
      end
      else
      chnode:= chnode.NextSibling;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Load From File
-------------------------------------------------------------------------------}
procedure TCESessionList.LoadFromFile(AFilePath: WideString; RootPath:
    WideString);
begin
  Storage.LoadFromFile(AFilePath);
  LoadFromDoc(Storage, RootPath);
end;

{-------------------------------------------------------------------------------
  Save Session
-------------------------------------------------------------------------------}
procedure TCESessionList.SaveToSession(Session: TCESession);
begin
  if assigned(Session) then
  begin
    Session.SaveSettings(false);
  end;
end;

{-------------------------------------------------------------------------------
  Save Session
-------------------------------------------------------------------------------}
procedure TCESessionList.SaveToSession(SessionName: WideString);
begin
  SaveToSession(FindSession(SessionName));
end;

{-------------------------------------------------------------------------------
  Save To File
-------------------------------------------------------------------------------}
procedure TCESessionList.SaveToFile(AFilePath: WideString; RootPath: WideString
    = '/');
var
  i: Integer;
begin
  for i:= 0 to fSessions.Count - 1 do
  begin
    TCESession(fSessions.Items[i]).SaveSettings(true);
  end;
  Storage.SaveToFile(AFilePath);
end;

{-------------------------------------------------------------------------------
  Set Active Session
-------------------------------------------------------------------------------}
procedure TCESessionList.SetActiveSession(const Value: WideString);
var
  s: TCESession;
begin
  if fActiveSession <> Value then
  begin
    // Save old session if needed
    s:= FindSession(fActiveSession);
    if assigned(s) then
    begin
      if s.AutoSave then
      SaveToSession(s);
    end;

    fActiveSession:= Value;
    fIsDefaultSession:= WideCompareText(fActiveSession, 'default') = 0;
    // Create if not found and is default
    s:= FindSession(fActiveSession);
    if s = nil then
    begin
      if fIsDefaultSession then
      begin
        s:= CreateSession('Default');
        s.AutoSave:= true;
      end;
    end;
    Changed;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCESessionsMenuItem
-------------------------------------------------------------------------------}
constructor TCESessionsMenuItem.Create(AOwner: TComponent);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Fill List
-------------------------------------------------------------------------------}
procedure TCESessionsMenuItem.FillList;
var
  item: TSpTBXItem;
  i: Integer;
begin
  // Default Session
  item:= TSpTBXItem.Create(self);
  item.Caption:= _('Default');
  item.Tag:= 1; // default
  item.OnClick:= OnSessionClick;
  Add(item);
  // Separator
  Add(TSpTBXSeparatorItem.Create(self));
  // User Sessions
  for i:= 0 to GlobalSessions.SessionCount - 1 do
  begin
    if WideCompareText(GlobalSessions.Session[i].SessionName, 'default') <> 0 then
    begin
      item:= TSpTBXItem.Create(self);
      item.Caption:= GlobalSessions.Session[i].SessionName;
      item.Tag:= 0; // user
      item.OnClick:= OnSessionClick;
      Add(item);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Initiate Action (update checked states)
-------------------------------------------------------------------------------}
procedure TCESessionsMenuItem.InitiateAction;
var
  i: Integer;
  found: Boolean;
  item: TSpTBXItem;
  ws: WideString;
begin
  inherited;
  Recreate;
  found:= false;
  for i:= 0 to Self.Count - 1 do
  begin
    if Self.Items[i] is TSpTBXItem then
    begin
      if not found then
      begin
        item:= TSpTBXItem(Self.Items[i]);
        if item.Tag = 1 then
        ws:= 'default'
        else
        ws:= item.Caption;

        found:= WideCompareText(ws, GlobalSessions.ActiveSession) = 0;
        item.Checked:= found;
      end
      else
      begin
        Self.Items[i].Checked:= false;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  OnSessionClick (change active session)
-------------------------------------------------------------------------------}
procedure TCESessionsMenuItem.OnSessionClick(Sender: TObject);
var
  item: TSpTBXItem;
begin
  item:= TSpTBXItem(Sender);
  if item.Tag = 1 then
  GlobalSessions.ActiveSession:= 'default'
  else
  GlobalSessions.ActiveSession:= item.Caption;
  GlobalSessions.LoadActiveSession;
end;

{-------------------------------------------------------------------------------
  Recreate
-------------------------------------------------------------------------------}
procedure TCESessionsMenuItem.Recreate;
begin
  Clear;
  FillList;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCESession
-------------------------------------------------------------------------------}
constructor TCESession.Create;
begin
  inherited;
  fSessionItemTypes:= [sitTabs];
  fAutoSave:= false;
end;

{-------------------------------------------------------------------------------
  Load session Settings
-------------------------------------------------------------------------------}
procedure TCESession.LoadSettings(OnlySessionProperties: Boolean = true);
  // Load Tabs
  procedure LoadTabs(TabsNode: TDOMNode);
  var
    node: TDOMNode;
    page: TCECustomTabPage;
    pageClass: TCECustomTabPageClass;
    oldRoot: TDOMNode;
    i,c: Integer;
  begin
    MainForm.BeginUIUpdate;

    try
      MainForm.TabSet.CloseAllTabs;
      node:= TabsNode.FirstChild;
      while node <> nil do
      begin
        pageClass:= TabPageClassList.GetClass(node.NodeName);
        if pageClass <> nil then
        begin
          page:= TCECustomTabPage(MainForm.TabSet.AddTab(pageClass, false).Page);
          oldRoot:= Storage.AppStorage.RootNode;
          Storage.AppStorage.RootNode:= node;
          try
            page.LoadFromStorage(Storage as TCESettingsStorage);
          finally
            Storage.AppStorage.RootNode:= oldRoot;
          end;
        end;
        node:= node.NextSibling;
      end;

      i:= StrToIntDef(TDOMElement(TabsNode).AttribStrings['selected'], 0);
      c:= MainForm.TabSet.Items.Count;
      if (c > i) and (i > -1) then
      begin
        if MainForm.TabSet.ActiveTabIndex <> i then
        MainForm.TabSet.ActiveTabIndex:= i
        else
        TCESpTabSetAccess(MainForm.TabSet).DoActiveTabChange(i);
      end
      else if c > 0 then
      begin
        MainForm.TabSet.ActiveTabIndex:= 0;
      end
      else
      begin
        MainForm.TabSet.AddTab(TCEFileViewPage, true);
      end;

    finally
      MainForm.EndUIUpdate;
    end;
  end;
  
var
  elem: TDOMElement;
  chNode: TDOMNode;
begin
  if assigned(fSessionNode) then
  begin
    elem:= TDOMElement(fSessionNode);
    SessionName:=  elem.AttribStrings['name'];
    fAutoSave:= WideCompareText(elem.AttribStrings['autosave'], 'true') = 0;
    fSessionItemTypes:= [];
    // Tabs
    chNode:= GetChildElement(fSessionNode, 'Tabs', false);
    if chNode <> nil then
    begin
      include(fSessionItemTypes, sitTabs);
      if not OnlySessionProperties then
      LoadTabs(chNode);
    end;
    // Bookmarks
    chNode:= GetChildElement(fSessionNode, 'Bookmarks', false);
    if chNode <> nil then
    begin
      include(fSessionItemTypes, sitBookmarks);
    end;
    // Layouts
    chNode:= GetChildElement(fSessionNode, 'Layouts', false);
    if chNode <> nil then
    begin
      include(fSessionItemTypes, sitLayouts);;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save session settings
-------------------------------------------------------------------------------}
procedure TCESession.SaveSettings(OnlySessionProperties: Boolean = true);

  // Save Tabs
  procedure SaveTabs(TabsNode: TDOMNode);
  var
    page: TCECustomTabPage;
    i: Integer;
    oldRoot: TDOMNode;
    tabNode: TDOMNode;
  begin
    for i:= 0 to MainForm.TabSet.Items.Count - 1 do
    begin
      if MainForm.TabSet.Items.Items[i] is TCESpTabItem then
      begin
        page:= TCESpTabItem(MainForm.TabSet.Items.Items[i]).Page;
        if assigned(page) then
        begin
          tabNode:= TabsNode.OwnerDocument.CreateElement(TabPageClassList.GetName(TCECustomTabPageClass(page.ClassType)));
          TabsNode.AppendChild(tabNode);

          oldRoot:= Storage.AppStorage.RootNode;
          Storage.AppStorage.RootNode:= tabNode;
          Storage.AppStorage.ActivePath:= '';
          try
            page.SaveToStorage(Storage);
          finally
            Storage.AppStorage.RootNode:= oldRoot;
          end;
        end;
      end;
    end;
    
    if MainForm.TabSet.ActiveTabIndex > -1 then
    TDOMElement(TabsNode).AttribStrings['selected']:= IntToStr(MainForm.TabSet.ActiveTabIndex);
  end;

var
  elem: TDOMElement;
  chNode: TDOMNode;
begin
  if assigned(fSessionNode) then
  begin
    elem:= TDOMElement(fSessionNode);
    elem.AttribStrings['name']:= SessionName;
    if fAutoSave then
    elem.AttribStrings['autosave']:= 'True'
    else
    elem.AttribStrings['autosave']:= 'False';
    // Tabs
    if sitTabs in SessionItemTypes then
    begin
      if not OnlySessionProperties then
      begin
        DeleteChildElement(fSessionNode, 'Tabs');
        chNode:= GetChildElement(fSessionNode, 'Tabs', true);
        SaveTabs(chNode);
      end
      else
      GetChildElement(fSessionNode, 'Tabs', true)
    end
    else
    DeleteChildElement(fSessionNode, 'Tabs');
    // Bookmarks
    if sitTabs in SessionItemTypes then
    GetChildElement(fSessionNode, 'Bookmarks', true)
    else
    DeleteChildElement(fSessionNode, 'Bookmarks');
    // Layouts
    if sitTabs in SessionItemTypes then
    GetChildElement(fSessionNode, 'Layouts', true)
    else
    DeleteChildElement(fSessionNode, 'Layouts');
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCESessionsToolbar
-------------------------------------------------------------------------------}
constructor TCESessionsToolbar.Create(AOwner: TComponent);
begin
  inherited;
  Self.Customizable:= false;
end;

{-------------------------------------------------------------------------------
  On Session Click
-------------------------------------------------------------------------------}
procedure TCESessionsToolbar.OnSessionClick(Sender: TObject);
var
  item: TSpTBXItem;
begin
  item:= TSpTBXItem(Sender);
  if item.Tag = 1 then
  GlobalSessions.ActiveSession:= 'default'
  else
  GlobalSessions.ActiveSession:= item.Caption;
  GlobalSessions.LoadActiveSession;
end;

{-------------------------------------------------------------------------------
  Populate
-------------------------------------------------------------------------------}
procedure TCESessionsToolbar.Populate;
var
  item: TSpTBXItem;
  i: Integer;
begin
  Self.BeginUpdate;
  try
    // Default Session
    item:= TSpTBXItem.Create(self);
    item.Caption:= _('Default');
    item.Tag:= 1; // default
    if WideCompareText(GlobalSessions.ActiveSession, 'default') = 0 then
    item.Checked:= true;
    item.OnClick:= OnSessionClick;
    Items.Add(item);
    // Separator
    Items.Add(TSpTBXSeparatorItem.Create(self));
    // User Sessions
    for i:= 0 to GlobalSessions.SessionCount - 1 do
    begin
      if WideCompareText(GlobalSessions.Session[i].SessionName, 'default') <> 0 then
      begin
        item:= TSpTBXItem.Create(self);
        item.Caption:= GlobalSessions.Session[i].SessionName;
        item.Tag:= 0; // user
        if WideCompareText(GlobalSessions.ActiveSession, item.Caption) = 0 then
        item.Checked:= true;
        item.OnClick:= OnSessionClick;
        Items.Add(item);
      end;
    end;
  finally
    Self.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Recreate
-------------------------------------------------------------------------------}
procedure TCESessionsToolbar.Recreate;
begin
  Items.Clear;
  Populate;
end;


{##############################################################################}
initialization

finalization
  FreeAndNil(fGlobalSessions);

end.
