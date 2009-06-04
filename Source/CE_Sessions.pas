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
  CE_AppSettings,
  // fcl-xml
  DOM,
  // SpTBX
  SpTBXItem, TB2Item,
  // Tnt
  TntDialogs,
  // System Units
  Classes, SysUtils, StrUtils, Windows, Contnrs, Math, Controls, Dialogs;

type
  TCESessionSaveLoadItem = (sliTabs, sliBookmarks, sliLayout);
  TCESessionSaveLoadItems = set of TCESessionSaveLoadItem;

  TCESessionItem = class(TPersistent)
  private
    fAutoSave: Boolean;
    fName: WideString;
    fSaveLoadItems: TCESessionSaveLoadItems;
    fTime: TDateTime;
    function GetTimeStr: string;
    procedure SetAutoSave(const Value: Boolean);
    procedure SetTimeStr(const Value: string);
    procedure SetName(const Value: WideString);
    procedure SetSaveLoadItems(const Value: TCESessionSaveLoadItems);
    procedure SetTime(const Value: TDateTime);
  public
    fNode: TDOMElement;
    property Node: TDOMElement read fNode write fNode;
  published
    property AutoSave: Boolean read fAutoSave write SetAutoSave;
    property TimeStr: string read GetTimeStr write SetTimeStr;
    property Name: WideString read fName write SetName;
    property SaveLoadItems: TCESessionSaveLoadItems read fSaveLoadItems write
        SetSaveLoadItems;
    property Time: TDateTime read fTime write SetTime;
  end;

  TCESessionList = class(TCECustomSettingStorage)
  private
    fRootNodeName: WideString;
  protected
    ARootNode: TDOMNode;
    AStorage: TCEAppSettings;
  public
    Items: TObjectList;
    constructor Create;
    destructor Destroy; override;
    function AddSession: TCESessionItem;
    procedure ClearSessions;
    function DeleteOldestSession: Boolean;
    function DeleteSession(AIndex: Integer): Boolean;
    function FindSession(AName: WideString): TCESessionItem;
    function GetNewestSession: TCESessionItem;
    function GetSession(AIndex: Integer): TCESessionItem;
    procedure Load(AAppStorage: TCEAppSettings; ANode: TDOMNode); override;
    procedure LoadSession(ASession: TCESessionItem);
    procedure SaveSession(ASession: TCESessionItem);
    procedure SortByTime;
    property RootNodeName: WideString read fRootNodeName write fRootNodeName;
  end;

  TCESessions = class(TCEAppSettings)
  private
    fActiveSession: TCESessionItem;
    fActiveSessionIsHistory: Boolean;
    fAutoLoadSession: TCESessionItem;
    fHistoryCount: Integer;
    fSessionHistory: TCESessionList;
    fSessions: TCESessionList;
    function GetActiveSessionIndex: Integer;
    procedure SetActiveSession(const Value: TCESessionItem);
    procedure SetHistoryCount(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddHistorySession;
    procedure ClearHistory;
    procedure LoadAutoSession;
    procedure LoadLatestHistorySession;
    procedure SaveActiveSession;
    procedure SaveSessionDlg;
    procedure SaveToFile(AFilePath: WideString); override;
    procedure ShowSessionManager;
    property ActiveSession: TCESessionItem read fActiveSession write
        SetActiveSession;
    property ActiveSessionIndex: Integer read GetActiveSessionIndex;
    property ActiveSessionIsHistory: Boolean read fActiveSessionIsHistory;
    property AutoLoadSession: TCESessionItem read fAutoLoadSession write
        fAutoLoadSession;
    property HistoryCount: Integer read fHistoryCount write SetHistoryCount;
    property SessionHistory: TCESessionList read fSessionHistory write
        fSessionHistory;
    property Sessions: TCESessionList read fSessions write fSessions;
  published
  end;

  TDOMElementHack = class(TDOMElement);

type
  TCESessionsMenuItem = class(TTBGroupItem)
  protected
    procedure FillList; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitiateAction; override;
    procedure OnSessionClick(Sender: TObject);
    procedure Recreate; virtual;
  end;

  TCESessionHistoryMenuItem = class(TTBGroupItem)
  protected
    procedure FillList; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitiateAction; override;
    procedure OnSessionClick(Sender: TObject);
    procedure Recreate; virtual;
  end;

  TCESessionsToolbar = class(TSpTBXToolbar)
  private
  protected
    procedure OnSessionClick(Sender: TObject); virtual;
    procedure Populate; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Recreate;
  end;

function CompareSessionTime(Item1, Item2: Pointer): Integer;

var
  GlobalSessions: TCESessions;

implementation

uses
  fCE_TabPage, WideSupport, XMLWrite, Main, CE_SpTabBar, fCE_SaveSessionDlg,
  fCE_SessionManager, CE_LanguageEngine;

{-------------------------------------------------------------------------------
  CompareSessionTime
-------------------------------------------------------------------------------}
function CompareSessionTime(Item1, Item2: Pointer): Integer;
begin
  if TCESessionItem(Item1).Time < TCESessionItem(Item2).Time then
  Result:= -1
  else if TCESessionItem(Item1).Time > TCESessionItem(Item2).Time then
  Result:= 1
  else
  Result:= 0;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCESessions
-------------------------------------------------------------------------------}
constructor TCESessions.Create;
begin
  inherited;
  fSessions:= TCESessionList.Create;
  fSessions.RootNodeName:= 'Sessions';
  AddItem('Sessions', fSessions, true);
  fSessions.AStorage:= Self;

  fSessionHistory:= TCESessionList.Create;
  fSessionHistory.RootNodeName:= 'SessionHistory';
  AddItem('SessionHistory', fSessionHistory);
  fSessionHistory.AStorage:= Self;

  fHistoryCount:= 10;
  fActiveSessionIsHistory:= false;
  fActiveSession:= nil;
end;

{-------------------------------------------------------------------------------
  Destroy TCESessions
-------------------------------------------------------------------------------}
destructor TCESessions.Destroy;
begin
  fSessions.Free;
  fSessionHistory.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add History Session
-------------------------------------------------------------------------------}
procedure TCESessions.AddHistorySession;
var
  session: TCESessionItem;
  c: Integer;
begin
  c:= fSessionHistory.Items.Count;
  while (c > fHistoryCount-1) and (c > 0) do
  begin
    if not SessionHistory.DeleteOldestSession then
    break;
    c:= fSessionHistory.Items.Count;
  end;
  session:= SessionHistory.AddSession;
  SessionHistory.SaveSession(session);
end;

{-------------------------------------------------------------------------------
  Clear History
-------------------------------------------------------------------------------}
procedure TCESessions.ClearHistory;
begin
  if WideMessageDlg(_('Are you sure you want to clear session history?'), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    SessionHistory.ClearSessions;
  end;
end;

{-------------------------------------------------------------------------------
  Get Active Session Index
-------------------------------------------------------------------------------}
function TCESessions.GetActiveSessionIndex: Integer;
begin
  if ActiveSessionIsHistory then
  Result:= SessionHistory.Items.IndexOf(ActiveSession)
  else
  Result:= Sessions.Items.IndexOf(ActiveSession);
end;

{-------------------------------------------------------------------------------
  Load Auto Session
-------------------------------------------------------------------------------}
procedure TCESessions.LoadAutoSession;
begin
  ActiveSession:= AutoLoadSession;
end;

{-------------------------------------------------------------------------------
  Load Latest History Session
-------------------------------------------------------------------------------}
procedure TCESessions.LoadLatestHistorySession;
begin
  ActiveSession:= SessionHistory.GetNewestSession;
end;

{-------------------------------------------------------------------------------
  Save Active Session
-------------------------------------------------------------------------------}
procedure TCESessions.SaveActiveSession;
begin
  if assigned(ActiveSession) then
  begin
    if Sessions.Items.IndexOf(ActiveSession) > -1 then
    Sessions.SaveSession(ActiveSession)
    else if SessionHistory.Items.IndexOf(ActiveSession) > -1 then
    SessionHistory.SaveSession(ActiveSession);
  end;
end;

{-------------------------------------------------------------------------------
  Save Session Dlg
-------------------------------------------------------------------------------}
procedure TCESessions.SaveSessionDlg;
var
  dlg: TCESaveSessionDlg;
  session: TCESessionItem;
begin
  dlg:= TCESaveSessionDlg.Create(nil);
  try
    if dlg.ShowModal = mrOK then
    begin
      session:= Sessions.FindSession(dlg.SessionCombo.Text);
      if not assigned(session) then
      begin
        session:= Sessions.AddSession;
        session.Name:= dlg.SessionCombo.Text;
      end;
      Sessions.SaveSession(session);
      MainForm.SessionsToolbar.Recreate;
    end;
  finally
    dlg.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Save To File
-------------------------------------------------------------------------------}
procedure TCESessions.SaveToFile(AFilePath: WideString);
var
  FileStream: TStream;
begin
  FileStream:= TWideFileStream.Create(AFilePath, fmCreate);
  try
    WriteXML(XML.DocumentElement, FileStream);
  finally
    FileStream.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Active Session
-------------------------------------------------------------------------------}
procedure TCESessions.SetActiveSession(const Value: TCESessionItem);
begin
  // AutoSave Previous session
  if assigned(ActiveSession) then
  begin
    if ActiveSession.AutoSave then
    SaveActiveSession;
  end;
  // Load session
  fActiveSession:= Value;
  if assigned(fActiveSession) then
  begin
    if Sessions.Items.IndexOf(fActiveSession) > -1 then
    begin
      Sessions.LoadSession(fActiveSession);
      fActiveSessionIsHistory:= false;
    end
    else if SessionHistory.Items.IndexOf(fActiveSession) > -1 then
    begin
      SessionHistory.LoadSession(fActiveSession);
      fActiveSessionIsHistory:= true;
    end
    else
    begin
      fActiveSessionIsHistory:= false;
      fActiveSession:= nil;
    end;
  end;
  MainForm.SessionsToolbar.Recreate;
end;

{-------------------------------------------------------------------------------
  Set History Count
-------------------------------------------------------------------------------}
procedure TCESessions.SetHistoryCount(const Value: Integer);
begin
  fHistoryCount:= Max(Value, 0);
  while (fSessionHistory.Items.Count > fHistoryCount) and (fSessionHistory.Items.Count > 0) do
  begin
    SessionHistory.DeleteOldestSession;
  end;
end;

{-------------------------------------------------------------------------------
  Show Session Manager
-------------------------------------------------------------------------------}
procedure TCESessions.ShowSessionManager;
var
  manager: TCESessionManager;
begin
  manager:= TCESessionManager.Create(nil);
  try
    manager.ShowModal;
  finally
    manager.Free;
    MainForm.SessionsToolbar.Recreate;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCESessionList
-------------------------------------------------------------------------------}
constructor TCESessionList.Create;
begin
  inherited;
  Items:= TObjectList.Create(true);
end;

{-------------------------------------------------------------------------------
  Destroy TCESessionList
-------------------------------------------------------------------------------}
destructor TCESessionList.Destroy;
begin
  Items.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add Session
-------------------------------------------------------------------------------}
function TCESessionList.AddSession: TCESessionItem;
begin
  Result:= TCESessionItem.Create;
  Items.Add(Result);
  Result.Node:= AStorage.XML.CreateElement('Session');
  if not assigned(ARootNode) then
  begin
    ARootNode:= AStorage.XML.CreateElement(RootNodeName);
    AStorage.XML.DocumentElement.AppendChild(ARootNode);
  end;
  ARootNode.AppendChild(Result.Node);
end;

{-------------------------------------------------------------------------------
  Clear Sessions
-------------------------------------------------------------------------------}
procedure TCESessionList.ClearSessions;
var
  i: Integer;
  session: TCESessionItem;
begin
  for i:= 0 to Items.Count - 1 do
  begin
    session:= TCESessionItem(Items.Items[i]);
    if assigned(session.Node) then
    begin
      if assigned(ARootNode) then
      ARootNode.RemoveChild(session.Node);
    end;
  end;
  Items.Clear;
end;

{-------------------------------------------------------------------------------
  Delete Oldest Session
-------------------------------------------------------------------------------}
function TCESessionList.DeleteOldestSession: Boolean;
var
  i, index: Integer;
  d: TDateTime;
begin
  d:= 0;
  index:= -1;
  for i:= 0 to Items.Count - 1 do
  begin
    if (TCESessionItem(Items.Items[i]).Time < d) or (i = 0) then
    begin
      d:= TCESessionItem(Items.Items[i]).Time;
      index:= i;
    end;
  end;
  Result:= index > -1;
  if Result then
  DeleteSession(index);
end;

{-------------------------------------------------------------------------------
  Delete Session
-------------------------------------------------------------------------------}
function TCESessionList.DeleteSession(AIndex: Integer): Boolean;
var
  session: TCESessionItem;
begin
  Result:= (AIndex > -1) and (AIndex < Items.Count);
  if Result then
  begin
    session:= TCESessionItem(Items.Items[AIndex]);
    if assigned(session.Node) then
    begin
      if assigned(ARootNode) then
      ARootNode.RemoveChild(session.Node);
    end;
    Items.Delete(AIndex);
  end;
end;

{-------------------------------------------------------------------------------
  Find Session
-------------------------------------------------------------------------------}
function TCESessionList.FindSession(AName: WideString): TCESessionItem;
var
  i: Integer;
  session: TCESessionItem;
begin
  Result:= nil;
  for i:= 0 to Items.Count - 1 do
  begin
    session:= TCESessionItem(Items.Items[i]);
    if session.Name = AName then
    begin
      Result:= session;
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Newest Session
-------------------------------------------------------------------------------}
function TCESessionList.GetNewestSession: TCESessionItem;
var
  i, index: Integer;
  d: TDateTime;
begin
  Result:= nil;
  d:= 0;
  index:= -1;
  for i:= 0 to Items.Count - 1 do
  begin
    if (TCESessionItem(Items.Items[i]).Time > d) or (i = 0) then
    begin
      d:= TCESessionItem(Items.Items[i]).Time;
      Result:= TCESessionItem(Items.Items[i]);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Session
-------------------------------------------------------------------------------}
function TCESessionList.GetSession(AIndex: Integer): TCESessionItem;
begin
  Result:= TCESessionItem(Items.Items[AIndex]);
end;

{-------------------------------------------------------------------------------
  Load
-------------------------------------------------------------------------------}
procedure TCESessionList.Load(AAppStorage: TCEAppSettings; ANode: TDOMNode);
var
  item: TCESessionItem;
  chNode, chNode2: TDOMNode;
  sli: TCESessionSaveLoadItems;
begin
  AStorage:= AAppStorage;
  ARootNode:= ANode;
  Items.Clear;
  chNode:= ANode.FirstChild;
  while assigned(chNode) do
  begin
    if chNode.NodeName = 'Session' then
    begin
      item:= TCESessionItem.Create;
      item.Name:= TDOMElement(chNode).AttribStrings['name'];
      item.TimeStr:= TDOMElement(chNode).AttribStrings['time'];
      item.AutoSave:= StrToBoolDef(TDOMElement(chNode).AttribStrings['autosave'], false);
      
      sli:= [];
      chNode2:= chNode.FirstChild;
      while assigned(chNode2) do
      begin
        if chNode2.NodeName = 'Tabs' then
        Include(sli, sliTabs)
        else if chNode2.NodeName = 'Bookmarks' then
        Include(sli, sliBookmarks)
        else if chNode2.NodeName = 'Layout' then
        Include(sli, sliLayout);
        chNode2:= chNode2.NextSibling;
      end;
      item.SaveLoadItems:= sli;

      item.Node:= TDOMElement(chNode);
      Items.Add(item);
    end;
    chNode:= chNode.NextSibling;
  end;
end;

{-------------------------------------------------------------------------------
  Load Session
-------------------------------------------------------------------------------}
procedure TCESessionList.LoadSession(ASession: TCESessionItem);

  procedure LoadTabs(ATabsNode: TDOMNode);
  var
    chNode: TDOMNode;
    tabClass: TCECustomTabPageClass;
    tab: TCECustomTabPage;
  begin
    chNode:= ATabsNode.FirstChild;
    while assigned(chNode) do
    begin
      tabClass:= TabPageClassList.GetClass(chNode.NodeName);
      if tabClass <> nil then
      begin
        tab:= MainForm.TabSet.AddTab(tabClass).Page;
        AStorage.LoadObjectProperties(tab.Settings, chNode);
      end;
      chNode:= chNode.NextSibling;
    end;
    MainForm.TabSet.ActiveTabIndex:= StrToIntDef(TDOMElement(ATabsNode).AttribStrings['active'], 0);
  end;

var
  chNode: TDOMNode;
begin
  if assigned(ASession) then
  begin
    MainForm.BeginUIUpdate;
    try
      MainForm.TabSet.CloseAllTabs;
      chNode:= ASession.Node.FirstChild;
      while assigned(chNode) do
      begin
        if chNode.NodeName = 'Tabs' then
        begin
          LoadTabs(chNode);
        end;
        chNode:= chNode.NextSibling;
      end;
    finally
      MainForm.EndUIUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save Session
-------------------------------------------------------------------------------}
procedure TCESessionList.SaveSession(ASession: TCESessionItem);

  procedure SaveTabs(ATabsNode: TDOMNode);
  var
    chNode: TDOMNode;
    tab: TCESpTabItem;
    i: Integer;
    s: String;
  begin
    for i:= 0 to MainForm.TabSet.Items.Count - 1 do
    begin
      if MainForm.TabSet.Items.Items[i] is TCESpTabItem then
      begin
        tab:= TCESpTabItem(MainForm.TabSet.Items.Items[i]);
        s:= TabPageClassList.GetName(TCECustomTabPageClass(tab.Page.ClassType));
        if s <> '' then
        begin
          chNode:= AStorage.XML.CreateElement(s);
          ATabsNode.AppendChild(chNode);
          AStorage.SaveObjectProperties(tab.Page.Settings, chNode);
        end;
      end;
    end;
    TDOMElement(ATabsNode).AttribStrings['active']:= IntToStr(MainForm.TabSet.ActiveTabIndex);
  end;

var
  chNode: TDOMElement;
begin
  if assigned(ASession) then
  begin
    TDOMElementHack(ASession.Node).FreeChildren;
    // TODO: Session SaveLoadItems
    // Tabs
    chNode:= AStorage.XML.CreateElement('Tabs');
    ASession.Node.AppendChild(chNode);
    SaveTabs(chNode);
    // Time
    ASession.Time:= Now;
  end;
end;

{-------------------------------------------------------------------------------
  Sort By Time
-------------------------------------------------------------------------------}
procedure TCESessionList.SortByTime;
begin
  Items.Sort(@CompareSessionTime);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Set Name
-------------------------------------------------------------------------------}
procedure TCESessionItem.SetName(const Value: WideString);
begin
  fName:= Value;
  if assigned(fNode) then
  fNode.AttribStrings['name']:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set TimeStr
-------------------------------------------------------------------------------}
function TCESessionItem.GetTimeStr: string;
var
  fs: TFormatSettings;
begin
  fs.DateSeparator:= '/';
  fs.TimeSeparator:= ':';
  fs.LongTimeFormat:= 'hh:nn:ss';
  fs.ShortDateFormat:= 'dd/mm/yyyy';
  Result:= DateTimeToStr(Self.Time, fs);
end;
procedure TCESessionItem.SetTimeStr(const Value: string);
var
  fs: TFormatSettings;
begin
  fs.DateSeparator:= '/';
  fs.TimeSeparator:= ':';
  fs.LongTimeFormat:= 'hh:nn:ss';
  fs.ShortDateFormat:= 'dd/mm/yyyy';
  try
    Self.Time:= StrToDateTime(Value, fs);
  except

  end;
end;

{-------------------------------------------------------------------------------
  Set AutoSave
-------------------------------------------------------------------------------}
procedure TCESessionItem.SetAutoSave(const Value: Boolean);
begin
  fAutoSave:= Value;
  if assigned(fNode) then
  fNode.AttribStrings['autosave']:= BoolToStr(fAutoSave, true);
end;

{-------------------------------------------------------------------------------
  Set SaveLoadItems
-------------------------------------------------------------------------------}
procedure TCESessionItem.SetSaveLoadItems(const Value: TCESessionSaveLoadItems);
begin
  // TODO: Session SaveLoadItems
  fSaveLoadItems:= Value;
  if assigned(fNode) then
  begin

  end;
end;

{-------------------------------------------------------------------------------
  Set Time
-------------------------------------------------------------------------------}
procedure TCESessionItem.SetTime(const Value: TDateTime);
var
  fs: TFormatSettings;
begin
  fs.DateSeparator:= '/';
  fs.TimeSeparator:= ':';
  fs.LongTimeFormat:= 'hh:nn:ss';
  fs.ShortDateFormat:= 'dd/mm/yyyy';
  fTime:= Value;
  if assigned(fNode) then
  begin
    fNode.AttribStrings['time']:= DateTimeToStr(Self.Time, fs);
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
  session: TCESessionItem;
begin
  for i:= 0 to GlobalSessions.Sessions.Items.Count - 1 do
  begin
    session:= TCESessionItem(GlobalSessions.Sessions.Items.Items[i]);
    item:= TSpTBXItem.Create(self);
    item.Caption:= session.Name;
    item.Tag:= i;
    item.OnClick:= OnSessionClick;
    if not GlobalSessions.ActiveSessionIsHistory then
    item.Checked:= GlobalSessions.ActiveSession = session;
    Add(item);
  end;
end;

{-------------------------------------------------------------------------------
  Initiate Action (update checked states)
-------------------------------------------------------------------------------}
procedure TCESessionsMenuItem.InitiateAction;
begin
  inherited;
  Recreate;
end;

{-------------------------------------------------------------------------------
  OnSessionClick (change active session)
-------------------------------------------------------------------------------}
procedure TCESessionsMenuItem.OnSessionClick(Sender: TObject);
var
  item: TSpTBXItem;
  session: TCESessionItem;
begin
  item:= TSpTBXItem(Sender);
  if (item.Tag > -1) and (item.Tag < GlobalSessions.Sessions.Items.Count) then
  begin
    session:= TCESessionItem(GlobalSessions.Sessions.Items.Items[item.Tag]);
    GlobalSessions.ActiveSession:= session;
  end;
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
  Create an instance of TCESessionHistoryMenuItem
-------------------------------------------------------------------------------}
constructor TCESessionHistoryMenuItem.Create(AOwner: TComponent);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Fill List
-------------------------------------------------------------------------------}
procedure TCESessionHistoryMenuItem.FillList;
var
  item: TSpTBXItem;
  i: Integer;
  session: TCESessionItem;
begin
  for i:= GlobalSessions.SessionHistory.Items.Count - 1 downto 0 do
  begin
    session:= TCESessionItem(GlobalSessions.SessionHistory.Items.Items[i]);
    item:= TSpTBXItem.Create(self);
    item.Caption:= 'Session at ' + session.TimeStr;
    item.Tag:= i;
    item.OnClick:= OnSessionClick;
    if GlobalSessions.ActiveSessionIsHistory then
    item.Checked:= GlobalSessions.ActiveSession = session;
    Add(item);
  end;
end;

{-------------------------------------------------------------------------------
  Initiate Action (update checked states)
-------------------------------------------------------------------------------}
procedure TCESessionHistoryMenuItem.InitiateAction;
begin
  inherited;
  Recreate;
end;

{-------------------------------------------------------------------------------
  OnSessionClick (change active session)
-------------------------------------------------------------------------------}
procedure TCESessionHistoryMenuItem.OnSessionClick(Sender: TObject);
var
  item: TSpTBXItem;
  session: TCESessionItem;
begin
  item:= TSpTBXItem(Sender);
  if (item.Tag > -1) and (item.Tag < GlobalSessions.SessionHistory.Items.Count) then
  begin
    session:= TCESessionItem(GlobalSessions.SessionHistory.Items.Items[item.Tag]);
    GlobalSessions.ActiveSession:= session;
  end;
end;

{-------------------------------------------------------------------------------
  Recreate
-------------------------------------------------------------------------------}
procedure TCESessionHistoryMenuItem.Recreate;
begin
  Clear;
  FillList;
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
begin
  GlobalSessions.ActiveSession:= GlobalSessions.Sessions.GetSession(TSpTBXItem(Sender).Tag);
end;

{-------------------------------------------------------------------------------
  Populate
-------------------------------------------------------------------------------}
procedure TCESessionsToolbar.Populate;
var
  item: TSpTBXItem;
  i: Integer;
  session: TCESessionItem;
begin
  for i:= 0 to GlobalSessions.Sessions.Items.Count - 1 do
  begin
    session:= GlobalSessions.Sessions.GetSession(i);
    item:= TSpTBXItem.Create(self);
    item.Caption:= session.Name;
    item.Tag:= i;
    item.Checked:= GlobalSessions.ActiveSession = session;
    item.OnClick:= OnSessionClick;
    Items.Add(item);
  end;
end;

{-------------------------------------------------------------------------------
  Recreate
-------------------------------------------------------------------------------}
procedure TCESessionsToolbar.Recreate;
begin
  Self.BeginUpdate;
  try
    Items.Clear;
    Populate;
  finally
    Self.EndUpdate;
  end;
end;


{##############################################################################}

initialization
  GlobalSessions:= TCESessions.Create;

finalization
  FreeAndNil(GlobalSessions);
  
end.
