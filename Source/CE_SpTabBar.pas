unit CE_SpTabBar;

interface

uses
  // CE Units
  fCE_TabPage, CE_Layout, CE_SettingsIntf, CE_Settings, CE_Utils,
  // SpTBX
  SpTBXItem, SpTBXTabs, TB2Item, TB2Dock, SpTBXSkins,
  // VSTools
  MPShellUtilities,
  // Windows
  Classes, Windows, SysUtils, Dialogs, Messages, Controls, Forms, ActiveX,
  ExtCtrls, Graphics, StrUtils, ShlObj, Menus;

type
  TSpTBXCustomTabSetAccess = class(TSpTBXCustomTabSet);
  TSpTBXTabItemViewerAccess = class(TSpTBXTabItemViewer);
  TCECustomTabPageAccess = class(TCECustomTabPage);
  
  // Tab Item Viewer
  TCESpTabItemViewer = class(TSpTBXTabItemViewer)
  private
    function IsTabCloseButtonVisible: Boolean;
  end;

  // Tab Item
  TCESpTabItem = class(TSpTBXTabItem)
  private
    function GetPageVisibility: Boolean;
    procedure SetPageVisibility(const Value: Boolean);
  protected
    fPage: TCECustomTabPage;
    procedure DoTabClosing(var Allow, CloseAndFree: Boolean); override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
    destructor Destroy; override;
    function CloseTab: Boolean;
    property Page: TCECustomTabPage read fPage;
    property PageVisibility: Boolean read GetPageVisibility write SetPageVisibility;
  end;
  
  // Tab Toolbar
  TCESpTabToolbar = class(TSpTBXTabToolbar)
  private
    fPreventLastTabClosing: Boolean;
    function GetTabCount: Integer;
  protected
    function CanDragCustomize(Button: TMouseButton; Shift: TShiftState; X, Y:
        Integer): Boolean; override;
    procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; PaintOnNCArea:
        Boolean; PaintBorders: Boolean = True); override;
    procedure WMLButtonDblClick(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
  public
    constructor Create(AOwner: TComponent); override;
    function CanTabClose: Boolean;
    function GetTab(Index: Integer): TCESpTabItem;
    function GetTabAt(X, Y: Integer): TCESpTabItem;
    property TabCount: Integer read GetTabCount;
  published
    property OnMouseWheel;
    property PreventLastTabClosing: Boolean read fPreventLastTabClosing write
        fPreventLastTabClosing;
  end;

  // Tab Set
  TCESpTabSet = class(TSpTBXTabSet, IDropTarget, ICESettingsHandler)
  private
    fActivePopupTab: TCESpTabItem;
    fClosingTab: TCESpTabItem;
    fDropTab: TCESpTabItem;
    fDropTimer: TTimer;
    fLayoutController: TCELayoutController;
    fNewTabPath: WideString;
    fNewTabNamespace: TNamespace;
    fNewTabSelect: Boolean;
    fNewTabType: Integer;
    fOpenTabSelect: Boolean;
    fReuseTabs: Boolean;
    fTabPageHost: TWinControl;
    fTabPopupMenu: TPopupMenu;
    function GetTabCount: Integer;
    procedure SetNewTabPath(const Value: WideString);
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure DoActiveTabChange(const ItemIndex: Integer); override;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt:
        TPoint; var dwEffect: Longint): HResult; virtual; stdcall;
    function DragLeave: HResult; virtual; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint):
        HResult; virtual; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var
        dwEffect: Longint): HResult; virtual; stdcall;
    function GetToolbarClass: TSpTBXToolbarClass; override;
    procedure HandleDrawBackground(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
        const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean); virtual;
    procedure HandleDropTimer(Sender: TObject); virtual;
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer); virtual;
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer); virtual;
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta:
        Integer; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure HandleToolbarResize(Sender: TObject); virtual;
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddTab(TabPageClass: TCECustomTabPageClass; SelectTab: Boolean =
        false; ActivatePage: Boolean = true): TCESpTabItem;
    function CloseAllTabs(ExceptThis: TCESpTabItem = nil; Force: Boolean = false):
        Boolean;
    function CloseSelectedTab(Force: Boolean = false): Boolean;
    function CloseTab(ATab: TCESpTabItem; Force: Boolean = false): Boolean;
    function CloseTabsOnLeft(ATab: TCESpTabItem; Force: Boolean = false): Boolean;
    function CloseTabsOnRight(ATab: TCESpTabItem; Force: Boolean = false): Boolean;
    function GetActiveTab: TCESpTabItem;
    function GetFirstTab: TSpTBXTabItem;
    function GetLastTab: TSpTBXTabItem;
    function GetNextTab(From: TSpTBXTabItem): TSpTBXTabItem;
    function GetPrevTab(From: TSpTBXTabItem): TSpTBXTabItem;
    function GetTabAt(X, Y: Integer): TCESpTabItem;
    procedure SelectNextTab(GoForward: Boolean = true);
    procedure SelectTab(ATab: TSpTBXTabItem);
    property ActivePopupTab: TCESpTabItem read fActivePopupTab write
        fActivePopupTab;
    property LayoutController: TCELayoutController read fLayoutController write
        fLayoutController;
    property NewTabPath: WideString read fNewTabPath write SetNewTabPath;
    property NewTabNamespace: TNamespace read fNewTabNamespace write
        fNewTabNamespace;
    property NewTabSelect: Boolean read fNewTabSelect write fNewTabSelect;
    property NewTabType: Integer read fNewTabType write fNewTabType;
    property OpenTabSelect: Boolean read fOpenTabSelect write fOpenTabSelect;
    property TabCount: Integer read GetTabCount;
    property TabPageHost: TWinControl read fTabPageHost write fTabPageHost;
    property TabPopupMenu: TPopupMenu read fTabPopupMenu write fTabPopupMenu;
  published
    property ReuseTabs: Boolean read fReuseTabs write fReuseTabs;
  end;

implementation

uses
  Main, dCE_Actions, fCE_FileView;

{##############################################################################}

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCESpTabItem.Destroy;
begin
  if assigned(fPage) then
  FreeAndNil(fPage);
  inherited;
end;

{-------------------------------------------------------------------------------
  Do Tab Closing
-------------------------------------------------------------------------------}
procedure TCESpTabItem.DoTabClosing(var Allow, CloseAndFree: Boolean);
var
  c: TComponent;
begin
  c:= GetParentComponent;
  if c is TCESpTabToolbar then
  Allow:= TCESpTabToolbar(c).CanTabClose;

  if Allow then
  begin
    if assigned(fPage) then
    begin
      Allow:= fPage.TabClosing;
    end
    else
    Allow:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Get Item Viewer Class
-------------------------------------------------------------------------------}
function TCESpTabItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  Result:= TCESpTabItemViewer;
end;

{-------------------------------------------------------------------------------
  Get PageVisibility
-------------------------------------------------------------------------------}
function TCESpTabItem.GetPageVisibility: Boolean;
begin
  if assigned(fPage) then
  Result:= fPage.Visible
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Set PageVisibility
-------------------------------------------------------------------------------}
procedure TCESpTabItem.SetPageVisibility(const Value: Boolean);
begin
  if assigned(fPage) then
  begin
    if Value then
    fPage.Show
    else
    fPage.HidePage;
  end;
end;

function TCESpTabItem.CloseTab: Boolean;
var
  NextTab: TSpTBXTabItem;
  T: TSpTBXTabToolbar;
  b: Boolean;
begin
  if Visible then
  begin
    Result:= True;
    DoTabClosing(Result, b);
    if Result then
    begin
      // Check the next visible tab
      if Checked and GetTabToolbar(T) then
      begin
        NextTab := GetNextTab(True, sivtInmediateSkipNonVisible);
        if not Assigned(NextTab) then
          NextTab := GetNextTab(False, sivtInmediateSkipNonVisible);
        if Assigned(NextTab) then
          NextTab.Checked := True;
      end;
      Visible:= False;
      DoTabClose;
    end;
  end;

  if GetTabToolbar(T) then
  begin
    if T.GetTabsCount(true) = 0 then
    begin
      CEActions.act_tabs_addtab.Execute;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create
-------------------------------------------------------------------------------}
constructor TCESpTabSet.Create(AOwner: TComponent);
begin
  inherited;
  Self.TabCloseButton:= tcbAll;
  Toolbar.OnMouseDown:= HandleMouseDown;
  Toolbar.OnMouseUp:= HandleMouseUp;
  Self.OnDrawBackground:= HandleDrawBackground;
  TCESpTabToolbar(Toolbar).OnMouseWheel:= HandleMouseWheel;
  fDropTimer:= TTimer.Create(self);
  fDropTimer.Interval:= 1000;
  fDropTimer.Enabled:= false;
  fDropTimer.OnTimer:= HandleDropTimer;
  GlobalSettings.RegisterHandler(Self);
  Toolbar.OnResize:= HandleToolbarResize;
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCESpTabSet.Destroy;
begin
  if assigned(fNewTabNamespace) then
  FreeAndNil(fNewTabNamespace);
  inherited;
end;

{-------------------------------------------------------------------------------
  Get Toolbar Class
-------------------------------------------------------------------------------}
function TCESpTabSet.GetToolbarClass: TSpTBXToolbarClass;
begin
  Result := TCESpTabToolbar;
end;

{-------------------------------------------------------------------------------
  Add Tab
-------------------------------------------------------------------------------}
function TCESpTabSet.AddTab(TabPageClass: TCECustomTabPageClass; SelectTab:
    Boolean = false; ActivatePage: Boolean = true): TCESpTabItem;
var
  page: TCECustomTabPage;
begin
  Result:= nil;
  if not assigned(TabPageClass) then
  Exit;
  if not assigned(fTabPageHost) then
  Exit;

  Result:= TCESpTabItem.Create(nil);
  page:= TabPageClass.Create(nil);
  TCECustomTabPageAccess(page).fTabItem:= Result;
  Result.fPage:= page;
  page.Visible:= false;
  page.Parent:= fTabPageHost;
  page.Align:= alClient;
  page.BoundsRect:= fTabPageHost.ClientRect;
  if ActivatePage then
  page.UpdateCaption;
  page.Active:= ActivatePage;
  Items.Add(Result);
  if SelectTab then
  begin
    Result.Checked:= true;
    DoActiveTabChange(Items.IndexOf(Result));
  end;
end;

{-------------------------------------------------------------------------------
  Close All Tabs
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseAllTabs(ExceptThis: TCESpTabItem = nil; Force:
    Boolean = false): Boolean;
var
  i: Integer;
begin
  Result:= false;
  i:= 0;
  while i < Items.Count do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      if Items.Items[i] <> ExceptThis then
      begin
        if Force then
        begin
          Items.Items[i].Free;
        end
        else
        begin
          if TCESpTabItem(Items.Items[i]).Page.TabClosing then
          Items.Items[i].Free
          else
          Exit;
        end;
      end
      else
      i:= i + 1;
    end
    else
    i:= i + 1;
  end;
  Result:= true;
end;

{-------------------------------------------------------------------------------
  Close SelectedTab
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseSelectedTab(Force: Boolean = false): Boolean;
begin
  Result:= CloseTab(GetActiveTab,Force);
end;

{-------------------------------------------------------------------------------
  Close Tab
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseTab(ATab: TCESpTabItem; Force: Boolean = false):
    Boolean;
begin
  if assigned(ATab) then
  begin
    Result:= ATab.CloseTab;
    if Result or Force then
    ATab.Free;
  end
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Close tabs on left
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseTabsOnLeft(ATab: TCESpTabItem; Force: Boolean =
    false): Boolean;
var
  i: Integer;
begin
  if assigned(ATab) then
  begin
    Result:= Items.IndexOf(ATab) > 0;
    if Result then
    begin
      i:= 0;
      while i < Items.Count do
      begin
        if Items.Items[i] is TCESpTabItem then
        begin
          if Items.Items[i] <> ATab then
          begin
            if Force then
            begin
              Items.Items[i].Free;
            end
            else
            begin
              if TCESpTabItem(Items.Items[i]).Page.TabClosing then
              Items.Items[i].Free
              else
              Exit;
            end;
          end
          else
          break;
        end
        else
        i:= i + 1;
      end;
    end;
  end
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Close tabs on right
-------------------------------------------------------------------------------}
function TCESpTabSet.CloseTabsOnRight(ATab: TCESpTabItem; Force: Boolean =
    false): Boolean;
var
  i: Integer;
begin
  if assigned(ATab) then
  begin
    i:= Items.IndexOf(ATab) + 1;
    Result:= (i > 0) and (i < Items.Count);
    if Result then
    begin
      while i < Items.Count do
      begin
        if Items.Items[i] is TCESpTabItem then
        begin
          if Items.Items[i] <> ATab then
          begin
            if Force then
            begin
              Items.Items[i].Free;
            end
            else
            begin
              if TCESpTabItem(Items.Items[i]).Page.TabClosing then
              Items.Items[i].Free
              else
              Exit;
            end;
          end
          else
          break;
        end
        else
        i:= i + 1;
      end;
    end;
  end
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Create Handle
-------------------------------------------------------------------------------}
procedure TCESpTabSet.CreateHandle;
begin
  inherited;
  RegisterDragDrop(WindowHandle, self);
end;

{-------------------------------------------------------------------------------
  Destroy Handle
-------------------------------------------------------------------------------}
procedure TCESpTabSet.DestroyHandle;
begin
  RevokeDragDrop(WindowHandle);
  inherited;
end;

{-------------------------------------------------------------------------------
  Do Active Tab Change
-------------------------------------------------------------------------------}
procedure TCESpTabSet.DoActiveTabChange(const ItemIndex: Integer);
var
  i: Integer;
  tab: TCESpTabItem;
begin
  inherited;
  if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
  Exit;

  if Items.Items[ItemIndex] is TCESpTabItem then
  tab:= TCESpTabItem(Items.Items[ItemIndex])
  else
  tab:= nil;

  if assigned(tab) then
  begin
    tab.PageVisibility:= true;
    if assigned(tab.Page) then
    begin
      if CompareText(tab.Page.Layout, LayoutController.CurrentLayout) <> 0 then
      begin
        if LayoutController.CurrentLayout <> '' then
        LayoutController.SaveLayout(LayoutController.CurrentLayout);
        LayoutController.LoadLayout(tab.page.Layout);
        LayoutController.CurrentLayout:= tab.page.Layout;
      end;
      tab.Page.SelectPage;
    end;
  end;

  // Hide everything else
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      if i <> ItemIndex then
      TCESpTabItem(Items.Items[i]).PageVisibility:= false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  DragEnter
-------------------------------------------------------------------------------}
function TCESpTabSet.DragEnter(const dataObj: IDataObject; grfKeyState:
    Longint; pt: TPoint; var dwEffect: Longint): HResult;
begin
  Result:= S_OK;
end;

{-------------------------------------------------------------------------------
  DragLeave
-------------------------------------------------------------------------------}
function TCESpTabSet.DragLeave: HResult;
begin
  Result:= S_OK;
  fDropTab:= nil;
  fDropTimer.Enabled:= false;
end;

{-------------------------------------------------------------------------------
  DragOver
-------------------------------------------------------------------------------}
function TCESpTabSet.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect:
    Longint): HResult;
var
  Shift: TShiftState;
  p: TPoint;
  tab: TCESpTabItem;
begin
  Result:= S_OK;
  p:= Self.ScreenToClient(pt);

  tab:= GetTabAt(p.X, p.Y);
  if tab <> fDropTab then
  begin
    fDropTimer.Enabled:= false;
    fDropTab:= tab;
  end;

  if fDropTab <> nil then
  begin
    Shift:= KeysToShiftState(grfKeyState);
    if Shift = [ssLeft] then
    dwEffect:= DROPEFFECT_MOVE
    else if Shift = [ssLeft, ssCtrl] then
    dwEffect:= DROPEFFECT_COPY
    else if Shift = [ssLeft, ssCtrl, ssShift] then
    dwEffect:= DROPEFFECT_LINK
    else
    fDropTab:= nil;
  end;

  if fDropTab = nil then
  begin
    dwEffect:= DROPEFFECT_NONE;
    fDropTimer.Enabled:= false;
  end
  else
  fDropTimer.Enabled:= true;
end;

{-------------------------------------------------------------------------------
  Drop
-------------------------------------------------------------------------------}
function TCESpTabSet.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt:
    TPoint; var dwEffect: Longint): HResult;
begin
  Result:= S_OK;
  fDropTab:= nil;
  fDropTimer.Enabled:= false;
end;

{-------------------------------------------------------------------------------
  Get Active Tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetActiveTab: TCESpTabItem;
begin
  Result:= nil;
  if (ActiveTabIndex > -1) and (ActiveTabIndex < Items.Count) then
  begin
    if Items.Items[ActiveTabIndex] is TCESpTabItem then
    Result:= TCESpTabItem(Items.Items[ActiveTabIndex]);
  end;
end;

{-------------------------------------------------------------------------------
  Get First tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetFirstTab: TSpTBXTabItem;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TSpTBXTabItem then
    begin
      Result:= TSpTBXTabItem(Items.Items[i]);
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Last tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetLastTab: TSpTBXTabItem;
var
  i: Integer;
begin
  Result:= nil;
  for i:= Items.Count - 1 downto 0 do
  begin
    if Items.Items[i] is TSpTBXTabItem then
    begin
      Result:= TSpTBXTabItem(Items.Items[i]);
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get next tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetNextTab(From: TSpTBXTabItem): TSpTBXTabItem;
var
  i: Integer;
begin
  Result:= nil;
  i:= Items.IndexOf(From) + 1;
  while i < Items.Count do
  begin
    if Items.Items[i] is TSpTBXTabItem then
    begin
      Result:= TSpTBXTabItem(Items.Items[i]);
      break;
    end;
    i:= i + 1;
  end;
end;

{-------------------------------------------------------------------------------
  Get previous tab
-------------------------------------------------------------------------------}
function TCESpTabSet.GetPrevTab(From: TSpTBXTabItem): TSpTBXTabItem;
var
  i: Integer;
begin
  Result:= nil;
  i:= Items.IndexOf(From) - 1;
  while i > -1 do
  begin
    if Items.Items[i] is TSpTBXTabItem then
    begin
      Result:= TSpTBXTabItem(Items.Items[i]);
      break;
    end;
    i:= i - 1;
  end;
end;

{-------------------------------------------------------------------------------
  GetTabAt
-------------------------------------------------------------------------------}
function TCESpTabSet.GetTabAt(X, Y: Integer): TCESpTabItem;
begin
  Result:= TCESpTabToolbar(Toolbar).GetTabAt(X, Y);
end;

{-------------------------------------------------------------------------------
  Get TabCount
-------------------------------------------------------------------------------}
function TCESpTabSet.GetTabCount: Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      Result:= Result + 1;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle DrawBackground
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleDrawBackground(Sender: TObject; ACanvas: TCanvas;
    ARect: TRect; const PaintStage: TSpTBXPaintStage; var PaintDefault:
    Boolean);
begin
  if PaintStage = pstPrePaint then
  begin
    ARect.Left:= ARect.Left - 2;
    ARect.Right:= ARect.Right + 3;
    SpDrawXPTabControlBackground(FBackground.Canvas, ARect, Color, TabPosition = ttpBottom, SkinType);
    PaintDefault:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Toolbar MouseDown
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssMiddle] then
  fClosingTab:= GetTabAt(X, Y)
  else
  fClosingTab:= nil;
end;

{-------------------------------------------------------------------------------
  Handle Toolbar MouseUp
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  tab: TCESpTabItem;
  p: TPoint;
begin
  if assigned(fClosingTab) then
  begin
    if Button = mbMiddle then
    begin
      tab:= GetTabAt(X, Y);
      if tab = fClosingTab then
      begin
        if tab.CloseTab then
        tab.Free;
      end;
    end;
    fClosingTab:= nil;
  end;

  if Shift = [ssLeft,ssDouble] then
  begin
    fActivePopupTab:= GetTabAt(X, Y);
    if assigned(fActivePopupTab) then
    begin
      if fActivePopupTab.Page is TCEFileViewPage then
      CEActions.act_tabs_duplicatetab.Execute
      else
      CEActions.act_tabs_addtab.Execute;
    end
    else
    begin
      CEActions.act_tabs_addtab.Execute;
    end;
    fActivePopupTab:= nil;
  end
  else if Button = mbRight then
  begin
    fActivePopupTab:= GetTabAt(X, Y);
    if not assigned(fActivePopupTab) then
    fActivePopupTab:= Self.GetActiveTab;

    if assigned(fActivePopupTab) and Assigned(fTabPopupMenu) then
    begin
      p:= Self.ClientToScreen(Point(X, Y));
      fTabPopupMenu.Popup(p.X, p.Y);
      Application.ProcessMessages;
      fActivePopupTab:= nil;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Mouse Wheel
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
  begin
    SelectNextTab;
    Handled:= true;
  end
  else if WheelDelta < 0 then
  begin
    SelectNextTab(false);
    Handled:= true;
  end;
end;

{-------------------------------------------------------------------------------
  On DropTimer
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleDropTimer(Sender: TObject);
begin
  fDropTimer.Enabled:= false;
  if assigned(fDropTab) then
  begin
    SelectTab(fDropTab);
    fDropTab:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  On Toolbar Resize
-------------------------------------------------------------------------------}
procedure TCESpTabSet.HandleToolbarResize(Sender: TObject);
begin
  if SkinManager.GetSkinType = sknSkin then
  Self.Height:= Toolbar.Height + 2
  else
  Self.Height:= Toolbar.Height + 4
end;

{-------------------------------------------------------------------------------
  Load from storage
-------------------------------------------------------------------------------}
procedure TCESpTabSet.LoadFromStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('Tabs');
  try
    fNewTabType:= Storage.ReadInteger('NewTabType',1);
    NewTabPath:= Storage.ReadString('NewTabPath','');
    fNewTabSelect:= Storage.ReadBoolean('NewTabSelect',true);
    fOpenTabSelect:= Storage.ReadBoolean('OpenTabSelect', false);
    fReuseTabs:= Storage.ReadBoolean('ReuseTabs', false);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Save to storage
-------------------------------------------------------------------------------}
procedure TCESpTabSet.SaveToStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('Tabs');
  try
    Storage.WriteInteger('NewTabType',fNewTabType);
    Storage.WriteString('NewTabPath',fNewTabPath);
    Storage.WriteBoolean('NewTabSelect',fNewTabSelect);
    Storage.WriteBoolean('OpenTabSelect', fOpenTabSelect);
    Storage.WriteBoolean('ReuseTabs', fReuseTabs);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Select Next Tab
-------------------------------------------------------------------------------}
procedure TCESpTabSet.SelectNextTab(GoForward: Boolean = true);
var
  tab: TSpTBXTabItem;
begin
  tab:= GetActiveTab;
  if assigned(tab) then
  begin
    if GoForward then
    tab:= GetNextTab(tab)
    else
    tab:= GetPrevTab(tab);
    
    if not assigned(tab) then
    begin
      if GoForward then
      tab:= GetFirstTab
      else
      tab:= GetLastTab;
    end;

    if assigned(tab) then
    ActiveTabIndex:= Items.IndexOf(tab);
  end;
end;

{-------------------------------------------------------------------------------
  Select Tab
-------------------------------------------------------------------------------}
procedure TCESpTabSet.SelectTab(ATab: TSpTBXTabItem);
var
  i: Integer;
begin
  i:= Items.IndexOf(ATab);
  if i <> -1 then
  ActiveTabIndex:= i;
end;

{-------------------------------------------------------------------------------
  Set NewTabPath
-------------------------------------------------------------------------------}
procedure TCESpTabSet.SetNewTabPath(const Value: WideString);
var
  ws: WideString;
  pidl: PItemIDList;
begin
  fNewTabPath:= Value;
  if assigned(fNewTabNamespace) then
  FreeAndNil(fNewTabNamespace);

  pidl:= nil;
  if Length(fNewTabPath) > 5 then
  begin
    if LeftStr(fNewTabPath, 5) = 'PIDL:' then
    begin
      ws:= Copy(fNewTabPath, 6, Length(fNewTabPath)-5);
      pidl:= LoadPIDLFromMime(ws);
    end;
  end;

  if not assigned(pidl) then
  pidl:= PathToPIDL(fNewTabPath);

  fNewTabNamespace:= TNamespace.Create(pidl, nil);
end;

{-------------------------------------------------------------------------------
  WM_SpSkinChange
-------------------------------------------------------------------------------}
procedure TCESpTabSet.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  HandleToolbarResize(self);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create
-------------------------------------------------------------------------------}
constructor TCESpTabToolbar.Create(AOwner: TComponent);
begin
  inherited;
  fPreventLastTabClosing:= false;
  Self.ShrinkMode:= tbsmChevron;
end;

{-------------------------------------------------------------------------------
  CanDragCustomize
-------------------------------------------------------------------------------}
function TCESpTabToolbar.CanDragCustomize(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer): Boolean;
var
  IV: TTBItemViewer;
  TabIV: TCESpTabItemViewer;
begin
  Result := False;
  FBeginDragIV := nil;

  if not (csDesigning in ComponentState) and (Button = mbLeft) then
  begin
    IV := SpGetItemViewerFromPoint(Items, View, Point(X, Y));
    if Assigned(IV) and (IV is TCESpTabItemViewer) and Assigned(IV.Item) and IV.Item.Enabled and IV.Item.Visible then
    begin
      // Close the tab if the close button is pressed
      TabIV := TCESpTabItemViewer(IV);
      if (TabIV.TabCloseButtonState = sknsHotTrack) and TabIV.IsTabCloseButtonVisible then
      begin
        if TCESpTabItem(TabIV.Item).CloseTab then
        TabIV.Item.Free;
      end
      else
      begin
        // Click the item on mouse down
        if not IV.Item.Checked then
        begin
          Result := True; // Bypass the inherited mouse down
          IV.Item.Click;
          if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
        end
        // Drag reorder
        else if TabDragReorder and not IsCustomizing and IV.Item.Checked then
        begin
          Result:= True; // Bypass the inherited mouse down
          FBeginDragIV := IV;
          BeginDrag(False, 2);
        end;
      end;
    end
    else
      Result := inherited CanDragCustomize(Button, Shift, X, Y);
  end;
end;

function TCESpTabToolbar.CanTabClose: Boolean;
begin
  Result:= not ((GetTabCount = 1) and PreventLastTabClosing);
end;

{-------------------------------------------------------------------------------
  Get Tab
-------------------------------------------------------------------------------}
function TCESpTabToolbar.GetTab(Index: Integer): TCESpTabItem;
var
  i,c: Integer;
begin
  Result:= nil;
  if (Index > -1) and (Index < Items.Count) then
  Exit;

  c:= -1;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      c:= c + 1;
      if c = Index then
      begin
        Result:= TCESpTabItem(Items.Items[i]);
        Break;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  GetTabAt
-------------------------------------------------------------------------------}
function TCESpTabToolbar.GetTabAt(X, Y: Integer): TCESpTabItem;
var
  viewer: TTBItemViewer;
begin
  Result:= nil;
  viewer:= View.ViewerFromPoint(Point(X,Y));
  if assigned(viewer) then
  begin
    if viewer.Item is TCESpTabItem then
    Result:= TCESpTabItem(viewer.Item);
  end;
end;

{-------------------------------------------------------------------------------
  Get TabCount
-------------------------------------------------------------------------------}
function TCESpTabToolbar.GetTabCount: Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 0 to Items.Count - 1 do
  begin
    if Items.Items[i] is TCESpTabItem then
    begin
      Result:= Result + 1;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Internal Draw Background
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.InternalDrawBackground(ACanvas: TCanvas; ARect:
    TRect; PaintOnNCArea: Boolean; PaintBorders: Boolean = True);
var
  B: TBitmap;
  R, BitmapR, DestR: TRect;
  Tab: TSpTBXTabItem;
  IV: TTBItemViewer;
  T: TSpTBXSkinType;
  PrevDelta, NextDelta: Integer;
begin
  T := SpTBXSkinType(SkinType);
  if PaintOnNCArea and Assigned(FOwnerTabControl) then
  begin
    B:= TBitmap.Create;
    B.Canvas.Lock;
    try
      R:= ARect;
      R.Left:= R.Left - 2;
      R.Right:= R.Right + 4;
      B.Width:= (R.Right - R.Left);
      B.Height:= R.Bottom - R.Top;

      SpDrawXPToolbar(Self, B.Canvas, R, PaintOnNCArea, TabBackgroundBorders and (T <> sknNone), skncTabToolbar);

      // Draw the bottom border of the active tab
      Tab:= ActiveTab;
      if Assigned(Tab) and Tab.Visible then
      begin
        IV:= SpFindItemViewer(View, Tab);
        if Assigned(IV) then
        begin
          DestR:= IV.BoundsRect;
          OffsetRect(DestR, 2, 2);  // Add the toolbar margins
          TSpTBXTabItemViewerAccess(IV).DrawBottomBorder(B.Canvas, DestR);
        end;
        if T = sknWindows then
        begin
          if Tab.IsFirstVisibleTab or Assigned(Tab.GetNextTab(False, sivtInmediateSkipNonVisible)) then
            PrevDelta:= 1
          else
            PrevDelta:= -1;
          if Assigned(Tab.GetNextTab(True, sivtInmediateSkipNonVisible)) then
            NextDelta:= 1
          else
            NextDelta:= -1;
          if TabPosition = ttpTop then
            ExcludeClipRect(B.Canvas.Handle, DestR.Left - PrevDelta, R.Bottom - 2, DestR.Right + NextDelta, R.Bottom + 4)
          else
            ExcludeClipRect(B.Canvas.Handle, DestR.Left - PrevDelta, R.Top + 2, DestR.Right + NextDelta, R.Top - 4);
        end
        else
          if TabPosition = ttpTop then
            ExcludeClipRect(B.Canvas.Handle, DestR.Left + 1, R.Bottom - 2, DestR.Right - 1, R.Bottom + 4)
          else
            ExcludeClipRect(B.Canvas.Handle, DestR.Left + 1, R.Top + 2, DestR.Right -1 , R.Top - 4);
      end;

      // Draw the bottom border of the tabs pane
      BitmapR := Rect(0, 0, TSpTBXCustomTabSetAccess(FOwnerTabControl).FBackground.Width, TSpTBXCustomTabSetAccess(FOwnerTabControl).FBackground.Height);
      case TabPosition of
        ttpTop:
          begin
            DestR := Rect(R.Left, R.Bottom - 2, R.Right, R.Bottom);
            BitmapR.Bottom := BitmapR.Top + 2;
          end;
        ttpBottom:
          begin
            DestR := Rect(R.Left, R.Top, R.Right, R.Top + 2);
            BitmapR.Top := BitmapR.Bottom - 2;
          end;
      end;

      B.Canvas.CopyRect(DestR, TSpTBXCustomTabSetAccess(FOwnerTabControl).FBackground.Canvas, BitmapR);
      ACanvas.Draw(0, 0, B);
    finally
      B.Canvas.UnLock;
      B.Free;
    end;
  end
  else
    SpDrawXPToolbar(Self, ACanvas, ARect, PaintOnNCArea, TabBackgroundBorders and (T <> sknNone), skncTabToolbar);
end;

{-------------------------------------------------------------------------------
  Handle left double click
-------------------------------------------------------------------------------}
procedure TCESpTabToolbar.WMLButtonDblClick(var Message: TWMMouse);
begin
  inherited;
  if assigned(OnMouseUp) then
  OnMouseUp(Self, mbLeft, [ssLeft, ssDouble], Message.XPos, Message.YPos);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  IsTabCloseButtonVisible
-------------------------------------------------------------------------------}
function TCESpTabItemViewer.IsTabCloseButtonVisible: Boolean;
var
  T: TSpTBXTabToolbar;
begin
  Result := False;
  if IsOnTabToolbar then begin
    T := TSpTBXTabToolbar(View.Window);
    case T.TabCloseButton of
      tcbNone:
        Exit;
      tcbActive:
        if not Item.Checked then Exit;
    end;
    Result := True;
  end;
end;

end.
