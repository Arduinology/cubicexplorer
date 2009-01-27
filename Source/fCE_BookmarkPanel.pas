unit fCE_BookmarkPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_GlobalCtrl, CE_Bookmarks, CE_BookmarkTree, dCE_Images,
  CE_SettingsIntf, CE_Settings, CEJvDockVSNetStyleTBX, CE_Layout,
  // JVCL
  JvDockVSNetStyle,
  // VSTools
  VirtualTrees, VirtualExplorerTree, MPCommonUtilities, MPShellUtilities,
  VirtualShellNotifier, VirtualResources,
  // PNG Controls
  PngImageList,
  // Toolbar2k, SpTBX
  TB2Dock, TB2Item, SpTBXItem,
  // Tnt Controls
  TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, ImgList, Contnrs, Menus;

type
  TCEBookmarkPanel = class(TCECustomDockableForm, ICESettingsHandler)
    BookmarkPopupMenu: TSpTBXPopupMenu;
    but_addCat: TSpTBXItem;
    but_addBookmark: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    but_rename: TSpTBXItem;
    but_delete: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    but_openAll: TSpTBXSubmenuItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    but_properties: TSpTBXItem;
    but_refresh: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BookmarkPopupMenuPopup(Sender: TObject);
    procedure PopupMenuClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);
    procedure RefreshBookmarks(OnlyIfLocal: Boolean = false);
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
    procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
  public
    BookmarkMenuItems: TComponentList;
    BookmarksPath: WideString;
    BookmarkTree: TCEBookmarkTree;
    procedure LoadBookmarks;
    procedure OnBookmarksChange(Sender: TObject);
    procedure OpenAll(Mode: Integer = 0);
    procedure SaveBookmarks;
  end;

var
  CEBookmarkPanel: TCEBookmarkPanel;

implementation

uses
  dCE_Actions, CE_BookmarkBar, fCE_FileView, CE_VistaFuncs,
  CE_StdBookmarkComps, fCE_BookmarkPropDlg;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called when TCEBookmarkPanel is created
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.FormCreate(Sender: TObject);
begin
  inherited;
  BookmarkMenuItems:= TComponentList.Create(false);
  TopDock.Name:= 'BookmarkPanel_TopDock';
  BottomDock.Name:= 'BookmarkPanel_BottomDock';
  Caption:= 'Bookmarks';
  BookmarkTree:= TCEBookmarkTree.Create(self);
  BookmarkTree.Parent:= self;
  SetDesktopIconFonts(BookmarkTree.Font);
  BookmarkTree.Align:= alClient;
  BookmarkTree.Indent:= 24;
  BookmarkTree.SingleClickMode:= true;
  BookmarkTree.Images:= CE_Images.BookmarkImages;
  ImageList:= CE_Images.SmallIcons;
  ImageIndex:= 18;
  GlobalFocusCtrl.CtrlList.Add(BookmarkTree);
  BookmarkTree.OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  BookmarkTree.OnBookmarksChange:= OnBookmarksChange;
  //BookmarkTree.PopupMenu:= BookmarkPopupMenu;
  BookmarkTree.OnMouseUp:= DoMouseUp;
  GlobalSettings.RegisterHandler(Self);
  ChangeNotifier.RegisterShellChangeNotify(Self);
end;

{*------------------------------------------------------------------------------
  Get's called when TCEBookmarkPanel is Destroyed
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.FormDestroy(Sender: TObject);
begin
  ChangeNotifier.UnRegisterShellChangeNotify(Self);
  BookmarkMenuItems.Free;
  BookmarkTree.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when bookmarks have changed
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.OnBookmarksChange(Sender: TObject);
var
  i: Integer;
  item: TTBCustomItem;
begin
  for i:= 0 to BookmarkMenuItems.Count - 1 do
  begin
    item:= TTBCustomItem(BookmarkMenuItems.Items[i]);
    item.ViewBeginUpdate;
    PopulateBookmarkItem(item, BookmarkTree, item is TSpTBXSubmenuItem);
    item.ViewEndUpdate;
  end;
  SaveBookmarks;
end;

{*------------------------------------------------------------------------------
  Get's called before Popup
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.BookmarkPopupMenuPopup(Sender: TObject);
begin
  if BookmarkTree.SelectedCount > 0 then
  begin
    but_properties.Enabled:= true;
    but_delete.Enabled:= true;
    but_rename.Enabled:=  BookmarkTree.SelectedCount = 1;
    if Assigned(BookmarkTree.FocusedNode) then
    begin
      if BookmarkTree.FocusedNode.ChildCount > 0 then
      but_openAll.Enabled:= true
      else
      but_openAll.Enabled:= false;
    end
    else
    but_openAll.Enabled:= false;
  end
  else
  begin
    but_delete.Enabled:= false;
    but_rename.Enabled:= false;
    but_openAll.Enabled:= false;
    but_properties.Enabled:= false;
  end;
end;

{*------------------------------------------------------------------------------
  Handle popup menu item click.
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.PopupMenuClick(Sender: TObject);
var
  apidl: PItemIDList;
  node: PVirtualNode;
  comp: TCECustomBookComp;
begin
  case TSpTBXItem(Sender).Tag of
    1: begin
         BookmarkTree.AddBookItem('category',BookmarkTree.FocusedNode);
         BookmarkTree.BookmarksChange;
       end;
    2: begin
         if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         begin
           node:= BookmarkTree.AddBookItem('item',BookmarkTree.FocusedNode);
           comp:= BookmarkTree.GetNodeComp(node);
           apidl:= PIDLMgr.CopyPIDL(TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.AbsolutePIDL);
           TCENormalItemComp(comp).LoadFromPIDL(apidl);
           BookmarkTree.BookmarksChange;
         end;
       end;
    3: BookmarkTree.EditSelectedNode;
    4: BookmarkTree.SafeDeleteSelectedNodes;
    5: OpenAll;
    6: OpenAll(1);
    7: OpenAll(2);
    8: begin
         if assigned(BookmarkTree.FocusedNode) then
         begin
           comp:= BookmarkTree.GetNodeComp(BookmarkTree.FocusedNode);
           ShowBookmarkPropDlg(comp,BookmarkTree);
         end;
       end;
    9: RefreshBookmarks;
  end;
end;

{*------------------------------------------------------------------------------
  Open all bookmarks

  0 = Only Folders
  1 = Open Folders and Launch Files
  2 = Launch only files
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.OpenAll(Mode: Integer = 0);
var
  node, chNode: PVirtualNode;
  comp: TCECustomBookComp;
begin
  if BookmarkTree.SelectedCount = 1 then
  begin
    node:= BookmarkTree.GetFirstSelected;
    chNode:= node.FirstChild;
    while assigned(chNode) do
    begin
      comp:= BookmarkTree.GetNodeComp(chNode);
      if TCENormalItemComp(comp).IsFolder then
      begin
        if Mode < 2 then
        comp.MouseClick([ssShift,ssMiddle],mbMiddle)
      end
      else if Mode > 0 then
      begin
        comp.MouseClick([ssDouble, ssLeft], mbLeft);
      end;
      chNode:= chNode.NextSibling;
    end;
  end
  else
  begin
    node:= BookmarkTree.GetFirstSelected;
    while assigned(node) do
    begin
      comp:= BookmarkTree.GetNodeComp(node);
      if comp is TCENormalItemComp then
      begin
        if TCENormalItemComp(comp).IsFolder then
        begin
          if Mode < 2 then
          comp.MouseClick([ssShift,ssMiddle],mbMiddle)
        end
        else if Mode > 0  then
        begin
          comp.MouseClick([ssDouble, ssLeft], mbLeft);
        end;
      end;
      node:= BookmarkTree.GetNextSelected(node);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Load Bookmarks
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.LoadBookmarks;
var
  i: Integer;
  item: TTBCustomItem;
begin

  if WideFileExists(BookmarksPath) then
  begin
    BookmarkTree.BeginUpdate;
    try
      BookmarkTree.LoadFromXmlFile(BookmarksPath);
    finally
      BookmarkTree.EndUpdate;
      for i:= 0 to BookmarkMenuItems.Count - 1 do
      begin
        item:= TTBCustomItem(BookmarkMenuItems.Items[i]);
        //item.ViewBeginUpdate;
        PopulateBookmarkItem(item, BookmarkTree, item is TSpTBXSubmenuItem);
        //item.ViewEndUpdate;

      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Save Bookmarks
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.SaveBookmarks;
begin
  if BookmarksPath <> '' then
  begin
    try
      BookmarkTree.SaveToXmlFile(BookmarksPath);
    except
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.LoadFromStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('/BookmarksPanel');
  try
    // Toggles
    BookmarkTree.SingleClickMode:= Storage.ReadBoolean('SingleClickMode',false);
    BookmarkTree.AutoExpand:= Storage.ReadBoolean('AutoExpand',false);
    BookmarkTree.AutoCollapse:= Storage.ReadBoolean('AutoCollapse',false);
    OpenBookmarkInNewTabByDefault:= Storage.ReadBoolean('OpenInNewTab',false);
  finally
    Storage.ClosePath;
  end;
end;

procedure TCEBookmarkPanel.DoMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  b: Boolean;
  p: TPoint;
begin
  if Button = mbRight then
  begin
    GetCursorPos(p);
    b:= CEDockStyle.ChannelOption.MouseleaveHide;
    CEDockStyle.ChannelOption.MouseleaveHide:= false;
    BookmarkPopupMenu.Popup(p.X, p.Y);
    CEDockStyle.ChannelOption.MouseleaveHide:= b;
  end;
end;

{-------------------------------------------------------------------------------
  Save to storage
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.SaveToStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('/BookmarksPanel');
  try
    // Toggles
    Storage.WriteBoolean('SingleClickMode', BookmarkTree.SingleClickMode);
    Storage.WriteBoolean('AutoExpand', BookmarkTree.AutoExpand);
    Storage.WriteBoolean('AutoCollapse', BookmarkTree.AutoCollapse);
    Storage.WriteBoolean('OpenInNewTab', OpenBookmarkInNewTabByDefault);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Refresh Bookmarks
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.RefreshBookmarks(OnlyIfLocal: Boolean = false);
var
  i: Integer;
  item: TTBCustomItem;
  node: PVirtualNode;
  data: PCEBookData;
begin
  BookmarkTree.BeginUpdate;
  try
    node:= BookmarkTree.GetFirst;
    while node <> nil do
    begin
      data:= BookmarkTree.GetNodeData(Node);
      if assigned(data) then
      begin
        if data.BookComp is TCENormalItemComp then
        begin
          TCENormalItemComp(data.BookComp).Refresh(OnlyIfLocal);
        end;
      end;
      node:= BookmarkTree.GetNext(node);
    end;
  finally
    BookmarkTree.EndUpdate;
    BookmarkTree.Refresh;
    for i:= 0 to BookmarkMenuItems.Count - 1 do
    begin
      item:= TTBCustomItem(BookmarkMenuItems.Items[i]);
      PopulateBookmarkItem(item, BookmarkTree, item is TSpTBXSubmenuItem);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Handle WMShellNotify messages
-------------------------------------------------------------------------------}
procedure TCEBookmarkPanel.WMShellNotify(var Msg: TMessage);
var
  ShellEventList: TVirtualShellEventList;
  ShellEvent: TVirtualShellEvent;
  List: TList;
  i, Count: Integer;
begin
  ShellEventList:= TVirtualShellEventList(Msg.wParam);
  List:= ShellEventList.LockList;
  try
    Count:= List.Count;
    for i:= 0 to Count - 1 do
    begin
      ShellEvent:= TVirtualShellEvent(List.Items[i]);
      case ShellEvent.ShellNotifyEvent of
        vsneDriveAddGUI,
        vsneDriveRemoved,
        vsneUpdateImage,
        vsneDriveAdd: RefreshBookmarks(false);
        //vsneUpdateImage, vsneUpdateDir: RefreshBookmarks(true);
      end;
    end;
  finally
    ShellEventList.UnlockList;
    ShellEventList.Release;
  end;
end;

end.
