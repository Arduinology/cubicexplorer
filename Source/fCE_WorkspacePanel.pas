unit fCE_WorkspacePanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_FileView, CE_Toolbar, dCE_Images,
  // SpTBX
  TB2Dock, SpTBXItem, TB2Toolbar, TB2Item,
  // VSTools
  MPShellUtilities, VirtualExplorerEasyListview, VirtualExplorerTree,
  EasyListview,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, TntMenus;

const
  WM_ActivateWorkspace = WM_USER + 155;

type
  TCEWorkspacePanelSettings = class;

  TCEWorkspacePanel = class(TCECustomDockableForm)
    WorkspaceToolbar: TCEToolbar;
    combo_path: TVirtualExplorerCombobox;
    item_path_combo: TTBControlItem;
    CEToolbarStretcherItem1: TCEToolbarStretcherItem;
    BackgroundCMItems_up: TTntPopupMenu;
    View1: TTntMenuItem;
    item_large_icons: TTntMenuItem;
    item_small_icons: TTntMenuItem;
    item_list: TTntMenuItem;
    item_details: TTntMenuItem;
    item_tiles: TTntMenuItem;
    item_thumbnails: TTntMenuItem;
    item_filmstrip: TTntMenuItem;
    N1: TTntMenuItem;
    MenuItem_ArragneBy: TTntMenuItem;
    MenuItem_GroupBy: TTntMenuItem;
    item_refresh: TTntMenuItem;
    N2: TTntMenuItem;
    Paste1: TTntMenuItem;
    PasteShortcut1: TTntMenuItem;
    CopyPath1: TTntMenuItem;
    N4: TTntMenuItem;
    CreateSymbolicLink1: TTntMenuItem;
    N5: TTntMenuItem;
    N3: TTntMenuItem;
    item_per_folder: TTntMenuItem;
    procedure BackgroundCMItems_upPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure combo_pathPathChange(Sender: TCustomVirtualExplorerCombobox;
      SelectedNamespace: TNamespace);
    procedure ContextMenuClick(Sender: TObject);
  private
    fDownShiftState: TShiftState;
    fShowItemContextMenu: Boolean;
    { Private declarations }
  protected
    fSettings: TCEWorkspacePanelSettings;
    procedure ContextMenuUpdate(AItem: TMenuItem); virtual;
    procedure DoAssigneByClick(Sender: TObject); virtual;
    procedure DoGroupByClick(Sender: TObject); virtual;
    procedure DoOpenMenuBookmarkClick(Sender: TObject); virtual;
    procedure DoOpenMenuBookmarkPopup(Sender: TTBCustomItem; FromLink: Boolean);
        virtual;
    procedure DoOpenMenuTabClick(Sender: TObject); virtual;
    procedure OnGetStorage(Sender: TCustomVirtualExplorerEasyListview; var Storage:
        TRootNodeStorage);
    procedure WMActivateWorkspace(var Message: TMessage); message
        WM_ActivateWorkspace;
  public
    FileView: TCEFileView;
    procedure DoFormHide; override;
    procedure DoFormShow; override;
    procedure DoStartUp; override;
    procedure OnContextMenu(Sender: TCustomEasyListview; MousePt: TPoint; var
        Handled: Boolean);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);
    procedure PopulateOpenMenuItem(AItem: TTBCustomItem);
    procedure RootChange(Sender: TCustomVirtualExplorerEasyListview);
    property Settings: TCEWorkspacePanelSettings read fSettings;
  end;

  TCEWorkspacePanelSettings = class(TPersistent)
  protected
    function GetPerFolderSettings: Boolean;
    function GetViewStyle: TEasyListStyle;
    procedure SetPerFolderSettings(const Value: Boolean);
    procedure SetViewStyle(const Value: TEasyListStyle);
  public
    WorkspacePanel: TCEWorkspacePanel;
    constructor Create;
  published
    property PerFolderSettings: Boolean read GetPerFolderSettings write
        SetPerFolderSettings;
    property ViewStyle: TEasyListStyle read GetViewStyle write SetViewStyle;
  end;

var
  CEWorkspacePanel: TCEWorkspacePanel;

implementation

uses
  CE_GlobalCtrl, MPCommonUtilities,
  CE_VistaFuncs, CE_Utils, fCE_FileView, Main, dCE_Actions, CE_Layout,
  CE_ToolbarButtons, CE_SpTabBar, CE_LanguageEngine, fCE_BookmarkPanel,
  VirtualTrees, CE_Bookmarks, CE_StdBookmarkComps, CE_ContextMenu,
  CE_AppSettings;

{$R *.dfm}

{##############################################################################}

{-------------------------------------------------------------------------------
  On BackgroundCMItems_up Popup
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.BackgroundCMItems_upPopup(Sender: TObject);
var
  item: TMenuItem;
  col: TEasyColumn;
begin
  MenuItem_ArragneBy.Clear;
  col:= FileView.Header.FirstVisibleColumn;
  while assigned(col) do
  begin
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= col.Caption;
    item.OnClick:= DoAssigneByClick;
    item.Tag:= Integer(col);
    item.RadioItem:= true;
    if col.SortDirection <> esdNone then
    item.Checked:= true;
    MenuItem_ArragneBy.Add(item);
    col:= FileView.Header.NextVisibleColumn(col);
  end;
  item:= BackgroundCMItems_up.CreateMenuItem;
  item.Caption:= '-';
  item.Tag:= 0;
  MenuItem_ArragneBy.Add(item);

  item:= BackgroundCMItems_up.CreateMenuItem;
  item.Caption:= _('More...');
  item.Tag:= -1;
  item.OnClick:= DoAssigneByClick;
  MenuItem_ArragneBy.Add(item);

  // Group By
    // Sho in Groups
  MenuItem_GroupBy.Clear;
  item:= BackgroundCMItems_up.CreateMenuItem;
  item.Caption:= _('Show in Groups');
  item.Checked:= FileView.Grouped;
  item.Tag:= -2;
  item.OnClick:= DoGroupByClick;
  MenuItem_GroupBy.Add(item);
    // separator
  item:= BackgroundCMItems_up.CreateMenuItem;
  item.Caption:= '-';
  item.Tag:= 0;
  MenuItem_GroupBy.Add(item);
    // group by items
  col:= FileView.Header.FirstVisibleColumn;
  while assigned(col) do
  begin
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= col.Caption;
    item.OnClick:= DoGroupByClick;
    item.Tag:= Integer(col);
    item.RadioItem:= true;
    item.Checked:= FileView.GroupingColumn = col.Index;
    MenuItem_GroupBy.Add(item);
    col:= FileView.Header.NextVisibleColumn(col);
  end;
    // separator
  item:= BackgroundCMItems_up.CreateMenuItem;
  item.Caption:= '-';
  item.Tag:= 0;
  MenuItem_GroupBy.Add(item);

  item:= BackgroundCMItems_up.CreateMenuItem;
  item.Caption:= _('More...');
  item.Tag:= -1;
  item.OnClick:= DoGroupByClick;
  MenuItem_GroupBy.Add(item);
end;

{*------------------------------------------------------------------------------
  Get's called when form gets hidden.
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoFormHide;
begin
  inherited;
  FileView.Active:= false;
end;

{*------------------------------------------------------------------------------
  Get's called when form gets shown.
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoFormShow;
begin
  inherited;
  Application.ProcessMessages;
  PostMessage(Handle, WM_ActivateWorkspace, 1,0);
  WorkspaceToolbar.Realign;
end;

{-------------------------------------------------------------------------------
  On TCEWorkspacePanel Create
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.FormCreate(Sender: TObject);
begin
  FileView:= TCEFileView.Create(Self);
  FileView.Parent:= self;
  FileView.Align:= alClient;
  FileView.Themed:= false;
  FileView.BorderStyle:= bsNone;
  FileView.BoundsRect:= Rect(0,0, self.ClientWidth, self.ClientHeight);
  FileView.FileObjects:= [foFolders,
                          foNonFolders];
  FileView.DragManager.MouseButton:= [cmbLeft,cmbRight];
  FileView.Selection.MouseButton:= [cmbLeft,cmbRight];
  FileView.ParentShowHint:= true;
  FileView.HintType:= ehtText;
  FileView.PaintInfoGroup.BandThickness:= 2;
  FileView.PaintInfoGroup.BandColor:= clWindowText;
  FileView.PaintInfoGroup.BandColorFade:= clWindow;
  FileView.CompressedFile.Hilight:= false;
  FileView.EncryptedFile.Hilight:= false;
  FileView.GroupFont.Style:= [fsBold];
  FileView.TranslateHeader:= false;
  FileView.TabOrder:= 1;
  FileView.PerFolderSettings:= false;
  FileView.View:= elsReport;

  FileView.OnGetStorage:= OnGetStorage;
  FileView.OnRootChange:= RootChange;
  FileView.OnMouseDown:= OnMouseDown;
  FileView.OnMouseUp:= OnMouseUp;
  FileView.OnContextMenu:= OnContextMenu;
  
  GlobalFocusCtrl.CtrlList.Add(FileView);
  FileView.OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  SetDesktopIconFonts(FileView.Font);
  GlobalFileViewSettings.RegisterNotify(FileView);

  //CELayoutItems.Add(WorkspaceToolbar);

  // Settings
  fSettings:= TCEWorkspacePanelSettings.Create;
  fSettings.WorkspacePanel:= Self;
  GlobalAppSettings.AddItem('WorkspacePanel', fSettings, true);
end;

{-------------------------------------------------------------------------------
  On TCEWorkspacePanel Destroy
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.FormDestroy(Sender: TObject);
begin
  fSettings.Free;
end;

{-------------------------------------------------------------------------------
  On Get Storage
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.OnGetStorage(Sender:
    TCustomVirtualExplorerEasyListview; var Storage: TRootNodeStorage);
begin
  Storage:= GlobalFileViewSettings.Storage;
end;

{-------------------------------------------------------------------------------
  Activate Workspace
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.WMActivateWorkspace(var Message: TMessage);
begin
  inherited;
  FileView.Active:= Message.WParam = 1;
  if FileView.PerFolderSettings and FileView.Active then
  FileView.LoadFolderFromPropertyBag(true);
  combo_path.Active:= FileView.Active;
end;

{-------------------------------------------------------------------------------
  On combo_path.PathChange
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.combo_pathPathChange(
  Sender: TCustomVirtualExplorerCombobox; SelectedNamespace: TNamespace);
begin
  FileView.BrowseToByPIDL(SelectedNamespace.AbsolutePIDL)
end;

{-------------------------------------------------------------------------------
  Handle ContextMenuClick
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.ContextMenuClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    100..107: begin
      FileView.View:= TEasyListStyle(TComponent(Sender).Tag - 100);
    end;
    110: FileView.PerFolderSettings:= not FileView.PerFolderSettings;
    606: FileView.Rebuild(true);  
  end;
end;

{-------------------------------------------------------------------------------
  Handle ContextMenuUpdate
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.ContextMenuUpdate(AItem: TMenuItem);
begin
  case AItem.Tag of
    100..107: begin
      AItem.Checked:= Ord(FileView.View) = AItem.Tag - 100;
    end;
    110: AItem.Checked:= FileView.PerFolderSettings;
  end;
end;

{-------------------------------------------------------------------------------
  Do AssigneBy Click
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoAssigneByClick(Sender: TObject);
var
  item: TMenuItem;
  col, tmpCol: TEasyColumn;
  view: TCEFileView;
begin
  item:= TMenuItem(Sender);
  if item.Tag = -1 then
  begin
    FileView.ShowHeaderSelector;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    view:= TCEFileView(col.OwnerListview);
    view.BeginUpdate;
    try
      tmpCol:= view.Header.FirstColumn;
      while assigned(tmpCol) do
      begin
        if tmpCol <> col then
        tmpCol.SortDirection:= esdNone
        else
        tmpCol.SortDirection:= esdAscending;
        tmpCol:= view.Header.NextColumn(tmpCol);
      end;
    finally
      view.EndUpdate(true);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do GroupBy Click
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoGroupByClick(Sender: TObject);
var
  item: TMenuItem;
  col: TEasyColumn;
begin
  item:= TMenuItem(Sender);
  if item.Tag = -2 then
  begin
    FileView.Grouped:= not FileView.Grouped;
  end
  else if item.Tag = -1 then
  begin
    FileView.ShowHeaderSelector;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    if assigned(col) then
    FileView.GroupingColumn:= col.Index;
  end;
end;

{-------------------------------------------------------------------------------
  Do OpenMenuBookmarkClick
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoOpenMenuBookmarkClick(Sender: TObject);
var
  data: PCEBookData;
begin
  // not safe!
  data:= CEBookmarkPanel.BookmarkTree.GetNodeData(PVirtualNode(TSpTBXItem(Sender).Tag));
  FileView.BrowseToByPIDL(TCENormalItemComp(data.BookComp).Namespace.AbsolutePIDL);
  WorkspaceToolbar.Repaint;
end;

{-------------------------------------------------------------------------------
  Do OpenMenuTabClick
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoOpenMenuTabClick(Sender: TObject);
var
  tab: TCESpTabItem;
begin
  tab:= TCESpTabItem(TSpTBXItem(Sender).Tag);
  if MainForm.TabSet.Items.IndexOf(tab) > -1 then // validate
  begin
    if tab.Page is TCEFileViewPage then
    begin
      FileView.BrowseToByPIDL(TCEFileViewPage(tab.Page).FileView.RootFolderNamespace.AbsolutePIDL);
    end;
  end;
  WorkspaceToolbar.Repaint;
end;

{-------------------------------------------------------------------------------
  Do OpenMenuBookmarkPopup
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoOpenMenuBookmarkPopup(Sender: TTBCustomItem;
  FromLink: Boolean);
var
  item: TSpTBXItem;
  sub: TSpTBXSubmenuItem;
  chNode: PVirtualNode;
  data: PCEBookData;
begin
  Sender.Clear;
  
  // root
  if Sender.Tag = 0 then
  chNode:= CEBookmarkPanel.BookmarkTree.GetFirst(false)
  else
  chNode:= CEBookmarkPanel.BookmarkTree.GetFirstChild(PVirtualNode(Sender.Tag));

  while assigned(chNode) do
  begin
    data:= CEBookmarkPanel.BookmarkTree.GetNodeData(chNode);
    // category
    if data.BookComp is TCECategoryComp then
    begin
      sub:= TSpTBXSubmenuItem.Create(Sender);
      sub.Caption:= data.BookComp.Title;
      sub.Images:= data.BookComp.ImageList;
      sub.ImageIndex:= data.BookComp.GetImageIndex(false, false);
      sub.DropdownCombo:= false;
      sub.Tag:= Integer(chNode);
      sub.OnPopup:= DoOpenMenuBookmarkPopup;
      Sender.Add(sub);
    end
    // normal item
    else if data.BookComp is TCENormalItemComp then
    begin
      if TCENormalItemComp(data.BookComp).Namespace.Folder then
      begin
        if CEBookmarkPanel.BookmarkTree.HasChildren[chNode] then
        begin
          sub:= TSpTBXSubmenuItem.Create(Sender);
          sub.DropdownCombo:= true;
          sub.OnPopup:= DoOpenMenuBookmarkPopup;
          item:= sub;
        end
        else
        begin
          item:= TSpTBXItem.Create(Sender);
        end;
        item.Caption:= data.BookComp.Title;
        item.Images:= data.BookComp.ImageList;
        item.ImageIndex:= data.BookComp.GetImageIndex(false, false);
        item.Tag:= Integer(chNode);
        item.OnClick:= DoOpenMenuBookmarkClick;
        Sender.Add(item);
      end;
    end;
    chNode:= CEBookmarkPanel.BookmarkTree.GetNextSibling(chNode);
  end;
end;


{-------------------------------------------------------------------------------
  Do StartUp
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoStartUp;
var
  item: TSpTBXCustomItem;
  sep: TCEToolbarSeparatorItem;
begin
  // Create default toolbar items
  // - back
  item:= TSpTBXItem.Create(WorkspaceToolbar);
  item.Action:= CEActions.act_workspace_back;
  WorkspaceToolbar.Items.Insert(0, item);
  // - forward
  item:= TSpTBXItem.Create(WorkspaceToolbar);
  item.Action:= CEActions.act_workspace_forward;
  WorkspaceToolbar.Items.Insert(1, item);
  // - folder up
  item:= TSpTBXItem.Create(WorkspaceToolbar);
  item.Action:= CEActions.act_workspace_folder_up;
  WorkspaceToolbar.Items.Insert(2, item);
  // - path combo
  //--- created at design time ---
  // - open
  item:= TCEWorkspaceOpenButton.Create(WorkspaceToolbar);
  item.Action:= CEActions.act_workspace_open;
  WorkspaceToolbar.Items.Add(item);


  // Popup
  WorkspaceToolbar.PopupMenu:= MainForm.ToolbarPopupMenu;
end;

{*------------------------------------------------------------------------------
  Get's called on background context menu
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.OnContextMenu(Sender: TCustomEasyListview; MousePt:
    TPoint; var Handled: Boolean);

  procedure EnumCMItems(AItem: TMenuItem);
  var
    i: Integer;
  begin
    for i:= 0 to AItem.Count - 1 do
    begin
      ContextMenuUpdate(AItem.Items[i]);
      EnumCMItems(AItem.Items[i]);
    end;
  end;

var
  menu: TCEBackContextMenu;
  item: TEasyItem;
begin
  if fShowItemContextMenu then
  begin
    CEActions.UpdateAll;
    menu:= TCEBackContextMenu.Create;
    try
      EnumCMItems(BackgroundCMItems_up.Items);

      menu.UpperMenuItems:= BackgroundCMItems_up;
      menu.LowerMenuItems:= CEActions.BackgroundCMItems_down;

      if menu.ShowMenu(MousePt, FileView.RootFolderNamespace, FileView) then
      begin
        item:= TCEFileViewHack(FileView).RereadAndRefresh(False);
        if Assigned(item) then
        begin
          FileView.Selection.SelectRange(item,item,false,true);
          FileView.Selection.FocusedItem:= item;
          item.Edit(nil);
        end;
      end;
    finally
      menu.Free;
      Handled:= true;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on item mouse down.
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.OnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  fDownShiftState:= Shift;

  fShowItemContextMenu:= true;

  if Shift = [ssRight, ssAlt] then
  fShowItemContextMenu:= false
  else if FileView.UseMouseRocker then
  begin
    if (Button = mbLeft) and FileView.RightMouseButton_IsDown then
    fShowItemContextMenu:= false
    else if (Button = mbRight) and FileView.LeftMouseButton_IsDown then
    fShowItemContextMenu:= false
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on item mouse up.
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.OnMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  NS, targetNS: TNamespace;
  item: TEasyItem;
  WindowPt: TPoint;
  freeTarget: Boolean;
begin
  // open new tab
  if (ssMiddle in fDownShiftState) or ((ssLeft in fDownShiftState) and (ssAlt in Shift)) then
  begin
    WindowPt := FileView.Scrollbars.MapWindowToView(Point(X,Y));
    item:= FileView.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not FileView.EditManager.Editing then
    begin
      FileView.EditManager.EndEdit;
      FileView.ValidateNamespace(Item,NS);
      if assigned(NS) then
      begin
        if NS.Link and assigned(NS.ShellLink) then
        begin
          targetNS:= TNamespace.Create(PIDLMgr.CopyPIDL(NS.ShellLink.TargetIDList), nil);
          freeTarget:= true;
        end
        else
        begin
          targetNS:= NS;
          freeTarget:= false;
        end;

        try
          if targetNS.FileSystem and not targetNS.Folder then
          begin
            if ssShift in Shift then
            OpenFileInTab(targetNS.NameForParsing, not MainForm.TabSet.Settings.OpenTabSelect)
            else
            OpenFileInTab(targetNS.NameForParsing, MainForm.TabSet.Settings.OpenTabSelect)
          end
          else
          begin
            if ssShift in Shift then
            OpenFolderInTab(Self, targetNS.AbsolutePIDL, not MainForm.TabSet.Settings.OpenTabSelect)
            else
            OpenFolderInTab(Self, targetNS.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect)
          end;
        finally
          if freeTarget then
          targetNS.Free;
        end;            
      end;
    end;
  end
  // show property sheet
  else if (ssRight in fDownShiftState) and (Shift = [ssAlt]) then
  begin
    WindowPt := FileView.Scrollbars.MapWindowToView(Point(X,Y));
    item:= FileView.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not FileView.EditManager.Editing then
    begin
      FileView.ValidateNamespace(Item,NS);
      if assigned(NS) then
      begin
        NS.ShowPropertySheet(MainForm);
      end;
    end;
    fShowItemContextMenu:= false;
  end;

  fDownShiftState:= [];
end;

{-------------------------------------------------------------------------------
  PopulateOpenMenuItem
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.PopulateOpenMenuItem(AItem: TTBCustomItem);
var
  i, idx: Integer;
  item: TSpTBXItem;
  sep: TSpTBXSeparatorItem;
  tab: TCESpTabItem;
begin
  AItem.Clear;
  // add tab items
  idx:= 0;
  for i:= 0 to MainForm.TabSet.Items.Count - 1 do
  begin
    if MainForm.TabSet.Items.Items[i] is TCESpTabItem then                      
      
    begin
      idx:= idx + 1;
      tab:= TCESpTabItem(MainForm.TabSet.Items.Items[i]);
      if tab.Page is TCEFileViewPage then
      begin
        item:= TSpTBXItem.Create(AItem);
        item.Caption:= tab.Caption;
        item.Hint:= tab.Hint;
        item.Options:= [tboShowHint];
        item.Alignment:= taLeftJustify;
        item.Images:= tab.Images;
        item.ImageIndex:= tab.ImageIndex;
        item.Tag:= Integer(tab);
        item.OnClick:= DoOpenMenuTabClick;
        AItem.Add(item);
      end;
    end;
  end;
  // add separator
  sep:= TSpTBXSeparatorItem.Create(AItem);
  AItem.Add(sep);
  // add bookmark item
  item:= TSpTBXSubmenuItem.Create(AItem);
  item.Caption:= _('Bookmarks');
  item.Tag:= 0;
  item.OnPopup:= DoOpenMenuBookmarkPopup;
  item.Images:= CE_Images.SmallIcons;
  item.ImageIndex:= 18;
  AItem.Add(item);
end;

{*------------------------------------------------------------------------------
  Get's called when root path is changed
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.RootChange(Sender:
    TCustomVirtualExplorerEasyListview);
begin
  combo_path.ChangeLinkChanging(FileView, FileView.RootFolderNamespace.AbsolutePIDL);
end;

{##############################################################################}
// TCEWorkspacePanelSettings

{-------------------------------------------------------------------------------
  Create an instance of TCEWorkspacePanelSettings
-------------------------------------------------------------------------------}
constructor TCEWorkspacePanelSettings.Create;
begin
  inherited Create;
end;

{-------------------------------------------------------------------------------
  Get PerFolderSettings
-------------------------------------------------------------------------------}
function TCEWorkspacePanelSettings.GetPerFolderSettings: Boolean;
begin
  Result:= WorkspacePanel.FileView.PerFolderSettings;
end;

{-------------------------------------------------------------------------------
  Get ViewStyle
-------------------------------------------------------------------------------}
function TCEWorkspacePanelSettings.GetViewStyle: TEasyListStyle;
begin
  Result:= WorkspacePanel.FileView.View;
end;

{-------------------------------------------------------------------------------
  Set PerFolderSettings
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanelSettings.SetPerFolderSettings(const Value: Boolean);
begin
  WorkspacePanel.FileView.PerFolderSettings:= Value;
end;

{-------------------------------------------------------------------------------
  Set ViewStyle
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanelSettings.SetViewStyle(const Value: TEasyListStyle);
begin
  WorkspacePanel.FileView.View:= Value;
end;


end.
