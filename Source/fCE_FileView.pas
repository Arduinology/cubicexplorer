unit fCE_FileView;

interface

uses
  // CE Units
  fCE_TabPage, CE_FileView, CE_GlobalCtrl, CE_QuickView, CE_Utils,
  dCE_Images, CE_ContextMenu, CE_LanguageEngine, CE_SettingsIntf, CE_Settings,
  // EasyListview
  EasyListview, 
  // VSTools
  VirtualExplorerEasyListview, MPCommonObjects, MPShellUtilities,
  VirtualExplorerTree, MPCommonUtilities, ColumnFormSpTBX,
  // VT
  VirtualTrees,
  // SpTBX
  SpTBXItem, SpTBXControls, SpTBXDkPanels,
  // TB2K
  TB2Item,
  // Tnt Controls
  TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, Menus, JvAppStorage, Contnrs;

type

  TEasyEditManagerHack = class(TEasyEditManager);

  TCEFileViewHack = class(TCEFileView);

  TCEFileViewPage = class(TCECustomTabPage)
    QuickViewPopupMenu: TSpTBXPopupMenu;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    but_thumbpos_left: TSpTBXItem;
    but_thumbpos_top: TSpTBXItem;
    but_thumbpos_right: TSpTBXItem;
    but_thumbpos_bottom: TSpTBXItem;
    QuickViewSplitter: TSpTBXSplitter;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    but_thumbstyle_icon: TSpTBXItem;
    but_thumbstyle_smallicon: TSpTBXItem;
    but_thumbstyle_list: TSpTBXItem;
    but_thumbstyle_details: TSpTBXItem;
    but_thumbstyle_tiles: TSpTBXItem;
    but_thumbstyle_thumbnails: TSpTBXItem;
    but_thumbstyle_filmstrip: TSpTBXItem;
    procedure ThumbPositionClick(Sender: TObject);
    procedure QuickViewPopupMenuPopup(Sender: TObject);
    procedure ThumbViewStyleClick(Sender: TObject);
    procedure QuickViewSplitterMoved(Sender: TObject);
  private
    fDownShiftState: TShiftState;
    fPathChanging: Boolean;
    fShowItemContextMenu: Boolean;
    fThumbPosition: TAlign;
    fViewStyle: TEasyListStyle;
    fThumbViewStyle: TEasyListStyle;
    procedure SetThumbPosition(const Value: TAlign);
    procedure SetViewStyle(const Value: TEasyListStyle);
    procedure SetThumbViewStyle(const Value: TEasyListStyle);
  protected
    fQuickView: TCEQuickView;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
    procedure OnItemContextMenu(Sender: TCustomEasyListview; HitInfo:
        TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu; var Handled:
        Boolean);
    procedure SetActive(const Value: Boolean); override;
  public
    FileView: TCEFileView;
    ThumbViewSize: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure RootChanging(Sender: TCustomVirtualExplorerEasyListview; const
        NewValue: TRootFolder; const CurrentNamespace, Namespace: TNamespace; var
        Allow: Boolean);
    procedure RootRebuild(Sender: TCustomVirtualExplorerEasyListview);
    procedure RootChange(
      Sender: TCustomVirtualExplorerEasyListview);
    procedure SelectPage; override;
    procedure UpdateCaption; override;
    procedure ColumnSizeChanged(
      Sender: TCustomEasyListview; Column: TEasyColumn);
    procedure HidePage; override;
    procedure LoadFromStorage(Storage: ICESettingsStorage); override; stdcall;
    procedure OnContextMenu(Sender: TCustomEasyListview; MousePt: TPoint; var
        Handled: Boolean);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);
    procedure SaveToStorage(Storage: ICESettingsStorage); override; stdcall;
    procedure ShowHeaderSelector;
    property ThumbPosition: TAlign read fThumbPosition write SetThumbPosition;
    property ViewStyle: TEasyListStyle read fViewStyle write SetViewStyle;
    property ThumbViewStyle: TEasyListStyle read fThumbViewStyle write
        SetThumbViewStyle;
  end;

  TCEFileViewSettings = class(TInterfacedObject, ICESettingsHandler)
  private
    fFullRowSelect: Boolean;
    fHiddenFiles: Boolean;
    fSelectPreviousFolder: Boolean;
    fAutoSelectFirstItem: Boolean;
    fShowExtensions: Boolean;
    fShowHeaderAlways: Boolean;
    fUpdating: Boolean;
    NotifyList: TComponentList;
    procedure SetFullRowSelect(const Value: Boolean);
    procedure SetHiddenFiles(const Value: Boolean);
    procedure SetSelectPreviousFolder(const Value: Boolean);
    procedure SetAutoSelectFirstItem(const Value: Boolean);
    procedure SetShowExtensions(const Value: Boolean);
    procedure SetShowHeaderAlways(const Value: Boolean);
    procedure SetSmoothScroll(const Value: Boolean);
  public
    CellSizes: Array [0..6] of TPoint;
    Columns_ControlPanel: TCEColSettings;
    Columns_Default: TCEColSettings;
    Columns_MyComputer: TCEColSettings;
    Columns_Network: TCEColSettings;
    fSmoothScroll: Boolean;
    GroupBy_ControlPanel: TCEGroupBySetting;
    GroupBy_Default: TCEGroupBySetting;
    GroupBy_MyComputer: TCEGroupBySetting;
    GroupBy_Network: TCEGroupBySetting;
    ViewStyle: TEasyListStyle;
    ThumbsPosition: TAlign;
    ThumbViewSize: Integer;
    ThumbViewStyle: TEasyListStyle;
    constructor Create;
    destructor Destroy; override;
    procedure AssignToFileView(FileViewPage: TCEFileViewPage);
    procedure AssignFromFileView(FileViewPage: TCEFileViewPage);
    procedure AssignFromActivePage;
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure RegisterNotify(FileViewPage: TComponent);
    procedure AssignColumnSettingsTo(FileView: TVirtualExplorerEasyListview);
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
    procedure SendChanges;
    procedure AssignColumnSettingsFrom(FileView: TVirtualExplorerEasyListview);
    property FullRowSelect: Boolean read fFullRowSelect write SetFullRowSelect;
    property HiddenFiles: Boolean read fHiddenFiles write SetHiddenFiles;
    property ShowExtensions: Boolean read fShowExtensions write SetShowExtensions;
    property ShowHeaderAlways: Boolean read fShowHeaderAlways write
        SetShowHeaderAlways;
    property SmoothScroll: Boolean read fSmoothScroll write SetSmoothScroll;
  published
    property SelectPreviousFolder: Boolean read fSelectPreviousFolder write
        SetSelectPreviousFolder;
    property AutoSelectFirstItem: Boolean read fAutoSelectFirstItem write
        SetAutoSelectFirstItem;
  end;

var
  GlobalFileViewSettings: TCEFileViewSettings;

implementation

{$R *.dfm}

uses
  dCE_Actions, Main, CE_VistaFuncs, fCE_FolderPanel, CE_CommonObjects;

{*------------------------------------------------------------------------------
  Get's called when TCEFileViewPage is created.
-------------------------------------------------------------------------------}
constructor TCEFileViewPage.Create(AOwner: TComponent);
begin
  inherited;
  ThumbViewSize:= 100;
  fThumbPosition:= alBottom;
  fThumbViewStyle:= elsFilmStrip;
  Layout:= 'FileView';
  Images:= SmallSysImages;
  FileView:= TCEFileView.Create(Self);
  FileView.Parent:= self;
  FileView.Align:= alClient;
  FileView.Themed:= false;
  FileView.BorderStyle:= bsNone;
  FileView.BoundsRect:= Rect(0,0, self.ClientWidth, self.ClientHeight);
  FileView.OnRootRebuild:= RootRebuild;
  FileView.OnRootChanging:= RootChanging;
  FileView.OnRootChange:= RootChange;
  FileView.OnItemSelectionsChanged:= ItemSelectionsChanged;
  FileView.OnColumnSizeChanged:= ColumnSizeChanged;
  FileView.OnContextMenu:= OnContextMenu;
  FileView.OnItemContextMenu:= OnItemContextMenu;
  FileView.OnMouseDown:= OnMouseDown;
  FileView.OnMouseUp:= OnMouseUp;
  FileView.FileObjects:= [foFolders,
                          foNonFolders];
  FileView.DragManager.MouseButton:= [cmbLeft,cmbRight];
  FileView.Selection.MouseButton:= [cmbLeft,cmbRight];
  FileView.ParentShowHint:= true;
  FileView.HintType:= ehtText;
  // translate header
  FileView.TranslateHeader:= false;

  FileView.PaintInfoGroup.BandThickness:= 2;
  FileView.PaintInfoGroup.BandColor:= clWindowText;
  FileView.PaintInfoGroup.BandColorFade:= clWindow;
  FileView.GroupFont.Style:= [fsBold];

  GlobalFocusCtrl.CtrlList.Add(FileView);
  FileView.OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  GlobalFileViewSettings.RegisterNotify(Self);
  //GlobalFileViewSettings.AssignToFileView(Self);
  SetDesktopIconFonts(FileView.Font);
  
  GlobalFileViewSettings.AssignToFileView(Self);
end;

{*------------------------------------------------------------------------------
  Get's called when TCEFileViewPage is destoyed.
-------------------------------------------------------------------------------}
destructor TCEFileViewPage.Destroy;
begin
  if GlobalPathCtrl.ActivePage = Self then
  begin
    GlobalPathCtrl.ActivePage:= nil;
  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when Global focus has changed
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
  // Do nothing
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  FileView.BrowseTo(NewPath);
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  fPathChanging:= true;
  FileView.Selection.ClearAll;
  FileView.BrowseToByPIDL(NewPIDL);
  if FileView.Selection.First <> nil then
  FileView.Selection.First.MakeVisible(emvMiddle);
end;

{*------------------------------------------------------------------------------
  Get's called on when item selection has changed
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.ItemSelectionsChanged(Sender: TCustomEasyListview);
var
  NS: TNamespace;
  Item: TEasyItem;
begin
  if FileView.Selection.Count > 1 then
  Item:= FileView.Selection.FocusedItem
  else
  Item:= FileView.Selection.First;
  
  if Assigned(Item) then
  begin
    FileView.ValidateNamespace(Item, NS);
    if assigned(NS) then
    GlobalPathCtrl.ChangeFocusedPath(Self, NS.NameForParsing);
    if assigned(fQuickView) then
    fQuickView.LoadFile(NS.NameForParsing);
  end
  else
  begin
    GlobalPathCtrl.ChangeFocusedPath(Self, '');
    if assigned(fQuickView) then
    fQuickView.CloseFile;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on item mouse down.
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  fDownShiftState:= Shift;
  fShowItemContextMenu:= not (Shift = [ssRight, ssAlt]);
end;

{*------------------------------------------------------------------------------
  Get's called on item mouse up.
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  NS: TNamespace;
  item: TEasyItem;
  WindowPt: TPoint;
begin
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
        if NS.FileSystem and not NS.Folder then
        begin
          if ssShift in Shift then
          OpenFileInTab(NS.NameForParsing, not MainForm.TabSet.OpenTabSelect)
          else
          OpenFileInTab(NS.NameForParsing, MainForm.TabSet.OpenTabSelect)
        end
        else
        begin
          if ssShift in Shift then
          OpenFolderInTab(Self, NS.AbsolutePIDL, not MainForm.TabSet.OpenTabSelect)
          else
          OpenFolderInTab(Self, NS.AbsolutePIDL, MainForm.TabSet.OpenTabSelect)
        end;
      end;
    end;
  end
  else if (ssRight in fDownShiftState) and (Shift = [ssAlt]) then
  begin
    WindowPt := FileView.Scrollbars.MapWindowToView(Point(X,Y));
    item:= FileView.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not FileView.EditManager.Editing then
    begin
      FileView.ValidateNamespace(Item,NS);
      if assigned(NS) then
      begin
        NS.ShowPropertySheet;
      end;
    end;
    fShowItemContextMenu:= false;
  end;

  fDownShiftState:= [];
end;

{*------------------------------------------------------------------------------
  Get's called when root path is changing
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.RootChanging(Sender: TCustomVirtualExplorerEasyListview;
    const NewValue: TRootFolder; const CurrentNamespace, Namespace: TNamespace; var Allow: Boolean);
begin
  if Namespace <> nil then
  begin
    if GlobalPathCtrl.ActivePage = Self then
    begin
      if not fPathChanging then
      GlobalPathCtrl.ChangeGlobalPathPIDL(Self, Namespace.AbsolutePIDL);
      GlobalFileViewSettings.AssignColumnSettingsFrom(FileView);
    end;
  end;
  fPathChanging:= false;
end;

{*------------------------------------------------------------------------------
  Get's called when root path is changed
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.RootChange(
  Sender: TCustomVirtualExplorerEasyListview);
begin
  GlobalFileViewSettings.AssignColumnSettingsTo(FileView);
end;

{*------------------------------------------------------------------------------
  Get's called on root rebuild
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.RootRebuild(Sender:
    TCustomVirtualExplorerEasyListview);
begin
  UpdateCaption;
  GlobalPathCtrl.ChangeGlobalContent(Self);
end;

{*------------------------------------------------------------------------------
  Makes Self as a Active Component in GlobalPathCtrl
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SelectPage;
begin
  inherited;
  // Save old tab's column settings
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  GlobalFileViewSettings.AssignColumnSettingsFrom(TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView);
  // Load column settings
  GlobalFileViewSettings.AssignColumnSettingsTo(FileView);
  // Set Page as active
  GlobalPathCtrl.ActivePage:= Self;
  GlobalPathCtrl.ChangeGlobalPathPIDL(Self, FileView.RootFolderNamespace.AbsolutePIDL);
  GlobalPathCtrl.GlobalPathCaption:= FileView.RootFolderNamespace.NameParseAddress;
  FileView.SetFocus;
end;

{*------------------------------------------------------------------------------
  Set Active value
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetActive(const Value: Boolean);
begin
  inherited;
  FileView.Active:= Value;
end;

{*------------------------------------------------------------------------------
  Update Caption
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.UpdateCaption;
begin
  TabCaption:= FileView.RootFolderNamespace.NameNormal;
  TabItem.Images:= SmallSysImages;
  TabItem.ImageIndex:= FileView.RootFolderNamespace.GetIconIndex(false, icSmall);
  TabItem.Hint:= UTF8Encode(FileView.RootFolderNamespace.NameParseAddress);
  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.GlobalPathCaption:= FileView.RootFolderNamespace.NameParseAddress;
end;

{*------------------------------------------------------------------------------
  Save Column settings
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.ColumnSizeChanged(
  Sender: TCustomEasyListview; Column: TEasyColumn);
begin
  if GlobalPathCtrl.ActivePage = Self then
  GlobalFileViewSettings.AssignColumnSettingsFrom(FileView);
end;

{*------------------------------------------------------------------------------
  Hide page
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.HidePage;
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Set Thumbnail Position
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetThumbPosition(const Value: TAlign);
begin
  if fThumbPosition = Value then Exit;

  case Value of
    alNone, alClient, alCustom: fThumbPosition:= alBottom;
    else
    fThumbPosition:= Value;
  end;

  if fViewStyle = elsFilmStrip then
  begin
    FileView.Align:= fThumbPosition;
    if (fThumbPosition = alTop) or (fThumbPosition = alBottom) then
    begin
      FileView.Height:= ThumbViewSize;
    end
    else
    begin
      FileView.Width:= ThumbViewSize;
    end;
    
    QuickViewSplitter.Align:= fThumbPosition;
    case fThumbPosition of
      alLeft: QuickViewSplitter.Left:= FileView.BoundsRect.Right;
      alRight: QuickViewSplitter.Left:= FileView.BoundsRect.Left-QuickViewSplitter.Width;
      alTop: QuickViewSplitter.Top:= FileView.BoundsRect.Bottom;
      alBottom: QuickViewSplitter.Top:= FileView.BoundsRect.Top-QuickViewSplitter.Height;
    end;
  end;

  GlobalFileViewSettings.AssignFromFileView(Self);
end;

{*------------------------------------------------------------------------------
  Set View Style
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetViewStyle(const Value: TEasyListStyle);
begin
  if fViewStyle = Value then Exit;
  
  fViewStyle:= Value;

  if fViewStyle = elsFilmStrip then
  begin
    if not assigned(fQuickView) then
    begin
      fQuickView:= TCEQuickView.Create(self);
      fQuickView.BorderWidth:= 2;
      fQuickView.Parent:= self;
      fQuickView.Align:= alClient;
      fQuickview.UseThumbImage:= false;
      fQuickview.PopupMenu:= QuickViewPopupMenu;
      fQuickview.Active:= true;
    end;

    FileView.Align:= fThumbPosition;
    FileView.View:= fThumbViewStyle;
    if ThumbViewSize < 20 then
    ThumbViewSize:= 20;
    
    if (fThumbPosition = alTop) or (fThumbPosition = alBottom) then
    begin
      FileView.Height:= ThumbViewSize;
    end
    else
    begin
      FileView.Width:= ThumbViewSize;
    end;
    QuickViewSplitter.Align:= fThumbPosition;
    QuickViewSplitter.Visible:= true;

    case fThumbPosition of
      alLeft: QuickViewSplitter.Left:= FileView.BoundsRect.Right;
      alRight: QuickViewSplitter.Left:= FileView.BoundsRect.Left-QuickViewSplitter.Width;
      alTop: QuickViewSplitter.Top:= FileView.BoundsRect.Bottom;
      alBottom: QuickViewSplitter.Top:= FileView.BoundsRect.Top-QuickViewSplitter.Height;
    end;
  end
  else
  begin
    FileView.Align:= alClient;
    
    if assigned(fQuickView) then
    begin
      FreeAndNil(fQuickView);
    end;
    QuickViewSplitter.Visible:= false;
    FileView.View:= fViewStyle;
  end;

  GlobalFileViewSettings.ViewStyle:= fViewStyle;
end;

{*------------------------------------------------------------------------------
  ThumbViewStyle Click
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.ThumbViewStyleClick(Sender: TObject);
begin
  case TSpTBXitem(Sender).Tag of
    1: ThumbViewStyle:= elsIcon;
    2: ThumbViewStyle:= elsSmallIcon;
    3: ThumbViewStyle:= elsList;
    4: ThumbViewStyle:= elsReport;
    5: ThumbViewStyle:= elsTile;
    6: ThumbViewStyle:= elsThumbnail;
    7: ThumbViewStyle:= elsFilmStrip;
  end;
end;

{*------------------------------------------------------------------------------
  Change Thumbnails position (menu item click)
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.ThumbPositionClick(Sender: TObject);
begin
  case TSpTBXitem(Sender).Tag of
    0: ThumbPosition:= alLeft;
    1: ThumbPosition:= alTop;
    2: ThumbPosition:= alRight;
    3: ThumbPosition:= alBottom;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on QuickView PopupMenu Popup
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.QuickViewPopupMenuPopup(Sender: TObject);
begin
  case ThumbPosition of
    alLeft: but_thumbpos_left.Checked:= true;
    alTop: but_thumbpos_top.Checked:= true;
    alRight: but_thumbpos_right.Checked:= true;
    alBottom: but_thumbpos_bottom.Checked:= true;
  end;

  case ThumbViewStyle of
    elsIcon: but_thumbstyle_icon.Checked:= true;
    elsSmallIcon: but_thumbstyle_smallicon.Checked:= true;
    elsList: but_thumbstyle_list.Checked:= true;
    elsReport: but_thumbstyle_details.Checked:= true;
    elsTile: but_thumbstyle_tiles.Checked:= true;
    elsThumbnail: but_thumbstyle_thumbnails.Checked:= true;
    elsFilmstrip: but_thumbstyle_filmstrip.Checked:= true;
  end;
end;

{*------------------------------------------------------------------------------
  QuickView Splitter Moved
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.QuickViewSplitterMoved(Sender: TObject);
begin
  if fViewStyle = elsFilmStrip then
  begin
    if (fThumbPosition = alTop) or (fThumbPosition = alBottom) then
    ThumbViewSize:= FileView.Height
    else
    ThumbViewSize:= FileView.Width;

    GlobalFileViewSettings.AssignFromFileView(Self);
  end;
end;

{*------------------------------------------------------------------------------
  Set Thumbnail View Style
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetThumbViewStyle(const Value: TEasyListStyle);
begin
  fThumbViewStyle:= Value;
  if fViewStyle = elsFilmStrip then
  begin
    FileView.View:= Value;
  end;
  
  GlobalFileViewSettings.AssignFromFileView(Self);
end;

{*------------------------------------------------------------------------------
  Get's called on background context menu
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnContextMenu(Sender: TCustomEasyListview; MousePt:
    TPoint; var Handled: Boolean);
var
  menu: TCEBackContextMenu;
  item: TEasyItem;
begin
  CEActions.UpdateAll;
  menu:= TCEBackContextMenu.Create;
  try
    menu.UpperMenuItems:= CEActions.BackgroundCMItems_up;
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

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.LoadFromStorage(Storage: ICESettingsStorage);
var
  ws: WideString;
  i: Integer;
  PIDL: PItemIDList;
begin
  FileView.BeginUpdate;
  try
    ViewStyle:= TEasyListStyle(Storage.ReadInteger('ViewStyle', 0));

    ws:= Storage.ReadString('PIDL', '');
    if ws <> '' then
    begin
      PIDL:= LoadPIDLFromMime(ws);
    end
    else
    begin
      i:= Storage.ReadInteger('Special', -1);
      if i <> -1 then
      begin
        SHGetspecialFolderLocation(0, i, PIDL);
      end
      else
      begin
        ws:= Storage.ReadString('Path', '');
        PIDL:= PathToPIDL(ws);
      end;
    end;

    if assigned(PIDL) then
    begin
      FileView.BrowseToByPIDL(PIDL);
      FileView.ClearHistory;
      FileView.fChangeHistory:= false;
      FileView.History.Add(TNamespace.Create(PIDLMgr.CopyPIDL(PIDL),nil),true);
      FileView.fChangeHistory:= true;
    end;
  finally
    FileView.EndUpdate(false);
    UpdateCaption;
  end;
end;

procedure TCEFileViewPage.OnItemContextMenu(Sender: TCustomEasyListview;
    HitInfo: TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu; var
    Handled: Boolean);
begin
  if not Handled then
  Handled:= not fShowItemContextMenu;
end;

{-------------------------------------------------------------------------------
  Save to Storage
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SaveToStorage(Storage: ICESettingsStorage);
var
  i: Integer;
begin
  if FileView.RootFolderNamespace.IsDesktop then
  i:= 0
  else
  i:= CE_SpecialNamespaces.GetSpecialID(FileView.RootFolderNamespace.AbsolutePIDL);

  if i > -1 then
  begin
    Storage.WriteInteger('Special', i);
    Storage.DeletePath('PIDL');
    Storage.DeletePath('Path');
  end
  else if WideDirectoryExists(FileView.RootFolderNamespace.NameForParsing) then
  begin
    Storage.DeletePath('Special');
    Storage.DeletePath('PIDL');
    Storage.WriteString('Path', FileView.RootFolderNamespace.NameForParsing);
  end
  else
  begin
    Storage.DeletePath('Special');
    Storage.DeletePath('Path');
    Storage.WriteString('PIDL', SavePIDLToMime(FileView.RootFolderNamespace.AbsolutePIDL));
  end;
  Storage.WriteInteger('ViewStyle', Ord(ViewStyle));
end;

procedure TCEFileViewPage.ShowHeaderSelector;

  function IsDuplicate(VST: TVirtualStringTree; Text: WideString): Boolean;
  var
    ColData: PColumnData;
    Node: PVirtualNode;
  begin
    Result := False;
    Node := VST.GetFirst;
    while not Result and Assigned(Node) do
    begin
      ColData := VST.GetNodeData(Node);
      Result := WideStrComp(PWideChar(ColData^.Title), PWideChar( Text)) = 0;
      Node := VST.GetNext(Node)
    end
  end;

var
  ColumnSettings: TFormColumnSettings;
  ColumnNames: TVirtualStringTree;
  ColData: PColumnData;
  BackupHeader: TMemoryStream;
  i, j: Integer;
begin
  ColumnSettings:= TFormColumnSettings.Create(nil);

  BackupHeader:= TMemoryStream.Create;
  ColumnNames:= ColumnSettings.VSTColumnNames;
  ColumnNames.BeginUpdate;
  try
    for i := 0 to FileView.Header.Columns.Count - 1 do
    begin
      j := 0;
      { Create the nodes ordered in columns items relative position }
      while (j < FileView.Header.Columns.Count) and (FileView.Header.Columns[j].Position <> i) do
        Inc(j);
      if (FileView.Header.Columns[j].Caption <> '') and not IsDuplicate(ColumnNames, FileView.Header.Columns[j].Caption) then
      begin
        ColData := ColumnNames.GetNodeData(ColumnNames.AddChild(nil));
        ColData.Title := FileView.Header.Columns[j].Caption;
        ColData.Enabled :=  FileView.Header.Columns[j].Visible;
        ColData.Width := FileView.Header.Columns[j].Width;
        ColData.ColumnIndex := FileView.Header.Columns[j].Index;
      end
    end;
    FileView.Header.SaveToStream(BackupHeader);
    BackupHeader.Seek(0, soFromBeginning);
  finally
    ColumnNames.EndUpdate;
  end;

  ColumnSettings.OnVETUpdate:= TCEFileViewHack(FileView).ColumnSettingCallback;
  if ColumnSettings.ShowModal = mrOk then
  begin
    TCEFileViewHack(FileView).UpdateColumnsFromDialog(ColumnNames);
  end
  else
  begin
    FileView.BeginUpdate;
    try
      FileView.Header.LoadFromStream(BackupHeader);
    finally
      FileView.EndUpdate
    end
  end;

  BackupHeader.Free;
  ColumnSettings.Release
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEFileViewSettings
-------------------------------------------------------------------------------}
constructor TCEFileViewSettings.Create;
begin
  inherited;
  fUpdating:= false;
  NotifyList:= TComponentList.Create(false);
  ThumbsPosition:= alBottom;
  ThumbViewStyle:= elsFilmstrip;
  ThumbViewSize:= 120;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEFileViewSettings
-------------------------------------------------------------------------------}
destructor TCEFileViewSettings.Destroy;
begin
  NotifyList.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Assign options from FileView.
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignFromFileView(FileViewPage: TCEFileViewPage);
begin
  if fUpdating then
  Exit;
  
  if not assigned(FileViewPage) then
  Exit;

  ViewStyle:= FileViewPage.ViewStyle;
  ThumbViewStyle:= FileViewPage.ThumbViewStyle;
  ThumbsPosition:= FileViewPage.ThumbPosition;
  ThumbViewSize:= FileViewPage.ThumbViewSize;
end;

{*------------------------------------------------------------------------------
  Assign options to FileView.
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignToFileView(FileViewPage: TCEFileViewPage);
var
  i: Integer;
begin
  if not assigned(FileViewPage) then
  Exit;
  fUpdating:= true;
  try
    FileViewPage.ThumbViewSize:= ThumbViewSize;
    FileViewPage.ViewStyle:= ViewStyle;
    FileViewPage.ThumbViewStyle:= ThumbViewStyle;
    FileViewPage.ThumbPosition:= ThumbsPosition;
    FileViewPage.FileView.SmoothScroll:= fSmoothScroll;
    if fHiddenFiles then
    FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders,foHidden] //,foShareable,foNetworkPrinters]
    else
    FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders]; //,foShareable,foNetworkPrinters];
    FileViewPage.FileView.Header.ShowInAllViews:= fShowHeaderAlways;
    if fShowHeaderAlways then
    FileViewPage.FileView.Header.Visible:= true;
    FileViewPage.FileView.Selection.FullRowSelect:= fFullRowSelect;
    FileViewPage.FileView.ShowExtension:= fShowExtensions;
    FileViewPage.FileView.SelectPreviousFolder:= SelectPreviousFolder;
    FileViewPage.FileView.AutoSelectFirstItem:= AutoSelectFirstItem;
    for i:= 0 to Length(CellSizes) - 1 do
    begin
      case i of
        0: FileViewPage.FileView.CellSizes.Icon.SetSize(CellSizes[i].X, CellSizes[i].Y);
        1: FileViewPage.FileView.CellSizes.SmallIcon.SetSize(CellSizes[i].X, CellSizes[i].Y);
        2: FileViewPage.FileView.CellSizes.List.SetSize(CellSizes[i].X, CellSizes[i].Y);
        3: FileViewPage.FileView.CellSizes.Report.SetSize(CellSizes[i].X, CellSizes[i].Y);
        4: FileViewPage.FileView.CellSizes.Tile.SetSize(CellSizes[i].X, CellSizes[i].Y);
        5: FileViewPage.FileView.CellSizes.Thumbnail.SetSize(CellSizes[i].X, CellSizes[i].Y);
        6: FileViewPage.FileView.CellSizes.FilmStrip.SetSize(CellSizes[i].X, CellSizes[i].Y);
      end;
    end;
  finally
    fUpdating:= false;
  end;
end;

{*------------------------------------------------------------------------------
  Save Settings From ActivePage
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignFromActivePage;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  AssignFromFileView(TCEFileViewPage(GlobalPathCtrl.ActivePage));
end;

{*------------------------------------------------------------------------------
  Register Notify
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.RegisterNotify(FileViewPage: TComponent);
begin
  NotifyList.Add(FileViewPage);
end;

{*------------------------------------------------------------------------------
  Restore Column Settings
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignColumnSettingsTo(FileView:
    TVirtualExplorerEasyListview);
var
  i: Integer;
  col: TEasyColumn;
  settings: PCEColSettings;
  grouping: PCEGroupBySetting;
begin
  if not assigned(FileView) then
  Exit;
  
  if FileView.RootFolderNamespace.IsMyComputer then
  begin
    settings:= @Columns_MyComputer;
    grouping:= @GroupBy_MyComputer;
  end
  else if FileView.RootFolderNamespace.IsControlPanel then
  begin
    settings:= @Columns_ControlPanel;
    grouping:= @GroupBy_ControlPanel
  end
  else if FileView.RootFolderNamespace.IsNetworkNeighborhood or FileView.RootFolderNamespace.IsNetworkNeighborhoodChild then
  begin
    settings:= @Columns_Network;
    grouping:= @GroupBy_Network
  end
  else
  begin
    settings:= @Columns_Default;
    grouping:= @GroupBy_Default
  end;

  FileView.GroupingColumn:= grouping.Index;
  FileView.Grouped:= grouping.Enabled;

  if Length(settings^) = 0 then
  Exit;

  FileView.Header.Columns.BeginUpdate(false);
  try
    for i:= 0 to FileView.Header.Columns.Count - 1 do
    begin
      if FileView.Header.Columns.Columns[i].Visible then
      FileView.Header.Columns.Columns[i].Visible:= false;
    end;

    for i:= 0 to Length(settings^) - 1 do
    begin
      if settings^[i].Index < FileView.Header.Columns.Count then
      begin
        col:= FileView.Header.Columns.Columns[settings^[i].Index];
        col.Visible:= true;
        col.Position:= settings^[i].Position;
        col.Width:= settings^[i].Width;
        col.SortDirection:= settings^[i].Sort;
      end;
    end;


  finally
    FileView.Header.Columns.EndUpdate(true);
  end;
end;

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.LoadFromStorage(Storage: ICESettingsStorage);

  // Load Column Settings
  procedure LoadColumns;
  var
    i,ic: Integer;
    colListSet, colList: TStrings;
    ColSettings: PCEColSettings;
  begin
    colListSet:= TStringList.Create;
    colList:= TStringList.Create;
    Storage.OpenPath('/FileView/Columns');
    try
      colListSet.Delimiter:= '|';
      colList.Delimiter:= ',';
      for ic:= 1 to 4 do
      begin
        case ic of
          2: begin
               colListSet.DelimitedText:= Storage.ReadString('MyComputer','');
               ColSettings:= @Columns_MyComputer;
             end;
          3: begin
               colListSet.DelimitedText:= Storage.ReadString('Network','');
               ColSettings:= @Columns_Network;
             end;
          4: begin
               colListSet.DelimitedText:= Storage.ReadString('ControlPanel','');
               ColSettings:= @Columns_ControlPanel;
             end;
          else
             begin
               colListSet.DelimitedText:= Storage.ReadString('Default','');
               ColSettings:= @Columns_Default;
             end;
        end;

        SetLength(ColSettings^, colListSet.Count);

        for i:= 0 to colListSet.Count - 1 do
        begin
          colList.DelimitedText:= colListSet.Strings[i];
          if colList.Count >= 3 then
          begin
            ColSettings^[i].Index:= StrToInt(colList.Strings[0]);
            ColSettings^[i].Position:= StrToInt(colList.Strings[1]);
            ColSettings^[i].Width:= StrToInt(colList.Strings[2]);
            if colList.Count = 4 then
            ColSettings^[i].Sort:= TEasySortDirection(StrToInt(colList.Strings[3]));
          end;
        end;

      end;
    finally
      Storage.OpenPath('/FileView');
      colListSet.Free;
      colList.Free;
    end;
  end;

  // Load GroupBy settings
  procedure LoadGroupBy;
  begin
    Storage.OpenPath('/FileView/GroupBy');
    try
      GroupBy_ControlPanel.Enabled:= Storage.ReadBoolean('ControlPanel?Enabled',false);
      GroupBy_ControlPanel.Index:= Storage.ReadInteger('ControlPanel?Index',0);
      GroupBy_Default.Enabled:= Storage.ReadBoolean('Default?Enabled',false);
      GroupBy_Default.Index:= Storage.ReadInteger('Default?Index',0);
      GroupBy_MyComputer.Enabled:= Storage.ReadBoolean('MyComputer?Enabled',false);
      GroupBy_MyComputer.Index:= Storage.ReadInteger('MyComputer?Index',0);
      GroupBy_Network.Enabled:= Storage.ReadBoolean('Network?Enabled',false);
      GroupBy_Network.Index:= Storage.ReadInteger('Network?Index',0);
    finally
      Storage.OpenPath('/FileView');
    end;
  end;

var
  i: Integer;
begin
  Storage.OpenPath('/FileView');
  try
    // Filmstrip settings
    Storage.OpenPath('/FileView/Filmstrip');
    ThumbsPosition:= TAlign(Storage.ReadInteger('ThumbPos', 1));
    case ThumbsPosition of
      alNone, alClient, alCustom: ThumbsPosition:= alBottom;
    end;
    ThumbViewStyle:= TEasyListStyle(Storage.ReadInteger('ThumbStyle',6));
    ThumbViewSize:= Storage.ReadInteger('ThumbSize', 120);
    Storage.OpenPath('/FileView');
    // View style
    ViewStyle:= TEasyListStyle(Storage.ReadInteger('ViewStyle',0));
    // Toggles
    fSmoothScroll:= Storage.ReadBoolean('SmoothScroll', false);
    fHiddenFiles:= Storage.ReadBoolean('HiddenFiles', false);
    fShowHeaderAlways:= Storage.ReadBoolean('ShowHeaderAlways',false);
    fFullRowSelect:= Storage.ReadBoolean('FullRowSelect',true);
    fShowExtensions:= Storage.ReadBoolean('ShowExtensions',true);
    fSelectPreviousFolder:= Storage.ReadBoolean('SelectPreviousFolder',true);
    fAutoSelectFirstItem:= Storage.ReadBoolean('AutoSelectFirstItem',true);
    // Cell Sizes
    Storage.OpenPath('/FileView/CellSizes');
    for i:= 0 to Length(CellSizes) -1  do
    begin
      case i of
        0: CellSizes[i]:= Storage.ReadPoint('Icon', Point(75,75));
        1: CellSizes[i]:= Storage.ReadPoint('SmallIcon', Point(200,17));
        2: CellSizes[i]:= Storage.ReadPoint('List', Point(250,17));
        3: CellSizes[i]:= Storage.ReadPoint('Details', Point(75,17));
        4: CellSizes[i]:= Storage.ReadPoint('Tiles', Point(218,58));
        5: CellSizes[i]:= Storage.ReadPoint('Thumbnails', Point(125,143));
        6: CellSizes[i]:= Storage.ReadPoint('Filmstrip', Point(125,143));
      end;

      if (CellSizes[i].X = 0) or (CellSizes[i].Y = 0) then
      begin
        case i of
          0: CellSizes[i]:= Point(75,75);
          1: CellSizes[i]:= Point(200,17);
          2: CellSizes[i]:= Point(250,17);
          3: CellSizes[i]:= Point(75,17);
          4: CellSizes[i]:= Point(218,58);
          5: CellSizes[i]:= Point(125,143);
          6: CellSizes[i]:= Point(125,143);
        end;
      end;
    end;
    Storage.OpenPath('/FileView');
    // Columns
    LoadColumns;
    // GroupBy
    LoadGroupBy;
    
    SendChanges;
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Save to storage
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SaveToStorage(Storage: ICESettingsStorage);

  // Save colums
  procedure SaveColumns;
  var
    s,sname: String;
    i,ic: Integer;
    ColSettings: PCEColSettings;
  begin
    Storage.OpenPath('/FileView/Columns');
    try
      for ic:= 1 to 4 do
      begin
        case ic of
          2: begin
               sname:= 'MyComputer';
               ColSettings:= @Columns_MyComputer;
             end;
          3: begin
               sname:= 'Network';
               ColSettings:= @Columns_Network;
             end;
          4: begin
               sname:= 'ControlPanel';
               ColSettings:= @Columns_ControlPanel;
             end;
          else
             begin
               sname:= 'Default';
               ColSettings:= @Columns_Default;
             end;
        end;

        s:= '';
        for i:= 0 to Length(ColSettings^) - 1 do
        begin
          if i > 0 then
          s:= s + '|';

          s:= s + IntToStr(ColSettings^[i].Index) + ',' +
                  IntToStr(ColSettings^[i].Position) + ',' +
                  IntToStr(ColSettings^[i].Width) + ',' +
                  IntToStr(Integer(ColSettings^[i].Sort));
        end;
        Storage.WriteString(sname, s);
      end;
    finally
      Storage.OpenPath('/FileView');
    end;
  end;

  // Save GroupBy
  procedure SaveGroupBy;
  begin
    Storage.OpenPath('/FileView/GroupBy');
    try
      Storage.WriteBoolean('ControlPanel?Enabled',GroupBy_ControlPanel.Enabled);
      Storage.WriteInteger('ControlPanel?Index',GroupBy_ControlPanel.Index);
      Storage.WriteBoolean('Default?Enabled',GroupBy_Default.Enabled);
      Storage.WriteInteger('Default?Index',GroupBy_Default.Index);
      Storage.WriteBoolean('MyComputer?Enabled',GroupBy_MyComputer.Enabled);
      Storage.WriteInteger('MyComputer?Index',GroupBy_MyComputer.Index);
      Storage.WriteBoolean('Network?Enabled',GroupBy_Network.Enabled);
      Storage.WriteInteger('Network?Index',GroupBy_Network.Index);
    finally
      Storage.OpenPath('/FileView');
    end;
  end;

begin
  Storage.OpenPath('/FileView');
  try
    // Filmstrip settings
    Storage.OpenPath('/FileView/Filmstrip');
    Storage.WriteInteger('ThumbPos', Ord(ThumbsPosition));
    Storage.WriteInteger('ThumbStyle', Ord(ThumbViewStyle));
    Storage.WriteInteger('ThumbSize', ThumbViewSize);
    Storage.OpenPath('/FileView');
    // View Style
    Storage.WriteInteger('ViewStyle', Ord(ViewStyle));
    // Toggles
    Storage.WriteBoolean('SmoothScroll', fSmoothScroll);
    Storage.WriteBoolean('HiddenFiles', fHiddenFiles);
    Storage.WriteBoolean('ShowHeaderAlways',fShowHeaderAlways);
    Storage.WriteBoolean('FullRowSelect',fFullRowSelect);
    Storage.WriteBoolean('ShowExtensions',fShowExtensions);
    Storage.WriteBoolean('SelectPreviousFolder',fSelectPreviousFolder);
    Storage.WriteBoolean('AutoSelectFirstItem',fAutoSelectFirstItem);
    // CellSizes
    Storage.OpenPath('/FileView/CellSizes');
    Storage.WritePoint('Icon', CellSizes[0]);
    Storage.WritePoint('SmallIcon', CellSizes[1]);
    Storage.WritePoint('List', CellSizes[2]);
    Storage.WritePoint('Details', CellSizes[3]);
    Storage.WritePoint('Tiles', CellSizes[4]);
    Storage.WritePoint('Thumbnails', CellSizes[5]);
    Storage.WritePoint('Filmstrip', CellSizes[6]);
    Storage.OpenPath('/FileView');
    // Columns
    SaveColumns;
    // GroupBy
    SaveGroupBy;
  finally
    Storage.ClosePath;
  end;
end;

{*------------------------------------------------------------------------------
  Send Changes
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SendChanges;
var
  i: Integer;
  FileViewPage: TCEFileViewPage;
  doRebuild: Boolean;
begin
  for i:= 0 to NotifyList.Count - 1 do
  begin
    if NotifyList.Items[i] is TCEFileViewPage then
    begin
      FileViewPage:= TCEFileViewPage(NotifyList.Items[i]);
      FileViewPage.FileView.BeginUpdate;
      FileViewPage.FileView.SmoothScroll:= fSmoothScroll;
      if fHiddenFiles then
      FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders,foHidden] //,foShareable,foNetworkPrinters]
      else
      FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders]; //,foShareable,foNetworkPrinters]
      // Toggles
      FileViewPage.FileView.Header.ShowInAllViews:= fShowHeaderAlways;
      if fShowHeaderAlways then
      FileViewPage.FileView.Header.Visible:= true;
      FileViewPage.FileView.Selection.FullRowSelect:= fFullRowSelect;
      FileViewPage.FileView.SelectPreviousFolder:= SelectPreviousFolder;
      FileViewPage.FileView.AutoSelectFirstItem:= AutoSelectFirstItem;

      doRebuild:=  FileViewPage.FileView.ShowExtension <> fShowExtensions;
      FileViewPage.FileView.ShowExtension:= fShowExtensions;
      
      if doRebuild then FileViewPage.FileView.Rebuild;
      FileViewPage.FileView.EndUpdate(FileViewPage.Visible);
    end
    else if NotifyList.Items[i] is TVirtualExplorerTree then
    begin
      if fHiddenFiles then
      TVirtualExplorerTree(NotifyList.Items[i]).FileObjects:= [foFolders,foNonFolders,foHidden] //,foShareable,foNetworkPrinters]
      else
      TVirtualExplorerTree(NotifyList.Items[i]).FileObjects:= [foFolders,foNonFolders];
    end;     
  end;
  CEFolderPanel.FolderTree.HiddenFiles:= fHiddenFiles;
end;

{*------------------------------------------------------------------------------
  Set Full Row select
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetFullRowSelect(const Value: Boolean);
begin
  fFullRowSelect:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Set Hidden Files
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetHiddenFiles(const Value: Boolean);
begin
  fHiddenFiles:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Set ShowExtensions
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetShowExtensions(const Value: Boolean);
begin
  fShowExtensions:= Value;
  SendChanges;
  
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Refresh;
end;

{*------------------------------------------------------------------------------
  Set Show Header Always
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetShowHeaderAlways(const Value: Boolean);
begin
  fShowHeaderAlways:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Set Smooth Scroll
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetSmoothScroll(const Value: Boolean);
begin
  fSmoothScroll:= Value;
  SendChanges;
end;

{*------------------------------------------------------------------------------
  Save Column Settings
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignColumnSettingsFrom(FileView:
    TVirtualExplorerEasyListview);
var
  col: TEasyColumn;
  i,c: Integer;
  settings: PCEColSettings;
  grouping: PCEGroupBySetting;
begin
  if not assigned(FileView) then
  Exit;

  if FileView.RootFolderNamespace.IsMyComputer then
  begin
    settings:= @Columns_MyComputer;
    grouping:= @GroupBy_MyComputer;
  end
  else if FileView.RootFolderNamespace.IsControlPanel then
  begin
    settings:= @Columns_ControlPanel;
    grouping:= @GroupBy_ControlPanel
  end
  else if FileView.RootFolderNamespace.IsNetworkNeighborhood or FileView.RootFolderNamespace.IsNetworkNeighborhoodChild then
  begin
    settings:= @Columns_Network;
    grouping:= @GroupBy_Network
  end
  else
  begin
    settings:= @Columns_Default;
    grouping:= @GroupBy_Default
  end;
  
  c:= 0;
  for i:= 0 to FileView.Header.Columns.Count - 1 do
  begin
    if FileView.Header.Columns.Columns[i].Visible then
    Inc(c,1);
  end;

  if Length(settings^) <> c then
  SetLength(settings^, c);
  
  i:= 0;

  col:= FileView.Header.FirstVisibleColumn;
  while assigned(col) do
  begin
    settings^[i].Index:= col.Index;
    settings^[i].Position:= col.Position;
    settings^[i].Width:= col.Width;
    settings^[i].Sort:= col.SortDirection;
    col:= FileView.Header.NextVisibleColumn(col);
    inc(i);
  end;

  grouping.Index:= FileView.GroupingColumn;
  grouping.Enabled:= FileView.Grouped;
end;

{-------------------------------------------------------------------------------
  Set SelectPreviousFolder
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetSelectPreviousFolder(const Value: Boolean);
begin
  fSelectPreviousFolder:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set SelectPreviousFolder
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetAutoSelectFirstItem(const Value: Boolean);
begin
  fAutoSelectFirstItem:= Value;
  SendChanges;
end;

initialization
  GlobalFileViewSettings:= TCEFileViewSettings.Create;
  GlobalSettings.RegisterHandler(GlobalFileViewSettings);
  TabPageClassList.RegisterClass('FileView', TCEFileViewPage);

finalization
  GlobalFileViewSettings:= nil;

end.
