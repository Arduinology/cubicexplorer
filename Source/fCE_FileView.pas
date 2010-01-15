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
//  The Original Code is fCE_FileView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_FileView;

interface

uses
  // CE Units
  fCE_TabPage, CE_FileView, CE_GlobalCtrl, CE_QuickView, CE_Utils,
  dCE_Images, CE_ContextMenu, CE_LanguageEngine, CE_AppSettings,
  CE_InfoBar,
  // EasyListview
  EasyListview, 
  // VSTools
  VirtualExplorerEasyListview, MPCommonObjects, MPShellUtilities,
  VirtualExplorerTree, MPCommonUtilities, ColumnFormSpTBX,
  // VT
  VirtualTrees,
  // SpTBX
  SpTBXItem, SpTBXControls, SpTBXDkPanels, SpTBXSkins,
  // TB2K
  TB2Item,
  // Tnt Controls
  TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, Menus, JvAppStorage, Contnrs, StrUtils, ExtCtrls;

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
    fShowInfoBar: Boolean;
    fShowItemContextMenu: Boolean;
    fThumbPosition: TAlign;
    fViewStyle: TEasyListStyle;
    fThumbViewStyle: TEasyListStyle;
    procedure SetShowInfoBar(const Value: Boolean);
    procedure SetThumbPosition(const Value: TAlign);
    procedure SetViewStyle(const Value: TEasyListStyle);
    procedure SetThumbViewStyle(const Value: TEasyListStyle);
  protected
    fQuickView: TCEQuickView;
    function GetSettingsClass: TCECustomTabPageSettingsClass; override;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
    procedure InfoBarSplitterMouseUp(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure InfoBarSplitterMoving(Sender: TObject; var NewSize: Integer; var
        Accept: Boolean);
    procedure ItemSelectionChanged(Sender: TCustomEasyListview; Item: TEasyItem);
        virtual;
    procedure ItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure OnItemContextMenu(Sender: TCustomEasyListview; HitInfo:
        TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu; var Handled:
        Boolean);
    procedure SetActive(const Value: Boolean); override;
  public
    FileView: TCEFileView;
    ThumbViewSize: Integer;
    InfoBar: TCEInfoBar;
    InfoBarSplitter: TSpTBXSplitter;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    procedure OnContextMenu(Sender: TCustomEasyListview; MousePt: TPoint; var
        Handled: Boolean);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);
    procedure ShowHeaderSelector;
    property ShowInfoBar: Boolean read fShowInfoBar write SetShowInfoBar;
    property ThumbPosition: TAlign read fThumbPosition write SetThumbPosition;
    property ViewStyle: TEasyListStyle read fViewStyle write SetViewStyle;
    property ThumbViewStyle: TEasyListStyle read fThumbViewStyle write
        SetThumbViewStyle;
  end;

  TCEFileViewPageSettings = class(TCECustomTabPageSettings)
  private
    function GetPath: WideString;
    function GetViewStyle: TEasyListStyle;
    procedure SetPath(const Value: WideString);
    procedure SetViewStyle(const Value: TEasyListStyle);
  protected
    function GetRememberInnerToolbarLayout: Boolean; override;
    function GetRememberOuterToolbarLayout: Boolean; override;
    function GetRememberPanelLayout: Boolean; override;
  public
    FileViewPage: TCEFileViewPage;
  published
    property Path: WideString read GetPath write SetPath;
    property ViewStyle: TEasyListStyle read GetViewStyle write SetViewStyle;
  end;

  TCEFileViewSettings = class(TPersistent)
  private
    fFullRowSelect: Boolean;
    fHiddenFiles: Boolean;
    fSelectPreviousFolder: Boolean;
    fAutoSelectFirstItem: Boolean;
    fAutosizeListViewStyle: Boolean;
    fBrowseZipFolders: Boolean;
    fColumns: TCEColumnSettings;
    fFilmstrip: TCEFilmstripSettings;
    fGroupBy: TCEGroupBySettings;
    fInfoBarSize: Integer;
    fRememberPanelLayout: Boolean;
    fRememberInnerToolbarLayout: Boolean;
    fRememberOuterToolbarLayout: Boolean;
    fShowExtensions: Boolean;
    fShowHeaderAlways: Boolean;
    fShowInfoBar: Boolean;
    fSortFolderFirstAlways: Boolean;
    fThreadedDetails: Boolean;
    fThreadedEnumeration: Boolean;
    fThreadedImages: Boolean;
    fUpdating: Boolean;
    NotifyList: TComponentList;
    procedure SetFullRowSelect(const Value: Boolean);
    procedure SetHiddenFiles(const Value: Boolean);
    procedure SetSelectPreviousFolder(const Value: Boolean);
    procedure SetAutoSelectFirstItem(const Value: Boolean);
    procedure SetAutosizeListViewStyle(const Value: Boolean);
    procedure SetBrowseZipFolders(const Value: Boolean);
    procedure SetShowExtensions(const Value: Boolean);
    procedure SetShowHeaderAlways(const Value: Boolean);
    procedure SetShowInfoBar(const Value: Boolean);
    procedure SetSmoothScroll(const Value: Boolean);
    procedure SetSortFolderFirstAlways(const Value: Boolean);
    procedure SetThreadedDetails(const Value: Boolean);
    procedure SetThreadedEnumeration(const Value: Boolean);
    procedure SetThreadedImages(const Value: Boolean);
  protected
    fViewStyle: TEasyListStyle;
  public
    fSmoothScroll: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AssignSettingsTo(FileViewPage: TCEFileViewPage; AssignColumnSettings:
        Boolean = true);
    procedure AssignSettingsFrom(FileViewPage: TCEFileViewPage);
    procedure AssignFromActivePage;
    procedure RegisterNotify(FileViewPage: TComponent);
    procedure AssignColumnSettingsTo(FileView: TVirtualExplorerEasyListview);
    procedure SendChanges;
    procedure AssignColumnSettingsFrom(FileView: TVirtualExplorerEasyListview);
  published
    property SelectPreviousFolder: Boolean read fSelectPreviousFolder write
        SetSelectPreviousFolder;
    property AutoSelectFirstItem: Boolean read fAutoSelectFirstItem write
        SetAutoSelectFirstItem;
    property AutosizeListViewStyle: Boolean read fAutosizeListViewStyle write
        SetAutosizeListViewStyle;
    property BrowseZipFolders: Boolean read fBrowseZipFolders write
        SetBrowseZipFolders;
    property Columns: TCEColumnSettings read fColumns write fColumns;
    property Filmstrip: TCEFilmstripSettings read fFilmstrip write fFilmstrip;
    property FullRowSelect: Boolean read fFullRowSelect write SetFullRowSelect;
    property GroupBy: TCEGroupBySettings read fGroupBy write fGroupBy;
    property HiddenFiles: Boolean read fHiddenFiles write SetHiddenFiles;
    property InfoBarSize: Integer read fInfoBarSize write fInfoBarSize;
    property RememberPanelLayout: Boolean read fRememberPanelLayout write
        fRememberPanelLayout;
    property RememberInnerToolbarLayout: Boolean read fRememberInnerToolbarLayout
        write fRememberInnerToolbarLayout;
    property RememberOuterToolbarLayout: Boolean read fRememberOuterToolbarLayout
        write fRememberOuterToolbarLayout;
    property ShowExtensions: Boolean read fShowExtensions write SetShowExtensions;
    property ShowHeaderAlways: Boolean read fShowHeaderAlways write
        SetShowHeaderAlways;
    property ShowInfoBar: Boolean read fShowInfoBar write SetShowInfoBar;
    property SmoothScroll: Boolean read fSmoothScroll write SetSmoothScroll;
    property SortFolderFirstAlways: Boolean read fSortFolderFirstAlways write
        SetSortFolderFirstAlways;
    property ThreadedDetails: Boolean read fThreadedDetails write
        SetThreadedDetails;
    property ThreadedEnumeration: Boolean read fThreadedEnumeration write
        SetThreadedEnumeration;
    property ThreadedImages: Boolean read fThreadedImages write SetThreadedImages;
    property ViewStyle: TEasyListStyle read fViewStyle write fViewStyle;
  end;

var
  GlobalFileViewSettings: TCEFileViewSettings;

implementation

{$R *.dfm}

uses
  dCE_Actions, CE_VistaFuncs, fCE_FolderPanel, CE_CommonObjects, CE_SpTabBar,
  GR32_Math, Math, Main;

{*------------------------------------------------------------------------------
  Get's called when TCEFileViewPage is created.
-------------------------------------------------------------------------------}
constructor TCEFileViewPage.Create(AOwner: TComponent);
begin
  inherited;
  TCEFileViewPageSettings(Settings).FileViewPage:= Self;
  ThumbViewSize:= 100;
  fThumbPosition:= alBottom;
  fThumbViewStyle:= elsFilmStrip;
  Layout:= 'FileView';
  Images:= SmallSysImages;
  FileView:= TCEFileView.Create(nil);
  FileView.Parent:= self;
  FileView.Align:= alClient;
  FileView.Themed:= false;
  FileView.BorderStyle:= bsNone;
  FileView.BoundsRect:= Rect(0,0, self.ClientWidth, self.ClientHeight);
  FileView.OnRootRebuild:= RootRebuild;
  FileView.OnRootChanging:= RootChanging;
  FileView.OnRootChange:= RootChange;
  FileView.OnItemSelectionsChanged:= ItemSelectionsChanged;
  FileView.OnItemSelectionChanged:= ItemSelectionChanged;
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
  FileView.TabOrder:= 1;
  // translate header
  FileView.TranslateHeader:= false;

  FileView.PaintInfoGroup.BandThickness:= 2;
  FileView.PaintInfoGroup.BandColor:= clWindowText;
  FileView.PaintInfoGroup.BandColorFade:= clWindow;
  FileView.GroupFont.Style:= [fsBold];
  GlobalFocusCtrl.CtrlList.Add(FileView);
  FileView.OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  GlobalFileViewSettings.RegisterNotify(Self);
  SetDesktopIconFonts(FileView.Font);

  InfoBar:= TCEInfoBar.Create(nil);
  InfoBar.Parent:= Self;
  InfoBar.Align:= alBottom;
  InfoBar.Visible:= false;
  InfoBar.RowHeight:= SpGetControlTextHeight(InfoBar, InfoBar.Font) + 6;
  InfoBar.Height:= Max(GlobalFileViewSettings.InfoBarSize,InfoBar.RowHeight*3 + 6);

  InfoBarSplitter:= TSpTBXSplitter.Create(nil);
  InfoBarSplitter.Parent:= Self;
  InfoBarSplitter.Align:= alBottom;
  InfoBarSplitter.GripSize:= 0;
  InfoBarSplitter.MinSize:= InfoBar.RowHeight;
  InfoBarSplitter.Top:= InfoBar.BoundsRect.Top - InfoBarSplitter.Height;
  InfoBarSplitter.OnMouseUp:= InfoBarSplitterMouseUp;
  InfoBarSplitter.OnMoving:= InfoBarSplitterMoving;
  InfoBarSplitter.Visible:= false;

  ShowInfoBar:= false;

  GlobalFileViewSettings.AssignSettingsTo(Self);
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
  FileView.Free;
  InfoBar.Free;
  InfoBarSplitter.Free;
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

{-------------------------------------------------------------------------------
  Get's called on InfoBarSplitter MouseUp
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.InfoBarSplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  InfoBar.RefreshThumbnail;
  GlobalFileViewSettings.InfoBarSize:= InfoBar.Height;
  InfoBar.Repaint;
end;

{-------------------------------------------------------------------------------
  Get's called on InfoBarSplitter Moving
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.InfoBarSplitterMoving(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
var
  row_count: Integer;
  i: Integer;
begin
  Accept:= NewSize > InfoBar.RowHeight;
//  if Accept then
//  begin
//    row_count:= Max(1, NewSize div InfoBar.RowHeight);
//    i:= NewSize - ((InfoBar.RowHeight * row_count) + 6);
//    Accept:= (i < 2) or (i > 2);
//  end;

  InfoBar.Paint;
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
    begin
      GlobalPathCtrl.ChangeFocusedPath(Self, NS.NameForParsing);
    end;
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
          OpenFileInTab(NS.NameForParsing, not MainForm.TabSet.Settings.OpenTabSelect)
          else
          OpenFileInTab(NS.NameForParsing, MainForm.TabSet.Settings.OpenTabSelect)
        end
        else
        begin
          if ssShift in Shift then
          OpenFolderInTab(Self, NS.AbsolutePIDL, not MainForm.TabSet.Settings.OpenTabSelect)
          else
          OpenFolderInTab(Self, NS.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect)
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
        NS.ShowPropertySheet(MainForm);
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
  GlobalFileViewSettings.AssignColumnSettingsTo(FileView);
end;

{*------------------------------------------------------------------------------
  Update Caption
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.UpdateCaption;
begin
  TabCaption:= FileView.RootFolderNamespace.NameNormal;
//  TCESpTabItem(TabItem).NormalImages:= SmallSysImages;
//  TCESpTabItem(TabItem).NormalImageIndex:= FileView.RootFolderNamespace.GetIconIndex(false, icSmall);
  TCESpTabItem(TabItem).Images:= SmallSysImages;
  TCESpTabItem(TabItem).ImageIndex:= FileView.RootFolderNamespace.GetIconIndex(false, icSmall);
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

{-------------------------------------------------------------------------------
  Get Settings Class
-------------------------------------------------------------------------------}
function TCEFileViewPage.GetSettingsClass: TCECustomTabPageSettingsClass;
begin
  Result:= TCEFileViewPageSettings;
end;

{*------------------------------------------------------------------------------
  Hide page
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.HidePage;
begin
  inherited;
end;

procedure TCEFileViewPage.ItemSelectionChanged(Sender: TCustomEasyListview;
    Item: TEasyItem);
var
  ns: TNamespace;
  lastItem, tmpItem: TEasyItem;
begin
  if not ShowInfoBar then
  Exit;
  
  if FileView.Selection.Count = 0 then
  begin
    InfoBar.Clear;
  end
  else
  begin
    if item.Selected then
    begin
      if FileView.ValidateNamespace(item, ns) then
      begin
        Caption:= ns.NameNormal;
        InfoBar.LoadFromPIDL(ns.AbsolutePIDL, FileView.Selection.Count);
      end
    end
    else
    begin
      lastItem:= nil;
      tmpItem:= FileView.Selection.First;
      while assigned(tmpItem) do
      begin
        lastItem:= tmpItem;
        tmpItem:= FileView.Selection.Next(tmpItem);
      end;

      if FileView.ValidateNamespace(lastItem, ns) then
      begin
        Caption:= ns.NameNormal;
        InfoBar.LoadFromPIDL(ns.AbsolutePIDL, FileView.Selection.Count);
      end;
    end;
  end;
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

  GlobalFileViewSettings.AssignSettingsFrom(Self);
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

    GlobalFileViewSettings.AssignSettingsFrom(Self);
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
  
  GlobalFileViewSettings.AssignSettingsFrom(Self);
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
  On Item ContextMenu
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.OnItemContextMenu(Sender: TCustomEasyListview;
    HitInfo: TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu; var
    Handled: Boolean);
begin
  if not Handled then
  Handled:= not fShowItemContextMenu;
end;

{-------------------------------------------------------------------------------
  Set Show InfoBar
-------------------------------------------------------------------------------}
procedure TCEFileViewPage.SetShowInfoBar(const Value: Boolean);
var
  NS: TNamespace;
  Item: TEasyItem;
begin
  if fShowInfoBar <> Value then
  begin
    fShowInfoBar:= Value;
    if fShowInfoBar then
    begin
      InfoBar.Visible:= true;
      InfoBar.Top:= Self.BoundsRect.Bottom;
      InfoBarSplitter.Top:= InfoBar.Top - InfoBarSplitter.Height;
      InfoBarSplitter.Visible:= true;
      InfoBar.Height:= GlobalFileViewSettings.InfoBarSize;

      if FileView.Selection.Count > 1 then
      Item:= FileView.Selection.FocusedItem
      else
      Item:= FileView.Selection.First;

      if FileView.ValidateNamespace(Item, NS) then
      InfoBar.LoadFromPIDL(NS.AbsolutePIDL);
    end
    else
    begin
      InfoBar.Visible:= false;
      InfoBar.Clear;
      InfoBarSplitter.Visible:= false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Show Header Selector
-------------------------------------------------------------------------------}
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
  ColumnSettings.PopupParent:= MainForm;
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
  fColumns:= TCEColumnSettings.Create;
  fGroupBy:= TCEGroupBySettings.Create;
  fFilmstrip:= TCEFilmstripSettings.Create;
  Filmstrip.ThumbPos:= alBottom;
  Filmstrip.ThumbStyle:= elsFilmstrip;
  Filmstrip.ThumbSize:= 120;
  fRememberInnerToolbarLayout:= true;
  fInfoBarSize:= 3;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEFileViewSettings
-------------------------------------------------------------------------------}
destructor TCEFileViewSettings.Destroy;
begin
  NotifyList.Free;
  fColumns.Free;
  fGroupBy.Free;
  fFilmstrip.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Assign options from FileView.
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignSettingsFrom(FileViewPage: TCEFileViewPage);
begin
  if fUpdating then
  Exit;
  
  if not assigned(FileViewPage) then
  Exit;

  ViewStyle:= FileViewPage.ViewStyle;
  Filmstrip.ThumbStyle:= FileViewPage.ThumbViewStyle;
  Filmstrip.ThumbPos:= FileViewPage.ThumbPosition;
  Filmstrip.ThumbSize:= FileViewPage.ThumbViewSize;
end;

{*------------------------------------------------------------------------------
  Assign options to FileView.
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignSettingsTo(FileViewPage: TCEFileViewPage;
    AssignColumnSettings: Boolean = true);
var
  options: TVirtualEasyListviewOptions;
begin
  if not assigned(FileViewPage) then
  Exit;
  fUpdating:= true;
  FileViewPage.FileView.BeginUpdate;
  try
    FileViewPage.ThumbViewSize:= Filmstrip.ThumbSize;
    FileViewPage.ViewStyle:= ViewStyle;
    FileViewPage.ThumbViewStyle:= Filmstrip.ThumbStyle;
    FileViewPage.ThumbPosition:= Filmstrip.ThumbPos;
    // Toggles
    FileViewPage.FileView.SmoothScroll:= fSmoothScroll;
    if fHiddenFiles then
    FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders,foHidden] //,foShareable,foNetworkPrinters]
    else
    FileViewPage.FileView.FileObjects:= [foFolders,foNonFolders]; //,foShareable,foNetworkPrinters];
    FileViewPage.InfoBar.CalculateHiddenItems:= fHiddenFiles;
    FileViewPage.FileView.Header.ShowInAllViews:= fShowHeaderAlways;
    if fShowHeaderAlways then
    FileViewPage.FileView.Header.Visible:= true;
    FileViewPage.FileView.Selection.FullRowSelect:= fFullRowSelect;
    FileViewPage.FileView.ShowExtension:= fShowExtensions;
    FileViewPage.FileView.SelectPreviousFolder:= SelectPreviousFolder;
    FileViewPage.FileView.AutoSelectFirstItem:= AutoSelectFirstItem;
    FileViewPage.FileView.AutosizeListViewStyle:= AutosizeListViewStyle;
    FileViewPage.FileView.SortFolderFirstAlways:= SortFolderFirstAlways;
    FileViewPage.ShowInfoBar:= ShowInfoBar;
    FileViewPage.InfoBar.Height:= InfoBarSize;
    // Options
    options:= FileViewPage.FileView.Options;
    if fBrowseZipFolders then Include(options, eloBrowseExecuteZipFolder) else Exclude(options, eloBrowseExecuteZipFolder);
    if fThreadedImages then Include(options, eloThreadedImages) else Exclude(options, eloThreadedImages);
    if fThreadedEnumeration then Include(options, eloThreadedEnumeration) else Exclude(options, eloThreadedEnumeration);
    if fThreadedDetails then Include(options, eloThreadedDetails) else Exclude(options, eloThreadedDetails);
    FileViewPage.FileView.Options:= options;
    
    if AssignColumnSettings then
    AssignColumnSettingsTo(FileViewPage.FileView);
  finally
    FileViewPage.FileView.EndUpdate(FileViewPage.Visible);
    fUpdating:= false;
  end;
end;

{*------------------------------------------------------------------------------
  Save Settings From ActivePage
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.AssignFromActivePage;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    AssignSettingsFrom(TCEFileViewPage(GlobalPathCtrl.ActivePage));
    InfoBarSize:= TCEFileViewPage(GlobalPathCtrl.ActivePage).InfoBar.Height;
  end;
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
    settings:= @Columns.MyComputerColSettings;
    grouping:= @GroupBy.MyComputerGroupBySettings;
  end
  else if FileView.RootFolderNamespace.IsControlPanel then
  begin
    settings:= @Columns.ControlPanelColSettings;
    grouping:= @GroupBy.ControlPanelGroupBySettings;
  end
  else if FileView.RootFolderNamespace.IsNetworkNeighborhood or FileView.RootFolderNamespace.IsNetworkNeighborhoodChild then
  begin
    settings:= @Columns.NetworkColSettings;
    grouping:= @GroupBy.NetworkGroupBySettings;
  end
  else
  begin
    settings:= @Columns.DefaultColSettings;
    grouping:= @GroupBy.DefaultGroupBySettings;
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
      doRebuild:= FileViewPage.FileView.ShowExtension <> ShowExtensions;
      AssignSettingsTo(FileViewPage, false);
      if doRebuild then
      FileViewPage.FileView.Rebuild;
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
  begin
    TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Refresh;
  end
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
    settings:= @Columns.MyComputerColSettings;
    grouping:= @GroupBy.MyComputerGroupBySettings;
  end
  else if FileView.RootFolderNamespace.IsControlPanel then
  begin
    settings:= @Columns.ControlPanelColSettings;
    grouping:= @GroupBy.ControlPanelGroupBySettings;
  end
  else if FileView.RootFolderNamespace.IsNetworkNeighborhood or FileView.RootFolderNamespace.IsNetworkNeighborhoodChild then
  begin
    settings:= @Columns.NetworkColSettings;
    grouping:= @GroupBy.NetworkGroupBySettings;
  end
  else
  begin
    settings:= @Columns.DefaultColSettings;
    grouping:= @GroupBy.DefaultGroupBySettings;
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

{-------------------------------------------------------------------------------
  Set AutosizeListViewStyle
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetAutosizeListViewStyle(const Value: Boolean);
begin
  fAutosizeListViewStyle:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Browse Zip Folders
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetBrowseZipFolders(const Value: Boolean);
begin
  fBrowseZipFolders:= Value;
  SendChanges;
end;

procedure TCEFileViewSettings.SetShowInfoBar(const Value: Boolean);
begin
  fShowInfoBar:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set SortFolderFirstAlways
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetSortFolderFirstAlways(const Value: Boolean);
begin
  fSortFolderFirstAlways:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set ThreadedDetails
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetThreadedDetails(const Value: Boolean);
begin
  fThreadedDetails:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set ThreadedEnumeration
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetThreadedEnumeration(const Value: Boolean);
begin
  fThreadedEnumeration:= Value;
  SendChanges;
end;

{-------------------------------------------------------------------------------
  Set ThreadedImages
-------------------------------------------------------------------------------}
procedure TCEFileViewSettings.SetThreadedImages(const Value: Boolean);
begin
  fThreadedImages:= Value;
  SendChanges;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set Path
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetPath: WideString;
var
  i: Integer;
begin
  if FileViewPage.FileView.RootFolderNamespace.IsDesktop then
  i:= 0
  else
  i:= CE_SpecialNamespaces.GetSpecialID(FileViewPage.FileView.RootFolderNamespace.AbsolutePIDL);

  if i > -1 then
  begin
    Result:= 'special://' + IntToStr(i);
  end
  else if WideDirectoryExists(FileViewPage.FileView.RootFolderNamespace.NameForParsing) then
  begin
    Result:= 'file://' + FileViewPage.FileView.RootFolderNamespace.NameForParsing;
  end
  else
  begin
    Result:= 'pidl://' + SavePIDLToMime(FileViewPage.FileView.RootFolderNamespace.AbsolutePIDL);
  end;
end;

{-------------------------------------------------------------------------------
  Get RememberInnerToolbarLayout
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetRememberInnerToolbarLayout: Boolean;
begin
  Result:= GlobalFileViewSettings.RememberInnerToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberOuterToolbarLayout
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetRememberOuterToolbarLayout: Boolean;
begin
  Result:= GlobalFileViewSettings.RememberOuterToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberPanelLayout
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetRememberPanelLayout: Boolean;
begin
  Result:= GlobalFileViewSettings.RememberPanelLayout;
end;

{-------------------------------------------------------------------------------
  Set Path
-------------------------------------------------------------------------------}
procedure TCEFileViewPageSettings.SetPath(const Value: WideString);
var
  format, fPath: WideString;
  i,c: Integer;
  PIDL: PItemIDList;
begin
  FileViewPage.FileView.BeginUpdate;
  try
    i:= Pos('://', Value);
    if i > 0 then
    begin
      c:= Length(Value);
      format:= LeftStr(Value, i-1);
      fPath:= RightStr(Value, c - i-2);

      if format = 'file' then
      begin
        PIDL:= PathToPIDL(fPath);
      end
      else if format = 'pidl' then
      begin
        PIDL:= LoadPIDLFromMime(fPath);
      end
      else if format = 'special' then
      begin
        SHGetspecialFolderLocation(0, StrToIntDef(fPath, 0), PIDL);
      end
      else begin
        PIDL:= PathToPIDL(fPath);
      end;
    end
    else
    begin
      PIDL:= PathToPIDL(Value);
    end;

    if assigned(PIDL) then
    begin
      FileViewPage.FileView.BrowseToByPIDL(PIDL);
      FileViewPage.FileView.ClearHistory;
      FileViewPage.FileView.fChangeHistory:= false;
      FileViewPage.FileView.History.Add(TNamespace.Create(PIDLMgr.CopyPIDL(PIDL),nil),true);
      FileViewPage.FileView.fChangeHistory:= true;
    end;
  finally
    FileViewPage.FileView.EndUpdate(false);
    FileViewPage.UpdateCaption;
  end;
end;

{-------------------------------------------------------------------------------
  Get/Set ViewStyle
-------------------------------------------------------------------------------}
function TCEFileViewPageSettings.GetViewStyle: TEasyListStyle;
begin
  Result:= FileViewPage.ViewStyle;
end;
procedure TCEFileViewPageSettings.SetViewStyle(const Value: TEasyListStyle);
begin
  FileViewPage.ViewStyle:= Value;
end;

{##############################################################################}

initialization
  GlobalFileViewSettings:= TCEFileViewSettings.Create;
  GlobalAppSettings.AddItem('FileView', GlobalFileViewSettings, true);

  TabPageClassList.RegisterClass('FileView', TCEFileViewPage, TCEFileViewPageSettings);

finalization
  FreeAndNil(GlobalFileViewSettings);

end.
