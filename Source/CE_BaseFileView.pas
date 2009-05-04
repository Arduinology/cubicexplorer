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
//  The Original Code is CE_BaseFileView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_BaseFileView;

interface

uses
  // CE Units
  CE_VistaFuncs, CE_Utils,
  // Jvcl
  JvBalloonHint,
  // Easy Listview
  EasyListview,
  // VSTools
  MPCommonObjects, MPCommonUtilities, MPShellUtilities,
  VirtualExplorerEasyListview, VirtualResources,
  VirtualExplorerTree,  VirtualShellHistory,
  VirtualShellNewMenu, VirtualThumbnails,
  // SpTBXLib
  SpTBXItem, SpTBXSkins,
  // Graphics32
  GR32,
  // Tnt Controls
  TntSysUtils, TntStdCtrls, TntWideStrUtils,
  // System Units
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Forms,
  Graphics, Menus, ShellAPI, Math, ShlObj, ActiveX, CommCtrl,
  VirtualShellNotifier;

type
  TNamespaceHack = class(TNamespace);

  TEasyEditManagerHack = class(TEasyEditManager);

  TCEStringEditor = class(TEasyStringEditor)
  protected
  public
    function SetEditorFocus: Boolean; override;
  end;

  TCEMemoEditor = class(TEasyMemoEditor)
  public
    function SetEditorFocus: Boolean; override;
  end;

  TCECustomFileView = class(TVirtualExplorerEasyListview, IShellBrowser, IOleWindow)
  private
    FContextMenuItem: TEasyItem;
    TmpScrollStep,TmpScrollSize, TmpScrollCountSize: Integer;
    ScrollDown: Boolean;
    ScrollAnimTimer: TTimer;
    fFullSizeColumn: Integer;
    fScrollSize: Integer;
    fScrollStep: Integer;
    fShowExtension: Boolean;
    fSmoothScroll: Boolean;
    fUsePNGAlpha: Boolean;
    procedure SetFullSizeColumn(const Value: Integer);
    procedure SpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  protected
    function BrowseObject(pidl: PItemIDList; flags: UINT): HResult; stdcall;
    procedure HandleContextMenuAfterCmdCallback(Namespace: TNamespace; Verb:
        WideString; MenuItemID: Integer; Successful: Boolean);
    procedure HandleContextMenuCmdCallback(Namespace: TNamespace; Verb: WideString;
        MenuItemID: Integer; var Handled: Boolean);
    procedure HandleContextMenuShowCallback(Namespace: TNamespace; Menu: hMenu; var
        Allow: Boolean);
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    procedure DoColumnCustomView(Column: TEasyColumn; var ViewClass:
        TEasyViewColumnClass); override;
    procedure DoColumnSizeChanging(Column: TEasyColumn; Size, NewSize: Integer; var
        Allow: Boolean); override;
    procedure DoColumnVisibilityChanged(Column: TEasyColumn); override;
    procedure DoItemContextMenu(HitInfo: TEasyHitInfoItem; WindowPoint: TPoint; var
        Menu: TPopupMenu; var Handled: Boolean); override;
    procedure DoItemCreateEditor(Item: TEasyItem; var Editor: IEasyCellEditor);
        override;
    procedure DoItemEditBegin(Item: TEasyItem; var Column: Integer; var Allow:
        Boolean); override;
    procedure DoItemEdited(Item: TEasyItem; var NewValue: Variant; var Accept:
        Boolean); override;
    procedure DoItemEditEnd(Item: TEasyItem); override;
    procedure DoItemGetCaption(Item: TEasyItem; Column: Integer; var ACaption:
        WideString); override;
    procedure DoItemGetEditCaption(Item: TEasyItem; Column: TEasyColumn; var
        Caption: WideString); override;
    procedure DoItemPaintText(Item: TEasyItem; Position: Integer; ACanvas:
        TCanvas); override;
    procedure DoItemSetCaption(Item: TEasyItem; Column: Integer; const Caption:
        WideString); override;
    procedure DoItemThumbnailDraw(Item: TEasyItem; ACanvas: TCanvas; ARect: TRect;
        AlphaBlender: TEasyAlphaBlender; var DoDefault: Boolean); override;
    procedure DoKeyAction(var CharCode: Word; var Shift: TShiftState; var
        DoDefault: Boolean); override;
    procedure DoLetterSearch(ALetter: WideChar); virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos:
        TPoint): Boolean; override;
    procedure DoPaintHeaderBkGnd(ACanvas: TCanvas; ARect: TRect; var Handled:
        Boolean); override;
    procedure DoScroll(DeltaX, DeltaY: Integer); override;
    procedure DoShellNotify(ShellEvent: TVirtualShellEvent); override;
    function EnableModelessSB(Enable: BOOL): HResult; stdcall;
    function GetControlWindow(ID: UINT; out Wnd: HWND): HResult; stdcall;
    function GetViewStateStream(Mode: DWORD; out Stream: IStream): HResult; stdcall;
    // IOLEWindow
    function GetWindow(out wnd: HWnd): HResult; stdcall;
    procedure HandleMouseDown(Button: TCommonMouseButton; Msg: TWMMouse); override;
    procedure HandleMouseUp(Button: TCommonMouseButton; Msg: TWMMouse); override;
    // IShellBrowser
    function InsertMenusSB(hMenuShared: HMENU; out MenuWidths:
        TOleMenuGroupWidths): HResult; stdcall;
    function OnViewWindowActive(var ShellView: IShellView): HResult; stdcall;
    function QueryActiveShellView(var ShellView: IShellView): HResult; stdcall;
    function RemoveMenusSB(hMenuShared: HMENU): HResult; stdcall;
    function SendControlMsg(ID, Msg: UINT; wParm: WPARAM; lParm: LPARAM; var Rslt:
        LResult): HResult; stdcall;
    function SetMenuSB(hMenuShared: HMENU; hOleMenuReserved: HOLEMENU;
        hwndActiveObject: HWND): HResult; stdcall;
    function SetStatusTextSB(StatusText: POleStr): HResult; stdcall;
    function SetToolbarItems(TBButton: PTBButton; nButtons, uFlags: UINT): HResult;
        stdcall;
    function TranslateAcceleratorSB(Msg: PMsg; ID: Word): HResult; stdcall;
    procedure WMMenuSelect(var Msg: TWMMenuSelect); message WM_MENUSELECT;
    procedure WMSize(var Msg: TWMSize); override;
  public
    BalloonHint: TJvBalloonHint;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnScrollAnimTimer(Sender: TObject);
    procedure ResizeColumns(ChangingColumn: TEasyColumn = nil; NewSize: Integer =
        0);
    property FullSizeColumn: Integer read fFullSizeColumn write SetFullSizeColumn;
    property ScrollSize: Integer read fScrollSize write fScrollSize;
    property ScrollStep: Integer read fScrollStep write fScrollStep;
    property ShowExtension: Boolean read fShowExtension write fShowExtension;
    property SmoothScroll: Boolean read fSmoothScroll write fSmoothScroll;
    property UsePNGAlpha: Boolean read fUsePNGAlpha write fUsePNGAlpha;
  end;

  TCEViewColumn = class(TEasyViewColumn)
  private
    fCEFileView: TCECustomFileView;
  protected
  public
    constructor Create(AnOwner: TCustomEasyListview); override;
    destructor Destroy; override;
    procedure LoadTextFont(Column: TEasyColumn; ACanvas: TCanvas); override;
    procedure PaintBkGnd(Column: TEasyColumn; ACanvas: TCanvas; HeaderType: TEasyHeaderType; RectArray: TEasyRectArrayObject); override;
  end;

implementation

{*------------------------------------------------------------------------------
  Get's called on set focus
-------------------------------------------------------------------------------}
function TCEStringEditor.SetEditorFocus: Boolean;
var
  l: Integer;
  edit: TEasyEdit;
  NS: TNamespace;
begin
  //inherited SetEditorFocus;
  Editor.SetFocus;
  edit:= (Editor as TEasyEdit);

  TCECustomFileView(self.Listview).ValidateNamespace(self.Item, NS);
  if assigned(NS) then
  begin
    if NS.Folder and (not NS.Browsable) then
    begin
      edit.SelectAll;
    end
    else if NS.FileSystem then
    begin
      l:= Length(WideExtractFileExt(edit.Text));
      edit.SelStart:= 0;
      edit.SelLength:= Length(edit.Text) - l;
    end;
  end
  else
  begin
    edit.SelectAll;
  end;
  Result:= true;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Get's called on set focus
-------------------------------------------------------------------------------}
function TCEMemoEditor.SetEditorFocus: Boolean;
var
  l: Integer;
  edit: TEasyMemo;
  NS: TNamespace;
begin
  //inherited SetEditorFocus;
  Editor.SetFocus;
  edit:= (Editor as TEasyMemo);

  TCECustomFileView(self.Listview).ValidateNamespace(self.Item, NS);
  if assigned(NS) then
  begin
    if NS.Folder and (not NS.Browsable) then
    begin
      edit.SelectAll;
    end
    else if NS.FileSystem then
    begin
      l:= Length(WideExtractFileExt(edit.Text));
      edit.SelStart:= 0;
      edit.SelLength:= Length(edit.Text) - l;
    end;
  end
  else
  begin
    edit.SelectAll;
  end;
  Result:= true;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create TCECustomFileView.
-------------------------------------------------------------------------------}
constructor TCECustomFileView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetVistaFont(Header.Font);
  fFullSizeColumn:= -1;
  self.BevelInner:= bvNone;
  self.BevelOuter:= bvNone;
  self.Options:= [
                  eloBrowseExecuteFolder,          // Browse the folder instead of opening it on Windows Explorer
                  eloBrowseExecuteFolderShortcut,  // Browse the folder shortcut instead of opening it on Windows Explorer
                  //eloBrowseExecuteZipFolder,       // Browse the zip folder instead of opening it
                  eloExecuteOnDblClick,            // Browse or execute on double click
                  //eloHideRecycleBin,               // Hides the RecycleBin
                  //eloThreadedEnumeration,          // Uses a thread to enumerate the items in a view before showing them
                  eloThreadedImages,               // Use a thread to retrieve the item's icons
                  //eloThreadedDetails,              // Use threaded detail extraction if the Column reports it is slow
                  eloQueryInfoHints,               // Show the popup shell information tip when hovering over items
                  eloShellContextMenus,            // Show the shell context menus for the items
                  eloChangeNotifierThread,         // Control tracks changes in the shell
                  eloTrackChangesInMappedDrives,   // When the shell notifies the control of a change any mapped drives are included in the refresh
                  eloNoRebuildIconListOnAssocChange, // In XP Rebuilding the IconList can cause the icons on the desktop to be rearranged
                  //eloRemoveContextMenuShortCut,    // Removes the Shortcut item from the context menu.  Used mainly when in the explorer Treeview to be consistent with Explorer
                  //eloPerFolderStorage,             // Saves the state of each folder (grouping, columns state etc)
                  //eloUseColumnOnByDefaultFlag,     // Checks to see if the column handler is "on by default" and makes the column visible if it is.  Some handlers misuse this so make it an option
                  //eloFullFlushItemsOnChangeNotify, // Enabled the best possible updating of item information (especially in Details mode such as when file size changes). When enabled the TNamespace is recreated for each item with the fresh PIDL.  Great for full update, bad for performance.
                  eloGhostHiddenFiles              // Draws the image blended (ghosted) if the item is hidden
                  ];
  self.DragManager.Enabled:= true;
  self.Header.Visible:= true;
  self.Header.Height:= 18;
  self.ShowInactive:= false;
  self.EditManager.Enabled:= true;
  self.Selection.MultiSelect:= true;
  self.Selection.EnableDragSelect:= true;
  self.Selection.FullRowSelect:= true;
  self.ThumbsManager.UseFoldersShellExtraction:= true;
  self.ThumbsManager.UseShellExtraction:= true;
  self.ThumbsManager.UseExifThumbnail:= true;
  self.ThumbsManager.UseSubsampling:= true;
  self.UsePNGAlpha:= false;
  self.ShowGroupMargins:= false;
  self.Grouped:= false;
  self.IncrementalSearch.Enabled:= true;
  self.IncrementalSearch.StartType:= eissFocusedNode;

  self.BackGround.Caption:= 'Empty folder.';
  self.BackGround.CaptionAlignment:= taCenter;
  self.BackGround.CaptionShowOnlyWhenEmpty:= true;
  self.BackGround.CaptionSingleLine:= true;
  self.BackGround.CaptionShow:= true;

  self.CompressedFile.Hilight:= true;
  self.CompressedFile.Font.Assign(self.Font);

  self.Scrollbars.SnapHorzView:= true;
  
  BalloonHint:= TJvBalloonHint.Create(self);
  BalloonHint.Options:= [];
  BalloonHint.DefaultBalloonPosition:= bpLeftUp;

  SkinManager.AddSkinNotification(Self);

  fScrollSize:= 80;
  fScrollStep:= 10;
  fSmoothScroll:= false;
  
  ScrollAnimTimer:= TTimer.Create(Self);
  ScrollAnimTimer.Enabled:= false;
  ScrollAnimTimer.Interval:= 10;
  ScrollAnimTimer.OnTimer:= OnScrollAnimTimer;
end;

{*------------------------------------------------------------------------------
  Destroy TCECustomFileView
-------------------------------------------------------------------------------}
destructor TCECustomFileView.Destroy;
begin
  SkinManager.RemoveSkinNotification(Self);
  inherited;
end;

{*------------------------------------------------------------------------------
  Assign a custom ColumnView
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoColumnCustomView(Column: TEasyColumn; var
    ViewClass: TEasyViewColumnClass);
begin
  ViewClass:= TCEViewColumn;
end;

{*------------------------------------------------------------------------------
  Get's called on item edit.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoItemEdited(Item: TEasyItem; var NewValue: Variant; var
    Accept: Boolean);

  function GetPopupPoint: TPoint;
  var
    r: TRect;
    RectArray: TEasyRectArrayObject;
  begin
    if assigned(Self.EditManager.Editor) then
    begin
      GetWindowRect(Self.EditManager.Editor.Handle,r);
      Result.Y:= r.Top;
      Result.X:= r.Left + ((r.Right - r.Left) div 2);
    end
    else
    begin
      Item.ItemRectArray(nil,nil,RectArray);
      Result.X:= RectArray.EditRect.Left + ((RectArray.EditRect.Right - RectArray.EditRect.Left) div 2);
      Result.Y:= RectArray.EditRect.Top;
      Result:= self.Scrollbars.MapViewToWindow(Result);
      Result:= self.ClientToScreen(Result);
    end;
  end;

var
  path: WideString;
  oldCaption: WideString;
  p: TPoint;
  NS: TNamespace;
begin
  inherited;

  // Hack around some bug in EasyListView. It adds 2 spaces into the file name for some reason.
  if TntWideLastChar(item.Caption) = ' ' then
  begin
    path:= NewValue + '  ';
  end
  else
  begin
    path:= NewValue;
  end;
  // end of hack

  self.ValidateNamespace(Item, NS);
  if NS.Folder and not NS.Browsable then
  oldCaption:= NS.NameForEditing // Folders
  else
  oldCaption:= NS.NameParseAddressInFolder;
  

  if WideCompareStr(path,oldCaption) = 0 then
  begin
    Exit;
  end
  else if WideCompareText(path,oldCaption) = 0 then
  begin
    Accept:= true;
    Exit;
  end;

  path:= self.RootFolderNamespace.NameForParsing +'\'+ NewValue;

  if NS.Folder and not NS.Browsable then
  begin
    if NewValue = '' then
    begin
      Accept:= false;
      p:= GetPopupPoint;
      BalloonHint.ActivateHintPos(nil, p,'Name can''t be empty.', 'Please choose another '#13'or press ESC to Cancel',2000, ikError);
    end
    else if WideDirectoryExists(path) then
    begin
      Accept:= false;
      p:= GetPopupPoint;
      BalloonHint.ActivateHintPos(nil, p,'Duplicate folder name','Please choose another '#13'or press ESC to Cancel',2000, ikError);
    end
    else
    begin
      Accept:= true;
    end;
  end
  else if NS.FileSystem then
  begin
    if NewValue = '' then
    begin
      Accept:= false;
      p:= GetPopupPoint;
      BalloonHint.ActivateHintPos(nil, p,'Name can''t be empty.', 'Please choose another '#13'or press ESC to Cancel',2000, ikError);
    end
    else if WideFileExists(path) then
    begin
      Accept:= false;
      p:= GetPopupPoint;
      BalloonHint.ActivateHintPos(nil, p,'Duplicate file name','Please choose another '#13'or press ESC to Cancel',2000, ikError);
    end
    else
      Accept:= true;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when Header background is painted.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoPaintHeaderBkGnd(ACanvas: TCanvas; ARect: TRect; var
    Handled: Boolean);
begin
  inherited;
  SpDrawXPHeader(ACanvas, ARect, false, false, SkinManager.GetSkinType);
  Handled:= true;
end;

{-------------------------------------------------------------------------------
  Get's called in mouse button down.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.HandleMouseDown(Button: TCommonMouseButton; Msg:
    TWMMouse);
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called in mouse button up.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.HandleMouseUp(Button: TCommonMouseButton; Msg: TWMMouse);
var
  Pt: TPoint;
  KeyState: TCommonKeyStates;
  GroupHitInfo: TEasyGroupHitTestInfoSet;
  ItemHitInfo: TEasyItemHitTestInfoSet;
  Group: TEasyGroup;
  Item: TEasyItem;
  CtlDown, ShiftDown, DoDefaultItemUp: Boolean;
begin
  Group := nil;
  KeyState := KeyToKeyStates(Msg.Keys);
  CtlDown := cksControl in KeyState;
  ShiftDown := cksShift in KeyState;
  Pt := Scrollbars.MapWindowToView(Msg.Pos);

  if ([ebcsLButtonDown, ebcsRButtonDown, ebcsMButtonDown] * States <> []) then
  begin
    if (ebcsGroupExpandPending in States) or (ebcsCheckboxClickPending in States) or
       (ebcsDragSelecting in States) or (ebcsDragging in States) then
    begin
      inherited;
    end
    else
    begin
      Item := ClickTestItem(Pt, Group, KeyState, ItemHitInfo);
      if Assigned(Item) then
      begin
        DoDefaultItemUp := True;
        DoItemMouseUp(Item, Button, DoDefaultItemUp);
        if DoDefaultItemUp then
        begin
          DoItemClick(Item, KeyToKeyStates(Msg.Keys), ItemHitInfo);
          if not Item.SelectionHitPt(Pt, eshtClickselect) then
            Selection.ClearAll
          else
          begin
            // Allow MultiSelect
            if Selection.MultiSelect then
            begin
              if not (ShiftDown or CtlDown) and (not Item.Selected or (Button = cmbLeft)) then
                Selection.ClearAllExcept(Item)
              else
              begin
                if CtlDown then
                begin
                  TEasyEditManagerHack(EditManager).StopAutoEditTimer;
                  Item.Focused := True;
                  if not ShiftDown then
                  Item.Selected:= not Item.Selected;
                end
              end
            end
          end
        end
      end
      else
      begin
        Group := ClickTestGroup(Pt, KeyToKeyStates(Msg.Keys), GroupHitInfo);
        if Assigned(Group) then
          DoGroupClick(Group, KeyToKeyStates(Msg.Keys), GroupHitInfo);
        Selection.ClearAll
      end
    end
  end;
end;

{*------------------------------------------------------------------------------
  Resize FullSized Column
-------------------------------------------------------------------------------}
procedure TCECustomFileView.ResizeColumns(ChangingColumn: TEasyColumn = nil;
    NewSize: Integer = 0);
var
  i, w: Integer;
  col: TEasyColumn;
begin
  if csDestroying in self.ComponentState then
  Exit;
  
  if (fFullSizeColumn < 0) or (fFullSizeColumn >= Self.Header.Columns.Count) then
  Exit;
  
  w:= 0;
  for i:= 0 to Self.Header.Columns.Count - 1 do
  begin
    col:= Self.Header.Columns.Columns[i];
    if col = ChangingColumn then
    begin
      Inc(w, NewSize);
    end
    else if col.Visible and (col.Index <> fFullSizeColumn) then
    begin
      Inc(w, col.Width);
    end;
  end;
  col:= Self.Header.Columns.Columns[fFullSizeColumn];
  if col.Visible and self.Active then
  col.Width:= Max(self.ClientWidth - w, 50);
end;

{*------------------------------------------------------------------------------
  Set FullSizeColumn Index
-------------------------------------------------------------------------------}
procedure TCECustomFileView.SetFullSizeColumn(const Value: Integer);
begin
  fFullSizeColumn:= Value;
  ResizeColumns;
end;

{*------------------------------------------------------------------------------
  Get's called on column resize.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoColumnSizeChanging(Column: TEasyColumn; Size,
    NewSize: Integer; var Allow: Boolean);
begin
  ResizeColumns(Column, NewSize);
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called on column visibility change.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoColumnVisibilityChanged(Column: TEasyColumn);
begin
  ResizeColumns;
end;

{*------------------------------------------------------------------------------
  Get's called on Item Context menu popup.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoItemContextMenu(HitInfo: TEasyHitInfoItem;
    WindowPoint: TPoint; var Menu: TPopupMenu; var Handled: Boolean);
var
  b, showM: Boolean;
begin
  Handled:= false;
  if Self.EditManager.Editing then
  begin
    if ehtOnLabel in HitInfo.HitInfo then
    begin
      Handled:= true;
      Exit;
    end;
  end;

  if View = elsReport then
  showM:= (HitInfo.HitInfo * [ehtOnLabel, ehtOnIcon]  <> [])
  else
  showM:= (HitInfo.HitInfo * [ehtOnText, ehtOnIcon]  <> []);

  Handled:= not showM;
  if Assigned(OnItemContextMenu) then
    OnItemContextMenu(Self, HitInfo, WindowPoint, Menu, Handled);
  showM:= not Handled;

  if showM then
  begin
    b:= ShellNotifySuspended;
    ShellNotifySuspended:= True;
    FContextMenuItem := HitInfo.Item;
    try
      TExplorerItem(HitInfo.Item).Namespace.ShowContextMenuMulti(Self, HandleContextMenuCmdCallback,
        HandleContextMenuShowCallback, HandleContextMenuAfterCmdCallback, SelectedToNamespaceArray, @WindowPoint,
        nil, '', TExplorerItem(HitInfo.Item).Namespace);
      Handled:= True
    finally
      FContextMenuItem := nil;
      ShellNotifySuspended:= b;
    end
  end;

//  Handled:= showM;
//
//  if Assigned(OnItemContextMenu) then
//    OnItemContextMenu(Self, HitInfo, WindowPoint, Menu, Handled);
end;

{*------------------------------------------------------------------------------
  Get's called on size change.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.WMSize(var Msg: TWMSize);
begin
  inherited;
  ResizeColumns;
end;

{*------------------------------------------------------------------------------
  Create editor
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoItemCreateEditor(Item: TEasyItem; var Editor:
    IEasyCellEditor);
begin
  if Assigned(OnItemCreateEditor) then
    OnItemCreateEditor(Self, Item, Editor);
  if not Assigned(Editor) then
  begin
    if (View in MULTILINEVIEWS) and (View <> elsTile) then
      Editor := TCEMemoEditor.Create
    else
      Editor := TCEStringEditor.Create;
  end
end;

{*------------------------------------------------------------------------------
  Get's called when item caption is needed.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoItemGetCaption(Item: TEasyItem; Column: Integer;
    var ACaption: WideString);
var
  NS: TNamespaceHack;
begin
  if Column = 0 then
  begin
    if Self.ValidateNamespace(Item, TNamespace(NS)) then
    begin
      if not (scInFolderName in NS.ShellCache.ShellCacheFlags) then
      begin
        // Normal files
        if NS.FileSystem and not (NS.Folder and not NS.Browsable) then
        begin
          // Show Extension
          if fShowExtension then 
          begin
            // Recycle bin item
            if assigned(NS.Parent) and NS.Parent.IsRecycleBin then 
            NS.FShellCache.Data.InFolderName:= WideExtractFileName(NS.DisplayNameOf(SHGDN_NORMAL))
            // Normal file
            else
            begin
              NS.FShellCache.Data.InFolderName:= NS.DisplayNameOf(SHGDN_INFOLDER or SHGDN_FORPARSING);
            end;
          end
          // Hide Extension
          else 
          begin
            // Recycle bin item
            if assigned(NS.Parent) and NS.Parent.IsRecycleBin then
            NS.FShellCache.Data.InFolderName:= WideStripExt(WideExtractFileName(NS.DisplayNameOf(SHGDN_NORMAL)))
            // Normal file
            else
            begin
              NS.FShellCache.Data.InFolderName:= WideStripExt(NS.DisplayNameOf(SHGDN_INFOLDER or SHGDN_FORPARSING));
              if NS.FShellCache.Data.InFolderName = '' then
              NS.FShellCache.Data.InFolderName:= NS.DisplayNameOf(SHGDN_INFOLDER or SHGDN_FORPARSING);
            end;
          end;
        end
        // Everything else
        else
        begin
          NS.FShellCache.Data.InFolderName:= NS.DisplayNameOf(SHGDN_INFOLDER or SHGDN_NORMAL);
          if NS.FShellCache.Data.InFolderName = '' then
          NS.FShellCache.Data.InFolderName:= NS.DisplayNameOf(SHGDN_INFOLDER or SHGDN_FORPARSING);
        end;
        Include(NS.FShellCache.ShellCacheFlags, scInFolderName);
      end;
      ACaption:= NS.NameInFolder;
   end;
  end
  else
  begin
    inherited;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when item text is being painted.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoItemPaintText(Item: TEasyItem; Position: Integer;
    ACanvas: TCanvas);
begin
  if assigned(Item) and assigned(TExplorerItem(Item).Namespace) then
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when thumbnail is drawn (used to draw PNG image with alpha)
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoItemThumbnailDraw(Item: TEasyItem; ACanvas:
    TCanvas; ARect: TRect; AlphaBlender: TEasyAlphaBlender; var DoDefault:
    Boolean);

  function HasAlpha(bitmap: TBitmap32): Boolean;
  var
    y,x: Integer;
    row: PColor32Array;
  begin
    Result:= false;
    for y:= 0 to bitmap.Height - 1 do
    begin
      row:= bitmap.ScanLine[y];
      for x:= 0 to bitmap.Width - 1 do
      begin
        if ((row[x] shr 24) > 0) then
        begin
          Result:= true;
          exit;
        end;
      end;
    end;
  end;

var
  bit,bit2: TBitmap32;
  info: TThumbInfo;
  NS: TNamespace;
  OldSubSampling: Boolean;
begin
  if not fUsePNGAlpha then
  begin
    inherited;
    Exit;
  end;

  ValidateNamespace(Item,NS);
  if assigned(NS) then
  begin
    if CompareText(NS.Extension, '.png') <> 0 then
    begin
      inherited;
      Exit;
    end
    else
    begin
      OldSubSampling:= ThumbsManager.UseSubsampling;
      ThumbsManager.UseSubsampling:= false;
    end;
  end
  else
  begin
    inherited;
    Exit;
  end;

  if TExplorerItem(Item).ThumbInfo = nil then
  inherited;

  info:= TExplorerItem(Item).ThumbInfo;
  if assigned(info) then
  begin
    bit:= TBitmap32.Create;
    bit.DrawMode:= dmBlend;
    bit2:= Tbitmap32.Create;
    bit2.DrawMode:= dmBlend;
    try
      bit.SetSize(info.ThumbSize.X,info.ThumbSize.Y);
      info.Draw(bit.Canvas,bit.BoundsRect,talCenter);
      if HasAlpha(bit) then
      begin
        bit2.SetSize(ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
        bit2.Clear(Color32(Color));
        bit2.Draw(Round((bit2.Width - bit.Width) / 2),Round((bit2.Height - bit.Height) / 2),bit);
        bit2.DrawTo(ACanvas.Handle,ARect,bit2.BoundsRect);
        DoDefault:= false;
      end
      else
      begin
        inherited;
      end;
    finally
      bit.Free;
      bit2.Free;
      ThumbsManager.UseSubsampling:= OldSubSampling;
    end;
  end
  else
    ThumbsManager.UseSubsampling:= OldSubSampling;
end;

{*------------------------------------------------------------------------------
  Handle Key press
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoKeyAction(var CharCode: Word; var Shift:
    TShiftState; var DoDefault: Boolean);
var
  NS: TNamespace;
begin
  if (CharCode = VK_RETURN) and (Selection.Count > 1) then
  begin
    Self.ValidateNamespace(Self.Selection.First, NS);
    NS.ExecuteContextMenuVerb('open', NamespaceToRelativePIDLArray(Self.SelectedToNamespaceArray));
    DoDefault:= false;
  end;

  case CharCode of
    VK_F5: DoDefault:= false;  
  end;
  
  inherited;// DoKeyAction;
end;

{*------------------------------------------------------------------------------
  Do Letter Search
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoLetterSearch(ALetter: WideChar);
var
  SearchBuffer,CompareStr: WideString;
  i: Integer;
  item: TEasyItem;
  CompareResult: Integer;
begin
  SearchBuffer:= ALetter;

  if assigned(Self.Selection.FocusedItem) then
  begin
    CompareStr:= Self.Selection.FocusedItem.Caption;
    SetLength(CompareStr, Length(SearchBuffer));

    CompareResult:= WideCompareText(SearchBuffer, CompareStr);

    if CompareResult <> 0 then
    item:= Self.Groups.FirstItem
    else
    item:= Self.Selection.FocusedItem;
  end
  else
  begin
    item:= Self.Groups.FirstItem;
  end;

  if not assigned(item) then
  exit;
  
  CompareResult:= -1;

  for i:= item.Index to Self.ItemCount-1 do
  begin
    CompareStr := Self.Items.Items[i].Caption;
    SetLength(CompareStr, Length(SearchBuffer));

    CompareResult:= WideCompareText(SearchBuffer, CompareStr);

    if CompareResult = 0 then
    begin
      if Self.Selection.FocusedItem <> Self.Items.Items[i] then
      begin
        if Self.Selection.Count > 0 then
        Self.Selection.ClearAll;

        Self.Selection.FocusedItem:= Self.Items.Items[i];
        Self.Items.Items[i].Selected:= true;
        Self.Items.Items[i].MakeVisible(emvAuto);
        break;
      end;
    end;
  end;
  if CompareResult <> 0 then
  begin
    for i:= 0 to item.Index do
    begin
      CompareStr := Self.Items.Items[i].Caption;
      SetLength(CompareStr, Length(SearchBuffer));

      CompareResult:= WideCompareText(SearchBuffer, CompareStr);

      if CompareResult = 0 then
      begin
        if Self.Selection.FocusedItem <> Self.Items.Items[i] then
        begin
          if Self.Selection.Count > 0 then
          Self.Selection.ClearAll;

          Self.Selection.FocusedItem:= Self.Items.Items[i];
          Self.Items.Items[i].Selected:= true;
          Self.Items.Items[i].MakeVisible(emvAuto);
          break;
        end;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on mouse wheel scroll.
-------------------------------------------------------------------------------}
function TCECustomFileView.DoMouseWheel(Shift: TShiftState; WheelDelta:
    Integer; MousePos: TPoint): Boolean;
var
  handled: Boolean;
  delta: Integer;
begin
  handled:= false;
  
  if assigned(OnMouseWheel) then
  OnMouseWheel(Self, Shift, WheelDelta, MousePos, handled);

  Result:= true;

  if handled then Exit;
  
  if not fSmoothScroll then
  begin
    if (Self.View = elsFilmstrip) or (Self.View = elsList) then
    begin
      case Self.View of
        elsList: delta:= Self.CellSizes.List.Width;
        elsFilmstrip: delta:= Self.CellSizes.Filmstrip.Width;
        else
        delta:= fScrollSize;
      end;

      if WheelDelta > 0 then
      self.Scrollbars.Scroll(-delta,0)
      else if WheelDelta < 0 then
      self.Scrollbars.Scroll(delta,0);
    end
    else
    begin
      if WheelDelta > 0 then
      self.Scrollbars.Scroll(0,-fScrollSize)
      else if WheelDelta < 0 then
      self.Scrollbars.Scroll(0,fScrollSize);
    end;
    Exit;
  end;

  if WheelDelta = 0 then
  Exit;

  if WheelDelta > 0 then  // Up Scroll
  begin
    if ScrollAnimTimer.Enabled and ScrollDown then
    begin
      ScrollAnimTimer.Enabled:= false;
      TmpScrollSize:= 0;
      TmpScrollStep:= 0;
      TmpScrollCountSize:= 0;
    end;
    ScrollDown:= false;
  end
  else if WheelDelta < 0 then // Down scroll
  begin
    if ScrollAnimTimer.Enabled and not ScrollDown then
    begin
      ScrollAnimTimer.Enabled:= false;
      TmpScrollSize:= 0;
      TmpScrollStep:= 0;
      TmpScrollCountSize:= 0;
    end;
    ScrollDown:= true;
  end;

  Inc(TmpScrollStep,fScrollStep);
  Inc(TmpScrollSize,fScrollSize);
  
  if not ScrollAnimTimer.Enabled then
  begin
    OnScrollAnimTimer(self);
    ScrollAnimTimer.Enabled:= true;
  end;
end;

{*------------------------------------------------------------------------------
  Animate scroll bars
-------------------------------------------------------------------------------}
procedure TCECustomFileView.OnScrollAnimTimer(Sender: TObject);
begin
  Inc(TmpScrollCountSize, TmpScrollStep);
  if (Self.View = elsFilmstrip) or (Self.View = elsList) then
  begin
    if ScrollDown then
    self.Scrollbars.Scroll(TmpScrollStep,0)
    else
    self.Scrollbars.Scroll(-TmpScrollStep,0);
  end
  else
  begin
    if ScrollDown then
    self.Scrollbars.Scroll(0,TmpScrollStep)
    else
    self.Scrollbars.Scroll(0,-TmpScrollStep);
  end;
  
  if TmpScrollCountSize >= TmpScrollSize then
  begin
    ScrollAnimTimer.Enabled:= false;
    TmpScrollCountSize:= 0;
    TmpScrollSize:= 0;
    TmpScrollStep:= 0;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when SpTBX skin is changed.
-------------------------------------------------------------------------------}
procedure TCECustomFileView.SpSkinChange(var Message: TMessage);
begin
  Repaint;
end;

{*------------------------------------------------------------------------------
  Get Item Edit Caption
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoItemGetEditCaption(Item: TEasyItem; Column:
    TEasyColumn; var Caption: WideString);
var
  NS: TNamespace;
begin
  NS:= TExplorerItem(Item).Namespace;

  if NS.FileSystem and not (NS.Folder and not NS.Browsable) then
  Caption:= NS.NameParseAddressInFolder // Files  (This is done because extensions might be cut when using NameForEditing).
  else
  Caption:= NS.NameForEditing; // Everything else
end;


{*------------------------------------------------------------------------------
  Set Item Caption
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoItemSetCaption(Item: TEasyItem; Column: Integer;
    const Caption: WideString);
const
  ALL_FOLDERS = SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN or SHGDN_FORPARSING;
var
  P, NewPIDL: PItemIDList;
  OldCursor: TCursor;
  NS: TNamespace;
  ParentWnd: HWND;
begin
  if Column < 1 then
  begin
    NS:= TExplorerItem(Item).Namespace;

    if NS.Folder and not NS.Browsable then
    NS.SetNameOf(Caption)
    else
    begin
      if NS.CanRename and Assigned(NS.ParentShellFolder) then
      begin
        OldCursor := Screen.Cursor;
        Screen.Cursor := crHourglass;
        try
        { The shell frees the PIDL so we need a copy }
          P:= PIDLMgr.CopyPIDL(NS.RelativePIDL);
          NewPIDL:= nil;

          ParentWnd:= 0;
          if MP_UseModalDialogs then
          ParentWnd:= GetActiveWindow;

          if Succeeded(NS.ParentShellFolder.SetNameOf(ParentWnd, P, PWideChar(Caption), ALL_FOLDERS, NewPIDL)) then
          begin
            if Assigned(NewPIDL) then
            begin
              TNamespaceHack(NS).ReplacePIDL(NewPIDL, NS.Parent);
            end
          end
        finally
          Screen.Cursor := OldCursor
        end
      end;
    end;

    TExplorerItem(Item).Namespace.InvalidateCache;
  end
end;

{*------------------------------------------------------------------------------
  Called on Item Edit Begin
-------------------------------------------------------------------------------}
procedure TCECustomFileView.DoItemEditBegin(Item: TEasyItem; var Column:
    Integer; var Allow: Boolean);
var
  NS: TNamespace;
begin
  inherited;
  if Column = 0 then
  begin
    NS:= TExplorerItem(Item).Namespace;
    Allow:= NS.CanRename;
  end;
end;

{*------------------------------------------------------------------------------
  Handle WMMenuSelect message
-------------------------------------------------------------------------------}
procedure TCECustomFileView.WMMenuSelect(var Msg: TWMMenuSelect);
var
  ChildMenu: hMenu;
begin
  if Assigned(FContextMenuItem) then
  begin
    if HiWord(Longword( TMessage( Msg).wParam)) and MF_POPUP <> 0 then
      ChildMenu := GetSubMenu(LongWord( TMessage( Msg).lParam), LoWord(Longword( TMessage( Msg).wParam)))
    else
      ChildMenu := 0;
    DoContextMenuSelect((FContextMenuItem as TExplorerItem).Namespace, LoWord(Longword( TMessage( Msg).wParam)), ChildMenu,
      HiWord(Longword( TMessage( Msg).wParam)) and MF_MOUSESELECT <> 0);
  end
end;

{-------------------------------------------------------------------------------
  Handle ContextMenuAfterCmdCallback
-------------------------------------------------------------------------------}
procedure TCECustomFileView.HandleContextMenuAfterCmdCallback(Namespace:
    TNamespace; Verb: WideString; MenuItemID: Integer; Successful: Boolean);
begin
  DoContextMenuAfterCmd(Namespace, Verb, MenuItemID, Successful);
end;

{-------------------------------------------------------------------------------
  Handle ContextMenuCmdCallback
-------------------------------------------------------------------------------}
procedure TCECustomFileView.HandleContextMenuCmdCallback(Namespace: TNamespace;
    Verb: WideString; MenuItemID: Integer; var Handled: Boolean);
begin
  if Assigned(Parent) then
    Handled := DoContextMenuCmd(Namespace, Verb, MenuItemID)
  else
    Handled := False
end;

{-------------------------------------------------------------------------------
  Handle ContextMenuShowCallback
-------------------------------------------------------------------------------}
procedure TCECustomFileView.HandleContextMenuShowCallback(Namespace:
    TNamespace; Menu: hMenu; var Allow: Boolean);

  function IndexIsSeparator(Index: Integer): Boolean;
  var
    MenuInfo: TMenuItemInfo;
  begin
    ZeroMemory(@MenuInfo, SizeOf(MenuInfo));
    MenuInfo.cbSize := SizeOf(MenuInfo);
    MenuInfo.fMask := MIIM_TYPE;
    GetMenuItemInfo(Menu, Index, True, MenuInfo);
    Result :=  MenuInfo.fType and MFT_SEPARATOR  <> 0
  end;

var
  i: Integer;
  S: AnsiString;
  Done: Boolean;

begin
  if Assigned(Parent) then
  begin
    if eloRemoveContextMenuShortCut in Options then
    begin
      Done := False;
      i := 0;
      while not Done and (i < GetMenuItemCount(Menu)) do
      begin
        S := Namespace.ContextMenuVerb(GetMenuItemID(Menu, i));
        if StrComp(PAnsiChar(S), 'link') = 0 then
        begin
          DeleteMenu(Menu, i, MF_BYPOSITION);
          if IndexIsSeparator(i - 1) then
          begin
            if (GetMenuItemCount(Menu) = i) or IndexIsSeparator(i) then
              DeleteMenu(Menu, i - 1, MF_BYPOSITION)
          end;
          Done := True
        end;
        Inc(i)
      end
    end;
    Allow := DoContextMenuShow(Namespace, Menu);
  end else
    Allow := False
end;

{*------------------------------------------------------------------------------
  BEGIN of IShellBrowser implementation
-------------------------------------------------------------------------------}
// IShellBrowser.BrowseObject
function TCECustomFileView.BrowseObject(pidl: PItemIDList; flags: UINT):
    HResult;
begin
  BrowseToByPIDL(pidl, True);
  Result:= S_OK
end;

// IShellBrowser.EnableModelessSB
function TCECustomFileView.EnableModelessSB(Enable: BOOL): HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.GetControlWindow
function TCECustomFileView.GetControlWindow(ID: UINT; out Wnd: HWND): HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.GetViewStateStream
function TCECustomFileView.GetViewStateStream(Mode: DWORD; out Stream:
    IStream): HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.InsertMenusSB
function TCECustomFileView.InsertMenusSB(hMenuShared: HMENU; out MenuWidths:
    TOleMenuGroupWidths): HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.OnViewWindowActive
function TCECustomFileView.OnViewWindowActive(var ShellView: IShellView):
    HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.QueryActiveShellView
function TCECustomFileView.QueryActiveShellView(var ShellView: IShellView):
    HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.RemoveMenusSB
function TCECustomFileView.RemoveMenusSB(hMenuShared: HMENU): HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.SendControlMsg
function TCECustomFileView.SendControlMsg(ID, Msg: UINT; wParm: WPARAM; lParm:
    LPARAM; var Rslt: LResult): HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.SetMenuSB
function TCECustomFileView.SetMenuSB(hMenuShared: HMENU; hOleMenuReserved:
    HOLEMENU; hwndActiveObject: HWND): HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.SetStatusTextSB
function TCECustomFileView.SetStatusTextSB(StatusText: POleStr): HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.SetToolbarItems
function TCECustomFileView.SetToolbarItems(TBButton: PTBButton; nButtons,
    uFlags: UINT): HResult;
begin
  Result:= E_NOTIMPL;
end;
// IShellBrowser.TranslateAcceleratorSB
function TCECustomFileView.TranslateAcceleratorSB(Msg: PMsg; ID: Word): HResult;
begin
  Result:= E_NOTIMPL;
end;
{*------------------------------------------------------------------------------
  END of IShellBrowser implementation
-------------------------------------------------------------------------------}

{*------------------------------------------------------------------------------
  BEGIN of IOleWindow implementation
-------------------------------------------------------------------------------}
// IOleWindow.GetWindow
function TCECustomFileView.GetWindow(out wnd: HWnd): HResult;
begin
  if HandleAllocated then
  begin
    wnd:= Handle;
    Result:= S_OK;
  end
  else
    Result:= E_NOTIMPL;
end;
// IOleWindow.ContextSensitiveHelp
function TCECustomFileView.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result:= E_NOTIMPL;
end;
{*------------------------------------------------------------------------------
  END of IShellBrowser implementation
-------------------------------------------------------------------------------}


procedure TCECustomFileView.DoItemEditEnd(Item: TEasyItem);
begin
  inherited;
end;

procedure TCECustomFileView.DoScroll(DeltaX, DeltaY: Integer);
begin
  if EditManager.Editing then
  EditManager.EndEdit;
  inherited;
end;

procedure TCECustomFileView.DoShellNotify(ShellEvent: TVirtualShellEvent);
begin
  inherited;
  if not ShellEvent.Handled then
  begin
    if ShellEvent.ShellNotifyEvent = vsneAssoccChanged then
    begin
      ShellEvent.Handled:= true;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create TCEViewColumn
-------------------------------------------------------------------------------}
constructor TCEViewColumn.Create(AnOwner: TCustomEasyListview);
begin
  inherited Create(AnOwner);
  fCEFileView:= TCECustomFileView(AnOwner);
end;

{*------------------------------------------------------------------------------
  Destroy TCEViewColumn
-------------------------------------------------------------------------------}
destructor TCEViewColumn.Destroy;
begin
  inherited Destroy;
end;

{*------------------------------------------------------------------------------
  Gets called when Text Font is loaded.
-------------------------------------------------------------------------------}
procedure TCEViewColumn.LoadTextFont(Column: TEasyColumn; ACanvas: TCanvas);
var
  state: TSpTBXSkinStatesType;
begin
  inherited LoadTextFont(Column, ACanvas);

  if Column.PaintMouseHovering then
  state:= sknsHotTrack
  else if Column.Clicking then
  state:= sknsPushed
  else
  state:= sknsNormal;
  ACanvas.Font.Color:= SkinManager.CurrentSkin.GetTextColor(skncHeader, state);
end;

{*------------------------------------------------------------------------------
  Gets called when BkGnd is painted.
-------------------------------------------------------------------------------}
procedure TCEViewColumn.PaintBkGnd(Column: TEasyColumn;
  ACanvas: TCanvas; HeaderType: TEasyHeaderType;
  RectArray: TEasyRectArrayObject);
begin
  inherited;
  SpDrawXPHeader(ACanvas, RectArray.BoundsRect, Column.PaintMouseHovering, Column.Clicking, SkinManager.GetSkinType);
end;

end.
