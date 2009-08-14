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
//  The Original Code is fCE_FileSearch.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_FileSearch;

interface

uses
  // CE Units
  CE_FileSearch, CE_VistaFuncs, CE_AppSettings, CE_FileView,
  // CE Frames
  fCE_TabPage, fCE_FileSearchDestDlg,
  // SpTBXLib
  SpTBXFormPopupMenu, SpTBXEditors, SpTBXControls,
  // EasyListview
  EasyListview,
  // Virtual Trees
  VirtualTrees,
  // Tnt Controls
  TntSysUtils,
  // VSTools
  MPCommonObjects,  VirtualExplorerEasyListview, MPCommonUtilities,
  VirtualExplorerTree, MPShellUtilities,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ShlObj, ExtCtrls, SpTBXItem, CE_BaseFileView,
  fCE_TextEditor;

type
  TCEFileSearchSettings = class;

  TCEFileSearchPage = class(TCECustomTabPage)
    SearchPanel: TSpTBXPanel;
    label1: TSpTBXLabel;
    edit_dosmask: TSpTBXEdit;
    but_start: TSpTBXButton;
    label2: TSpTBXLabel;
    edit_wordphrase: TSpTBXEdit;
    but_pause: TSpTBXButton;
    but_stop: TSpTBXButton;
    label3: TSpTBXLabel;
    check_subdir: TSpTBXCheckBox;
    DestinationEdit: TSpTBXButtonEdit;
    StatusPanel: TSpTBXPanel;
    StatusLabel: TSpTBXLabel;
    FolderTreePopup: TSpTBXFormPopupMenu;
    UpdateTimer: TTimer;
    procedure but_startClick(Sender: TObject);
    procedure but_pauseClick(Sender: TObject);
    procedure but_stopClick(Sender: TObject);
    procedure FolderTreePopupPopup(Sender: TObject);
    procedure FolderTreePopupClosePopup(Sender: TObject; Selected: Boolean);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    fDownShiftState: TShiftState;
    fShowItemContextMenu: Boolean;
    { Private declarations }
  protected
    function GetSettingsClass: TCECustomTabPageSettingsClass; override;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
    procedure OnItemContextMenu(Sender: TCustomEasyListview; HitInfo:
        TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu; var Handled:
        Boolean);
  public
    Results: TCEFileSearchView;
    DestDlg: TCEDestDlg;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ColumnSizeChanged(Sender: TCustomEasyListview; Column: TEasyColumn);
    procedure ItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure OnDirectoryChange(Sender: TObject; NewDirectory: WideString);
    procedure OnFileMatch(Sender: TObject; Directory: WideString; FileName:
        WideString);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer);
    procedure OnSearchBegin(Sender: TObject);
    procedure OnSearchFinished(Sender: TObject);
    procedure SelectPage; override;
    procedure StartSearch;
    procedure UpdateCaption; override;
    { Public declarations }
  published
  end;

  TCEFileSearchSettings = class(TPersistent)
  private
    fRememberPanelLayout: Boolean;
    fRememberInnerToolbarLayout: Boolean;
    fRememberOuterToolbarLayout: Boolean;
    fShowExtensions: Boolean;
    fSubFolders: Boolean;
    function GetColumns: string;
    procedure SetColumns(const Value: string);
  protected
  public
    ColumnSettings: TCEColSettings;
    constructor Create;
    procedure AssignColumnSettingsFrom(FileView: TCECustomFileView);
    procedure AssignColumnSettingsTo(FileView: TCECustomFileView);
    procedure AssignSettingsFrom(FileSearch: TCEFileSearchPage);
    procedure AssignSettingsTo(FileSearch: TCEFileSearchPage);
  published
    property Columns: string read GetColumns write SetColumns;
    property RememberPanelLayout: Boolean read fRememberPanelLayout write
        fRememberPanelLayout;
    property RememberInnerToolbarLayout: Boolean read fRememberInnerToolbarLayout write
        fRememberInnerToolbarLayout;
    property RememberOuterToolbarLayout: Boolean read fRememberOuterToolbarLayout write
        fRememberOuterToolbarLayout;
    property ShowExtensions: Boolean read fShowExtensions write fShowExtensions;
    property SubFolders: Boolean read fSubFolders write fSubFolders;
  end;

type
  TCEFileSearchPageSettings = class(TCECustomTabPageSettings)
  private
    function GetPath: WideString;
    function GetSubFolders: Boolean;
    procedure SetPath(const Value: WideString);
    procedure SetSubFolders(const Value: Boolean);
  protected
    function GetRememberPanelLayout: Boolean; override;
    function GetRememberInnerToolbarLayout: Boolean; override;
    function GetRememberOuterToolbarLayout: Boolean; override;
  public
    FileSearchPage: TCEFileSearchPage;
  published
    property Path: WideString read GetPath write SetPath;
    property SubFolders: Boolean read GetSubFolders write SetSubFolders;
  end;

var
  CEFileSearch: TCEFileSearchPage;
  CEFileSearchSettings: TCEFileSearchSettings;

implementation

uses
  dCE_Images, CE_GlobalCtrl, dCE_Actions, Main;
{$R *.dfm}

{*------------------------------------------------------------------------------
  Create an instance of TCEFileSearchPage
-------------------------------------------------------------------------------}
constructor TCEFileSearchPage.Create(AOwner: TComponent);
begin
  inherited;
  TCEFileSearchPageSettings(Settings).FileSearchPage:= Self;
  Layout:= 'FileView';
  Results:= TCEFileSearchView.Create(self);
  Results.Parent:= self;
  SetDesktopIconFonts(Results.Font);
  Results.Align:= alClient;
  Results.FindFile.OnSearchBegin:= OnSearchBegin;
  Results.FindFile.OnSearchFinished:= OnSearchFinished;
  Results.FindFile.OnDirectoryChange:= OnDirectoryChange;
  Results.OnItemSelectionsChanged:= ItemSelectionsChanged;
  Results.FullSizeColumn:= Results.PathIndex;
  Results.OnMouseDown:= OnMouseDown;
  Results.OnMouseUp:= OnMouseUp;
  Results.OnItemContextMenu:= OnItemContextMenu;
  Results.OnColumnSizeChanged:= ColumnSizeChanged;

  DestDlg:= TCEDestDlg.Create(self);
  FolderTreePopup.PopupForm:= DestDlg;

  CEFileSearchSettings.AssignSettingsTo(Self);
end;

{*------------------------------------------------------------------------------
  Destroy TCEFileSearchPage
-------------------------------------------------------------------------------}
destructor TCEFileSearchPage.Destroy;
begin
  CEFileSearchSettings.AssignSettingsFrom(Self);
  inherited;
end;

{*------------------------------------------------------------------------------
  On Close Popup
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.FolderTreePopupClosePopup(Sender: TObject;
  Selected: Boolean);
begin
  DestinationEdit.Text:= DestDlg.GetCheckedFolders(check_subdir.Checked);
end;

{*------------------------------------------------------------------------------
  On Popup
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.FolderTreePopupPopup(Sender: TObject);
var
  ao: TVTAutoOptions;
begin
  if check_subdir.Checked <> DestDlg.TriState then
  begin
    ao:= DestDlg.FolderTree.TreeOptions.AutoOptions;
    if check_subdir.Checked then
    Include(ao, toAutoTristateTracking)
    else
    Exclude(ao, toAutoTristateTracking);
    DestDlg.FolderTree.TreeOptions.AutoOptions:= ao;
    DestDlg.TriState:= check_subdir.Checked;
    DestDlg.FolderTree.RebuildTree;
  end;
  DestDlg.SetCheckedFolders(DestinationEdit.Text);
end;

{*------------------------------------------------------------------------------
  but_pauseClick
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.but_pauseClick(Sender: TObject);
begin
  Results.FindFile.Pause;
  but_start.Enabled:= true;
  but_pause.Enabled:= false;
  StatusLabel.Caption:= 'Paused';
end;

{*------------------------------------------------------------------------------
  but_startClick
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.but_startClick(Sender: TObject);
begin
  if Results.FindFile.IsPaused then
  Results.FindFile.Start
  else
  StartSearch;
  but_start.Enabled:= false;
  but_pause.Enabled:= true;
end;

{*------------------------------------------------------------------------------
  but_stopClick
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.but_stopClick(Sender: TObject);
begin
  Results.FindFile.Abort;
end;

{*------------------------------------------------------------------------------
  Save Column settings
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.ColumnSizeChanged(Sender: TCustomEasyListview;
    Column: TEasyColumn);
begin
  if GlobalPathCtrl.ActivePage = Self then
  CEFileSearchSettings.AssignColumnSettingsFrom(Results);
end;

{-------------------------------------------------------------------------------
  Get Settings Class
-------------------------------------------------------------------------------}
function TCEFileSearchPage.GetSettingsClass: TCECustomTabPageSettingsClass;
begin
  Result:= TCEFileSearchPageSettings;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  if WideDirectoryExists(NewPath) then
  DestinationEdit.Text:= NewPath;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
var
  ws: WideString;
begin
  ws:= PIDLtoPath(NewPIDL);
  if WideDirectoryExists(ws) then
  DestinationEdit.Text:= ws;
end;

{*------------------------------------------------------------------------------
  Get's called on when item selection has changed
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.ItemSelectionsChanged(Sender: TCustomEasyListview);
var
  NS: TNamespace;
  Item: TEasyItem;
begin
  if Results.Selection.Count > 1 then
  Item:= Results.Selection.FocusedItem
  else
  Item:= Results.Selection.First;
  
  if Assigned(Item) then
  begin
    Results.ValidateNamespace(Item, NS);
    if assigned(NS) then
    GlobalPathCtrl.ChangeFocusedPath(Self, NS.NameForParsing);
  end
  else
  GlobalPathCtrl.ChangeFocusedPath(Self, '');
end;

{*------------------------------------------------------------------------------
  Get's called on search directory change
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.OnDirectoryChange(Sender: TObject; NewDirectory:
    WideString);
begin
  StatusLabel.Caption:= NewDirectory;
end;

{*------------------------------------------------------------------------------
  Get's called on search file match
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.OnFileMatch(Sender: TObject; Directory: WideString;
    FileName: WideString);
var
  group: TEasyGroup;
  item: TExplorerItem;
begin
  if Results.Groups.Count = 0 then
  begin
    group:= Results.Groups.Add;
    //group.Caption:= Directory;
  end
  else
  begin
    group:= Results.Groups[Results.Groups.Count-1];
  end;

  Results.BeginUpdate;
  try
    item:= group.Items.InsertCustom(group.Items.Count,TExplorerItem) as TExplorerItem;
    item.Namespace:= TNamespace.CreateFromFileName(Directory + FileName);
  finally
    Results.EndUpdate(false);
  end;  
end;

{*------------------------------------------------------------------------------
  Get's called on search begin
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.OnSearchBegin(Sender: TObject);
begin
  UpdateTimer.Enabled:= true;
  StatusLabel.Caption:= 'Searching...';
  but_stop.Enabled:= true;
  but_pause.Enabled:= true;
  but_start.Enabled:= false;
end;

{*------------------------------------------------------------------------------
  Get's called on search finished
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.OnSearchFinished(Sender: TObject);
var
  S: String;
begin
  but_pause.Enabled:= false;
  but_stop.Enabled:= false;
  but_start.Enabled:= true;
  S:= 'Done. ' + 'Found ' + IntToStr(Results.FindFile.ResultCount) + ' files in ' + IntToStr(Results.FindFile.SearchTime) + 'ms.';
  StatusLabel.Caption:= s;
  UpdateTimer.Enabled:= false;
  GlobalPathCtrl.ChangeGlobalContent(Self);
end;

{*------------------------------------------------------------------------------
  Select Page
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.SelectPage;
begin
  GlobalPathCtrl.ActivePage:= Self;
  CEFileSearchSettings.AssignColumnSettingsTo(Results);
end;

{*------------------------------------------------------------------------------
  Initialize and start search
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.StartSearch;
begin
  Results.Groups.Clear(true);
  if (edit_wordphrase.Text <> '') and (edit_dosmask.Text = '') then
  Results.FindFile.FileCriteria.DOSMask:= '*.*'
  else
  Results.FindFile.FileCriteria.DOSMask:= edit_dosmask.Text;
  Results.FindFile.FileCriteria.ContainsText:= edit_wordphrase.Text;


  Results.FindFile.SubDirectories:= check_subdir.Checked;
  Results.FindFile.Directories.DelimitedText:= DestinationEdit.Text;
  Results.FindFile.Start;
end;

{*------------------------------------------------------------------------------
  Update Tab item Caption
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.UpdateCaption;
begin
  if assigned(TabItem) then
  begin
    TabItem.Images:= CE_Images.SmallIcons;
    TabItem.ImageIndex:= 22;
  end;
  TabCaption:= 'File Search';
end;

procedure TCEFileSearchPage.OnItemContextMenu(Sender: TCustomEasyListview;
    HitInfo: TEasyHitInfoItem; WindowPoint: TPoint; var Menu: TPopupMenu; var
    Handled: Boolean);
begin
  if not Handled then
  Handled:= not fShowItemContextMenu;
end;

{*------------------------------------------------------------------------------
  Get's called on item mouse down.
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.OnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  fDownShiftState:= Shift;
  fShowItemContextMenu:= not (Shift = [ssRight, ssAlt]);
end;

{*------------------------------------------------------------------------------
  Get's called on item mouse up.
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.OnMouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  NS: TNamespace;
  item: TEasyItem;
  WindowPt: TPoint;
begin
  if (ssMiddle in fDownShiftState) or ((ssLeft in fDownShiftState) and (ssAlt in Shift)) then
  begin
    WindowPt := Results.Scrollbars.MapWindowToView(Point(X,Y));
    item:= Results.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not Results.EditManager.Editing then
    begin
      Results.EditManager.EndEdit;
      Results.ValidateNamespace(Item,NS);
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
    WindowPt := Results.Scrollbars.MapWindowToView(Point(X,Y));
    item:= Results.Groups.ItemByPoint(WindowPt);
    if assigned(item) and not Results.EditManager.Editing then
    begin
      Results.ValidateNamespace(Item,NS);
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
  Update global content timer
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.UpdateTimerTimer(Sender: TObject);
begin
  GlobalPathCtrl.ChangeGlobalContent(Self);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEFileSearchSettings
-------------------------------------------------------------------------------}
constructor TCEFileSearchSettings.Create;
begin
  inherited;
  fShowExtensions:= true;
  fSubFolders:= true;
  fRememberInnerToolbarLayout:= true;
end;

{-------------------------------------------------------------------------------
  Assign ColumnSettings From
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignColumnSettingsFrom(FileView:
    TCECustomFileView);
var
  col: TEasyColumn;
  i,c: Integer;
begin
  if not assigned(FileView) then
  Exit;

  c:= 0;
  for i:= 0 to FileView.Header.Columns.Count - 1 do
  begin
    if FileView.Header.Columns.Columns[i].Visible then
    Inc(c,1);
  end;

  if Length(ColumnSettings) <> c then
  SetLength(ColumnSettings, c);
  
  i:= 0;
  col:= FileView.Header.FirstVisibleColumn;
  while assigned(col) do
  begin
    ColumnSettings[i].Index:= col.Index;
    ColumnSettings[i].Position:= col.Position;
    ColumnSettings[i].Width:= col.Width;
    ColumnSettings[i].Sort:= col.SortDirection;
    col:= FileView.Header.NextVisibleColumn(col);
    inc(i);
  end;
end;

{-------------------------------------------------------------------------------
  Assign ColumnSettings To
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignColumnSettingsTo(FileView:
    TCECustomFileView);
var
  i: Integer;
  col: TEasyColumn;
begin
  if not assigned(FileView) or (Length(ColumnSettings) = 0) then
  Exit;

  FileView.Header.Columns.BeginUpdate(false);
  try
    for i:= 0 to FileView.Header.Columns.Count - 1 do
    begin
      if FileView.Header.Columns.Columns[i].Visible then
      FileView.Header.Columns.Columns[i].Visible:= false;
    end;

    for i:= 0 to Length(ColumnSettings) - 1 do
    begin
      if ColumnSettings[i].Index < FileView.Header.Columns.Count then
      begin
        col:= FileView.Header.Columns.Columns[ColumnSettings[i].Index];
        col.Visible:= true;
        col.Position:= ColumnSettings[i].Position;
        col.Width:= ColumnSettings[i].Width;
        col.SortDirection:= ColumnSettings[i].Sort;
      end;
    end;
  finally
    FileView.Header.Columns.EndUpdate(true);
  end;
end;

{-------------------------------------------------------------------------------
  Assign Settings From
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignSettingsFrom(FileSearch:
    TCEFileSearchPage);
begin
  if not assigned(FileSearch) then
  Exit;

  fSubFolders:= FileSearch.check_subdir.Checked;
  AssignColumnSettingsFrom(FileSearch.Results);
end;

{-------------------------------------------------------------------------------
  Assign Settings To
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignSettingsTo(FileSearch: TCEFileSearchPage);
begin
  if not assigned(FileSearch) then
  Exit;

  FileSearch.check_subdir.Checked:= fSubFolders;
  FileSearch.Results.ShowExtension:= fShowExtensions;
  AssignColumnSettingsTo(FileSearch.Results);
end;

{-------------------------------------------------------------------------------
  Get/Set Columns
-------------------------------------------------------------------------------}
function TCEFileSearchSettings.GetColumns: string;
begin
  Result:= ColSettingsToString(ColumnSettings);
end;

procedure TCEFileSearchSettings.SetColumns(const Value: string);
begin
  StringToColSettings(Value, ColumnSettings);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set Path
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetPath: WideString;
begin
  Result:= FileSearchPage.DestinationEdit.Text;
end;

{-------------------------------------------------------------------------------
  Get RememberPanelLayout
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetRememberPanelLayout: Boolean;
begin
  Result:= CEFileSearchSettings.RememberPanelLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberInnerToolbarLayout
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetRememberInnerToolbarLayout: Boolean;
begin
  Result:= CEFileSearchSettings.RememberInnerToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberOuterToolbarLayout
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetRememberOuterToolbarLayout: Boolean;
begin
  Result:= CEFileSearchSettings.RememberOuterToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Set Path
-------------------------------------------------------------------------------}
procedure TCEFileSearchPageSettings.SetPath(const Value: WideString);
begin
  FileSearchPage.DestinationEdit.Text:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set SubFolder
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetSubFolders: Boolean;
begin
  Result:= FileSearchPage.check_subdir.Checked;
end;
procedure TCEFileSearchPageSettings.SetSubFolders(const Value: Boolean);
begin
  FileSearchPage.check_subdir.Checked:= Value;
end;

{##############################################################################}

initialization
  CEFileSearchSettings:= TCEFileSearchSettings.Create;
  GlobalAppSettings.AddItem('FileSearch', CEFileSearchSettings, true);
  TabPageClassList.RegisterClass('FileSearch', TCEFileSearchPage, TCEFileSearchPageSettings);

finalization
  FreeAndNil(CEFileSearchSettings);
  
end.
