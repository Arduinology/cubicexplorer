unit fCE_SearchPage;

interface

uses
  // CE Units
  CE_VistaFuncs, CE_AppSettings, dCE_Images, CE_GlobalCtrl, CE_BaseFileView,
  CE_FileView,
  // CE Frames
  fCE_TabPage, fCE_FileSearchDestDlg,
  // VSTools
  MPCommonObjects, EasyListview, VirtualExplorerEasyListview, VirtualTrees,
  VirtualExplorerTree, MPShellUtilities,
  // SpTBX
  SpTBXItem, SpTBXTabs, TB2Item, SpTBXSkins, SpTBXEditors, SpTBXControls,
  // FindFile
  FindFileW,
  // Tnt
  TntStdCtrls, TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ShlObj, Menus, SpTBXFormPopupMenu;

type
  TCustomVirtualExplorerEasyListviewHack = class(TCustomVirtualExplorerEasyListview);

  TCESearchPage = class(TCECustomTabPage)
    ResultView: TVirtualMultiPathExplorerEasyListview;
    SearchPanel: TPanel;
    group_searchbuttons: TSpTBXGroupBox;
    but_search_start: TSpTBXButton;
    but_search_stop: TSpTBXButton;
    check_clear_before: TSpTBXCheckBox;
    CriteriaTabControl: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    SpTBXTabItem2: TSpTBXTabItem;
    SpTBXTabItem3: TSpTBXTabItem;
    SpTBXTabItem4: TSpTBXTabItem;
    SpTBXTabItem8: TSpTBXTabItem;
    sheet_size_attributes: TSpTBXTabSheet;
    group_size: TSpTBXGroupBox;
    spin_size_atleast: TSpTBXSpinEdit;
    combo_size_atleast: TSpTBXComboBox;
    check_size_atleast: TSpTBXCheckBox;
    spin_size_atmost: TSpTBXSpinEdit;
    combo_size_atmost: TSpTBXComboBox;
    check_size_atmost: TSpTBXCheckBox;
    group_attributes: TSpTBXGroupBox;
    check_attr_readonly: TSpTBXCheckBox;
    check_attr_hidden: TSpTBXCheckBox;
    check_attr_compressed: TSpTBXCheckBox;
    check_attr_system: TSpTBXCheckBox;
    sheet_content: TSpTBXTabSheet;
    SpTBXLabel4: TSpTBXLabel;
    memo_content: TTntMemo;
    check_content_wordwrap: TSpTBXCheckBox;
    check_content_case_sensitive: TSpTBXCheckBox;
    check_content_wholeword: TSpTBXCheckBox;
    SpTBXTabSheet1: TSpTBXTabSheet;
    SpTBXLabel1: TSpTBXLabel;
    memo_filters_exclude: TTntMemo;
    memo_filters_include: TTntMemo;
    SpTBXLabel3: TSpTBXLabel;
    SpTBXLabel5: TSpTBXLabel;
    sheet_date_time: TSpTBXTabSheet;
    TabControl_DateTime: TSpTBXTabControl;
    SpTBXTabItem5: TSpTBXTabItem;
    SpTBXTabItem6: TSpTBXTabItem;
    SpTBXTabItem7: TSpTBXTabItem;
    sheet_modified: TSpTBXTabSheet;
    ModifiedBeforeTime: TDateTimePicker;
    ModifiedBeforeDate: TDateTimePicker;
    ModifiedAfterDate: TDateTimePicker;
    ModifiedAfterTime: TDateTimePicker;
    check_ModifiedBeforeDate: TSpTBXCheckBox;
    check_ModifiedBeforeTime: TSpTBXCheckBox;
    check_ModifiedAfterDate: TSpTBXCheckBox;
    check_ModifiedAfterTime: TSpTBXCheckBox;
    sheet_accessed: TSpTBXTabSheet;
    AccessedBeforeTime: TDateTimePicker;
    AccessedBeforeDate: TDateTimePicker;
    AccessedAfterDate: TDateTimePicker;
    AccessedAfterTime: TDateTimePicker;
    check_AccessedBeforeDate: TSpTBXCheckBox;
    check_AccessedBeforeTime: TSpTBXCheckBox;
    check_AccessedAfterDate: TSpTBXCheckBox;
    check_AccessedAfterTime: TSpTBXCheckBox;
    sheet_created: TSpTBXTabSheet;
    CreatedBeforeTime: TDateTimePicker;
    CreatedBeforeDate: TDateTimePicker;
    CreatedAfterDate: TDateTimePicker;
    CreatedAfterTime: TDateTimePicker;
    check_CreatedBeforeDate: TSpTBXCheckBox;
    check_CreatedBeforeTime: TSpTBXCheckBox;
    check_CreatedAfterDate: TSpTBXCheckBox;
    check_CreatedAfterTime: TSpTBXCheckBox;
    sheet_name_location: TSpTBXTabSheet;
    edit_filemask: TSpTBXEdit;
    SpTBXLabel2: TSpTBXLabel;
    check_subfolders: TSpTBXCheckBox;
    spin_minlevel: TSpTBXSpinEdit;
    check_minlevel: TSpTBXCheckBox;
    check_maxlevel: TSpTBXCheckBox;
    spin_maxlevel: TSpTBXSpinEdit;
    edit_wordphrase: TSpTBXEdit;
    radio_name_word: TSpTBXRadioButton;
    combo_extension: TSpTBXComboBox;
    radio_name_mask: TSpTBXRadioButton;
    panel_status: TSpTBXPanel;
    label_status: TSpTBXLabel;
    FolderTreePopup: TSpTBXFormPopupMenu;
    edit_location: TSpTBXButtonEdit;
    procedure but_search_startClick(Sender: TObject);
    procedure but_search_stopClick(Sender: TObject);
    procedure check_content_wordwrapClick(Sender: TObject);
    procedure FolderTreePopupClosePopup(Sender: TObject; Selected: Boolean);
    procedure FolderTreePopupPopup(Sender: TObject);
  private
    fFileCount: Integer;
    fFolderCount: Integer;
    fStartTime: Integer;
    function GetAttributeStatus(CB: TSpTBXCheckBox): TFileAttributeStatus;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
    { Private declarations }
  protected
    function GetSettingsClass: TCECustomTabPageSettingsClass; override;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
    procedure UpdateTheme;
  public
    DestDlg: TCEDestDlg;
    Find: TFindFileW;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleFileMatch(Sender: TObject; const FileInfo: TFileDetails);
    procedure HandleFolderChange(Sender: TObject; const Folder: WideString; var
        IgnoreFolder: TFolderIgnore);
    procedure HandleSearchAbort(Sender: TObject);
    procedure HandleSearchBegin(Sender: TObject);
    procedure HandleSearchFinish(Sender: TObject);
    procedure SelectPage; override;
    procedure StartSearch;
    procedure UpdateCaption; override;
    { Public declarations }
  end;

type
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
    procedure AssignColumnSettingsFrom(FileView:
        TCustomVirtualExplorerEasyListview);
    procedure AssignColumnSettingsTo(FileView: TCustomVirtualExplorerEasyListview);
    procedure AssignSettingsFrom(FileSearch: TCESearchPage);
    procedure AssignSettingsTo(FileSearch: TCESearchPage);
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
    FileSearchPage: TCESearchPage;
  published
    property Path: WideString read GetPath write SetPath;
    property SubFolders: Boolean read GetSubFolders write SetSubFolders;
  end;

var
  CEFileSearchSettings: TCEFileSearchSettings;  

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCESearchPage
-------------------------------------------------------------------------------}
constructor TCESearchPage.Create(AOwner: TComponent);
begin
  inherited;
  TCEFileSearchPageSettings(Settings).FileSearchPage:= Self;
  Layout:= 'FileSearch';
  Find:= TFindFileW.Create(nil);
  Find.Threaded:= true;
  Find.OnFileMatch:= HandleFileMatch;
  Find.OnFolderChange:= HandleFolderChange;
  Find.OnSearchBegin:= HandleSearchBegin;
  Find.OnSearchFinish:= HandleSearchFinish;
  Find.OnSearchAbort:= HandleSearchAbort;

  DestDlg:= TCEDestDlg.Create(self);
  FolderTreePopup.PopupForm:= DestDlg;
  
  ResultView.Active:= true;

  SkinManager.AddSkinNotification(Self);

  CreatedBeforeDate.Date:= Date;
  CreatedBeforeTime.Time:= Now;
  CreatedAfterDate.Date:= Date;
  CreatedAfterTime.Time:= Now;
  CEFileSearchSettings.AssignSettingsTo(Self);
  UpdateTheme;
end;

{-------------------------------------------------------------------------------
  Destroy TCESearchPage
-------------------------------------------------------------------------------}
destructor TCESearchPage.Destroy;
begin
  CEFileSearchSettings.AssignSettingsFrom(Self);
  Find.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  but_search_start Click
-------------------------------------------------------------------------------}
procedure TCESearchPage.but_search_startClick(Sender: TObject);
begin
  StartSearch;
end;

{-------------------------------------------------------------------------------
  but_search_stop Click
-------------------------------------------------------------------------------}
procedure TCESearchPage.but_search_stopClick(Sender: TObject);
begin
  Find.Abort;
end;

{-------------------------------------------------------------------------------
  check_content_wordwrap Click
-------------------------------------------------------------------------------}
procedure TCESearchPage.check_content_wordwrapClick(Sender: TObject);
begin
  memo_content.WordWrap:= check_content_wordwrap.Checked;
  if memo_content.WordWrap then
  memo_content.ScrollBars:= ssVertical
  else
  memo_content.ScrollBars:= ssBoth;
end;

{*------------------------------------------------------------------------------
  On Close Popup
-------------------------------------------------------------------------------}
procedure TCESearchPage.FolderTreePopupClosePopup(Sender: TObject; Selected:
    Boolean);
begin
  edit_location.Text:= DestDlg.GetCheckedFolders(check_subfolders.Checked);
end;

{*------------------------------------------------------------------------------
  On Popup
-------------------------------------------------------------------------------}
procedure TCESearchPage.FolderTreePopupPopup(Sender: TObject);
var
  ao: TVTAutoOptions;
begin
  if check_subfolders.Checked <> DestDlg.TriState then
  begin
    ao:= DestDlg.FolderTree.TreeOptions.AutoOptions;
    if check_subfolders.Checked then
    Include(ao, toAutoTristateTracking)
    else
    Exclude(ao, toAutoTristateTracking);
    DestDlg.FolderTree.TreeOptions.AutoOptions:= ao;
    DestDlg.TriState:= check_subfolders.Checked;
    DestDlg.FolderTree.RebuildTree;
  end;
  DestDlg.SetCheckedFolders(edit_location.Text);
end;

{-------------------------------------------------------------------------------
  GetAttributeStatus
-------------------------------------------------------------------------------}
function TCESearchPage.GetAttributeStatus(CB: TSpTBXCheckBox):
    TFileAttributeStatus;
begin
  case CB.State of
    cbUnchecked: Result:= fsUnset;
    cbChecked: Result:= fsSet;
  else
    Result:= fsIgnore;
  end;
end;

{-------------------------------------------------------------------------------
  Get Settings Class
-------------------------------------------------------------------------------}
function TCESearchPage.GetSettingsClass: TCECustomTabPageSettingsClass;
begin
  Result:= TCEFileSearchPageSettings;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCESearchPage.GlobalPathChanged(Sender: TObject; NewPath: WideString);
begin
  if WideDirectoryExists(NewPath) then
  edit_location.Text:= NewPath;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCESearchPage.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
var
  ws: WideString;
begin
  ws:= PIDLtoPath(NewPIDL);
  if WideDirectoryExists(ws) then
  edit_location.Text:= ws;
end;

{-------------------------------------------------------------------------------
  HandleFileMatch
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleFileMatch(Sender: TObject; const FileInfo:
    TFileDetails);
var
  NS: TNamespace;
begin
  try
    NS:= TNamespace.CreateFromFileName(FileInfo.Location + FileInfo.Name);
    ResultView.AddCustomItem(nil, NS, true);
    fFileCount:= fFileCount + 1;
  except
  end;
end;

{-------------------------------------------------------------------------------
  HandleFolderChange
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleFolderChange(Sender: TObject; const Folder:
    WideString; var IgnoreFolder: TFolderIgnore);
begin
  label_status.Caption:= 'Searching from: ' + Folder;
  fFolderCount:= fFolderCount + 1;
end;

{-------------------------------------------------------------------------------
  HandleSearchAbort
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleSearchAbort(Sender: TObject);
begin
  //
end;

{-------------------------------------------------------------------------------
  HandleSearchBegin
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleSearchBegin(Sender: TObject);
begin
  but_search_start.Enabled:= false;
  but_search_stop.Enabled:= true;
  fFolderCount:= 0;
  fFileCount:= 0;
  fStartTime:= GetTickCount;
end;

{-------------------------------------------------------------------------------
  HandleSearchFinish
-------------------------------------------------------------------------------}
procedure TCESearchPage.HandleSearchFinish(Sender: TObject);
var
  ws: WideString;
begin
  but_search_start.Enabled:= true;
  but_search_stop.Enabled:= false;

  if Find.Aborted then
  ws:= 'Cancelled'
  else
  ws:= 'Finished';

  label_status.Caption:= ws + ' - ' + WideFormat('%d folder(s) searched and %d file(s) found in %.3f second(s)',
                                             [fFolderCount, fFileCount, (GetTickCount - fStartTime) / 1000]);

  GlobalPathCtrl.ChangeGlobalContent(Self);
end;

{*------------------------------------------------------------------------------
  Select Page
-------------------------------------------------------------------------------}
procedure TCESearchPage.SelectPage;
begin
  GlobalPathCtrl.ActivePage:= Self;
  CEFileSearchSettings.AssignColumnSettingsTo(ResultView);
end;

{-------------------------------------------------------------------------------
  Start Search
-------------------------------------------------------------------------------}
procedure TCESearchPage.StartSearch;
var
  ext: WideString;
  i: Integer;
begin
  // Location
  Find.Criteria.Files.Location:= edit_location.Text;
  Find.Criteria.Files.Subfolders:= check_subfolders.Checked;
  if check_minlevel.Checked then
  Find.Criteria.Files.MinLevel:= StrToIntDef(spin_minlevel.Text, 0)
  else
  Find.Criteria.Files.MinLevel:= 0;
  if check_maxlevel.Checked then
  Find.Criteria.Files.MaxLevel:= StrToIntDef(spin_maxlevel.Text, 0)
  else
  Find.Criteria.Files.MaxLevel:= 0;
  // FileName
  if radio_name_word.Checked then
  begin
    if combo_extension.ItemIndex = 0 then
    ext:= '.*'
    else
    begin
      ext:= combo_extension.Text;
      if Length(ext) > 0 then
      begin
        if ext[1] <> '.' then
        ext:= '.' + ext;
      end;
    end;
    if edit_wordphrase.Text <> '' then
    Find.Criteria.Files.FileName:= '*' + edit_wordphrase.Text + '*' + ext
    else
    Find.Criteria.Files.FileName:= '*' + ext;
  end
  else
  Find.Criteria.Files.FileName:= edit_filemask.Text;
  // Date & Time
  Find.Criteria.TimeStamp.Clear;
    // Created on
    if Self.check_CreatedBeforeDate.Checked then
    Find.Criteria.TimeStamp.CreatedBefore:= Self.CreatedBeforeDate.Date;
    if Self.check_CreatedBeforeTime.Checked then
    Find.Criteria.TimeStamp.CreatedBefore:= Find.Criteria.TimeStamp.CreatedBefore + Self.CreatedBeforeTime.Time;
    if Self.check_CreatedAfterDate.Checked then
    Find.Criteria.TimeStamp.CreatedAfter:= Self.CreatedAfterDate.Date;
    if Self.check_CreatedAfterTime.Checked then
    Find.Criteria.TimeStamp.CreatedAfter:= Find.Criteria.TimeStamp.CreatedAfter + Self.CreatedAfterTime.Time;
    // Modified on
    if Self.check_ModifiedBeforeDate.Checked then
    Find.Criteria.TimeStamp.ModifiedBefore:= Self.ModifiedBeforeDate.Date;
    if Self.check_ModifiedBeforeTime.Checked then
    Find.Criteria.TimeStamp.ModifiedBefore:= Find.Criteria.TimeStamp.ModifiedBefore + Self.ModifiedBeforeTime.Time;
    if Self.check_ModifiedAfterDate.Checked then
    Find.Criteria.TimeStamp.ModifiedAfter:= Self.ModifiedAfterDate.Date;
    if Self.check_ModifiedAfterTime.Checked then
    Find.Criteria.TimeStamp.ModifiedAfter:= Find.Criteria.TimeStamp.ModifiedAfter + Self.ModifiedAfterTime.Time;
    // Accessed on
    if Self.check_AccessedBeforeDate.Checked then
    Find.Criteria.TimeStamp.AccessedBefore:= Self.AccessedBeforeDate.Date;
    if Self.check_AccessedBeforeTime.Checked then
    Find.Criteria.TimeStamp.AccessedBefore:= Find.Criteria.TimeStamp.AccessedBefore + Self.AccessedBeforeTime.Time;
    if Self.check_AccessedAfterDate.Checked then
    Find.Criteria.TimeStamp.AccessedAfter:= Self.AccessedAfterDate.Date;
    if Self.check_AccessedAfterTime.Checked then
    Find.Criteria.TimeStamp.AccessedAfter:= Find.Criteria.TimeStamp.AccessedAfter + Self.AccessedAfterTime.Time;
  // Size
  if check_size_atleast.Checked then
  begin
    Find.Criteria.Size.Min:= Self.spin_size_atleast.SpinOptions.ValueAsInteger;
    case Self.combo_size_atleast.ItemIndex of
      1: Find.Criteria.Size.Min:= Find.Criteria.Size.Min * 1024;
      2: Find.Criteria.Size.Min:= Find.Criteria.Size.Min * 1024 * 1024;
      3: Find.Criteria.Size.Min:= Find.Criteria.Size.Min * 1024 * 1024 * 1024;
    end;
  end
  else
  Find.Criteria.Size.Min:= 0;
  if check_size_atmost.Checked then
  begin
    Find.Criteria.Size.Max:= Self.spin_size_atmost.SpinOptions.ValueAsInteger;
    case Self.combo_size_atmost.ItemIndex of
      1: Find.Criteria.Size.Max:= Find.Criteria.Size.Max * 1024;
      2: Find.Criteria.Size.Max:= Find.Criteria.Size.Max * 1024 * 1024;
      3: Find.Criteria.Size.Max:= Find.Criteria.Size.Max * 1024 * 1024 * 1024;
    end;
  end
  else
  Find.Criteria.Size.Max:= 0;
  // Attributes
  Find.Criteria.Attributes.Readonly:= GetAttributeStatus(check_attr_readonly);
  Find.Criteria.Attributes.Hidden:= GetAttributeStatus(check_attr_hidden);
  Find.Criteria.Attributes.System:= GetAttributeStatus(check_attr_system);
  Find.Criteria.Attributes.Compressed:= GetAttributeStatus(check_attr_compressed);
  // Content
  Find.Criteria.Content.Phrase := Self.memo_content.Text;
  Find.Criteria.Content.Options := [];
  if check_content_case_sensitive.Checked then
  Find.Criteria.Content.Options:= [csoCaseSensitive];
  if check_content_wholeword.Checked then
  Find.Criteria.Content.Options:= [csoWholeWord];
  // Filters
  Find.Criteria.Files.Filters.Clear;
  for i:= 0 to memo_filters_exclude.Lines.Count - 1 do
  Find.Criteria.Files.Filters.Add('>' + memo_filters_exclude.Lines.Strings[i]);  
  for i:= 0 to memo_filters_exclude.Lines.Count - 1 do
  Find.Criteria.Files.Filters.Add(memo_filters_exclude.Lines.Strings[i]);
  // Clear previous results
  if check_clear_before.Checked then
  ResultView.Clear;
  // Execute Search
  Find.Execute;
end;

{*------------------------------------------------------------------------------
  Update Tab item Caption
-------------------------------------------------------------------------------}
procedure TCESearchPage.UpdateCaption;
begin
  if assigned(TabItem) then
  begin
    TabItem.Images:= CE_Images.SmallIcons;
    TabItem.ImageIndex:= 22;
  end;
  TabCaption:= 'File Search';
end;

{-------------------------------------------------------------------------------
  UpdateTheme
-------------------------------------------------------------------------------}
procedure TCESearchPage.UpdateTheme;
var
  c: TColor;
begin
  c:= SkinManager.CurrentSkin.Options(skncDock, sknsNormal).Body.Color1;
  if c = clNone then
  c:= clBtnFace;
  SearchPanel.Color:= c;
  CriteriaTabControl.TabBackgroundColor:= c;
end;

{-------------------------------------------------------------------------------
  WMSpSkinChange
-------------------------------------------------------------------------------}
procedure TCESearchPage.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  UpdateTheme;
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
    TCustomVirtualExplorerEasyListview);
var
  col: TEasyColumn;
  i,c: Integer;
  view: TCustomVirtualExplorerEasyListviewHack;
begin
  if not assigned(FileView) then
  Exit;

  view:= TCustomVirtualExplorerEasyListviewHack(FileView);

  c:= 0;
  for i:= 0 to view.Header.Columns.Count - 1 do
  begin
    if view.Header.Columns.Columns[i].Visible then
    Inc(c,1);
  end;

  if Length(ColumnSettings) <> c then
  SetLength(ColumnSettings, c);
  
  i:= 0;
  col:= view.Header.FirstVisibleColumn;
  while assigned(col) do
  begin
    ColumnSettings[i].Index:= col.Index;
    ColumnSettings[i].Position:= col.Position;
    ColumnSettings[i].Width:= col.Width;
    ColumnSettings[i].Sort:= col.SortDirection;
    col:= view.Header.NextVisibleColumn(col);
    inc(i);
  end;
end;

{-------------------------------------------------------------------------------
  Assign ColumnSettings To
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignColumnSettingsTo(FileView:
    TCustomVirtualExplorerEasyListview);
var
  i: Integer;
  col: TEasyColumn;
  view: TCustomVirtualExplorerEasyListviewHack;
begin
  if not assigned(FileView) or (Length(ColumnSettings) = 0) then
  Exit;
  view:= TCustomVirtualExplorerEasyListviewHack(FileView);

  view.Header.Columns.BeginUpdate(false);
  try
    for i:= 0 to view.Header.Columns.Count - 1 do
    begin
      if view.Header.Columns.Columns[i].Visible then
      view.Header.Columns.Columns[i].Visible:= false;
    end;

    for i:= 0 to Length(ColumnSettings) - 1 do
    begin
      if ColumnSettings[i].Index < view.Header.Columns.Count then
      begin
        col:= view.Header.Columns.Columns[ColumnSettings[i].Index];
        col.Visible:= true;
        col.Position:= ColumnSettings[i].Position;
        col.Width:= ColumnSettings[i].Width;
        col.SortDirection:= ColumnSettings[i].Sort;
      end;
    end;
  finally
    view.Header.Columns.EndUpdate(true);
  end;
end;

{-------------------------------------------------------------------------------
  Assign Settings From
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignSettingsFrom(FileSearch: TCESearchPage);
begin
  if not assigned(FileSearch) then
  Exit;

  fSubFolders:= FileSearch.check_subfolders.Checked;
  AssignColumnSettingsFrom(FileSearch.ResultView);
end;

{-------------------------------------------------------------------------------
  Assign Settings To
-------------------------------------------------------------------------------}
procedure TCEFileSearchSettings.AssignSettingsTo(FileSearch: TCESearchPage);
begin
  if not assigned(FileSearch) then
  Exit;

  FileSearch.check_subfolders.Checked:= fSubFolders;
  AssignColumnSettingsTo(FileSearch.ResultView);
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
  Result:= FileSearchPage.edit_location.Text;
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
  FileSearchPage.edit_location.Text:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set SubFolder
-------------------------------------------------------------------------------}
function TCEFileSearchPageSettings.GetSubFolders: Boolean;
begin
  Result:= FileSearchPage.check_subfolders.Checked;
end;

procedure TCEFileSearchPageSettings.SetSubFolders(const Value: Boolean);
begin
  FileSearchPage.check_subfolders.Checked:= Value;
end;

{##############################################################################}

initialization
  CEFileSearchSettings:= TCEFileSearchSettings.Create;
  GlobalAppSettings.AddItem('FileSearch', CEFileSearchSettings, true);
  TabPageClassList.RegisterClass('FileSearch', TCESearchPage, TCEFileSearchPageSettings);

finalization
  FreeAndNil(CEFileSearchSettings);

end.
