unit fCE_FileSearch;

interface

uses
  // CE Units
  CE_FileSearch, CE_VistaFuncs, CE_SettingsIntf, CE_Settings,
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
  Dialogs, Menus, StdCtrls, ShlObj, ExtCtrls, SpTBXItem;

type
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
    { Private declarations }
  protected
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
  public
    Results: TCEFileSearchView;
    DestDlg: TCEDestDlg;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure LoadFromStorage(Storage: ICESettingsStorage); override; stdcall;
    procedure OnDirectoryChange(Sender: TObject; NewDirectory: WideString);
    procedure OnFileMatch(Sender: TObject; Directory: WideString; FileName:
        WideString);
    procedure OnSearchBegin(Sender: TObject);
    procedure OnSearchFinished(Sender: TObject);
    procedure SaveToStorage(Storage: ICESettingsStorage); override; stdcall;
    procedure SelectPage; override;
    procedure StartSearch;
    procedure UpdateCaption; override;
    { Public declarations }
  end;

var
  CEFileSearch: TCEFileSearchPage;

implementation

uses
  dCE_Images, CE_GlobalCtrl;
{$R *.dfm}

{*------------------------------------------------------------------------------
  Create an instance of TCEFileSearchPage
-------------------------------------------------------------------------------}
constructor TCEFileSearchPage.Create(AOwner: TComponent);
begin
  inherited;
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

  DestDlg:= TCEDestDlg.Create(self);
  FolderTreePopup.PopupForm:= DestDlg;

  GlobalSettings.WriteSettingTo(Self);
end;

{*------------------------------------------------------------------------------
  Destroy TCEFileSearchPage
-------------------------------------------------------------------------------}
destructor TCEFileSearchPage.Destroy;
begin
  GlobalSettings.ReadSettingsFrom(Self);
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

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.LoadFromStorage(Storage: ICESettingsStorage);

  procedure LoadColumns;
  var
    i,index: Integer;
    col: TEasyColumn;
    colListSet, colList: TStrings;
    s: String;
  begin
    s:= Storage.ReadString('Columns','');
    if s = '' then
    Exit;
    colListSet:= TStringList.Create;
    colList:= TStringList.Create;
    try
      colListSet.Delimiter:= '|';
      colList.Delimiter:= ',';
      colListSet.DelimitedText:= s;
      Results.Header.Columns.BeginUpdate(false);
      try
        for i:= 0 to Results.Header.Columns.Count - 1 do
        begin
          if Results.Header.Columns.Columns[i].Visible then
          Results.Header.Columns.Columns[i].Visible:= false;
        end;

        for i:= 0 to colListSet.Count - 1 do
        begin
          colList.DelimitedText:= colListSet.Strings[i];
          if colList.Count >= 3 then
          begin
            index:= StrToIntDef(colList.Strings[0], -1);
            if (index > -1) and (index < Results.Header.Columns.Count) then
            begin
              col:= Results.Header.Columns.Columns[index];
              col.Visible:= true;
              col.Position:= StrToIntDef(colList.Strings[1],0);
              col.Width:= StrToIntDef(colList.Strings[2],50);
              if colList.Count = 4 then
              col.SortDirection:= TEasySortDirection(StrToIntDef(colList.Strings[3],0));
            end;
          end;
        end;
      finally
        Results.Header.Columns.EndUpdate(true);
      end;
    finally
      colListSet.Free;
      colList.Free;
    end;
  end;

begin
  Storage.OpenPath('/FileSearch');
  try
    // Columns
    LoadColumns;
    // Toggles
    check_subdir.Checked:= Storage.ReadBoolean('SubFolders', true);
    Results.ShowExtension:= Storage.ReadBoolean('ShowExtensions', true);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Save to storage
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.SaveToStorage(Storage: ICESettingsStorage);

  procedure SaveColumns;
  var
    col: TEasyColumn;
    s: String;
  begin
    s:= '';
    col:= Results.Header.FirstVisibleColumn;
    while assigned(col) do
    begin
      s:= s + IntToStr(col.Index) + ',' +
              IntToStr(col.Position) + ',' +
              IntToStr(col.Width) + ',' +
              IntToStr(Ord(col.SortDirection));
      col:= Results.Header.NextVisibleColumn(col);
      if col <> nil then
      s:= s + '|';
    end;
    Storage.WriteString('Columns', s);
  end;

begin
  Storage.OpenPath('/FileSearch');
  try
    // Columns
    SaveColumns;
    // Toggles
    Storage.WriteBoolean('SubFolders', check_subdir.Checked);
    Storage.WriteBoolean('ShowExtensions', Results.ShowExtension);
  finally
    Storage.ClosePath;
  end;
end;

{*------------------------------------------------------------------------------
  Update global content timer
-------------------------------------------------------------------------------}
procedure TCEFileSearchPage.UpdateTimerTimer(Sender: TObject);
begin
  GlobalPathCtrl.ChangeGlobalContent(Self);
end;

{##############################################################################}

initialization
  TabPageClassList.RegisterClass('FileSearch', TCEFileSearchPage);

finalization

end.
