unit fCE_FolderPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_GlobalCtrl, dCE_Images, CE_VistaFuncs, CE_FolderTree,
  CE_SettingsIntf, CE_Settings,
  // VSTools
  VirtualTrees, VirtualExplorerTree, MPCommonUtilities, MPShellUtilities,
  VirtualShellNotifier,
  // PNG Controls
  PngImageList,
  // TB2K, SpTBX
  TB2Dock, SpTBXItem,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, ImgList;

type
  TControlHack = class(TControl);
  
  TCEFolderPanel = class(TCECustomDockableForm, ICESettingsHandler)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fNewTabByDefault: Boolean;
  protected
    procedure FolderTreeSelectedChange(Node: PVirtualNode);

    procedure FolderTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FolderTreeIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
    procedure FolderTreeKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
  public
    FolderTree: TCEFolderTree;
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure OnEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex);
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
  published
    property NewTabByDefault: Boolean read fNewTabByDefault write fNewTabByDefault;
  end;

var
  CEFolderPanel: TCEFolderPanel;

implementation

uses
  dCE_Actions, Main;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called when TCEFolderPanel is created.
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FormCreate(Sender: TObject);
begin
  inherited;
  FolderTree:= TCEFolderTree.Create(self);
  FolderTree.Parent:= Self;
  FolderTree.Align:= alClient;
  FolderTree.OnSelectedChange:= FolderTreeSelectedChange;
  FolderTree.OnIncrementalSearch:= FolderTreeIncrementalSearch;
  FolderTree.OnKeyAction:= FolderTreeKeyAction;
  FolderTree.OnMouseDown:= FolderTreeMouseDown;
  FolderTree.IncrementalSearch:= isVisibleOnly;
  FolderTree.Active:= true;
  FolderTree.OnEdited:= OnEdited;

  FolderTree.AutoCollapse:= true;
  FolderTree.AutoExpand:= true;
  FolderTree.Indent:= 24;
  FolderTree.TabOrder:= 1;
  
  SetDesktopIconFonts(FolderTree.Font);
  TopDock.Name:= 'FolderPanel_TopDock';
  BottomDock.Name:= 'FolderPanel_BottomDock';
  Caption:= 'Folders';
  GlobalPathCtrl.RegisterNotify(self);
  ImageList:= CE_Images.SmallIcons;
  ImageIndex:= 28;
  GlobalFocusCtrl.CtrlList.Add(FolderTree);
  TControlHack(FolderTree).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  GlobalSettings.RegisterHandler(self);

  fNewTabByDefault:= false;
end;

{-------------------------------------------------------------------------------
  On Destroy
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FormDestroy(Sender: TObject);
begin
  GlobalSettings.UnRegisterHandler(Self);
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called on focused node change
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FolderTreeSelectedChange(Node: PVirtualNode);
var
  NS: TNamespace;
begin
  if FolderTree.ValidateNamespace(Node, NS) then
  begin
    if NewTabByDefault then
    OpenFolderInTab(FolderTree, NS.AbsolutePIDL, MainForm.TabSet.OpenTabSelect)
    else
    GlobalPathCtrl.ChangeGlobalPathPIDL(Self, NS.AbsolutePIDL);
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  if FolderTree.AutoCollapse then
  FolderTree.FullCollapse;
  FolderTree.BrowseTo(NewPath, true, true, false, false);

  //if FolderTree.AutoCollapse then
  FolderTree.ScrollIntoView(FolderTree.FocusedNode,true,true);
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  if FolderTree.AutoCollapse then
  FolderTree.FullCollapse;
  FolderTree.BrowseToByPIDL(NewPIDL, true, true, false, false);

  //if FolderTree.AutoCollapse then
  FolderTree.ScrollIntoView(FolderTree.FocusedNode,true,true);
end;

{*------------------------------------------------------------------------------
  Mouse Down event
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FolderTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  node: PVirtualNode;
  NS: TNamespace;
begin
  if (Button = mbMiddle) or (Shift = [ssLeft, ssAlt]) or (Shift = [ssLeft, ssAlt, ssShift])  then
  begin
    node:= FolderTree.GetNodeAt(X,Y);
    if assigned(node) then
    begin
      FolderTree.ValidateNamespace(node, NS);
      if assigned(NS) then
      begin
        if (Shift = [ssLeft, ssAlt]) then
        begin
          if fNewTabByDefault then
          GlobalPathCtrl.ChangeGlobalPathPIDL(Self, NS.AbsolutePIDL)
          else
          OpenFolderInTab(FolderTree, NS.AbsolutePIDL, MainForm.TabSet.OpenTabSelect);
        end
        else
        begin
          if ssShift in Shift then
          OpenFolderInTab(FolderTree, NS.AbsolutePIDL, not MainForm.TabSet.OpenTabSelect)
          else
          OpenFolderInTab(FolderTree, NS.AbsolutePIDL, MainForm.TabSet.OpenTabSelect);
        end;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Incremental Search
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FolderTreeIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: WideString; var Result: Integer);

  function DoCompare(Str1: WideString; Str2: WideString): Integer;
  begin
    if IsUnicode then
    Result:= lstrcmpiW(PWideChar(Str1), PWideChar(Str2))
    else
    Result:= lstrcmpi(PChar(string(Str1)), PChar(string(Str2)));
  end;

var
  CompareStr: WideString;
begin
  CompareStr:= FolderTree.Text[Node,-1];
  SetLength(CompareStr, Length(SearchText));
  Result:= DoCompare(CompareStr, SearchText);
end;

{*------------------------------------------------------------------------------
  On Key Action
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FolderTreeKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  case CharCode of
    VK_F5: DoDefault:= false;  
  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  On Edited
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.OnEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
    Column: TColumnIndex);
begin
  FolderTree.RefreshNode(Node);
  FolderTreeSelectedChange(Node);
end;

{-------------------------------------------------------------------------------
  Load Settings
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.LoadFromStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('FolderPanel');
  try
    FolderTree.AutoCollapse:= Storage.ReadBoolean('AutoCollapse', false);
    FolderTree.AutoExpand:= Storage.ReadBoolean('AutoExpand', false);
    NewTabByDefault:= Storage.ReadBoolean('OpenInNewTab', false);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Save Settings
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.SaveToStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('FolderPanel');
  try
    Storage.WriteBoolean('AutoCollapse', FolderTree.AutoCollapse);
    Storage.WriteBoolean('AutoExpand', FolderTree.AutoExpand);
    Storage.WriteBoolean('OpenInNewTab', NewTabByDefault);
  finally
    Storage.ClosePath;
  end;
end;

end.
