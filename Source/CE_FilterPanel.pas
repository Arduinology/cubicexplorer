unit CE_FilterPanel;

interface

uses
  // CE Units
  CE_LanguageEngine,
  // Tnt Controls
  TntClasses, TntSysUtils,
  // VT
  VirtualTrees,
  // JVCL
  JvSimpleXml, JvAppStorage,    
  // VSTools
  VirtualExplorerEasyListview, EasyListview, MPShellUtilities,
  // System Units
  Classes, Windows, Messages, SysUtils, Graphics, Forms;

type

  PFilterItem = ^AFilterItem;
  AFilterItem = record
    Extension: WideString;
    Count: Integer;
    ShowAllItem: Boolean;
    ShowFoldersItem: Boolean;
  end;

  TCEFilterPanel = class(TVirtualStringTree, IJvAppStorageHandler)
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath:
        string);
  private
    fActive: Boolean;
    fFilteringImage: TBitmap;
    fExplorerEasyListview: TVirtualExplorerEasyListview;
    fShowAllExtensions: Boolean;
    fShowFolders: Boolean;
    fShowAllNode: PVirtualNode;
    fShowFilteringBackground: Boolean;
    fShowFoldersNode: PVirtualNode;
    procedure SetActive(const Value: Boolean);
    procedure SetExplorerEasyListview(const Value: TVirtualExplorerEasyListview);
    procedure SetShowAllExtensions(const Value: Boolean);
    procedure SetShowFilteringBackground(const Value: Boolean);
    procedure SetShowFolders(const Value: Boolean);
  protected
    procedure DoChecked(Node: PVirtualNode); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer;
        override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType:
        TVSTTextType; var Text: WideString); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column:
        TColumnIndex; TextType: TVSTTextType); override;
    procedure HandleMouseDown(var Message: TWMMouse; const HitInfo: THitInfo);
        override;
  public
    ActiveFilters: TTntStrings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearFilters;
    procedure DeFilter;
    procedure DoFiltering;
    function FindByExtension(ext: WideString): PVirtualNode;
    procedure PopulateTree;
    property Active: Boolean read fActive write SetActive;
    property FilteringImage: TBitmap read fFilteringImage write fFilteringImage;
    property ExplorerEasyListview: TVirtualExplorerEasyListview read
        fExplorerEasyListview write SetExplorerEasyListview;
    property ShowAllExtensions: Boolean read fShowAllExtensions write
        SetShowAllExtensions;
    property ShowFilteringBackground: Boolean read fShowFilteringBackground write
        SetShowFilteringBackground;
    property ShowFolders: Boolean read fShowFolders write SetShowFolders;
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEFilterPanel
-------------------------------------------------------------------------------}
constructor TCEFilterPanel.Create(AOwner: TComponent);
begin
  inherited;
  Self.NodeDataSize:= SizeOf(AFilterItem);
  ActiveFilters:= TTntStringList.Create;

  Self.BorderStyle:= bsNone;
  Self.CheckImageKind:= ckSystem;
  Self.TreeOptions.PaintOptions:= [toHideFocusRect,toHideSelection,toShowButtons,toShowDropmark,toThemeAware,toAlwaysHideSelection]; //toShowHorzGridLines];
  Self.TreeOptions.MiscOptions:= [toCheckSupport,toFullRepaintOnResize,toInitOnSave,toToggleOnDblClick];
  fShowAllExtensions:= true;
  fShowFolders:= true;
  fShowFilteringBackground:= true;
  fActive:= false;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEFilterPanel
-------------------------------------------------------------------------------}
destructor TCEFilterPanel.Destroy;
begin
  ActiveFilters.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Clear Filters
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.ClearFilters;
begin
  ActiveFilters.Clear;
  ShowAllExtensions:= true;
  ShowFolders:= true;
end;

{*------------------------------------------------------------------------------
  Find Node By Extension
-------------------------------------------------------------------------------}
function TCEFilterPanel.FindByExtension(ext: WideString): PVirtualNode;
var
  d: PFilterItem;
begin
  Result:= Self.GetFirst;
  while assigned(Result) do
  begin
    d:= Self.GetNodeData(Result);
    if WideCompareText(d.extension, ext) = 0 then
    break
    else
    Result:= Result.NextSibling;
  end;
end;

{*------------------------------------------------------------------------------
  Populate tree
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.PopulateTree;
var
  i: Integer;
  item: TEasyItem;
  NS: TNamespace;
  data, dataAllItem, dataFolderItem: PFilterItem;
  node: PVirtualNode;
begin
  if not assigned(fExplorerEasyListview) then
  Exit;
  
  Self.BeginUpdate;
  Self.Clear;
  try
    // Add Show Folders node
    fShowFoldersNode:= Self.AddChild(nil);
    fShowFoldersNode.CheckType:= ctCheckBox;
    if ShowFolders then
    fShowFoldersNode.CheckState:= csCheckedNormal;
    dataFolderItem:= Self.GetNodeData(fShowFoldersNode);
    dataFolderItem.ShowFoldersItem:= true;
    // Add Show All node
    fShowAllNode:= Self.AddChild(nil);
    fShowAllNode.CheckType:= ctCheckBox;
    if ShowAllExtensions or (ActiveFilters.Count = 0) then
    fShowAllNode.CheckState:= csCheckedNormal;
    dataAllItem:= Self.GetNodeData(fShowAllNode);
    dataAllItem.ShowAllItem:= true;
    // Add Filter nodes
    for i:= 0 to ActiveFilters.Count - 1 do
    begin
      node:= Self.AddChild(nil);
      node.CheckType:= ctCheckBox;
      node.CheckState:= csCheckedNormal;
      data:= Self.GetNodeData(node);
      data.extension:= ActiveFilters.Strings[i];
      data.count:= 0;
    end;

    for i:= 0 to fExplorerEasyListview.ItemCount - 1 do
    begin
      item:= fExplorerEasyListview.Items.Items[i];
      fExplorerEasyListview.ValidateNamespace(item, NS);
      if assigned(NS) then
      begin
        if (not NS.Folder) or (WideCompareText(NS.Extension,'.zip') = 0) then
        begin
          Inc(dataAllItem.Count, 1);
          if NS.Extension <> '' then
          begin
            node:= FindByExtension(NS.Extension);
            if assigned(node) then
            begin
              data:= Self.GetNodeData(node);
              Inc(data.count,1);
            end
            else
            begin
              node:= Self.AddChild(nil);
              node.CheckType:= ctCheckBox;
              data:= Self.GetNodeData(node);
              data.extension:= NS.Extension;
              data.count:= 1;
            end;
          end
          else
          begin
            node:= FindByExtension('none');
            if assigned(node) then
            begin
              data:= Self.GetNodeData(node);
              Inc(data.count,1);
            end
            else
            begin
              node:= Self.AddChild(nil);
              node.CheckType:= ctCheckBox;
              data:= Self.GetNodeData(node);
              data.extension:= 'none';
              data.count:= 1;
            end;          
          end;
        end
        else
        begin
          Inc(dataFolderItem.Count, 1);
        end;
      end;
    end;
  finally
    Self.EndUpdate;
    Self.SortTree(-1,sdAscending);
  end;
end;

{*------------------------------------------------------------------------------
  Do filtering
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.DoFiltering;
var
  i: Integer;
  item: TEasyItem;
  NS: TNamespace;
  NoFiltering: Boolean;
begin
  if not assigned(fExplorerEasyListview) then
  Exit;

  NoFiltering:= true;
  fExplorerEasyListview.BeginUpdate;
  try
    for i:= 0 to fExplorerEasyListview.ItemCount - 1 do
    begin
      item:= fExplorerEasyListview.Items.Items[i];
      if (ActiveFilters.Count = 0) or fShowAllExtensions then
      begin
        if not fShowFolders then
        begin
          fExplorerEasyListview.ValidateNamespace(item, NS);
          if assigned(NS) then
          begin
            if NS.Folder and (WideCompareText(NS.Extension,'.zip') <> 0) then
            item.Visible:= false
            else
            item.Visible:= true;
          end;
        end
        else
        item.Visible:= true;
      end
      else
      begin
        fExplorerEasyListview.ValidateNamespace(item, NS);
        if assigned(NS) then
        begin
          if NS.Folder and (WideCompareText(NS.Extension,'.zip') <> 0) then
          item.Visible:= fShowFolders
          else if NS.Extension = '' then
          item.Visible:= ActiveFilters.IndexOf('none') > -1
          else
          item.Visible:= ActiveFilters.IndexOf(NS.Extension) > -1;
        end;
      end;

      if NoFiltering then
      NoFiltering:= item.Visible;
    end;
  finally
    if fShowFilteringBackground then
    fExplorerEasyListview.BackGround.Enabled:= not NoFiltering;
    
    fExplorerEasyListview.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  De-filter items (show all)
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.DeFilter;
var
  i: Integer;
begin
  if not assigned(fExplorerEasyListview) then
  Exit;

  fExplorerEasyListview.BeginUpdate;
  try
    for i:= 0 to fExplorerEasyListview.ItemCount - 1 do
    begin
      fExplorerEasyListview.Items.Items[i].Visible:= true;
    end;
  finally
    if fShowFilteringBackground then
    fExplorerEasyListview.BackGround.Enabled:= false;
    
    fExplorerEasyListview.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  DoGetText
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var Text: WideString);
var
  data: PFilterItem;
begin
  data:= Self.GetNodeData(Node);
  if data.ShowAllItem then
  Text:= _('Show All Files') + ' (' + IntToStr(data.count) + ')'
  else if data.ShowFoldersItem then
  Text:= _('Show Folders') + ' (' + IntToStr(data.count) + ')'
  else if data.Extension = 'none' then
  begin
    Text:= _('No Extension') + ' (' + IntToStr(data.count) + ')';
  end
  else
  Text:= data.extension + ' (' + IntToStr(data.count) + ')';
end;

{*------------------------------------------------------------------------------
  DoChecked
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.DoChecked(Node: PVirtualNode);
var
  n: PVirtualNode;
  data: PFilterItem;
begin
  data:= Self.GetNodeData(Node);
  if data.ShowAllItem then
  begin
    ShowAllExtensions:= (Node.CheckState = csCheckedNormal) or (ActiveFilters.Count = 0);
    Self.Repaint;
  end
  else if data.ShowFoldersItem then
  begin
    ShowFolders:= (Node.CheckState = csCheckedNormal);
    Self.Repaint;
  end
  else
  begin
    n:= Self.GetFirst;
    ActiveFilters.Clear;
    while assigned(n) do
    begin
      if n.CheckState = csCheckedNormal then
      begin
        data:= Self.GetNodeData(n);
        if not data.ShowAllItem and not data.ShowFoldersItem then
        ActiveFilters.Add(data.extension);
      end;
      n:= n.NextSibling;
    end;

    ShowAllExtensions:= ActiveFilters.Count = 0;

    DoFiltering;
    Self.Repaint;
  end;
end;

{*------------------------------------------------------------------------------
  Do Compare
-------------------------------------------------------------------------------}
function TCEFilterPanel.DoCompare(Node1, Node2: PVirtualNode; Column:
    TColumnIndex): Integer;
var
  data1, data2: PFilterItem;
begin
  data1:= GetNodeData(Node1);
  data2:= GetNodeData(Node2);
  if data1.ShowFoldersItem then
  Result:= -1
  else if data2.ShowFoldersItem then
  Result:= 1
  else if data1.ShowAllItem then
  Result:= -1
  else if data2.ShowAllItem then
  Result:= 1
  else if data1.Extension = 'none' then
  Result:= 1
  else if data2.Extension = 'none' then
  Result:= -1
  else
  Result:= CompareText(data1.Extension, data2.Extension);
end;

{*------------------------------------------------------------------------------
  DoPaintText
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
    Column: TColumnIndex; TextType: TVSTTextType);
var
  data: PFilterItem;
begin
  inherited;

  data:= Self.GetNodeData(Node);
  if data.ShowFoldersItem then
  begin
    if Node.CheckState = csCheckedNormal then
    Canvas.Font.Color:= clWindowText
    else
    Canvas.Font.Color:= clBtnShadow;
  end
  else if fShowAllExtensions then
  begin
    if data.ShowAllItem then
    Canvas.Font.Color:= clWindowText
    else
    Canvas.Font.Color:= clBtnShadow;
  end
  else
  begin
    if data.ShowAllItem then
    begin
      if ActiveFilters.Count = 0 then
      Canvas.Font.Color:= clWindowText
      else
      Canvas.Font.Color:= clBtnShadow;
    end
    else
    Canvas.Font.Color:= clWindowText;
  end;

  if Node.CheckState = csCheckedNormal then
  begin
    if data.ShowAllItem or data.ShowFoldersItem then
    Canvas.Font.Style:= [fsBold,fsUnderline]
    else if not fShowAllExtensions then
    Canvas.Font.Style:= [fsBold];
  end
  else
  begin
    if data.ShowAllItem or data.ShowFoldersItem then
    Canvas.Font.Style:= [fsUnderline]
    else
    Canvas.Font.Style:= [];
  end;

end;

{*------------------------------------------------------------------------------
  Handle Mouse Down
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.HandleMouseDown(var Message: TWMMouse; const HitInfo:
    THitInfo);
begin
  inherited;
  if not assigned(HitInfo.HitNode) then
  Exit;
  
  if Message.Msg = WM_LBUTTONDOWN then
  begin
    if hiOnItemLabel in HitInfo.HitPositions then
    begin
      if HitInfo.HitNode.CheckState = csCheckedNormal then
      Self.CheckState[HitInfo.HitNode]:= csUncheckedNormal
      else
      Self.CheckState[HitInfo.HitNode]:= csCheckedNormal;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Set ExplorerEasyListview
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.SetExplorerEasyListview(const Value:
    TVirtualExplorerEasyListview);
begin
  if fExplorerEasyListview <> Value then
  begin
    fExplorerEasyListview:= Value;
    if assigned(fExplorerEasyListview) then
    begin
      if fExplorerEasyListview.BackGround.Image.Empty and fShowFilteringBackground then
      fExplorerEasyListview.BackGround.Image:= fFilteringImage;
    end;

    if fActive then
    begin
      PopulateTree;
      DoFiltering;
    end
    else
    DeFilter;
  end;
end;

{*------------------------------------------------------------------------------
  Set ShowAllExtension
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.SetShowAllExtensions(const Value: Boolean);
begin
  if assigned(fShowAllNode) then
  begin
    if Value then
    fShowAllNode.CheckState:= csCheckedNormal
    else
    fShowAllNode.CheckState:= csUncheckedNormal;
  end;

  if fShowAllExtensions <> Value then
  begin
    fShowAllExtensions:= Value;
    DoFiltering;
  end;
end;

{*------------------------------------------------------------------------------
  Set Active
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.SetActive(const Value: Boolean);
begin
  if fActive <> Value then
  begin
    fActive:= Value;
    if fActive then
    begin
      PopulateTree;
      DoFiltering;
    end
    else
    DeFilter;
  end;
end;

{*------------------------------------------------------------------------------
  Set ShowFilteringBackground
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.SetShowFilteringBackground(const Value: Boolean);
begin
  fShowFilteringBackground:= Value;

  if assigned(fExplorerEasyListview) then
  begin
    if fShowFilteringBackground then
    begin
      fExplorerEasyListview.BackGround.Enabled:= false;
      fExplorerEasyListview.BackGround.Image:= fFilteringImage;
    end
    else
    begin
      fExplorerEasyListview.BackGround.Enabled:= false;
      fExplorerEasyListview.BackGround.Image.FreeImage;
    end;
  end;

  if fActive then
  begin
    PopulateTree;
    DoFiltering;
  end;
end;

{*------------------------------------------------------------------------------
  Set SetShowFolders
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.SetShowFolders(const Value: Boolean);
begin
  if assigned(fShowFoldersNode) then
  begin
    if Value then
    fShowFoldersNode.CheckState:= csCheckedNormal
    else
    fShowFoldersNode.CheckState:= csUncheckedNormal;
  end;

  if fShowFolders <> Value then
  begin
    fShowFolders:= Value;
    DoFiltering;
  end;
end;

{*------------------------------------------------------------------------------
  Read properties from Storage
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  OldPath: String;
begin
  if not assigned(AppStorage) then
  Exit;
  OldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'FilterPanel']);
    with AppStorage do
    begin
      fShowFilteringBackground:= ReadBoolean('ShowBkgrd',true);
    end;
  finally
    AppStorage.Path:= OldPath;
  end;
end;

{*------------------------------------------------------------------------------
  Write properties to Storage
-------------------------------------------------------------------------------}
procedure TCEFilterPanel.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
    const BasePath: string);
var
  OldPath: String;
begin
  if not assigned(AppStorage) then
  Exit;
  OldPath:= AppStorage.Path;
  try
    AppStorage.DeleteSubTree(BasePath + '\FilterPanel');
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, BasePath, 'FilterPanel']);
    with AppStorage do
    begin
      WriteBoolean('ShowBkgrd',fShowFilteringBackground);
    end;
  finally
    AppStorage.Path:= OldPath;
  end;
end;

end.
