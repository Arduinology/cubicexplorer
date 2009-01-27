unit fCE_FileSearchDestDlg;

interface

uses
  // CE Units
  CE_VistaFuncs,
  // VSTools
  VirtualExplorerTree, MPCommonUtilities, MPShellUtilities,
  // VirtualTree
  VirtualTrees,
  // Tnt Controls
  TntClasses,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TCEDestDlg = class(TForm)
    FolderTree: TVirtualExplorerTree;
    procedure FolderTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
  public
    CheckedFolders: TTntStringList;
    TriState: Boolean;
    function GetCheckedFolders(ParentsOnly: Boolean = false): WideString;
    procedure SetCheckedFolders(Folders: WideString);
  end;

implementation

{$R *.dfm}

{*------------------------------------------------------------------------------
  Create an instance of TCEDestDlg.
-------------------------------------------------------------------------------}
procedure TCEDestDlg.FormCreate(Sender: TObject);
begin
  SetVistaFont(Font);
  CheckedFolders:= TTntStringList.Create;
  CheckedFolders.Delimiter:= ',';
  CheckedFolders.StrictDelimiter:= true;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEDestDlg.
-------------------------------------------------------------------------------}
procedure TCEDestDlg.FormDestroy(Sender: TObject);
begin
  CheckedFolders.Free;
end;

{*------------------------------------------------------------------------------
  Get's called when node is initialized. Set the CheckType.
-------------------------------------------------------------------------------}
procedure TCEDestDlg.FolderTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if TriState then
  Node.CheckType:= ctTriStateCheckBox
  else
  Node.CheckType:= ctCheckBox;
end;

{*------------------------------------------------------------------------------
  Get Checked Folders.
-------------------------------------------------------------------------------}
function TCEDestDlg.GetCheckedFolders(ParentsOnly: Boolean = false): WideString;
var
  node, pnode: PVirtualNode;
  NS: TNamespace;
begin
  CheckedFolders.Clear;
  node:= FolderTree.GetFirstChecked(csCheckedNormal);
  while node <> nil do
  begin
    pnode:= node.Parent;
    if pnode <> nil then
    begin
      if not ParentsOnly or (pnode.CheckState <> csCheckedNormal) then
      begin
        FolderTree.ValidateNamespace(node, NS);
        if assigned(NS) then
        CheckedFolders.Add(NS.NameForParsing);        
      end;
    end;
    node:= FolderTree.GetNextChecked(node);
  end;
    
  Result:= CheckedFolders.DelimitedText;
end;

{*------------------------------------------------------------------------------
  Set Checked folders.
-------------------------------------------------------------------------------}
procedure TCEDestDlg.SetCheckedFolders(Folders: WideString);
var
  i: Integer;
  node: PVirtualNode;
begin
  CheckedFolders.Clear;
  CheckedFolders.DelimitedText:= Folders;
  FolderTree.BeginUpdate;
  try
    for i:= 0 to CheckedFolders.Count - 1 do
    begin
      node:= FolderTree.FindNode(CheckedFolders.Strings[i]);
      if node = nil then
      begin
        FolderTree.BrowseTo(CheckedFolders.Strings[i], false, false, false, false);
        node:= FolderTree.FindNode(CheckedFolders.Strings[i]);
      end;
      if node <> nil then
      node.CheckState:= csCheckedNormal;
    end;
  finally
    FolderTree.EndUpdate;
  end;
end;

end.
