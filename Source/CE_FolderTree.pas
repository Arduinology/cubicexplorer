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
//  The Original Code is CE_FolderTree.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_FolderTree;

interface

uses
  // CE Units
  CE_Utils,
  // VSTools, VT
  VirtualTrees, VirtualExplorerTree, MPShellUtilities, VirtualShellNotifier,
  MPCommonObjects, MPThreadManager,
  // System Units
  Classes, Windows, Messages, SysUtils, Graphics, Forms, Controls, ExtCtrls;

type
  TCEFolderTreeSelectedChangeEvent = procedure(Node: PVirtualNode) of object;

  TCEFolderTree = class(TVirtualExplorerTreeview)
  private
    fAutoCollapse: Boolean;
    fAutoExpand: Boolean;
    fBrowseZipFolders: Boolean;
    fHiddenFiles: Boolean;
    fOnSelectedChange: TCEFolderTreeSelectedChangeEvent;
    fSelectionTimer: TTimer;
    procedure SetBrowseZipFolders(const Value: Boolean);
    procedure SetHiddenFiles(const Value: Boolean);
  protected
    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;
        override;
    procedure DoEnumFolder(const Namespace: TNamespace; var AllowAsChild: Boolean);
        override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean;
        override;
    procedure DoSelectedChange(Node: PVirtualNode);
    procedure DoShellNotify(ShellEvent: TVirtualShellEvent); override;
    procedure HandleMouseDblClick(var Message: TWMMouse; const HitInfo: THitInfo);
        override;
    procedure HandleMouseDown(var Message: TWMMouse; const HitInfo: THitInfo);
        override;
    procedure ReReadAndRefreshNode(Node: PVirtualNode; SortNode: Boolean); override;
    procedure SelectionTimer(Sender: TObject);
    procedure WMKILLFOCUS(var Message: TMessage); message WM_KILLFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    function PasteShortcutFromClipboard: Boolean;
    function Refresh: Boolean;
    procedure SelectedFilesDelete(ShiftKeyState: TExecuteVerbShift = evsCurrent);
        override;
    property AutoCollapse: Boolean read fAutoCollapse write fAutoCollapse;
    property AutoExpand: Boolean read fAutoExpand write fAutoExpand;
    property BrowseZipFolders: Boolean read fBrowseZipFolders write
        SetBrowseZipFolders;
    property HiddenFiles: Boolean read fHiddenFiles write SetHiddenFiles;
  published
    property OnSelectedChange: TCEFolderTreeSelectedChangeEvent read
        fOnSelectedChange write fOnSelectedChange;
  end;

  TCE_VTEdit = class(TVTEdit)
  private
  protected
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
  end;

  TCE_StringEditLink = class(TStringEditLink)
  public
    constructor Create;
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEFolderTree
-------------------------------------------------------------------------------}
constructor TCEFolderTree.Create(AOwner: TComponent);
begin
  inherited;
  Self.BorderStyle:= bsNone;
  Self.TreeOptions.AutoOptions:= [toAutoDropExpand,toAutoScrollOnExpand,toAutoTristateTracking,toAutoDeleteMovedNodes];
  Self.TreeOptions.MiscOptions:= [toAcceptOLEDrop,toEditable,toFullRepaintOnResize,toInitOnSave,toToggleOnDblClick];
  Self.TreeOptions.PaintOptions:= [toShowButtons,toShowTreeLines,toUseBlendedImages,toGhostedIfUnfocused];
  Self.TreeOptions.VETImageOptions:= [toImages,toMarkCutAndCopy];
  Self.TreeOptions.VETMiscOptions:= [toBrowseExecuteFolder, toBrowseExecuteZipFolder, toRemoveContextMenuShortCut,toBrowseExecuteFolderShortcut,toChangeNotifierThread,toTrackChangesInMappedDrives,toExecuteOnDblClk, toNoRebuildIconListOnAssocChange];
  Self.TreeOptions.VETShellOptions:= [toRightAlignSizeColumn,toContextMenus,toDragDrop];
  Self.VETColors.FileTextColor:= clWindowText;
  Self.VETColors.FolderTextColor:= clWindowText;
  HiddenFiles:= false;

  fSelectionTimer:= TTimer.Create(Self);
  fSelectionTimer.Enabled:= false;
  fSelectionTimer.Interval:= 500;
  fSelectionTimer.OnTimer:= SelectionTimer;
  Self.DefaultNodeHeight:= SmallShellIconSize + 1;
end;

{*------------------------------------------------------------------------------
  Create custom editor
-------------------------------------------------------------------------------}
function TCEFolderTree.DoCreateEditor(Node: PVirtualNode; Column:
    TColumnIndex): IVTEditLink;
begin
  Result:= TCE_StringEditLink.Create;
end;

procedure TCEFolderTree.DoEnumFolder(const Namespace: TNamespace; var
    AllowAsChild: Boolean);
begin
  AllowAsChild:= Namespace.Folder;
  inherited;
end;

{*------------------------------------------------------------------------------
  Handle Key Actions
-------------------------------------------------------------------------------}
function TCEFolderTree.DoKeyAction(var CharCode: Word; var Shift: TShiftState):
    Boolean;
begin
  Result:= true;
  case CharCode of
    VK_F2,
    VK_F5,
    VK_DELETE:
    begin
      if Shift = [] then
      Result:= false;
    end;
    Ord('C'), Ord('c'):
    begin
      if ssCtrl in Shift then
      Result:= false;
    end;
    Ord('X'), Ord('x'):
    begin
      if ssCtrl in Shift then
      Result:= false;
    end;
    Ord('V'), Ord('v'):
    begin
      if ssCtrl in Shift then
      Result:= false;
    end;
    Ord('A'), Ord('a'):
    begin
      if (Shift = [ssShift,ssCtrl]) or (Shift = [ssCtrl]) then
      Result:= false;
    end;
    VK_LEFT, VK_RIGHT:
    begin
      if Shift = [ssAlt] then
      Result:= false
      else
      begin
        fSelectionTimer.Enabled:= false;
        fSelectionTimer.Enabled:= true;
      end;
    end;
    VK_DOWN, VK_UP:
    begin
      fSelectionTimer.Enabled:= false;
      fSelectionTimer.Enabled:= true;
    end;
    VK_INSERT:
    begin
      if (ssShift in Shift) or (ssCtrl in Shift) then
      Result:= false;
    end;
    VK_RETURN, VK_SPACE:
    begin
      SelectionTimer(self);
      Result:= false;
    end;
  end;

  if Result then
  inherited DoKeyAction(CharCode, Shift);
end;

{-------------------------------------------------------------------------------
  Do Selected Change
-------------------------------------------------------------------------------}
procedure TCEFolderTree.DoSelectedChange(Node: PVirtualNode);
begin
  if Assigned(fOnSelectedChange) then fOnSelectedChange(Node);
end;

{-------------------------------------------------------------------------------
  Do Shell Notify
-------------------------------------------------------------------------------}
procedure TCEFolderTree.DoShellNotify(ShellEvent: TVirtualShellEvent);
begin
  inherited;
  if not ShellEvent.Handled then
  begin
    if ShellEvent.ShellNotifyEvent = vsneAssoccChanged then
    begin
//      RebuildTree;
//      Refresh;
      ShellEvent.Handled:= true;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Down
-------------------------------------------------------------------------------}
procedure TCEFolderTree.HandleMouseDown(var Message: TWMMouse; const HitInfo:
    THitInfo);
var
  ShiftState: TShiftState;
begin
  if not assigned(HitInfo.HitNode) then
  begin
    inherited;
  end
  else if (hiOnItemButton in HitInfo.HitPositions) then
  begin
    if HitInfo.HitNode <> self.FocusedNode then
    begin
      if Self.Expanded[HitInfo.HitNode] and Self.HasAsParent(Self.FocusedNode, HitInfo.HitNode) then
      begin
        Self.Expanded[HitInfo.HitNode]:= false;
        DoSelectedChange(HitInfo.HitNode);
      end
      else
      begin
        Self.ToggleNode(HitInfo.HitNode);
      end;
    end
    else
    Self.ToggleNode(HitInfo.HitNode);
  end
  else if HitInfo.HitNode = self.FocusedNode then
  begin
    inherited;
  end
  else
  begin
    Self.BeginUpdate;
    try
      if fAutoCollapse then
      Self.FullCollapse;

      Self.ClearSelection;

      Self.FullyVisible[HitInfo.HitNode]:= true;
      if fAutoExpand and not Self.Expanded[HitInfo.HitNode] then
      Self.ToggleNode(HitInfo.HitNode);

      Self.Selected[HitInfo.HitNode]:= true;
      Self.FocusedNode:= HitInfo.HitNode;
      Self.ScrollIntoView(HitInfo.HitNode,false,false);
    finally
      Self.EndUpdate;
    end;
    
    ShiftState := KeysToShiftState(Message.Keys) * [ssShift, ssCtrl, ssAlt];
    if ShiftState = [] then
    DoSelectedChange(Self.FocusedNode);
  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Dbl Click
-------------------------------------------------------------------------------}
procedure TCEFolderTree.HandleMouseDblClick(var Message: TWMMouse; const HitInfo:
    THitInfo);
begin
  if not assigned(HitInfo.HitNode) then
  begin
    inherited;
    Exit;
  end;

  if Self.Expanded[HitInfo.HitNode] then
  begin
    Self.ToggleNode(HitInfo.HitNode);
    Exit;
  end;

  Self.BeginUpdate;
  try
    if fAutoCollapse then
    Self.FullCollapse;
    
    Self.ClearSelection;
    Self.FullyVisible[HitInfo.HitNode]:= true;
    Self.ToggleNode(HitInfo.HitNode);
    Self.Selected[HitInfo.HitNode]:= true;
    Self.FocusedNode:= HitInfo.HitNode;
    Self.ScrollIntoView(HitInfo.HitNode,false,true);
  finally
    Self.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Paste Shortcut From Clipboard
-------------------------------------------------------------------------------}
function TCEFolderTree.PasteShortcutFromClipboard: Boolean;
var
  NS: TNamespace;
  NSA: TNamespaceArray;
  Handled: Boolean;
begin
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    WaitCursor(True);
    try
      Handled := False;
      DoClipboardPaste(Handled);
      if not Handled then
      begin
        Result := False;
        if SelectedCount = 1 then
        begin
          SetLength(NSA, 1);
          if ValidateNamespace(GetFirstSelected, NS) then
          begin
            NSA[0] := NS;
            NS.Paste(Self, NSA, true);
            Result := True
          end
        end
      end else
        Result := True
    finally
      WaitCursor(False)
    end
  end else
    Result := False
end;


procedure TCEFolderTree.WMKILLFOCUS(var Message: TMessage);
begin
  inherited;
  //if Self.IsEditing then
  //Self.EndEditNode;
end;

{-------------------------------------------------------------------------------
  Refresh
-------------------------------------------------------------------------------}
function TCEFolderTree.Refresh: Boolean;
begin
  Result:= RefreshTree(true);
end;

procedure TCEFolderTree.ReReadAndRefreshNode(Node: PVirtualNode; SortNode:
    Boolean);
///
/// NOTE:  Make sure any changes to this method are reflected in both VirtualExplorerTree.pas
//         and VirtualExplorerListview.pas
///
var
  i, j, PIDLsRead, NodesRead, PIDLArrayLen, NodeArrayLen: Integer;
  PIDLArray: TPIDLArray;
  NS, NewNS: TNamespace;
  NodeArray: TNodeSearchArray;
  Compare: ShortInt;
  Allow: Boolean;
  CheckSupport: Boolean;
  ResultIsParent: Boolean;
begin
  // TODO: Not sure why this was overrided.
  inherited;
  Exit;
  
  if ValidateNamespace(Node, NS) then
  begin
    // First see if the namespace is valid
    if NS.Valid then
    begin
      BeginUpdate;  //TODO: Testing a fix for bugs #146 and #174
      
      // Smarter to read child nodes that are currently cached in the tree
      // first so ReadFolder does not trigger more events
      ReadChildNodes(Node, NodeArray, True, NodesRead);

      // Need to invalidate namespace as if a new item is added it may not be recognized by
      // the cached IShellFolder!
      NS.InvalidateNamespace(True);

      ReadFolder(NS.ShellFolder, FileObjectsToFlags(FileObjects), PIDLArray, True, PIDLsRead);
      CheckSupport := toCheckSupport in TreeOptions.MiscOptions;  // Local variable for speed

      //BeginUpdate;  //TODO: Testing a fix for bugs #146 and #174
      try
        PIDLArrayLen := PIDLsRead;
        NodeArrayLen := NodesRead;
        j := 0;
        i := 0;
        // Run the current nodes in the tree with the nodes read from the folder in
        // parallel to see what is missing/added
        while (i < PIDLArrayLen) and (j < NodeArrayLen) do
        begin
          Compare := ShortInt(NS.ShellFolder.CompareIDs(0, PIDLArray[i], NodeArray[j].NS.RelativePIDL));
          if Compare = 0 then
          begin
            Inc(j);  // Node exists move on
            Inc(i)
          end else
          if Compare < 0 then
          begin
            // Must be a new node, don't Inc j
            Allow := True;
            NewNS := TNamespace.Create(PIDLMgr.CopyPIDL(PIDLArray[i]), NS);
            // Need to make sure any additions are ok'ed by the application
            if Assigned(OnEnumFolder) then
              OnEnumFolder(Self, NewNS, Allow);
            // Add it to the bottom of the list
            if Allow then
              AddCustomNode(Node, NewNS, CheckSupport)
            else
              NewNS.Free;
            Inc(i)
          end else
          begin
            GlobalThreadManager.FlushAllMessageCache(Self, NodeArray[j].Node);
            // Must be a removed node, don't Inc i
            if ValidateNamespace(NodeArray[j].Node, NewNS) then
            begin
              if CheckSupport then
                Storage.Delete(NewNS.AbsolutePIDL, [], True);
              DoNamespaceStructureChange(NodeArray[j].Node, NewNS, nscDelete);
            end;
            DeleteNode(NodeArray[j].Node);
            Inc(j)
          end
        end;

        // Add any new items
        while i < PIDLArrayLen do
        begin
          Allow := True;
          NewNS := TNamespace.Create(PIDLMgr.CopyPIDL(PIDLArray[i]), NS);
          { Need to make sure any additions are ok'ed by the application }
          if Assigned(OnEnumFolder) then
            OnEnumFolder(Self, NewNS, Allow);
          if Allow then
            AddCustomNode(Node, NewNS, CheckSupport)
          else
            NewNS.Free;
          Inc(i)
        end;

        while j < NodeArrayLen do
        begin
          GlobalThreadManager.FlushAllMessageCache(Self, NodeArray[j].Node);
          // Must be a removed node, don't Inc i
          if ValidateNamespace(NodeArray[j].Node, NewNS) then
          begin
            if CheckSupport then
              Storage.Delete(NewNS.AbsolutePIDL, [], True);
            DoNamespaceStructureChange(NodeArray[j].Node, NewNS, nscDelete);
          end;
          DeleteNode(NodeArray[j].Node);
          Inc(j)
        end;

        for i := 0 to Length(PIDLArray) - 1 do
          PIDLMgr.FreePIDL(PIDLArray[i])
      finally
        EndUpdate
      end;

      if SortNode then
        Sort(Node, Header.SortColumn, Header.SortDirection, False);

      if (Node.ChildCount > 0) and (toFoldersExpandable in TreeOptions.VETFolderOptions) then
        HasChildren[Node] := True;
    end else
    begin
      // The passed node is invalid itself
      if ValidateNamespace(Node, NS) then
        DoNamespaceStructureChange(Node, NS, nscDelete);
      NextSelectedNode(Node, True, ResultIsParent);
      DeleteNode(Node)
    end
  end
end;

procedure TCEFolderTree.SelectedFilesDelete(ShiftKeyState: TExecuteVerbShift =
    evsCurrent);
var
  Node: PVirtualNode;
  NS: TNamespace;
begin
  if not (toVETReadOnly in TreeOptions.VETMiscOptions) then
  begin
    WaitCursor(True);
    try
      Node:= GetFirstSelected;
      if Assigned(Node) then
      begin
        if ValidateNamespace(Node, NS) then
        begin
          if NS.Delete(Self, SelectedToNamespaceArray, ShiftKeyState) then
          begin
            ReReadAndRefreshNode(node, false);
            DoSelectedChange(GetFirstSelected);
          end;
        end;
      end;
    finally
      WaitCursor(False)
    end
  end
end;

{-------------------------------------------------------------------------------
  Handle Selection Timer Event
-------------------------------------------------------------------------------}
procedure TCEFolderTree.SelectionTimer(Sender: TObject);
begin
  fSelectionTimer.Enabled:= false;
  DoSelectedChange(FocusedNode);
end;

{-------------------------------------------------------------------------------
  Set BrowseZipFolders
-------------------------------------------------------------------------------}
procedure TCEFolderTree.SetBrowseZipFolders(const Value: Boolean);
var
  obj: TFileObjects;
begin
  fBrowseZipFolders:= Value;
  obj:= FileObjects;
  if fBrowseZipFolders then Include(obj, foNonFolders) else Exclude(obj, foNonFolders);
  FileObjects:= obj;
end;

{-------------------------------------------------------------------------------
  Set Hidden Files
-------------------------------------------------------------------------------}
procedure TCEFolderTree.SetHiddenFiles(const Value: Boolean);
var
  obj: TFileObjects;
begin
  fHiddenFiles:= Value;
  obj:= FileObjects;
  if fHiddenFiles then Include(obj, foHidden) else Exclude(obj, foHidden);
  FileObjects:= obj;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Handle WM_KillFocus message
-------------------------------------------------------------------------------}
procedure TCE_VTEdit.WMKillFocus(var Message: TMessage);
begin
  Self.Perform(CM_Exit,0,0);
  inherited;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create instance of TCE_StringEditLink
-------------------------------------------------------------------------------}
constructor TCE_StringEditLink.Create;
var
  fEdit: TVTEdit;
begin
  //inherited;
  FEdit := TCE_VTEdit.Create(Self);
  fEdit.Visible := False;
  fEdit.BorderStyle := bsSingle;
  fEdit.AutoSize := False;

  Self.Edit:= fEdit;
end;



end.
