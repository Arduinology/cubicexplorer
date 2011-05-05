unit CE_StackTree;

interface

uses
  // CE
  CE_Stacks, CE_Utils, CE_CommonObjects,
  // VT
  VirtualTrees,
  // Tnt
  TntClasses,
  // VS Tools
  MPDataObject, MPShellUtilities, MPCommonObjects, MPCommonUtilities,
  VirtualExplorerTree,
  // System Units
  SysUtils, Classes, Windows, Forms, Controls, ActiveX, ImgList, ShlObj,
  Graphics, Menus;

type
  TCEStackDataObject = class(TVETDataObject);

  TCEStackItemType = (sitGroup, sitShell);

  PCEStackItemData = ^ACEStackItemData;
  ACEStackItemData = record
    Caption: WideString;
    Namespace: TNamespace;
    ItemType: TCEStackItemType;
    CEPath: WideString;
    Offline: Boolean;
  end;

  TCEStackTree = class(TVirtualStringTree)
  private
    fActiveDataObject: IDataObject;
    fActiveStack: TCEStackItem;
    fAutoSaveActiveStack: Boolean;
    fBackgroundPopupMenu: TPopupMenu;
    fGroupImageIndex: Integer;
    fGroupOpenImageIndex: Integer;
    fNotAvailableImageIndex: Integer;
    fSafeOperationsOnly: Boolean;
    fStacks: TCEStacks;
    function GetStacks: TCEStacks;
  protected
    function CanShowDragImage: Boolean; override;
    procedure DoAutoSaveActiveStack; virtual;
    function DoCancelEdit: Boolean; override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed:
        Boolean); override;
    procedure DoCollapsed(Node: PVirtualNode); override;
    function DoCreateDataObject: IDataObject; override;
    procedure DoDragDrop(Source: TObject; DataObject: IDataObject; Formats:
        TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode:
        TDropMode); override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt:
        TPoint; Mode: TDropMode; var Effect: Integer): Boolean; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    function DoEndEdit: Boolean; override;
    procedure DoExpanded(Node: PVirtualNode); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column:
        TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
        override;
    function DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex; var
        LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType:
        TVSTTextType; var Text: UnicodeString); override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean;
        override;
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text:
        UnicodeString); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column:
        TColumnIndex; TextType: TVSTTextType); override;
    procedure DoPopupMenu(Node: PVirtualNode; Column: TColumnIndex; Position:
        TPoint); override;
    procedure DragAndDrop(AllowedEffects: Integer; DataObject: IDataObject;
        DragEffect: Integer); override;
    procedure UpdateScrollbarSize;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CheckAvailability(APIDL: PItemIDList = nil);
    function InsertGroupNode(ToNode: PVirtualNode; Mode: TVTNodeAttachMode;
        ACaption: WideString = ''): PVirtualNode; virtual;
    function InsertShellNode(ToNode: PVirtualNode; Mode: TVTNodeAttachMode; APIDL:
        PItemIDList): PVirtualNode; virtual;
    procedure LoadFromStack(AStack: TCEStackItem); virtual;
    function RemoveEmptyGroups: Boolean;
    function SaveActiveStack: Boolean;
    procedure SaveToStack(AStack: TCEStackItem); virtual;
    function SelectedToNamespaceArray: TNamespaceArray;
    property ActiveStack: TCEStackItem read fActiveStack write fActiveStack;
  published
    property AutoSaveActiveStack: Boolean read fAutoSaveActiveStack write
        fAutoSaveActiveStack;
    property BackgroundPopupMenu: TPopupMenu read fBackgroundPopupMenu write
        fBackgroundPopupMenu;
    property GroupImageIndex: Integer read fGroupImageIndex write fGroupImageIndex;
    property GroupOpenImageIndex: Integer read fGroupOpenImageIndex write
        fGroupOpenImageIndex;
    property NotAvailableImageIndex: Integer read fNotAvailableImageIndex write
        fNotAvailableImageIndex;
    property SafeOperationsOnly: Boolean read fSafeOperationsOnly write
        fSafeOperationsOnly;
    property Stacks: TCEStacks read GetStacks write fStacks;
  end;

function PIDLToCEPath(APIDL: PItemIDList): WideString;

function CEPathToPIDL(APath: WideString): PItemIDList;

implementation

uses
  CE_LanguageEngine;

{-------------------------------------------------------------------------------
  PIDL to CEPath
-------------------------------------------------------------------------------}
function PIDLToCEPath(APIDL: PItemIDList): WideString;
var
  folderID: Integer;
  pidl: PItemIDList;
  ns: TNamespace;
begin
  Result:= '';
  if not assigned(APIDL) then Exit;
  // Get Special folder ID
  if PIDLMgr.IsDesktopFolder(APIDL) then
  folderID:= 0
  else
  folderID:= CE_SpecialNamespaces.GetSpecialID(APIDL);

  // Special Folder ID
  if folderID > -1 then
  Result:= 'special://' + IntToStr(folderID)
  else
  begin
    ns:= TNamespace.Create(APIDL, nil);
    ns.FreePIDLOnDestroy:= false;
    try
      pidl:= PathToPIDL(ns.NameForParsing);
      // Normal Path
      if assigned(pidl) then
      begin
        Result:= ns.NameForParsing;
        PIDLMgr.FreeAndNilPIDL(pidl);
      end
      // PIDL
      else
      begin
        Result:= 'pidl://' + SavePIDLToMime(APIDL);
      end;
    finally
      ns.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  CE Path to PIDL
-------------------------------------------------------------------------------}
function CEPathToPIDL(APath: WideString): PItemIDList;
var
  ws: WideString;
  c, folderID: Integer;
begin
  Result:= nil;
  c:= Length(APath);
  if c = 0 then Exit;
  // PIDL
  if Pos('pidl://', APath) = 1 then
  begin
    if c > 7 then
    begin
      ws:= Copy(APath, 8, c - 7);
      Result:= LoadPIDLFromMime(ws);
    end;
  end
  // Special folder id
  else if Pos('special://', APath) = 1 then
  begin
    if c > 10 then
    begin
      ws:= Copy(APath, 11, c - 10);
      folderID:= StrToIntDef(ws, -1);
      if folderID > -1 then
      SHGetspecialFolderLocation(0, folderID, Result);
    end;
  end
  // Normal Path
  else
  begin
    Result:= PathToPIDL(APath);
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEStackTree
-------------------------------------------------------------------------------}
constructor TCEStackTree.Create(AOwner: TComponent);
begin
  inherited;
  NodeDataSize:= SizeOf(ACEStackItemData);
  fStacks:= nil;
  fGroupImageIndex:= -1;
  fGroupOpenImageIndex:= -1;
  fNotAvailableImageIndex:= -1;
  fSafeOperationsOnly:= true;
  BorderStyle:= bsNone;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  DragMode:= dmAutomatic;
  DragOperations:= [doCopy,doMove,doLink];
  Header.Columns.Add.Text:= 'Name';
  Header.Options:= [hoAutoResize,hoColumnResize,hoDrag,hoShowSortGlyphs];
  HintMode:= hmHint;
  ShowHint:= true;
  TreeOptions.PaintOptions:= [toShowButtons,toShowDropmark,toShowRoot,toShowTreeLines, toThemeAware, toUseBlendedImages];
  TreeOptions.SelectionOptions:= [toMultiSelect,toRightClickSelect];
  TreeOptions.StringOptions:= [toSaveCaptions,toShowStaticText,toAutoAcceptEditChange];
  TreeOptions.MiscOptions:= [toEditable, toAcceptOLEDrop,toFullRepaintOnResize,toInitOnSave,toToggleOnDblClick,toWheelPanning,toEditOnClick]
end;

{-------------------------------------------------------------------------------
  Can Show DragImage
-------------------------------------------------------------------------------}
function TCEStackTree.CanShowDragImage: Boolean;
begin
  // Vista and up
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) and Assigned(SHDoDragDrop_MP) then
  Result:= False
  else // Older Windows versions
  Result:= inherited CanShowDragImage;
end;

{-------------------------------------------------------------------------------
  Check Availability of Shell items (if APIDL is assigned, then only it will be checked, else all items are checked)
-------------------------------------------------------------------------------}
procedure TCEStackTree.CheckAvailability(APIDL: PItemIDList = nil);
var
  node, chNode: PVirtualNode;
  data, chData: PCEStackItemData;
  check: Boolean;
  ws: WideString;
  pidl: PItemIDList;
begin
  node:= Self.GetFirst;
  while assigned(node) do
  begin
    data:= Self.GetNodeData(node);
    if data.ItemType = sitGroup then
    begin
      chNode:= Self.GetFirstChild(node);
      while assigned(chNode) do
      begin
        chData:= Self.GetNodeData(chNode);
        if (chData.ItemType = sitShell) then
        begin
          if assigned(chData.Namespace) then
          begin
            if assigned(APIDL) then
            check:= PIDLMgr.EqualPIDL(chData.Namespace.AbsolutePIDL, APIDL)
            else
            check:= true;

            if check then
            begin
              pidl:= CEPathToPIDL(chData.CEPath);
              if assigned(pidl) then
              begin
                chData.Offline:= false;
                PIDLMgr.FreeAndNilPIDL(pidl);
              end
              else
              begin
                chData.Offline:= true;
                chData.Namespace.Free;
                chData.Namespace:= nil;
              end;
            end;
          end
          else
          begin
            pidl:= CEPathToPIDL(chData.CEPath);
            if assigned(pidl) then
            begin
              chData.Namespace:= TNamespace.Create(PIDLMgr.CopyPIDL(pidl),nil);
              chData.Caption:= chData.Namespace.NameNormal;
              chData.Offline:= false;
              PIDLMgr.FreeAndNilPIDL(pidl);
            end
            else
            begin
              chData.Offline:= true;
            end;
          end;
        end;
        chNode:= Self.GetNextSibling(chNode);
      end;
    end;
    node:= Self.GetNextSibling(node);
  end;
end;

{-------------------------------------------------------------------------------
  Do Auto Save Active Stack
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoAutoSaveActiveStack;
begin
  if AutoSaveActiveStack and (UpdateCount = 0) then
  SaveActiveStack;
end;

{-------------------------------------------------------------------------------
  Do CancelEdit
-------------------------------------------------------------------------------}
function TCEStackTree.DoCancelEdit: Boolean;
begin
  Result:= inherited DoCancelEdit;
  Self.InvalidateNode(Self.FocusedNode);
end;

{-------------------------------------------------------------------------------
  Do CanEdit
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var
    Allowed: Boolean);
var
  data: PCEStackItemData;
begin
  data:= Self.GetNodeData(Node);
  Allowed:= (data.ItemType = sitGroup) and (Column = 0);
  inherited;
end;

{-------------------------------------------------------------------------------
  Do Collapsed
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoCollapsed(Node: PVirtualNode);
begin
  inherited;
  UpdateScrollbarSize;
end;

{-------------------------------------------------------------------------------
  Do CreateDataObject
-------------------------------------------------------------------------------}
function TCEStackTree.DoCreateDataObject: IDataObject;
var
  DataObj: TCEStackDataObject;
  NSA: TNamespaceArray;
  NSList: TList;
begin
  NSA:= nil;
  Result:= nil;
  if not Assigned(Result) then
  begin
    if (Self.SelectedCount > 0) then
    begin
      NSA:= SelectedToNamespaceArray;
      // Create IDataObject
      DataObj:= TCEStackDataObject.Create(Self, False);
      Result:= DataObj;
      fActiveDataObject:= Result;
      // Create ShellDataObject
      DataObj.ShellDataObject:= nil;
      NSList:= NamespaceToNamespaceList(NSA);
      try
        CreateFullyQualifiedShellDataObject(NSList, true,  Result);
      finally
        FreeAndNil(NSList)
      end;
    end
  end
end;

{-------------------------------------------------------------------------------
  Do DragDrop
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoDragDrop(Source: TObject; DataObject: IDataObject;
    Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer;
    Mode: TDropMode);
var
  hd: TCommonShellIDList;
  i: Integer;
  Attachmode: TVTNodeAttachMode;
  node, groupNode: PVirtualNode;
  data: PCEStackItemData;
  Nodes: TNodeArray;
begin
  inherited;
  BeginUpdate;
  try
    groupNode:= nil;
    // Get Attach mode
    case Mode of
      dmAbove:
        AttachMode := amInsertBefore;
      dmOnNode:
        AttachMode := amAddChildLast;
      dmBelow:
        AttachMode := amInsertAfter;
    else
      AttachMode := amNowhere;
    end;

    // Move items in the tree
    if Self.Dragging then
    begin
      // Add group if needed
      if Mode = dmNowhere then
      begin
        AttachMode:= amAddChildLast;
        groupNode:= InsertGroupNode(nil, amInsertAfter, '');
        node:= groupNode;
      end
      else
      begin
        node:= Self.DropTargetNode;
      end;

      data:= Self.GetNodeData(node);
      if assigned(data) and (data.ItemType = sitShell) then
      AttachMode:= amInsertBefore;

      Nodes:= self.GetSortedSelection(True);
      for I := 0 to High(Nodes) do
      begin
        if Mode = dmOnNode then
        begin
          if node.Parent = Nodes[i].Parent then
          begin
            if node.Index > Nodes[i].Index then
            AttachMode:= amInsertAfter
            else
            AttachMode:= amInsertBefore;
          end;
        end;

        data:= Self.GetNodeData(Nodes[i]);
        if data.ItemType = sitGroup then
        begin
          data:= Self.GetNodeData(node);
          if data.ItemType = sitShell then
          self.MoveTo(Nodes[I], node.Parent, AttachMode, False)
          else
          self.MoveTo(Nodes[I], node, AttachMode, False);
        end
        else
        self.MoveTo(Nodes[I], node, AttachMode, False);
      end;
    end
    // Drop items from outside
    else if assigned(DataObject) then
    begin
      Effect:= DROPEFFECT_LINK;
      hd:= TCommonShellIDList.Create;
      try
        hd.LoadFromDataObject(DataObject);
        data:= Self.GetNodeData(Self.DropTargetNode);
        // Insert group
        if (Mode = dmNowhere) then
        begin
          AttachMode:= amAddChildLast;
          groupNode:= InsertGroupNode(nil, amInsertAfter, '');
          node:= groupNode;
        end
        else if assigned(data) and (data.ItemType = sitGroup) and (Mode <> dmOnNode) then
        begin
          AttachMode:= amAddChildLast;
          if Mode = dmAbove then
          groupNode:= InsertGroupNode(Self.DropTargetNode, amInsertBefore, '')
          else
          groupNode:= InsertGroupNode(Self.DropTargetNode, amInsertAfter, '');
          node:= groupNode;
        end
        else // Make sure items are added as a child to Group
        begin
          node:= Self.DropTargetNode;
          if Mode = dmOnNode then
          begin
            data:= Self.GetNodeData(node);
            if data.ItemType = sitShell then
            AttachMode := amInsertBefore;
          end;
        end;
        // Add shell items
        for i:= 0 to hd.PIDLCount-1 do
        begin
          InsertShellNode(node, AttachMode, hd.AbsolutePIDL(i));
        end;
      finally
        hd.Free;
      end;
    end;
    RemoveEmptyGroups;
    if assigned(groupNode) then
    Self.Expanded[groupNode]:= true;
  finally
    EndUpdate;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  DoDragOver
-------------------------------------------------------------------------------}
function TCEStackTree.DoDragOver(Source: TObject; Shift: TShiftState; State:
    TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer): Boolean;
var
  node: PVirtualNode;
  data: PCEStackItemData;
  itemType: TCEStackItemType;
  isFirst: Boolean;
begin
  Result:= true;
  if Self.Dragging then
  begin
    Effect:= DROPEFFECT_MOVE;

    // Allow item move only if all selected items are same type
    itemType:= sitShell;
    isFirst:= true;
    node:= Self.GetFirstSelected;
    while assigned(node) do
    begin
      data:= Self.GetNodeData(node);
      if isFirst then
      begin
        itemType:= data.ItemType;
        isFirst:= false;
      end
      else if itemType <> data.ItemType then
      begin
        Result:= false;
        break;
      end;
      node:= Self.GetNextSelected(node);
    end;

    // Allow group move only to other groups
    if Result and (itemType = sitGroup) then
    begin
      if assigned(Self.DropTargetNode) then
      begin
        data:= Self.GetNodeData(Self.DropTargetNode);
        Result:= data.ItemType = sitGroup;
      end
      else
      Result:= false;
    end;
  end
  else
  Effect:= DROPEFFECT_LINK;
end;

{-------------------------------------------------------------------------------
  Do EndDrag
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoEndDrag(Target: TObject; X, Y: Integer);
var
  Effect: TCommonLogicalPerformedDropEffect;
begin
  inherited;

  if assigned(fActiveDataObject) then
  begin
    Effect:= TCommonLogicalPerformedDropEffect.Create;
    try
      if fActiveDataObject.QueryGetData(Effect.GetFormatEtc) = S_OK then
      begin
        Effect.LoadFromDataObject(fActiveDataObject);
        if (Effect.Action = effectMove) then
        Self.DeleteSelectedNodes; // TODO: All items might not have been moved!!!
      end;
    finally
      Effect.Free;
    end;
    fActiveDataObject:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  Do EndEdit
-------------------------------------------------------------------------------}
function TCEStackTree.DoEndEdit: Boolean;
begin
  Result:= inherited DoEndEdit;
  Self.InvalidateNode(Self.FocusedNode);
end;

{-------------------------------------------------------------------------------
  Do Expanded
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoExpanded(Node: PVirtualNode);
begin
  inherited;
  UpdateScrollbarSize
end;

{-------------------------------------------------------------------------------
  Do FreeNode
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoFreeNode(Node: PVirtualNode);
var
  data: PCEStackItemData;
begin
  data:= Self.GetNodeData(Node);
  if assigned(data.Namespace) then
  FreeAndNil(data.Namespace);
  
  inherited;
end;

{-------------------------------------------------------------------------------
  Do GetImageIndex
-------------------------------------------------------------------------------}
function TCEStackTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
    Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer):
    TCustomImageList;
var
  data: PCEStackItemData;
begin
  Result:= nil;
  if (Kind = ikState) then
  Exit;
  
  data:= Self.GetNodeData(Node);
  if data.ItemType = sitShell then
  begin
    if not data.Offline then
    begin
      Result:= SmallSysImages;
      if Kind = ikOverlay then
      Index:= data.Namespace.OverlayIconIndex
      else
      Index:= data.Namespace.GetIconIndex(false, icSmall);
      Ghosted:= false;
    end
    else
    begin
      Result:= Images;
      if Kind = ikOverlay then
      Index:= -1
      else
      Index:= fNotAvailableImageIndex;
      Ghosted:= true;
    end;
  end
  else if (data.ItemType = sitGroup) and (Kind <> ikOverlay) then
  begin
    Result:= Images;
    if Self.Expanded[Node] then
    Index:= fGroupOpenImageIndex
    else
    Index:= fGroupImageIndex;
    Ghosted:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Do GetNodeHint
-------------------------------------------------------------------------------}
function TCEStackTree.DoGetNodeHint(Node: PVirtualNode; Column: TColumnIndex;
    var LineBreakStyle: TVTTooltipLineBreakStyle): UnicodeString;
var
  data: PCEStackItemData;
  chNode: PVirtualNode;
begin
  Result:= inherited DoGetNodeHint(Node, Column, LineBreakStyle);
  data:= Self.GetNodeData(Node);
  if data.ItemType = sitShell then
  begin
    LineBreakStyle:= hlbForceSingleLine;
    if assigned(data.Namespace) then
    Result:= data.Namespace.NameForParsing;
  end
  else
  begin
    LineBreakStyle:= hlbForceMultiLine;
    Result:= data.Caption + ' (' + IntToStr(Self.ChildCount[Node]) + ' items):';
    chNode:= Self.GetFirstChild(Node);
    while assigned(chNode) do
    begin
      data:= Self.GetNodeData(chNode);
      if assigned(data.Namespace) then
      Result:= Result + #13#10 + data.Namespace.NameForParsing;
      chNode:= Self.GetNextSibling(chNode);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do GetText
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var Text: UnicodeString);
var
  data: PCEStackItemData;
  c: Integer;
begin
  data:= Self.GetNodeData(Node);
  if Column = 0 then
  begin
    if data.ItemType = sitGroup then
    begin
      if TextType = ttStatic then
      begin
        c:= Self.ChildCount[Node];
        if c = 1 then
        Text:= '(' + IntToStr(Self.ChildCount[Node]) + ' ' + _('item') + ')'
        else
        Text:= '(' + IntToStr(Self.ChildCount[Node]) + ' ' + _('items') + ')'
      end
      else
      begin
        Text:= data.Caption;
      end;
    end
    else
    begin
      if TextType = ttNormal then
      Text:= data.Caption;
    end;
  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  Do KeyAction
-------------------------------------------------------------------------------}
function TCEStackTree.DoKeyAction(var CharCode: Word; var Shift: TShiftState):
    Boolean;
begin
  Result:= inherited DoKeyAction(CharCode, Shift);
  if CharCode = VK_DELETE then
  begin
    self.DeleteSelectedNodes;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  Do NewText
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoNewText(Node: PVirtualNode; Column: TColumnIndex;
    Text: UnicodeString);
var
  data: PCEStackItemData;
begin
  inherited;
  if Column = 0 then
  begin
    data:= Self.GetNodeData(Node);
    if data.ItemType = sitGroup then
    data.Caption:= Text;
  end;
  DoAutoSaveActiveStack;
end;

{-------------------------------------------------------------------------------
  Do PaintText
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
    Column: TColumnIndex; TextType: TVSTTextType);
var
  data: PCEStackItemData;
begin
  inherited;
  data:= Self.GetNodeData(Node);
  if (TextType = ttStatic) or data.Offline then
  Canvas.Font.Color:= clGrayText
  else
  Canvas.Font.Color:= clWindowText;
end;

{-------------------------------------------------------------------------------
  Do PopupMenu
-------------------------------------------------------------------------------}
procedure TCEStackTree.DoPopupMenu(Node: PVirtualNode; Column: TColumnIndex;
    Position: TPoint);
var
  data: PCEStackItemData;
  info: THitInfo;
  p: TPoint;
begin
  Self.GetHitTestInfoAt(Position.X, Position.Y, false, info);
  if assigned(info.HitNode) and ((hiOnNormalIcon in info.HitPositions) or (hiOnItemLabel in info.HitPositions)) then
  begin
    data:= Self.GetNodeData(Node);
    if assigned(data.Namespace) then
    data.Namespace.ShowContextMenuMulti(Self, nil,nil,nil, SelectedToNamespaceArray, nil, nil, '', data.Namespace)
  end
  else if not (hiOnItemButton in info.HitPositions) then
  begin
    if assigned(BackgroundPopupMenu) then
    begin
      p:= ClientToScreen(Position);
      BackgroundPopupMenu.Popup(p.X, p.Y);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Drag And Drop
-------------------------------------------------------------------------------}
procedure TCEStackTree.DragAndDrop(AllowedEffects: Integer; DataObject:
    IDataObject; DragEffect: Integer);
var
  effect: Integer;
begin
  if SafeOperationsOnly then
  effect:= DROPEFFECT_COPY or DROPEFFECT_LINK
  else
  effect:= DROPEFFECT_COPY or DROPEFFECT_LINK or DROPEFFECT_MOVE;
  // Vista and up
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) and Assigned(SHDoDragDrop_MP) then
  SHDoDragDrop_MP(Handle, DataObject, nil, effect, DragEffect)
  else // Older Windows versions
  inherited DragAndDrop(effect, DataObject, DragEffect);
end;

{-------------------------------------------------------------------------------
  Get Stacks
-------------------------------------------------------------------------------}
function TCEStackTree.GetStacks: TCEStacks;
begin
  Result:= fStacks;
  if not assigned(Result) then
  raise Exception.CreateFmt('Stacks must be assigned before using TCEStackTree!', []);
end;


{-------------------------------------------------------------------------------
  Insert Group Node
-------------------------------------------------------------------------------}
function TCEStackTree.InsertGroupNode(ToNode: PVirtualNode; Mode:
    TVTNodeAttachMode; ACaption: WideString = ''): PVirtualNode;
var
  data, groupData: PCEStackItemData;
  groupNode: PVirtualNode;
  i: Integer;
  found: Boolean;
begin
  BeginUpdate;
  try
    Result:= InsertNode(ToNode, Mode);
    data:= GetNodeData(Result);
    data.ItemType:= sitGroup;
    // Auto create caption
    if ACaption = '' then
    begin
      i:= 0;
      repeat
        found:= false;
        i:= i + 1;
        groupNode:= Self.GetFirstChild(nil);
        while assigned(groupNode) do
        begin
          groupData:= Self.GetNodeData(groupNode);
          if groupData.Caption = (_('Group') + ' ' + IntToStr(i)) then
          begin
            found:= true;
            break;
          end;
          groupNode:= Self.GetNextSibling(groupNode);
        end;
      until not found;

      data.Caption:= _('Group') + ' ' + IntToStr(i);
    end
    else
    data.Caption:= ACaption;
  finally
    EndUpdate;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  Insert Group Node
-------------------------------------------------------------------------------}
function TCEStackTree.InsertShellNode(ToNode: PVirtualNode; Mode:
    TVTNodeAttachMode; APIDL: PItemIDList): PVirtualNode;
var
  data: PCEStackItemData;
begin
  BeginUpdate;
  try
    Result:= InsertNode(ToNode, Mode);
    data:= GetNodeData(Result);
    data.ItemType:= sitShell;
    data.Namespace:= TNamespace.Create(PIDLMgr.CopyPIDL(APIDL), nil);
    data.Caption:= data.Namespace.NameNormal;
    data.Offline:= false;
    data.CEPath:= PIDLToCEPath(APIDL);
  finally
    EndUpdate;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  Load From Stack
-------------------------------------------------------------------------------}
procedure TCEStackTree.LoadFromStack(AStack: TCEStackItem);
var
  i, i2: Integer;
  list, l: TTntStrings;
  groupNode, node: PVirtualNode;
  APIDL: PItemIDList;
  data: PCEStackItemData;
begin
  fActiveStack:= AStack;
  if not assigned(AStack) then
  Exit;

  l:= TTntStringList.Create;
  BeginUpdate;
  Self.Clear;
  try
    for i:= 0 to AStack.GroupCount - 1 do
    begin
      groupNode:= InsertGroupNode(nil, amAddChildLast, AStack.GroupName[i]);
      list:= AStack.GroupItems[i];
      for i2:= 0 to list.Count - 1 do
      begin
        if list.Strings[i2] <> '' then
        begin
          l.CommaText:= list.Strings[i2];
          if l.Count > 0 then
          begin
            APIDL:= CEPathToPIDL(l.Strings[0]);
            if assigned(APIDL) then
            begin
              node:= InsertShellNode(groupNode, amAddChildLast, APIDL);
              PIDLMgr.FreePIDL(APIDL);
            end
            else
            begin
              node:= InsertNode(groupNode, amAddChildLast);
              data:= GetNodeData(node);
              data.ItemType:= sitShell;
              data.Namespace:= nil;
              data.Offline:= true;
              data.CEPath:= l.Strings[0];
              if l.Count > 1 then
              data.Caption:= l.Strings[1]
              else
              data.Caption:= WideExtractFileName(data.CEPath, false);
            end;
          end;
        end;
      end;
    end;
  finally
    EndUpdate;
    l.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Remove Empty Groups (Result is TRUE if node(s) have been deleted)
-------------------------------------------------------------------------------}
function TCEStackTree.RemoveEmptyGroups: Boolean;
var
  node, tmpNode: PVirtualNode;
  data: PCEStackItemData;
begin
  Result:= false;
  BeginUpdate;
  try
    node:= Self.GetFirst;
    while assigned(node) do
    begin
      data:= Self.GetNodeData(node);
      if data.ItemType = sitGroup then
      begin
        tmpNode:= node;
        node:= GetNext(node);
        if not Self.HasChildren[tmpNode] then
        begin
          Self.DeleteNode(tmpNode);
          Result:= true;
        end;
      end
      else
      node:= GetNext(node);
    end;
  finally
    EndUpdate;
    DoAutoSaveActiveStack;
  end;
end;

{-------------------------------------------------------------------------------
  Save Active Stack
-------------------------------------------------------------------------------}
function TCEStackTree.SaveActiveStack: Boolean;
begin
  Result:= assigned(ActiveStack);
  if Result then
  SaveToStack(ActiveStack);
end;

{-------------------------------------------------------------------------------
  Save to Stack
-------------------------------------------------------------------------------}
procedure TCEStackTree.SaveToStack(AStack: TCEStackItem);

  procedure enumGroup(groupNode: PVirtualNode; groupItems: TTntStrings);
  var
    chNode: PVirtualNode;
    chData: PCEStackItemData;
    l: TTntStrings;
  begin
    l:= TTntStringList.Create;
    try
    chNode:= Self.GetFirstChild(groupNode);
      while assigned(chNode) do
      begin
        chData:= Self.GetNodeData(chNode);
        if assigned(chData.Namespace) then
        begin
          l.Clear;
          l.Add(PIDLToCEPath(chData.Namespace.AbsolutePIDL));
          l.Add(chData.Caption);
          groupItems.Add(l.CommaText);
        end;
        chNode:= Self.GetNextSibling(chNode);
      end;
    finally
      l.Free;
    end;
  end;

var
  node: PVirtualNode;
  data: PCEStackItemData;
  list: TTntStrings;
begin
  AStack.ClearGroups;
  node:= Self.GetFirstChild(nil);
  while assigned(node) do
  begin
    data:= Self.GetNodeData(node);
    if data.ItemType = sitGroup then
    begin
      list:= AStack.GroupItems[AStack.AddGroup(data.Caption)];
      enumGroup(node, list);
    end;
    node:= Self.GetNextSibling(node);
  end;
end;

{-------------------------------------------------------------------------------
  Selected To NamespaceArray
-------------------------------------------------------------------------------}
function TCEStackTree.SelectedToNamespaceArray: TNamespaceArray;
var
  i: Integer;
  node, chNode: PVirtualNode;
  data, chData: PCEStackItemData;
  list: TList;
begin
  list:= TList.Create;
  try
    // Create a list of selected items
    node:= Self.GetFirstSelected;
    while assigned(node) do
    begin
      data:= Self.GetNodeData(node);
      if data.ItemType = sitShell then
      begin
        if assigned(data.Namespace) then
        list.Add(Pointer(data.Namespace));
      end
      else if data.ItemType = sitGroup then
      begin
        chNode:= Self.GetFirstChild(node);
        while assigned(chNode) do
        begin
          chData:= Self.GetNodeData(chNode);
          if assigned(chData.Namespace) then
          list.Add(Pointer(chData.Namespace));
          chNode:= Self.GetNextSibling(chNode);
        end;
      end;
      node:= Self.GetNextSelected(node);
    end;
    // Create NamespaceArray
    SetLength(Result, list.Count);
    for i:= 0 to list.Count - 1 do
    begin
      Result[i]:= TNamespace(list.Items[i]);
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Update Scrollbar Size
-------------------------------------------------------------------------------}
procedure TCEStackTree.UpdateScrollbarSize;
var
  i: Integer;
  n: PVirtualNode;
begin
  i:= Self.RootNode.NodeHeight + Self.BottomSpace;
  n:= Self.GetFirstVisible;
  while assigned(n) do
  begin
    i:= i + n.NodeHeight;
    n:= Self.GetNextVisible(n);
  end;
  Self.RootNode.TotalHeight:= i;
end;

end.
