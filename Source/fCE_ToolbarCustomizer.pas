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
//  The Original Code is fCE_ToolbarCustomizer.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_ToolbarCustomizer;

interface

uses
  // CE Units
  dCE_Actions, dCE_Images,  CE_Utils, CE_Toolbar, CE_TBActions, CE_VistaFuncs,
  CE_LanguageEngine,
  // VirtualTree
  VirtualTrees,
  // Toolbar2000
  TB2Dock, TB2Toolbar, TB2Item,
  // TNT Controls
  TntActnList, TntForms,
  // Png Controls
  PngImageList,
  // SpTBXLib
  SpTBXItem, SpTBXControls,  SpTBXDkPanels,
  // JVCL
  JvSimpleXml,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ActiveX, StdCtrls, Math, TntStdCtrls;

type

  PCEActTreeData = ^TCEActTreeData;
  TCEActTreeData = record
    ActionItem: TTntAction;
    IsCategory: Boolean;
    Name: WideString;
    IconIndex: Integer;
    IsSeparator: Boolean;
  end;

  TCEToolbarCustomizer = class(TTntForm)
    ActionTree: TVirtualStringTree;
    TntLabel1: TTntLabel;
    procedure ActionTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; var Allowed: Boolean);
    procedure ActionTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject;
        DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt:
        TPoint; var Effect: Integer; Mode: TDropMode);
    procedure ActionTreeDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift:
        TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect:
        Integer; var Accept: Boolean);
    procedure ActionTreeGetImageIndexEx(Sender: TBaseVirtualTree; Node:
        PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted:
        Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
    procedure ActionTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure ActionTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure ActionTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    tmpItem: TSpTBXItem;

  public
    ParentComponent: TComponent;
    function GetCategory(CatName: String): PVirtualNode;
    procedure SetupForm;

  end;

procedure ShowCustomizer(ParentComponent: TComponent);

procedure SaveToolbarItems(Toolbar: TSpTBXToolbar; ToolbarItem:
    TJvSimpleXMLElem);

procedure LoadToolbarItems(Toolbar: TSpTBXToolbar; ToolbarItem:
    TJvSimpleXMLElem);

var
  CEToolbarCustomizer: TCEToolbarCustomizer;

implementation

uses
  Main, CE_Layout;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Show toolbar customizer.
-------------------------------------------------------------------------------}
procedure ShowCustomizer(ParentComponent: TComponent);
begin
  if not assigned(CEToolbarCustomizer) then
  begin
    CEToolbarCustomizer:= TCEToolbarCustomizer.Create(ParentComponent);
  end
  else
  begin
    if not CEToolbarCustomizer.Visible then
    CEToolbarCustomizer.Show;
    Exit;
  end;
  CEToolbarCustomizer.ParentComponent:= ParentComponent;
  CEToolbarCustomizer.SetupForm;
  CEToolbarCustomizer.Show;
  BeginToolbarCustomize;
  MainForm.TabSet.Toolbar.BeginCustomize;
end;

{*------------------------------------------------------------------------------
  Save toolbar items to XML node.
-------------------------------------------------------------------------------}
procedure SaveToolbarItems(Toolbar: TSpTBXToolbar; ToolbarItem:
    TJvSimpleXMLElem);
var
  i: Integer;
  chNode: TJvSimpleXMLElem;
  item: TTBCustomItem;
begin
  if not assigned(Toolbar) then
  Exit;
  if not assigned(ToolbarItem) then
  Exit;
  // Loop through toolbar items
  for i:= 0 to Toolbar.Items.Count - 1 do
  begin
    item:= Toolbar.Items.Items[i];

    // Normal Item
    if item is TSpTBXItem then 
    begin
      chNode:= ToolbarItem.Items.Add('item');
      if assigned(item.Action) then
      chNode.Properties.Add('action', item.Action.Name)
      else
      chNode.Properties.Add('name', item.Name);
    end
    // Separator
    else if (item.ClassType = TTBSeparatorItem) or (item.ClassType = TSpTBXSeparatorItem) then
    begin
      ToolbarItem.Items.Add('separator');
    end
    // Submenu
    else if item.ClassType = TSpTBXSubmenuItem then
    begin
      chNode:= ToolbarItem.Items.Add('submenu');
      chNode.Properties.Add('name', item.Name);
    end;    
  end;
end;

{*------------------------------------------------------------------------------
  Load toolbar items from XML node.
-------------------------------------------------------------------------------}
procedure LoadToolbarItems(Toolbar: TSpTBXToolbar; ToolbarItem:
    TJvSimpleXMLElem);
var
  i: Integer;
  chNode: TJvSimpleXMLElem;
  item: TTBCustomItem;
  itemClass: TTBCustomItemClass;
  act: TTntAction;
begin
  if not assigned(Toolbar) then
  Exit;
  if not assigned(ToolbarItem) then
  Exit;
  if not assigned(CEActions.ActionList) then
  Exit;

  if Toolbar.MenuBar or (toolbar.Tag = 1) then
  Exit;

  Toolbar.BeginUpdate;

  Toolbar.Items.Clear;
  try
    for i:= 0 to ToolbarItem.Items.Count - 1 do
    begin
      chNode:= ToolbarItem.Items.Item[i];
      // Normal Item
      if SameText(chNode.Name, 'item') then
      begin
        act:= FindAction(CEActions.ActionList, chNode.Properties.Value('action'));
        if act is TCEToolbarAction then
        itemClass:= TCEToolbarAction(act).ItemClass
        else
        itemClass:= TSpTBXItem;

        if assigned(itemClass) then
        begin
          item:= itemClass.Create(Toolbar);
          item.Action:= act;
          Toolbar.Items.Add(item);
        end;
      end
      // Separator
      else if SameText(chNode.Name, 'separator') then
      begin
        item:= TSpTBXSeparatorItem.Create(Toolbar);
        Toolbar.Items.Add(item);
      end
      // Submenu item
      else if SameText(chNode.Name, 'submenu') then
      begin
        item:= TSpTBXSubmenuItem.Create(Toolbar);
        Toolbar.Items.Add(item);
      end;
    end;
  finally
    Toolbar.EndUpdate;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.FormCreate(Sender: TObject);
begin
  SetVistaFont(Font);
  CEGlobalTranslator.TranslateComponent(Self);
  ActionTree.NodeDataSize:= SizeOf(TCEActTreeData);
  tmpItem:= TSpTBXItem.Create(self);
end;

{*------------------------------------------------------------------------------
  On Form Destroy
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.FormDestroy(Sender: TObject);
begin
  tmpItem.Free;
end;

{*------------------------------------------------------------------------------
  On Form Closed
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:= caFree;
  CEToolbarCustomizer:= nil;
  MainForm.TabSet.Toolbar.EndCustomize;
  EndToolbarCustomize;
end;

{*------------------------------------------------------------------------------
  Find Category by it's name.
-------------------------------------------------------------------------------}
function TCEToolbarCustomizer.GetCategory(CatName: String): PVirtualNode;
var
  data: PCEActTreeData;
  Node: PVirtualNode;
begin
  //Result:= nil;
  Node:= ActionTree.GetFirst;
  while Node <> nil do
  begin
    data:= ActionTree.GetNodeData(Node);
    if data.IsCategory then
    begin
      if CompareText(CatName, data.Name) = 0 then
      begin
        Result:= Node;
        Exit;
      end;
    end;
    Node:= Node.NextSibling;
  end;
  Node:= ActionTree.AddChild(nil);
  data:= ActionTree.GetNodeData(Node);
  data.IsCategory:= true;
  data.IsSeparator:= false;
  data.Name:= CatName;
  data.IconIndex:= -1;
  Result:= Node;
end;

{*------------------------------------------------------------------------------
  Setup Form
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.SetupForm;
var
  i: Integer;
  act: TTntAction;
  node, chNode: PVirtualNode;
  data: PCEActTreeData;
begin
  ActionTree.Images:= CE_Images.SmallIcons;
  ActionTree.Clear;
  for i:= 0 to CEActions.ActionList.ActionCount - 1 do
  begin
    act:= TTntAction(CEActions.ActionList.Actions[i]);
    node:= GetCategory(act.Category);
    chNode:= ActionTree.InsertNode(node, amAddChildLast);
    data:= ActionTree.GetNodeData(chNode);
    data.ActionItem:= act;
    data.Name:= act.Caption;
    data.IconIndex:= act.ImageIndex;
    data.IsCategory:= false;
    data.IsSeparator:= false;
  end;
  // Add Separator category
  node:= ActionTree.GetFirst;
  node:= ActionTree.InsertNode(node, amInsertBefore);
  data:= ActionTree.GetNodeData(Node);
  data.Name:= UTF8Encode(_('Separator'));
  data.IconIndex:= -1;
  data.IsCategory:= true;
  data.IsSeparator:= false;
  // Add Separator item
  chNode:= ActionTree.InsertNode(node, amAddChildLast);
  data:= ActionTree.GetNodeData(chNode);
  data.Name:= '---';
  data.IconIndex:= -1;
  data.IsCategory:= false;
  data.IsSeparator:= true;
  ActionTree.FullExpand;
end;

{*------------------------------------------------------------------------------
  Get node Image index.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeGetImageIndexEx(Sender:
    TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column:
    TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer; var ImageList:
    TCustomImageList);
var
  data: PCEActTreeData;
begin
  data:= ActionTree.GetNodeData(Node);
  ImageIndex:= data.IconIndex;
  if not data.IsCategory or not data.IsSeparator then
  ImageList:= CE_Images.SmallIcons
  else
  ImageList:= ActionTree.Images;
end;

{*------------------------------------------------------------------------------
  Get node text.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeGetText(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var
    CellText: WideString);
var
  data: PCEActTreeData;
begin
  if Column <> 0 then
  Exit;
  data:= ActionTree.GetNodeData(Node);
  if data.IsCategory then
  CellText:= UTF8Decode(data.Name)
  else
  CellText:= data.Name;
end;

{*------------------------------------------------------------------------------
  Get's called when text gets painted.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreePaintText(Sender: TBaseVirtualTree;
    const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType);
var
  data: PCEActTreeData;
begin
  if Column <> 0 then
  Exit;

  data:= ActionTree.GetNodeData(Node);
  if data.IsCategory then
  TargetCanvas.Font.Style:= [fsBold, fsUnderline]
  else
  TargetCanvas.Font.Style:= [];
end;

{*------------------------------------------------------------------------------
  Get's called when Drag is started.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeStartDrag(Sender: TObject; var
    DragObject: TDragObject);
var
  data: PCEActTreeData;
  item: TTBCustomItem;
  itemClass: TTBCustomItemClass;
begin
  if ActionTree.FocusedNode = nil then
  Exit;

  data:= ActionTree.GetNodeData(ActionTree.FocusedNode);
  if data.IsCategory then
  Exit;

  if data.IsSeparator then
  begin
    itemClass:= TSpTBXSeparatorItem;
  end
  else if data.ActionItem is TCEToolbarAction then
  begin
    if Assigned(TCEToolbarAction(data.ActionItem).ItemClass) then
    itemClass:= TCEToolbarAction(data.ActionItem).ItemClass
    else
    itemClass:= TCEToolbarItem;
  end
  else       
  begin
    itemClass:= TCEToolbarItem;
  end;

  item:= itemClass.Create(nil);
  item.Action:= data.ActionItem;
  tmpItem.Add(item);
  DragObject := TSpTBXItemDragObject.Create(ActionTree, item);
end;

{*------------------------------------------------------------------------------
  Get's called when determing if it's allowed to drag an item.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeDragAllowed(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
var
  data: PCEActTreeData;
begin
  data:= ActionTree.GetNodeData(Node);
  if data.IsCategory then
  Allowed:= false
  else
  Allowed:= true;
end;

{*------------------------------------------------------------------------------
  Get's called when item is dropped.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeDragDrop(Sender: TBaseVirtualTree;
    Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift:
    TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  OrigItem: TTBCustomItem;
begin
  if Assigned(Source) and (Source is TSpTBXItemDragObject) and
    (TSpTBXItemDragObject(Source).SourceControl <> Sender) then
  begin
    OrigItem := TSpTBXItemDragObject(Source).SouceItem;
    OrigItem.Parent.Remove(OrigItem);
    OrigItem.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when item is dragged over.
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeDragOver(Sender: TBaseVirtualTree;
    Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode:
    TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  Accept := Assigned(Source) and (Source is TSpTBXItemDragObject);
end;

end.
