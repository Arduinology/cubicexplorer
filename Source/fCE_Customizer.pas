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
//  The Original Code is fCE_Customizer.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_Customizer;

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
  SpTBXItem, SpTBXControls,  SpTBXDkPanels, SpTBXTabs, SpTBXEditors, SpTBXSkins,
  // JVCL
  JvSimpleXml,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ActiveX, StdCtrls, Math, TntStdCtrls, CheckLst, TntCheckLst;

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
    TabControl: TSpTBXTabControl;
    tab_toolbars: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    tab_buttons: TSpTBXTabItem;
    SpTBXTabSheet2: TSpTBXTabSheet;
    SpTBXPanel1: TSpTBXPanel;
    but_close: TSpTBXButton;
    tab_theme: TSpTBXTabItem;
    SpTBXTabSheet3: TSpTBXTabSheet;
    tab_hotkeys: TSpTBXTabItem;
    SpTBXTabSheet4: TSpTBXTabSheet;
    ThemeList: TSpTBXListBox;
    but_loadTheme: TSpTBXButton;
    ToolbarList: TSpTBXCheckListBox;
    group_displayMode: TSpTBXRadioGroup;
    check_largeIcons: TSpTBXCheckBox;
    check_borders: TSpTBXCheckBox;
    check_stretch: TSpTBXCheckBox;
    check_dragHandle: TSpTBXCheckBox;
    label_help: TSpTBXLabel;
    label_themeName: TSpTBXLabel;
    label_themeAuthor: TSpTBXLabel;
    procedure ActionTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2:
        PVirtualNode; Column: TColumnIndex; var Result: Integer);
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
    procedure but_closeClick(Sender: TObject);
    procedure but_loadThemeClick(Sender: TObject);
    procedure check_bordersClick(Sender: TObject);
    procedure check_dragHandleClick(Sender: TObject);
    procedure check_largeIconsClick(Sender: TObject);
    procedure check_stretchClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure group_displayModeClick(Sender: TObject);
    procedure TabControlActiveTabChange(Sender: TObject; TabIndex: Integer);
    procedure ThemeListClick(Sender: TObject);
    procedure ToolbarListClick(Sender: TObject);
  private
    fselectedToolbar: TSpTBXToolbar;
    tmpItem: TSpTBXItem;
    procedure SetselectedToolbar(const Value: TSpTBXToolbar);

  protected
    property selectedToolbar: TSpTBXToolbar read fselectedToolbar write
        SetselectedToolbar;
  public
    ParentComponent: TComponent;
    function GetCategory(CatName: String): PVirtualNode;
    procedure SetupForm;

  end;

procedure ShowCustomizer(ParentComponent: TComponent);

procedure SaveToolbarItems(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);

procedure LoadToolbarItems(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);

procedure LoadToolbarProperties(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);

procedure SaveToolbarProperties(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);

var
  CEToolbarCustomizer: TCEToolbarCustomizer;

implementation

uses
  Main, CE_Layout, CE_DriveBar;

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
end;

{*------------------------------------------------------------------------------
  Save toolbar items to XML node.
-------------------------------------------------------------------------------}
procedure SaveToolbarItems(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);
var
  i: Integer;
  chNode: TJvSimpleXMLElem;
  item: TTBCustomItem;
begin
  if not assigned(Toolbar) then
  Exit;
  if not assigned(ToolbarNode) then
  Exit;
  // Loop through toolbar items
  for i:= 0 to Toolbar.Items.Count - 1 do
  begin
    item:= Toolbar.Items.Items[i];

    // Normal Item
    if item is TSpTBXItem then 
    begin
      chNode:= ToolbarNode.Items.Add('item');
      if assigned(item.Action) then
      chNode.Properties.Add('action', item.Action.Name)
      else
      chNode.Properties.Add('name', item.Name);
    end
    // Separator
    else if (item.ClassType = TTBSeparatorItem) or (item.ClassType = TSpTBXSeparatorItem) then
    begin
      ToolbarNode.Items.Add('separator');
    end
    // Submenu
    else if item.ClassType = TSpTBXSubmenuItem then
    begin
      chNode:= ToolbarNode.Items.Add('submenu');
      chNode.Properties.Add('name', item.Name);
    end;    
  end;
end;

{*------------------------------------------------------------------------------
  Load toolbar items from XML node.
-------------------------------------------------------------------------------}
procedure LoadToolbarItems(Toolbar: TSpTBXToolbar; ToolbarNode:
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
  if not assigned(ToolbarNode) then
  Exit;
  if not assigned(CEActions.ActionList) then
  Exit;

  if Toolbar.MenuBar or (toolbar.Tag = 1) then
  Exit;

  Toolbar.BeginUpdate;

  Toolbar.Items.Clear;
  try
    for i:= 0 to ToolbarNode.Items.Count - 1 do
    begin
      chNode:= ToolbarNode.Items.Item[i];
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

{-------------------------------------------------------------------------------
  Load toolbar properties from XML node.
-------------------------------------------------------------------------------}
procedure LoadToolbarProperties(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);
var
  i: Integer;
begin
  if not assigned(Toolbar) or not assigned(ToolbarNode) then
  Exit;

  toolbar.BeginUpdate;
  try
    // Display Mode
    i:= ToolbarNode.Properties.IntValue('DisplayMode', -1);
    if (i > -1) and (i < 4) then
    Toolbar.DisplayMode:= TSpTBXToolbarDisplayMode(i);
    if Toolbar.DisplayMode = tbdmImageAboveCaption then
    Toolbar.Options:= [tboImageAboveCaption];

    // Large Icons
    if ToolbarNode.Properties.BoolValue('LargeIcons', Toolbar.Images = CE_Images.MediumIcons) then
    Toolbar.Images:= CE_Images.MediumIcons
    else
    Toolbar.Images:= CE_Images.SmallIcons;

    // Borders
    if ToolbarNode.Properties.BoolValue('Borders', Toolbar.BorderStyle <> bsNone) then
    Toolbar.BorderStyle:= bsSingle
    else
    Toolbar.BorderStyle:= bsNone;

    // Stretch
    Toolbar.Stretch:= ToolbarNode.Properties.BoolValue('Stretch', Toolbar.Stretch);

    // Drag Handle
    if ToolbarNode.Properties.BoolValue('DragHandle', Toolbar.DragHandleStyle <> dhNone) then
    Toolbar.DragHandleStyle:= dhSingle
    else
    Toolbar.DragHandleStyle:= dhNone;
  finally
    toolbar.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Save toolbar properties to XML node.
-------------------------------------------------------------------------------}
procedure SaveToolbarProperties(Toolbar: TSpTBXToolbar; ToolbarNode:
    TJvSimpleXMLElem);
begin
  if not assigned(Toolbar) or not assigned(ToolbarNode) then
  Exit;

  ToolbarNode.Properties.Clear;
  ToolbarNode.Properties.Add('DisplayMode', Ord(toolbar.DisplayMode));
  ToolbarNode.Properties.Add('LargeIcons', Toolbar.Images = CE_Images.MediumIcons);
  ToolbarNode.Properties.Add('Borders', Toolbar.BorderStyle <> bsNone);
  ToolbarNode.Properties.Add('Stretch', Toolbar.Stretch);
  ToolbarNode.Properties.Add('DragHandle', Toolbar.DragHandleStyle <> dhNone);
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.FormCreate(Sender: TObject);
var
  i, index: Integer;
  toolbar: TSpTBXToolbar;
begin
  SetVistaFont(Font);
  CEGlobalTranslator.TranslateComponent(Self);
  ActionTree.NodeDataSize:= SizeOf(TCEActTreeData);
  tmpItem:= TSpTBXItem.Create(self);

  // Populate Theme list
  SkinManager.SkinsList.GetSkinNames(ThemeList.Items.AnsiStrings);
  ThemeList.Sorted:= true;
  i:= ThemeList.Items.IndexOf('Default');
  if i > -1 then ThemeList.Items.Move(i, 0);
  ThemeList.ItemIndex:= ThemeList.Items.IndexOf(SkinManager.CurrentSkinName);
  label_themeName.Caption:= SkinManager.CurrentSkin.SkinName;
  if SkinManager.CurrentSkin.SkinAuthor <> '' then
  label_themeAuthor.Caption:= _('Author') + ': ' + SkinManager.CurrentSkin.SkinAuthor
  else
  label_themeAuthor.Caption:= '';

  // Populate Toolbar list
  for i:= 0 to CELayoutItems.Count - 1 do
  begin
    if CELayoutItems.Items[i] is TSpTBXToolbar then
    begin
      toolbar:= TSpTBXToolbar(CELayoutItems.Items[i]);
      index:= ToolbarList.Items.AddObject(toolbar.Caption, toolbar);
      ToolbarList.Checked[index]:= toolbar.Visible;
    end;
  end;

  // Populate group_displayMode
  group_displayMode.Items.Add(_('Default'));
  group_displayMode.Items.Add(_('Icon only'));
  group_displayMode.Items.Add(_('Icon above text'));
  group_displayMode.Items.Add(_('Text only'));

  but_loadTheme.Caption:= CEActions.act_view_loadskin.Caption;
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
  if MainForm.TabSet.Toolbar.IsCustomizing then
  begin
    MainForm.TabSet.Toolbar.EndCustomize;
    EndToolbarCustomize;
  end;
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
        break;
      end;
    end;
    Node:= Node.NextSibling;
  end;

  if not assigned(Node) then
  begin
    Node:= ActionTree.AddChild(nil);
    data:= ActionTree.GetNodeData(Node);
    data.IsCategory:= true;
    data.IsSeparator:= false;
    data.Name:= CatName;
    data.IconIndex:= -1;
  end;

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
    chNode:= ActionTree.AddChild(node);
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
  chNode:= ActionTree.AddChild(node);
  data:= ActionTree.GetNodeData(chNode);
  data.Name:= '---';
  data.IconIndex:= -1;
  data.IsCategory:= false;
  data.IsSeparator:= true;

  // Sort actions
  node:= ActionTree.GetFirst;
  while node <> nil do
  begin
    data:= ActionTree.GetNodeData(Node);
    if data.IsCategory then
    begin
      ActionTree.Sort(node, 0, sdAscending, false);
    end;
    node:= node.NextSibling;
  end;
  
  ActionTree.FullExpand;
end;

{-------------------------------------------------------------------------------
  On ActionTree.CompareNodes
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ActionTreeCompareNodes(Sender: TBaseVirtualTree;
    Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  data1, data2: PCEActTreeData;
begin
  data1:= ActionTree.GetNodeData(Node1);
  data2:= ActionTree.GetNodeData(Node2);
  Result:= WideCompareText(data1.Name, data2.Name);
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

{-------------------------------------------------------------------------------
  On but_close Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.but_closeClick(Sender: TObject);
begin
  Self.Close;
end;

{-------------------------------------------------------------------------------
  On but_loadTheme Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.but_loadThemeClick(Sender: TObject);
var
  i: Integer;
begin
  CEActions.act_view_loadskin.Execute;
  // Populate Theme list
  SkinManager.SkinsList.GetSkinNames(ThemeList.Items.AnsiStrings);
  ThemeList.Sorted:= true;
  i:= ThemeList.Items.IndexOf('Default');
  if i > -1 then ThemeList.Items.Move(i, 0);
  ThemeList.ItemIndex:= ThemeList.Items.IndexOf(SkinManager.CurrentSkinName);
  label_themeName.Caption:= SkinManager.CurrentSkin.SkinName;
  if SkinManager.CurrentSkin.SkinAuthor <> '' then
  label_themeAuthor.Caption:= _('Author') + ': ' + SkinManager.CurrentSkin.SkinAuthor
  else
  label_themeAuthor.Caption:= '';
end;

{-------------------------------------------------------------------------------
  On TabControl.ActiveTabChange
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.TabControlActiveTabChange(Sender: TObject;
    TabIndex: Integer);
begin
  if (TabControl.ActiveTab = tab_buttons) and not MainForm.TabSet.Toolbar.IsCustomizing then
  begin
    BeginToolbarCustomize;
    MainForm.TabSet.Toolbar.BeginCustomize;
  end
  else if MainForm.TabSet.Toolbar.IsCustomizing then
  begin
    MainForm.TabSet.Toolbar.EndCustomize;
    EndToolbarCustomize;
  end;

  if TabControl.ActiveTab = tab_buttons then
  label_help.Caption:= _('Drag buttons to/from toolbars')
  else
  label_help.Caption:= '';
end;

{-------------------------------------------------------------------------------
  On ThemeList Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ThemeListClick(Sender: TObject);
begin
  if ThemeList.ItemIndex > -1 then
  begin
    SkinManager.SetSkin(ThemeList.Items.Strings[ThemeList.ItemIndex]);
    label_themeName.Caption:= SkinManager.CurrentSkin.SkinName;
    if SkinManager.CurrentSkin.SkinAuthor <> '' then
    label_themeAuthor.Caption:= _('Author') + ': ' + SkinManager.CurrentSkin.SkinAuthor
    else
    label_themeAuthor.Caption:= '';
  end
  else
  begin
    label_themeName.Caption:= '';
    label_themeAuthor.Caption:= '';
  end;
end;

{-------------------------------------------------------------------------------
  Set selectedToolbar
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.SetselectedToolbar(const Value: TSpTBXToolbar);
begin
  if Value <> fselectedToolbar then
  begin
    fselectedToolbar:= Value;
    if assigned(fselectedToolbar) then
    begin
      // Display Mode
      group_displayMode.ItemIndex:= Ord(selectedToolbar.DisplayMode);
      group_displayMode.Enabled:= true;
      // Large Icons
      check_largeIcons.Checked:= selectedToolbar.Images = CE_Images.MediumIcons;
      check_largeIcons.Enabled:= true;
      // Borders
      check_borders.Checked:= selectedToolbar.BorderStyle = bsSingle;
      check_borders.Enabled:= true;
      // Stretch
      check_stretch.Checked:= selectedToolbar.Stretch;
      check_stretch.Enabled:= true;
      // Drag Handle
      check_dragHandle.Checked:= selectedToolbar.DragHandleStyle <> dhNone;
      check_dragHandle.Enabled:= true;
    end
    else
    begin
      group_displayMode.Enabled:= false;
      check_largeIcons.Enabled:= false;
      check_borders.Enabled:= false;
      check_stretch.Enabled:= false;
      check_dragHandle.Enabled:= false;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On ToolbarList Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.ToolbarListClick(Sender: TObject);
begin
  if ToolbarList.ItemIndex > -1 then
  begin
    selectedToolbar:= TSpTBXToolbar(ToolbarList.Items.Objects[ToolbarList.ItemIndex]);
    selectedToolbar.Visible:= ToolbarList.Checked[ToolbarList.ItemIndex];
  end
  else
  selectedToolbar:= nil;
end;

{-------------------------------------------------------------------------------
  On group_displayMode.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.group_displayModeClick(Sender: TObject);
begin
  if assigned(selectedToolbar) then
  begin
    if group_displayMode.ItemIndex > -1 then
    begin
      selectedToolbar.BeginUpdate;
      selectedToolbar.DisplayMode:= TSpTBXToolbarDisplayMode(group_displayMode.ItemIndex);
      if selectedToolbar.DisplayMode = tbdmImageAboveCaption then
      selectedToolbar.Options:= [tboImageAboveCaption];
      selectedToolbar.EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On check_largeIcons.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.check_largeIconsClick(Sender: TObject);
begin
  if assigned(selectedToolbar) then
  begin
    if check_largeIcons.Checked then
    selectedToolbar.Images:= CE_Images.MediumIcons
    else
    selectedToolbar.Images:= CE_Images.SmallIcons;
  end;
end;

{-------------------------------------------------------------------------------
  On check_borders.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.check_bordersClick(Sender: TObject);
begin
  if assigned(selectedToolbar) then
  begin
    if check_borders.Checked then
    selectedToolbar.BorderStyle:= bsSingle
    else
    selectedToolbar.BorderStyle:= bsNone;
  end;
end;

{-------------------------------------------------------------------------------
  On check_stretch.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.check_stretchClick(Sender: TObject);
begin
  if assigned(selectedToolbar) then
  selectedToolbar.Stretch:= check_stretch.Checked;
end;

{-------------------------------------------------------------------------------
  On check_dragHandle.Click
-------------------------------------------------------------------------------}
procedure TCEToolbarCustomizer.check_dragHandleClick(Sender: TObject);
begin
  if assigned(selectedToolbar) then
  begin
    if check_dragHandle.Checked then
    selectedToolbar.DragHandleStyle:= dhSingle
    else
    selectedToolbar.DragHandleStyle:= dhNone;
  end;
end;

end.
