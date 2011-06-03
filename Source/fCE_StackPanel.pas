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
//  The Original Code is fCE_StackPanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_StackPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_Stacks, CE_StackTree, CE_GlobalCtrl, dCE_Images, CE_VistaFuncs,
  CE_AppSettings, CE_Toolbar, dCE_Actions,
  // SpTBX
  TB2Dock, SpTBXItem, TB2Item, TB2Toolbar, SpTBXEditors, SpTBXSkins,
  // VSTools
  MPCommonObjects, EasyListview, MPCommonUtilities,
  // Tnt
  TntClasses,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, VirtualTrees, ImgList;

type
  TControlHack = class(TControl);

  TCEStackPanelSettings = class;

  TCEStackStartupType = (stEmpty, stLastUsed, stUserSelected);

  TCEStackPanel = class(TCECustomDockableForm)
    DropStackPopup: TSpTBXPopupMenu;
    but_clearlist: TSpTBXItem;
    StackToolbar: TCEToolbar;
    sub_load: TSpTBXSubmenuItem;
    sub_save: TSpTBXSubmenuItem;
    item_safe_operations: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    item_clear_list: TSpTBXItem;
    item_remove: TSpTBXItem;
    procedure FormCreate(Sender: TObject);
    procedure DropStackPopupPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sub_loadPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure sub_savePopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure item_safe_operationsClick(Sender: TObject);
    procedure item_clear_listClick(Sender: TObject);
    procedure item_removeClick(Sender: TObject);
  private
    fSettings: TCEStackPanelSettings;
    { Private declarations }
  protected
    fStackPaths: TTntStrings;
    procedure HandleSafeOperationsOnlyChange(Sender: TObject);
    procedure HandleStackLoadClick(Sender: TObject);
    procedure HandleStackSaveClick(Sender: TObject);
    procedure HandleStackTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure HandleStackTreeStructureChange(Sender: TBaseVirtualTree; Node:
        PVirtualNode; Reason: TChangeReason);
  public
    StackTree: TCEStackTree;
    procedure DoFormShow; override;
    function FindStackNames(AList: TTntStrings): Integer;
    procedure LoadStartupStack;
    procedure PopulateStackSaveMenuItem(AItem: TTBCustomItem; ShowAutoSaveItem:
        Boolean = false);
    property Settings: TCEStackPanelSettings read fSettings write fSettings;
  end;

  TCEStackPanelSettings = class(TPersistent)
  private
    fLoadOnStartup: WideString;
    fStartupType: TCEStackStartupType;
    function GetAutoCollapse: Boolean;
    function GetAutoExpand: Boolean;
    function GetAutoSaveStack: Boolean;
    function GetFullExpandOnLoad: Boolean;
    function GetMaxHintItemCount: Integer;
    function GetSafeOperationsOnly: Boolean;
    function GetShowWarnings: Boolean;
    procedure SetAutoCollapse(const Value: Boolean);
    procedure SetAutoExpand(const Value: Boolean);
    procedure SetAutoSaveStack(const Value: Boolean);
    procedure SetFullExpandOnLoad(const Value: Boolean);
    procedure SetMaxHintItemCount(const Value: Integer);
    procedure SetSafeOperationsOnly(const Value: Boolean);
    procedure SetShowWarnings(const Value: Boolean);
  public
    StackPanel: TCEStackPanel;
  published
    property AutoCollapse: Boolean read GetAutoCollapse write SetAutoCollapse;
    property AutoExpand: Boolean read GetAutoExpand write SetAutoExpand;
    property LoadOnStartup: WideString read fLoadOnStartup write fLoadOnStartup;
    property AutoSaveStack: Boolean read GetAutoSaveStack write SetAutoSaveStack;
    property FullExpandOnLoad: Boolean read GetFullExpandOnLoad write
        SetFullExpandOnLoad;
    property StartupType: TCEStackStartupType read fStartupType write fStartupType;
    property MaxHintItemCount: Integer read GetMaxHintItemCount write
        SetMaxHintItemCount;
    property SafeOperationsOnly: Boolean read GetSafeOperationsOnly write
        SetSafeOperationsOnly;
    property ShowWarnings: Boolean read GetShowWarnings write SetShowWarnings;
  end;

var
  CEStackPanel: TCEStackPanel;

implementation

uses
  CE_LanguageEngine, fCE_SaveSessionDlg, CE_LanguageUtils;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Gets called when TCEStackPanel is created
-------------------------------------------------------------------------------}
procedure TCEStackPanel.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  // Properties
  TopDock.Name:= 'StackPanel_TopDock';
  BottomDock.Name:= 'StackPanel_BottomDock';
  Caption:= 'Stack';
  // StackTree
  StackTree:= TCEStackTree.Create(self);
  StackTree.Parent:= Self;
  StackTree.Align:= alClient;
  StackTree.BackgroundPopupMenu:= DropStackPopup;
  StackTree.Images:= CE_Images.BookmarkImages;
  StackTree.GroupImageIndex:= 6;
  StackTree.GroupOpenImageIndex:= 6;
  StackTree.NotAvailableImageIndex:= 4;
  StackTree.BottomSpace:= 6;
  StackTree.OnSafeOperationsOnlyChange:= HandleSafeOperationsOnlyChange;
  StackTree.OnChange:= HandleStackTreeChange;
  StackTree.OnStructureChange:= HandleStackTreeStructureChange;
  HandleSafeOperationsOnlyChange(Self);
  // Focus control
  GlobalFocusCtrl.CtrlList.Add(StackTree);
  TControlHack(StackTree).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  // Settings
  fSettings:= TCEStackPanelSettings.Create;
  fSettings.StackPanel:= Self;
  GlobalAppSettings.AddItem('StackPanel', fSettings, true);
  // Add dynamic spacer to toolbar
  i:= StackToolbar.Items.IndexOf(item_safe_operations);
  if i > 0 then
  StackToolbar.Items.Insert(i, TCEToolbarDynamicSpacerItem.Create(StackToolbar.Items));

  fStackPaths:= TTntStringList.Create;
end;

{-------------------------------------------------------------------------------
  Gets called when TCEStackPanel is destroyed
-------------------------------------------------------------------------------}
procedure TCEStackPanel.FormDestroy(Sender: TObject);
begin
  if assigned(StackTree.ActiveStack) then
  begin
    StackTree.ActiveStack.Free;
    StackTree.ActiveStack:= nil;
  end;
  fStackPaths.Free;
  fSettings.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  On sub_load Popup
-------------------------------------------------------------------------------}
procedure TCEStackPanel.sub_loadPopup(Sender: TTBCustomItem; FromLink: Boolean);
var
  i: Integer;
  item: TSpTBXItem;
  ws: WideString;
begin
  Sender.Clear;

  FindStacks(StackDirPath, fStackPaths);

  for i:= 0 to fStackPaths.Count - 1 do
  begin
    item:= TSpTBXItem.Create(Sender);
    ws:= fStackPaths.Strings[i];
    item.Caption:= WideExtractFileName(ws, true);
    item.Tag:= i;
    item.OnClick:= HandleStackLoadClick;
    item.Checked:= assigned(StackTree.ActiveStack) and (StackTree.ActiveStack.StackPath = ws);
    item.Images:= CE_Images.SmallIcons;
    item.ImageIndex:= 43;
    Sender.Add(item);
  end;
end;

{-------------------------------------------------------------------------------
  On sub_save Popup
-------------------------------------------------------------------------------}
procedure TCEStackPanel.sub_savePopup(Sender: TTBCustomItem; FromLink: Boolean);
begin
  PopulateStackSaveMenuItem(Sender, true);
end;

{-------------------------------------------------------------------------------
  On DropStackPopup popup
-------------------------------------------------------------------------------}
procedure TCEStackPanel.DropStackPopupPopup(Sender: TObject);
begin
  //
end;

{-------------------------------------------------------------------------------
  Do Form Show
-------------------------------------------------------------------------------}
procedure TCEStackPanel.DoFormShow;
begin
  inherited;
  StackToolbar.Realign;
end;

{-------------------------------------------------------------------------------
  Find Stack Names
-------------------------------------------------------------------------------}
function TCEStackPanel.FindStackNames(AList: TTntStrings): Integer;
var
  i: Integer;
begin
  if not assigned(AList) then
  begin
    Result:= -1;
    Exit;
  end;
  
  FindStacks(StackDirPath, fStackPaths);
  Result:= fStackPaths.Count;
  AList.Clear;
  for i:= 0 to fStackPaths.Count - 1 do
  begin
    AList.Add(WideExtractFileName(fStackPaths.Strings[i], true));
  end;
end;

{-------------------------------------------------------------------------------
  On OnSafeOperationsOnlyChange
-------------------------------------------------------------------------------}
procedure TCEStackPanel.HandleSafeOperationsOnlyChange(Sender: TObject);
begin
  item_safe_operations.Checked:= not StackTree.SafeOperationsOnly;
end;

{-------------------------------------------------------------------------------
  On Stack Load item Click
-------------------------------------------------------------------------------}
procedure TCEStackPanel.HandleStackLoadClick(Sender: TObject);
var
  i: Integer;
  ws: WideString;
begin
  i:= TSpTBXItem(Sender).Tag;
  if (i > -1) and (i < fStackPaths.Count) then
  begin
    ws:= fStackPaths.Strings[i];
    // Free previous Stack
    if assigned(StackTree.ActiveStack) then
    begin
      StackTree.ActiveStack.Free;
      StackTree.ActiveStack:= nil;
    end;
    // Create new stack
    StackTree.ActiveStack:= TCEStackItem.Create;
    StackTree.ActiveStack.StackName:= WideExtractFileName(ws, true);
    StackTree.ActiveStack.LoadFromFile(ws);
    StackTree.LoadFromStack(StackTree.ActiveStack);
    if Settings.StartupType = stLastUsed then
    Settings.LoadOnStartup:= StackTree.ActiveStack.StackName;
  end;
end;

{-------------------------------------------------------------------------------
  On Stack Save Click
-------------------------------------------------------------------------------}
procedure TCEStackPanel.HandleStackSaveClick(Sender: TObject);
var
  id, i: Integer;
  dlg: TCESaveSessionDlg;
  stack: TCEStackItem;
  ws, ws2: WideString;
  p: TPoint;
begin
  id:= TSpTBXItem(Sender).Tag;
  // Save As...
  if id = -1 then
  begin
    dlg:= TCESaveSessionDlg.Create(Self); // Borrow Save Session Dialog
    try
      dlg.Caption:= _('Save Stack');
      dlg.TntLabel1.Caption:= _('Stack Name');
      dlg.SessionCombo.Clear;
      for i:= 0 to fStackPaths.Count - 1 do
      dlg.SessionCombo.Items.Add(WideExtractFileName(fStackPaths.Strings[i], true));
      dlg.SessionCombo.ItemIndex:= -1;
      dlg.SessionCombo.Text:= '';
      dlg.but_save.OnClick:= nil;
      dlg.Position:= poDesigned;
      p:= StackTree.ClientToScreen(Point(0,0));
      dlg.Left:= p.X;
      dlg.Top:= p.Y;
      // Save stack
      if dlg.ShowModal = mrOK then
      begin
        if dlg.SessionCombo.ItemIndex > -1 then
        begin
          ws2:= _('You are about to save over existing stack.')+#13+#10+
                _('Are you sure you want to do that?');
          ws:= _('Confirm Stack Save');
          if (WideMessageBox(Self.Handle, ws, ws2, MB_ICONWARNING or MB_YESNO) <> idYes) then
          Exit;

          if assigned(StackTree.ActiveStack) and
            (StackTree.ActiveStack.StackPath = fStackPaths.Strings[dlg.SessionCombo.ItemIndex]) then
          stack:= StackTree.ActiveStack
          else
          begin
            if assigned(StackTree.ActiveStack) then
            begin
              StackTree.ActiveStack.Free;
              StackTree.ActiveStack:= nil;
            end;
            stack:= TCEStackItem.Create;
            stack.StackPath:= fStackPaths.Strings[dlg.SessionCombo.ItemIndex];
            stack.StackName:= WideExtractFileName(stack.StackPath, true);
          end;
        end
        else
        begin
          if assigned(StackTree.ActiveStack) then
          begin
            StackTree.ActiveStack.Free;
            StackTree.ActiveStack:= nil;
          end;
          stack:= TCEStackItem.Create;
          stack.StackPath:= StackDirPath + dlg.SessionCombo.Text + '.stk';
          stack.StackName:= dlg.SessionCombo.Text;
        end;
        StackTree.SaveToStack(stack);
        stack.SaveToFile(stack.StackPath);
        StackTree.ActiveStack:= stack;
      end;
    finally
      dlg.Free;
    end;
  end
  // Auto Save option
  else if id = -2 then
  begin
    StackTree.AutoSaveActiveStack:= not StackTree.AutoSaveActiveStack;
  end
  // Save to existing Stack
  else if (id > -1) and (id < fStackPaths.Count) then
  begin
    
    ws2:= _('You are about to save over existing stack.')+#13+#10+
         _('Are you sure you want to do that?');
    ws:= _('Confirm Stack Save');
    if (WideMessageBox(Self.Handle, ws, ws2, MB_ICONWARNING or MB_YESNO) = idYes) then
    begin
      ws:= fStackPaths.Strings[id];
      if assigned(StackTree.ActiveStack) then
      begin
        StackTree.ActiveStack.Free;
        StackTree.ActiveStack:= nil;
      end;
      stack:= TCEStackItem.Create;
      stack.StackPath:= ws;
      stack.StackName:= WideExtractFileName(ws, true);
      StackTree.SaveToStack(stack);
      stack.SaveToFile(stack.StackPath);
      StackTree.ActiveStack:= stack;
    end;
  end; 
end;

{-------------------------------------------------------------------------------
  Handle StackTree.OnChange
-------------------------------------------------------------------------------}
procedure TCEStackPanel.HandleStackTreeChange(Sender: TBaseVirtualTree; Node:
    PVirtualNode);
begin
  item_remove.Enabled:= StackTree.SelectedCount > 0;
end;

{-------------------------------------------------------------------------------
  Handle StackTree.OnStructureChange
-------------------------------------------------------------------------------}
procedure TCEStackPanel.HandleStackTreeStructureChange(Sender:
    TBaseVirtualTree; Node: PVirtualNode; Reason: TChangeReason);
begin
  item_clear_list.Enabled:= StackTree.RootNode.ChildCount > 0;
  item_remove.Enabled:= StackTree.SelectedCount > 0;
end;

{-------------------------------------------------------------------------------
  On item_safe_operations Click
-------------------------------------------------------------------------------}
procedure TCEStackPanel.item_safe_operationsClick(Sender: TObject);
begin
  StackTree.SafeOperationsOnly:= not StackTree.SafeOperationsOnly;
end;

{-------------------------------------------------------------------------------
  On item_clear_list.Click
-------------------------------------------------------------------------------}
procedure TCEStackPanel.item_clear_listClick(Sender: TObject);
begin
  if Settings.ShowWarnings then
  begin
    if TaskDialog(Self.Handle,
                  _('Clear Stack'),
                  _('Removing all items from Stack!'),
                  _('Are you sure you want to clear the list?'),
                  TD_ICON_QUESTION,
                  TD_BUTTON_YES + TD_BUTTON_NO) = TD_RESULT_YES then
    begin
      StackTree.Clear;
    end;
  end
  else
  StackTree.Clear;
end;

{-------------------------------------------------------------------------------
  On item_remove.Click
-------------------------------------------------------------------------------}
procedure TCEStackPanel.item_removeClick(Sender: TObject);
begin
  StackTree.DeleteSelectedNodes;
end;

{-------------------------------------------------------------------------------
  Load Startup Stack
-------------------------------------------------------------------------------}
procedure TCEStackPanel.LoadStartupStack;
var
  i: integer;
  ws: WideString;
begin
  if (Settings.StartupType <> stEmpty) and (Settings.LoadOnStartup <> '') then
  begin
    FindStacks(StackDirPath, fStackPaths);
    for i:= 0 to fStackPaths.Count - 1 do
    begin
      ws:= WideExtractFileName(fStackPaths.Strings[i], true);
      if ws = Settings.LoadOnStartup then
      begin
        StackTree.ActiveStack:= TCEStackItem.Create;
        StackTree.ActiveStack.StackName:= ws;
        StackTree.ActiveStack.LoadFromFile(fStackPaths.Strings[i]);
        StackTree.LoadFromStack(StackTree.ActiveStack);
        break;
      end;     
    end;
  end;       
end;

{-------------------------------------------------------------------------------
  Populate Stack Save MenuItem
-------------------------------------------------------------------------------}
procedure TCEStackPanel.PopulateStackSaveMenuItem(AItem: TTBCustomItem;
    ShowAutoSaveItem: Boolean = false);
var
  i: Integer;
  item: TSpTBXItem;
  ws: WideString;
begin
  AItem.Clear;
  // Save As...
  item:= TSpTBXItem.Create(AItem);
  item.Caption:= _('Save As...');
  item.Tag:= -1;
  item.OnClick:= HandleStackSaveClick;
  item.Images:= CE_Images.MiscImages;
  item.ImageIndex:= 6;
  AItem.Add(item);
  // Separator
  AItem.Add(TSpTBXSeparatorItem.Create(AItem));
  // Stack Items ->
  FindStacks(StackDirPath, fStackPaths);
  for i:= 0 to fStackPaths.Count - 1 do
  begin
    item:= TSpTBXItem.Create(AItem);
    ws:= fStackPaths.Strings[i];
    item.Caption:= WideExtractFileName(ws, true);
    item.Checked:= assigned(StackTree.ActiveStack) and (StackTree.ActiveStack.StackPath = ws);
    item.Tag:= i;
    item.OnClick:= HandleStackSaveClick;
    item.Images:= CE_Images.SmallIcons;
    item.ImageIndex:= 43;
    AItem.Add(item);
  end;
  // Auto Save Item
  if ShowAutoSaveItem then
  begin
    // Separator
    AItem.Add(TSpTBXSeparatorItem.Create(AItem));
    // Auto Save
    item:= TSpTBXItem.Create(AItem);
    item.Caption:= _('Enable Auto Save');
    item.Tag:= -2;
    item.Checked:= StackTree.AutoSaveActiveStack;
    item.OnClick:= HandleStackSaveClick;
    AItem.Add(item);
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set AutoCollapse
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetAutoCollapse: Boolean;
begin
  Result:= StackPanel.StackTree.AutoCollapse;
end;
procedure TCEStackPanelSettings.SetAutoCollapse(const Value: Boolean);
begin
  StackPanel.StackTree.AutoCollapse:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AutoExpand
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetAutoExpand: Boolean;
begin
  Result:= StackPanel.StackTree.AutoExpand;
end;
procedure TCEStackPanelSettings.SetAutoExpand(const Value: Boolean);
begin
  StackPanel.StackTree.AutoExpand:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AutoSaveStack
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetAutoSaveStack: Boolean;
begin
  Result:= StackPanel.StackTree.AutoSaveActiveStack;
end;
procedure TCEStackPanelSettings.SetAutoSaveStack(const Value: Boolean);
begin
  StackPanel.StackTree.AutoSaveActiveStack:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set MaxHintItemCount
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetMaxHintItemCount: Integer;
begin
  Result:= StackPanel.StackTree.MaxHintItemCount;
end;
procedure TCEStackPanelSettings.SetMaxHintItemCount(const Value: Integer);
begin
  StackPanel.StackTree.MaxHintItemCount:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set SafeOperationsOnly
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetSafeOperationsOnly: Boolean;
begin
  Result:= StackPanel.StackTree.SafeOperationsOnly;
end;
procedure TCEStackPanelSettings.SetSafeOperationsOnly(const Value: Boolean);
begin
  StackPanel.StackTree.SafeOperationsOnly:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set ShowWarnings
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetShowWarnings: Boolean;
begin
  Result:= StackPanel.StackTree.ShowWarnings;
end;
procedure TCEStackPanelSettings.SetShowWarnings(const Value: Boolean);
begin
  StackPanel.StackTree.ShowWarnings:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set FullExpandOnLoad
-------------------------------------------------------------------------------}
function TCEStackPanelSettings.GetFullExpandOnLoad: Boolean;
begin
  Result:= StackPanel.StackTree.FullExpandOnLoad;
end;
procedure TCEStackPanelSettings.SetFullExpandOnLoad(const Value: Boolean);
begin
  StackPanel.StackTree.FullExpandOnLoad:= Value;
end;

end.
