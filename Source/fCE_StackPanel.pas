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
  CE_AppSettings, CE_Toolbar,
  // SpTBX
  TB2Dock, SpTBXItem, TB2Item, TB2Toolbar, SpTBXEditors,
  // VSTools
  MPCommonObjects, EasyListview, MPCommonUtilities,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus;

type
  TControlHack = class(TControl);

  TCEStackPanelSettings = class;

  TCEStackPanel = class(TCECustomDockableForm)
    DropStackPopup: TSpTBXPopupMenu;
    but_clearlist: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    but_safetyswitch: TSpTBXItem;
    StackToolbar: TCEToolbar;
    sub_load: TSpTBXSubmenuItem;
    sub_save: TSpTBXSubmenuItem;
    procedure FormCreate(Sender: TObject);
    procedure but_clearlistClick(Sender: TObject);
    procedure DropStackPopupPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure but_safetyswitchClick(Sender: TObject);
    procedure sub_loadPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure sub_savePopup(Sender: TTBCustomItem; FromLink: Boolean);
  private
    fSettings: TCEStackPanelSettings;
    { Private declarations }
  protected
    procedure HandleStackLoadClick(Sender: TObject);
    procedure HandleStackSaveClick(Sender: TObject);
  public
    StackTree: TCEStackTree;
    procedure PopulateStackSaveMenuItem(AItem: TTBCustomItem; ShowAutoSaveItem:
        Boolean = false);
    property Settings: TCEStackPanelSettings read fSettings write fSettings;
  end;

  TCEStackPanelSettings = class(TPersistent)
  private
    function GetSafeOperationsOnly: Boolean;
    procedure SetSafeOperationsOnly(const Value: Boolean);
  public
    StackPanel: TCEStackPanel;
  published
    property SafeOperationsOnly: Boolean read GetSafeOperationsOnly write
        SetSafeOperationsOnly;
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
  StackTree.GroupImageIndex:= 5;
  StackTree.GroupOpenImageIndex:= 5;
  StackTree.NotAvailableImageIndex:= 4;
  StackTree.BottomSpace:= 6;
  // Focus control
  GlobalFocusCtrl.CtrlList.Add(StackTree);
  TControlHack(StackTree).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  // Settings
  fSettings:= TCEStackPanelSettings.Create;
  fSettings.StackPanel:= Self;
  GlobalAppSettings.AddItem('StackPanel', fSettings, true);
end;

{-------------------------------------------------------------------------------
  Gets called when TCEStackPanel is destroyed
-------------------------------------------------------------------------------}
procedure TCEStackPanel.FormDestroy(Sender: TObject);
begin
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
begin
  Sender.Clear;
  for i:= 0 to GlobalStacks.Count - 1 do
  begin
    item:= TSpTBXItem.Create(Sender);
    item.Caption:= GlobalStacks.Items[i].StackName;
    item.OnClick:= HandleStackLoadClick;
    item.Tag:= i;
    item.Checked:= StackTree.ActiveStack = GlobalStacks.Items[i];
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
  but_safetyswitch.Checked:= StackTree.SafeOperationsOnly;
end;

{-------------------------------------------------------------------------------
  On but_safetyswitch Click
-------------------------------------------------------------------------------}
procedure TCEStackPanel.but_safetyswitchClick(Sender: TObject);
begin
  StackTree.SafeOperationsOnly:= not StackTree.SafeOperationsOnly;
end;


{-------------------------------------------------------------------------------
  Clear List
-------------------------------------------------------------------------------}
procedure TCEStackPanel.but_clearlistClick(Sender: TObject);
begin
  StackTree.Clear;
end;

{-------------------------------------------------------------------------------
  On Stack Load item Click
-------------------------------------------------------------------------------}
procedure TCEStackPanel.HandleStackLoadClick(Sender: TObject);
var
  i: Integer;
  stack: TCEStackItem;
begin
  i:= TSpTBXItem(Sender).Tag;
  if (i > -1) and (i < GlobalStacks.Count) then
  begin
    stack:= GlobalStacks.Items[i];
    StackTree.LoadFromStack(stack);
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
      for i:= 0 to GlobalStacks.Count - 1 do
      dlg.SessionCombo.Items.Add(GlobalStacks.Items[i].StackName);
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

          stack:= GlobalStacks.Items[dlg.SessionCombo.ItemIndex];
        end
        else
        stack:= GlobalStacks.AddStack(dlg.SessionCombo.Text);

        StackTree.SaveToStack(stack);
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
  else if (id > -1) and (id < GlobalStacks.Count) then
  begin
    stack:= GlobalStacks.Items[id];
    ws2:= _('You are about to save over existing stack.')+#13+#10+
         _('Are you sure you want to do that?');
    ws:= _('Confirm Stack Save');
    if (WideMessageBox(Self.Handle, ws, ws2, MB_ICONWARNING or MB_YESNO) = idYes) then
    begin
      StackTree.SaveToStack(stack);
      StackTree.ActiveStack:= stack;
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
  for i:= 0 to GlobalStacks.Count - 1 do
  begin
    item:= TSpTBXItem.Create(AItem);
    item.Caption:= GlobalStacks.Items[i].StackName;
    item.Tag:= i;
    item.Checked:= StackTree.ActiveStack = GlobalStacks.Items[i];
    item.OnClick:= HandleStackSaveClick;
    item.Images:= CE_Images.SmallIcons;
    item.ImageIndex:= 43;
    AItem.Add(item);
  end;

  if ShowAutoSaveItem then
  begin
    // Separator
    AItem.Add(TSpTBXSeparatorItem.Create(AItem));
    // Auto Save
    item:= TSpTBXItem.Create(AItem);
    item.Caption:= 'Enable Auto Save';
    item.Tag:= -2;
    item.Checked:= StackTree.AutoSaveActiveStack;
    item.OnClick:= HandleStackSaveClick;
    AItem.Add(item);
  end;
end;

{##############################################################################}

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

end.
