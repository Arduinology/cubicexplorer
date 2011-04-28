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
  CE_AppSettings,
  // SpTBX
  TB2Dock, SpTBXItem, 
  // VSTools
  MPCommonObjects, EasyListview, MPCommonUtilities,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, TB2Item;

type
  TControlHack = class(TControl);

  TCEStackPanelSettings = class;

  TCEStackPanel = class(TCECustomDockableForm)
    DropStackPopup: TSpTBXPopupMenu;
    but_clearlist: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    but_safetyswitch: TSpTBXItem;
    procedure FormCreate(Sender: TObject);
    procedure but_clearlistClick(Sender: TObject);
    procedure DropStackPopupPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure but_safetyswitchClick(Sender: TObject);
  private
    fSettings: TCEStackPanelSettings;
    { Private declarations }
  public
    StackTree: TCEStackTree;
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
