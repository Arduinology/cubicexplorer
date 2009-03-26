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
  fCE_DockableForm, CE_DropStack, CE_GlobalCtrl, dCE_Images, CE_VistaFuncs,
  CE_SettingsIntf, CE_Settings,
  // SpTBX
  TB2Dock, SpTBXItem, 
  // VSTools
  MPCommonObjects, EasyListview, VirtualExplorerEasyListview, MPCommonUtilities,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, TB2Item;

type
  TControlHack = class(TControl);

  TCEStackPanel = class(TCECustomDockableForm, ICESettingsHandler)
    DropStackPopup: TSpTBXPopupMenu;
    but_clearlist: TSpTBXItem;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    viewstyle_1: TSpTBXItem;
    viewstyle_2: TSpTBXItem;
    viewstyle_4: TSpTBXItem;
    viewstyle_3: TSpTBXItem;
    viewstyle_5: TSpTBXItem;
    viewstyle_6: TSpTBXItem;
    procedure FormCreate(Sender: TObject);
    procedure but_clearlistClick(Sender: TObject);
    procedure ViewStyleClick(Sender: TObject);
    procedure DropStackPopupPopup(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    DropStack: TCEDropStack;
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
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
  DropStack:= TCEDropStack.Create(self);
  DropStack.Parent:= Self;
  DropStack.Align:= alClient;
  DropStack.BorderStyle:= bsNone;
  DropStack.View:= elsReport;
  DropStack.DragManager.MouseButton:= [cmbLeft,cmbRight];
  DropStack.PopupMenu:= DropStackPopup;
  DropStack.Selection.MouseButton:= [cmbLeft,cmbRight];

  TopDock.Name:= 'StackPanel_TopDock';
  BottomDock.Name:= 'StackPanel_BottomDock';
  Caption:= 'Drop Stack';
  GlobalFocusCtrl.CtrlList.Add(DropStack);
  TControlHack(DropStack).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  GlobalSettings.RegisterHandler(self);
end;

{-------------------------------------------------------------------------------
  Gets called when TCEStackPanel is destroyed
-------------------------------------------------------------------------------}
procedure TCEStackPanel.FormDestroy(Sender: TObject);
begin
  GlobalSettings.UnRegisterHandler(Self);
  inherited;
end;

{-------------------------------------------------------------------------------
  ViewStyleClick
-------------------------------------------------------------------------------}
procedure TCEStackPanel.ViewStyleClick(Sender: TObject);
begin
  case TSpTBXItem(Sender).Tag of
    0: DropStack.View:= elsIcon;
    1: DropStack.View:= elsSmallIcon;
    2: DropStack.View:= elsList;
    3: DropStack.View:= elsReport;
    4: DropStack.View:= elsTile;
    5: DropStack.View:= elsThumbnail;
  end;
end;

{-------------------------------------------------------------------------------
  On DropStackPopup popup
-------------------------------------------------------------------------------}
procedure TCEStackPanel.DropStackPopupPopup(Sender: TObject);
begin
  case DropStack.View of
    elsIcon: viewstyle_1.Checked:= true;
    elsSmallIcon: viewstyle_2.Checked:= true;
    elsList: viewstyle_3.Checked:= true;
    elsReport: viewstyle_4.Checked:= true;
    elsTile: viewstyle_5.Checked:= true;
    elsThumbnail: viewstyle_6.Checked:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Clear List
-------------------------------------------------------------------------------}
procedure TCEStackPanel.but_clearlistClick(Sender: TObject);
begin
  DropStack.Items.Clear;
end;

{-------------------------------------------------------------------------------
  Load Settings
-------------------------------------------------------------------------------}
procedure TCEStackPanel.LoadFromStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('StackPanel');
  try
    DropStack.View:= TEasyListStyle(Storage.ReadInteger('ViewStyle', 2));
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Save Settings
-------------------------------------------------------------------------------}
procedure TCEStackPanel.SaveToStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('StackPanel');
  try
    Storage.WriteInteger('ViewStyle', Ord(DropStack.View));
  finally
    Storage.ClosePath;
  end;
end;

end.
