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
//  The Original Code is fCE_FiltersPanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_FiltersPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_FilterPanel, CE_VistaFuncs, CE_GlobalCtrl,
  fCE_FileView, dCE_Images, fCE_FileSearch, CE_SettingsIntf, CE_Settings,
  // TB2k, TBX, SpTBX
  TB2Dock, SpTBXItem,
  // VSTools
  VirtualExplorerEasyListview,
  // Graphics32
  GR32_Image, GR32,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, Menus, TB2Item;

type
  TControlHack = class(TControl);

  TCE_FiltersPanel = class(TCECustomDockableForm, ICESettingsHandler)
    Images: TBitmap32List;
    FiltersPopupMenu: TSpTBXPopupMenu;
    check_resetfilters: TSpTBXItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure check_resetfiltersClick(Sender: TObject);
    procedure FiltersPopupMenuPopup(Sender: TObject);
  private
    fResetOnPathChange: Boolean;
  protected
    FilterBackgroundBitmap: TBitmap;
    procedure DrawFilterBitmap;
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); override;
        stdcall;
    procedure GlobalContentChange(Sender: TObject); override; stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
  public
    Filters: TCEFilterPanel;
    procedure DoFormHide; override;
    procedure DoFormShow; override;
    property ResetOnPathChange: Boolean read fResetOnPathChange write
        fResetOnPathChange;
  end;

var
  CEFiltersPanel: TCE_FiltersPanel;

implementation

uses
  Main;
{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called on Create
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.FormCreate(Sender: TObject);
begin
  inherited;
  FilterBackgroundBitmap:= TBitmap.Create;
  DrawFilterBitmap;
  Filters:= TCEFilterPanel.Create(self);
  Filters.Parent:= self;
  Filters.Align:= alClient;
  Filters.FilteringImage:= FilterBackgroundBitmap;
  Filters.PopupMenu:= FiltersPopupMenu;
  SetDesktopIconFonts(Filters.Font);
  ImageList:= CE_Images.SmallIcons;
  ImageIndex:= 29;
  GlobalFocusCtrl.CtrlList.Add(Filters);
  TControlHack(Filters).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  GlobalPathCtrl.RegisterNotify(self);
  GlobalSettings.RegisterHandler(Self);
  fResetOnPathChange:= true;
end;

{*------------------------------------------------------------------------------
  Get's called on Destroy
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.FormDestroy(Sender: TObject);
begin
  FilterBackgroundBitmap.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Draw filtering background image
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.DrawFilterBitmap;
var
  b: TBitmap32;
begin
  b:= TBitmap32.Create;
  try
    b.SetSizeFrom(Images.Bitmap[0]);
    b.DrawMode:= dmBlend;
    b.Clear(Color32(clWindow));
    b.Draw(0,0, Images.Bitmap[0]);
    FilterBackgroundBitmap.SetSize(b.Width, b.Height);
    b.DrawTo(FilterBackgroundBitmap.Canvas.Handle,0,0);
  finally
    b.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when Active page has changed
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.GlobalActivePageChange(OldPage, NewPage: TComponent);
begin
  if assigned(NewPage) then
  begin
    if NewPage is TCEFileViewPage then
    begin
      if ResetOnPathChange then
      Filters.ClearFilters;
      Filters.ExplorerEasyListview:= TCEFileViewPage(NewPage).FileView;
    end
    else if NewPage is TCEFileSearchPage then
    begin
      if ResetOnPathChange then
      Filters.ClearFilters;
      Filters.ExplorerEasyListview:= TCEFileSearchPage(NewPage).Results;
    end
    else
    begin
      Filters.ExplorerEasyListview:= nil;
      Filters.Clear;
    end;
  end
  else
  begin
    Filters.ExplorerEasyListview:= nil;
    Filters.Clear;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when global content has changed
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.GlobalContentChange(Sender: TObject);
begin
  if Filters.Active then
  begin
    Filters.PopulateTree;
    Filters.DoFiltering;
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when form gets shown.
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.DoFormShow;
begin
  inherited;
  Filters.Active:= true;
end;

{*------------------------------------------------------------------------------
  Get's called when form gets hidden.
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.DoFormHide;
begin
  inherited;
  // TODO: make filters deactivate when panel is hidden but not when auto hidden.
  //Filters.Active:= false;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  if ResetOnPathChange then
  Filters.ClearFilters;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  if ResetOnPathChange then
  Filters.ClearFilters;
end;

{-------------------------------------------------------------------------------
  Load From storage
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.LoadFromStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('/FilterPanel');
  try
    // Toggles
    Filters.ShowFilteringBackground:= Storage.ReadBoolean('ShowBkgrd', true);
    ResetOnPathChange:= Storage.ReadBoolean('AutoResetFilters', true);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Save to storage
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.SaveToStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('/FilterPanel');
  try
    // Toggles
    Storage.WriteBoolean('ShowBkgrd', Filters.ShowFilteringBackground);
    Storage.WriteBoolean('AutoResetFilters', ResetOnPathChange);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  On check_resetfilter click
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.check_resetfiltersClick(Sender: TObject);
begin
  ResetOnPathChange:= not ResetOnPathChange;
end;

{-------------------------------------------------------------------------------
  On FiltersPopupMenu popup
-------------------------------------------------------------------------------}
procedure TCE_FiltersPanel.FiltersPopupMenuPopup(Sender: TObject);
begin
  check_resetfilters.Checked:= ResetOnPathChange;
end;

end.
