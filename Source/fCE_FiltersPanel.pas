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
  fCE_FileView, dCE_Images, fCE_FileSearch, CE_AppSettings,
  // TB2k, TBX, SpTBX
  TB2Dock, SpTBXItem,
  // VSTools
  VirtualExplorerEasyListview,
  // Graphics32
  GR32_Image, GR32,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, Menus, TB2Item, SpTBXEditors, TB2Toolbar, StdCtrls,
  TntStdCtrls, ExtCtrls;

type
  TControlHack = class(TControl);

  TCEFiltersPanelSettings = class;

  TCEFiltersPanel = class(TCECustomDockableForm)
    Images: TBitmap32List;
    FiltersPopupMenu: TSpTBXPopupMenu;
    check_resetfilters: TSpTBXItem;
    SpTBXToolbar1: TSpTBXToolbar;
    combo_filterpattern: TSpTBXComboBox;
    TBControlItem1: TTBControlItem;
    check_wildcards: TSpTBXItem;
    FilterTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure check_resetfiltersClick(Sender: TObject);
    procedure FiltersPopupMenuPopup(Sender: TObject);
    procedure check_wildcardsClick(Sender: TObject);
    procedure combo_filterpatternSelect(Sender: TObject);
    procedure combo_filterpatternKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FilterTimerTimer(Sender: TObject);
    procedure combo_filterpatternChange(Sender: TObject);
  private
    fSettings: TCEFiltersPanelSettings;
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
  public
    Filters: TCEFilterList;
    procedure DoFormHide; override;
    procedure DoFormShow; override;
  published
    property Settings: TCEFiltersPanelSettings read fSettings write fSettings;
  end;

  TCEFiltersPanelSettings = class(TPersistent)
  private
    fAutoResetFilters: Boolean;
    function GetShowBkgrd: Boolean;
    procedure SetShowBkgrd(const Value: Boolean);
  public
    FilterPanel: TCEFiltersPanel;
  published
    property AutoResetFilters: Boolean read fAutoResetFilters write
        fAutoResetFilters;
    property ShowBkgrd: Boolean read GetShowBkgrd write SetShowBkgrd;
  end;

var
  CEFiltersPanel: TCEFiltersPanel;

implementation

uses
  Main;
{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called on Create
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.FormCreate(Sender: TObject);
begin
  inherited;
  fSettings:= TCEFiltersPanelSettings.Create;
  fSettings.FilterPanel:= Self;
  FilterBackgroundBitmap:= TBitmap.Create;
  DrawFilterBitmap;
  Filters:= TCEFilterList.Create(self);
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

  GlobalAppSettings.AddItem('FilterPanel', fSettings, true);
end;

{*------------------------------------------------------------------------------
  Get's called on Destroy
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.FormDestroy(Sender: TObject);
begin
  FilterBackgroundBitmap.Free;
  fSettings.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Draw filtering background image
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DrawFilterBitmap;
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
procedure TCEFiltersPanel.GlobalActivePageChange(OldPage, NewPage: TComponent);
begin
  if assigned(NewPage) then
  begin
    if NewPage is TCEFileViewPage then
    begin
      if Settings.AutoResetFilters then
      Filters.ClearFilters;
      Filters.ExplorerEasyListview:= TCEFileViewPage(NewPage).FileView;
    end
    else if NewPage is TCEFileSearchPage then
    begin
      if Settings.AutoResetFilters then
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
procedure TCEFiltersPanel.GlobalContentChange(Sender: TObject);
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
procedure TCEFiltersPanel.DoFormShow;
begin
  inherited;
  Filters.Active:= true;
end;

{*------------------------------------------------------------------------------
  Get's called when form gets hidden.
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.DoFormHide;
begin
  inherited;
  // TODO: make filters deactivate when panel is hidden but not when auto hidden.
  //Filters.Active:= false;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  if Settings.AutoResetFilters then
  Filters.ClearFilters;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  if Settings.AutoResetFilters then
  Filters.ClearFilters;
end;

{-------------------------------------------------------------------------------
  On check_wildcards click
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.check_wildcardsClick(Sender: TObject);
begin
  inherited;
  Filters.UseWildcards:= check_wildcards.Checked;
end;

{-------------------------------------------------------------------------------
  On combo_filterpattern Change
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.combo_filterpatternChange(Sender: TObject);
begin
  inherited;
  FilterTimer.Enabled:= true;
end;

{-------------------------------------------------------------------------------
  On combo_filterpattern KeyDown
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.combo_filterpatternKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: begin
      combo_filterpattern.Items.Insert(0, combo_filterpattern.Text);
      combo_filterpattern.ItemIndex:= 0;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On combo_filterpattern Select
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.combo_filterpatternSelect(Sender: TObject);
begin
  Filters.PatternText:= combo_filterpattern.Text;
end;

{-------------------------------------------------------------------------------
  On check_resetfilter click
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.check_resetfiltersClick(Sender: TObject);
begin
  Settings.AutoResetFilters:= not Settings.AutoResetFilters;
end;

{-------------------------------------------------------------------------------
  On FiltersPopupMenu popup
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.FiltersPopupMenuPopup(Sender: TObject);
begin
  check_resetfilters.Checked:= Settings.AutoResetFilters;
end;

{-------------------------------------------------------------------------------
  On Filter Timer
-------------------------------------------------------------------------------}
procedure TCEFiltersPanel.FilterTimerTimer(Sender: TObject);
begin
  FilterTimer.Enabled:= false;
  Filters.PatternText:= combo_filterpattern.Text;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set ShowBkgrd
-------------------------------------------------------------------------------}
function TCEFiltersPanelSettings.GetShowBkgrd: Boolean;
begin
  Result:= FilterPanel.Filters.ShowFilteringBackground;
end;
procedure TCEFiltersPanelSettings.SetShowBkgrd(const Value: Boolean);
begin
  FilterPanel.Filters.ShowFilteringBackground:= Value;
end;

end.
