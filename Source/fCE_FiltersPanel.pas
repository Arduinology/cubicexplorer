unit fCE_FiltersPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_FilterPanel, CE_VistaFuncs, CE_GlobalCtrl,
  fCE_FileView, dCE_Images, fCE_FileSearch, CE_SettingsIntf, CE_Settings,
  // TB2k, TBX, SpTBX
  TB2Dock, SpTBXItem,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualExplorerEasyListview, GR32_Image, GR32;

type
  TControlHack = class(TControl);

  TCE_FiltersPanel = class(TCECustomDockableForm, ICESettingsHandler)
    Images: TBitmap32List;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  protected
    FilterBackgroundBitmap: TBitmap;
    procedure DrawFilterBitmap;
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); override;
        stdcall;
    procedure GlobalContentChange(Sender: TObject); override; stdcall;
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
  public
    Filters: TCEFilterPanel;
    procedure DoFormHide; override;
    procedure DoFormShow; override;
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
  SetDesktopIconFonts(Filters.Font);
  ImageList:= CE_Images.SmallIcons;
  ImageIndex:= 29;
  GlobalFocusCtrl.CtrlList.Add(Filters);
  TControlHack(Filters).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  GlobalPathCtrl.RegisterNotify(self);
  GlobalSettings.RegisterHandler(Self);
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
      Filters.ExplorerEasyListview:= TCEFileViewPage(NewPage).FileView;
    end
    else if NewPage is TCEFileSearchPage then
    begin
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
  Filters.Active:= false;
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
  finally
    Storage.ClosePath;
  end;
end;

end.
