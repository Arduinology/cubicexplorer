unit fCE_QuickViewTab;

interface

uses
  // CE Units
  fCE_TabPage, CE_QuickView, CE_LanguageEngine, dCE_Images, CE_Utils,
  CE_GlobalCtrl, 
  // VSTools
  MPCommonUtilities, MPCommonObjects,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCEQuickViewPage = class(TCECustomTabPage)
  private
    fActiveFile: WideString;
    { Private declarations }
  public
    Viewer: TCEQuickView;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenFile(AFilePath: WideString);
    procedure SelectPage; override;
    function TabClosing: Boolean; override;
    procedure UpdateCaption; override;
    property ActiveFile: WideString read fActiveFile write fActiveFile;
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEQuickViewPage
-------------------------------------------------------------------------------}
constructor TCEQuickViewPage.Create(AOwner: TComponent);
begin
  inherited;
  Viewer:= TCEQuickView.Create(nil);
  Viewer.Parent:= Self;
  Viewer.Align:= alClient;
  Layout:= 'QuickView';
end;

{-------------------------------------------------------------------------------
  Destroy TCEQuickViewPage
-------------------------------------------------------------------------------}
destructor TCEQuickViewPage.Destroy;
begin
  Viewer.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Open File
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.OpenFile(AFilePath: WideString);
begin
  if WideFileExists(AFilePath) then
  begin
    ActiveFile:= AFilePath;
    UpdateCaption;
    Application.ProcessMessages;
    Viewer.AutoLoadFile(AFilePath);
  end
  else
  begin
    ActiveFile:= '';
    UpdateCaption;
  end;
end;

{*------------------------------------------------------------------------------
  Select Page
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.SelectPage;
begin
  GlobalPathCtrl.ActivePage:= Self;
  GlobalPathCtrl.GlobalPathCaption:= ActiveFile;
end;

{*------------------------------------------------------------------------------
  Get's called when tab is closing.
-------------------------------------------------------------------------------}
function TCEQuickViewPage.TabClosing: Boolean;
begin
  Result:= true;
  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.ActivePage:= nil;
end;

{*------------------------------------------------------------------------------
  Update Tab item Caption
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.UpdateCaption;
begin
  if ActiveFile = '' then
  begin
    TabCaption:= _('Loading...');
    TabItem.Images:= SmallSysImages;
    TabItem.ImageIndex:= 0;
  end
  else
  begin
    TabCaption:= WideExtractFileName(ActiveFile);
    TabItem.Images:= SmallSysImages;
    TabItem.ImageIndex:= GetIconIndex(ActiveFile);
  end;

  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.GlobalPathCaption:= ActiveFile;
end;

end.
