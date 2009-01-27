unit fCE_ExtAppPage;

interface

uses
  // CE Units
  CE_AppPanel, fCE_TabPage,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TCEExtAppTabPage = class(TCECustomTabPage)
  private
  public
    ExtApp: TCEAppEmbedPanel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectPage; override;
    procedure UpdateCaption; override;
  end;

implementation

{$R *.dfm}

{*------------------------------------------------------------------------------
  Create an instance of TCEExtAppTabPage
-------------------------------------------------------------------------------}
constructor TCEExtAppTabPage.Create(AOwner: TComponent);
begin
  inherited;
  Layout:= 'ExternalApp';
  ExtApp:= TCEAppEmbedPanel.Create(self);
  ExtApp.Parent:= self;
  ExtApp.Align:= alClient;
end;

{*------------------------------------------------------------------------------
  Destroy TCEExtAppTabPage
-------------------------------------------------------------------------------}
destructor TCEExtAppTabPage.Destroy;
begin
  ExtApp.Close;
  inherited;
end;

{*------------------------------------------------------------------------------
  Select page
-------------------------------------------------------------------------------}
procedure TCEExtAppTabPage.SelectPage;
begin
  ExtApp.ResizeApp;
end;

{*------------------------------------------------------------------------------
  Update Tab item Caption
-------------------------------------------------------------------------------}
procedure TCEExtAppTabPage.UpdateCaption;
begin
  TabCaption:= ExtApp.GetAppWndText;
end;

end.
