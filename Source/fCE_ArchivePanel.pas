unit fCE_ArchivePanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_ArchiveTree,
  // SpTbx
  TB2Dock, SpTBXItem,
  // VSTools
  MPCommonUtilities,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TCEArchiverPanel = class(TCECustomDockableForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
  public
    Browser: TCEArchiveTree;
  end;

var
  CEArchiverPanel: TCEArchiverPanel;

implementation

uses
  CE_GlobalCtrl;

{$R *.dfm}

{-------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCEArchiverPanel.FormCreate(Sender: TObject);
begin
  inherited;
  Browser:= TCEArchiveTree.Create(Self);
  Browser.Parent:= Self;
  Browser.Align:= alClient;
  
  GlobalPathCtrl.RegisterNotify(self);
end;

{*------------------------------------------------------------------------------
  Get's called when Global focus has changed
-------------------------------------------------------------------------------}
procedure TCEArchiverPanel.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
  if not Self.IsVisible then
  Exit;
  
  try
    if WideFileExists(NewPath) then
    Browser.OpenArchive(NewPath);
  except
  end;
end;

end.
