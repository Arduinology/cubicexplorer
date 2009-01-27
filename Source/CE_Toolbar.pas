unit CE_Toolbar;

interface

uses
  // TB2K, TBX, SpTBX
  SpTBXItem, TB2Item,
  // System Units
  Classes, Windows, SysUtils, Controls, Messages, TntActnList;

type
  TCEToolbarItem = class(TSpTBXItem)
  end;

  TCEToolbarSubmenuItem = class(TCEToolbarItem)
  private
    function GetDropdownCombo: Boolean;
    procedure SetDropdownCombo(Value: Boolean);
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DropdownCombo: Boolean read GetDropdownCombo write SetDropdownCombo
        default False;
  end;

  TCEToolbar = class(TSpTBXToolbar)
  private
  protected
  public
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEToolbarSubmenuItem
-------------------------------------------------------------------------------}
constructor TCEToolbarSubmenuItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisSubitemsEditable,tbisCombo];
end;

{*------------------------------------------------------------------------------
  DoPopup
-------------------------------------------------------------------------------}
procedure TCEToolbarSubmenuItem.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
begin
  // Do nothing?
end;

{*------------------------------------------------------------------------------
  GetDropdownCombo
-------------------------------------------------------------------------------}
function TCEToolbarSubmenuItem.GetDropdownCombo: Boolean;
begin
  Result := tbisCombo in ItemStyle;
end;

{*------------------------------------------------------------------------------
  SetDropdownCombo
-------------------------------------------------------------------------------}
procedure TCEToolbarSubmenuItem.SetDropdownCombo(Value: Boolean);
begin
  if (tbisCombo in ItemStyle) <> Value then begin
    if Value then ItemStyle := ItemStyle + [tbisCombo]
    else ItemStyle := ItemStyle - [tbisCombo];
    Change(True);
  end;
end;

end.
