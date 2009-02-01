unit CE_TBActions;

interface

uses
  // TB2k, TBX, SpTBX
  TB2Item, SpTBXItem,
  // Tnt Control
  TntActnList,
  // System Units
  ActnList;

type

  TCEToolbarAction = class(TTntAction)
  public
    ItemClass: TTBCustomItemClass;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterActions('CubicExplorer', [TCEToolbarAction], nil);
end;

end.
