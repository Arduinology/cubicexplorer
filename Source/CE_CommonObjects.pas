unit CE_CommonObjects;

interface

uses
  // VSTools
  MPShellUtilities, MPCommonObjects,
  // System Units
  ShlObj, ShellAPI, Windows, Contnrs, Classes, SysUtils;

type
  TCESpecialNamespaces = class(TObject)  
  private
    fPIDLList: TList;
  protected
    procedure ClearList;
    procedure PopulateList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSpecialID(APIDL: PItemIDList): Integer;
  end;

var
  CE_SpecialNamespaces: TCESpecialNamespaces;

implementation

{*------------------------------------------------------------------------------
   Create an instance of TCESpecialNamespaces
-------------------------------------------------------------------------------}
constructor TCESpecialNamespaces.Create;
begin
  inherited;
  fPIDLList:= TList.Create;
  PopulateList;
end;

{*------------------------------------------------------------------------------
  Destroy TCESpecialNamespaces
-------------------------------------------------------------------------------}
destructor TCESpecialNamespaces.Destroy;
begin
  ClearList;
  fPIDLList.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Populate list with Special Folder PIDLs
-------------------------------------------------------------------------------}
procedure TCESpecialNamespaces.PopulateList;
var
  PIDL: PItemIDList;
  FolderID: Integer;
begin
  for FolderID:= 0 to 63 do
  begin
    SHGetspecialFolderLocation(0, FolderID, PIDL);
    fPIDLList.Add(PIDL);
  end;
end;

{*------------------------------------------------------------------------------
  Clear list and free PIDLs
-------------------------------------------------------------------------------}
procedure TCESpecialNamespaces.ClearList;
var
  i: Integer;
begin
  for i:= 0 to fPIDLList.Count-1 do
  begin
    if assigned(fPIDLList.Items[i]) then
    PidlMgr.FreePIDL(fPIDLList.Items[i]);
  end;
  fPIDLList.Clear;
end;

{*------------------------------------------------------------------------------
  Get Special Folder's ID. If not found, -1 is returned.
-------------------------------------------------------------------------------}
function TCESpecialNamespaces.GetSpecialID(APIDL: PItemIDList): Integer;
var
  i: Integer;
begin
  Result:= -1;
  if not assigned(APIDL) then
  Exit;
  
  for i:= 0 to fPIDLList.Count - 1 do
  begin
    if assigned(fPIDLList.Items[i]) then
    begin
      if ILIsEqual(APIDL, fPIDLList.Items[i]) then
      begin
        Result:= i;
        break;
      end;
    end;
  end;
end;

{##############################################################################}

initialization
  CE_SpecialNamespaces:= TCESpecialNamespaces.Create;

finalization
  FreeAndNil(CE_SpecialNamespaces);

end.
