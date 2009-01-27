unit CE_AppStorage;

interface

uses
  // JVCL
  JvAppXMLStorage, JvAppStorage,
  // System Units
  Windows, SysUtils, Messages, Classes;

type
  TCEAppStorage = class(TJvAppXMLFileStorage)
  protected
    function GetPhysicalReadOnly: Boolean; override;
  end;

implementation

{*------------------------------------------------------------------------------
  Fix bug in Windows 98.
-------------------------------------------------------------------------------}
function TCEAppStorage.GetPhysicalReadOnly: Boolean;
begin
  if Self.FileName = '' then
  Result:= false
  else
  Result:= FPhysicalReadOnly;
end;

end.
