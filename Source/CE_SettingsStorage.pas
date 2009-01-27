unit CE_SettingsStorage;

interface

uses
  CE_XmlStorage, CE_SettingsIntf,
  // System Units
  Windows, SysUtils, Messages, Classes;

type
  TCESettingsStorage = class(TInterfacedObject, ICESettingsStorage)
  private
    fAppStorage: TCEXmlStorage;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClosePath; stdcall;
    procedure DeletePath(Path: WideString); stdcall;
    procedure LoadFromFile(AFilePath: WideString);
    procedure OpenPath(Path: WideString); stdcall;
    function ReadBoolean(Path: WideString; Default: Boolean): Boolean; stdcall;
    function ReadInteger(Path: WideString; Default: Integer): Integer; stdcall;
    function ReadString(Path: WideString; Default: WideString): WideString; stdcall;
    function ReadPoint(Path: WideString; Default: TPoint): TPoint; stdcall;
    procedure SaveToFile(AFilePath: WideString);
    procedure WriteBoolean(Path: WideString; Value: Boolean); stdcall;
    procedure WriteInteger(Path: WideString; Value: Integer); stdcall;
    procedure WriteString(Path: WideString; Value: WideString); stdcall;
    procedure WritePoint(Path: WideString; Value: TPoint); stdcall;
    property AppStorage: TCEXmlStorage read fAppStorage;
  end;

  //TCESettingList = class

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCESettingsStorage
-------------------------------------------------------------------------------}
constructor TCESettingsStorage.Create;
begin
  inherited;
  fAppStorage:= TCEXmlStorage.Create;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCESettingsStorage
-------------------------------------------------------------------------------}
destructor TCESettingsStorage.Destroy;
begin
  fAppStorage.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Close Path
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.ClosePath;
begin
  fAppStorage.ActivePath:= '/';
end;

{-------------------------------------------------------------------------------
  Delete Path
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.DeletePath(Path: WideString);
begin
  fAppStorage.DeletePath(Path);
end;

{-------------------------------------------------------------------------------
  Load From File
--------------------------------------------------------------------------------}
procedure TCESettingsStorage.LoadFromFile(AFilePath: WideString);
begin
  fAppStorage.LoadFromFile(AFilePath);
end;

{-------------------------------------------------------------------------------
  Open Path
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.OpenPath(Path: WideString);
begin
  fAppStorage.ActivePath:= Path;
end;

{*------------------------------------------------------------------------------
  ReadBoolean
-------------------------------------------------------------------------------}
function TCESettingsStorage.ReadBoolean(Path: WideString; Default: Boolean):
    Boolean;
begin
  Result:= fAppStorage.GetValue(Path, Default);
end;

{*------------------------------------------------------------------------------
  ReadInteger
-------------------------------------------------------------------------------}
function TCESettingsStorage.ReadInteger(Path: WideString; Default: Integer):
    Integer;
begin
  Result:= fAppStorage.GetValue(Path, Default);
end;

{*------------------------------------------------------------------------------
  ReadString
-------------------------------------------------------------------------------}
function TCESettingsStorage.ReadString(Path: WideString; Default: WideString):
    WideString;
begin
  Result:= fAppStorage.GetValue(Path, Default);
end;

{*------------------------------------------------------------------------------
  ReadPoint
-------------------------------------------------------------------------------}
function TCESettingsStorage.ReadPoint(Path: WideString; Default: TPoint):
    TPoint;
begin
  Result:= fAppStorage.GetValue(Path, Default);
end;

{-------------------------------------------------------------------------------
  Save to File
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.SaveToFile(AFilePath: WideString);
begin
  fAppStorage.SaveToFile(AFilePath);
end;

{*------------------------------------------------------------------------------
  WriteBoolean
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.WriteBoolean(Path: WideString; Value: Boolean);
begin
  fAppStorage.SetValue(Path, Value);
end;

{*------------------------------------------------------------------------------
  WriteInteger
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.WriteInteger(Path: WideString; Value: Integer);
begin
  fAppStorage.SetValue(Path, Value);
end;

{*------------------------------------------------------------------------------
  WriteString
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.WriteString(Path: WideString; Value: WideString);
begin
  fAppStorage.SetValue(Path, Value);
end;

{*------------------------------------------------------------------------------
  WritePoint
-------------------------------------------------------------------------------}
procedure TCESettingsStorage.WritePoint(Path: WideString; Value: TPoint);
begin
  fAppStorage.SetValue(Path, Value);
end;

{##############################################################################}


end.
