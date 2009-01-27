unit CE_SettingsIntf;

interface

uses
  Windows;

type
  ICESettingsStorage = interface(IInterface)
  ['{310E498B-2643-451C-8DB1-364F8D53405C}']
    procedure ClosePath; stdcall;
    procedure DeletePath(Path: WideString); stdcall;
    procedure OpenPath(Path: WideString); stdcall;
    function ReadBoolean(Path: WideString; Default: Boolean): Boolean; stdcall;
    function ReadInteger(Path: WideString; Default: Integer): Integer; stdcall;
    function ReadPoint(Path: WideString; Default: TPoint): TPoint; stdcall;
    function ReadString(Path: WideString; Default: WideString): WideString; stdcall;
    procedure WriteBoolean(Path: WideString; Value: Boolean); stdcall;
    procedure WriteInteger(Path: WideString; Value: Integer); stdcall;
    procedure WritePoint(Path: WideString; Value: TPoint); stdcall;
    procedure WriteString(Path: WideString; Value: WideString); stdcall;
  end;
  
  ICESettingsHandler = interface(IInterface)
  ['{4EC9F1B1-3E44-40A9-8810-76500D48DE83}']
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
  end;

implementation

end.
