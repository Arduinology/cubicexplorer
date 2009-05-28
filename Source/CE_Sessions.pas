unit CE_Sessions;

interface

uses
  // CE Units
  CE_AppSettings,
  // System Units
  Classes, SysUtils, StrUtils;

type
  TSessionTabsList = class(TCECustomSettingStorage)
  
  end;

  TSessionItem = class(TObject)
  private
    fSessionName: WideString;
    fTabs: TSessionTabsList;
  published
    property SessionName: WideString read fSessionName write fSessionName;
    property Tabs: TSessionTabsList read fTabs write fTabs;
  end;

  TCESessionList = class(TCECustomSettingStorage)
  end;

implementation

end.
