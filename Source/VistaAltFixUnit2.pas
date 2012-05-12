unit VistaAltFixUnit2;

interface

uses
  Windows, Classes;

type
  TVistaAltFix2 = class(TComponent)
  private
    FInstalled: Boolean;
    function VistaWithTheme: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses
  Messages, Themes;

procedure Register;
begin
  RegisterComponents('MEP', [TVistaAltFix2]);
end;

var
  FInstallCount: Integer = 0;
  FCallWndProcHook: Cardinal = 0;

{ TVistaAltFix2 }

function CallWndProcFunc(
  nCode: Integer;
  wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall;
var
  p: PCWPSTRUCT;
begin
  if nCode = HC_ACTION then
  begin
    p := PCWPSTRUCT(lParam);
    if p.message = WM_UPDATEUISTATE then
    begin
      InvalidateRect(p.hwnd, nil, False);
    end;
  end;
  Result := CallNextHookEx(FCallWndProcHook, nCode, wParam, lParam);
end;

constructor TVistaAltFix2.Create(AOwner: TComponent);
begin
  inherited;
  if VistaWithTheme and not (csDesigning in ComponentState) then
  begin
    Inc(FInstallCount); // Allow more than 1 instance, assume single threaded as VCL is not thread safe anyway
    if FInstallCount = 1 then
      FCallWndProcHook := SetWindowsHookEx(WH_CALLWNDPROC, CallWndProcFunc, 0, GetCurrentThreadID);
    FInstalled := True;
  end;
end;

destructor TVistaAltFix2.Destroy;
begin
  if FInstalled then
  begin
    Dec(FInstallCount);
    if FInstallCount = 0 then
    begin
      UnhookWindowsHookEx(FCallWndProcHook);
      FCallWndProcHook := 0;
    end;
  end;
  inherited Destroy;
end;

function TVistaAltFix2.VistaWithTheme: Boolean;
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) and
     (OSVersionInfo.dwMajorVersion >= 6) and
     ThemeServices.ThemesEnabled then
    Result := True
  else
    Result := False;
end;

end.