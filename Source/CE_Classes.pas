unit CE_Classes;

interface

uses
  Windows, Messages, Classes;

type
  TCEWndObject = class(TObject)
  protected
    fHandle: HWND;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AllocHWnd(Method: TWndMethod): HWND; virtual;
    procedure DeallocHWnd(Wnd: HWND); virtual;
    procedure WindowProc(var Msg : TMessage); virtual;
    property Handle: HWND read fHandle;
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEWndObject
-------------------------------------------------------------------------------}
constructor TCEWndObject.Create;
begin
  inherited;
  fHandle:= AllocHWnd(WindowProc);
end;

{*------------------------------------------------------------------------------
  Destroy TCEWndObject
-------------------------------------------------------------------------------}
destructor TCEWndObject.Destroy;
begin
  DeallocHWnd(fHandle);
  inherited;
end;

{*------------------------------------------------------------------------------
  Allocate Window Handle
-------------------------------------------------------------------------------}
function TCEWndObject.AllocHWnd(Method: TWndMethod): HWND;
begin
  Result:= AllocateHWnd(WindowProc);
end;

{*------------------------------------------------------------------------------
  Deallocate Window Handle
-------------------------------------------------------------------------------}
procedure TCEWndObject.DeallocHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
  if Instance <> @DefWindowProc then
  begin
    SetWindowLong(Wnd, GWL_WNDPROC, Longint(@DefWindowProc));
    FreeObjectInstance(Instance);
  end;
  DestroyWindow(Wnd);
end;

{*------------------------------------------------------------------------------
  Handle messages
-------------------------------------------------------------------------------}
procedure TCEWndObject.WindowProc(var Msg : TMessage);
begin
  Msg.Result:= DefWindowProc(fHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.
