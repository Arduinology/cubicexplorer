unit CE_ToolbarEditorItems;

interface

uses
  // SpTBX
  SpTBXItem, SpTBXEditors, TB2Item, SpTBXSkins, TB2Toolbar, TB2Common,
  // Tnt
  TntStdCtrls,
  // System units
  Graphics, Windows, Classes, SysUtils, StdCtrls, Controls, Messages,
  Forms;

type
  TCustomEditAccess = class(TCustomEdit);
  TCustomComboAccess = class(TTntCustomComboBox);
  TTBCustomItemAccess = class(TTBCustomItem);

  TCEClearButtonClickEvent = procedure(Sender: TObject; Shift: TShiftState; var ClearText: Boolean) of object;

  TCEToolbarEditItem = class(TSpTBXEditItem)
  private
    fAutoShowClearButton: Boolean;
    fOnClearButtonClick: TCEClearButtonClickEvent;
    fShowClearButton: Boolean;
    procedure SetShowClearButton(const Value: Boolean);
  protected
    fResizingEditControl: Boolean;
    procedure DoChange(const AText: WideString); override;
    procedure DoClearButtonClick(Shift: TShiftState); virtual;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  public
  published
    property AutoShowClearButton: Boolean read fAutoShowClearButton write
        fAutoShowClearButton;
    property ShowClearButton: Boolean read fShowClearButton write
        SetShowClearButton;
    property OnClearButtonClick: TCEClearButtonClickEvent read fOnClearButtonClick write
        fOnClearButtonClick;
  end;

  TCEToolbarEditItemViewer = class(TSpTBXEditItemViewer)
  private
    fMouseDownOnClearButton: Boolean;
    fMouseDownShiftState: TShiftState;
    function GetEditControlText: WideString;
  protected
    fDragSizing: Boolean;
    fInvertedSizing: Boolean;
    fMouseDownOffset: TPoint;
    ResizeAreaSize: Integer;
    procedure GetClearButtonRect(var R: TRect); virtual;
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    procedure GetEditRect(var R: TRect); override;
    function GetIndentAfter: Integer; override;
    function GetIndentBefore: Integer; override;
    procedure InternalDrawFrame(ACanvas: TCanvas; ARect: TRect; ItemInfo:
        TSpTBXMenuItemInfo); override;
    procedure InternalEditControlExit; override;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu:
        Boolean); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
  public
    constructor Create(AView: TTBView; AItem: TTBCustomItem; AGroupLevel: Integer);
        override;
  end;

  TCEToolbarComboBoxItem = class(TCEToolbarEditItem)
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  end;

  TCEToolbarComboBoxItemViewer = class(TCEToolbarEditItemViewer)
  private
    fComboBox: TTntComboBox;
    fMouseOverDropButton: Boolean;
    function EditLoop(const CapHandle: HWND): Boolean;
    procedure EditWndProc(var Message: TMessage);
    procedure MouseBeginEdit;
  protected
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    function GetIndentAfter: Integer; override;
    procedure InternalDrawFrame(ACanvas: TCanvas; ARect: TRect; ItemInfo:
        TSpTBXMenuItemInfo); override;
    procedure DoChange(NeedResize: Boolean);
    function DoExecute: Boolean; override;
    procedure GetEditRect(var R: TRect); override;
    procedure InternalEditControlExit; override;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu:
        Boolean); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
    procedure SetMouseOverDropButton(Value: Boolean);
  public
    destructor Destroy; override;
    function GetDropDownButtonRect: TRect;
    function GetMouseInDropDownButton: Boolean;
  end;

implementation

procedure TCEToolbarEditItem.DoChange(const AText: WideString);
begin
  inherited;
  if fAutoShowClearButton then
  ShowClearButton:= AText <> '';
end;

{-------------------------------------------------------------------------------
  Do ClearButtonClick
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItem.DoClearButtonClick(Shift: TShiftState);
var
  ClearText: Boolean;
begin
  ClearText:= true;
  if Assigned(fOnClearButtonClick) then fOnClearButtonClick(Self, Shift, ClearText);
  if ClearText then
  Text:= '';
end;

{-------------------------------------------------------------------------------
  Get Item Viewer Class
-------------------------------------------------------------------------------}
function TCEToolbarEditItem.GetItemViewerClass(AView: TTBView):
    TTBItemViewerClass;
begin
  Result:= TCEToolbarEditItemViewer;
end;

{-------------------------------------------------------------------------------
  Show Clear Button
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItem.SetShowClearButton(const Value: Boolean);
begin
  if Value <> fShowClearButton then
  begin
    fShowClearButton:= Value;
    fResizingEditControl:= true;    
    Change(true);
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarEditItemViewer
-------------------------------------------------------------------------------}
constructor TCEToolbarEditItemViewer.Create(AView: TTBView; AItem:
    TTBCustomItem; AGroupLevel: Integer);
begin
  inherited;
  ResizeAreaSize:= 3;
end;

{-------------------------------------------------------------------------------
  Get ClearButtonRect
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.GetClearButtonRect(var R: TRect);
begin
  R.Left:= (BoundsRect.Right - BoundsRect.Left) - GetIndentAfter;
  R.Right:= R.Left + 16;
  R.Top:= 0;
  R.Bottom:= BoundsRect.Bottom - BoundsRect.Top;
end;

{-------------------------------------------------------------------------------
  Get Cursor
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.GetCursor(const Pt: TPoint; var ACursor:
    HCURSOR);
var
  R: TRect;
begin
  if not Item.Enabled then
    Exit;
  GetEditRect(R);
  OffsetRect(R, -BoundsRect.Left, -BoundsRect.Top);
  //InflateRect(R, -2, 0);
  if PtInRect(R, Pt) then
    ACursor:= LoadCursor(0, IDC_IBEAM)
  else if IsToolbarStyle and (//((Pt.X <= ResizeAreaSize) or
          (Pt.X >= (BoundsRect.Right-BoundsRect.Left-ResizeAreaSize))) then
    ACursor:= LoadCursor(0, IDC_SIZEWE)
  else
    ACursor:= LoadCursor(0, IDC_ARROW);
end;

{-------------------------------------------------------------------------------
  Get EditControlText
-------------------------------------------------------------------------------}
function TCEToolbarEditItemViewer.GetEditControlText: WideString;
begin
  Result := '';
  if Assigned(FEditControl) then begin
    if FEditControl is TSpTBXUnicodeEdit then
      Result := TSpTBXUnicodeEdit(FEditControl).Text
    else
      Result := TCustomEditAccess(FEditControl).Text;
  end;
end;

procedure TCEToolbarEditItemViewer.GetEditRect(var R: TRect);
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Get Indent After
-------------------------------------------------------------------------------}
function TCEToolbarEditItemViewer.GetIndentAfter: Integer;
begin
  Result:= ResizeAreaSize;
  if TCEToolbarEditItem(Item).ShowClearButton then
  Result:= Result + 16;
end;

{-------------------------------------------------------------------------------
  Get Indent Before
-------------------------------------------------------------------------------}
function TCEToolbarEditItemViewer.GetIndentBefore: Integer;
begin
  Result:= ResizeAreaSize;
end;

{-------------------------------------------------------------------------------
  Internal Draw Frame
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.InternalDrawFrame(ACanvas: TCanvas; ARect:
    TRect; ItemInfo: TSpTBXMenuItemInfo);
var
  R: TRect;
begin
  inherited;

  if TCEToolbarEditItem(Item).fResizingEditControl then
  begin
    if assigned(FEditControl) then
    begin
      GetEditRect(R);
      InflateRect(R, -3, -3);
      FEditControl.BoundsRect:= R;
    end;
    TCEToolbarEditItem(Item).fResizingEditControl:= false;
  end;

  // Draw Clear button
  if TCEToolbarEditItem(Item).ShowClearButton then
  begin
    GetClearButtonRect(R);
//    if ItemInfo.HotTrack then
//    SpDrawGlyphPattern(ACanvas, R, 0, clWindowText)
//    else

    SpDrawGlyphPattern(ACanvas, R, 0, clGrayText);
  end;
end;

{-------------------------------------------------------------------------------
  InternalEditControlExit
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.InternalEditControlExit;
begin
  Item.Text:= GetEditControlText;
end;

{-------------------------------------------------------------------------------
  Mouse Down
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.MouseDown(Shift: TShiftState; X, Y: Integer;
    var MouseDownOnMenu: Boolean);
var
  w,h: Integer;
  R: TRect;
begin
  w:= BoundsRect.Right-BoundsRect.Left;
  if (X <= ResizeAreaSize) then
  begin
    fDragSizing:= false;
    fInvertedSizing:= true;
  end
  else if (X >= (w-ResizeAreaSize)) then
  begin
    fDragSizing:= true;
    fInvertedSizing:= false;
  end
  else
  fDragSizing:= false;

  if IsToolbarStyle and fDragSizing and (Shift = []) then
  begin
    h:= BoundsRect.Bottom-BoundsRect.Top;
    fMouseDownOffset:= Point(w-X, h-Y);
    Self.View.Selected:= Self;
    Self.View.SetCapture;
  end
  else
  begin
    GetClearButtonRect(R);
    fMouseDownOnClearButton:= PtInRect(R, Point(X, Y));
    fMouseDownShiftState:= Shift;
    if not fMouseDownOnClearButton then
    inherited;
  end;
end;

{-------------------------------------------------------------------------------
  Mouse Move
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.MouseMove(X, Y: Integer);
begin
  if fDragSizing then
  begin
    if Self.Item.Visible then
    begin
      if fInvertedSizing then
      Self.Item.CustomWidth:= fMouseDownOffset.X - X
      else
      Self.Item.CustomWidth:= X + fMouseDownOffset.X;
    end
    else
    begin

    end;
  end;
end;

{-------------------------------------------------------------------------------
  Mouse Up
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu:
    Boolean);
var
  R: TRect;
begin
  if fDragSizing then
  begin
    fDragSizing:= false;
    Self.View.CancelCapture;
  end
  else
  begin
    if fMouseDownOnClearButton and TCEToolbarEditItem(Item).ShowClearButton then
    begin
      GetClearButtonRect(R);
      if PtInRect(R, Point(X,Y)) then
      begin
        TCEToolbarEditItem(Item).DoClearButtonClick(fMouseDownShiftState);
      end;
    end
    else
    inherited;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get Item Viewer Class
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItem.GetItemViewerClass(AView: TTBView):
    TTBItemViewerClass;
begin
  Result:= TCEToolbarComboBoxItemViewer;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Destroy TCEToolbarComboBoxItemViewer
-------------------------------------------------------------------------------}
destructor TCEToolbarComboBoxItemViewer.Destroy;
begin
  if assigned(fComboBox) then
  FreeAndNil(fComboBox);
  inherited;
end;

{-------------------------------------------------------------------------------
  Get Cursor
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.GetCursor(const Pt: TPoint; var ACursor:
    HCURSOR);
var
  R: TRect;
begin
  if not Item.Enabled then
    Exit;
  GetEditRect(R);
  OffsetRect(R, -BoundsRect.Left, -BoundsRect.Top);
  //InflateRect(R, -2, 0);
  if PtInRect(R, Pt) then
  begin
    ACursor:= LoadCursor(0, IDC_IBEAM);
    SetMouseOverDropButton(false);
  end
  else if IsToolbarStyle and ((Pt.X <= ResizeAreaSize) or (Pt.X >= (BoundsRect.Right-BoundsRect.Left-ResizeAreaSize))) then
  begin
    ACursor:= LoadCursor(0, IDC_SIZEWE);
    SetMouseOverDropButton(false);
  end
  else
  begin
    ACursor:= LoadCursor(0, IDC_ARROW);
    SetMouseOverDropButton(GetMouseInDropDownButton);
  end;
end;

{-------------------------------------------------------------------------------
  Get DropDown Button Rect
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItemViewer.GetDropDownButtonRect: TRect;
var
  ButtonWidth: Integer;
  T: TSpTBXSkinType;
begin
  ButtonWidth:= GetSystemMetrics(SM_CXHSCROLL);
  Result.Left:= (Self.BoundsRect.Right - Self.BoundsRect.Left) - ButtonWidth;
  Result.Top:= 0;
  Result.Right:= Result.Left + ButtonWidth;
  Result.Bottom:= Self.BoundsRect.Bottom - Self.BoundsRect.Top;


  T:= SkinManager.GetSkinType;
  case T of
    sknNone:
      begin
        InflateRect(Result, 0, -1);
        OffsetRect(Result, -1, 0);
      end;
    sknWindows:
      begin
        InflateRect(Result, 0, -1);
        OffsetRect(Result, -1, 0);
      end;
    sknSkin:
      begin
        InflateRect(Result, 0, -2);
        OffsetRect(Result, -2, 0);
      end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Indent After
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItemViewer.GetIndentAfter: Integer;
begin
  Result:= inherited GetIndentAfter;
  Result:= Result + GetSystemMetrics(SM_CXHSCROLL) - ResizeAreaSize;
end;

{-------------------------------------------------------------------------------
  Get MouseInDropDownButton
-------------------------------------------------------------------------------}
function TCEToolbarComboBoxItemViewer.GetMouseInDropDownButton: Boolean;
var
  P: TPoint;
  ButtonR: TRect;
  ButtonWidth: Integer;
begin
  Result := False;

  if not (csDesigning in Item.ComponentState) and GetCursorPos(P) then
  begin
    P:= ScreenToClient(P);
    ButtonWidth:= GetSystemMetrics(SM_CXHSCROLL);
    ButtonR.Left:= (BoundsRect.Right - BoundsRect.Left) - ButtonWidth;
    ButtonR.Top:= 0;
    ButtonR.Right:= ButtonR.Left + ButtonWidth;
    ButtonR.Bottom:= BoundsRect.Bottom - BoundsRect.Top;
    Result:= PtInRect(ButtonR, P);
  end;
end;

{-------------------------------------------------------------------------------
  Internal Draw Frame
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.InternalDrawFrame(ACanvas: TCanvas;
    ARect: TRect; ItemInfo: TSpTBXMenuItemInfo);
var
  R: TRect;
  T: TSpTBXSkinType;
begin
  inherited;
  T:= SkinManager.GetSkinType;

  R:= GetDropDownButtonRect;
  fMouseOverDropButton:= GetMouseInDropDownButton;
  SpDrawXPComboButton(ACanvas, R, ItemInfo.Enabled,  ItemInfo.HotTrack, fMouseOverDropButton, false, True, T);
end;

{-------------------------------------------------------------------------------
  DoChange
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.DoChange(NeedResize: Boolean);
begin
  TTBCustomItemAccess(Item).Change(NeedResize);
end;

function TCEToolbarComboBoxItemViewer.DoExecute: Boolean;
begin
  // Close any delay-close popup menus before entering the edit loop
  View.CancelChildPopups;
  Result:= False;
  if EditLoop(View.GetCaptureWnd) then
  begin
    View.EndModal;
    if ecsAccept in FEditControlStatus then
      Result := True;
  end;
end;

function TCEToolbarComboBoxItemViewer.EditLoop(const CapHandle: HWND): Boolean;

  procedure ControlMessageLoop;

    function PointInWindow(const Wnd: HWND; const P: TPoint): Boolean;
    var
      W: HWND;
    begin
      Result := False;
      W := WindowFromPoint(P);
      if W = 0 then Exit;
      if W = Wnd then
        Result := True
      else
        if IsChild(Wnd, W) then
          Result := True;
    end;

    function ContinueLoop: Boolean;
    begin
      Result := (ecsContinueLoop in FEditControlStatus) and
        // TODO: Why is View.IsModalEnding true at the beginning?
        //not View.IsModalEnding and
        fComboBox.Focused and Item.Enabled;
      { Note: View.IsModalEnding is checked since TTBView.CancelMode doesn't
        destroy popup windows; it merely hides them and calls EndModal. So if
        IsModalEnding returns True we can infer that CancelMode was likely
        called. }
    end;

  var
    Msg: TMsg;
    IsKeypadDigit: Boolean;
    V: Integer;
  begin
    try
      while ContinueLoop do begin
        { Examine the next message before popping it out of the queue }
        if not PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then begin
          WaitMessage;
          Continue;
        end;
        case Msg.message of
          WM_SYSKEYDOWN: begin
              { Exit immediately if Alt+[key] or F10 are pressed, but not
                Alt+Shift, Alt+`, or Alt+[keypad digit] }
              if (Msg.wParam <> VK_MENU) and (Msg.wParam <> VK_SHIFT) and
                 (Msg.wParam <> VK_HANJA) then begin
                IsKeypadDigit := False;
                { This detect digits regardless of whether Num Lock is on: }
                if Lo(LongRec(Msg.lParam).Hi) <> 0 then
                  for V := VK_NUMPAD0 to VK_NUMPAD9 do
                    if MapVirtualKey(V, 0) = Lo(LongRec(Msg.lParam).Hi) then begin
                      IsKeypadDigit := True;
                      Break;
                    end;
                if not IsKeypadDigit then begin
                  FEditControlStatus := [ecsClose];
                  Exit;
                end;
              end;
            end;
          WM_SYSKEYUP: begin
              { Exit when Alt is released by itself }
              if Msg.wParam = VK_MENU then begin
                FEditControlStatus := [ecsClose];
                Exit;
              end;
            end;
          WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
          WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
          WM_MBUTTONDOWN, WM_MBUTTONDBLCLK,
          WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK,
          WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK,
          WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK: begin
              { If a mouse click outside the edit control is in the queue,
                exit and let the upstream message loop deal with it }
              if Msg.hwnd <> fComboBox.Handle then
                Exit;
            end;
          WM_MOUSEMOVE, WM_NCMOUSEMOVE: begin
              if GetCapture = CapHandle then begin
                if PointInWindow(fComboBox.Handle, Msg.pt) then
                  ReleaseCapture;
              end
              else if GetCapture = 0 then begin
                if not PointInWindow(fComboBox.Handle, Msg.pt) then
                  SetCapture(CapHandle);
              end;
              if GetCapture = CapHandle then
                SetCursor(LoadCursor(0, IDC_ARROW));
            end;
        end;
        { Now pop the message out of the queue }
        if not PeekMessage(Msg, 0, Msg.message, Msg.message, PM_REMOVE or PM_NOYIELD) then
          Continue;
        if ((Msg.message >= WM_MOUSEFIRST) and (Msg.message <= WM_MOUSELAST)) and
           (Msg.hwnd = CapHandle) then
          { discard, so that the selection doesn't get changed }
        else begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end;
    finally
      { Make sure there are no outstanding WM_*CHAR messages }
      RemoveMessages(WM_CHAR, WM_DEADCHAR);
      RemoveMessages(WM_SYSCHAR, WM_SYSDEADCHAR);
    end;
  end;

var
  R: TRect;
  ActiveWnd, FocusWnd: HWND;
  S: WideString;
begin
  GetEditRect(R);
  if IsRectEmpty(R) then begin
    Result := False;
    Exit;
  end;

  ActiveWnd := GetActiveWindow;
  FocusWnd := GetFocus;

  { Create the edit control }
  //InflateRect(R, -3, -3);
  fComboBox:= TTntComboBox.Create(nil);
  try
    fComboBox.Name := Format('%s_combo_control_%p', [ClassName, Pointer(fComboBox)]);
    fComboBox.Visible := False;
//    TCustomEditAccess(FEditControl).ReadOnly := Item.ReadOnly;
//    TCustomEditAccess(FEditControl).BorderStyle := bsNone;
//    TCustomEditAccess(FEditControl).AutoSize := False;
    fComboBox.Font.Assign(View.GetFont);
    Item.EditorFontSettings.Apply(fComboBox.Font);
//    if FEditControl is TSpTBXUnicodeEdit then begin
//      TSpTBXUnicodeEdit(FEditControl).Alignment := Item.Alignment;
//      TSpTBXUnicodeEdit(FEditControl).PasswordChar := Item.PasswordChar;
//      TSpTBXUnicodeEdit(FEditControl).Text := Item.Text
//    end
//    else
//      TCustomEditAccess(FEditControl).Text := Item.Text;

    fComboBox.Text:= Item.Text;

    fComboBox.CharCase := Item.CharCase;
    fComboBox.MaxLength := Item.MaxLength;
    fComboBox.BoundsRect := R;
    fComboBox.WindowProc := EditWndProc;
    fComboBox.ParentWindow := View.Window.Handle;
    fComboBox.OnChange := InternalEditControlChange;
    fComboBox.SelectAll;
    DoBeginEdit;
    fComboBox.Visible := True;
    fComboBox.SetFocus;
    if GetActiveWindow <> ActiveWnd then
      SendMessage(ActiveWnd, WM_NCACTIVATE, 1, 0) // Don't gray out title bar of old active window
    else
      ActiveWnd := 0;

    FEditControlStatus := [ecsContinueLoop];
    ControlMessageLoop;
  finally
    if FEditControlStatus = [ecsContinueLoop] then
      InternalEditControlExit;
    S:= fComboBox.Text;
    FreeAndNil(fComboBox);
  end;

//  if (FEditControlStatus = [ecsContinueLoop]) and Item.ExtendedAccept then
//    if Item.DoAcceptText(S) then Item.SetTextEx(S, tcrEditControl);

  { ensure the area underneath the edit control is repainted immediately }
  View.Window.Update;
  { If app is still active, set focus to previous control and restore capture
    to CapHandle if another control hasn't taken it }
  if GetActiveWindow <> 0 then begin
    SetFocus(FocusWnd);
    if GetCapture = 0 then
      SetCapture(CapHandle);
  end;
  if ActiveWnd <> 0 then
    SendMessage(ActiveWnd, WM_NCACTIVATE, Ord(GetActiveWindow = ActiveWnd), 0);
  { The SetFocus call above can change the Z order of windows. If the parent
    window is a popup window, reassert its topmostness. }
  if View.Window is TTBPopupWindow then
    SetWindowPos(View.Window.Handle, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  { Send an MSAA "focus" event now that we're returning to the regular modal loop }
  View.NotifyFocusEvent;

  Result := ecsClose in FEditControlStatus;
  if not Result and (GetCapture = CapHandle) then begin
    if ecsAccept in FEditControlStatus then
      { if we are accepting but not closing, Tab must have been pressed }
      View.Selected := View.NextSelectable(View.Selected,
        GetKeyState(VK_SHIFT) >= 0);
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXEditItemViewer }

procedure TCEToolbarComboBoxItemViewer.EditWndProc(var Message: TMessage);

  procedure AcceptText;
  var
    S: WideString;
  begin
    S := GetEditControlText;
//    if Item.DoAcceptText(S) then Item.SetTextEx(S, tcrEditControl);
  end;

var
  R: TRect;
begin
  if fComboBox = nil then
    Exit;

  if not HandleEditMessage(Message) then begin
    if Message.Msg = WM_CHAR then
      case TWMChar(Message).CharCode of
        VK_TAB: begin
            FEditControlStatus := [ecsAccept];
            AcceptText;
            Exit;
          end;
        VK_RETURN: begin
            FEditControlStatus := [ecsAccept, ecsClose];
            AcceptText;
            Exit;
          end;
        VK_ESCAPE: begin
            FEditControlStatus := [];
            Exit;
          end;
      end;
    TCustomComboAccess(fComboBox).WndProc(Message);
  end;
  
  if Message.Msg = WM_KILLFOCUS then begin
    View.CancelMode;
    FEditControlStatus := [ecsClose];
  end;

  if Message.Msg = WM_PAINT then
  begin
    fComboBox.Canvas.Lock; // lock the canvas to prevent flicker on mouse click
    R:= fComboBox.BoundsRect;
    OffsetRect(R, -R.Left, -R.Top);
    try
      fComboBox.Canvas.Brush.Color:= clWindow;
      fComboBox.Canvas.FillRect(R);
    finally
      fComboBox.Canvas.Unlock;
    end;
  end;
end;

procedure TCEToolbarComboBoxItemViewer.GetEditRect(var R: TRect);
begin
  R:= BoundsRect;
  R.Left:= R.Left + GetIndentBefore;

  case SkinManager.GetSkinType of
    sknNone:
      begin
        InflateRect(R, 0, -1);
        OffsetRect(R, -1, 0);
      end;
    sknWindows:
      begin
        InflateRect(R, 0, -1);
        OffsetRect(R, -1, 0);
      end;
    sknSkin:
      begin
        InflateRect(R, 0, -2);
        OffsetRect(R, -2, 0);
      end;
  end;
end;

{-------------------------------------------------------------------------------
  InternalEditControlExit
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.InternalEditControlExit;
begin
  Item.Text:= fComboBox.Text;
end;

procedure TCEToolbarComboBoxItemViewer.MouseBeginEdit;
begin
  if Item.Enabled then
    Execute(True)
  else
  begin
    if (View.ParentView = nil) and not View.IsPopup then
      View.EndModal;
  end;
end;

{-------------------------------------------------------------------------------
  Mouse Down
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.MouseDown(Shift: TShiftState; X, Y:
    Integer; var MouseDownOnMenu: Boolean);
begin
  if IsPtInButtonPart(X, Y) then
    MouseBeginEdit
//  else
//    inherited;
end;

procedure TCEToolbarComboBoxItemViewer.MouseUp(X, Y: Integer;
    MouseWasDownOnMenu: Boolean);
begin
  if IsPtInButtonPart(X, Y) then
    MouseBeginEdit
//  else
//    inherited;
end;

{-------------------------------------------------------------------------------
  Set MouseOverButton
-------------------------------------------------------------------------------}
procedure TCEToolbarComboBoxItemViewer.SetMouseOverDropButton(Value: Boolean);
begin
  if Value <> fMouseOverDropButton then
  begin
    fMouseOverDropButton:= Value;
    DoChange(false);
  end;
end;

end.
