unit CE_SpTBXItems;

interface

uses
  // SpTBX
  SpTBXControls, SpTBXSkins, SpTBXItem,
  // System Units
  Classes, Controls, Messages, Windows, CommCtrl, Graphics, ComCtrls;

{==============================================================================}
type
{-------------------------------------------------------------------------------
  TCETrackBar
-------------------------------------------------------------------------------}
  TCETrackBar = class(TSpTBXTrackBar)
  private
    FCanDrawChannelSelection: Boolean;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    fChannelSize: Integer;
    fShowFocusRect: Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetChannelSize(const Value: Integer);
    procedure SetParent(AParent: TWinControl); override;
    procedure SetShowFocusRect(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function ChannelRect: TRect; reintroduce; virtual;
    function MouseInThumb: Boolean; reintroduce; virtual;
  published
    property ChannelSize: Integer read fChannelSize write SetChannelSize default -1;
    property ShowFocusRect: Boolean read fShowFocusRect write SetShowFocusRect
        default true;
  end;

{==============================================================================}
implementation

{##############################################################################}
// TCETrackBar

{-------------------------------------------------------------------------------
  Create an instance of TCETrackBar
-------------------------------------------------------------------------------}
constructor TCETrackBar.Create(AOwner: TComponent);
begin
  inherited;
  fShowFocusRect:= true;
  fChannelSize:= -1;
end;

{-------------------------------------------------------------------------------
  ChannelRect
-------------------------------------------------------------------------------}
function TCETrackBar.ChannelRect: TRect;
var
  R: TRect;
begin
  // TBM_GETCHANNELRECT allways returns the horizontal channel rect, even
  // when the Orientation is vertical.
  SendMessage(Handle, TBM_GETCHANNELRECT, 0, LPARAM(@Result));
  if Orientation = trVertical then
  begin
    R:= Result;
    Result:= Rect(R.Top, R.Left, R.Bottom, R.Right);
    if ChannelSize > -1 then
    begin
      Result.Left:= Round((Result.Left + ((Result.Right - Result.Left) / 2)) - (ChannelSize / 2));
      Result.Right:= Result.Left + ChannelSize;
    end;
  end
  else
  begin
    if ChannelSize > -1 then
    begin
      Result.Top:= Round((Result.Top + ((Result.Bottom - Result.Top) / 2)) - (ChannelSize / 2));
      Result.Bottom:= Result.Top + ChannelSize;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle CNNotify message (paint here)
-------------------------------------------------------------------------------}
procedure TCETrackBar.CNNotify(var Message: TWMNotify);
var
  Info: PNMCustomDraw;
  ACanvas: TCanvas;
  R: TRect;
  Rgn: HRGN;
  Offset: Integer;
begin
  if Message.NMHdr.code = NM_CUSTOMDRAW then
  begin
    Message.Result:= CDRF_DODEFAULT;
    Info := Pointer(Message.NMHdr);
    case Info.dwDrawStage of
      CDDS_PREPAINT:
        Message.Result := CDRF_NOTIFYITEMDRAW;
      CDDS_ITEMPREPAINT:
        begin
          ACanvas:= TCanvas.Create;
          ACanvas.Lock;
          try
            ACanvas.Handle := Info.hdc;
            case Info.dwItemSpec of
              TBCD_TICS:
                begin
                  R:= ClientRect;
                  SpDrawParentBackground(Self, ACanvas.Handle, R);
                  
                  if Focused and fShowFocusRect then
                  SpDrawFocusRect(ACanvas, R);

                  if TickMarks <> tmxCenter then
                  DrawTicks(ACanvas);

                  Message.Result:= CDRF_SKIPDEFAULT;
                end;
              TBCD_THUMB:
                begin
                  if SliderVisible then
                  begin
                    SendMessage(Handle, TBM_GETTHUMBRECT, 0, LPARAM(@R));
                    if DoDrawThumb(ACanvas, R, pstPrePaint) then
                    SpDrawXPTrackBar(ACanvas, R, TBCD_THUMB, Orientation = trVertical, MouseInThumb, False, TickMarks, Min, Max, SelStart, SelEnd, SkinType);

                    DoDrawThumb(ACanvas, R, pstPostPaint);
                    Message.Result:= CDRF_SKIPDEFAULT;
                  end;
                end;
              TBCD_CHANNEL:
                begin
                  SendMessage(Handle, TBM_GETTHUMBRECT, 0, LPARAM(@R));
                  Offset:= 0;
                  if Focused then
                  Inc(Offset);
                  
                  if Orientation = trHorizontal then
                  begin
                    R.Left:= ClientRect.Left + Offset;
                    R.Right:= ClientRect.Right - Offset;
                  end
                  else
                  begin
                    R.Top:= ClientRect.Top + Offset;
                    R.Bottom:= ClientRect.Bottom - Offset;
                  end;

                  Rgn:= CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
                  SelectClipRgn(ACanvas.Handle, Rgn);
                  try
                    SpDrawParentBackground(Self, ACanvas.Handle, ClientRect);
                    R:= ChannelRect;

                    if DoDrawChannel(ACanvas, R, pstPrePaint) then
                      SpDrawXPTrackBar(ACanvas, R, TBCD_CHANNEL, Orientation = trVertical, False, FCanDrawChannelSelection, TickMarks, Min, Max, SelStart, SelEnd, SkinType);
                    DoDrawChannel(ACanvas, R, pstPostPaint);

                    // Draw channel tics
                    if TickMarks = tmxCenter then
                      DrawTicks(ACanvas);
                  finally
                    DeleteObject(Rgn);
                    SelectClipRgn(ACanvas.Handle, 0);
                  end;
                  Message.Result := CDRF_SKIPDEFAULT;
                end;
            end;
          finally
            ACanvas.Unlock;
            ACanvas.Handle := 0;
            ACanvas.Free;
          end;
        end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  CreateParams
-------------------------------------------------------------------------------}
procedure TCETrackBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  FCanDrawChannelSelection:= (Params.Style and TBS_ENABLESELRANGE) <> 0;
end;

{-------------------------------------------------------------------------------
  MouseInThumb
-------------------------------------------------------------------------------}
function TCETrackBar.MouseInThumb: Boolean;
var
  P: TPoint;
  R: TRect;
begin
  if csDesigning in ComponentState then
    Result := False
  else begin
    SendMessage(Handle, TBM_GETTHUMBRECT, 0, LPARAM(@R));
    GetCursorPos(P);
    P := ScreenToClient(P);
    Result := PtInRect(R, P)
  end;
end;

{-------------------------------------------------------------------------------
  Set ChannelSize
-------------------------------------------------------------------------------}
procedure TCETrackBar.SetChannelSize(const Value: Integer);
begin
  if fChannelSize <> Value then
  begin
    fChannelSize:= Value;
    Self.InvalidateBackground;
  end;
end;

procedure TCETrackBar.SetParent(AParent: TWinControl);
begin
  inherited;
  SetShowFocusRect(fShowFocusRect);
end;

{-------------------------------------------------------------------------------
  Set ShowFocusRect
-------------------------------------------------------------------------------}
procedure TCETrackBar.SetShowFocusRect(const Value: Boolean);
begin
  fShowFocusRect:= Value;
  if assigned(Self.Parent) then
  begin
    if fShowFocusRect then
    SendMessage(Self.Handle, WM_CHANGEUISTATE, MakeWParam(UIS_CLEAR, UISF_HIDEFOCUS), 0)
    else
    SendMessage(Self.Handle, WM_CHANGEUISTATE, MakeWParam(UIS_SET, UISF_HIDEFOCUS), 0);
  end;
end;

end.
