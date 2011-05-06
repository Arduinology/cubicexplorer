unit CE_ToolbarEditorItems;

interface

uses
  SpTBXItem, SpTBXEditors, TB2Item, SpTBXSkins,
  // System units
  Graphics, Windows, Classes, SysUtils, StdCtrls;

type
  TCustomEditAccess = class(TCustomEdit);

  TCEToolbarEditItem = class(TSpTBXEditItem)
  protected
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
  end;

  TCEToolbarEditItemViewer = class(TSpTBXEditItemViewer)
  private
    fDragSizing: Boolean;
    fMouseDownOffset: TPoint;
    function GetEditControlText: WideString;
  protected
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    function GetIndentAfter: Integer; override;
    procedure InternalDrawFrame(ACanvas: TCanvas; ARect: TRect; ItemInfo:
        TSpTBXMenuItemInfo); override;
    procedure InternalEditControlExit; override;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer; var MouseDownOnMenu:
        Boolean); override;
    procedure MouseMove(X, Y: Integer); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
  end;

implementation

{-------------------------------------------------------------------------------
  Get Item Viewer Class
-------------------------------------------------------------------------------}
function TCEToolbarEditItem.GetItemViewerClass(AView: TTBView):
    TTBItemViewerClass;
begin
  Result:= TCEToolbarEditItemViewer;
end;

{##############################################################################}

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
  InflateRect(R, -2, -2);
  if PtInRect(R, Pt) then
    ACursor:= LoadCursor(0, IDC_IBEAM)
  else
    ACursor:= LoadCursor(0, IDC_SIZEWE)
end;

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

{-------------------------------------------------------------------------------
  Get Indent After
-------------------------------------------------------------------------------}
function TCEToolbarEditItemViewer.GetIndentAfter: Integer;
begin
  Result:= 6;
end;

{-------------------------------------------------------------------------------
  Internal Draw Frame
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.InternalDrawFrame(ACanvas: TCanvas; ARect:
    TRect; ItemInfo: TSpTBXMenuItemInfo);
var
  R: TRect;
begin
  R:= ARect;
  inherited;
  
  R.Left:= ARect.Right - GetIndentAfter;
  R:= SpCenterRectVert(R, ARect.Bottom-ARect.Top-4);
  R:= SpCenterRectHoriz(R, 4);

  SpDrawXPMenuSeparator(ACanvas, R, false, true);
end;

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
begin
  w:= BoundsRect.Right-BoundsRect.Left;
  if (X > (w-8)) and (Shift = []) then
  begin
    h:= BoundsRect.Bottom-BoundsRect.Top;
    fDragSizing:= true;
    fMouseDownOffset:= Point(w-X, h-Y);
    Self.View.Selected:= Self;
    Self.View.SetCapture;
  end
  else
  begin
    fDragSizing:= false;
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
    Self.Item.CustomWidth:= X + fMouseDownOffset.X;
  end;
end;

{-------------------------------------------------------------------------------
  Mouse Up
-------------------------------------------------------------------------------}
procedure TCEToolbarEditItemViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu:
    Boolean);
begin
  if fDragSizing then
  begin
    fDragSizing:= false;
    Self.View.CancelCapture;
  end;
  inherited;
end;

end.
