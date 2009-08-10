unit CE_DualView;

interface

uses
  // SpTBX
  TB2Item, SpTBXItem, TB2Dock, TB2Toolbar, SpTBXSkins,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TDualViewSplitChange = procedure(Sender: TObject; NewRect: TRect) of object;
  TCEDualViewSplitter = class(TCustomControl)
  private
    fBrush: TBrush;
    fDownPos: TPoint;
    fHorizontal: Boolean;
    fLineDC: HDC;
    fLineVisible: Boolean;
    fOnSplitChange: TDualViewSplitChange;
    fPrevBrush: HBrush;
    fSplit: Integer;
    fSplitterSize: Integer;
    procedure AllocateLineDC;
    procedure DrawLine;
    procedure ReleaseLineDC;
    procedure SetHorizontal(const Value: Boolean);
    procedure SetSplitterSize(const Value: Integer);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure Paint; override;
    procedure SplitChange(NewRect: TRect);
    procedure SpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Horizontal: Boolean read fHorizontal write SetHorizontal;
    property SplitterSize: Integer read fSplitterSize write SetSplitterSize;
    property OnSplitChange: TDualViewSplitChange read fOnSplitChange write
        fOnSplitChange;
  end;

  TDualViewPane = class(TWinControl)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
  public
  end;
  
  TCEDualViewHost = class(TPanel)
  private
    fCenterToolbar: Boolean;
    fDividePos: Single;
    fDualViewEnabled: Boolean;
    fHorizontalPanes: Boolean;
    procedure SetCenterToolbar(const Value: Boolean);
    procedure SetDividePos(const Value: Single);
    procedure SetDualViewEnabled(const Value: Boolean);
    procedure SetHorizontalPanes(const Value: Boolean);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure AlignPanes;
    procedure HandleSplitChange(Sender: TObject; NewRect: TRect);
    procedure Resize; override;
  public
    TopPageToolDock: TSpTBXDock;
    BottomPageToolDock: TSpTBXDock;
    LeftPageToolDock: TSpTBXDock;
    RightPageToolDock: TSpTBXDock;
    TopDualToolDock: TSpTBXDock;
    BottomDualToolDock: TSpTBXDock;
    LeftDualToolDock: TSpTBXDock;
    RightDualToolDock: TSpTBXDock;
    DualPane: TDualViewPane;
    MainPane: TDualViewPane;
    ToolbarDock: TSpTBXDock;
    Toolbar: TSpTBXToolbar;
    TLSplitter: TCEDualViewSplitter;
    BRSplitter: TCEDualViewSplitter;
    ToolbarPanel: TDualViewPane;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CenterToolbar: Boolean read fCenterToolbar write SetCenterToolbar;
    property DividePos: Single read fDividePos write SetDividePos;
    property DualViewEnabled: Boolean read fDualViewEnabled write
        SetDualViewEnabled;
    property HorizontalPanes: Boolean read fHorizontalPanes write
        SetHorizontalPanes;
  end;

  TCEDualViewSettings = class(TPersistent)
  private
    function GetCenterToolbar: Boolean;
    function GetEnabled: Boolean;
    function GetHorizontal: Boolean;
    function GetDividerPos: Single;
    procedure SetCenterToolbar(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetHorizontal(const Value: Boolean);
    procedure SetDividerPos(const Value: Single);
  public
    DualViewHost: TCEDualViewHost;
  published
    property CenterToolbar: Boolean read GetCenterToolbar write SetCenterToolbar;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Horizontal: Boolean read GetHorizontal write SetHorizontal;
    property DividerPos: Single read GetDividerPos write SetDividerPos;
  end;

implementation

uses
  Main, CE_SpTabBar, dCE_Actions;

{-------------------------------------------------------------------------------
  Create an instance of TCEDualViewHost
-------------------------------------------------------------------------------}
constructor TCEDualViewHost.Create(AOwner: TComponent);
begin
  inherited;
  fDividePos:= 0.5;
  fDualViewEnabled:= true;
  fHorizontalPanes:= true;
  fCenterToolbar:= true;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  // DualPane
  DualPane:= TDualViewPane.Create(nil);
  DualPane.Parent:= Self;
  // MainPane
  MainPane:= TDualViewPane.Create(nil);
  MainPane.Parent:= Self;
  // Toolbar Panel
  ToolbarPanel:= TDualViewPane.Create(nil);
  ToolbarPanel.Parent:= Self;
  // ToolbarDock
  ToolbarDock:= TSpTBXDock.Create(nil);
  ToolbarDock.Position:= dpTop;
  ToolbarDock.Parent:= ToolbarPanel;
  ToolbarDock.AllowDrag:= false;
  // Toolbar
  Toolbar:= TSpTBXToolbar.Create(nil);
  Toolbar.CurrentDock:= ToolbarDock;
  Toolbar.Stretch:= true;
  Toolbar.DockMode:= dmCannotFloatOrChangeDocks;
  Toolbar.DragHandleStyle:= dhNone;
  Toolbar.BorderStyle:= bsNone;
  // Splitters
  TLSplitter:= TCEDualViewSplitter.Create(nil);
  TLSplitter.Parent:= Self;
  TLSplitter.Horizontal:= false;
  TLSplitter.SplitterSize:= 3;
  TLSplitter.OnSplitChange:= HandleSplitChange;

  BRSplitter:= TCEDualViewSplitter.Create(nil);
  BRSplitter.Parent:= Self;
  BRSplitter.Horizontal:= false;
  BRSplitter.SplitterSize:= 3;
  BRSplitter.OnSplitChange:= HandleSplitChange;
  // Toolbar docks
  TopPageToolDock:= TSpTBXDock.Create(nil);
  TopPageToolDock.Name:= 'TopPageToolDock';
  TopPageToolDock.Parent:= MainPane;
  TopPageToolDock.Position:= dpTop;
  BottomPageToolDock:= TSpTBXDock.Create(nil);
  BottomPageToolDock.Name:= 'BottomPageToolDock';
  BottomPageToolDock.Parent:= MainPane;
  BottomPageToolDock.Position:= dpBottom;
  LeftPageToolDock:= TSpTBXDock.Create(nil);
  LeftPageToolDock.Name:= 'LeftPageToolDock';
  LeftPageToolDock.Parent:= MainPane;
  LeftPageToolDock.Position:= dpLeft;
  RightPageToolDock:= TSpTBXDock.Create(nil);
  RightPageToolDock.Name:= 'RightPageToolDock';
  RightPageToolDock.Parent:= MainPane;
  RightPageToolDock.Position:= dpRight;

  TopDualToolDock:= TSpTBXDock.Create(nil);
  TopDualToolDock.Name:= 'TopDualToolDock';
  TopDualToolDock.Parent:= DualPane;
  TopDualToolDock.Position:= dpTop;
  BottomDualToolDock:= TSpTBXDock.Create(nil);
  BottomDualToolDock.Name:= 'BottomDualToolDock';
  BottomDualToolDock.Parent:= DualPane;
  BottomDualToolDock.Position:= dpBottom;
  LeftDualToolDock:= TSpTBXDock.Create(nil);
  LeftDualToolDock.Name:= 'LeftDualToolDock';
  LeftDualToolDock.Parent:= DualPane;
  LeftDualToolDock.Position:= dpLeft;
  RightDualToolDock:= TSpTBXDock.Create(nil);
  RightDualToolDock.Name:= 'RightDualToolDock';
  RightDualToolDock.Parent:= DualPane;
  RightDualToolDock.Position:= dpRight;
end;

{-------------------------------------------------------------------------------
  Destroy TCEDualViewHost
-------------------------------------------------------------------------------}
destructor TCEDualViewHost.Destroy;
begin
  TopPageToolDock.Free;
  BottomPageToolDock.Free;
  LeftPageToolDock.Free;
  RightPageToolDock.Free;
  TopDualToolDock.Free;
  BottomDualToolDock.Free;
  LeftDualToolDock.Free;
  RightDualToolDock.Free;
  Toolbar.Free;
  ToolbarDock.Free;
  TLSplitter.Free;
  BRSplitter.Free;
  DualPane.Free;
  MainPane.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Align Panes
-------------------------------------------------------------------------------}
procedure TCEDualViewHost.AlignPanes;
var
  mainSize: Integer;
  centerSize: Integer;
begin
  SendMessage(Handle, WM_SETREDRAW, 0,0);

  if (DividePos < 0) and (DividePos > 1) then
  DividePos:= 0.5;

  if DualViewEnabled then
  begin
    TLSplitter.Horizontal:= not HorizontalPanes;
    BRSplitter.Horizontal:= not HorizontalPanes;
    if HorizontalPanes then
    begin
      if ToolbarDock.Position <> dpTop then
      begin
        Toolbar.Visible:= false;
        Toolbar.CurrentDock:= nil;
        ToolbarDock.Position:= dpTop;
        Toolbar.CurrentDock:= ToolbarDock;
        Toolbar.Visible:= true;
      end;
      centerSize:= TLSplitter.SplitterSize;
      if CenterToolbar then
      begin
        centerSize:= centerSize + ToolbarDock.Height;
        centerSize:= centerSize + BRSplitter.SplitterSize;
      end;
      mainSize:= Round((Self.ClientHeight - centerSize) * DividePos);
      MainPane.BoundsRect:= Rect(0, 0, ClientWidth, mainSize);
      TLSplitter.BoundsRect:= Rect(0, MainPane.BoundsRect.Bottom, ClientWidth, MainPane.BoundsRect.Bottom + TLSplitter.SplitterSize);
      if CenterToolbar then
      begin
        ToolbarPanel.BoundsRect:= Rect(0, TLSplitter.BoundsRect.Bottom, ClientWidth, TLSplitter.BoundsRect.Bottom + ToolbarDock.Height);
        BRSplitter.BoundsRect:= Rect(0, ToolbarPanel.BoundsRect.Bottom, ClientWidth, ToolbarPanel.BoundsRect.Bottom + BRSplitter.SplitterSize);
        DualPane.BoundsRect:= Rect(0, BRSplitter.BoundsRect.Bottom, ClientWidth, ClientHeight);
      end
      else
      begin
        DualPane.BoundsRect:= Rect(0, TLSplitter.BoundsRect.Bottom, ClientWidth, ClientHeight);
      end;
    end
    else
    begin
      if ToolbarDock.Position <> dpLeft then
      begin
        Toolbar.Visible:= false;
        Toolbar.CurrentDock:= nil;
        ToolbarDock.Position:= dpLeft;
        Toolbar.CurrentDock:= ToolbarDock;
        Toolbar.Visible:= true;
      end;
      centerSize:= TLSplitter.SplitterSize;
      if CenterToolbar then
      begin
        centerSize:= centerSize + ToolbarDock.Width;
        centerSize:= centerSize + BRSplitter.SplitterSize;
      end;
      mainSize:= Round((Self.ClientWidth - centerSize) * DividePos);
      MainPane.BoundsRect:= Rect(0, 0, mainSize, ClientHeight);
      TLSplitter.BoundsRect:= Rect(MainPane.BoundsRect.Right, 0, MainPane.BoundsRect.Right + TLSplitter.SplitterSize, ClientHeight);
      if CenterToolbar then
      begin
        ToolbarPanel.BoundsRect:= Rect(TLSplitter.BoundsRect.Right, 0, TLSplitter.BoundsRect.Right + ToolbarDock.Width, ClientHeight);
        BRSplitter.BoundsRect:= Rect(ToolbarPanel.BoundsRect.Right, 0, ToolbarPanel.BoundsRect.Right + BRSplitter.SplitterSize, ClientHeight);
        DualPane.BoundsRect:= Rect(BRSplitter.BoundsRect.Right, 0, ClientWidth, ClientHeight);
      end
      else
      begin
        DualPane.BoundsRect:= Rect(TLSplitter.BoundsRect.Right, 0, ClientWidth, ClientHeight);
      end;
    end;

    DualPane.Visible:= true;
    TLSplitter.Visible:= true;
    ToolbarPanel.Visible:= CenterToolbar;
    BRSplitter.Visible:= CenterToolbar;
  end
  else
  begin
    DualPane.Visible:= false;
    ToolbarPanel.Visible:= false;
    TLSplitter.Visible:= false;
    BRSplitter.Visible:= false;
    MainPane.BoundsRect:= Self.ClientRect;
  end;

  SendMessage(Handle, WM_SETREDRAW, 1,0);
  RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
end;

{-------------------------------------------------------------------------------
  Handle SplitChange
-------------------------------------------------------------------------------}
procedure TCEDualViewHost.HandleSplitChange(Sender: TObject; NewRect: TRect);
var
  s: Single;
  centerSize: Integer;
begin
  if HorizontalPanes then
  begin
    centerSize:= TLSplitter.SplitterSize;
    centerSize:= centerSize + ToolbarDock.Height;
    centerSize:= centerSize + BRSplitter.SplitterSize;  
    if Sender = TLSplitter then
    s:= NewRect.Top / (ClientHeight - centerSize)
    else
    s:= (NewRect.Bottom - centerSize) / (ClientHeight - centerSize)
  end
  else
  begin
    centerSize:= TLSplitter.SplitterSize;
    centerSize:= centerSize + ToolbarDock.Width;
    centerSize:= centerSize + BRSplitter.SplitterSize;
    if Sender = TLSplitter then
    s:= NewRect.Left / (ClientWidth - centerSize)
    else
    s:= (NewRect.Right - centerSize) / (ClientWidth - centerSize)
  end;

  if (s > 0) and (s < 1.0) then
  DividePos:= s;
end;

procedure TCEDualViewHost.Resize;
begin
  inherited;
  AlignPanes;
end;

{-------------------------------------------------------------------------------
  Set CenterToolbar
-------------------------------------------------------------------------------}
procedure TCEDualViewHost.SetCenterToolbar(const Value: Boolean);
begin
  if fCenterToolbar <> Value then
  begin
    fCenterToolbar:= Value;
    AlignPanes;
  end;
end;

{-------------------------------------------------------------------------------
  Set DividePos
-------------------------------------------------------------------------------}
procedure TCEDualViewHost.SetDividePos(const Value: Single);
begin
  fDividePos:= Value;
  AlignPanes;
end;

{-------------------------------------------------------------------------------
  Set DualView Enabled
-------------------------------------------------------------------------------}
procedure TCEDualViewHost.SetDualViewEnabled(const Value: Boolean);
begin
  fDualViewEnabled:= Value;
  AlignPanes;
end;

{-------------------------------------------------------------------------------
  Set HorizontalPanes
-------------------------------------------------------------------------------}
procedure TCEDualViewHost.SetHorizontalPanes(const Value: Boolean);
begin
  fHorizontalPanes:= Value;
  AlignPanes;
end;

{-------------------------------------------------------------------------------
  WM_EraseBkgnd
-------------------------------------------------------------------------------}
procedure TCEDualViewHost.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result:= 0;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEDualViewSplitter
-------------------------------------------------------------------------------}
constructor TCEDualViewSplitter.Create(AOwner: TComponent);
begin
  inherited;
  Height:= 5;
  Width:= 20;
  Horizontal:= true;
  Cursor:= crHSplit;
  SkinManager.AddSkinNotification(Self);
end;

{-------------------------------------------------------------------------------
  Destroy TCEDualViewSplitter
-------------------------------------------------------------------------------}
destructor TCEDualViewSplitter.Destroy;
begin
  fBrush.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Allocate Line DC
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.AllocateLineDC;
begin
  fLineDC:= GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);
  if fBrush = nil then
  begin
    fBrush:= TBrush.Create;
    fBrush.Bitmap:= AllocPatternBitmap(clBlack, clWhite);
  end;
  fPrevBrush:= SelectObject(fLineDC, fBrush.Handle);
end;

{-------------------------------------------------------------------------------
  Draw Line
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.DrawLine;
var
  p: TPoint;
begin
  fLineVisible:= not fLineVisible;
  p:= Point(Left, Top);
  if Horizontal then
  p.X:= Left + fSplit
  else
  p.Y:= Top + fSplit;
  PatBlt(fLineDC, p.X, p.Y, Width, Height, PATINVERT);
end;

{-------------------------------------------------------------------------------
  Release Line DC
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.ReleaseLineDC;
begin
  if fPrevBrush <> 0 then SelectObject(fLineDC, fPrevBrush);
  ReleaseDC(Parent.Handle, fLineDC);
  if fBrush <> nil then
  begin
    fBrush.Free;
    fBrush:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Mouse Down
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
  begin
    fSplit:= 0;
    fDownPos:= Point(X, Y);
    AllocateLineDC;
    DrawLine;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Mouse Move
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Split: Integer;
begin
  inherited;
  if Horizontal then
  Split:= X - fDownPos.X
  else
  Split:= Y - fDownPos.Y;
  DrawLine;
  fSplit:= Split;
  DrawLine;
end;

{-------------------------------------------------------------------------------
  Handle Mouse Up
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  r: TRect;
begin
  inherited;
  DrawLine;
  if fLineVisible then DrawLine;
  ReleaseLineDC;
  if Button = mbLeft then
  begin
    if not Horizontal then
    begin
      r.Left:= BoundsRect.Left;
      r.Right:= BoundsRect.Right;
      r.Top:= BoundsRect.Top + fSplit;
      r.Bottom:= r.Top + Height;
      SplitChange(r);
    end
    else
    begin
      r.Left:= BoundsRect.Left + fSplit;
      r.Right:= r.Left + Width;
      r.Top:= BoundsRect.Top;
      r.Bottom:= BoundsRect.Bottom;
      SplitChange(r);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Paint;
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.Paint;
begin
  //CurrentSkin.PaintBackground(Canvas, ClientRect, skncSplitter, sknsNormal, True, False, Horizontal)
  SpDrawXPDock(Canvas, Self.ClientRect, SkinManager.GetSkinType, Horizontal);
end;

{-------------------------------------------------------------------------------
  Set Horizontal
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.SetHorizontal(const Value: Boolean);
begin
  if fHorizontal <> Value then
  begin
    fHorizontal:= Value;
    if Horizontal then
    Cursor:= crHSplit
    else
    Cursor:= crVSplit;
  end;
end;

{-------------------------------------------------------------------------------
  Set SplitterSize
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.SetSplitterSize(const Value: Integer);
begin
  fSplitterSize:= Value;
  if Horizontal then
  Width:= fSplitterSize
  else
  Height:= fSplitterSize;
end;

{-------------------------------------------------------------------------------
  Split Change
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.SplitChange(NewRect: TRect);
begin
  if Assigned(fOnSplitChange) then fOnSplitChange(Self, NewRect);
end;

{-------------------------------------------------------------------------------
  On Skin Change
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.SpSkinChange(var Message: TMessage);
begin
  inherited;
  Paint;
end;

{-------------------------------------------------------------------------------
  WM_EraseBkgnd
-------------------------------------------------------------------------------}
procedure TCEDualViewSplitter.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result:= 0;
end;


{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set Enabled
-------------------------------------------------------------------------------}
function TCEDualViewSettings.GetEnabled: Boolean;
begin
  Result:= DualViewHost.DualViewEnabled;
end;
procedure TCEDualViewSettings.SetEnabled(const Value: Boolean);
begin
  DualViewHost.DualViewEnabled:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set Horizontal
-------------------------------------------------------------------------------}
function TCEDualViewSettings.GetHorizontal: Boolean;
begin
  Result:= DualViewHost.HorizontalPanes;
end;
procedure TCEDualViewSettings.SetHorizontal(const Value: Boolean);
begin
  DualViewHost.HorizontalPanes:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set CenterToolbar
-------------------------------------------------------------------------------}
function TCEDualViewSettings.GetCenterToolbar: Boolean;
begin
  Result:= DualViewHost.CenterToolbar;
end;
procedure TCEDualViewSettings.SetCenterToolbar(const Value: Boolean);
begin
  DualViewHost.CenterToolbar:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set DividerPos
-------------------------------------------------------------------------------}
function TCEDualViewSettings.GetDividerPos: Single;
begin
  Result:= DualViewHost.DividePos;
end;
procedure TCEDualViewSettings.SetDividerPos(const Value: Single);
begin
  DualViewHost.DividePos:= Value;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  WM_EraseBkgnd
-------------------------------------------------------------------------------}
procedure TDualViewPane.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result:= 0;
end;

end.
