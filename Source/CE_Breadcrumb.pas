//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
//                                                                                            
//  The contents of this file are subject to the Mozilla Public License                       
//  Version 1.1 (the "License"); you may not use this file except in                          
//  compliance with the License. You may obtain a copy of the License at                      
//  http://www.mozilla.org/MPL/                                                               
//                                                                                            
//  Software distributed under the License is distributed on an "AS IS"
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
//  License for the specific language governing rights and limitations                        
//  under the License.                                                                        
//                                                                                            
//  The Original Code is CE_Breadcrumb.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Breadcrumb;

interface

uses
  // CE Units
  CE_ScrollToolbar, CE_GlobalCtrl, CE_VistaFuncs, fCE_FolderTreeForm,
  // SpTBX, TB2K
  SpTBXFormPopupMenu, SpTBXSkins, SpTBXItem,
  //TBXUtils, TBXThemes,
  // VSTools
  MPShellUtilities,
  // Graphics32
  GR32,
  // System Units
  Windows, SysUtils, Messages, Classes, Controls, ShlObj, Graphics, Math, Forms;

type
  TCEBreadcrumbSettings = class(TPersistent)
  protected
    fMaxCrumbSize: Integer;
    fHideComputerCrumb: Boolean;
    fHideDesktopCrumb: Boolean;
  public
    constructor Create;
  published
    property MaxCrumbSize: Integer read fMaxCrumbSize write fMaxCrumbSize;
    property HideComputerCrumb: Boolean read fHideComputerCrumb write
        fHideComputerCrumb;
    property HideDesktopCrumb: Boolean read fHideDesktopCrumb write
        fHideDesktopCrumb;
  end;
  
  TCEBreadcrumb = class(TCEScrollToolbar, ICEPathChangeHandler)
  private
    fShowBorder: Boolean;
  protected
    fSettings: TCEBreadcrumbSettings;
    procedure DrawBackground; override;
    function GetItemRect(Index: Integer): TRect; override;
    function GetMaxItemWidth: Integer; override;
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); virtual;
        stdcall;
    procedure GlobalContentChange(Sender: TObject); virtual; stdcall;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); virtual;
        stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); virtual;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); virtual;
        stdcall;
    procedure OnClosePopup(Sender: TObject; Selected: Boolean); virtual;
    procedure PopupFolderForm(X, Y: Integer; APIDL: PItemIDList); virtual;
  public
    FolderPopup: TSpTBXFormPopupMenu;
    PopupFormSize: TSize;
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
    property Settings: TCEBreadcrumbSettings read fSettings write fSettings;
    property ShowBorder: Boolean read fShowBorder write fShowBorder;
  end;

  TCEBreadcrumbItem = class(TCEScrollToolbarItem)
  private
    fOnClick: TNotifyEvent;
    fParent: TCEBreadcrumb;
  protected
    procedure DrawItem(Buffer: TBitmap32; ARect: TRect; ItemStyle: TItemStyle =
        itNormal); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        override;
  public
    Namespace: TNamespace;
    destructor Destroy; override;
    function GetWidth(Buffer: TBitmap32): Integer; override;
    property Parent: TCEBreadcrumb read fParent write fParent;
  published
    property OnClick: TNotifyEvent read fOnClick write fOnClick;
  end;

  TCEBreadcrumbBar = class(TSpTBXToolWindow)
  protected
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
  public
    Breadcrumb: TCEBreadcrumb;
    constructor Create(AOwner: TComponent); override;
  end;

var
  GlobalBreadcrumbSettings: TCEBreadcrumbSettings;

implementation

uses
  dCE_Actions, CE_AppSettings;

{*------------------------------------------------------------------------------
  Destroy TCEBreadcrumbItem
-------------------------------------------------------------------------------}
destructor TCEBreadcrumbItem.Destroy;
begin
  if assigned(Namespace) then
  Namespace.Free;
  inherited;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Draw Item
-------------------------------------------------------------------------------}
procedure TCEBreadcrumbItem.DrawItem(Buffer: TBitmap32; ARect: TRect;
    ItemStyle: TItemStyle = itNormal);
var
  state: TSpTBXSkinStatesType;
begin
  state:= sknsNormal;
  case ItemStyle of
    itNormal: state:= sknsNormal;
    itSelected: state:= sknsHotTrack;
    itPushed: state:= sknsPushed;
    itChecked: state:= sknsChecked;
  end;
  if Checked then
  state:= sknsChecked;

  // draw button
  DrawMenuItem(Buffer, ARect, Checked, ItemStyle);

  // draw text
  InflateRect(ARect, -3, 0);
  Buffer.Font.Color:= SkinManager.CurrentSkin.GetTextColor(skncToolbarItem, state);
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
  Buffer.Textout(ARect,DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS, Caption)
  else
  Buffer.TextoutW(ARect,DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS, Caption);
end;

{*------------------------------------------------------------------------------
  Get item width
-------------------------------------------------------------------------------}
function TCEBreadcrumbItem.GetWidth(Buffer: TBitmap32): Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
  Result:= Buffer.TextWidth(Caption) + 8
  else
  Result:= Buffer.TextWidthW(Caption) + 8;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Down
-------------------------------------------------------------------------------}
procedure TCEBreadcrumbItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    if assigned(fOnClick) then
    fOnClick(Self);
    if assigned(Namespace) then
    GlobalPathCtrl.ChangeGlobalPathPIDL(nil, Namespace.AbsolutePIDL);
  end
  else if (Shift = [ssMiddle]) or (Shift = [ssAlt,ssLeft]) then
  begin
    if assigned(Namespace) then
    OpenFolderInTab(Self, Namespace.AbsolutePIDL);
  end
  else if (Shift = [ssShift,ssMiddle]) or (Shift = [ssShift,ssAlt,ssLeft]) then
  begin
    if assigned(Namespace) then
    OpenFolderInTab(Self, Namespace.AbsolutePIDL, false);
  end;
end;

{*------------------------------------------------------------------------------
  Handle Mouse Up
-------------------------------------------------------------------------------}
procedure TCEBreadcrumbItem.MouseUp(Button: TMouseButton; Shift: TShiftState;
    X, Y: Integer);
var
  p: TPoint;
  r: TRect;
begin
  if Button = mbRight then
  begin
    if assigned(Parent) then
    begin
      r:= Parent.GetItemRect(Parent.Items.IndexOf(Self));
      p.X:= r.Left;
      p.Y:= r.Bottom;
      p:= Parent.ClientToScreen(p);
      Parent.PopupFolderForm(p.X,p.Y,Namespace.AbsolutePIDL);
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create instance of TCEFolderCombo
-------------------------------------------------------------------------------}
constructor TCEBreadcrumb.Create(AOwner: TComponent);
var
  ncm: TNonClientMetrics;
begin
  inherited;

  ncm.cbSize := SizeOf(TNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(TNonClientMetrics), @ncm, 0);
  Bitmap.Font.Handle :=  CreateFontIndirect(ncm.lfMenuFont);
  Self.Constraints.MinHeight:= Bitmap.TextHeight('jJ') + 2;

  fShowBorder:= true;
  PopupFormSize.cx:= 200;
  PopupFormSize.cy:= 200;
  FolderPopup:= TSpTBXFormPopupMenu.Create(Self);
  FolderPopup.BorderStyle:= pbsSizeableRightBottom;
  FolderPopup.OnClosePopup:= OnClosePopup;
  FolderPopup.PopupFocus:= true;
  GlobalPathCtrl.RegisterNotify(self);

  MaxItemWidth:= 200;
end;

{*------------------------------------------------------------------------------
  Draw Background
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.DrawBackground;
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Get item position rect
-------------------------------------------------------------------------------}
function TCEBreadcrumb.GetItemRect(Index: Integer): TRect;
var
  i: Integer;
  w, maxW: Integer;
begin
  if assigned(Settings) then
  maxW:= Settings.MaxCrumbSize
  else
  maxW:= fMaxItemWidth;
  
  Result.Left:= 0;
  for i:= 0 to fItems.Count - 1 do
  begin
    Inc(Result.Left, fSeparatorSize);
    w:= TCEScrollToolbarItem(fItems.Items[i]).GetWidth(Bitmap);
    if (maxW > 0) and (w > maxW) then
    w:= maxW;
    Result.Right:= Result.Left + w;
    if i = Index then
    begin
      Break;
    end
    else
    Result.Left:= Result.Right;
  end;
  Result.Top:= 0;
  Result.Bottom:= Bitmap.Height
end;

{-------------------------------------------------------------------------------
  Get MaxItemWidth
-------------------------------------------------------------------------------}
function TCEBreadcrumb.GetMaxItemWidth: Integer;
begin
  if assigned(Settings) then
  Result:= Settings.MaxCrumbSize
  else
  Result:= fMaxItemWidth;
end;

{*------------------------------------------------------------------------------
  Get's called when Active page has changed
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalActivePageChange(OldPage, NewPage: TComponent);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when global content has changed
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalContentChange(Sender: TObject);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called on global focus change.
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
  // Do nothing
end;

{*------------------------------------------------------------------------------
  Get's called on global path change (string)
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalPathChanged(Sender: TObject; NewPath: WideString);
var
  apidl: PItemIDList;
begin
  apidl:= PathToPIDL(NewPath);
  try
    GlobalPIDLChanged(Sender, apidl);
  finally
    if assigned(apidl) then
    PIDLMgr.FreeAndNilPIDL(apidl);
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on global path change (PIDL)
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
var
  item: TCEBreadcrumbItem;
  NS: TNamespace;
  APIDL: PItemIDList;
  i: Integer;
  recreate: Boolean;
  showAll: Boolean;
begin
  recreate:= true;

  if assigned(NewPIDL) then
  begin
    for i:= 0 to Items.Count - 1 do
    begin
      item:= TCEBreadcrumbItem(Items.Items[i]);
      if PIDLMgr.EqualPIDL(item.Namespace.AbsolutePIDL, NewPIDL) then
      begin
        item.Checked:= true;
        recreate:= false;
        Self.MakeItemVisible(i);
      end
      else
      begin
        item.Checked:= false;
      end;
    end;
  end;

  if not recreate then
  begin
    DrawBar;
    Exit;
  end;

  Clear;

  item:= TCEBreadcrumbItem(InsertItem(TCEBreadcrumbItem,0));
  item.Parent:= self;
  APIDL:= PIDLMgr.CopyPIDL(NewPIDL);
  item.Namespace:= TNamespace.Create(APIDL,nil);
  item.Caption:= item.Namespace.NameInFolder;
  item.Checked:= true;
  i:= 0;
  NS:= item.Namespace;
  showAll:= NS.IsMyComputer;

  if not NS.IsDesktop then
  begin
    while assigned(NS.Parent) do
    begin
      NS:= NS.Parent;

      if showAll or
         not assigned(Settings) or
         not ((Settings.HideComputerCrumb and NS.IsMyComputer) or
             (Settings.HideDesktopCrumb and NS.IsDesktop))
      then
      begin
        item:= TCEBreadcrumbItem(InsertItem(TCEBreadcrumbItem,0));
        item.Parent:= self;
        APIDL:= PIDLMgr.CopyPIDL(NS.AbsolutePIDL);
        item.Namespace:= TNamespace.Create(APIDL,nil);
        item.Caption:= item.Namespace.NameInFolder;
        Inc(i);
      end;

      if NS.IsDesktop then
      break;
    end;
  end;
  Self.MakeItemVisible(i);
  DrawBar;
end;

{*------------------------------------------------------------------------------
  Popup Folder Form
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.PopupFolderForm(X, Y: Integer; APIDL: PItemIDList);
var
  form: TCE_FolderTreeForm;
  w: Integer;
begin
  if Assigned(ActiveFormPopupMenu) then
  ActiveFormPopupMenu.ClosePopup(false);

  if assigned(FolderPopup.PopupForm) then
  begin
    FolderPopup.PopupForm.Free;
    FolderPopup.PopupForm:= nil;
  end;
  
  form:= TCE_FolderTreeForm.Create(nil);
  form.ChangeGlobalPathOnChange:= true;
  form.CloseOnChange:= true;
  form.FolderTree.RootFolderCustomPIDL:= APIDL;
  form.Width:= PopupFormSize.cx;
  form.Height:= PopupFormSize.cy;
  form.AutoSizeWidth;

  w:= GetSystemMetrics(SM_CXVSCROLL) + 8;
  form.Width:= form.Width + w;
  FolderPopup.PopupForm:= form;
  FolderPopup.Popup(self.Offset + X,Y);
end;

{*------------------------------------------------------------------------------
  Get's called on form popup close
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.OnClosePopup(Sender: TObject; Selected: Boolean);
begin
  if assigned(FolderPopup.PopupForm) then
  begin
    PopupFormSize.cx:= FolderPopup.PopupForm.Width;
    PopupFormSize.cy:= FolderPopup.PopupForm.Height;
    FolderPopup.PopupForm.Free;
    FolderPopup.PopupForm:= nil;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Resize
-------------------------------------------------------------------------------}
procedure TCEBreadcrumb.Resize;
var
  i: Integer;
  r: TRect;
begin
  Inherited;
  for i:= 0 to Self.Items.Count-1 do
  begin
    if TCEBreadcrumbItem(Self.Items.Items[i]).Checked then
    break;
  end;
  if i > -1 then
  begin
    r:= Self.GetItemRect(Self.Items.Count-1);
    if r.Right < Self.Bitmap.Width then
    Self.MakeItemVisible(0)
    else
    Self.MakeItemVisible(i);
  end;
  DrawBar;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create instance of TCEBreadcrumbBar
-------------------------------------------------------------------------------}
constructor TCEBreadcrumbBar.Create(AOwner: TComponent);
begin
  inherited;
  SetVistaFont(Font);
  Parent:= TWinControl(AOwner);

  self.Stretch:= true;
  Breadcrumb:= TCEBreadcrumb.Create(self);
  Breadcrumb.Parent:= self;
  Breadcrumb.Align:= alClient;
  Breadcrumb.SeparatorSize:= 1;
  Breadcrumb.Settings:= GlobalBreadcrumbSettings;
  self.MinClientHeight:= Max(Breadcrumb.Constraints.MinHeight,20);
  self.ClientHeight:= 22;
end;

{-------------------------------------------------------------------------------
  Do ContextPopup
-------------------------------------------------------------------------------}
procedure TCEBreadcrumbBar.DoContextPopup(MousePos: TPoint; var Handled:
    Boolean);
begin
  inherited;
  Handled:= Self.Breadcrumb.IndexByPos(MousePos.X, MousePos.Y) > -1;
end;

{##############################################################################}
// TCEBreadcrumbSettings

{-------------------------------------------------------------------------------
  Create an instance of TCEBreadcrumbSettings
-------------------------------------------------------------------------------}
constructor TCEBreadcrumbSettings.Create;
begin
  inherited Create;
  fMaxCrumbSize:= 200;
  fHideComputerCrumb:= true;
  fHideDesktopCrumb:= true;
end;

{##############################################################################}

initialization
  GlobalBreadcrumbSettings:= TCEBreadcrumbSettings.Create;
  GlobalAppSettings.AddItem('Breadcrumb', GlobalBreadcrumbSettings);

finalization
  FreeAndNil(GlobalBreadcrumbSettings);

end.
