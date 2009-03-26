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
//  The Original Code is CE_AddressBar.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_AddressBar;

interface

uses
  // CE Units
  fCE_FolderTreeForm, CE_VistaFuncs, CE_GlobalCtrl,
  // TB2K, TBX, SpTBX
  SpTBXEditors, SpTBXFormPopupMenu, TBX, TBXThemes, TB2Item, SpTBXItem,
  // VSTools
  MPShellUtilities, MPCommonObjects, MPCommonUtilities,
  // Tnt Controls
  TntSysUtils,
  // System Units
  Windows, Messages, Classes, SysUtils, Graphics, Controls, ExtCtrls,
  Forms, ShlObj;

type
  TFolderIcon = class(TCustomControl)
  private
    fIconIndex: Integer;
    procedure SetIconIndex(const Value: Integer);
  public
    procedure Paint; override;
    property IconIndex: Integer read fIconIndex write SetIconIndex;
  end;

  TCEFormPopup = class(TSpTBXFormPopupMenu)
  private
    fPopupFormHeight: Integer;
    fPopupFormWidth: Integer;
  protected
    function InternalPopup(X, Y: Integer; out ClickedItem: TTBCustomItem;
        PopupControl: TControl = nil; ReturnClickedItemOnly: Boolean = False):
        Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property PopupFormHeight: Integer read fPopupFormHeight write fPopupFormHeight;
    property PopupFormWidth: Integer read fPopupFormWidth write fPopupFormWidth;
  end;

  TCEAddressBar = class(TSpTBXButtonEdit, ICEPathChangeHandler)
  private
  protected
    FolderIcon: TFolderIcon;
    FolderForm: TCE_FolderTreeForm;
    FormPopup: TCEFormPopup;
    RootNamespace: TNamespace;
    procedure DoValueChanged;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); stdcall;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UpdateEditRect; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BrowseTo(APIDL: PItemIDList);
    procedure OnPopup(Sender: TObject);
    procedure Resize; override;
  end;

  TCEAddressBarToolbar = class(TTBXToolWindow)
  public
    AddressBar: TCEAddressBar;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEFormPopup
-------------------------------------------------------------------------------}
constructor TCEFormPopup.Create(AOwner: TComponent);
begin
  inherited;
  fPopupFormWidth:= 200;
  fPopupFormHeight:= 200;
end;

{*------------------------------------------------------------------------------
  Handle Popup
-------------------------------------------------------------------------------}
function TCEFormPopup.InternalPopup(X, Y: Integer; out ClickedItem:
    TTBCustomItem; PopupControl: TControl = nil; ReturnClickedItemOnly: Boolean
    = False): Boolean;
begin
  Result := False;
  ClickedItem := nil;

  {$IFDEF JR_D9}
  SetPopupPoint(Point(X, Y));
  {$ELSE}
  PPoint(@PopupPoint)^ := Point(X, Y);
  {$ENDIF}

  if Assigned(FPopupForm) then
  begin

    FPopupForm.Parent := FForm;
    FPopupForm.Align := alClient;
    FPopupForm.BorderStyle := bsNone;
    FPopupForm.Visible := True;
    FPopupForm.Color := CurrentTheme.GetViewColor(PVT_POPUPMENU);
    fPopupFormHeight:= FPopupForm.Height;

    if Assigned(OnPopup) then OnPopup(Self);

    if Assigned(PopupControl) then
      FForm.RollDown(PopupControl, fPopupFormWidth, fPopupFormHeight, False, PopupFocus)
    else
      FForm.RollDown(X, Y, fPopupFormWidth, fPopupFormHeight, PopupFocus);
    Result := True;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEAddressBar
-------------------------------------------------------------------------------}
constructor TCEAddressBar.Create(AOwner: TComponent);
begin
  inherited;
  FolderForm:= TCE_FolderTreeForm.Create(self);
  FolderForm.CloseOnChange:= true;
  FolderForm.ChangeGlobalPathOnChange:= true;
  FormPopup:= TCEFormPopup.Create(self);
  FormPopup.OnPopup:= OnPopup;
  FormPopup.BorderStyle:= pbsSizeableRightBottom;
  FormPopup.PopupFocus:= true;
  FormPopup.PopupForm:= FolderForm;
  Self.EditButton.DropDownArrow:= true;
  Self.EditButton.DropDownMenu:= FormPopup;
  FolderIcon:= TFolderIcon.Create(self);
  FolderIcon.Parent:= Self;
  FolderIcon.Align:= alLeft;
  FolderIcon.Width:= 20;
  RootNamespace:= TNamespace.Create(nil,nil);
  GlobalPathCtrl.RegisterNotify(self);
  Self.HotTrack:= false;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEAddressBar
-------------------------------------------------------------------------------}
destructor TCEAddressBar.Destroy;
begin
  if assigned(RootNamespace) then
  RootNamespace.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Change RootNamespace
-------------------------------------------------------------------------------}
procedure TCEAddressBar.BrowseTo(APIDL: PItemIDList);
begin
  if assigned(RootNamespace) then
  FreeAndNil(RootNamespace);

  RootNamespace:= TNamespace.Create(PIDLMgr.CopyPIDL(APIDL),nil);
  Text:= RootNamespace.NameParseAddress;
  Self.SetSelStart(Length(Text));
  FolderIcon.IconIndex:= RootNamespace.GetIconIndex(false, icSmall);
end;

{*------------------------------------------------------------------------------
  Change Value
-------------------------------------------------------------------------------}
procedure TCEAddressBar.DoValueChanged;
var
  ws: WideString;
begin
  ws:= IncludeTrailingBackslashW(Text);
  if WideDirectoryExists(ws) then
  begin
    if assigned(RootNamespace) then
    FreeAndNil(RootNamespace);

    RootNamespace:= TNamespace.CreateFromFileName(ws);
    Text:= RootNamespace.NameParseAddress;
    Self.SetSelStart(Length(Text));
    FolderIcon.IconIndex:= RootNamespace.GetIconIndex(false, icSmall);
    GlobalPathCtrl.ChangeGlobalPathPIDL(Self, RootNamespace.AbsolutePIDL);
  end;
end;

{*------------------------------------------------------------------------------
  Get's called on global focus change.
-------------------------------------------------------------------------------}
procedure TCEAddressBar.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
  // Do nothing
end;

{*------------------------------------------------------------------------------
  Get's called on global path change (string)
-------------------------------------------------------------------------------}
procedure TCEAddressBar.GlobalPathChanged(Sender: TObject; NewPath: WideString);
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
procedure TCEAddressBar.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  BrowseTo(NewPIDL);
end;

{*------------------------------------------------------------------------------
  On Key Down
-------------------------------------------------------------------------------}
procedure TCEAddressBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_RETURN: DoValueChanged;
  end;
end;

procedure TCEAddressBar.OnPopup(Sender: TObject);
begin
  if assigned(RootNamespace) then
  FolderForm.FolderTree.BrowseToByPIDL(RootNamespace.AbsolutePIDL, true, true, false, true);
end;

{*------------------------------------------------------------------------------
  Get's called on Resize
-------------------------------------------------------------------------------}
procedure TCEAddressBar.Resize;
begin
  inherited;
  UpdateEditRect;
  FormPopup.PopupFormWidth:= Width;
end;

{*------------------------------------------------------------------------------
  Update Edit Rect
-------------------------------------------------------------------------------}
procedure TCEAddressBar.UpdateEditRect;
var
  X1, X2: Integer;
begin
  if not HandleAllocated then Exit;

  X1 := 20;
  X2 := Self.EditButton.Width;

  SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN or EC_RIGHTMARGIN, MakeLong(X1, X2));
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Paint Icon
-------------------------------------------------------------------------------}
procedure TFolderIcon.Paint;
var
  r: TRect;
  x,y: Integer;
begin
  r:= BoundsRect;
  x:= 2;
  y:= Round((r.Bottom - r.Top - 16) / 2);
  Canvas.Brush.Color:= clWindow;
  Canvas.FillRect(BoundsRect);
  if fIconIndex < 0 then
  Exit;
  
  SmallSysImages.Draw(Canvas,x,y,fIconIndex);
end;

{*------------------------------------------------------------------------------
  Set Icon Index
-------------------------------------------------------------------------------}
procedure TFolderIcon.SetIconIndex(const Value: Integer);
begin
  fIconIndex:= Value;
  Paint;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create instance of TCEAddressBarToolbar
-------------------------------------------------------------------------------}
constructor TCEAddressBarToolbar.Create(AOwner: TComponent);
begin
  inherited;
  SetVistaFont(Font);
  Parent:= TWinControl(AOwner);
  self.MinClientHeight:= 22;
  self.ClientHeight:= 22;
  self.Stretch:= true;
  AddressBar:= TCEAddressBar.Create(self);
  AddressBar.Parent:= self;
  AddressBar.Align:= alClient;
end;




end.
