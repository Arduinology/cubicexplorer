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
//  The Original Code is CE_Toolbar.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Toolbar;

interface

uses
  // TB2K, TBX, SpTBX
  SpTBXItem, TB2Item, SpTBXSkins,
  // System Units
  Classes, Windows, SysUtils, Controls, Messages, TntActnList, Graphics, Math;

type
  TTBItemViewerAccess = class(TTBItemViewer);
  
  TCEToolbarItem = class(TSpTBXItem)
  end;

  TCEToolbarSubmenuItem = class(TCEToolbarItem)
  private
    function GetDropdownCombo: Boolean;
    procedure SetDropdownCombo(Value: Boolean);
  protected
    procedure DoPopup(Sender: TTBCustomItem; FromLink: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DropdownCombo: Boolean read GetDropdownCombo write SetDropdownCombo
        default False;
  end;

  TCEToolbar = class(TSpTBXToolbar)
  private
    fLargeImages: Boolean;
  protected
    procedure RightAlignItems; override;
    procedure SetLargeImages(const Value: Boolean); virtual;
  public
    property LargeImages: Boolean read fLargeImages write SetLargeImages;
  end;

  TCEToolbarSeparatorItem = class(TSpTBXSeparatorItem)
  end;

  TCECustomToolbarSpacerItem = class(TSpTBXCustomLabelItem)
  protected
    procedure DoDrawButton(ACanvas: TCanvas; ARect: TRect; ItemInfo:
        TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage; var PaintDefault:
        Boolean); override;
  public
  published
    property CustomHeight;
    property CustomWidth;
    property Options;
  end;

  TCEToolbarFixedSpacerItem = class(TCECustomToolbarSpacerItem)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCEToolbarDynamicSpacerItem = class(TCECustomToolbarSpacerItem)
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CubicExplorer', [TCEToolbar]);
end;

{*------------------------------------------------------------------------------
  Create an instance of TCEToolbarSubmenuItem
-------------------------------------------------------------------------------}
constructor TCEToolbarSubmenuItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisSubMenu, tbisSubitemsEditable,tbisCombo];
end;

{*------------------------------------------------------------------------------
  DoPopup
-------------------------------------------------------------------------------}
procedure TCEToolbarSubmenuItem.DoPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
begin
  // Do nothing?
end;

{*------------------------------------------------------------------------------
  GetDropdownCombo
-------------------------------------------------------------------------------}
function TCEToolbarSubmenuItem.GetDropdownCombo: Boolean;
begin
  Result := tbisCombo in ItemStyle;
end;

{*------------------------------------------------------------------------------
  SetDropdownCombo
-------------------------------------------------------------------------------}
procedure TCEToolbarSubmenuItem.SetDropdownCombo(Value: Boolean);
begin
  if (tbisCombo in ItemStyle) <> Value then begin
    if Value then ItemStyle := ItemStyle + [tbisCombo]
    else ItemStyle := ItemStyle - [tbisCombo];
    Change(True);
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  RightAlignItems
-------------------------------------------------------------------------------}
procedure TCEToolbar.RightAlignItems;
var
  i: Integer;
  spacers: TList;
  spacer: TCEToolbarDynamicSpacerItem;
  size, totalSize, fullSize, dynamicSize, overSize: Integer;
  IV: TTBItemViewer;
  IsRotated: Boolean;
begin
  if (csDestroying in ComponentState)
     or (tstRightAligning in FState)
     //or not Assigned(CurrentDock)
     or (Items.Count <= 0)
     or not Stretch
     //or ((CurrentDock.Width <= 0) and (CurrentDock.Height <= 0))
     or IsUpdating
  then Exit;

  // Set Dynamic Spacers to Zero size if floating
  if Self.Floating then
  begin
    for i:= 0 to Items.Count-1 do
    begin
      if Items.Items[i] is TCEToolbarDynamicSpacerItem then
      TCEToolbarDynamicSpacerItem(Items.Items[i]).CustomWidth:= 0;
    end;
    Exit; // --> EXIT
  end;

  FState := FState + [tstRightAligning];

  spacers:= TList.Create;
  View.ValidatePositions;
  View.BeginUpdate;
  try
    IsRotated:= IsVertical;
    
    if IsRotated then
    fullSize:= Self.ClientHeight
    else
    fullSize:= Self.ClientWidth;

    // calculate total size of items
    totalSize:= 0;
    for i:= 0 to View.ViewerCount - 1 do
    begin
      IV:= View.Viewers[i];
      if not (IV.Item is TCEToolbarDynamicSpacerItem) then
      begin
        if IV.Item.Visible then
        begin
          if IsRotated then
          size:= IV.BoundsRect.Bottom - IV.BoundsRect.Top
          else
          size:= IV.BoundsRect.Right - IV.BoundsRect.Left;
        end;
        totalSize:= totalSize + size;
      end
      else
      begin
        spacers.Add(Pointer(IV.Item));
      end;
    end;

    // Calculate size for spacers
    overSize:= fullSize - totalSize;
    if (overSize > 0) and (spacers.Count > 0) then
    dynamicSize:= Floor(overSize / spacers.Count)
    else
    dynamicSize:= 0;

    // Set spacer sizes
    for i:= 0 to spacers.Count - 1 do
    begin
      spacer:= TCEToolbarDynamicSpacerItem(spacers.Items[i]);
      if i < spacers.Count-1 then
      spacer.CustomWidth:= dynamicSize
      else
      spacer.CustomWidth:= overSize;
      overSize:= overSize - dynamicSize;
    end;
  finally
    View.EndUpdate;
    FState:= FState - [tstRightAligning];
    spacers.Free;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Set Large Images
-------------------------------------------------------------------------------}
procedure TCEToolbar.SetLargeImages(const Value: Boolean);
begin
  fLargeImages:= Value;
end;

{-------------------------------------------------------------------------------
  DoDrawButton
-------------------------------------------------------------------------------}
procedure TCECustomToolbarSpacerItem.DoDrawButton(ACanvas: TCanvas; ARect:
    TRect; ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
    var PaintDefault: Boolean);
begin
  //inherited;
  PaintDefault:= false;
  if Self.GetParentComponent is TSpTBXToolbar then
  begin
    if TSpTBXToolbar(Self.GetParentComponent).IsCustomizing then
    begin
      ACanvas.Brush.Color:= clWhite;
      ACanvas.Brush.Style:= bsSolid;
      ACanvas.FillRect(ARect);
      ACanvas.Brush.Color:= clBlack;
      ACanvas.FrameRect(ARect);
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarFixedSpacerItem
-------------------------------------------------------------------------------}
constructor TCEToolbarFixedSpacerItem.Create(AOwner: TComponent);
begin
  inherited;
  Self.CustomWidth:= 12;
  Self.CustomHeight:= 16;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarDynamicSpacerItem
-------------------------------------------------------------------------------}
constructor TCEToolbarDynamicSpacerItem.Create(AOwner: TComponent);
begin
  inherited;
  Self.CustomWidth:= 12;
  Self.CustomHeight:= 16;
end;

end.
