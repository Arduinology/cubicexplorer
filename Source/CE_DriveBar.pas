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
//  The Original Code is CE_DriveBar.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_DriveBar;

interface

uses
  // CE Units
  CE_GlobalCtrl, dCE_Actions, CE_VistaFuncs,
  // Toolbar2K
  TB2Item,
  // SpTBXLib
  SpTBXItem, SpTBXSkins,
  // VSTools
  MPCommonUtilities, MPCommonObjects, MPShellUtilities, VirtualShellNotifier,
  VirtualResources,
  // Tnt Ctrls
  TntWideStrUtils,
  // System Units
  Classes, Graphics, ImgList, Windows, ShlObj, SysUtils, Controls, Messages,
  ActiveX;

type
  TCEShellToolbarItem = class(TSpTBXCustomItem)
  private
    fDriveLetter: WideString;
    fIsDrive: Boolean;
    fShortName: Boolean;
  protected
    procedure DoDrawCaption(ACanvas: TCanvas; ClientAreaRect: TRect; State:
        TSpTBXSkinStatesType; var ACaption: WideString; var CaptionRect: TRect; var
        CaptionFormat: Cardinal; IsTextRotated: Boolean; const PaintStage:
        TSpTBXPaintStage; var PaintDefault: Boolean); override;
    procedure DoDrawImage(ACanvas: TCanvas; State: TSpTBXSkinStatesType; const
        PaintStage: TSpTBXPaintStage; var AImageList: TCustomImageList; var
        AImageIndex: Integer; var ARect: TRect; var PaintDefault: Boolean);
        override;
  public
    Namespace: TNamespace;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure LoadNamespacePIDL(APIDL: PItemIDList);
    procedure LoadNamespace(Path: WideString);
    property DriveLetter: WideString read fDriveLetter write fDriveLetter;
    property IsDrive: Boolean read fIsDrive write fIsDrive;
    property ShortName: Boolean read fShortName write fShortName;
  end;

  TCEDriveToolbar = class(TSpTBXToolbar)
  protected
    function CanItemClick(Item: TTBCustomItem; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer): Boolean; override;
    procedure DoItemClick(Item: TTBCustomItem; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer); override;
    procedure WMShellNotify(var Msg: TMessage); message WM_SHELLNOTIFY;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Populate;
  end;

implementation

uses
  fCE_FolderPanel, Main;

{*------------------------------------------------------------------------------
  Create an instance of TCEShellToolbarItem
-------------------------------------------------------------------------------}
constructor TCEShellToolbarItem.Create(AOwner: TComponent);
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEShellToolbarItem
-------------------------------------------------------------------------------}
destructor TCEShellToolbarItem.Destroy;
begin
  if assigned(Namespace) then
  FreeAndNil(Namespace);
  inherited;
end;

{*------------------------------------------------------------------------------
  Handle Click
-------------------------------------------------------------------------------}
procedure TCEShellToolbarItem.Click;
begin
  inherited;
  if assigned(Namespace) then
  begin
    if CEFolderPanel.NewTabByDefault then
    OpenFolderInTab(self, Namespace.AbsolutePIDL, MainForm.TabSet.OpenTabSelect)
    else
    GlobalPathCtrl.ChangeGlobalPathPIDL(self.Owner,Namespace.AbsolutePIDL);
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when Caption is drawn
-------------------------------------------------------------------------------}
procedure TCEShellToolbarItem.DoDrawCaption(ACanvas: TCanvas; ClientAreaRect:
    TRect; State: TSpTBXSkinStatesType; var ACaption: WideString; var
    CaptionRect: TRect; var CaptionFormat: Cardinal; IsTextRotated: Boolean;
    const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
//  if assigned(Namespace) then
//  begin
//    if IsDrive and ShortName then
//    Caption:= DriveLetter
//    else
//    Caption:= Namespace.NameNormal;
//  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when Image is drawn
-------------------------------------------------------------------------------}
procedure TCEShellToolbarItem.DoDrawImage(ACanvas: TCanvas; State:
    TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage; var AImageList:
    TCustomImageList; var AImageIndex: Integer; var ARect: TRect; var
    PaintDefault: Boolean);
begin
  if assigned(Namespace) then
  AImageIndex:= Namespace.GetIconIndex(false,icSmall);
  inherited;
end;

{*------------------------------------------------------------------------------
  Load namespace from PIDL
-------------------------------------------------------------------------------}
procedure TCEShellToolbarItem.LoadNamespacePIDL(APIDL: PItemIDList);
begin
  if assigned(Namespace) then
  FreeAndNil(Namespace);

  Namespace:= TNamespace.Create(APIDL,nil);
  Caption:= Namespace.NameNormal;
  ImageIndex:= Namespace.GetIconIndex(false,icSmall);
end;

{*------------------------------------------------------------------------------
  Load namespace from path
-------------------------------------------------------------------------------}
procedure TCEShellToolbarItem.LoadNamespace(Path: WideString);
begin
  if assigned(Namespace) then
  FreeAndNil(Namespace);

  Namespace:= TNamespace.CreateFromFileName(Path);
  ImageIndex:= Namespace.GetIconIndex(false,icSmall);
end;

{##############################################################################}

constructor TCEDriveToolbar.Create(AOwner: TComponent);
begin
  inherited;
  Customizable:= false;
  SetVistaFont(Font);
  Images:= SmallSysImages;
  ChangeNotifier.RegisterShellChangeNotify(Self);
end;

destructor TCEDriveToolbar.Destroy;
begin
  ChangeNotifier.UnRegisterShellChangeNotify(Self);
  inherited;
end;

{*------------------------------------------------------------------------------
  Can Item Click
-------------------------------------------------------------------------------}
function TCEDriveToolbar.CanItemClick(Item: TTBCustomItem; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  if (Button = mbMiddle) or (Shift = [ssCtrl,ssLeft]) or (Button = mbRight) then
  Result:= false
  else
  Result:= true;
end;

{*------------------------------------------------------------------------------
  Do Item Click
-------------------------------------------------------------------------------}
procedure TCEDriveToolbar.DoItemClick(Item: TTBCustomItem; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  shitem: TCEShellToolbarItem;
begin
  if (Button = mbMiddle) or (Shift = [ssCtrl,ssLeft]) then
  begin
    if Item is TCEShellToolbarItem then
    begin
      shitem:= TCEShellToolbarItem(Item);
      if assigned(shitem.Namespace) then
      OpenFolderInTab(self,shitem.Namespace.AbsolutePIDL, MainForm.TabSet.OpenTabSelect);
    end;
  end
  else if Button = mbRight then
  begin
    if Item is TCEShellToolbarItem then
    begin
      shitem:= TCEShellToolbarItem(Item);
      if assigned(shitem.Namespace) then
      shitem.Namespace.ShowContextMenu(Self,nil,nil,nil);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Populate Toolbar
-------------------------------------------------------------------------------}
procedure TCEDriveToolbar.Populate;

const
  FLAGS = SHCONTF_FOLDERS;

var
  Desktop, Folder: IShellFolder;
  EnumIDList: IEnumIDList;
  PIDL, SubPIDL: PItemIDList;
  celtFetched: LongWord;
  NS: TNamespace;
  IsFloppy, IsDrive: Boolean;
  //IsReadOnly: Boolean;
  item: TCEShellToolbarItem;
begin  
  Self.BeginUpdate;
  Self.Items.Clear;
  try
    SHGetDesktopFolder(Desktop);
    SHGetSpecialFolderLocation(0, CSIDL_DRIVES, PIDL);
    if Assigned(PIDL) then
    begin
      if Desktop.BindToObject(PIDL, nil, IShellFolder, Pointer(Folder)) = S_OK then
      begin
        if Folder.EnumObjects(0, FLAGS, EnumIDList) = NOERROR then
        begin
          while EnumIDList.Next(1, SubPIDL, celtFetched) = NOERROR do
          begin
            NS:= TNamespace.Create(PIDLMgr.AppendPIDL(PIDL, SubPIDL), nil);

            IsDrive := WideIsDrive(NS.NameForParsing);
            IsFloppy :=  IsDrive and (Char(NS.NameForParsing[1]) in ['A', 'B']);
            //IsReadOnly := (not IsFloppy) and NS.ReadOnly;
            if IsDrive and not IsFloppy then
            begin
              item:= TCEShellToolbarItem.Create(Self);
              item.IsDrive:= true;
              item.Namespace:= NS;
              item.DriveLetter:= NS.NameForParsing[1];
              item.ShortName:= true;
              item.Caption:= item.DriveLetter;
              item.Hint:= NS.NameInFolder;
              item.ImageIndex:= item.Namespace.GetIconIndex(false,icSmall);
              item.DisplayMode:= nbdmImageAndText;
              Self.Items.Add(item);
            end
            else
            NS.Free;

            CoTaskMemFree(SubPIDL);
          end
        end
      end;
      CoTaskMemFree(PIDL);
    end;  
  finally
    Self.Realign;
    Self.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Handle WMShellNotify messages
-------------------------------------------------------------------------------}
procedure TCEDriveToolbar.WMShellNotify(var Msg: TMessage);
var
  ShellEventList: TVirtualShellEventList;
  ShellEvent: TVirtualShellEvent;
  List: TList;
  i, Count: Integer;
begin
  ShellEventList := TVirtualShellEventList( Msg.wParam);
  List := ShellEventList.LockList;
  try
    Count := List.Count;
    for i := 0 to Count - 1 do
    begin
      ShellEvent := TVirtualShellEvent(List.Items[i]);
      case ShellEvent.ShellNotifyEvent of
        vsneDriveAdd,vsneDriveAddGUI,vsneDriveRemoved,vsneUpdateImage,vsneUpdateDir: Populate;
      end;
    end;
  finally
    ShellEventList.UnlockList;
    ShellEventList.Release;
  end;
end;


end.
