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
//  The Original Code is fCE_FolderPanel.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_FolderPanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_GlobalCtrl, dCE_Images, CE_VistaFuncs, CE_FolderTree,
  CE_AppSettings,
  // VSTools
  VirtualTrees, VirtualExplorerTree, MPCommonUtilities, MPShellUtilities,
  VirtualShellNotifier,
  // PNG Controls
  PngImageList,
  // TB2K, SpTBX
  TB2Dock, SpTBXItem,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, ImgList;

type
  TControlHack = class(TControl);

  TCEFolderPanelSettings = class;
  
  TCEFolderPanel = class(TCECustomDockableForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fSettings: TCEFolderPanelSettings;
  protected
    procedure FolderTreeSelectedChange(Node: PVirtualNode);

    procedure FolderTreeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FolderTreeIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: WideString; var Result: Integer);
    procedure FolderTreeKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); override;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); override;
        stdcall;
  public
    FolderTree: TCEFolderTree;
    procedure OnEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
        TColumnIndex);
  published
    property Settings: TCEFolderPanelSettings read fSettings write fSettings;
  end;

  TCEFolderPanelSettings = class(TPersistent)
  private
    fOpenInNewTab: Boolean;
    function GetAutoCollapse: Boolean;
    function GetAutoExpand: Boolean;
    function GetBrowseZipFolders: Boolean;
    procedure SetAutoCollapse(const Value: Boolean);
    procedure SetAutoExpand(const Value: Boolean);
    procedure SetBrowseZipFolders(const Value: Boolean);
  public
    FolderPanel: TCEFolderPanel;
  published
    property AutoCollapse: Boolean read GetAutoCollapse write SetAutoCollapse;
    property AutoExpand: Boolean read GetAutoExpand write SetAutoExpand;
    property BrowseZipFolders: Boolean read GetBrowseZipFolders write
        SetBrowseZipFolders;
    property OpenInNewTab: Boolean read fOpenInNewTab write fOpenInNewTab;
  end;

var
  CEFolderPanel: TCEFolderPanel;

implementation

uses
  dCE_Actions, Main;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called when TCEFolderPanel is created.
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FormCreate(Sender: TObject);
begin
  inherited;
  fSettings:= TCEFolderPanelSettings.Create;
  fSettings.FolderPanel:= self;
  FolderTree:= TCEFolderTree.Create(self);
  FolderTree.Parent:= Self;
  FolderTree.Align:= alClient;
  FolderTree.OnSelectedChange:= FolderTreeSelectedChange;
  FolderTree.OnIncrementalSearch:= FolderTreeIncrementalSearch;
  FolderTree.OnKeyAction:= FolderTreeKeyAction;
  FolderTree.OnMouseDown:= FolderTreeMouseDown;
  FolderTree.IncrementalSearch:= isVisibleOnly;
  FolderTree.Active:= true;
  FolderTree.OnEdited:= OnEdited;

  FolderTree.AutoCollapse:= true;
  FolderTree.AutoExpand:= true;
  FolderTree.Indent:= 24;
  FolderTree.TabOrder:= 1;
  
  SetDesktopIconFonts(FolderTree.Font);
  TopDock.Name:= 'FolderPanel_TopDock';
  BottomDock.Name:= 'FolderPanel_BottomDock';
  Caption:= 'Folders';
  GlobalPathCtrl.RegisterNotify(self);
  ImageList:= CE_Images.SmallIcons;
  ImageIndex:= 28;
  GlobalFocusCtrl.CtrlList.Add(FolderTree);
  TControlHack(FolderTree).OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;

  GlobalAppSettings.AddItem('FolderPanel', Settings, true);
end;

{-------------------------------------------------------------------------------
  On Destroy
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FormDestroy(Sender: TObject);
begin
  fSettings.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called on focused node change
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FolderTreeSelectedChange(Node: PVirtualNode);
var
  NS: TNamespace;
begin
  if FolderTree.ValidateNamespace(Node, NS) then
  begin
    if Settings.OpenInNewTab then
    OpenFolderInTab(FolderTree, NS.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect)
    else
    GlobalPathCtrl.ChangeGlobalPathPIDL(Self, NS.AbsolutePIDL);
  end;
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  if FolderTree.AutoCollapse then
  FolderTree.FullCollapse;
  FolderTree.BrowseTo(NewPath, true, true, false, false);

  //if FolderTree.AutoCollapse then
  FolderTree.ScrollIntoView(FolderTree.FocusedNode,true,true);
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  if FolderTree.AutoCollapse then
  FolderTree.FullCollapse;
  FolderTree.BrowseToByPIDL(NewPIDL, true, true, false, false);

  //if FolderTree.AutoCollapse then
  FolderTree.ScrollIntoView(FolderTree.FocusedNode,true,true);
end;

{*------------------------------------------------------------------------------
  Mouse Down event
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FolderTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  node: PVirtualNode;
  NS: TNamespace;
begin
  if (Button = mbMiddle) or (Shift = [ssLeft, ssAlt]) or (Shift = [ssLeft, ssAlt, ssShift])  then
  begin
    node:= FolderTree.GetNodeAt(X,Y);
    if assigned(node) then
    begin
      FolderTree.ValidateNamespace(node, NS);
      if assigned(NS) then
      begin
        if (Shift = [ssLeft, ssAlt]) then
        begin
          if Settings.OpenInNewTab then
          GlobalPathCtrl.ChangeGlobalPathPIDL(Self, NS.AbsolutePIDL)
          else
          OpenFolderInTab(FolderTree, NS.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect);
        end
        else
        begin
          if ssShift in Shift then
          OpenFolderInTab(FolderTree, NS.AbsolutePIDL, not MainForm.TabSet.Settings.OpenTabSelect)
          else
          OpenFolderInTab(FolderTree, NS.AbsolutePIDL, MainForm.TabSet.Settings.OpenTabSelect);
        end;
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Incremental Search
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FolderTreeIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: WideString; var Result: Integer);

  function DoCompare(Str1: WideString; Str2: WideString): Integer;
  begin
    if IsUnicode then
    Result:= lstrcmpiW(PWideChar(Str1), PWideChar(Str2))
    else
    Result:= lstrcmpi(PChar(string(Str1)), PChar(string(Str2)));
  end;

var
  CompareStr: WideString;
begin
  CompareStr:= FolderTree.Text[Node,-1];
  SetLength(CompareStr, Length(SearchText));
  Result:= DoCompare(CompareStr, SearchText);
end;

{*------------------------------------------------------------------------------
  On Key Action
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.FolderTreeKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  case CharCode of
    VK_F5: DoDefault:= false;  
  end;
  inherited;
end;

{*------------------------------------------------------------------------------
  On Edited
-------------------------------------------------------------------------------}
procedure TCEFolderPanel.OnEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
    Column: TColumnIndex);
begin
  FolderTree.RefreshNode(Node);
  FolderTreeSelectedChange(Node);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get/Set AutoCollapse
-------------------------------------------------------------------------------}
function TCEFolderPanelSettings.GetAutoCollapse: Boolean;
begin
  Result:= FolderPanel.FolderTree.AutoCollapse;
end;
procedure TCEFolderPanelSettings.SetAutoCollapse(const Value: Boolean);
begin
  FolderPanel.FolderTree.AutoCollapse:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set AutoExpand
-------------------------------------------------------------------------------}
function TCEFolderPanelSettings.GetAutoExpand: Boolean;
begin
  Result:= FolderPanel.FolderTree.AutoExpand;
end;
procedure TCEFolderPanelSettings.SetAutoExpand(const Value: Boolean);
begin
  FolderPanel.FolderTree.AutoExpand:= Value;
end;

{-------------------------------------------------------------------------------
  Get/Set BrowseZipFolders
-------------------------------------------------------------------------------}
function TCEFolderPanelSettings.GetBrowseZipFolders: Boolean;
begin
  Result:= FolderPanel.FolderTree.BrowseZipFolders;
end;
procedure TCEFolderPanelSettings.SetBrowseZipFolders(const Value: Boolean);
begin
  FolderPanel.FolderTree.BrowseZipFolders:= Value;
end;

end.
