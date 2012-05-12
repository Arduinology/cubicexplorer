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
//  The Original Code is fCE_QuickViewTab.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_QuickViewTab;

interface

uses
  // CE Units
  fCE_TabPage, CE_LanguageEngine, dCE_Images, CE_Utils, fCE_QuickView,
  CE_GlobalCtrl, 
  // VSTools
  MPCommonUtilities, MPCommonObjects,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SpTBXItem, CE_Toolbar, TB2Item, ComCtrls, SpTBXControls,
  CE_SpTBXItems, TB2Dock, TB2Toolbar, ExtCtrls;

type
  TCEQuickViewPage = class(TCECustomTabPage)
  private
    fActiveFile: WideString;
    { Private declarations }
  protected
    function GetSettingsClass: TCECustomTabPageSettingsClass; override;
    procedure HandleCurrentFileChange(Sender: TObject); virtual;
    procedure HandleDetach(Sender: TObject); virtual;
  public
    QuickView: TCEQuickView;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure OpenFile(AFilePath: WideString);
    procedure SaveToStream(AStream: TStream); override;
    procedure SelectPage; override;
    function TabClosing: Boolean; override;
    procedure UpdateCaption; override;
    property ActiveFile: WideString read fActiveFile;
  end;

type
  TCEQuickViewPageSettings = class(TCECustomTabPageSettings)
  private
  protected
    fQuickViewPage: TCEQuickViewPage;
    function GetPath: WideString;
    function GetRememberPanelLayout: Boolean; override;
    function GetRememberInnerToolbarLayout: Boolean; override;
    function GetRememberOuterToolbarLayout: Boolean; override;
    procedure SetPath(const Value: WideString);
  public
    property QuickViewPage: TCEQuickViewPage read fQuickViewPage write
        fQuickViewPage;
  published
    property Path: WideString read GetPath write SetPath;
  end;

implementation

uses
  CE_SpTabBar;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEQuickViewPage
-------------------------------------------------------------------------------}
constructor TCEQuickViewPage.Create(AOwner: TComponent);
begin
  inherited;
  TCEQuickViewPageSettings(Settings).QuickViewPage:= Self;
  
  QuickView:= TCEQuickView.Create(nil);
  QuickView.Parent:= Self;
  QuickView.Align:= alClient;
  QuickView.Active:= true;
  QuickView.ShowPreview:= false;
  QuickView.OnDetach:= HandleDetach;
  QuickView.OnCurrentFileChange:= HandleCurrentFileChange;
  QuickView.OnEditorClose:= HandleDetach;
  CEGlobalTranslator.TranslateComponent(QuickView);
  Layout:= 'QuickView';

  TabPageClassList
end;

{-------------------------------------------------------------------------------
  Destroy TCEQuickViewPage
-------------------------------------------------------------------------------}
destructor TCEQuickViewPage.Destroy;
begin
  QuickView.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Get Settings Class
-------------------------------------------------------------------------------}
function TCEQuickViewPage.GetSettingsClass: TCECustomTabPageSettingsClass;
begin
  Result:= TCEQuickViewPageSettings;
end;

{-------------------------------------------------------------------------------
  HandleCurrentFileChange
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.HandleCurrentFileChange(Sender: TObject);
begin
  fActiveFile:= QuickView.CurrentFilePath;
  UpdateCaption;
end;

{-------------------------------------------------------------------------------
  Handle Detach
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.HandleDetach(Sender: TObject);
begin
  if assigned(Self.fTabItem) then
  begin
    if (Self.fTabItem is TCESpTabItem) then
    TCESpTabItem(Self.fTabItem).CloseTab
    else
    Self.fTabItem.TabClose;
  end;
end;

{-------------------------------------------------------------------------------
  Load from stream
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.LoadFromStream(AStream: TStream);
var
  ws: WideString;
begin
  LoadWideString(AStream, ws);
  if ws <> '' then
  OpenFile(ws);
end;

{-------------------------------------------------------------------------------
  Open File
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.OpenFile(AFilePath: WideString);
begin
  if WideFileExists(AFilePath) then
  begin
    fActiveFile:= AFilePath;
    UpdateCaption;
    Application.ProcessMessages;
    QuickView.ActiveFilePath:= AFilePath;
  end
  else
  begin
    fActiveFile:= '';
    UpdateCaption;
  end;
end;

{-------------------------------------------------------------------------------
  Save to stream
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.SaveToStream(AStream: TStream);
begin
  SaveWideString(AStream, ActiveFile);
end;

{*------------------------------------------------------------------------------
  Select Page
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.SelectPage;
begin
  GlobalPathCtrl.ActivePage:= Self;
  GlobalPathCtrl.GlobalPathCaption:= ActiveFile;
end;

{*------------------------------------------------------------------------------
  Get's called when tab is closing.
-------------------------------------------------------------------------------}
function TCEQuickViewPage.TabClosing: Boolean;
begin
  Result:= QuickView.CanClose;
  
  if Result and (GlobalPathCtrl.ActivePage = Self) then
  GlobalPathCtrl.ActivePage:= nil;
end;

{*------------------------------------------------------------------------------
  Update Tab item Caption
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.UpdateCaption;
var
  ws: WideString;
begin
  if ActiveFile = '' then
  begin
    ws:= QuickView.GetCurrentTitle;
    if ws <> '' then
    TabCaption:= ws
    else
    TabCaption:= _('QuickView');

    TabItem.Images:= CE_Images.SmallIcons;
    TabItem.ImageIndex:= 20;
  end
  else
  begin
    ws:= QuickView.GetCurrentTitle;
    if ws <> '' then
    TabCaption:= ws
    else
    TabCaption:= WideExtractFileName(ActiveFile);
    TabItem.Images:= SmallSysImages;
    TabItem.ImageIndex:= GetIconIndex(ActiveFile);
  end;

  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.GlobalPathCaption:= ActiveFile;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Get RememberPanelLayout
-------------------------------------------------------------------------------}
function TCEQuickViewPageSettings.GetRememberPanelLayout: Boolean;
begin
  Result:= GlobalQuickViewSettings.RememberPanelLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberInnerToolbarLayout
-------------------------------------------------------------------------------}
function TCEQuickViewPageSettings.GetRememberInnerToolbarLayout: Boolean;
begin
  Result:= GlobalQuickViewSettings.RememberInnerToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberOuterToolbarLayout
-------------------------------------------------------------------------------}
function TCEQuickViewPageSettings.GetRememberOuterToolbarLayout: Boolean;
begin
  Result:= GlobalQuickViewSettings.RememberOuterToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get/Set Path
-------------------------------------------------------------------------------}
function TCEQuickViewPageSettings.GetPath: WideString;
begin
  Result:= QuickViewPage.QuickView.ActiveFilePath;
end;
procedure TCEQuickViewPageSettings.SetPath(const Value: WideString);
begin
  QuickViewPage.OpenFile(Value);
end;

{##############################################################################}

initialization
  TabPageClassList.RegisterClass('QuickView', TCEQuickViewPage, TCEQuickViewPageSettings);

finalization

end.
