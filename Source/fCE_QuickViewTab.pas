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
  fCE_TabPage, CE_QuickView, CE_LanguageEngine, dCE_Images, CE_Utils,
  CE_GlobalCtrl, 
  // VSTools
  MPCommonUtilities, MPCommonObjects,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCEQuickViewPage = class(TCECustomTabPage)
  private
    fActiveFile: WideString;
    { Private declarations }
  protected
    function GetSettingsClass: TCECustomTabPageSettingsClass; override;
  public
    Viewer: TCEQuickView;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenFile(AFilePath: WideString);
    procedure SelectPage; override;
    function TabClosing: Boolean; override;
    procedure UpdateCaption; override;
    property ActiveFile: WideString read fActiveFile write fActiveFile;
  end;

type
  TCEQuickViewPageSettings = class(TCECustomTabPageSettings)
  private
  protected
    function GetRememberPanelLayout: Boolean; override;
    function GetRememberInnerToolbarLayout: Boolean; override;
    function GetRememberOuterToolbarLayout: Boolean; override;
  public
  published
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEQuickViewPage
-------------------------------------------------------------------------------}
constructor TCEQuickViewPage.Create(AOwner: TComponent);
begin
  inherited;
  Viewer:= TCEQuickView.Create(nil);
  Viewer.Parent:= Self;
  Viewer.Align:= alClient;
  Layout:= 'QuickView';
end;

{-------------------------------------------------------------------------------
  Destroy TCEQuickViewPage
-------------------------------------------------------------------------------}
destructor TCEQuickViewPage.Destroy;
begin
  Viewer.Free;
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
  Open File
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.OpenFile(AFilePath: WideString);
begin
  if WideFileExists(AFilePath) then
  begin
    ActiveFile:= AFilePath;
    UpdateCaption;
    Application.ProcessMessages;
    Viewer.AutoLoadFile(AFilePath);
  end
  else
  begin
    ActiveFile:= '';
    UpdateCaption;
  end;
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
  Result:= true;
  if GlobalPathCtrl.ActivePage = Self then
  GlobalPathCtrl.ActivePage:= nil;
end;

{*------------------------------------------------------------------------------
  Update Tab item Caption
-------------------------------------------------------------------------------}
procedure TCEQuickViewPage.UpdateCaption;
begin
  if ActiveFile = '' then
  begin
    TabCaption:= _('QuickView');
    TabItem.Images:= CE_Images.SmallIcons;
    TabItem.ImageIndex:= 20;
  end
  else
  begin
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
  Result:= QuickViewSettings.RememberPanelLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberInnerToolbarLayout
-------------------------------------------------------------------------------}
function TCEQuickViewPageSettings.GetRememberInnerToolbarLayout: Boolean;
begin
  Result:= QuickViewSettings.RememberInnerToolbarLayout;
end;

{-------------------------------------------------------------------------------
  Get RememberOuterToolbarLayout
-------------------------------------------------------------------------------}
function TCEQuickViewPageSettings.GetRememberOuterToolbarLayout: Boolean;
begin
  Result:= QuickViewSettings.RememberOuterToolbarLayout;
end;

end.
