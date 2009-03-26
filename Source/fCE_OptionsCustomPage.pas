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
//  The Original Code is fCE_OptionsCustomPage.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsCustomPage;

interface

uses
  // CE Units
  CE_SettingsIntf,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs;

type
  TCEOptionsCustomPageClass = class of TCEOptionsCustomPage;
  TCEOptionsCustomPage = class(TFrame, ICESettingsHandler)
    procedure HandleChange(Sender: TObject);
  private
    fOptionsDialog: TForm;
  protected
    fImageIndex: Integer;
    fPageName: WideString;
    fPagePath: WideString;
    fPageTitle: WideString;
  public
    procedure ApplySettings; virtual;
    procedure LoadFromStorage(Storage: ICESettingsStorage); virtual; stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); virtual; stdcall;
    procedure SetModified;
    property ImageIndex: Integer read fImageIndex write fImageIndex;
    property OptionsDialog: TForm read fOptionsDialog write fOptionsDialog;
    property PageName: WideString read fPageName write fPageName;
    property PagePath: WideString read fPagePath write fPagePath;
    property PageTitle: WideString read fPageTitle write fPageTitle;
  end;

implementation

{$R *.dfm}

uses
  fCE_OptionsDialog;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.ApplySettings;
begin
  // Override from descendant
end;

{-------------------------------------------------------------------------------
  Handle Change
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.HandleChange(Sender: TObject);
begin
  SetModified;
end;

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.LoadFromStorage(Storage: ICESettingsStorage);
begin
  // Override from descendant
end;

{-------------------------------------------------------------------------------
  Save To Storage
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.SaveToStorage(Storage: ICESettingsStorage);
begin
  // Override from descendant
end;

{-------------------------------------------------------------------------------
  SetModified
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.SetModified;
begin
  if assigned(fOptionsDialog) then
  TCEOptionsDialog(fOptionsDialog).Modified:= true;
end;

end.
