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
//  The Original Code is fCE_OptionsPage_Display_FileView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_Display_FileView;

interface

uses
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, CE_LanguageEngine,
  // Tnt
  TntStdCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCE_OptionsPage_Display_FileView = class(TCEOptionsCustomPage)
    check_fullrowselect: TTntCheckBox;
    check_selectprev: TTntCheckBox;
    check_autoselect: TTntCheckBox;
    check_autosize_liststyle: TTntCheckBox;
    check_sortfoldersfirst: TTntCheckBox;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

var
  CE_OptionsPage_Display_FileView: TCE_OptionsPage_Display_FileView;

implementation

uses
  fCE_FileView;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Display
-------------------------------------------------------------------------------}
constructor TCE_OptionsPage_Display_FileView.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('Fileview');
  PageTitle:= _('Fileview Settings');
  PagePath:= 'Display/Fileview';
  ImageIndex:= 5;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.ApplySettings;
begin
  GlobalFileViewSettings.FullRowSelect:= check_fullrowselect.Checked;
  GlobalFileViewSettings.SelectPreviousFolder:= check_selectprev.Checked;
  GlobalFileViewSettings.AutoSelectFirstItem:= check_autoselect.Checked;
  GlobalFileViewSettings.AutosizeListViewStyle:= check_autosize_liststyle.Checked;
  GlobalFileViewSettings.SortFolderFirstAlways:= check_sortfoldersfirst.Checked;
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.RefreshSettings;
begin
  check_fullrowselect.Checked:= GlobalFileViewSettings.FullRowSelect;
  check_selectprev.Checked:= GlobalFileViewSettings.SelectPreviousFolder;
  check_autoselect.Checked:= GlobalFileViewSettings.AutoSelectFirstItem;
  check_autosize_liststyle.Checked:= GlobalFileViewSettings.AutosizeListViewStyle;
  check_sortfoldersfirst.Checked:= GlobalFileViewSettings.SortFolderFirstAlways;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCE_OptionsPage_Display_FileView);

finalization

end.
