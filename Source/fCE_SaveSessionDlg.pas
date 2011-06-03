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
//  The Original Code is fCE_SaveSessionDlg.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_SaveSessionDlg;

interface

uses
  // Tnt
  TntStdCtrls, TntDialogs, TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCESaveSessionDlg = class(TTntForm)
    TntLabel1: TTntLabel;
    SessionCombo: TTntComboBox;
    but_save: TTntButton;
    but_cancel: TTntButton;
    procedure but_saveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SessionComboChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure PopulateSessionNames;
    { Public declarations }
  end;

implementation

uses
  CE_Sessions, CE_LanguageEngine, CE_VistaFuncs;

{$R *.dfm}

{-------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCESaveSessionDlg.FormCreate(Sender: TObject);
begin
  PopulateSessionNames;
  SetVistaFont(Font);
  CEGlobalTranslator.TranslateComponent(Self);
end;

{-------------------------------------------------------------------------------
  Populate Session Names
-------------------------------------------------------------------------------}
procedure TCESaveSessionDlg.PopulateSessionNames;
var
  i: Integer;
  session: TCESessionItem;
begin
  SessionCombo.Items.Clear;
  for i:= 0 to GlobalSessions.Sessions.Count - 1 do
  begin
    session:= GlobalSessions.Sessions.Items[i];
    SessionCombo.Items.Add(session.Name);
  end;
  if GlobalSessions.ActiveSessionIndex > -1 then
  begin
    SessionCombo.ItemIndex:= GlobalSessions.ActiveSessionIndex;
    but_save.Enabled:= SessionCombo.Text <> '';
  end;
end;

{-------------------------------------------------------------------------------
  On SessionCombo change
-------------------------------------------------------------------------------}
procedure TCESaveSessionDlg.SessionComboChange(Sender: TObject);
begin
  but_save.Enabled:= SessionCombo.Text <> '';
end;

{-------------------------------------------------------------------------------
  Save button click
-------------------------------------------------------------------------------}
procedure TCESaveSessionDlg.but_saveClick(Sender: TObject);
begin
  if SessionCombo.Items.IndexOf(SessionCombo.Text) > -1 then
  begin
    if (TaskDialog(Application.MainFormHandle,
                   _('Confirm'),
                   _('Override existing session?'),
                   _('Do you want to override existing session?'),
                   TD_ICON_QUESTION,
                   TD_BUTTON_YES + TD_BUTTON_NO) = TD_RESULT_YES) then
    ModalResult:= mrOK;
  end
  else
  ModalResult:= mrOK;

  if ModalResult = mrOK then
  Self.CloseModal;
end;

end.
