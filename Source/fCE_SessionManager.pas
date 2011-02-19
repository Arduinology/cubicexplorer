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
//  The Original Code is fCE_SessionManager.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_SessionManager;

interface

uses
  // CubicExplorer
  CE_Sessions,
  // VirtualTrees
  VirtualTrees,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SpTBXControls, SpTBXEditors, SpTBXItem,
  SpTBXDkPanels, SpTBXTabs, TB2Item, TntStdCtrls;

type
  TCESessionManager = class(TForm)
    SpTBXLabel1: TSpTBXLabel;
    edit_name: TSpTBXEdit;
    group_loadsave: TSpTBXGroupBox;
    check_tabs: TSpTBXCheckBox;
    check_bookmarks: TSpTBXCheckBox;
    check_layout: TSpTBXCheckBox;
    check_autosave: TSpTBXCheckBox;
    but_delete: TSpTBXButton;
    SpTBXPanel2: TSpTBXPanel;
    SpTBXButton1: TSpTBXButton;
    SpTBXTabControl1: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    list_sessions: TSpTBXListBox;
    procedure but_deleteClick(Sender: TObject);
    procedure check_autosaveClick(Sender: TObject);
    procedure edit_nameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure list_sessionsClick(Sender: TObject);
  private
    fSessionPropertiesEnabled: Boolean;
    procedure SetSessionPropertiesEnabled(const Value: Boolean);
    { Private declarations }
  protected
    fSelectedSession: TCESessionItem;
    procedure PopulateSessionsList;
  public
    property SessionPropertiesEnabled: Boolean read fSessionPropertiesEnabled write
        SetSessionPropertiesEnabled;
    { Public declarations }
  end;

var
  CESessionManager: TCESessionManager;

implementation

uses
  CE_LanguageEngine;

{$R *.dfm}

{-------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCESessionManager.FormCreate(Sender: TObject);
begin
  SessionPropertiesEnabled:= false;
  PopulateSessionsList;
  list_sessions.ItemIndex:= GlobalSessions.ActiveSessionIndex;
  list_sessionsClick(self);  
end;

{-------------------------------------------------------------------------------
  On Session Name Change
-------------------------------------------------------------------------------}
procedure TCESessionManager.edit_nameChange(Sender: TObject);
begin
  if SessionPropertiesEnabled and assigned(fSelectedSession) then
  begin
    if fSelectedSession.Name <> edit_name.Text then
    begin
      fSelectedSession.Name:= edit_name.Text;
      list_sessions.Items.Strings[list_sessions.ItemIndex]:= fSelectedSession.Name;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set SessionPropertiesEnabled
-------------------------------------------------------------------------------}
procedure TCESessionManager.SetSessionPropertiesEnabled(const Value: Boolean);
begin
  fSessionPropertiesEnabled:= Value;
  edit_name.Enabled:= SessionPropertiesEnabled;

  // TODO: Session SaveLoadItems
  //check_tabs.Enabled:= SessionPropertiesEnabled;
  //check_bookmarks.Enabled:= SessionPropertiesEnabled;
  //check_layout.Enabled:= SessionPropertiesEnabled;

  check_autosave.Enabled:= SessionPropertiesEnabled;
  but_delete.Enabled:= SessionPropertiesEnabled;
  if not SessionPropertiesEnabled then
  begin
    edit_name.Text:= '';
    check_tabs.Checked:= false;
    check_bookmarks.Checked:= false;
    check_layout.Checked:= false;
    check_autosave.Checked:= false;
  end;
end;

{-------------------------------------------------------------------------------
  On Delete Click
-------------------------------------------------------------------------------}
procedure TCESessionManager.but_deleteClick(Sender: TObject);
var
  old_index: Integer;
begin
  if (MessageDlg(_('Are you sure you want to delete this session?'), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    GlobalSessions.Sessions.DeleteSession(list_sessions.ItemIndex);
    old_index:= list_sessions.ItemIndex;
    list_sessions.Items.Delete(list_sessions.ItemIndex);
    
    if (old_index < list_sessions.Count) then
    list_sessions.ItemIndex:= old_index
    else if list_sessions.Count > 0 then
    list_sessions.ItemIndex:= list_sessions.Count - 1;

    list_sessionsClick(self);
  end;
end;

{-------------------------------------------------------------------------------
  On AutoSave Click
-------------------------------------------------------------------------------}
procedure TCESessionManager.check_autosaveClick(Sender: TObject);
begin
  if SessionPropertiesEnabled and assigned(fSelectedSession) then
  fSelectedSession.AutoSave:= check_autosave.Checked;
end;

{-------------------------------------------------------------------------------
  On list_sessions Click
-------------------------------------------------------------------------------}
procedure TCESessionManager.list_sessionsClick(Sender: TObject);
begin
  fSessionPropertiesEnabled:= false;

  if (list_sessions.ItemIndex > -1) and (list_sessions.ItemIndex < GlobalSessions.Sessions.Items.Count) then
  fSelectedSession:= GlobalSessions.Sessions.GetSession(list_sessions.ItemIndex)
  else
  fSelectedSession:= nil;  

  if assigned(fSelectedSession) then
  begin
    edit_name.Text:= fSelectedSession.Name;
    check_autosave.Checked:= fSelectedSession.AutoSave;
    
    // TODO: Session SaveLoadItems
    check_tabs.Checked:= true; //sliTabs in fSelectedSession.SaveLoadItems;
    //check_bookmarks.Checked:= sliBookmarks in fSelectedSession.SaveLoadItems;
    //check_layout.Checked:= sliLayout in fSelectedSession.SaveLoadItems;

    SessionPropertiesEnabled:= true;
  end
  else
  SessionPropertiesEnabled:= false;
end;

{-------------------------------------------------------------------------------
  Populate Sessions List
-------------------------------------------------------------------------------}
procedure TCESessionManager.PopulateSessionsList;
var
  i: Integer;
begin
  list_sessions.Clear;
  for i:= 0 to GlobalSessions.Sessions.Items.Count - 1 do
  begin
    list_sessions.Items.Add(GlobalSessions.Sessions.GetSession(i).Name);
  end;
end;


end.
