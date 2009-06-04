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
  // VirtualTrees
  VirtualTrees, 
  // TNT
  TntExtCtrls,  TntStdCtrls, TntDialogs,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TCESessionManager = class(TForm)
    TntButton1: TTntButton;
    Panel1: TPanel;
    SessionList: TVirtualStringTree;
    edit_name: TTntEdit;
    TntLabel1: TTntLabel;
    check_autosave: TTntCheckBox;
    check_onstartup: TTntCheckBox;
    TntGroupBox1: TTntGroupBox;
    check_bookmarks: TTntCheckBox;
    check_tabs: TTntCheckBox;
    check_layout: TTntCheckBox;
    Panel2: TPanel;
    but_delete: TTntButton;
    procedure but_deleteClick(Sender: TObject);
    procedure check_autosaveClick(Sender: TObject);
    procedure check_onstartupClick(Sender: TObject);
    procedure edit_nameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SessionListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex);
    procedure SessionListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure SessionListKeyDown(Sender: TObject; var Key: Word; Shift:
        TShiftState);
    procedure SessionListNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; NewText: WideString);
    procedure SessionListPaintText(Sender: TBaseVirtualTree; const TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
    fSessionPropertiesEnabled: Boolean;
    procedure SetSessionPropertiesEnabled(const Value: Boolean);
    { Private declarations }
  public
    property SessionPropertiesEnabled: Boolean read fSessionPropertiesEnabled write
        SetSessionPropertiesEnabled;
    { Public declarations }
  end;

var
  CESessionManager: TCESessionManager;

implementation

uses
  CE_Sessions, CE_LanguageEngine;

{$R *.dfm}

{-------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCESessionManager.FormCreate(Sender: TObject);
begin
  SessionPropertiesEnabled:= false;
  SessionList.RootNodeCount:= GlobalSessions.Sessions.Items.Count;
end;

{-------------------------------------------------------------------------------
  On SessionList Focus Changed
-------------------------------------------------------------------------------}
procedure TCESessionManager.SessionListFocusChanged(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Column: TColumnIndex);
var
  session: TCESessionItem;
begin
  fSessionPropertiesEnabled:= false;
  if assigned(Node) then
  begin
    session:= GlobalSessions.Sessions.GetSession(Node.Index);
    edit_name.Text:= session.Name;
    check_autosave.Checked:= session.AutoSave;
    check_onstartup.Checked:= session = GlobalSessions.AutoLoadSession;
    // TODO: Session SaveLoadItems
//    check_tabs.Checked:= sliTabs in session.SaveLoadItems;
//    check_bookmarks.Checked:= sliBookmarks in session.SaveLoadItems;
//    check_layout.Checked:= sliLayout in session.SaveLoadItems;
    SessionPropertiesEnabled:= true;
  end
  else
  SessionPropertiesEnabled:= false;
end;

{-------------------------------------------------------------------------------
  On SessionList Get Text
-------------------------------------------------------------------------------}
procedure TCESessionManager.SessionListGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    WideString);
begin
  CellText:= GlobalSessions.Sessions.GetSession(Node.Index).Name;
end;

{-------------------------------------------------------------------------------
  On Session Name Change
-------------------------------------------------------------------------------}
procedure TCESessionManager.edit_nameChange(Sender: TObject);
var
  session: TCESessionItem;
begin
  if SessionPropertiesEnabled then
  begin
    session:= GlobalSessions.Sessions.GetSession(SessionList.FocusedNode.Index);
    if session.Name <> edit_name.Text then
    begin
      session.Name:= edit_name.Text;
      SessionList.RepaintNode(SessionList.FocusedNode);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On SessionList New text
-------------------------------------------------------------------------------}
procedure TCESessionManager.SessionListNewText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  session: TCESessionItem;
begin
  session:= GlobalSessions.Sessions.GetSession(Node.Index);
  session.Name:= NewText;
  edit_name.Text:= session.Name;
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
  check_onstartup.Enabled:= SessionPropertiesEnabled;
  but_delete.Enabled:= SessionPropertiesEnabled;
  if not SessionPropertiesEnabled then
  begin
    edit_name.Text:= '';
    //check_tabs.Checked:= false;
    check_bookmarks.Checked:= false;
    check_layout.Checked:= false;
    check_autosave.Checked:= false;
    check_onstartup.Checked:= false;
  end;
end;

{-------------------------------------------------------------------------------
  On Delete Click
-------------------------------------------------------------------------------}
procedure TCESessionManager.but_deleteClick(Sender: TObject);
begin
  if (MessageDlg('Are you sure you want to delete this session?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    SessionList.BeginUpdate;
    try
      GlobalSessions.Sessions.DeleteSession(SessionList.FocusedNode.Index);
    finally
      SessionList.RootNodeCount:= GlobalSessions.Sessions.Items.Count;
      SessionList.EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On AutoSave Click
-------------------------------------------------------------------------------}
procedure TCESessionManager.check_autosaveClick(Sender: TObject);
var
  session: TCESessionItem;
begin
  if SessionPropertiesEnabled then
  begin
    session:= GlobalSessions.Sessions.GetSession(SessionList.FocusedNode.Index);
    session.AutoSave:= check_autosave.Checked;
  end;
end;

{-------------------------------------------------------------------------------
  On AutoLoad Click
-------------------------------------------------------------------------------}
procedure TCESessionManager.check_onstartupClick(Sender: TObject);
var
  session: TCESessionItem;
begin
  if SessionPropertiesEnabled then
  begin
    GlobalSessions.AutoLoadSession:= GlobalSessions.Sessions.GetSession(SessionList.FocusedNode.Index);
    SessionList.Repaint;
  end;
end;

{-------------------------------------------------------------------------------
  On SessionList Key Down
-------------------------------------------------------------------------------}
procedure TCESessionManager.SessionListKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    if assigned(SessionList.FocusedNode) then
    but_deleteClick(self);
  end;
end;

{-------------------------------------------------------------------------------
  On SessionList PaintText
-------------------------------------------------------------------------------}
procedure TCESessionManager.SessionListPaintText(Sender: TBaseVirtualTree;
    const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType);
var
  session: TCESessionItem;
begin
  session:= GlobalSessions.Sessions.GetSession(Node.Index);
  if session = GlobalSessions.AutoLoadSession then
  TargetCanvas.Font.Style:= [fsBold]
  else
  TargetCanvas.Font.Style:= [];
end;

end.
