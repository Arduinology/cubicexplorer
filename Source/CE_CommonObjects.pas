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
//  The Original Code is CE_CommonObjects.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_CommonObjects;

interface

uses
  // VSTools
  MPShellUtilities, MPCommonObjects,
  // System Units
  ShlObj, ShellAPI, Windows, Contnrs, Classes, SysUtils;

type
  TCESpecialNamespaces = class(TObject)  
  private
    fPIDLList: TList;
  protected
    procedure ClearList;
    procedure PopulateList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSpecialID(APIDL: PItemIDList): Integer;
  end;

var
  CE_SpecialNamespaces: TCESpecialNamespaces;

implementation

{*------------------------------------------------------------------------------
   Create an instance of TCESpecialNamespaces
-------------------------------------------------------------------------------}
constructor TCESpecialNamespaces.Create;
begin
  inherited;
  fPIDLList:= TList.Create;
  PopulateList;
end;

{*------------------------------------------------------------------------------
  Destroy TCESpecialNamespaces
-------------------------------------------------------------------------------}
destructor TCESpecialNamespaces.Destroy;
begin
  ClearList;
  fPIDLList.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Populate list with Special Folder PIDLs
-------------------------------------------------------------------------------}
procedure TCESpecialNamespaces.PopulateList;
var
  PIDL: PItemIDList;
  FolderID: Integer;
begin
  for FolderID:= 0 to 63 do
  begin
    SHGetspecialFolderLocation(0, FolderID, PIDL);
    fPIDLList.Add(PIDL);
  end;
end;

{*------------------------------------------------------------------------------
  Clear list and free PIDLs
-------------------------------------------------------------------------------}
procedure TCESpecialNamespaces.ClearList;
var
  i: Integer;
begin
  for i:= 0 to fPIDLList.Count-1 do
  begin
    if assigned(fPIDLList.Items[i]) then
    PidlMgr.FreePIDL(fPIDLList.Items[i]);
  end;
  fPIDLList.Clear;
end;

{*------------------------------------------------------------------------------
  Get Special Folder's ID. If not found, -1 is returned.
-------------------------------------------------------------------------------}
function TCESpecialNamespaces.GetSpecialID(APIDL: PItemIDList): Integer;
var
  i: Integer;
begin
  Result:= -1;
  if not assigned(APIDL) then
  Exit;
  
  for i:= 0 to fPIDLList.Count - 1 do
  begin
    if assigned(fPIDLList.Items[i]) then
    begin
      if ILIsEqual(APIDL, fPIDLList.Items[i]) then
      begin
        Result:= i;
        break;
      end;
    end;
  end;
end;

{##############################################################################}

initialization
  CE_SpecialNamespaces:= TCESpecialNamespaces.Create;

finalization
  FreeAndNil(CE_SpecialNamespaces);

end.
