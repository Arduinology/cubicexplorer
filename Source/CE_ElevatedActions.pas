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
//  The Original Code is CE_ElevatedActions.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_ElevatedActions;

interface

uses
  // CE Units
  CE_FileUtils,
  // Tnt
  TntSysUtils, TntWindows, TntSystem,
  // System Units
  Windows, SysUtils, ShellAPI, Forms;

function HandleElevatedCommands: Boolean;

function Elevated_CreateJunction(ALink: WideString; ATarget: WideString;
    ParentHWND: HWND): Boolean;

implementation

uses
  CE_VersionUpdater;

{-------------------------------------------------------------------------------
  Handle Elevated Commands
-------------------------------------------------------------------------------}
function HandleElevatedCommands: Boolean;
var
  i, c: Integer;
  doAdmin: Boolean;
  doUpdate: Boolean;
  action: String;
  param1, param2: WideString;
  paramI: Integer;
begin
  Result:= false;
  doAdmin:= false;
  doUpdate:= false;
  i:= 1;
  c:= ParamCount;
  // Loop through cmd params.
  while i <= c do
  begin
    if doAdmin then
    begin
      action:= ParamStr(i);

      // Create Junction
      if action = 'create_symlink' then
      begin
        if i + 1 < c then // make sure we have all needed params present
        begin
          param1:= WideParamStr(i + 1);
          param2:= WideParamStr(i + 2);
          i:= i + 2;

          //ShowMessage(action + ': ' + '"'+ param1 + '", "' + param2 + '"');
          CreateJunction(param1, param2);
        end;
      end
      else
      i:= i + 1;

      doAdmin:= false;
    end
    else if doUpdate then
    begin
      if i + 1 < c then
      begin
        param1:= WideParamStr(i); // zip path
        param2:= WideParamStr(i + 1); // dest folder
        i:= i + 2;
        if i <= c then
        begin
          paramI:= StrToIntDef(WideParamStr(i), 0); // Handle to old app
          i:= i + 1;
        end
        else
        paramI:= 0;
        
        UpdateCEFromZip(param1, param2, paramI, false);
        doUpdate:= false;
      end
      else
      i:= i + 1;
    end
    else
    begin
      doAdmin:= (ParamStr(i) = '/admin') and (i < ParamCount); // Check if /admin switch is present
      if doAdmin then
      Result:= true
      else
      begin
        doUpdate:= (ParamStr(i) = '/update') and (i < ParamCount);
        if doUpdate then
        Result:= true;
      end;
      i:= i + 1;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  CreateJunction (Elevated)
-------------------------------------------------------------------------------}
function Elevated_CreateJunction(ALink: WideString; ATarget: WideString;
    ParentHWND: HWND): Boolean;
var
  ws: WideString;
  op: WideString;
begin
  Result:= true; // TODO: add proper result value
  
  ws:= '/admin create_symlink "' + ALink + '" "' + ATarget + '"';
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) then
  op:= 'runas'
  else
  op:= 'open';

  Tnt_ShellExecuteW(ParentHWND,
                    PWideChar(op),
                    PWideChar(WideParamStr(0)),
                    PWideChar(ws),
                    '',
                    SW_HIDE);
end;

end.
