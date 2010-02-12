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

{-------------------------------------------------------------------------------
  Handle Elevated Commands
-------------------------------------------------------------------------------}
function HandleElevatedCommands: Boolean;
var
  i: Integer;
  doAdmin: Boolean;
  action: String;
  param1, param2: WideString;
begin
  Result:= false;
  doAdmin:= false;
  i:= 1;
  // Loop through cmd params.
  while i <= ParamCount do
  begin
    if doAdmin then
    begin
      action:= ParamStr(i);

      // Create Junction
      if action = 'create_symlink' then
      begin
        if i + 1 < ParamCount then // make sure we have all needed params present
        begin
          param1:= ParamStr(i + 1);
          param2:= ParamStr(i + 2);
          i:= i + 2;

          //ShowMessage(action + ': ' + '"'+ param1 + '", "' + param2 + '"');
          CreateJunction(param1, param2);
        end;
      end;

      doAdmin:= false;
    end
    else
    begin
      doAdmin:= (ParamStr(i) = '/admin') and (i < ParamCount); // Check if /admin switch is present
      if doAdmin then
      Result:= true;
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
