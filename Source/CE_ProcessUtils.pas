unit CE_ProcessUtils;

interface

uses
  // TNT Controls
  TntWindows, TntSysUtils,
  // System Units
  Windows;

procedure RunProcess(Path: WideString; Params: WideString = ''; Hidden: Boolean
    = false; WaitUntilDone: Boolean = true);

implementation

procedure RunProcess(Path: WideString; Params: WideString = ''; Hidden: Boolean
    = false; WaitUntilDone: Boolean = true);
var
  cmd: WideString;
  currDir: WideString;
  flags: DWORD;
  startupInfo: TStartupInfoW;
  procInfo   : TProcessInformation;
  createOK   : Boolean;
begin
  cmd:= Path;
  if Params <> '' then
  cmd:= cmd + ' ' + Params;
  currDir:= WideExtractFilePath(Path);
  // Flags
  flags:= NORMAL_PRIORITY_CLASS;
  // Startup info
  FillChar(startupInfo, SizeOf(TStartupInfo), #0);
  startupInfo.cb:= SizeOf(TStartupInfo);
  if Hidden then
  begin
    startupInfo.wShowWindow:= SW_HIDE;
    Inc(startupInfo.dwFlags, STARTF_USESHOWWINDOW);
  end;
  FillChar(procInfo,SizeOf(TProcessInformation),#0);
  // Create process
  createOK:= Tnt_CreateProcessW(nil,                 // Application
                                PWideChar(cmd),      // Command line
                                nil,                 // lpProcessAttributes
                                nil,                 // lpThreadAttributes
                                false,               // bInheritHandles
                                flags,               // dwCreationFlags
                                nil,                 // lpEnvironment
                                PWideChar(currDir),  // lpCurrentDirectory
                                startupInfo,         // lpStartupInfo
                                procInfo);           // lpProcessInformation 

  if CreateOK and WaitUntilDone then
  WaitForSingleObject(ProcInfo.hProcess, INFINITE);
end;

end.
