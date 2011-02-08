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
//  The Original Code is CE_Utils.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Utils;

interface

uses
  // JCL
  JclFileUtils, JclMime,
  // TNT Controls
  TntActnList, TntSysUtils, TntSystem, TntWindows,
  // VSTools
  MPCommonUtilities, MPCommonObjects, MPShellUtilities,
  // System Units
  SysUtils, Classes, Windows, StrUtils, ShlObj, ShellAPI, Forms, Controls, Registry, WideStrUtils;

type
  TWinVersion = (wvUnknown, wvWin95, wvWin98, wvWin98SE, wvWinNT, wvWinME, wvWin2000, wvWinXP, wvWin2003, wvWinVista);

  function SavePIDLToMime(APIDL: PItemIDList): String;
  function LoadPIDLFromMime(MimeStr: String): PItemIDList;
  function DecodeRelativePath(Path: WideString): WideString;
  function EncodeRelativePath(Path: WideString): WideString;
  function FindAction(ActionList: TTntActionList; ActionName: String): TTntAction;
  function IsInsideRect(ARect: TRect; X, Y: Integer): Boolean;
  procedure ReplaceChar(var S: string; Orig: Char; Replacement: Char);
  function CEStrToRect(S: String; Sep: String = ','): TRect;
  function CERectToStr(ARect: TRect; Sep: String = ','): String;
  function CEPointToStr(P: TPoint; Sep: String = ','): String;
  function CEStrToPoint(S: String; Sep: String = ','): TPoint;
  function GetIconIndex(FilePath: WideString): Integer;
  function GetWinVersion: TWinVersion;
  procedure EmptyRecycleBin(NoConfirmation: Boolean = false; NoProgressGUI:
      Boolean = false; NoSound: Boolean = false);
  function IsSameText(Str1: WideString; Str2: WideString; CaseSensitive: Boolean
      = false): Boolean;
  function StringToPIDL(aStr: String): PItemIDList;
  procedure ReplaceSystemVariablePath(var Path: WideString);
  function GetFileVersionBuild(Path: WideString): Integer;
  function GetRealVisibility(Comp: TWinControl): Boolean;
  function PathExists(Path: WideString): Boolean;
  function UseConnection(RemotePath: String; Handle: HWND): Integer;
  function IsUNC(Path: WideString): Boolean;
  function WideGetDriveType(lpRootPathName: WideString): Integer;
  function BrowseForFolderPIDL(aTitle: WideString): PItemIDList;
  function GetIsWindows64: Boolean;

function GetLargeShellIconSize: Integer;

function GetSmallShellIconSize: Integer;

function WideStringMatch(ASource: WideString; APattern: WideString;
    ACaseSensitive: Boolean = false): Boolean;

function IsWindowsVista: Boolean;

function IsWindows64: Boolean;

var
  ExePath: WideString;
  LargeShellIconSize, SmallShellIconSize: Integer;
  CE_SHLockShared: function(Handle: THandle; DWord: DWord): Pointer; stdcall;
  CE_SHUnlockShared: function (Pnt: Pointer): BOOL; stdcall;


implementation

var
  fIsWindowsVista: Boolean;
  fIsWindows64: Boolean;
  
{*------------------------------------------------------------------------------
  Save PIDL to Mime encoded string
-------------------------------------------------------------------------------}
function SavePIDLToMime(APIDL: PItemIDList): String;
var
  stream: TStream;
  buf: AnsiString;
begin
  Result:= '';
  stream:= TMemoryStream.Create;
  try
    PIDLMgr.SaveToStream(stream, APIDL);
    SetLength(buf,stream.Size);
    stream.Position:= 0;
    stream.Read(buf[1],stream.Size);
    Result:= MimeEncodeStringNoCRLF(buf);
  finally
    stream.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Load PIDL from Mime encoded string
-------------------------------------------------------------------------------}
function LoadPIDLFromMime(MimeStr: String): PItemIDList;
var
  stream: TStream;
  buf: AnsiString;
begin
  stream:= TMemoryStream.Create;
  try
    buf:= MimeDecodeString(MimeStr);
    stream.Write(buf[1],Length(buf));
    stream.Position:= 0;
    Result:= PIDLMgr.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Decode Relative Path (relative to application path)
-------------------------------------------------------------------------------}
function DecodeRelativePath(Path: WideString): WideString;
begin
  Result:= WideExpandFileName(Path);
end;

{*------------------------------------------------------------------------------
  Encode Relative Path (relative to application path)
-------------------------------------------------------------------------------}
function EncodeRelativePath(Path: WideString): WideString;
begin
  Result:= WideExtractRelativePath(WideParamStr(0), Path);
end;

{*------------------------------------------------------------------------------
  Find action from ActionList
-------------------------------------------------------------------------------}
function FindAction(ActionList: TTntActionList; ActionName: String): TTntAction;
var
  i: Integer;
begin
  Result:= nil;
  if ActionName = '' then
  Exit;
  
  for i:= 0 to ActionList.ActionCount - 1 do
  begin
    if SameText(ActionList.Actions[i].Name, ActionName) then
    begin
      Result:= TTntAction(ActionList.Actions[i]);
      Break;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Check if coordinates are inside rect
-------------------------------------------------------------------------------}
function IsInsideRect(ARect: TRect; X, Y: Integer): Boolean;
begin
  Result:= false;
  if (X > ARect.Left) and (X < ARect.Right) then
    if (Y > ARect.Top) and (Y < ARect.Bottom) then
    Result:= true;
end;

{*------------------------------------------------------------------------------
  Replace char
-------------------------------------------------------------------------------}
procedure ReplaceChar(var S: string; Orig: Char; Replacement: Char);
var
  i,c: Integer;
begin
  c:= Length(S);
  for i:= 1 to c do
  begin
    if S[i] = Orig then
    S[i]:= Replacement;
  end;
end;

{*------------------------------------------------------------------------------
  String to Rect (in format: 'left,top,right,bottom')
-------------------------------------------------------------------------------}
function CEStrToRect(S: String; Sep: String = ','): TRect;
var
  i,c: Integer;
  tmpS: String;
begin
  // Left
  i:= 1;
  c:= Pos(Sep,S);
  tmpS:= copy(S,i,c-1);
  Result.Left:= StrToIntDef(tmpS,0);
  // Top
  i:= c+1;
  c:= PosEx(Sep,S,i);
  tmpS:= copy(S,i,c-i);
  Result.Top:= StrToIntDef(tmpS,0);
  // Right
  i:= c+1;
  c:= PosEx(Sep,S,i);
  tmpS:= copy(S,i,c-i);
  Result.Right:= StrToIntDef(tmpS,0);
  // Bottom
  i:= c+1;
  tmpS:= copy(S,i,Length(S));
  Result.Bottom:= StrToIntDef(tmpS,0);
end;

{*------------------------------------------------------------------------------
  Rect to String (in format: 'left,top,right,bottom')
-------------------------------------------------------------------------------}
function CERectToStr(ARect: TRect; Sep: String = ','): String;
begin
  Result:= IntToStr(ARect.Left) + Sep +
           IntToStr(ARect.Top) + Sep +
           IntToStr(ARect.Right) + Sep +
           IntToStr(ARect.Bottom);
end;

{*------------------------------------------------------------------------------
  Point to String (in format: 'X,Y')
-------------------------------------------------------------------------------}
function CEPointToStr(P: TPoint; Sep: String = ','): String;
begin
  Result:= IntToStr(P.X) + Sep + IntToStr(P.Y);
end;

{*------------------------------------------------------------------------------
 String to Point (in format: 'X,Y')
-------------------------------------------------------------------------------}
function CEStrToPoint(S: String; Sep: String = ','): TPoint;
var
  i,c: Integer;
  tmpS: String;
begin
  // X
  i:= 1;
  c:= Pos(Sep,S);
  tmpS:= copy(S,i,c-1);
  Result.X:= StrToIntDef(tmpS,0);
  // Y
  i:= c+1;
  tmpS:= copy(S,i,Length(S));
  Result.Y:= StrToIntDef(tmpS,0);
end;

{*------------------------------------------------------------------------------
  Get file's icon index
-------------------------------------------------------------------------------}
function GetIconIndex(FilePath: WideString): Integer;
var
  Flags: integer;
  InfoA: TSHFileInfoA;
  InfoW: TSHFileInfoW;
begin
  Flags := SHGFI_SYSICONINDEX or SHGFI_SHELLICONSIZE or SHGFI_SMALLICON;

  if IsUnicode then
  begin
    FillChar(InfoW, SizeOf(InfoW), #0);
    if SHGetFileInfoW_MP(PWideChar(FilePath), 0, InfoW, SizeOf(InfoW), Flags) <> 0 then
      Result:= InfoW.iIcon
    else
      Result:= 0
  end
  else
  begin
    FillChar(InfoA, SizeOf(InfoA), #0);
    if SHGetFileInfoA(PChar(String(FilePath)), 0, InfoA, SizeOf(InfoA), Flags) <> 0 then
      Result:= InfoA.iIcon
    else
      Result:= 0
  end
end;

{*------------------------------------------------------------------------------
  Get Windows Version
-------------------------------------------------------------------------------}
function GetWinVersion: TWinVersion;
var
   osVerInfo: TOSVersionInfo;
   majorVersion, minorVersion: Integer;
begin
   Result := wvUnknown;
   osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo) ;
   if GetVersionEx(osVerInfo) then
   begin
     minorVersion := osVerInfo.dwMinorVersion;
     majorVersion := osVerInfo.dwMajorVersion;
     case osVerInfo.dwPlatformId of
       VER_PLATFORM_WIN32_NT:
       begin
         if majorVersion <= 4 then
         Result:= wvWinNT
         else if (majorVersion = 5) and (minorVersion = 0) then
         Result:= wvWin2000
         else if (majorVersion = 5) and (minorVersion = 1) then
         Result:= wvWinXP
         else if (majorVersion = 5) and (minorVersion = 2) then
         Result:= wvWin2003
         else if (majorVersion = 6) then
         Result:= wvWinVista;
       end;
       VER_PLATFORM_WIN32_WINDOWS:
       begin
         if (majorVersion = 4) and (minorVersion = 0) then
         Result:= wvWin95
         else if (majorVersion = 4) and (minorVersion = 10) then
         begin
           if osVerInfo.szCSDVersion[1] = 'A' then
           Result:= wvWin98SE
           else
           Result:= wvWin98;
         end
         else if (majorVersion = 4) and (minorVersion = 90) then
         Result := wvWinME
         else
         Result := wvUnknown;
       end;
     end;
   end;
end;


{*------------------------------------------------------------------------------
  Empty Recycle Bin
-------------------------------------------------------------------------------}
procedure EmptyRecycleBin(NoConfirmation: Boolean = false; NoProgressGUI:
    Boolean = false; NoSound: Boolean = false);
const
  SHERB_NOCONFIRMATION = $00000001;
  SHERB_NOPROGRESSUI = $00000002;
  SHERB_NOSOUND = $00000004;
type
  TSHEmptyRecycleBin = function(Wnd: HWND;
                                pszRootPath: PChar;
                                dwFlags: DWORD): HRESULT;  stdcall;
var
  SHEmptyRecycleBin: TSHEmptyRecycleBin;
  LibHandle: THandle;
  flags: DWORD;
begin
  LibHandle := LoadLibrary(PChar('Shell32.dll'));
  if LibHandle <> 0 then
  begin
    @SHEmptyRecycleBin:= GetProcAddress(LibHandle, 'SHEmptyRecycleBinA')
  end
  else
  begin
    Exit;
  end;

  if @SHEmptyRecycleBin <> nil then
  begin
    flags:= 0;
    if NoConfirmation then
    flags:= flags or SHERB_NOCONFIRMATION;
    if NoProgressGUI then
    flags:= flags or SHERB_NOPROGRESSUI;
    if NoSound then
    flags:= flags or SHERB_NOSOUND;
    SHEmptyRecycleBin(Application.MainFormHandle,
                      nil,
                      flags);
  end;
  FreeLibrary(LibHandle);
  @SHEmptyRecycleBin := nil;
end;

{*------------------------------------------------------------------------------
  Is Same Text?
-------------------------------------------------------------------------------}
function IsSameText(Str1: WideString; Str2: WideString; CaseSensitive: Boolean
    = false): Boolean;
begin
  if CaseSensitive then
  Result:= WideCompareStr(Str1, str2) = 0
  else
  Result:= WideCompareText(Str1, str2) = 0;
end;

{*------------------------------------------------------------------------------
  Get ItemIDList from text ('PID of explorer:PIDL')
-------------------------------------------------------------------------------}
function StringToPIDL(aStr: String): PItemIDList;
var
  map, pid: Cardinal;
  p: Pointer;
  list: TStrings;
begin
  Result:= nil;
  if assigned(CE_SHLockShared) and assigned(CE_SHUnlockShared) then
  begin
    list:= TStringList.Create;
    try
      list.Delimiter:= ':';
      list.DelimitedText:= aStr;
      if list.Count >= 3 then
      begin
        map:= StrToIntDef(list.Strings[1], 0);
        pid:= StrToIntDef(list.Strings[2], 0);
        p:= CE_SHLockShared(map, pid);
        if p <> nil then
        begin
          try
            Result:= PIDLMgr.CopyPIDL(PItemIDList(p));
          finally
            CE_SHUnlockShared(p);
          end;
        end;
      end;
    finally
      list.Free;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Replace System Variable inside Path
-------------------------------------------------------------------------------}
procedure ReplaceSystemVariablePath(var Path: WideString);

  function ReplacePath(Path, Variable, VarPath: WideString): WideString;
  begin
    Result := Tnt_WideStringReplace(Path, Variable, VarPath, [rfReplaceAll, rfIgnoreCase])
  end;

begin
  /// TODO: Optimize this method. 

  // Psudo Variables
  Path := ReplacePath(Path, '%sysdir%', WideLowerCase(WideStripTrailingBackslash(SystemDirectory)));
  //Path := ReplacePath(Path, '%temp%', WideLowerCase(WideStripTrailingBackslash(WideGetTempDir)));
  //Path := ReplacePath(Path, '%appdata%', WideLowerCase(WideStripTrailingBackslash(UserDocumentsFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%favorites%', WideLowerCase(WideStripTrailingBackslash(FavoritesFolder.NameForParsing)));
  Path := ReplacePath(Path, '%personal%', WideLowerCase(WideStripTrailingBackslash(MyDocumentsFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%templates%', WideLowerCase(WideStripTrailingBackslash(TemplatesFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%history%', WideLowerCase(WideStripTrailingBackslash(HistoryFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%desktopfolder%', WideLowerCase(WideStripTrailingBackslash(PhysicalDesktopFolder.NameForParsing)));
  Path := ReplacePath(Path, '%cedrive%', WideLowerCase(WideStripTrailingBackslash(WideExtractFileDrive(ExePath))));

  // Environment variables
  Path := ReplacePath(Path, '%userprofile%', WideStripTrailingBackslash(WideExpandEnviromentString('%USERPROFILE%')));
  Path := ReplacePath(Path, '%allusersprofile%', WideStripTrailingBackslash(WideExpandEnviromentString('%ALLUSERSPROFILE%')));
  Path := ReplacePath(Path, '%programfiles%', WideStripTrailingBackslash(WideExpandEnviromentString('%ProgramFiles%')));
  Path := ReplacePath(Path, '%systemroot%', WideStripTrailingBackslash(WideExpandEnviromentString('%SystemRoot%')));
  Path := ReplacePath(Path, '%systemdrive%', WideStripTrailingBackslash(WideExpandEnviromentString('%SystemDrive%')));
  Path := ReplacePath(Path, '%windir%', WideStripTrailingBackslash(WideExpandEnviromentString('%windir%')));
  Path := ReplacePath(Path, '%tmp%', WideStripTrailingBackslash(WideExpandEnviromentString('%TMP%')));
  Path := ReplacePath(Path, '%temp%', WideStripTrailingBackslash(WideExpandEnviromentString('%TEMP%')));
  Path := ReplacePath(Path, '%public%', WideStripTrailingBackslash(WideExpandEnviromentString('%PUBLIC%')));
  Path := ReplacePath(Path, '%programdata%', WideStripTrailingBackslash(WideExpandEnviromentString('%ProgramData%')));
  Path := ReplacePath(Path, '%homedrive%', WideStripTrailingBackslash(WideExpandEnviromentString('%HOMEDRIVE%')));
  Path := ReplacePath(Path, '%homepath%', WideStripTrailingBackslash(WideExpandEnviromentString('%HOMEPATH%')));
  Path := ReplacePath(Path, '%commonprogramfiles%', WideStripTrailingBackslash(WideExpandEnviromentString('%CommonProgramFiles%')));
  Path := ReplacePath(Path, '%appdata%', WideStripTrailingBackslash(WideExpandEnviromentString('%APPDATA%')));
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Load DLL procs
-------------------------------------------------------------------------------}
procedure CELoadShellProcs;
var
  hDLL: HMODULE;
begin
  hDLL:= GetModuleHandle('shlwapi.dll');
  if hDLL <> 0 then
  begin
    CE_SHLockShared:= GetProcAddress(hDLL, PChar(8));
    CE_SHUnlockShared:= GetProcAddress(hDLL, PChar(9));
  end;
end;

{-------------------------------------------------------------------------------
  Get File Version Build
-------------------------------------------------------------------------------}
function GetFileVersionBuild(Path: WideString): Integer;
var
  Size, FixInfoLen: DWORD;
  Handle: THandle;
  Buffer: WideString;
  FixInfoBuf: PVSFixedFileInfo;
begin
  Result:= 0;
  Size:= Tnt_GetFileVersionInfoSizeW(PWideChar(Path), Handle);
  if Size > 0 then
  begin
    SetLength(Buffer, Size);
    if Tnt_GetFileVersionInfoW(PWideChar(Path), Handle, Size, Pointer(Buffer)) and
      Tnt_VerQueryValueW(Pointer(Buffer), DirDelimiter, Pointer(FixInfoBuf), FixInfoLen) and
      (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
    begin
      Result:= LoWord(FixInfoBuf^.dwProductVersionLS)
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Real Visibility
-------------------------------------------------------------------------------}
function GetRealVisibility(Comp: TWinControl): Boolean;
var
  p: TWinControl;
begin
  Result:= false;
  p:= Comp;
  while assigned(p) do
  begin
    Result:= p.Visible;
    if not Result then
    break
    else
    begin
      p:= p.Parent;
      if not assigned(p) then
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Check if Path Exists
-------------------------------------------------------------------------------}
function PathExists(Path: WideString): Boolean;
begin
  if Win32PlatformIsUnicode then
  Result:= GetFileAttributesW(PWideChar(Path)) <> INVALID_FILE_ATTRIBUTES
  else
  Result:= GetFileAttributesA(PChar(String(Path))) <> INVALID_FILE_ATTRIBUTES;
end;

{-------------------------------------------------------------------------------
  Use UNC connection
-------------------------------------------------------------------------------}
function UseConnection(RemotePath: String; Handle: HWND): Integer;
var
  nRes: TNetResource;
  flags: Cardinal;
  size, res: Cardinal;
begin
  FillChar(nRes, SizeOf(nRes), #0);
  nRes.lpRemoteName:= PChar(RemotePath);
  nRes.dwType:= RESOURCETYPE_ANY;
  nRes.lpLocalName:= nil;
  nRes.lpProvider:= nil;
  size:= 0;
  flags:= CONNECT_INTERACTIVE;
  Result:= WNetUseConnection(Handle,
                              nRes,
                              nil,
                              nil,
                              flags,
                              nil,
                              size,
                              res);
end;

{-------------------------------------------------------------------------------
  is UNC path
-------------------------------------------------------------------------------}
function IsUNC(Path: WideString): Boolean;
begin
  if Length(Path) > 1 then
  Result:= ((Path[1] = '\') and (Path[2] = '\'))
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  GetDriveType (unicode)
-------------------------------------------------------------------------------}
function WideGetDriveType(lpRootPathName: WideString): Integer;
begin
  if Win32PlatformIsUnicode then
  Result:= GetDriveTypeW(PWideChar(lpRootPathName))
  else
  Result:= GetDriveTypeA(PAnsiChar(AnsiString(lpRootPathName)));
end;

{-------------------------------------------------------------------------------
  Show BrowseForFolder dialog (returns PIDL)
-------------------------------------------------------------------------------}
function BrowseForFolderPIDL(aTitle: WideString): PItemIDList;
var
  info: TBrowseInfoW;
begin
  FillChar(info, SizeOf(info), 0);
  info.hwndOwner:= Application.ActiveFormHandle;
  info.ulFlags:= BIF_USENEWUI;
  info.lpszTitle:= PWideChar(aTitle);
  Result:= Tnt_SHBrowseForFolderW(info);
end;

{-------------------------------------------------------------------------------
  Get Small Shell Icon Size
-------------------------------------------------------------------------------}
function GetSmallShellIconSize: Integer;
var
  reg: TRegistry;
begin
  Result:= 16;
  reg:= TRegistry.Create(KEY_READ);
  try
    reg.RootKey:= HKEY_CURRENT_USER;
    reg.OpenKey('\Control Panel\Desktop\WindowMetrics', False);
    if reg.ValueExists('Shell Small Icon Size') then
    begin
      try
      case reg.GetDataType('Shell Small Icon Size') of
        rdString, rdExpandString: Result:= StrToIntDef(reg.ReadString('Shell Small Icon Size'), GetSystemMetrics(SM_CXSMICON));
        rdInteger, rdUnknown, rdBinary: Result:= reg.ReadInteger('Shell Small Icon Size');
      end;
      except
        Result:= GetSystemMetrics(SM_CXSMICON);
      end;
    end
    else
    Result:= GetSystemMetrics(SM_CXSMICON);
  finally
    reg.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Get Large Shell Icon Size
-------------------------------------------------------------------------------}
function GetLargeShellIconSize: Integer;
var
  reg: TRegistry;
begin
  Result:= 32;
  reg:= TRegistry.Create(KEY_READ);
  try
    reg.RootKey:= HKEY_CURRENT_USER;
    reg.OpenKey('\Control Panel\Desktop\WindowMetrics', False);
    if reg.ValueExists('Shell Icon Size') then
    begin
      try
      case reg.GetDataType('Shell Icon Size') of
        rdString, rdExpandString: Result:= StrToIntDef(reg.ReadString('Shell Icon Size'), GetSystemMetrics(SM_CXICON));
        rdInteger, rdUnknown, rdBinary: Result:= reg.ReadInteger('Shell Icon Size');
      end;
      except
        Result:= GetSystemMetrics(SM_CXICON);
      end;
    end
    else
    Result:= GetSystemMetrics(SM_CXICON);
  finally
    reg.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Wide String Match
-------------------------------------------------------------------------------}
function WideStringMatch(ASource: WideString; APattern: WideString;
    ACaseSensitive: Boolean = false): Boolean;

  function DoMatch(source, pattern: PWideChar): Boolean;
  begin
    if 0 = WStrComp(pattern,'*') then
      Result:= True
    else if (source^ = Chr(0)) and (pattern^ <> Chr(0)) then
      Result:= False
    else if source^ = Chr(0) then
      Result:= True
    else begin
      case pattern^ of
      '*': if DoMatch(source,@pattern[1]) then
             Result:= True
           else
             Result:= DoMatch(@source[1],pattern);
      '?': Result:= DoMatch(@source[1],@pattern[1]);
      else
        if ACaseSensitive then
        begin
          if source^ = pattern^ then
          Result:= DoMatch(@source[1],@pattern[1])
          else
          Result:= False;
        end
        else
        begin
          if CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, source, 1, pattern, 1) = 2 then
          Result:= DoMatch(@source[1],@pattern[1])
          else
          Result:= False;
        end;
      end;
    end;
  end;

begin
  Result:= DoMatch(PWideChar(ASource), PWideChar(APattern));
end;

{-------------------------------------------------------------------------------
  Is Windows Vista And Up
-------------------------------------------------------------------------------}
function IsWindowsVista: Boolean;
begin
  Result:= fIsWindowsVista;
end;

{-------------------------------------------------------------------------------
  Get Is Windows 64
-------------------------------------------------------------------------------}
function GetIsWindows64: Boolean;
type
  TIsWow64Process = function(AHandle:THandle; var AIsWow64: BOOL): BOOL; stdcall;
var
  fKernel32Handle: DWORD;
  fIsWow64Process: TIsWow64Process;
  fIsWow64       : BOOL;
begin
  Result:= False;

  fKernel32Handle:= LoadLibrary('kernel32.dll');
  if (fKernel32Handle = 0) then Exit;

  try
    @fIsWow64Process:= GetProcAddress(fKernel32Handle, 'IsWow64Process');
    if not Assigned(fIsWow64Process) then Exit;

    fIsWow64:= False;
    if (fIsWow64Process(GetCurrentProcess, fIsWow64)) then
    Result:= fIsWow64;
  finally
    FreeLibrary(fKernel32Handle);
  end;
end;

{-------------------------------------------------------------------------------
  Is Windows 64
-------------------------------------------------------------------------------}
function IsWindows64: Boolean;
begin
  Result:= fIsWindows64;
end;

{##############################################################################}

initialization
  ExePath:= WideExtractFilePath(WideParamStr(0));
  LargeShellIconSize:= GetLargeShellIconSize;
  SmallShellIconSize:= GetSmallShellIconSize;
  CELoadShellProcs;
  fIsWindowsVista:= GetWinVersion = wvWinVista;
  fIsWindows64:= GetIsWindows64;

finalization

end.
