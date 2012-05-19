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
  JclMime, JclResources, JclStrings, JclBase,
  // TNT Controls
  TntActnList, TntSysUtils, TntSystem, TntWindows, TntClasses,
  // VSTools
  MPCommonUtilities, MPCommonObjects, MPShellUtilities,
  // System Units
  SysUtils, Classes, Windows, StrUtils, ShlObj, ShellAPI, Forms, Controls,
  Registry, WideStrUtils, Consts, Menus;

const
  PathDevicePrefix = '\\.\';
  DirDelimiter = '\';
  DirSeparator = ';';
  PathUncPrefix    = '\\';

  VerKeyNames: array [1..12] of string =
   ('Comments',
    'CompanyName',
    'FileDescription',
    'FileVersion',
    'InternalName',
    'LegalCopyright',
    'LegalTradeMarks',
    'OriginalFilename',
    'ProductName',
    'ProductVersion',
    'SpecialBuild',
    'PrivateBuild');

type
  TWinVersion = (wvUnknown, wvWin95, wvWin98, wvWin98SE, wvWinNT, wvWinME, wvWin2000, wvWinXP, wvWin2003, wvWinVista);

  ECEWin32Error = class(Exception);
  ECEError = class(Exception);

{-------------------------------------------------------------------------------
  TCEFileVersionInfo
-------------------------------------------------------------------------------}
  TFileFlag = (ffDebug, ffInfoInferred, ffPatched, ffPreRelease, ffPrivateBuild, ffSpecialBuild);
  TFileFlags = set of TFileFlag;

  PLangIdRec = ^TLangIdRec;
  TLangIdRec = packed record
    case Integer of
    0: (
      LangId: Word;
      CodePage: Word);
    1: (
      Pair: DWORD);
  end;

  ECEFileVersionInfoError = class(Exception);

  TCEFileVersionInfo = class(TObject)
  private
    FBuffer: AnsiString;
    FFixedInfo: PVSFixedFileInfo;
    FFileFlags: TFileFlags;
    FItemList: TStringList;
    FItems: TStringList;
    FLanguages: array of TLangIdRec;
    FLanguageIndex: Integer;
    FTranslations: array of TLangIdRec;
    function GetFixedInfo: TVSFixedFileInfo;
    function GetItems: TStrings;
    function GetLanguageCount: Integer;
    function GetLanguageIds(Index: Integer): string;
    function GetLanguageNames(Index: Integer): string;
    function GetLanguages(Index: Integer): TLangIdRec;
    function GetTranslationCount: Integer;
    function GetTranslations(Index: Integer): TLangIdRec;
    procedure SetLanguageIndex(const Value: Integer);
  protected
    procedure CreateItemsForLanguage;
    procedure CheckLanguageIndex(Value: Integer);
    procedure ExtractData;
    procedure ExtractFlags;
    function GetBinFileVersion: string;
    function GetBinProductVersion: string;
    function GetFileOS: DWORD;
    function GetFileSubType: DWORD;
    function GetFileType: DWORD;
    function GetFileVersionBuild: string;
    function GetFileVersionMajor: string;
    function GetFileVersionMinor: string;
    function GetFileVersionRelease: string;
    function GetProductVersionBuild: string;
    function GetProductVersionMajor: string;
    function GetProductVersionMinor: string;
    function GetProductVersionRelease: string;
    function GetVersionKeyValue(Index: Integer): string;
  public
    constructor Attach(VersionInfoData: Pointer; Size: Integer);
    constructor Create(const FileName: WideString); overload;
    constructor Create(const Window: HWND); overload;
    constructor Create(const Module: HMODULE); overload;
    destructor Destroy; override;
    function GetCustomFieldValue(const FieldName: string): string;
    class function VersionLanguageId(const LangIdRec: TLangIdRec): string;
    class function VersionLanguageName(const LangId: Word): string;
    function TranslationMatchesLanguages(Exact: Boolean = True): Boolean;
    property BinFileVersion: string read GetBinFileVersion;
    property BinProductVersion: string read GetBinProductVersion;
    property Comments: string index 1 read GetVersionKeyValue;
    property CompanyName: string index 2 read GetVersionKeyValue;
    property FileDescription: string index 3 read GetVersionKeyValue;
    property FixedInfo: TVSFixedFileInfo read GetFixedInfo;
    property FileFlags: TFileFlags read FFileFlags;
    property FileOS: DWORD read GetFileOS;
    property FileSubType: DWORD read GetFileSubType;
    property FileType: DWORD read GetFileType;
    property FileVersion: string index 4 read GetVersionKeyValue;
    property FileVersionBuild: string read GetFileVersionBuild;
    property FileVersionMajor: string read GetFileVersionMajor;
    property FileVersionMinor: string read GetFileVersionMinor;
    property FileVersionRelease: string read GetFileVersionRelease;
    property Items: TStrings read GetItems;
    property InternalName: string index 5 read GetVersionKeyValue;
    property LanguageCount: Integer read GetLanguageCount;
    property LanguageIds[Index: Integer]: string read GetLanguageIds;
    property LanguageIndex: Integer read FLanguageIndex write SetLanguageIndex;
    property Languages[Index: Integer]: TLangIdRec read GetLanguages;
    property LanguageNames[Index: Integer]: string read GetLanguageNames;
    property LegalCopyright: string index 6 read GetVersionKeyValue;
    property LegalTradeMarks: string index 7 read GetVersionKeyValue;
    property OriginalFilename: string index 8 read GetVersionKeyValue;
    property PrivateBuild: string index 12 read GetVersionKeyValue;
    property ProductName: string index 9 read GetVersionKeyValue;
    property ProductVersion: string index 10 read GetVersionKeyValue;
    property ProductVersionBuild: string read GetProductVersionBuild;
    property ProductVersionMajor: string read GetProductVersionMajor;
    property ProductVersionMinor: string read GetProductVersionMinor;
    property ProductVersionRelease: string read GetProductVersionRelease;
    property SpecialBuild: string index 11 read GetVersionKeyValue;
    property TranslationCount: Integer read GetTranslationCount;
    property Translations[Index: Integer]: TLangIdRec read GetTranslations;
  end;

{-------------------------------------------------------------------------------
  Public methods
-------------------------------------------------------------------------------}

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
  function ShortCutToTextRaw(ShortCut: TShortCut): string;
  function GetSpecialName(ShortCut: TShortCut): string;
  procedure SwitchToThisWindow(h1: hWnd; x: bool); stdcall;
    external user32 Name 'SwitchToThisWindow';
  function ShiftState2Modifier(const Shift: TShiftState):Word;
  function GetShortCutKey(ShortCut: TShortCut):Word;
  function GetShortCutModifier(ShortCut: TShortCut):Word;
  function GetSettingsFolderPath(var IsReadOnly: Boolean; ACreate: Boolean):
      WideString;
  function GetAppVersionStr: string;
  function GetShiftState: TShiftState;
  function GetSystemProxyServer(Protocol: String = 'http'): String;
  function ExtractUrlPort(Address: String; var Port: Integer): String;
  function CleanDateTimeStr(AStr: WideString): String;
  function GetWinMajorVersion: Integer;
  function IsWindowsAdmin: Boolean;
  function ForceForegroundWindow(hwnd: THandle): Boolean;
  procedure ForceForegroundWindow2(hwnd: THandle);
  function FindDialogWindow(AProcessID: DWORD = 0): HWND;
  function WindowToModuleFileName(const Window: HWND): WideString;
  function GetModulePath(const Module: HMODULE): WideString;

var
  ExePath: WideString;
  SettingsDirPath: WideString;
  ReadOnlySettings: Boolean;
  LargeShellIconSize, SmallShellIconSize: Integer;
  CE_SHLockShared: function(Handle: THandle; DWord: DWord): Pointer; stdcall;
  CE_SHUnlockShared: function (Pnt: Pointer): BOOL; stdcall;

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt);

var
  MenuKeyCaps: array[TMenuKeyCap] of string = (
    SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter, SmkcSpace, SmkcPgUp,
    SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight,
    SmkcDown, SmkcIns, SmkcDel, SmkcShift, SmkcCtrl, SmkcAlt);


implementation

var
  fIsWindowsVista: Boolean;
  fIsWindows64: Boolean;
  
{*------------------------------------------------------------------------------
  Decode Relative Path (relative to application path)
-------------------------------------------------------------------------------}
function DecodeRelativePath(Path: WideString): WideString;
var
  ws: WideString;
begin
  ws:= WideGetCurrentDir;
  WideSetCurrentDir(ExePath);
  Result:= WideExpandFileName(Path);
  WideSetCurrentDir(ws);
end;

{*------------------------------------------------------------------------------
  Encode Relative Path (relative to application path)
-------------------------------------------------------------------------------}
function EncodeRelativePath(Path: WideString): WideString;
var
  ws: WideString;
begin
  ws:= WideGetCurrentDir;
  WideSetCurrentDir(ExePath);
  Result:= WideExtractRelativePath(WideParamStr(0), Path);
  WideSetCurrentDir(ws);
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

{-------------------------------------------------------------------------------
  Get WinMajorVersion
-------------------------------------------------------------------------------}
function GetWinMajorVersion: Integer;
var
   osVerInfo: TOSVersionInfo;
begin
  osVerInfo.dwOSVersionInfoSize:= SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  Result:= osVerInfo.dwMajorVersion
  else
  Result:= -1;
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
    Result:= Tnt_WideStringReplace(Path, Variable, VarPath, [rfReplaceAll, rfIgnoreCase])
  end;

  function CEExpandEnviromentStringForUser(APath: WideString): WideString;
  var
    EnviromentStringA, ResultA: AnsiString;
    Token: THandle;
  begin
    if Assigned(ExpandEnvironmentStringsForUserW_MP) then
    begin
      if OpenProcessToken(GetCurrentProcess, TOKEN_IMPERSONATE or TOKEN_QUERY, Token) then
      begin
        SetLength(Result, 1024);
        ExpandEnvironmentStringsForUserW_MP(Token, PWideChar(APath), PWideChar(@Result[1]), 1024);
        SetLength(Result, lstrlenW(PWideChar( Result)));
        CloseHandle(Token)
      end
    end
    else
    begin
      if Assigned(ExpandEnvironmentStringsForUserA_MP) then
      begin
        if OpenProcessToken(GetCurrentProcess, TOKEN_IMPERSONATE or TOKEN_QUERY, Token) then
        begin
          EnviromentStringA:= APath;
          SetLength(ResultA, 1024);
          ExpandEnvironmentStringsForUserA_MP(Token, PAnsiChar(EnviromentStringA), PAnsiChar( @ResultA[1]), 1024);
          SetLength(ResultA, lstrlenA(PAnsiChar( ResultA)));
          Result:= ResultA;
          CloseHandle(Token)
        end
      end
    end
  end;

  function CEExpandEnviromentString(APath: WideString): WideString;
  var
    Length: Integer;
    EnviromentStringA, ResultA: AnsiString;
  begin
    if Assigned(ExpandEnvironmentStringsW_MP) then
    begin
      Length:= ExpandEnvironmentStringsW_MP(PWideChar( APath), nil, 0);
      if Length > 0 then
      begin
        SetLength(Result, Length - 1); // Includes the null
        ExpandEnvironmentStringsW_MP( PWideChar( APath), PWideChar(@Result[1]), Length);
      end
    end
    else
    Result:= WideExpandEnviromentString(APath);
  end;


begin
  /// TODO: Optimize this method. 

  // Psudo Variables
  Path := ReplacePath(Path, '%sysdir%', WideLowerCase(WideStripTrailingBackslash(SystemDirectory)));
  //Path := ReplacePath(Path, '%temp%', WideLowerCase(WideStripTrailingBackslash(WideGetTempDir)));
  //Path := ReplacePath(Path, '%appdata%', WideLowerCase(WideStripTrailingBackslash(UserDocumentsFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%favorites%', WideLowerCase(WideStripTrailingBackslash(FavoritesFolder.NameForParsing)));
  if assigned(MyDocumentsFolder) then
  Path := ReplacePath(Path, '%personal%', WideLowerCase(WideStripTrailingBackslash(MyDocumentsFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%templates%', WideLowerCase(WideStripTrailingBackslash(TemplatesFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%history%', WideLowerCase(WideStripTrailingBackslash(HistoryFolder.NameForParsing)));
  //Path := ReplacePath(Path, '%desktopfolder%', WideLowerCase(WideStripTrailingBackslash(PhysicalDesktopFolder.NameForParsing)));
  Path := ReplacePath(Path, '%cedrive%', WideLowerCase(WideStripTrailingBackslash(WideExtractFileDrive(ExePath))));

  // Environment variables
  Path:= CEExpandEnviromentStringForUser(Path);
  Path:= CEExpandEnviromentString(Path);
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

{-------------------------------------------------------------------------------
  ShortCutToText (returns Ctrl,Shift,Alt also)
-------------------------------------------------------------------------------}
function ShortCutToTextRaw(ShortCut: TShortCut): string;
var
  Name: string;
begin
  case WordRec(ShortCut).Lo of
    $08, $09: Name:= MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + WordRec(ShortCut).Lo - $08)];
    $0D: Name:= MenuKeyCaps[mkcEnter];
    $10..$12: begin
      Name:= '';
    end;
    $1B: Name:= MenuKeyCaps[mkcEsc];
    $20..$28: Name:= MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + WordRec(ShortCut).Lo - $20)];
    $2D..$2E: Name:= MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + WordRec(ShortCut).Lo - $2D)];
    $30..$39: Name:= Chr(WordRec(ShortCut).Lo - $30 + Ord('0'));
    $41..$5A: Name:= Chr(WordRec(ShortCut).Lo - $41 + Ord('A'));
    $60..$69: Name:= Chr(WordRec(ShortCut).Lo - $60 + Ord('0'));
    $70..$87: Name:= 'F' + IntToStr(WordRec(ShortCut).Lo - $6F);
  else
    Name:= GetSpecialName(ShortCut);
  end;

  Result := '';
  if ShortCut and scShift <> 0 then Result := Result + MenuKeyCaps[mkcShift];
  if ShortCut and scCtrl <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
  if ShortCut and scAlt <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
  Result:= Result + Name;
end;

{-------------------------------------------------------------------------------
  Get Special Name of Shortcut
-------------------------------------------------------------------------------}
function GetSpecialName(ShortCut: TShortCut): string;
var
  ScanCode: Integer;
  KeyName: array[0..255] of Char;
begin
  Result:= '';
  ScanCode:= MapVirtualKey(WordRec(ShortCut).Lo, 0) shl 16;
  if ScanCode <> 0 then
  begin
    GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName));
    Result:= KeyName;
  end;
end;

{-------------------------------------------------------------------------------
  ShiftState2Modifier
-------------------------------------------------------------------------------}
function ShiftState2Modifier(const Shift: TShiftState):Word;
begin
  Result := 0;
  if ssShift in Shift then
    Result := Result or MOD_SHIFT;
  if ssAlt in Shift then
    Result := Result or MOD_ALT;
  if ssCtrl in Shift then
    Result := Result or MOD_CONTROL;
end;

{-------------------------------------------------------------------------------
  GetShortCutKey
-------------------------------------------------------------------------------}
function GetShortCutKey(ShortCut: TShortCut):Word;
var
  shift: TShiftState;
begin
  ShortCutToKey(ShortCut,Result,shift);
end;

{-------------------------------------------------------------------------------
  GetShortCutModifier
-------------------------------------------------------------------------------}
function GetShortCutModifier(ShortCut: TShortCut):Word;
var
  key: Word;
  shift: TShiftState;
begin
  ShortCutToKey(ShortCut,key,shift);
  Result := ShiftState2Modifier(shift);
end; 

{-------------------------------------------------------------------------------
  Get SettingsFolderPath
-------------------------------------------------------------------------------}
function GetSettingsFolderPath(var IsReadOnly: Boolean; ACreate: Boolean):
    WideString;
var
  list: TTntStrings;
  i, p: Integer;
  ws: WideString;
begin
  Result:= '';
  IsReadOnly:= false;
  if WideFileExists(exePath + 'settings.path') then
  begin
    list:= TTntStringList.Create;
    try
      list.LoadFromFile(exePath + 'settings.path');
      for i:= 0 to list.Count - 1 do
      begin
        ws:= Trim(list.Strings[i]);
        if Length(ws) > 0 then
        begin
          if ws[1] <> ';' then
          begin
            p:= Pos('[READONLY]', ws);
            IsReadOnly:= p = 1;
            if not IsReadOnly then
            begin
              ReplaceSystemVariablePath(ws);
              Result:= DecodeRelativePath(ws);
              if ACreate then
              begin
                if not WideDirectoryExists(Result) then
                begin
                  if not WideCreateDir(Result) then
                  Result:= '';
                end;
              end;
            end
            else
            begin
              if Length(ws) > 10 then
              begin
                ws:= Copy(ws, 11, Length(ws) - 11);
                ReplaceSystemVariablePath(ws);
                Result:= DecodeRelativePath(ws);
              end
              else
              Result:= '';
            end;
            Break;
          end;
        end;
      end;
    finally
      list.Free;
    end;
  end;

  if Result = '' then
  Result:= ExePath;

  Result:= WideIncludeTrailingBackslash(Result);
end;

{-------------------------------------------------------------------------------
  Get AppVersionStr
-------------------------------------------------------------------------------}
function GetAppVersionStr: string;
var
  ver: TCEFileVersionInfo;
begin
  ver:= TCEFileVersionInfo.Create(WideParamStr(0));
  try
    Result:= ver.FileVersion;
  finally
    ver.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Get ShiftState
-------------------------------------------------------------------------------}
function GetShiftState: TShiftState;
begin
  Result := [];
  if GetAsyncKeyState(VK_SHIFT) < 0 then
  Include(Result, ssShift);
  if GetAsyncKeyState(VK_CONTROL) < 0 then
  Include(Result, ssCtrl);
  if GetAsyncKeyState(VK_MENU) < 0 then
  Include(Result, ssAlt);
end;

{-------------------------------------------------------------------------------
  Get SystemProxyServer
-------------------------------------------------------------------------------}
function GetSystemProxyServer(Protocol: String = 'http'): String;
var
  i, j: Integer;
  Handle: HKey;
  Buffer: array[0..256] of Char;
  BufSize: Integer;
  DataType: Integer;
  ProxyServer: String;
begin
  ProxyServer:= '';

  // Get ProxyServer
  if RegOpenKeyEx(HKEY_CURRENT_USER,
                  'SOFTWARE\Microsoft\Windows\CurrentVersion\Internet Settings',
                  0, KEY_READ, Handle) = ERROR_SUCCESS then
  begin
    BufSize := SizeOf(Buffer);
    DataType := reg_sz;
    if RegQueryValueEx(Handle, 'ProxyServer', nil, @DataType, @Buffer, @BufSize) = ERROR_SUCCESS then
    ProxyServer:= Buffer;

    RegCloseKey(Handle);
  end;

  // Extract address by protocol
  if ProxyServer <> '' then
  begin
    i:= Pos(Protocol + '=', ProxyServer);
    if (i > 0) then
    begin
      Delete(ProxyServer, 1, i+Length(Protocol));
      j:= Pos(';', ProxyServer);
      if (j > 0) then
      ProxyServer:= Copy(ProxyServer, 1, j-1);
    end;
  end;
  Result:= ProxyServer;
end;

{-------------------------------------------------------------------------------
  ExtractUrlPort
-------------------------------------------------------------------------------}
function ExtractUrlPort(Address: String; var Port: Integer): String;
var
  i: Integer;
begin
  i:= Pos('://', Address);
  if i > 0 then
  i:= i + 3
  else
  i:= 1;
  
  i:= PosEx(':', Address, i);
  if (i > 0) then
  begin
    Port:= StrToIntDef(Copy(Address, i+1, Length(Address)-i), 0);
    Result:= Copy(Address, 1, i-1);
  end
  else
  Result:= Address;
end;

{-------------------------------------------------------------------------------
  Clean DateTime String
-------------------------------------------------------------------------------}
function CleanDateTimeStr(AStr: WideString): String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 1 to Length(AStr) do
  begin
    if (Ord(AStr[i]) > 31) and (Ord(AStr[i]) < 127) then
    Result:= Result + AStr[i];
  end;
end;

{-------------------------------------------------------------------------------
  IsWindowsAdmin
-------------------------------------------------------------------------------}
function IsWindowsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5)) ;
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  g: Integer;
  bSuccess: BOOL;
begin
  Result:= False;

  bSuccess:= OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
    bSuccess:= OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  end;

  if bSuccess then
  begin
    GetMem(ptgGroups, 1024);
    bSuccess:= GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, 1024, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
      for g := 0 to ptgGroups.GroupCount - 1 do
      if EqualSid(psidAdministrators, ptgGroups.Groups[g].Sid) then
      begin
        Result:= True;
        Break;
      end;
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

{-------------------------------------------------------------------------------
  ForceForegroundWindow
-------------------------------------------------------------------------------}
function ForceForegroundWindow(hwnd: THandle): Boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID: DWORD;
  timeout: DWORD;
begin
  if IsIconic(hwnd) then ShowWindow(hwnd, SW_RESTORE);

  if GetForegroundWindow = hwnd then Result := True
  else
  begin
    // Windows 98/2000 doesn't want to foreground a window when some other
    // window has keyboard focus

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion > 4)) or
      ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
      ((Win32MajorVersion > 4) or ((Win32MajorVersion = 4) and
      (Win32MinorVersion > 0)))) then
    begin
      // Code from Karl E. Peterson, www.mvps.org/vb/sample.htm
      // Converted to Delphi by Ray Lischner
      // Published in The Delphi Magazine 55, page 16

      Result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow, nil);
      ThisThreadID := GetWindowThreadPRocessId(hwnd, nil);
      if AttachThreadInput(ThisThreadID, ForegroundThreadID, True) then
      begin
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hwnd);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, False);
        Result := (GetForegroundWindow = hwnd);
      end;
      if not Result then
      begin
        // Code by Daniel P. Stasinski
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0),
          SPIF_SENDCHANGE);
        BringWindowToTop(hwnd); // IE 5.5 related hack
        SetForegroundWindow(hWnd);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
      end;
    end
    else
    begin
      BringWindowToTop(hwnd); // IE 5.5 related hack
      SetForegroundWindow(hwnd);
    end;

    Result := (GetForegroundWindow = hwnd);
  end;                                                                
end;

{-------------------------------------------------------------------------------
  ForceForegroundWindow2 (sketchy hack)
-------------------------------------------------------------------------------}
procedure ForceForegroundWindow2(hwnd: THandle);
var
  hlp: TForm;
  p: TPoint;
begin
  hlp:= TForm.Create(nil);
  try
    hlp.BorderStyle := bsNone;
    hlp.FormStyle := fsStayOnTop;
    GetCursorPos(p);
    hlp.SetBounds(p.X, p.Y, 1, 1);
    hlp.Show;
    hlp.Left:= p.X;
    hlp.Top:= p.Y;
    mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    mouse_event(MOUSEEVENTF_ABSOLUTE or MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    SetForegroundWindow(hwnd);
  finally
    hlp.Free;
  end;
end;

{-------------------------------------------------------------------------------
  FindDialogWindow (if AProcessID = 0, MainThread is used)
-------------------------------------------------------------------------------}
var
  fDlgWindow: HWND;
  
  function EnumWindowsProc(hHwnd: HWND; lParam : integer): boolean; stdcall;
  var
    pid : DWORD;
    ClassName : string;
  begin
    if (hHwnd=0) then
    begin
      Result:= false;
    end
    else
    begin
      GetWindowThreadProcessId(hHwnd, pid);

      if pid <> DWORD(lParam) then
      begin
        Result:= true;
        Exit;
      end;

      SetLength(ClassName, 255);
      SetLength(ClassName, GetClassName(hHwnd, PChar(ClassName), 255));

      if ClassName = '#32770' then
      begin
        if IsWindowVisible(hHwnd) then
        fDlgWindow:= hHwnd;
      end;
      
      Result:= fDlgWindow = 0;
    end;
  end;

{-------------------------------------------------------------------------------
  FindDialogWindow
-------------------------------------------------------------------------------}
function FindDialogWindow(AProcessID: DWORD = 0): HWND;
var
  pid: DWORD;
begin
  fDlgWindow:= 0;
  if AProcessID <> 0 then
  pid:= AProcessID
  else
  GetWindowThreadProcessId(Application.MainFormHandle, pid);
  EnumWindows(@EnumWindowsProc, pid);
  Result:= fDlgWindow;
end;

{-------------------------------------------------------------------------------
  WindowToModuleFileName
-------------------------------------------------------------------------------}
function WindowToModuleFileName(const Window: HWND): WideString;
type
  TGetModuleFileNameEx = function(hProcess: THandle; hModule: HMODULE; FileName: PWideChar; nSize: DWORD): DWORD; stdcall;
  TQueryFullProcessImageName = function(HProcess: THandle; dwFlags: DWORD; lpExeName: PWideChar; lpdwSize: PDWORD): integer; stdcall;
var
  FileName: array[0..300] of WideChar;
  DllHinst: HMODULE;
  ProcessID: DWORD;
  HProcess: THandle;
  GetModuleFileNameExAddress: TGetModuleFileNameEx;
  QueryFullProcessImageNameAddress: TQueryFullProcessImageName;
begin
  Result := '';
  if Window <> 0 then
  begin
    Windows.GetWindowThreadProcessId(Window, @ProcessID);
    hProcess := Windows.OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, ProcessID);
    if hProcess <> 0 then
    begin
      if GetWinVersion() < WVWin2000 then
      raise ECEWin32Error.CreateRes(@RsEWindowsVersionNotSupported)

      else if IsWindowsVista then
      begin
        DllHinst:= LoadLibrary('Kernel32.dll');
        if DllHinst < HINSTANCE_ERROR then
        begin
          try
            QueryFullProcessImageNameAddress:= GetProcAddress(DllHinst, 'QueryFullProcessImageNameW');
            if Assigned(QueryFullProcessImageNameAddress) then
            begin
              QueryFullProcessImageNameAddress(hProcess, 0, FileName, PDWORD(sizeof(FileName)));
              Result := FileName;
            end
            else
            begin
              raise ECEError.CreateResFmt(@RsEFunctionNotFound, ['Kernel32.dll', 'QueryFullProcessImageNameW']);
            end
          finally
            FreeLibrary(DllHinst);
          end;
        end
        else
          raise ECEError.CreateResFmt(@RsELibraryNotFound, ['Kernel32.dll']);
      end
      else
      begin
        DllHinst:= LoadLibrary('Psapi.dll');
        if DllHinst < HINSTANCE_ERROR then
        begin
          try
            GetModuleFileNameExAddress:= GetProcAddress(DllHinst, 'GetModuleFileNameExW');
            if Assigned(GetModuleFileNameExAddress) then
            begin
              GetModuleFileNameExAddress(hProcess, 0, FileName, sizeof(FileName));
              Result:= FileName;
            end
            else
            begin
              raise ECEError.CreateResFmt(@RsEFunctionNotFound, ['Psapi.dll', 'GetModuleFileNameExW']);
            end
          finally
            FreeLibrary(DllHinst);
          end;
        end
        else
          raise ECEError.CreateResFmt(@RsELibraryNotFound, ['Psapi.dll']);
      end;
    end
    else
      raise ECEError.CreateResFmt(@RsEProcessNotValid, [ProcessID]);
  end
  else
    raise ECEError.CreateResFmt(@RsEWindowNotValid, [Window]);
end;

{-------------------------------------------------------------------------------
  GetModulePath
-------------------------------------------------------------------------------}
function GetModulePath(const Module: HMODULE): WideString;
var
  L: Integer;
  path: String;
begin
  L:= MAX_PATH + 1;
  if IsUnicode then
  begin
    SetLength(Result, L);
    L:= Windows.GetModuleFileNameW(Module, Pointer(Result), L);
    SetLength(Result, L);
  end
  else
  begin
    SetLength(path, L);
    L:= Windows.GetModuleFileName(Module, Pointer(path), L);
    SetLength(path, L);
    Result:= path;
  end;
  
end;

{##############################################################################}
// TCEFileVersionInfo

{-------------------------------------------------------------------------------
  Attach
-------------------------------------------------------------------------------}
constructor TCEFileVersionInfo.Attach(VersionInfoData: Pointer; Size: Integer);
begin
  SetLength(FBuffer, Size);
  CopyMemory(PAnsiChar(FBuffer), VersionInfoData, Size);
  ExtractData;
end;

{-------------------------------------------------------------------------------
  Create an instance of TCEFileVersionInfo
-------------------------------------------------------------------------------}
constructor TCEFileVersionInfo.Create(const FileName: WideString);
var
  Handle: DWORD;
  Size: DWORD;
begin
  if not WideFileExists(FileName) then
  raise ECEFileVersionInfoError.CreateResFmt(@RsFileUtilsFileDoesNotExist, [FileName]);

  // get info size
  Handle:= 0;
  if IsUnicode then
  Size:= GetFileVersionInfoSizeW(PWideChar(FileName), Handle)
  else
  Size:= GetFileVersionInfoSizeA(PChar(String(FileName)), Handle);

  if Size = 0 then
  raise ECEFileVersionInfoError.CreateRes(@RsFileUtilsNoVersionInfo);

  // get info
  SetLength(FBuffer, Size);
  if IsUnicode then
  Win32Check(GetFileVersionInfoW(PWideChar(FileName), Handle, Size, PAnsiChar(FBuffer)))
  else
  Win32Check(GetFileVersionInfoA(PChar(String(FileName)), Handle, Size, PAnsiChar(FBuffer)));

  // extract data
  ExtractData;
end;

{-------------------------------------------------------------------------------
  Create an instance of TCEFileVersionInfo
-------------------------------------------------------------------------------}
constructor TCEFileVersionInfo.Create(const Window: HWND);
begin
  Create(WindowToModuleFileName(Window));
end;

{-------------------------------------------------------------------------------
  Create an instance of TCEFileVersionInfo
-------------------------------------------------------------------------------}
constructor TCEFileVersionInfo.Create(const Module: HMODULE);
begin
  if Module <> 0 then
    Create(GetModulePath(Module))
  else
    raise ECEError.CreateResFmt(@RsEModuleNotValid, [Module]);
end;

{-------------------------------------------------------------------------------
  Destroy TCEFileVersionInfo
-------------------------------------------------------------------------------}
destructor TCEFileVersionInfo.Destroy;
begin
  FreeAndNil(FItemList);
  FreeAndNil(FItems);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Check LanguageIndex
-------------------------------------------------------------------------------}
procedure TCEFileVersionInfo.CheckLanguageIndex(Value: Integer);
begin
  if (Value < 0) or (Value >= LanguageCount) then
  raise ECEFileVersionInfoError.CreateRes(@RsFileUtilsLanguageIndex);
end;

{-------------------------------------------------------------------------------
  CreateItemsForLanguage
-------------------------------------------------------------------------------}
procedure TCEFileVersionInfo.CreateItemsForLanguage;
var
  I: Integer;
begin
  Items.Clear;
  for I := 0 to FItemList.Count - 1 do
  begin
    if Integer(FItemList.Objects[I]) = FLanguageIndex then
    Items.AddObject(FItemList[I], Pointer(FLanguages[FLanguageIndex].Pair));
  end;
end;

{-------------------------------------------------------------------------------
  ExtractData
-------------------------------------------------------------------------------}
procedure TCEFileVersionInfo.ExtractData;
var
  Data, EndOfData: PAnsiChar;
  Len, ValueLen, DataType: Word;
  HeaderSize: Integer;
  Key: string;
  Error, IsUnicode: Boolean;

  procedure Padding(var DataPtr: PAnsiChar);
  begin
    while TJclAddr(DataPtr) and 3 <> 0 do
    Inc(DataPtr);
  end;

  procedure GetHeader;
  var
    P: PAnsiChar;
    TempKey: PWideChar;
  begin
    P:= Data;
    Len:= PWord(P)^;
    if Len = 0 then
    begin
      Error:= True;
      Exit;
    end;
    Inc(P, SizeOf(Word));
    ValueLen:= PWord(P)^;
    Inc(P, SizeOf(Word));
    if IsUnicode then
    begin
      DataType:= PWord(P)^;
      Inc(P, SizeOf(Word));
      TempKey:= PWideChar(P);
      Inc(P, (lstrlenW(TempKey) + 1) * SizeOf(WideChar)); // length + #0#0
      Key:= TempKey;
    end
    else
    begin
      DataType:= 1;
      Key:= string(PAnsiChar(P));
      Inc(P, lstrlenA(PAnsiChar(P)) + 1);
    end;
    Padding(P);
    HeaderSize:= P - Data;
    Data:= P;
  end;

  procedure FixKeyValue;
  const
    HexNumberCPrefix = '0x';
  var
    I: Integer;
  begin // GAPI32.DLL version 5.5.2803.1 contanins '04050x04E2' value
    repeat
      I:= Pos(HexNumberCPrefix, Key);
      if I > 0 then
      Delete(Key, I, Length(HexNumberCPrefix));
    until I = 0;

    I:= 1;
    while I <= Length(Key) do
    begin
      if CharIsHexDigit(Key[I]) then
        Inc(I)
      else
        Delete(Key, I, 1);
    end;
  end;

  procedure ProcessStringInfo(Size: Integer);
  var
    EndPtr, EndStringPtr: PAnsiChar;
    LangIndex: Integer;
    LangIdRec: TLangIdRec;
    Value: string;
  begin
    EndPtr:= Data + Size;
    LangIndex:= 0;
    while not Error and (Data < EndPtr) do
    begin
      GetHeader; // StringTable
      FixKeyValue;
      if (ValueLen <> 0) or (Length(Key) <> 8) then
      begin
        Error:= True;
        Break;
      end;
      Padding(Data);
      LangIdRec.LangId:= StrToIntDef('$' + Copy(Key, 1, 4), 0);
      LangIdRec.CodePage:= StrToIntDef('$' + Copy(Key, 5, 4), 0);
      SetLength(FLanguages, LangIndex + 1);
      FLanguages[LangIndex]:= LangIdRec;
      EndStringPtr:= Data + Len - HeaderSize;
      while not Error and (Data < EndStringPtr) do
      begin
        GetHeader; // string
        case DataType of
          0:
            if ValueLen in [1..4] then
              Value:= Format('$%.*x', [ValueLen * 2, PInteger(Data)^])
            else
            begin
              if (ValueLen > 0) and IsUnicode then
                Value:=PWideChar(Data)
              else
                Value:= '';
            end;
          1:
            if ValueLen = 0 then
              Value:= ''
            else
            if IsUnicode then
            begin
              Value:= WideCharLenToString(PWideChar(Data), ValueLen);
              StrResetLength(Value);
            end
            else
              Value:= string(PAnsiChar(Data));
        else
          Error:= True;
          Break;
        end;
        Inc(Data, Len - HeaderSize);
        Padding(Data); // String.Padding
        FItemList.AddObject(Format('%s=%s', [Key, Value]), Pointer(LangIndex));
      end;
      Inc(LangIndex);
    end;
  end;

  procedure ProcessVarInfo;
  var
    TranslationIndex: Integer;
  begin
    GetHeader; // Var
    if SameText(Key, 'Translation') then
    begin
      SetLength(FTranslations, ValueLen div SizeOf(TLangIdRec));
      for TranslationIndex:= 0 to Length(FTranslations) - 1 do
      begin
        FTranslations[TranslationIndex]:= PLangIdRec(Data)^;
        Inc(Data, SizeOf(TLangIdRec));
      end;
    end;
  end;

begin
  FItemList:= TStringList.Create;
  FItems:= TStringList.Create;
  Data:= Pointer(FBuffer);
  Assert(TJclAddr(Data) mod 4 = 0);
  IsUnicode:= (PWord(Data + 4)^ in [0, 1]);
  Error:= True;
  GetHeader;
  EndOfData:= Data + Len - HeaderSize;
  if SameText(Key, 'VS_VERSION_INFO') and (ValueLen = SizeOf(TVSFixedFileInfo)) then
  begin
    FFixedInfo:= PVSFixedFileInfo(Data);
    Error:= FFixedInfo.dwSignature <> $FEEF04BD;
    Inc(Data, ValueLen); // VS_FIXEDFILEINFO
    Padding(Data);       // VS_VERSIONINFO.Padding2
    while not Error and (Data < EndOfData) do
    begin
      GetHeader;
      Inc(Data, ValueLen); // some files (VREDIR.VXD 4.00.1111) has non zero value of ValueLen
      Dec(Len, HeaderSize + ValueLen);
      if SameText(Key, 'StringFileInfo') then
        ProcessStringInfo(Len)
      else
      if SameText(Key, 'VarFileInfo') then
        ProcessVarInfo
      else
        Break;
    end;
    ExtractFlags;
    CreateItemsForLanguage;
  end;
  
  if Error then
  raise ECEFileVersionInfoError.CreateRes(@RsFileUtilsNoVersionInfo);
end;

{-------------------------------------------------------------------------------
  ExtractFlags
-------------------------------------------------------------------------------}
procedure TCEFileVersionInfo.ExtractFlags;
var
  Masked: DWORD;
begin
  FFileFlags:= [];
  Masked:= FFixedInfo^.dwFileFlags and FFixedInfo^.dwFileFlagsMask;
  if (Masked and VS_FF_DEBUG) <> 0 then
  Include(FFileFlags, ffDebug);
  if (Masked and VS_FF_INFOINFERRED) <> 0 then
  Include(FFileFlags, ffInfoInferred);
  if (Masked and VS_FF_PATCHED) <> 0 then
  Include(FFileFlags, ffPatched);
  if (Masked and VS_FF_PRERELEASE) <> 0 then
  Include(FFileFlags, ffPreRelease);
  if (Masked and VS_FF_PRIVATEBUILD) <> 0 then
  Include(FFileFlags, ffPrivateBuild);
  if (Masked and VS_FF_SPECIALBUILD) <> 0 then
  Include(FFileFlags, ffSpecialBuild);
end;

{-------------------------------------------------------------------------------
  GetBinFileVersion
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetBinFileVersion: string;
begin
  Result:= Format('%u.%u.%u.%u', [HiWord(FFixedInfo^.dwFileVersionMS),
    LoWord(FFixedInfo^.dwFileVersionMS), HiWord(FFixedInfo^.dwFileVersionLS),
    LoWord(FFixedInfo^.dwFileVersionLS)]);
end;

{-------------------------------------------------------------------------------
  GetBinProductVersion
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetBinProductVersion: string;
begin
  Result:= Format('%u.%u.%u.%u', [HiWord(FFixedInfo^.dwProductVersionMS),
    LoWord(FFixedInfo^.dwProductVersionMS), HiWord(FFixedInfo^.dwProductVersionLS),
    LoWord(FFixedInfo^.dwProductVersionLS)]);
end;

{-------------------------------------------------------------------------------
  GetCustomFieldValue
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetCustomFieldValue(const FieldName: string): string;
var
  ItemIndex: Integer;
begin
  if FieldName <> '' then
  begin
    ItemIndex:= FItems.IndexOfName(FieldName);
    if ItemIndex <> -1 then
      //Return the required value, the value the user passed in was found.
      Result:= FItems.Values[FieldName]
    else
      raise ECEFileVersionInfoError.CreateResFmt(@RsFileUtilsValueNotFound, [FieldName]);
  end
  else
    raise ECEFileVersionInfoError.CreateRes(@RsFileUtilsEmptyValue);
end;

{-------------------------------------------------------------------------------
  GetFileOS
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetFileOS: DWORD;
begin
  Result:= FFixedInfo^.dwFileOS;
end;

{-------------------------------------------------------------------------------
  GetFileSubType
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetFileSubType: DWORD;
begin
  Result:= FFixedInfo^.dwFileSubtype;
end;

{-------------------------------------------------------------------------------
  GetFileType
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetFileType: DWORD;
begin
  Result:= FFixedInfo^.dwFileType;
end;

{-------------------------------------------------------------------------------
  GetFileVersionBuild
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetFileVersionBuild: string;
var
  Left: Integer;
begin
  Result:= FileVersion;
  StrReplaceChar(Result, ',', '.');
  Left:= CharLastPos(Result, '.') + 1;
  Result:= StrMid(Result, Left, Length(Result) - Left + 1);
  Result:= Trim(Result);
end;

{-------------------------------------------------------------------------------
  GetFileVersionMajor
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetFileVersionMajor: string;
begin
  Result:= FileVersion;
  StrReplaceChar(Result, ',', '.');
  Result:= StrBefore('.', Result);
  Result:= Trim(Result);
end;

{-------------------------------------------------------------------------------
  GetFileVersionMinor
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetFileVersionMinor: string;
var
  Left, Right: integer;
begin
  Result:= FileVersion;
  StrReplaceChar(Result, ',', '.');
  Left:= CharPos(Result, '.') + 1;           // skip major
  Right:= CharPos(Result, '.', Left) {-1};
  Result:= StrMid(Result, Left, Right - Left {+1});
  Result:= Trim(Result);
end;

{-------------------------------------------------------------------------------
  GetFileVersionRelease
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetFileVersionRelease: string;
var
  Left, Right: Integer;
begin
  Result:= FileVersion;
  StrReplaceChar(Result, ',', '.');
  Left:= CharPos(Result, '.') + 1;           // skip major
  Left:= CharPos(Result, '.', Left) + 1;     // skip minor
  Right:= CharPos(Result, '.', Left) {-1};
  Result:= StrMid(Result, Left, Right - Left {+1});
  Result:= Trim(Result);
end;

{-------------------------------------------------------------------------------
  GetFixedInfo
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetFixedInfo: TVSFixedFileInfo;
begin
  Result:= FFixedInfo^;
end;

{-------------------------------------------------------------------------------
  GetItems
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetItems: TStrings;
begin
  Result:= FItems;
end;

{-------------------------------------------------------------------------------
  GetLanguageCount
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetLanguageCount: Integer;
begin
  Result:= Length(FLanguages);
end;

{-------------------------------------------------------------------------------
  GetLanguageIds
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetLanguageIds(Index: Integer): string;
begin
  CheckLanguageIndex(Index);
  Result:= VersionLanguageId(FLanguages[Index]);
end;

{-------------------------------------------------------------------------------
  GetLanguages
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetLanguages(Index: Integer): TLangIdRec;
begin
  CheckLanguageIndex(Index);
  Result:= FLanguages[Index];
end;

{-------------------------------------------------------------------------------
  GetLanguageNames
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetLanguageNames(Index: Integer): string;
begin
  CheckLanguageIndex(Index);
  Result:= VersionLanguageName(FLanguages[Index].LangId);
end;

{-------------------------------------------------------------------------------
  GetTranslationCount
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetTranslationCount: Integer;
begin
  Result:= Length(FTranslations);
end;

{-------------------------------------------------------------------------------
  GetTranslations
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetTranslations(Index: Integer): TLangIdRec;
begin
  Result:= FTranslations[Index];
end;

{-------------------------------------------------------------------------------
  GetProductVersionBuild
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetProductVersionBuild: string;
var
  Left: Integer;
begin
  Result:= ProductVersion;
  StrReplaceChar(Result, ',', '.');
  Left:= CharLastPos(Result, '.') + 1;
  Result:= StrMid(Result, Left, Length(Result) - Left + 1);
  Result:= Trim(Result);
end;

{-------------------------------------------------------------------------------
  GetProductVersionMajor
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetProductVersionMajor: string;
begin
  Result:= ProductVersion;
  StrReplaceChar(Result, ',', '.');
  Result:= StrBefore('.', Result);
  Result:= Trim(Result);
end;

{-------------------------------------------------------------------------------
  GetProductVersionMinor
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetProductVersionMinor: string;
var
  Left, Right: integer;
begin
  Result:= ProductVersion;
  StrReplaceChar(Result, ',', '.');
  Left:= CharPos(Result, '.') + 1;           // skip major
  Right:= CharPos(Result, '.', Left) {-1};
  Result:= StrMid(Result, Left, Right - Left {+1});
  Result:= Trim(Result);
end;

{-------------------------------------------------------------------------------
  GetProductVersionRelease
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetProductVersionRelease: string;
var
  Left, Right: Integer;
begin
  Result:= ProductVersion;
  StrReplaceChar(Result, ',', '.');
  Left:= CharPos(Result, '.') + 1;           // skip major
  Left:= CharPos(Result, '.', Left) + 1;     // skip minor
  Right:= CharPos(Result, '.', Left) {-1};
  Result:= StrMid(Result, Left, Right - Left {+1});
  Result:= Trim(Result);
end;

{-------------------------------------------------------------------------------
  GetVersionKeyValue
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.GetVersionKeyValue(Index: Integer): string;
begin
  Result:= Items.Values[VerKeyNames[Index]];
end;

{-------------------------------------------------------------------------------
  SetLanguageIndex
-------------------------------------------------------------------------------}
procedure TCEFileVersionInfo.SetLanguageIndex(const Value: Integer);
begin
  CheckLanguageIndex(Value);
  if FLanguageIndex <> Value then
  begin
    FLanguageIndex:= Value;
    CreateItemsForLanguage;
  end;
end;

{-------------------------------------------------------------------------------
  TranslationMatchesLanguages
-------------------------------------------------------------------------------}
function TCEFileVersionInfo.TranslationMatchesLanguages(Exact: Boolean): Boolean;
var
  TransIndex, LangIndex: Integer;
  TranslationPair: DWORD;
begin
  Result:= (LanguageCount = TranslationCount) or (not Exact and (TranslationCount > 0));
  if Result then
  begin
    for TransIndex:= 0 to TranslationCount - 1 do
    begin
      TranslationPair:= FTranslations[TransIndex].Pair;
      LangIndex:= LanguageCount - 1;
      while (LangIndex >= 0) and (TranslationPair <> FLanguages[LangIndex].Pair) do
        Dec(LangIndex);
      if LangIndex < 0 then
      begin
        Result:= False;
        Break;
      end;
    end;
  end;
end;
{-------------------------------------------------------------------------------
  VersionLanguageId
-------------------------------------------------------------------------------}
class function TCEFileVersionInfo.VersionLanguageId(const LangIdRec: TLangIdRec): string;
begin
  with LangIdRec do
  Result:= Format('%.4x%.4x', [LangId, CodePage]);
end;

{-------------------------------------------------------------------------------
  VersionLanguageName
-------------------------------------------------------------------------------}
class function TCEFileVersionInfo.VersionLanguageName(const LangId: Word): string;
var
  R: DWORD;
begin
  SetLength(Result, MAX_PATH);
  R:= VerLanguageName(LangId, PChar(Result), MAX_PATH);
  SetLength(Result, R);
end;

{##############################################################################}

initialization
  ExePath:= WideExtractFilePath(WideParamStr(0));
  SettingsDirPath:= GetSettingsFolderPath(ReadOnlySettings, true);
  LargeShellIconSize:= GetLargeShellIconSize;
  SmallShellIconSize:= GetSmallShellIconSize;
  CELoadShellProcs;
  fIsWindowsVista:= GetWinVersion = wvWinVista;
  fIsWindows64:= GetIsWindows64;

finalization

end.
