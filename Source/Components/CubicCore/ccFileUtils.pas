//******************************************************************************
//  CubicCore
//  Version: 1.00
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
//  The Original Code is ccFileUtils.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccFileUtils;

{==============================================================================}
interface

uses
  // CubicCore
  ccBase, ccStrings, ccTypes, ccSysUtils,
  // System Units
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}

  {$ENDIF}
  SysUtils;

const
  faReadOnly  = $00000001;
  faHidden    = $00000002;
  faSysFile   = $00000004;
  faDirectory = $00000010;
  faArchive   = $00000020;
  faSymLink   = $00000040;
  faNormal    = $00000080;
  faTemporary = $00000100;
  faAnyFile   = $000001FF;
  faRoot      = $00000200;
  faParent    = $00000400;

type
  TCCSearchRec = record
    Time: Integer;
    Size: Int64;
    Attr: Integer;
    Name: TWideFileName;
    {$IFDEF MSWINDOWS}
    FindHandle: THandle;
    FindData: TWin32FindDataW;
    {$ELSE}
    FindData: TSearchRec;
    {$ENDIF}
  end;

{-------------------------------------------------------------------------------
  Public Functions
-------------------------------------------------------------------------------}
  function WideFileCreate(const FileName: WideString): Integer;
  function WideFileOpen(const FileName: WideString; Mode: LongWord): Integer;
  function WideExpandFileName(const FileName, BaseDir: WideString): WideString;
      overload;
  function WideFindFirst(const Path: WideString; Attr: LongInt; out Rslt:
    TCCSearchRec): LongInt;
  function WideFindNext(var Rslt: TCCSearchRec): Integer;
  procedure WideFindClose(var Rslt: TCCSearchRec);
  function WideFileExists(const FileName: WideString): Boolean;
  function WideFileGetAttr(const FileName: WideString): Integer;
  function WideDirectoryExists(const Dir: WideString): Boolean;
  function WideIsPathDelimiter(const S: WideString; Index: Integer): Boolean;
  function WideLastDelimiter(const Delimiters, S: WideString): Integer;
  function WideIncludeTrailingPathDelimiter(const S: WideString): WideString;
  function WideExcludeTrailingPathDelimiter(const S: WideString): WideString;
  function WideExtractFileDir(const FileName: WideString): WideString;
  function WideExtractFilePath(const FileName: WideString): WideString;
  function WideExtractFileName(const FileName: WideString): WideString;
  function WideExtractFileDrive(const FileName: WideString): WideString;
  function WideExtractFileExt(const FileName: WideString; RemoveDot: Boolean =
      false): WideString;
  function WideExtractRelativePath(const BaseName, DestName: WideString):
    WideString; 
  function WideSetCurrentDir(const Dir: WideString): Boolean;
  function WideExpandFileName(const FileName: WideString): WideString; overload;
  function WideGetCurrentDir: WideString;
  function WideFileAge(const FileName: WideString): Integer; overload;
  function WideDeleteFile(const FileName: WideString): Boolean;
  function WideCreateDir(const ADirPath: WideString): Boolean;
  function WideForceDirectories(ADirPath: WideString): Boolean;

var
  AppPath: WideString = '';
  AppDirPath: WideString = '';

{==============================================================================}
implementation

{##############################################################################}
// Private methods

function _PrepareFindResult(var Rslt: TCCSearchRec): Integer;
{$IFDEF MSWINDOWS}
var
  LocalFileTime: TFileTime;
  l: Integer;
begin
  FileTimeToLocalFileTime(Rslt.FindData.ftLastWriteTime, LocalFileTime);
  FileTimeToDosDateTime(LocalFileTime, LongRec(Rslt.Time).Hi, LongRec(Rslt.Time).Lo);
  Rslt.Size:= (Int64(Rslt.FindData.nFileSizeHigh) shl 32) + Rslt.FindData.nFileSizeLow;
  Rslt.Attr:= Rslt.FindData.dwFileAttributes;
  Rslt.Name:= Rslt.FindData.cFileName;
  // Add faRoot or faParent attributes
  l:= Length(Rslt.Name);
  if (l > 0) and (Rslt.Name[1] = '.') then
  begin
    if l = 1 then
    Rslt.Attr:= Rslt.Attr or faRoot
    else if (l = 2) and (Rslt.Name[2] = '.') then
    Rslt.Attr:= Rslt.Attr or faParent;
  end;
  Result:= 0;
end;
{$ELSE}
var
  l: Integer;
begin
  Rslt.Time:= Rslt.FindData.Time;
  Rslt.Size:= Rslt.FindData.Size;
  Rslt.Attr:= Rslt.FindData.Attr;

  // Add faRoot, faParent and faHidden attributes
  //   There is no hidden attribute in Unix, check for dot prefix to find hidden files/folders.
  l:= Length(Rslt.FindData.Name);
  if (l > 0) and (Rslt.FindData.Name[1] = '.') then
  begin
    if (Rslt.Attr and faDirectory) = faDirectory then
    begin
      if l = 1 then
      Rslt.Attr:= Rslt.Attr or faRoot
      else if (l = 2) and (Rslt.FindData.Name[2] = '.') then
      Rslt.Attr:= Rslt.Attr or faParent
      else
      Rslt.Attr:= Rslt.Attr or faHidden
    end
    else
    Rslt.Attr:= Rslt.Attr or faHidden;
  end;

  Rslt.Name:= UTF8Decode(Rslt.FindData.Name);
  Result:= 0;
end;
{$ENDIF}

{##############################################################################}
// Public methods

{-------------------------------------------------------------------------------
  WideFindFirst
-------------------------------------------------------------------------------}
function WideFindFirst(const Path: WideString; Attr: LongInt; out Rslt: TCCSearchRec): LongInt;
{$IFDEF MSWINDOWS}
begin
  Rslt.FindHandle:= FindFirstFileW(PWideChar(Path), Rslt.FindData);
  if Rslt.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result:= _PrepareFindResult(Rslt);
    if Result <> 0 then
    WideFindClose(Rslt);
  end
  else
    Result:= GetLastError;
end;
{$ELSE}
begin
  if FindFirst(UTF8Encode(Path), Attr, Rslt.FindData) = S_OK then
  Result:= _PrepareFindResult(Rslt)
  else
  Result:= S_FAIL;
  if Result <> S_OK then
  WideFindClose(Rslt);
end;
{$ENDIF}

{-------------------------------------------------------------------------------
  WideFindNext
-------------------------------------------------------------------------------}
function WideFindNext(var Rslt: TCCSearchRec): Integer;
{$IFDEF MSWINDOWS}
begin
  if FindNextFileW(Rslt.FindHandle, Rslt.FindData) then
  Result:= _PrepareFindResult(Rslt)
  else
  Result:= GetLastError;
end;
{$ELSE}
begin
  if FindNext(Rslt.FindData) = S_OK then
  Result:= _PrepareFindResult(Rslt)
  else
  Result:= S_FAIL;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
  WideFindClose
-------------------------------------------------------------------------------}
procedure WideFindClose(var Rslt: TCCSearchRec);
{$IFDEF MSWINDOWS}
begin
  if Rslt.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Rslt.FindHandle);
    Rslt.FindHandle:= INVALID_HANDLE_VALUE;
  end;
end;
{$ELSE}
begin
  FindClose(Rslt.FindData);
end;
{$ENDIF}


{-------------------------------------------------------------------------------
  WideFileCreate
-------------------------------------------------------------------------------}
function WideFileCreate(const FileName: WideString): Integer;
begin
{$IFDEF MSWINDOWS}
  if WinIsUnicode then
  begin
    Result:= Integer(CreateFileW(PWideChar(FileName),
                                 GENERIC_READ or GENERIC_WRITE,
                                 0,
                                 nil,
                                 CREATE_ALWAYS,
                                 FILE_ATTRIBUTE_NORMAL,
                                 0))
  end
  else
  begin
    Result:= Integer(CreateFileA(PAnsiChar(AnsiString(FileName)),
                                 GENERIC_READ or GENERIC_WRITE,
                                 0,
                                 nil,
                                 CREATE_ALWAYS,
                                 FILE_ATTRIBUTE_NORMAL,
                                 0))
  end;
{$ELSE}
  Result:= FileCreate(UTF8Encode(FileName));
{$ENDIF}
end;

{-------------------------------------------------------------------------------
  WideFileOpen
-------------------------------------------------------------------------------}
function WideFileOpen(const FileName: WideString; Mode: LongWord): Integer;
{$IFDEF MSWINDOWS}
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  if WinIsUnicode then
  begin
    Result:= Integer(CreateFileW(PWideChar(FileName),
                                 AccessMode[Mode and 3],
                                 ShareMode[(Mode and $F0) shr 4],
                                 nil,
                                 OPEN_EXISTING,
                                 FILE_ATTRIBUTE_NORMAL,
                                 0))
  end
  else
  begin
    Result:= Integer(CreateFileA(PAnsiChar(AnsiString(FileName)),
                                 AccessMode[Mode and 3],
                                 ShareMode[(Mode and $F0) shr 4],
                                 nil,
                                 OPEN_EXISTING,
                                 FILE_ATTRIBUTE_NORMAL,
                                 0))
  end;
end;
{$ELSE}
begin
  Result:= FileOpen(UTF8Encode(FileName), Mode);
end;
{$ENDIF}

{-------------------------------------------------------------------------------
  WideExpandFileName (Uses current dir as base)
-------------------------------------------------------------------------------}
function WideExpandFileName(const FileName: WideString): WideString;
{$IFDEF MSWINDOWS}
var
  fName: PWideChar;
  Buffer: array[0..MAX_PATH - 1] of WideChar;
begin
  fName:= nil;
  SetString(Result, Buffer, GetFullPathNameW(PWideChar(FileName), MAX_PATH, Buffer, fName));
end;
{$ELSE}
begin
  Result:= ExpandFileName(UTF8Encode(FileName));
end;
{$ENDIF}

{-------------------------------------------------------------------------------
  WideExpandFileName
-------------------------------------------------------------------------------}
function WideExpandFileName(const FileName, BaseDir: WideString): WideString;
var
  oldDir: WideString;
  {$IFDEF MSWINDOWS}
  fName: PWideChar;
  Buffer: array[0..MAX_PATH - 1] of WideChar;
  {$ENDIF}
begin
  oldDir:= WideGetCurrentDir;
  try
    WideSetCurrentDir(BaseDir);
    {$IFDEF MSWINDOWS}
    fName:= nil;
    SetString(Result, Buffer, GetFullPathNameW(PWideChar(FileName), MAX_PATH, Buffer, fName));
    {$ELSE}
     Result:= ExpandFileName(UTF8Encode(FileName));
     {$ENDIF}
  finally
    WideSetCurrentDir(oldDir);
  end;
end;

{-------------------------------------------------------------------------------
  WideFileExists (Returns false if FileName is folder, even in unix!)
-------------------------------------------------------------------------------}
function WideFileExists(const FileName: WideString): Boolean;
var
  attr: Integer;
begin
  // TODO: not the fastest routine in Linux
  attr:= WideFileGetAttr(FileName);
  Result:= (attr > -1) and ((attr and faDirectory) = 0);
end;

{-------------------------------------------------------------------------------
  WideDirectoryExists
-------------------------------------------------------------------------------}
function WideDirectoryExists(const Dir: WideString): Boolean;
var
  attr: Integer;
begin
  attr:= WideFileGetAttr(Dir);
  Result:= (attr > -1) and ((attr and faDirectory) = faDirectory);
end;

{-------------------------------------------------------------------------------
  WideFileGetAttr
-------------------------------------------------------------------------------}
function WideFileGetAttr(const FileName: WideString): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result:= Integer(GetFileAttributesW(PWideChar(FileName)));
  {$ELSE}
  Result:= FileGetAttr(UTF8Encode(FileName));
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  WideIsPathDelimiter
-------------------------------------------------------------------------------}
function WideIsPathDelimiter(const S: WideString; Index: Integer): Boolean;
begin
  Result:= (Index > 0) and (Index <= Length(S)) and (S[Index] = PathDelim);
end;

{-------------------------------------------------------------------------------
  WideLastDelimiter (Returns the index of last delimiter)
-------------------------------------------------------------------------------}
function WideLastDelimiter(const Delimiters, S: WideString): Integer;
var
  P: PWideChar;
begin
  Result:= Length(S);
  P:= PWideChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (WideStrScan(P, S[Result]) <> nil) then
    Exit; // -->
    Dec(Result);
  end;
end;

{-------------------------------------------------------------------------------
  WideIncludeTrailingPathDelimiter
-------------------------------------------------------------------------------}
function WideIncludeTrailingPathDelimiter(const S: WideString): WideString;
begin
  Result:= S;
  if not WideIsPathDelimiter(Result, Length(Result)) then
  Result:= Result + PathDelim;
end;

{-------------------------------------------------------------------------------
  WideExcludeTrailingPathDelimiter
-------------------------------------------------------------------------------}
function WideExcludeTrailingPathDelimiter(const S: WideString): WideString;
begin
  Result:= S;
  if WideIsPathDelimiter(Result, Length(Result)) then
  SetLength(Result, Length(Result)-1);
end;

{-------------------------------------------------------------------------------
  WideExtractFileDir (Doesn't include trailing path delimiter)
-------------------------------------------------------------------------------}
function WideExtractFileDir(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I:= WideLastDelimiter(DriveDelim + PathDelim, Filename);

  if (I > 1) and (FileName[I] = PathDelim) and
     (not (FileName[I - 1] = DriveDelim)) and (not (FileName[I - 1] = PathDelim)) then
  Dec(I);

  Result:= Copy(FileName, 1, I);
end;

{-------------------------------------------------------------------------------
  WideExtractFilePath (Includes trailing path delimiter)
-------------------------------------------------------------------------------}
function WideExtractFilePath(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I:= WideLastDelimiter(DriveDelim + PathDelim, FileName);
  Result:= Copy(FileName, 1, I);
end;

{-------------------------------------------------------------------------------
  WideExtractFileName
-------------------------------------------------------------------------------}
function WideExtractFileName(const FileName: WideString): WideString;
var
  I: Integer;
begin
  {$IFDEF MSWINDOWS}
  I:= WideLastDelimiter('\:/', FileName);
  {$ELSE}
  I:= WideLastDelimiter('/', FileName);
  {$ENDIF}
  Result:= Copy(FileName, I + 1, MaxInt);
end;

{-------------------------------------------------------------------------------
  WideExtractFileDrive (Returns C: etc. On non windows returns empty string)
-------------------------------------------------------------------------------}
function WideExtractFileDrive(const FileName: WideString): WideString;
{$IFDEF MSWINDOWS}
var
  i, j: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = DriveDelim) then
  Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and
          (FileName[1] = PathDelim) and
          (FileName[2] = PathDelim) then
  begin
    j:= 0;
    i:= 3;
    while (i < Length(FileName)) and (j < 2) do
    begin
      if FileName[i] = PathDelim then Inc(j);
      if j < 2 then Inc(i);
    end;
    if FileName[i] = PathDelim then Dec(i);
    Result:= Copy(FileName, 1, i);
  end
  else
  Result:= '';
end;
{$ELSE}
begin
  Result:= '';
end;
{$ENDIF}

{-------------------------------------------------------------------------------
  WideExtractFileExt
-------------------------------------------------------------------------------}
function WideExtractFileExt(const FileName: WideString; RemoveDot: Boolean =
    false): WideString;
var
  i: Integer;
  ch: PWideChar;
begin
  i:= WideLastDelimiter('.' + DriveDelim + PathDelim, FileName);
  if (i > 0) and (FileName[i] = '.') then
  begin
    Result:= Copy(FileName, i, MaxInt);
    if RemoveDot and (Length(Result) > 0) then
    begin
      ch:= Pointer(Result);
      if (ch^ = '.') then
      begin
        Inc(ch);
        Result:= ch;
      end;
    end;
  end
  else
  Result:= '';
end;

{-------------------------------------------------------------------------------
  WideExtractRelativePath
-------------------------------------------------------------------------------}
function WideExtractRelativePath(const BaseName, DestName: WideString): WideString;
var
  BasePath, DestPath: WideString;
  BaseLead, DestLead: PWideChar;
  BasePtr, DestPtr: PWideChar;
  {$IFDEF MSWINDOWS}
  BaseDrive, DestDrive: WideString;
  {$ENDIF}

  function Next(var Lead: PWideChar): PWideChar;
  begin
    Result:= Lead;
    if Result = nil then
    Exit;
    Lead:= WideStrScan(Lead, PathDelim);
    if Lead <> nil then
    begin
      Lead^:= #0;
      Inc(Lead);
    end;
  end;

begin
{$IFDEF MSWINDOWS}
  BaseDrive:= WideExtractFileDrive(BaseName);
  DestDrive:= WideExtractFileDrive(DestName);
  if ccStrings.WideCompareText(BaseDrive, DestDrive) = 0 then
  begin
    BasePath:= WideExtractFilePath(BaseName);
    Delete(BasePath, 1, Length(BaseDrive));
    DestPath:= WideExtractFilePath(DestName);
    Delete(DestPath, 1, Length(DestDrive));
{$ELSE}
    BasePath:= WideExtractFilePath(BaseName);
    DestPath:= WideExtractFilePath(DestName);
{$ENDIF}
    BaseLead:= Pointer(BasePath);
    BasePtr:= Next(BaseLead);
    DestLead:= Pointer(DestPath);
    DestPtr:= Next(DestLead);
    while (BasePtr <> nil) and (DestPtr <> nil) and
          {$IFDEF MSWINDOWS}
          (ccStrings.WideCompareText(BasePtr, DestPtr) = 0) do
          {$ELSE}
          (ccStrings.WideCompareStr(BasePtr, DestPtr) = 0) do
          {$ENDIF}
    begin
      BasePtr:= Next(BaseLead);
      DestPtr:= Next(DestLead);
    end;
    Result:= '';
    while BaseLead <> nil do
    begin
      Result:= Result + '..' + PathDelim;             { Do not localize }
      Next(BaseLead);
    end;
    if (DestPtr <> nil) and (DestPtr^ <> #0) then
      Result:= Result + DestPtr + PathDelim;
    if DestLead <> nil then
      Result:= Result + DestLead;     // destlead already has a trailing backslash
    Result:= Result + WideExtractFileName(DestName);
{$IFDEF MSWINDOWS}
  end
  else
    Result:= DestName;
{$ENDIF}
end;

{-------------------------------------------------------------------------------
  WideSetCurrentDir
-------------------------------------------------------------------------------}
function WideSetCurrentDir(const Dir: WideString): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result:= SetCurrentDirectoryW(PWideChar(Dir));
  {$ELSE}
  Result:= SetCurrentDir(UTF8Encode(Dir));
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  WideGetCurrentDir
-------------------------------------------------------------------------------}
function WideGetCurrentDir: WideString;
begin
  {$IFDEF MSWINDOWS}
  SetLength(Result, MAX_PATH);
  GetCurrentDirectoryW(MAX_PATH, PWideChar(Result));
  Result:= PWideChar(Result);
  {$ELSE}
  Result:= UTF8Decode(GetCurrentDir);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  WideFileAge
-------------------------------------------------------------------------------}
function WideFileAge(const FileName: WideString): Integer;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  FindData: TWin32FindDataW;
  LocalFileTime: TFileTime;
begin
  Handle:= FindFirstFileW(PWideChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi, LongRec(Result).Lo) then
      Exit; // -->
    end;
  end;
  Result:= -1;
end;
{$ELSE}
begin
  Result:= FileAge(UTF8Encode(FileName));
end;
{$ENDIF}

{-------------------------------------------------------------------------------
  WideDeleteFile
-------------------------------------------------------------------------------}
function WideDeleteFile(const FileName: WideString): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result:= DeleteFileW(PWideChar(FileName));
  {$ELSE}
  Result:= DeleteFile(UTF8Encode(FileName));
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  WideCreateDir
-------------------------------------------------------------------------------}
function WideCreateDir(const ADirPath: WideString): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result:= CreateDirectoryW(PWideChar(ADirPath), nil);
  {$ELSE}
  Result:= CreateDir(UTF8Encode(ADirPath));
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  WideForceDirectories
-------------------------------------------------------------------------------}
function WideForceDirectories(ADirPath: WideString): Boolean;
begin
  if ADirPath <> '' then
  begin
    Result:= True;
    ADirPath:= WideExcludeTrailingPathDelimiter(ADirPath);

    if (Length(ADirPath) < 3) or
        WideDirectoryExists(ADirPath) or
        (WideExtractFilePath(ADirPath) = ADirPath) then
    Exit;

    Result:= WideForceDirectories(WideExtractFilePath(ADirPath));
    if Result then
    Result:= WideCreateDir(ADirPath)
  end
  else
  Result:= false;
end;

{==============================================================================}
initialization
  AppPath:= WideParamStr(0);
  AppDirPath:= WideExtractFilePath(AppPath);

finalization

end.
