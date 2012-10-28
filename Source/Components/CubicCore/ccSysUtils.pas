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
//  The Original Code is ccSysUtils.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccSysUtils;

{$INCLUDE Core.inc}

{==============================================================================}
interface

uses
  // CubicCore
  ccStrings,
  // System Units
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes;

{-------------------------------------------------------------------------------
  Public Methods
-------------------------------------------------------------------------------}
  function WideParamCount: Integer;
  function WideParamStr(Index: Integer): WideString;
  function RunsInConsole: Boolean;
  procedure WideWriteLnSafe(Line: WideString);
  procedure WideWriteLn(const Line: WideString);
  function IsEqualGUID(const Guid1, Guid2: TGUID): Boolean;

function WideLoadLibrary(AFilePath: WideString): Cardinal;

{$IFDEF MSWINDOWS}
var
  WinIsUnicode: Boolean;
  WinIsXP: Boolean;
  WinIs2003: Boolean;
  WinIsVista: Boolean;
  WinIsWin7: Boolean;
{$ENDIF}

{==============================================================================}
implementation

{##############################################################################}
// Private Methods

{-------------------------------------------------------------------------------
  WideGetParamStr
-------------------------------------------------------------------------------}
function WideGetParamStr(P: PWideChar; var Param: WideString): PWideChar;
var
  i, Len: Integer;
  Start, S, Q: PWideChar;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
    Inc(P);

    if (P[0] = '"') and (P[1] = '"') then
    Inc(P, 2)
    else
    Break;
  end;

  Len:= 0;
  Start:= P;

  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q:= P + 1;
        Inc(Len, Q - P);
        P:= Q;
      end;
      if P[0] <> #0 then
      Inc(P);
    end
    else
    begin
      Q:= P + 1;
      Inc(Len, Q - P);
      P:= Q;
    end;
  end;

  SetLength(Param, Len);

  P:= Start;
  S:= PWideChar(Param);
  i:= 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Q:= P + 1;
        while P < Q do
        begin
          S[i]:= P^;
          Inc(P);
          Inc(i);
        end;
      end;
      if P[0] <> #0 then
      Inc(P);
    end
    else
    begin
      Q:= P + 1;
      while P < Q do
      begin
        S[i]:= P^;
        Inc(P);
        Inc(i);
      end;
    end;
  end;

  Result:= P;
end;

{##############################################################################}
// Public methods

{-------------------------------------------------------------------------------
  WideParamCount
-------------------------------------------------------------------------------}
function WideParamCount: Integer;
{$IFDEF MSWindows}
var
  P: PWideChar;
  S: WideString;
begin
  P:= WideGetParamStr(GetCommandLineW, S);
  Result:= 0;
  while True do
  begin
    P:= WideGetParamStr(P, S);
    if S = '' then Break;
    Inc(Result);
  end;
end;
{$ELSE}
begin
  Result:= ParamCount;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
  WideParamStr
-------------------------------------------------------------------------------}
function WideParamStr(Index: Integer): WideString;
{$IFDEF MSWindows}
var
  P: PWideChar;
begin
  if Index = 0 then
  begin
    SetLength(Result, MAX_PATH);
    GetModuleFileNameW(0, PWideChar(Result), Length(Result));
    Result:= PWideChar(Result);
  end
  else
  begin
    P:= GetCommandLineW;
    while True do
    begin
      P:= WideGetParamStr(P, Result);
      if (Index = 0) or (Result = '') then
      Break;
      Dec(Index);
    end;
  end;
end;
{$ELSE}
begin
  Result:= UTF8Decode(ParamStr(Index));
end;
{$ENDIF}

{-------------------------------------------------------------------------------
  RunsInConsole
-------------------------------------------------------------------------------}
function RunsInConsole: Boolean;
{$IFDEF MSWINDOWS}
var
  Stdout: THandle;
begin
  Stdout:= GetStdHandle(Cardinal(Std_Output_Handle));
  Result:= (Stdout <> Invalid_Handle_Value) and (Stdout <> 0);
end;
{$ELSE}
begin
  Result:= true;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
  WideWriteLnSafe
-------------------------------------------------------------------------------}
procedure WideWriteLnSafe(Line: WideString);
begin
  if RunsInConsole then
  WideWriteLn(Line);
end;

{-------------------------------------------------------------------------------
  WideWriteLn
-------------------------------------------------------------------------------}
procedure WideWriteLn(const Line: WideString);
begin
  {$IFDEF MSWindows}SetConsoleOutputCP(65001);{$ENDIF}
  writeln(UTF8Encode(Line));
end;

{-------------------------------------------------------------------------------
  IsEqualGUID
-------------------------------------------------------------------------------}
function IsEqualGUID(const Guid1, Guid2: TGUID): Boolean;
var
  a1,a2: PIntegerArray;
begin
  a1:=PIntegerArray(@guid1);
  a2:=PIntegerArray(@guid2);
  Result:=(a1^[0]=a2^[0]) and
          (a1^[1]=a2^[1]) and
          (a1^[2]=a2^[2]) and
          (a1^[3]=a2^[3]);
end;

{-------------------------------------------------------------------------------
  WideLoadLibrary
-------------------------------------------------------------------------------}
function WideLoadLibrary(AFilePath: WideString): Cardinal;
begin
  {$IFDEF MSWINDOWS}
  Result:= LoadLibraryW(PWideChar(AFilePath));
  {$ELSE}
  Result:= LoadLibrary(UTF8Encode(AFilePath));
  {$ENDIF}
end;

{==============================================================================}
initialization
  {$IFDEF MSWINDOWS}
  WinIsUnicode:= (Win32Platform = VER_PLATFORM_WIN32_NT);
  WinIsXP:= ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))
            or  (Win32MajorVersion > 5);
  WinIs2003:= ((Win32MajorVersion = 5) and (Win32MinorVersion >= 2))
              or  (Win32MajorVersion > 5);
  WinIsVista:= ((Win32MajorVersion = 6) and (Win32MinorVersion >= 0))
               or (Win32MajorVersion > 6);
  WinIsWin7:= ((Win32MajorVersion = 6) and (Win32MinorVersion >= 1))
               or (Win32MajorVersion > 6);

  {$ENDIF}
end.
