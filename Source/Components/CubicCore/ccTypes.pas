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
//  The Original Code is ccTypes.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccTypes;

{$INCLUDE Core.inc}

{==============================================================================}
interface

uses
  {$IFDEF MSWindows}
  Windows,
  {$ENDIF}
  Types;

{==============================================================================}
type
  TWideFileName = type WideString;

  TRect = Types.TRect;
  TPoint = Types.TPoint;

  {$IFDEF MSWindows}
  HWND = Windows.HWND;
  {$ELSE}
  HWND = type LongWord;
  {$ENDIF}

  TCCInsertMode = (imBefore, imAfter, imFirstChild, imLastChild);

{==============================================================================}
implementation

end.
