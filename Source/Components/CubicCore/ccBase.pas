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
//  The Original Code is ccBase.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccBase;

{$INCLUDE Core.inc}

{==============================================================================}
interface

type
  {$IFDEF MATH_EXTENDED_PRECISION}
  Float = Extended;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  Float = Double;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  Float = Single;
  {$ENDIF MATH_SINGLE_PRECISION}
  PFloat = ^Float;

  {$IFDEF FPC}
  Largeint = Int64;
  {$ELSE ~FPC}
  {$IFDEF CPU32}
  SizeInt = Integer;
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  SizeInt = NativeInt;
  {$ENDIF CPU64}
  PSizeInt = ^SizeInt;
  PPointer = ^Pointer;
  PByte = System.PByte;
  Int8 = ShortInt;
  Int16 = Smallint;
  Int32 = Integer;
  UInt8 = Byte;
  UInt16 = Word;
  UInt32 = LongWord;
  PCardinal = ^Cardinal;
  {$IFNDEF COMPILER7_UP}
  UInt64 = Int64;
  {$ENDIF ~COMPILER7_UP}
  PWideChar = System.PWideChar;
  PPWideChar = ^ccBase.PWideChar;
  PPAnsiChar = ^PAnsiChar;
  PInt64 = type System.PInt64;
  {$ENDIF ~FPC}
  PPInt64 = ^PInt64;
  PPPAnsiChar = ^PPAnsiChar;

  PShortAnsi = ^ShortAnsi;
  ShortAnsi = array [0..255] of AnsiChar;
const
  MaxShortAnsi = 256;

const
  MinByte     = Low(Byte);
  MaxByte     = High(Byte);
  MinWord     = Low(Word);
  MaxWord     = High(Word);
  MinShortInt = Low(ShortInt);
  MaxShortInt = High(ShortInt);
  MinSmallInt = Low(SmallInt);
  MaxSmallInt = High(SmallInt);
  MinLongWord = LongWord(Low(LongWord));
  MaxLongWord = LongWord(High(LongWord));
  MinLongInt  = LongInt(Low(LongInt));
  MaxLongInt  = LongInt(High(LongInt));
  MaxInt64    = Int64(High(Int64));
  MinInt64    = Int64(Low(Int64));
  MinInteger  = Integer(Low(Integer));
  MaxInteger  = Integer(High(Integer));
  MinCardinal = Cardinal(Low(Cardinal));
  MaxCardinal = Cardinal(High(Cardinal));

const
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

{==============================================================================}
implementation

end.
