//******************************************************************************
//  CubicCore
//  Version: 1.0
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
//  The Original Code is from:
//   - JclWideString.pas (JCL library)
//   - JclUnicode.pas    (JCL library)
//   - cUnicode.pas.     (Fundamentals library)
//   - ccStrings.pas     (CubicCore library)
//
//  The Initial Developers of the JCL library Code are:
//   - Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
//   - Mike Lischke (WideQuotedStr & WideExtractQuotedStr from Unicode.pas)
//  Portions created by Andreas Hausladen are Copyright (C) of Andreas Hausladen. All rights reserved.
//  Portions created by Mike Lischke are Copyright (C) of Mike Lischke. All rights reserved.
//
//  The Initial Developer of the Fundamentals library Code is David J Butler.
//  Portions created by David J Butler are Copyright (C) of David J Butler. All rights reserved.
//                                                                                            
//  The Initial Developer of the CubicCore library Code is Marko Savolainen (cubicreality at gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit ccStrings;

{$INCLUDE Core.inc}

{==============================================================================}
interface

uses
  // CubicCore
  ccBase,
  // System Units
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils;

type
{$IFDEF DELPHI5}
  UCS4Char = LongWord;
{$ENDIF}
{$IFDEF CLR}
  UCS4Char = LongWord;
{$ENDIF}
  UnicodeChar = UCS4Char;
{$IFNDEF SUPPORTS_UNICODE_STRING}
  UnicodeString = WideString;
{$ENDIF}
  UnicodeStringArray = array of UnicodeString;

const
  WideNULL = WideChar(#0);
  WideSOH  = WideChar(#1);
  WideSTX  = WideChar(#2);
  WideETX  = WideChar(#3);
  WideEOT  = WideChar(#4);
  WideENQ  = WideChar(#5);
  WideACK  = WideChar(#6);
  WideBEL  = WideChar(#7);
  WideBS   = WideChar(#8);
  WideHT   = WideChar(#9);
  WideLF   = WideChar(#10);
  WideVT   = WideChar(#11);
  WideFF   = WideChar(#12);
  WideCR   = WideChar(#13);
  WideNAK  = WideChar(#21);
  WideSYN  = WideChar(#22);
  WideCAN  = WideChar(#24);
  WideEOF  = WideChar(#26);
  WideESC  = WideChar(#27);
  WideSP   = WideChar(#32);

  WideCRLF : WideString = #13#10;

  WideSingleQuote        = WideChar('''');
  WideDoubleQuote        = WideChar('"');

  WideNoBreakSpace       = WideChar(#$00A0);
  WideLineSeparator      = WideChar(#$2028);
  WideParagraphSeparator = WideChar(#$2029);

  WideBOM_MSB_First      = WideChar(#$FFFE);
  WideBOM_LSB_First      = WideChar(#$FEFF);

  WideObjectReplacement  = WideChar(#$FFFC);
  WideCharReplacement    = WideChar(#$FFFD);
  WideInvalid            = WideChar(#$FFFF);

  WideCopyrightSign      = WideChar(#$00A9);
  WideRegisteredSign     = WideChar(#$00AE);

  WideHighSurrogateFirst        = WideChar(#$D800);
  WideHighSurrogateLast         = WideChar(#$DB7F);
  WideLowSurrogateFirst         = WideChar(#$DC00);
  WideLowSurrogateLast          = WideChar(#$DFFF);
  WidePrivateHighSurrogateFirst = WideChar(#$DB80);
  WidePrivateHighSurrogateLast  = WideChar(#$DBFF);

{-------------------------------------------------------------------------------
  Public Methods
-------------------------------------------------------------------------------}
  function CharToWideChar(Ch: AnsiChar): WideChar;
  procedure SwapWordByteOrder(P: PWideChar; Len: SizeInt);
  procedure MoveWideChar(const Source; var Dest; Count: SizeInt);
  function WideCharToChar(Ch: WideChar): AnsiChar;
  function WideCompareText(const S1, S2: WideString): SizeInt;
  function WideCompareStr(const S1, S2: WideString): SizeInt;
  function WidePos(const SubStr, Str: WideString): SizeInt;
  function WidePPos(const SubStr, Str: PWideChar): PWideChar;
  function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
  function WideStrScan(const Str: PWideChar; Ch: WideChar): PWideChar; overload;
  function WideStrScan(Str: PWideChar; Chr: WideChar; StrLen: SizeInt): 
    PWideChar; overload;
  function WideStrNew(const Str: PWideChar): PWideChar; overload;
  function WideStrNew(const Str: WideString): PWideChar; overload;
  function WideStrEnd(const Str: PWideChar): PWideChar;
  function WideStrLen(const Str: PWideChar): SizeInt;
  function WideStrMove(Dest: PWideChar; const Source: PWideChar; Count: 
    SizeInt): PWideChar;
  function WideStrAlloc(WideSize: SizeInt): PWideChar;
  function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): 
    WideString;
  function WideStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
  function WideStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
  function WideStrComp(const Str1, Str2: PWideChar): SizeInt;
  function WideStrBufSize(const Str: PWideChar): SizeInt;
  function WideStrIComp(const Str1, Str2: PWideChar): SizeInt;
  function WideLowCase(const Ch: WideChar): WideChar;
  function WideIsSameStr(const S1, S2: WideString): Boolean;
  function WideIsSameText(const S1, S2: WideString): Boolean;
  function  WideLowCaseFolding(const Ch: WideChar): WideString;
  function  WideLowerCase(const S: WideString): WideString;
  function  WideUpCase(const Ch: WideChar): WideChar;
  function  WideUpCaseFolding(const Ch: WideChar): WideString;
  function  WideUpperCase(const S: WideString): WideString;
  function WideGetNextItem(var AFromString: WideString; const ASeparator:
      WideChar; ADeleteFromString: Boolean = true; const ADefault: WideString =
      ''): WideString;
  procedure WideTrunkFromNull(var AStr: WideString);
  procedure AnsiTrunkFromNull(var AStr: AnsiString);

  function WideReplaceChar(const Find: WideChar; const Replace: WideString; const
      AStr: WideString): WideString;
  function WideCountChar(const AChar: WideChar; const AStr: WideString): Integer;
      overload;

function WideRectToStr(ARect: TRect): WideString;

function WidePointToStr(APoint: TPoint): WideString;

function WideStrToRectDef(const AStr: WideString; ADefaultRect: TRect): TRect;

function WideStrToPointDef(const AStr: WideString; ADefaultPoint: TPoint):
    TPoint;

procedure WideStrDispose(Str: PWideChar);

procedure WideStrDisposeAndNil(var Str: PWideChar);

{==============================================================================}
implementation

{##############################################################################}
// Private constants & types

type
  TUnicodeLetterAttr = (laUpper, laLower);
  TUnicodeLetterInfo = packed record
    Unicode  : WideChar;
    Attr     : TUnicodeLetterAttr;
    CaseCode : WideChar;
  end;
  PUnicodeLetterInfo = ^TUnicodeLetterInfo;

const
  // Derived from 'Lu' and 'Ll' class
  UnicodeLetterEntries = 1492; // ~7K table
  UnicodeLetterInfo : Array[0..UnicodeLetterEntries - 1] of TUnicodeLetterInfo = (
    (Unicode:#$0041; Attr:laUpper; CaseCode:#$0061),   // LATIN CAPITAL LETTER A
    (Unicode:#$0042; Attr:laUpper; CaseCode:#$0062),   // LATIN CAPITAL LETTER B
    (Unicode:#$0043; Attr:laUpper; CaseCode:#$0063),   // LATIN CAPITAL LETTER C
    (Unicode:#$0044; Attr:laUpper; CaseCode:#$0064),   // LATIN CAPITAL LETTER D
    (Unicode:#$0045; Attr:laUpper; CaseCode:#$0065),   // LATIN CAPITAL LETTER E
    (Unicode:#$0046; Attr:laUpper; CaseCode:#$0066),   // LATIN CAPITAL LETTER F
    (Unicode:#$0047; Attr:laUpper; CaseCode:#$0067),   // LATIN CAPITAL LETTER G
    (Unicode:#$0048; Attr:laUpper; CaseCode:#$0068),   // LATIN CAPITAL LETTER H
    (Unicode:#$0049; Attr:laUpper; CaseCode:#$0069),   // LATIN CAPITAL LETTER I
    (Unicode:#$004A; Attr:laUpper; CaseCode:#$006A),   // LATIN CAPITAL LETTER J
    (Unicode:#$004B; Attr:laUpper; CaseCode:#$006B),   // LATIN CAPITAL LETTER K
    (Unicode:#$004C; Attr:laUpper; CaseCode:#$006C),   // LATIN CAPITAL LETTER L
    (Unicode:#$004D; Attr:laUpper; CaseCode:#$006D),   // LATIN CAPITAL LETTER M
    (Unicode:#$004E; Attr:laUpper; CaseCode:#$006E),   // LATIN CAPITAL LETTER N
    (Unicode:#$004F; Attr:laUpper; CaseCode:#$006F),   // LATIN CAPITAL LETTER O
    (Unicode:#$0050; Attr:laUpper; CaseCode:#$0070),   // LATIN CAPITAL LETTER P
    (Unicode:#$0051; Attr:laUpper; CaseCode:#$0071),   // LATIN CAPITAL LETTER Q
    (Unicode:#$0052; Attr:laUpper; CaseCode:#$0072),   // LATIN CAPITAL LETTER R
    (Unicode:#$0053; Attr:laUpper; CaseCode:#$0073),   // LATIN CAPITAL LETTER S
    (Unicode:#$0054; Attr:laUpper; CaseCode:#$0074),   // LATIN CAPITAL LETTER T
    (Unicode:#$0055; Attr:laUpper; CaseCode:#$0075),   // LATIN CAPITAL LETTER U
    (Unicode:#$0056; Attr:laUpper; CaseCode:#$0076),   // LATIN CAPITAL LETTER V
    (Unicode:#$0057; Attr:laUpper; CaseCode:#$0077),   // LATIN CAPITAL LETTER W
    (Unicode:#$0058; Attr:laUpper; CaseCode:#$0078),   // LATIN CAPITAL LETTER X
    (Unicode:#$0059; Attr:laUpper; CaseCode:#$0079),   // LATIN CAPITAL LETTER Y
    (Unicode:#$005A; Attr:laUpper; CaseCode:#$007A),   // LATIN CAPITAL LETTER Z
    (Unicode:#$0061; Attr:laLower; CaseCode:#$0041),   // LATIN SMALL LETTER A
    (Unicode:#$0062; Attr:laLower; CaseCode:#$0042),   // LATIN SMALL LETTER B
    (Unicode:#$0063; Attr:laLower; CaseCode:#$0043),   // LATIN SMALL LETTER C
    (Unicode:#$0064; Attr:laLower; CaseCode:#$0044),   // LATIN SMALL LETTER D
    (Unicode:#$0065; Attr:laLower; CaseCode:#$0045),   // LATIN SMALL LETTER E
    (Unicode:#$0066; Attr:laLower; CaseCode:#$0046),   // LATIN SMALL LETTER F
    (Unicode:#$0067; Attr:laLower; CaseCode:#$0047),   // LATIN SMALL LETTER G
    (Unicode:#$0068; Attr:laLower; CaseCode:#$0048),   // LATIN SMALL LETTER H
    (Unicode:#$0069; Attr:laLower; CaseCode:#$0049),   // LATIN SMALL LETTER I
    (Unicode:#$006A; Attr:laLower; CaseCode:#$004A),   // LATIN SMALL LETTER J
    (Unicode:#$006B; Attr:laLower; CaseCode:#$004B),   // LATIN SMALL LETTER K
    (Unicode:#$006C; Attr:laLower; CaseCode:#$004C),   // LATIN SMALL LETTER L
    (Unicode:#$006D; Attr:laLower; CaseCode:#$004D),   // LATIN SMALL LETTER M
    (Unicode:#$006E; Attr:laLower; CaseCode:#$004E),   // LATIN SMALL LETTER N
    (Unicode:#$006F; Attr:laLower; CaseCode:#$004F),   // LATIN SMALL LETTER O
    (Unicode:#$0070; Attr:laLower; CaseCode:#$0050),   // LATIN SMALL LETTER P
    (Unicode:#$0071; Attr:laLower; CaseCode:#$0051),   // LATIN SMALL LETTER Q
    (Unicode:#$0072; Attr:laLower; CaseCode:#$0052),   // LATIN SMALL LETTER R
    (Unicode:#$0073; Attr:laLower; CaseCode:#$0053),   // LATIN SMALL LETTER S
    (Unicode:#$0074; Attr:laLower; CaseCode:#$0054),   // LATIN SMALL LETTER T
    (Unicode:#$0075; Attr:laLower; CaseCode:#$0055),   // LATIN SMALL LETTER U
    (Unicode:#$0076; Attr:laLower; CaseCode:#$0056),   // LATIN SMALL LETTER V
    (Unicode:#$0077; Attr:laLower; CaseCode:#$0057),   // LATIN SMALL LETTER W
    (Unicode:#$0078; Attr:laLower; CaseCode:#$0058),   // LATIN SMALL LETTER X
    (Unicode:#$0079; Attr:laLower; CaseCode:#$0059),   // LATIN SMALL LETTER Y
    (Unicode:#$007A; Attr:laLower; CaseCode:#$005A),   // LATIN SMALL LETTER Z
    (Unicode:#$00AA; Attr:laLower; CaseCode:#$FFFF),   // FEMININE ORDINAL INDICATOR
    (Unicode:#$00B5; Attr:laLower; CaseCode:#$039C),   // MICRO SIGN
    (Unicode:#$00BA; Attr:laLower; CaseCode:#$FFFF),   // MASCULINE ORDINAL INDICATOR
    (Unicode:#$00C0; Attr:laUpper; CaseCode:#$00E0),   // LATIN CAPITAL LETTER A WITH GRAVE
    (Unicode:#$00C1; Attr:laUpper; CaseCode:#$00E1),   // LATIN CAPITAL LETTER A WITH ACUTE
    (Unicode:#$00C2; Attr:laUpper; CaseCode:#$00E2),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00C3; Attr:laUpper; CaseCode:#$00E3),   // LATIN CAPITAL LETTER A WITH TILDE
    (Unicode:#$00C4; Attr:laUpper; CaseCode:#$00E4),   // LATIN CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$00C5; Attr:laUpper; CaseCode:#$00E5),   // LATIN CAPITAL LETTER A WITH RING ABOVE
    (Unicode:#$00C6; Attr:laUpper; CaseCode:#$00E6),   // LATIN CAPITAL LETTER AE
    (Unicode:#$00C7; Attr:laUpper; CaseCode:#$00E7),   // LATIN CAPITAL LETTER C WITH CEDILLA
    (Unicode:#$00C8; Attr:laUpper; CaseCode:#$00E8),   // LATIN CAPITAL LETTER E WITH GRAVE
    (Unicode:#$00C9; Attr:laUpper; CaseCode:#$00E9),   // LATIN CAPITAL LETTER E WITH ACUTE
    (Unicode:#$00CA; Attr:laUpper; CaseCode:#$00EA),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00CB; Attr:laUpper; CaseCode:#$00EB),   // LATIN CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$00CC; Attr:laUpper; CaseCode:#$00EC),   // LATIN CAPITAL LETTER I WITH GRAVE
    (Unicode:#$00CD; Attr:laUpper; CaseCode:#$00ED),   // LATIN CAPITAL LETTER I WITH ACUTE
    (Unicode:#$00CE; Attr:laUpper; CaseCode:#$00EE),   // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00CF; Attr:laUpper; CaseCode:#$00EF),   // LATIN CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$00D0; Attr:laUpper; CaseCode:#$00F0),   // LATIN CAPITAL LETTER ETH
    (Unicode:#$00D1; Attr:laUpper; CaseCode:#$00F1),   // LATIN CAPITAL LETTER N WITH TILDE
    (Unicode:#$00D2; Attr:laUpper; CaseCode:#$00F2),   // LATIN CAPITAL LETTER O WITH GRAVE
    (Unicode:#$00D3; Attr:laUpper; CaseCode:#$00F3),   // LATIN CAPITAL LETTER O WITH ACUTE
    (Unicode:#$00D4; Attr:laUpper; CaseCode:#$00F4),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00D5; Attr:laUpper; CaseCode:#$00F5),   // LATIN CAPITAL LETTER O WITH TILDE
    (Unicode:#$00D6; Attr:laUpper; CaseCode:#$00F6),   // LATIN CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$00D8; Attr:laUpper; CaseCode:#$00F8),   // LATIN CAPITAL LETTER O WITH STROKE
    (Unicode:#$00D9; Attr:laUpper; CaseCode:#$00F9),   // LATIN CAPITAL LETTER U WITH GRAVE
    (Unicode:#$00DA; Attr:laUpper; CaseCode:#$00FA),   // LATIN CAPITAL LETTER U WITH ACUTE
    (Unicode:#$00DB; Attr:laUpper; CaseCode:#$00FB),   // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00DC; Attr:laUpper; CaseCode:#$00FC),   // LATIN CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$00DD; Attr:laUpper; CaseCode:#$00FD),   // LATIN CAPITAL LETTER Y WITH ACUTE
    (Unicode:#$00DE; Attr:laUpper; CaseCode:#$00FE),   // LATIN CAPITAL LETTER THORN
    (Unicode:#$00DF; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SHARP S
    (Unicode:#$00E0; Attr:laLower; CaseCode:#$00C0),   // LATIN SMALL LETTER A WITH GRAVE
    (Unicode:#$00E1; Attr:laLower; CaseCode:#$00C1),   // LATIN SMALL LETTER A WITH ACUTE
    (Unicode:#$00E2; Attr:laLower; CaseCode:#$00C2),   // LATIN SMALL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00E3; Attr:laLower; CaseCode:#$00C3),   // LATIN SMALL LETTER A WITH TILDE
    (Unicode:#$00E4; Attr:laLower; CaseCode:#$00C4),   // LATIN SMALL LETTER A WITH DIAERESIS
    (Unicode:#$00E5; Attr:laLower; CaseCode:#$00C5),   // LATIN SMALL LETTER A WITH RING ABOVE
    (Unicode:#$00E6; Attr:laLower; CaseCode:#$00C6),   // LATIN SMALL LETTER AE
    (Unicode:#$00E7; Attr:laLower; CaseCode:#$00C7),   // LATIN SMALL LETTER C WITH CEDILLA
    (Unicode:#$00E8; Attr:laLower; CaseCode:#$00C8),   // LATIN SMALL LETTER E WITH GRAVE
    (Unicode:#$00E9; Attr:laLower; CaseCode:#$00C9),   // LATIN SMALL LETTER E WITH ACUTE
    (Unicode:#$00EA; Attr:laLower; CaseCode:#$00CA),   // LATIN SMALL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00EB; Attr:laLower; CaseCode:#$00CB),   // LATIN SMALL LETTER E WITH DIAERESIS
    (Unicode:#$00EC; Attr:laLower; CaseCode:#$00CC),   // LATIN SMALL LETTER I WITH GRAVE
    (Unicode:#$00ED; Attr:laLower; CaseCode:#$00CD),   // LATIN SMALL LETTER I WITH ACUTE
    (Unicode:#$00EE; Attr:laLower; CaseCode:#$00CE),   // LATIN SMALL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00EF; Attr:laLower; CaseCode:#$00CF),   // LATIN SMALL LETTER I WITH DIAERESIS
    (Unicode:#$00F0; Attr:laLower; CaseCode:#$00D0),   // LATIN SMALL LETTER ETH
    (Unicode:#$00F1; Attr:laLower; CaseCode:#$00D1),   // LATIN SMALL LETTER N WITH TILDE
    (Unicode:#$00F2; Attr:laLower; CaseCode:#$00D2),   // LATIN SMALL LETTER O WITH GRAVE
    (Unicode:#$00F3; Attr:laLower; CaseCode:#$00D3),   // LATIN SMALL LETTER O WITH ACUTE
    (Unicode:#$00F4; Attr:laLower; CaseCode:#$00D4),   // LATIN SMALL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00F5; Attr:laLower; CaseCode:#$00D5),   // LATIN SMALL LETTER O WITH TILDE
    (Unicode:#$00F6; Attr:laLower; CaseCode:#$00D6),   // LATIN SMALL LETTER O WITH DIAERESIS
    (Unicode:#$00F8; Attr:laLower; CaseCode:#$00D8),   // LATIN SMALL LETTER O WITH STROKE
    (Unicode:#$00F9; Attr:laLower; CaseCode:#$00D9),   // LATIN SMALL LETTER U WITH GRAVE
    (Unicode:#$00FA; Attr:laLower; CaseCode:#$00DA),   // LATIN SMALL LETTER U WITH ACUTE
    (Unicode:#$00FB; Attr:laLower; CaseCode:#$00DB),   // LATIN SMALL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00FC; Attr:laLower; CaseCode:#$00DC),   // LATIN SMALL LETTER U WITH DIAERESIS
    (Unicode:#$00FD; Attr:laLower; CaseCode:#$00DD),   // LATIN SMALL LETTER Y WITH ACUTE
    (Unicode:#$00FE; Attr:laLower; CaseCode:#$00DE),   // LATIN SMALL LETTER THORN
    (Unicode:#$00FF; Attr:laLower; CaseCode:#$0178),   // LATIN SMALL LETTER Y WITH DIAERESIS
    (Unicode:#$0100; Attr:laUpper; CaseCode:#$0101),   // LATIN CAPITAL LETTER A WITH MACRON
    (Unicode:#$0101; Attr:laLower; CaseCode:#$0100),   // LATIN SMALL LETTER A WITH MACRON
    (Unicode:#$0102; Attr:laUpper; CaseCode:#$0103),   // LATIN CAPITAL LETTER A WITH BREVE
    (Unicode:#$0103; Attr:laLower; CaseCode:#$0102),   // LATIN SMALL LETTER A WITH BREVE
    (Unicode:#$0104; Attr:laUpper; CaseCode:#$0105),   // LATIN CAPITAL LETTER A WITH OGONEK
    (Unicode:#$0105; Attr:laLower; CaseCode:#$0104),   // LATIN SMALL LETTER A WITH OGONEK
    (Unicode:#$0106; Attr:laUpper; CaseCode:#$0107),   // LATIN CAPITAL LETTER C WITH ACUTE
    (Unicode:#$0107; Attr:laLower; CaseCode:#$0106),   // LATIN SMALL LETTER C WITH ACUTE
    (Unicode:#$0108; Attr:laUpper; CaseCode:#$0109),   // LATIN CAPITAL LETTER C WITH CIRCUMFLEX
    (Unicode:#$0109; Attr:laLower; CaseCode:#$0108),   // LATIN SMALL LETTER C WITH CIRCUMFLEX
    (Unicode:#$010A; Attr:laUpper; CaseCode:#$010B),   // LATIN CAPITAL LETTER C WITH DOT ABOVE
    (Unicode:#$010B; Attr:laLower; CaseCode:#$010A),   // LATIN SMALL LETTER C WITH DOT ABOVE
    (Unicode:#$010C; Attr:laUpper; CaseCode:#$010D),   // LATIN CAPITAL LETTER C WITH CARON
    (Unicode:#$010D; Attr:laLower; CaseCode:#$010C),   // LATIN SMALL LETTER C WITH CARON
    (Unicode:#$010E; Attr:laUpper; CaseCode:#$010F),   // LATIN CAPITAL LETTER D WITH CARON
    (Unicode:#$010F; Attr:laLower; CaseCode:#$010E),   // LATIN SMALL LETTER D WITH CARON
    (Unicode:#$0110; Attr:laUpper; CaseCode:#$0111),   // LATIN CAPITAL LETTER D WITH STROKE
    (Unicode:#$0111; Attr:laLower; CaseCode:#$0110),   // LATIN SMALL LETTER D WITH STROKE
    (Unicode:#$0112; Attr:laUpper; CaseCode:#$0113),   // LATIN CAPITAL LETTER E WITH MACRON
    (Unicode:#$0113; Attr:laLower; CaseCode:#$0112),   // LATIN SMALL LETTER E WITH MACRON
    (Unicode:#$0114; Attr:laUpper; CaseCode:#$0115),   // LATIN CAPITAL LETTER E WITH BREVE
    (Unicode:#$0115; Attr:laLower; CaseCode:#$0114),   // LATIN SMALL LETTER E WITH BREVE
    (Unicode:#$0116; Attr:laUpper; CaseCode:#$0117),   // LATIN CAPITAL LETTER E WITH DOT ABOVE
    (Unicode:#$0117; Attr:laLower; CaseCode:#$0116),   // LATIN SMALL LETTER E WITH DOT ABOVE
    (Unicode:#$0118; Attr:laUpper; CaseCode:#$0119),   // LATIN CAPITAL LETTER E WITH OGONEK
    (Unicode:#$0119; Attr:laLower; CaseCode:#$0118),   // LATIN SMALL LETTER E WITH OGONEK
    (Unicode:#$011A; Attr:laUpper; CaseCode:#$011B),   // LATIN CAPITAL LETTER E WITH CARON
    (Unicode:#$011B; Attr:laLower; CaseCode:#$011A),   // LATIN SMALL LETTER E WITH CARON
    (Unicode:#$011C; Attr:laUpper; CaseCode:#$011D),   // LATIN CAPITAL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011D; Attr:laLower; CaseCode:#$011C),   // LATIN SMALL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011E; Attr:laUpper; CaseCode:#$011F),   // LATIN CAPITAL LETTER G WITH BREVE
    (Unicode:#$011F; Attr:laLower; CaseCode:#$011E),   // LATIN SMALL LETTER G WITH BREVE
    (Unicode:#$0120; Attr:laUpper; CaseCode:#$0121),   // LATIN CAPITAL LETTER G WITH DOT ABOVE
    (Unicode:#$0121; Attr:laLower; CaseCode:#$0120),   // LATIN SMALL LETTER G WITH DOT ABOVE
    (Unicode:#$0122; Attr:laUpper; CaseCode:#$0123),   // LATIN CAPITAL LETTER G WITH CEDILLA
    (Unicode:#$0123; Attr:laLower; CaseCode:#$0122),   // LATIN SMALL LETTER G WITH CEDILLA
    (Unicode:#$0124; Attr:laUpper; CaseCode:#$0125),   // LATIN CAPITAL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0125; Attr:laLower; CaseCode:#$0124),   // LATIN SMALL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0126; Attr:laUpper; CaseCode:#$0127),   // LATIN CAPITAL LETTER H WITH STROKE
    (Unicode:#$0127; Attr:laLower; CaseCode:#$0126),   // LATIN SMALL LETTER H WITH STROKE
    (Unicode:#$0128; Attr:laUpper; CaseCode:#$0129),   // LATIN CAPITAL LETTER I WITH TILDE
    (Unicode:#$0129; Attr:laLower; CaseCode:#$0128),   // LATIN SMALL LETTER I WITH TILDE
    (Unicode:#$012A; Attr:laUpper; CaseCode:#$012B),   // LATIN CAPITAL LETTER I WITH MACRON
    (Unicode:#$012B; Attr:laLower; CaseCode:#$012A),   // LATIN SMALL LETTER I WITH MACRON
    (Unicode:#$012C; Attr:laUpper; CaseCode:#$012D),   // LATIN CAPITAL LETTER I WITH BREVE
    (Unicode:#$012D; Attr:laLower; CaseCode:#$012C),   // LATIN SMALL LETTER I WITH BREVE
    (Unicode:#$012E; Attr:laUpper; CaseCode:#$012F),   // LATIN CAPITAL LETTER I WITH OGONEK
    (Unicode:#$012F; Attr:laLower; CaseCode:#$012E),   // LATIN SMALL LETTER I WITH OGONEK
    (Unicode:#$0130; Attr:laUpper; CaseCode:#$0069),   // LATIN CAPITAL LETTER I WITH DOT ABOVE
    (Unicode:#$0131; Attr:laLower; CaseCode:#$0049),   // LATIN SMALL LETTER DOTLESS I
    (Unicode:#$0132; Attr:laUpper; CaseCode:#$0133),   // LATIN CAPITAL LIGATURE IJ
    (Unicode:#$0133; Attr:laLower; CaseCode:#$0132),   // LATIN SMALL LIGATURE IJ
    (Unicode:#$0134; Attr:laUpper; CaseCode:#$0135),   // LATIN CAPITAL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0135; Attr:laLower; CaseCode:#$0134),   // LATIN SMALL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0136; Attr:laUpper; CaseCode:#$0137),   // LATIN CAPITAL LETTER K WITH CEDILLA
    (Unicode:#$0137; Attr:laLower; CaseCode:#$0136),   // LATIN SMALL LETTER K WITH CEDILLA
    (Unicode:#$0138; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER KRA
    (Unicode:#$0139; Attr:laUpper; CaseCode:#$013A),   // LATIN CAPITAL LETTER L WITH ACUTE
    (Unicode:#$013A; Attr:laLower; CaseCode:#$0139),   // LATIN SMALL LETTER L WITH ACUTE
    (Unicode:#$013B; Attr:laUpper; CaseCode:#$013C),   // LATIN CAPITAL LETTER L WITH CEDILLA
    (Unicode:#$013C; Attr:laLower; CaseCode:#$013B),   // LATIN SMALL LETTER L WITH CEDILLA
    (Unicode:#$013D; Attr:laUpper; CaseCode:#$013E),   // LATIN CAPITAL LETTER L WITH CARON
    (Unicode:#$013E; Attr:laLower; CaseCode:#$013D),   // LATIN SMALL LETTER L WITH CARON
    (Unicode:#$013F; Attr:laUpper; CaseCode:#$0140),   // LATIN CAPITAL LETTER L WITH MIDDLE DOT
    (Unicode:#$0140; Attr:laLower; CaseCode:#$013F),   // LATIN SMALL LETTER L WITH MIDDLE DOT
    (Unicode:#$0141; Attr:laUpper; CaseCode:#$0142),   // LATIN CAPITAL LETTER L WITH STROKE
    (Unicode:#$0142; Attr:laLower; CaseCode:#$0141),   // LATIN SMALL LETTER L WITH STROKE
    (Unicode:#$0143; Attr:laUpper; CaseCode:#$0144),   // LATIN CAPITAL LETTER N WITH ACUTE
    (Unicode:#$0144; Attr:laLower; CaseCode:#$0143),   // LATIN SMALL LETTER N WITH ACUTE
    (Unicode:#$0145; Attr:laUpper; CaseCode:#$0146),   // LATIN CAPITAL LETTER N WITH CEDILLA
    (Unicode:#$0146; Attr:laLower; CaseCode:#$0145),   // LATIN SMALL LETTER N WITH CEDILLA
    (Unicode:#$0147; Attr:laUpper; CaseCode:#$0148),   // LATIN CAPITAL LETTER N WITH CARON
    (Unicode:#$0148; Attr:laLower; CaseCode:#$0147),   // LATIN SMALL LETTER N WITH CARON
    (Unicode:#$0149; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
    (Unicode:#$014A; Attr:laUpper; CaseCode:#$014B),   // LATIN CAPITAL LETTER ENG
    (Unicode:#$014B; Attr:laLower; CaseCode:#$014A),   // LATIN SMALL LETTER ENG
    (Unicode:#$014C; Attr:laUpper; CaseCode:#$014D),   // LATIN CAPITAL LETTER O WITH MACRON
    (Unicode:#$014D; Attr:laLower; CaseCode:#$014C),   // LATIN SMALL LETTER O WITH MACRON
    (Unicode:#$014E; Attr:laUpper; CaseCode:#$014F),   // LATIN CAPITAL LETTER O WITH BREVE
    (Unicode:#$014F; Attr:laLower; CaseCode:#$014E),   // LATIN SMALL LETTER O WITH BREVE
    (Unicode:#$0150; Attr:laUpper; CaseCode:#$0151),   // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0151; Attr:laLower; CaseCode:#$0150),   // LATIN SMALL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0152; Attr:laUpper; CaseCode:#$0153),   // LATIN CAPITAL LIGATURE OE
    (Unicode:#$0153; Attr:laLower; CaseCode:#$0152),   // LATIN SMALL LIGATURE OE
    (Unicode:#$0154; Attr:laUpper; CaseCode:#$0155),   // LATIN CAPITAL LETTER R WITH ACUTE
    (Unicode:#$0155; Attr:laLower; CaseCode:#$0154),   // LATIN SMALL LETTER R WITH ACUTE
    (Unicode:#$0156; Attr:laUpper; CaseCode:#$0157),   // LATIN CAPITAL LETTER R WITH CEDILLA
    (Unicode:#$0157; Attr:laLower; CaseCode:#$0156),   // LATIN SMALL LETTER R WITH CEDILLA
    (Unicode:#$0158; Attr:laUpper; CaseCode:#$0159),   // LATIN CAPITAL LETTER R WITH CARON
    (Unicode:#$0159; Attr:laLower; CaseCode:#$0158),   // LATIN SMALL LETTER R WITH CARON
    (Unicode:#$015A; Attr:laUpper; CaseCode:#$015B),   // LATIN CAPITAL LETTER S WITH ACUTE
    (Unicode:#$015B; Attr:laLower; CaseCode:#$015A),   // LATIN SMALL LETTER S WITH ACUTE
    (Unicode:#$015C; Attr:laUpper; CaseCode:#$015D),   // LATIN CAPITAL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015D; Attr:laLower; CaseCode:#$015C),   // LATIN SMALL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015E; Attr:laUpper; CaseCode:#$015F),   // LATIN CAPITAL LETTER S WITH CEDILLA
    (Unicode:#$015F; Attr:laLower; CaseCode:#$015E),   // LATIN SMALL LETTER S WITH CEDILLA
    (Unicode:#$0160; Attr:laUpper; CaseCode:#$0161),   // LATIN CAPITAL LETTER S WITH CARON
    (Unicode:#$0161; Attr:laLower; CaseCode:#$0160),   // LATIN SMALL LETTER S WITH CARON
    (Unicode:#$0162; Attr:laUpper; CaseCode:#$0163),   // LATIN CAPITAL LETTER T WITH CEDILLA
    (Unicode:#$0163; Attr:laLower; CaseCode:#$0162),   // LATIN SMALL LETTER T WITH CEDILLA
    (Unicode:#$0164; Attr:laUpper; CaseCode:#$0165),   // LATIN CAPITAL LETTER T WITH CARON
    (Unicode:#$0165; Attr:laLower; CaseCode:#$0164),   // LATIN SMALL LETTER T WITH CARON
    (Unicode:#$0166; Attr:laUpper; CaseCode:#$0167),   // LATIN CAPITAL LETTER T WITH STROKE
    (Unicode:#$0167; Attr:laLower; CaseCode:#$0166),   // LATIN SMALL LETTER T WITH STROKE
    (Unicode:#$0168; Attr:laUpper; CaseCode:#$0169),   // LATIN CAPITAL LETTER U WITH TILDE
    (Unicode:#$0169; Attr:laLower; CaseCode:#$0168),   // LATIN SMALL LETTER U WITH TILDE
    (Unicode:#$016A; Attr:laUpper; CaseCode:#$016B),   // LATIN CAPITAL LETTER U WITH MACRON
    (Unicode:#$016B; Attr:laLower; CaseCode:#$016A),   // LATIN SMALL LETTER U WITH MACRON
    (Unicode:#$016C; Attr:laUpper; CaseCode:#$016D),   // LATIN CAPITAL LETTER U WITH BREVE
    (Unicode:#$016D; Attr:laLower; CaseCode:#$016C),   // LATIN SMALL LETTER U WITH BREVE
    (Unicode:#$016E; Attr:laUpper; CaseCode:#$016F),   // LATIN CAPITAL LETTER U WITH RING ABOVE
    (Unicode:#$016F; Attr:laLower; CaseCode:#$016E),   // LATIN SMALL LETTER U WITH RING ABOVE
    (Unicode:#$0170; Attr:laUpper; CaseCode:#$0171),   // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0171; Attr:laLower; CaseCode:#$0170),   // LATIN SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0172; Attr:laUpper; CaseCode:#$0173),   // LATIN CAPITAL LETTER U WITH OGONEK
    (Unicode:#$0173; Attr:laLower; CaseCode:#$0172),   // LATIN SMALL LETTER U WITH OGONEK
    (Unicode:#$0174; Attr:laUpper; CaseCode:#$0175),   // LATIN CAPITAL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0175; Attr:laLower; CaseCode:#$0174),   // LATIN SMALL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0176; Attr:laUpper; CaseCode:#$0177),   // LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0177; Attr:laLower; CaseCode:#$0176),   // LATIN SMALL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0178; Attr:laUpper; CaseCode:#$00FF),   // LATIN CAPITAL LETTER Y WITH DIAERESIS
    (Unicode:#$0179; Attr:laUpper; CaseCode:#$017A),   // LATIN CAPITAL LETTER Z WITH ACUTE
    (Unicode:#$017A; Attr:laLower; CaseCode:#$0179),   // LATIN SMALL LETTER Z WITH ACUTE
    (Unicode:#$017B; Attr:laUpper; CaseCode:#$017C),   // LATIN CAPITAL LETTER Z WITH DOT ABOVE
    (Unicode:#$017C; Attr:laLower; CaseCode:#$017B),   // LATIN SMALL LETTER Z WITH DOT ABOVE
    (Unicode:#$017D; Attr:laUpper; CaseCode:#$017E),   // LATIN CAPITAL LETTER Z WITH CARON
    (Unicode:#$017E; Attr:laLower; CaseCode:#$017D),   // LATIN SMALL LETTER Z WITH CARON
    (Unicode:#$017F; Attr:laLower; CaseCode:#$0053),   // LATIN SMALL LETTER LONG S
    (Unicode:#$0180; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER B WITH STROKE
    (Unicode:#$0181; Attr:laUpper; CaseCode:#$0253),   // LATIN CAPITAL LETTER B WITH HOOK
    (Unicode:#$0182; Attr:laUpper; CaseCode:#$0183),   // LATIN CAPITAL LETTER B WITH TOPBAR
    (Unicode:#$0183; Attr:laLower; CaseCode:#$0182),   // LATIN SMALL LETTER B WITH TOPBAR
    (Unicode:#$0184; Attr:laUpper; CaseCode:#$0185),   // LATIN CAPITAL LETTER TONE SIX
    (Unicode:#$0185; Attr:laLower; CaseCode:#$0184),   // LATIN SMALL LETTER TONE SIX
    (Unicode:#$0186; Attr:laUpper; CaseCode:#$0254),   // LATIN CAPITAL LETTER OPEN O
    (Unicode:#$0187; Attr:laUpper; CaseCode:#$0188),   // LATIN CAPITAL LETTER C WITH HOOK
    (Unicode:#$0188; Attr:laLower; CaseCode:#$0187),   // LATIN SMALL LETTER C WITH HOOK
    (Unicode:#$0189; Attr:laUpper; CaseCode:#$0256),   // LATIN CAPITAL LETTER AFRICAN D
    (Unicode:#$018A; Attr:laUpper; CaseCode:#$0257),   // LATIN CAPITAL LETTER D WITH HOOK
    (Unicode:#$018B; Attr:laUpper; CaseCode:#$018C),   // LATIN CAPITAL LETTER D WITH TOPBAR
    (Unicode:#$018C; Attr:laLower; CaseCode:#$018B),   // LATIN SMALL LETTER D WITH TOPBAR
    (Unicode:#$018D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED DELTA
    (Unicode:#$018E; Attr:laUpper; CaseCode:#$01DD),   // LATIN CAPITAL LETTER REVERSED E
    (Unicode:#$018F; Attr:laUpper; CaseCode:#$0259),   // LATIN CAPITAL LETTER SCHWA
    (Unicode:#$0190; Attr:laUpper; CaseCode:#$025B),   // LATIN CAPITAL LETTER OPEN E
    (Unicode:#$0191; Attr:laUpper; CaseCode:#$0192),   // LATIN CAPITAL LETTER F WITH HOOK
    (Unicode:#$0192; Attr:laLower; CaseCode:#$0191),   // LATIN SMALL LETTER F WITH HOOK
    (Unicode:#$0193; Attr:laUpper; CaseCode:#$0260),   // LATIN CAPITAL LETTER G WITH HOOK
    (Unicode:#$0194; Attr:laUpper; CaseCode:#$0263),   // LATIN CAPITAL LETTER GAMMA
    (Unicode:#$0195; Attr:laLower; CaseCode:#$01F6),   // LATIN SMALL LETTER HV
    (Unicode:#$0196; Attr:laUpper; CaseCode:#$0269),   // LATIN CAPITAL LETTER IOTA
    (Unicode:#$0197; Attr:laUpper; CaseCode:#$0268),   // LATIN CAPITAL LETTER I WITH STROKE
    (Unicode:#$0198; Attr:laUpper; CaseCode:#$0199),   // LATIN CAPITAL LETTER K WITH HOOK
    (Unicode:#$0199; Attr:laLower; CaseCode:#$0198),   // LATIN SMALL LETTER K WITH HOOK
    (Unicode:#$019A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH BAR
    (Unicode:#$019B; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LAMBDA WITH STROKE
    (Unicode:#$019C; Attr:laUpper; CaseCode:#$026F),   // LATIN CAPITAL LETTER TURNED M
    (Unicode:#$019D; Attr:laUpper; CaseCode:#$0272),   // LATIN CAPITAL LETTER N WITH LEFT HOOK
    (Unicode:#$019E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER N WITH LONG RIGHT LEG
    (Unicode:#$019F; Attr:laUpper; CaseCode:#$0275),   // LATIN CAPITAL LETTER O WITH MIDDLE TILDE
    (Unicode:#$01A0; Attr:laUpper; CaseCode:#$01A1),   // LATIN CAPITAL LETTER O WITH HORN
    (Unicode:#$01A1; Attr:laLower; CaseCode:#$01A0),   // LATIN SMALL LETTER O WITH HORN
    (Unicode:#$01A2; Attr:laUpper; CaseCode:#$01A3),   // LATIN CAPITAL LETTER OI
    (Unicode:#$01A3; Attr:laLower; CaseCode:#$01A2),   // LATIN SMALL LETTER OI
    (Unicode:#$01A4; Attr:laUpper; CaseCode:#$01A5),   // LATIN CAPITAL LETTER P WITH HOOK
    (Unicode:#$01A5; Attr:laLower; CaseCode:#$01A4),   // LATIN SMALL LETTER P WITH HOOK
    (Unicode:#$01A6; Attr:laUpper; CaseCode:#$0280),   // LATIN LETTER YR
    (Unicode:#$01A7; Attr:laUpper; CaseCode:#$01A8),   // LATIN CAPITAL LETTER TONE TWO
    (Unicode:#$01A8; Attr:laLower; CaseCode:#$01A7),   // LATIN SMALL LETTER TONE TWO
    (Unicode:#$01A9; Attr:laUpper; CaseCode:#$0283),   // LATIN CAPITAL LETTER ESH
    (Unicode:#$01AA; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER REVERSED ESH LOOP
    (Unicode:#$01AB; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER T WITH PALATAL HOOK
    (Unicode:#$01AC; Attr:laUpper; CaseCode:#$01AD),   // LATIN CAPITAL LETTER T WITH HOOK
    (Unicode:#$01AD; Attr:laLower; CaseCode:#$01AC),   // LATIN SMALL LETTER T WITH HOOK
    (Unicode:#$01AE; Attr:laUpper; CaseCode:#$0288),   // LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
    (Unicode:#$01AF; Attr:laUpper; CaseCode:#$01B0),   // LATIN CAPITAL LETTER U WITH HORN
    (Unicode:#$01B0; Attr:laLower; CaseCode:#$01AF),   // LATIN SMALL LETTER U WITH HORN
    (Unicode:#$01B1; Attr:laUpper; CaseCode:#$028A),   // LATIN CAPITAL LETTER UPSILON
    (Unicode:#$01B2; Attr:laUpper; CaseCode:#$028B),   // LATIN CAPITAL LETTER V WITH HOOK
    (Unicode:#$01B3; Attr:laUpper; CaseCode:#$01B4),   // LATIN CAPITAL LETTER Y WITH HOOK
    (Unicode:#$01B4; Attr:laLower; CaseCode:#$01B3),   // LATIN SMALL LETTER Y WITH HOOK
    (Unicode:#$01B5; Attr:laUpper; CaseCode:#$01B6),   // LATIN CAPITAL LETTER Z WITH STROKE
    (Unicode:#$01B6; Attr:laLower; CaseCode:#$01B5),   // LATIN SMALL LETTER Z WITH STROKE
    (Unicode:#$01B7; Attr:laUpper; CaseCode:#$0292),   // LATIN CAPITAL LETTER EZH
    (Unicode:#$01B8; Attr:laUpper; CaseCode:#$01B9),   // LATIN CAPITAL LETTER EZH REVERSED
    (Unicode:#$01B9; Attr:laLower; CaseCode:#$01B8),   // LATIN SMALL LETTER EZH REVERSED
    (Unicode:#$01BA; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER EZH WITH TAIL
    (Unicode:#$01BC; Attr:laUpper; CaseCode:#$01BD),   // LATIN CAPITAL LETTER TONE FIVE
    (Unicode:#$01BD; Attr:laLower; CaseCode:#$01BC),   // LATIN SMALL LETTER TONE FIVE
    (Unicode:#$01BE; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER INVERTED GLOTTAL STOP WITH STROKE
    (Unicode:#$01BF; Attr:laLower; CaseCode:#$01F7),   // LATIN LETTER WYNN
    (Unicode:#$01C4; Attr:laUpper; CaseCode:#$01C6),   // LATIN CAPITAL LETTER DZ WITH CARON
    (Unicode:#$01C6; Attr:laLower; CaseCode:#$01C4),   // LATIN SMALL LETTER DZ WITH CARON
    (Unicode:#$01C7; Attr:laUpper; CaseCode:#$01C9),   // LATIN CAPITAL LETTER LJ
    (Unicode:#$01C9; Attr:laLower; CaseCode:#$01C7),   // LATIN SMALL LETTER LJ
    (Unicode:#$01CA; Attr:laUpper; CaseCode:#$01CC),   // LATIN CAPITAL LETTER NJ
    (Unicode:#$01CC; Attr:laLower; CaseCode:#$01CA),   // LATIN SMALL LETTER NJ
    (Unicode:#$01CD; Attr:laUpper; CaseCode:#$01CE),   // LATIN CAPITAL LETTER A WITH CARON
    (Unicode:#$01CE; Attr:laLower; CaseCode:#$01CD),   // LATIN SMALL LETTER A WITH CARON
    (Unicode:#$01CF; Attr:laUpper; CaseCode:#$01D0),   // LATIN CAPITAL LETTER I WITH CARON
    (Unicode:#$01D0; Attr:laLower; CaseCode:#$01CF),   // LATIN SMALL LETTER I WITH CARON
    (Unicode:#$01D1; Attr:laUpper; CaseCode:#$01D2),   // LATIN CAPITAL LETTER O WITH CARON
    (Unicode:#$01D2; Attr:laLower; CaseCode:#$01D1),   // LATIN SMALL LETTER O WITH CARON
    (Unicode:#$01D3; Attr:laUpper; CaseCode:#$01D4),   // LATIN CAPITAL LETTER U WITH CARON
    (Unicode:#$01D4; Attr:laLower; CaseCode:#$01D3),   // LATIN SMALL LETTER U WITH CARON
    (Unicode:#$01D5; Attr:laUpper; CaseCode:#$01D6),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D6; Attr:laLower; CaseCode:#$01D5),   // LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D7; Attr:laUpper; CaseCode:#$01D8),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D8; Attr:laLower; CaseCode:#$01D7),   // LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D9; Attr:laUpper; CaseCode:#$01DA),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DA; Attr:laLower; CaseCode:#$01D9),   // LATIN SMALL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DB; Attr:laUpper; CaseCode:#$01DC),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DC; Attr:laLower; CaseCode:#$01DB),   // LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DD; Attr:laLower; CaseCode:#$018E),   // LATIN SMALL LETTER TURNED E
    (Unicode:#$01DE; Attr:laUpper; CaseCode:#$01DF),   // LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01DF; Attr:laLower; CaseCode:#$01DE),   // LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01E0; Attr:laUpper; CaseCode:#$01E1),   // LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E1; Attr:laLower; CaseCode:#$01E0),   // LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E2; Attr:laUpper; CaseCode:#$01E3),   // LATIN CAPITAL LETTER AE WITH MACRON
    (Unicode:#$01E3; Attr:laLower; CaseCode:#$01E2),   // LATIN SMALL LETTER AE WITH MACRON
    (Unicode:#$01E4; Attr:laUpper; CaseCode:#$01E5),   // LATIN CAPITAL LETTER G WITH STROKE
    (Unicode:#$01E5; Attr:laLower; CaseCode:#$01E4),   // LATIN SMALL LETTER G WITH STROKE
    (Unicode:#$01E6; Attr:laUpper; CaseCode:#$01E7),   // LATIN CAPITAL LETTER G WITH CARON
    (Unicode:#$01E7; Attr:laLower; CaseCode:#$01E6),   // LATIN SMALL LETTER G WITH CARON
    (Unicode:#$01E8; Attr:laUpper; CaseCode:#$01E9),   // LATIN CAPITAL LETTER K WITH CARON
    (Unicode:#$01E9; Attr:laLower; CaseCode:#$01E8),   // LATIN SMALL LETTER K WITH CARON
    (Unicode:#$01EA; Attr:laUpper; CaseCode:#$01EB),   // LATIN CAPITAL LETTER O WITH OGONEK
    (Unicode:#$01EB; Attr:laLower; CaseCode:#$01EA),   // LATIN SMALL LETTER O WITH OGONEK
    (Unicode:#$01EC; Attr:laUpper; CaseCode:#$01ED),   // LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01ED; Attr:laLower; CaseCode:#$01EC),   // LATIN SMALL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01EE; Attr:laUpper; CaseCode:#$01EF),   // LATIN CAPITAL LETTER EZH WITH CARON
    (Unicode:#$01EF; Attr:laLower; CaseCode:#$01EE),   // LATIN SMALL LETTER EZH WITH CARON
    (Unicode:#$01F0; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER J WITH CARON
    (Unicode:#$01F1; Attr:laUpper; CaseCode:#$01F3),   // LATIN CAPITAL LETTER DZ
    (Unicode:#$01F3; Attr:laLower; CaseCode:#$01F1),   // LATIN SMALL LETTER DZ
    (Unicode:#$01F4; Attr:laUpper; CaseCode:#$01F5),   // LATIN CAPITAL LETTER G WITH ACUTE
    (Unicode:#$01F5; Attr:laLower; CaseCode:#$01F4),   // LATIN SMALL LETTER G WITH ACUTE
    (Unicode:#$01F6; Attr:laUpper; CaseCode:#$0195),   // LATIN CAPITAL LETTER HWAIR
    (Unicode:#$01F7; Attr:laUpper; CaseCode:#$01BF),   // LATIN CAPITAL LETTER WYNN
    (Unicode:#$01F8; Attr:laUpper; CaseCode:#$01F9),   // LATIN CAPITAL LETTER N WITH GRAVE
    (Unicode:#$01F9; Attr:laLower; CaseCode:#$01F8),   // LATIN SMALL LETTER N WITH GRAVE
    (Unicode:#$01FA; Attr:laUpper; CaseCode:#$01FB),   // LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FB; Attr:laLower; CaseCode:#$01FA),   // LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FC; Attr:laUpper; CaseCode:#$01FD),   // LATIN CAPITAL LETTER AE WITH ACUTE
    (Unicode:#$01FD; Attr:laLower; CaseCode:#$01FC),   // LATIN SMALL LETTER AE WITH ACUTE
    (Unicode:#$01FE; Attr:laUpper; CaseCode:#$01FF),   // LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$01FF; Attr:laLower; CaseCode:#$01FE),   // LATIN SMALL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$0200; Attr:laUpper; CaseCode:#$0201),   // LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0201; Attr:laLower; CaseCode:#$0200),   // LATIN SMALL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0202; Attr:laUpper; CaseCode:#$0203),   // LATIN CAPITAL LETTER A WITH INVERTED BREVE
    (Unicode:#$0203; Attr:laLower; CaseCode:#$0202),   // LATIN SMALL LETTER A WITH INVERTED BREVE
    (Unicode:#$0204; Attr:laUpper; CaseCode:#$0205),   // LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0205; Attr:laLower; CaseCode:#$0204),   // LATIN SMALL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0206; Attr:laUpper; CaseCode:#$0207),   // LATIN CAPITAL LETTER E WITH INVERTED BREVE
    (Unicode:#$0207; Attr:laLower; CaseCode:#$0206),   // LATIN SMALL LETTER E WITH INVERTED BREVE
    (Unicode:#$0208; Attr:laUpper; CaseCode:#$0209),   // LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$0209; Attr:laLower; CaseCode:#$0208),   // LATIN SMALL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$020A; Attr:laUpper; CaseCode:#$020B),   // LATIN CAPITAL LETTER I WITH INVERTED BREVE
    (Unicode:#$020B; Attr:laLower; CaseCode:#$020A),   // LATIN SMALL LETTER I WITH INVERTED BREVE
    (Unicode:#$020C; Attr:laUpper; CaseCode:#$020D),   // LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020D; Attr:laLower; CaseCode:#$020C),   // LATIN SMALL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020E; Attr:laUpper; CaseCode:#$020F),   // LATIN CAPITAL LETTER O WITH INVERTED BREVE
    (Unicode:#$020F; Attr:laLower; CaseCode:#$020E),   // LATIN SMALL LETTER O WITH INVERTED BREVE
    (Unicode:#$0210; Attr:laUpper; CaseCode:#$0211),   // LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0211; Attr:laLower; CaseCode:#$0210),   // LATIN SMALL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0212; Attr:laUpper; CaseCode:#$0213),   // LATIN CAPITAL LETTER R WITH INVERTED BREVE
    (Unicode:#$0213; Attr:laLower; CaseCode:#$0212),   // LATIN SMALL LETTER R WITH INVERTED BREVE
    (Unicode:#$0214; Attr:laUpper; CaseCode:#$0215),   // LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0215; Attr:laLower; CaseCode:#$0214),   // LATIN SMALL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0216; Attr:laUpper; CaseCode:#$0217),   // LATIN CAPITAL LETTER U WITH INVERTED BREVE
    (Unicode:#$0217; Attr:laLower; CaseCode:#$0216),   // LATIN SMALL LETTER U WITH INVERTED BREVE
    (Unicode:#$0218; Attr:laUpper; CaseCode:#$0219),   // LATIN CAPITAL LETTER S WITH COMMA BELOW
    (Unicode:#$0219; Attr:laLower; CaseCode:#$0218),   // LATIN SMALL LETTER S WITH COMMA BELOW
    (Unicode:#$021A; Attr:laUpper; CaseCode:#$021B),   // LATIN CAPITAL LETTER T WITH COMMA BELOW
    (Unicode:#$021B; Attr:laLower; CaseCode:#$021A),   // LATIN SMALL LETTER T WITH COMMA BELOW
    (Unicode:#$021C; Attr:laUpper; CaseCode:#$021D),   // LATIN CAPITAL LETTER YOGH
    (Unicode:#$021D; Attr:laLower; CaseCode:#$021C),   // LATIN SMALL LETTER YOGH
    (Unicode:#$021E; Attr:laUpper; CaseCode:#$021F),   // LATIN CAPITAL LETTER H WITH CARON
    (Unicode:#$021F; Attr:laLower; CaseCode:#$021E),   // LATIN SMALL LETTER H WITH CARON
    (Unicode:#$0222; Attr:laUpper; CaseCode:#$0223),   // LATIN CAPITAL LETTER OU
    (Unicode:#$0223; Attr:laLower; CaseCode:#$0222),   // LATIN SMALL LETTER OU
    (Unicode:#$0224; Attr:laUpper; CaseCode:#$0225),   // LATIN CAPITAL LETTER Z WITH HOOK
    (Unicode:#$0225; Attr:laLower; CaseCode:#$0224),   // LATIN SMALL LETTER Z WITH HOOK
    (Unicode:#$0226; Attr:laUpper; CaseCode:#$0227),   // LATIN CAPITAL LETTER A WITH DOT ABOVE
    (Unicode:#$0227; Attr:laLower; CaseCode:#$0226),   // LATIN SMALL LETTER A WITH DOT ABOVE
    (Unicode:#$0228; Attr:laUpper; CaseCode:#$0229),   // LATIN CAPITAL LETTER E WITH CEDILLA
    (Unicode:#$0229; Attr:laLower; CaseCode:#$0228),   // LATIN SMALL LETTER E WITH CEDILLA
    (Unicode:#$022A; Attr:laUpper; CaseCode:#$022B),   // LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022B; Attr:laLower; CaseCode:#$022A),   // LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022C; Attr:laUpper; CaseCode:#$022D),   // LATIN CAPITAL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022D; Attr:laLower; CaseCode:#$022C),   // LATIN SMALL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022E; Attr:laUpper; CaseCode:#$022F),   // LATIN CAPITAL LETTER O WITH DOT ABOVE
    (Unicode:#$022F; Attr:laLower; CaseCode:#$022E),   // LATIN SMALL LETTER O WITH DOT ABOVE
    (Unicode:#$0230; Attr:laUpper; CaseCode:#$0231),   // LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0231; Attr:laLower; CaseCode:#$0230),   // LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0232; Attr:laUpper; CaseCode:#$0233),   // LATIN CAPITAL LETTER Y WITH MACRON
    (Unicode:#$0233; Attr:laLower; CaseCode:#$0232),   // LATIN SMALL LETTER Y WITH MACRON
    (Unicode:#$0250; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED A
    (Unicode:#$0251; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER ALPHA
    (Unicode:#$0252; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED ALPHA
    (Unicode:#$0253; Attr:laLower; CaseCode:#$0181),   // LATIN SMALL LETTER B WITH HOOK
    (Unicode:#$0254; Attr:laLower; CaseCode:#$0186),   // LATIN SMALL LETTER OPEN O
    (Unicode:#$0255; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER C WITH CURL
    (Unicode:#$0256; Attr:laLower; CaseCode:#$0189),   // LATIN SMALL LETTER D WITH TAIL
    (Unicode:#$0257; Attr:laLower; CaseCode:#$018A),   // LATIN SMALL LETTER D WITH HOOK
    (Unicode:#$0258; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED E
    (Unicode:#$0259; Attr:laLower; CaseCode:#$018F),   // LATIN SMALL LETTER SCHWA
    (Unicode:#$025A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SCHWA WITH HOOK
    (Unicode:#$025B; Attr:laLower; CaseCode:#$0190),   // LATIN SMALL LETTER OPEN E
    (Unicode:#$025C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED OPEN E
    (Unicode:#$025D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED OPEN E WITH HOOK
    (Unicode:#$025E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER CLOSED REVERSED OPEN E
    (Unicode:#$025F; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DOTLESS J WITH STROKE
    (Unicode:#$0260; Attr:laLower; CaseCode:#$0193),   // LATIN SMALL LETTER G WITH HOOK
    (Unicode:#$0261; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SCRIPT G
    (Unicode:#$0262; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL G
    (Unicode:#$0263; Attr:laLower; CaseCode:#$0194),   // LATIN SMALL LETTER GAMMA
    (Unicode:#$0264; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER RAMS HORN
    (Unicode:#$0265; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED H
    (Unicode:#$0266; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER H WITH HOOK
    (Unicode:#$0267; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER HENG WITH HOOK
    (Unicode:#$0268; Attr:laLower; CaseCode:#$0197),   // LATIN SMALL LETTER I WITH STROKE
    (Unicode:#$0269; Attr:laLower; CaseCode:#$0196),   // LATIN SMALL LETTER IOTA
    (Unicode:#$026A; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL I
    (Unicode:#$026B; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH MIDDLE TILDE
    (Unicode:#$026C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH BELT
    (Unicode:#$026D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH RETROFLEX HOOK
    (Unicode:#$026E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LEZH
    (Unicode:#$026F; Attr:laLower; CaseCode:#$019C),   // LATIN SMALL LETTER TURNED M
    (Unicode:#$0270; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED M WITH LONG LEG
    (Unicode:#$0271; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER M WITH HOOK
    (Unicode:#$0272; Attr:laLower; CaseCode:#$019D),   // LATIN SMALL LETTER N WITH LEFT HOOK
    (Unicode:#$0273; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER N WITH RETROFLEX HOOK
    (Unicode:#$0274; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL N
    (Unicode:#$0275; Attr:laLower; CaseCode:#$019F),   // LATIN SMALL LETTER BARRED O
    (Unicode:#$0276; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL OE
    (Unicode:#$0277; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER CLOSED OMEGA
    (Unicode:#$0278; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER PHI
    (Unicode:#$0279; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED R
    (Unicode:#$027A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED R WITH LONG LEG
    (Unicode:#$027B; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED R WITH HOOK
    (Unicode:#$027C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER R WITH LONG LEG
    (Unicode:#$027D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER R WITH TAIL
    (Unicode:#$027E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER R WITH FISHHOOK
    (Unicode:#$027F; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED R WITH FISHHOOK
    (Unicode:#$0280; Attr:laLower; CaseCode:#$01A6),   // LATIN LETTER SMALL CAPITAL R
    (Unicode:#$0281; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL INVERTED R
    (Unicode:#$0282; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER S WITH HOOK
    (Unicode:#$0283; Attr:laLower; CaseCode:#$01A9),   // LATIN SMALL LETTER ESH
    (Unicode:#$0284; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DOTLESS J WITH STROKE AND HOOK
    (Unicode:#$0285; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SQUAT REVERSED ESH
    (Unicode:#$0286; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER ESH WITH CURL
    (Unicode:#$0287; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED T
    (Unicode:#$0288; Attr:laLower; CaseCode:#$01AE),   // LATIN SMALL LETTER T WITH RETROFLEX HOOK
    (Unicode:#$0289; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER U BAR
    (Unicode:#$028A; Attr:laLower; CaseCode:#$01B1),   // LATIN SMALL LETTER UPSILON
    (Unicode:#$028B; Attr:laLower; CaseCode:#$01B2),   // LATIN SMALL LETTER V WITH HOOK
    (Unicode:#$028C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED V
    (Unicode:#$028D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED W
    (Unicode:#$028E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED Y
    (Unicode:#$028F; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL Y
    (Unicode:#$0290; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Z WITH RETROFLEX HOOK
    (Unicode:#$0291; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Z WITH CURL
    (Unicode:#$0292; Attr:laLower; CaseCode:#$01B7),   // LATIN SMALL LETTER EZH
    (Unicode:#$0293; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER EZH WITH CURL
    (Unicode:#$0294; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER GLOTTAL STOP
    (Unicode:#$0295; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER PHARYNGEAL VOICED FRICATIVE
    (Unicode:#$0296; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER INVERTED GLOTTAL STOP
    (Unicode:#$0297; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER STRETCHED C
    (Unicode:#$0298; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER BILABIAL CLICK
    (Unicode:#$0299; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL B
    (Unicode:#$029A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER CLOSED OPEN E
    (Unicode:#$029B; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL G WITH HOOK
    (Unicode:#$029C; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL H
    (Unicode:#$029D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER J WITH CROSSED-TAIL
    (Unicode:#$029E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED K
    (Unicode:#$029F; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL L
    (Unicode:#$02A0; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Q WITH HOOK
    (Unicode:#$02A1; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER GLOTTAL STOP WITH STROKE
    (Unicode:#$02A2; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER REVERSED GLOTTAL STOP WITH STROKE
    (Unicode:#$02A3; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DZ DIGRAPH
    (Unicode:#$02A4; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DEZH DIGRAPH
    (Unicode:#$02A5; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DZ DIGRAPH WITH CURL
    (Unicode:#$02A6; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TS DIGRAPH
    (Unicode:#$02A7; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TESH DIGRAPH
    (Unicode:#$02A8; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TC DIGRAPH WITH CURL
    (Unicode:#$02A9; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER FENG DIGRAPH
    (Unicode:#$02AA; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LS DIGRAPH
    (Unicode:#$02AB; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LZ DIGRAPH
    (Unicode:#$02AC; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER BILABIAL PERCUSSIVE
    (Unicode:#$02AD; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER BIDENTAL PERCUSSIVE
    (Unicode:#$0386; Attr:laUpper; CaseCode:#$03AC),   // GREEK CAPITAL LETTER ALPHA WITH TONOS
    (Unicode:#$0388; Attr:laUpper; CaseCode:#$03AD),   // GREEK CAPITAL LETTER EPSILON WITH TONOS
    (Unicode:#$0389; Attr:laUpper; CaseCode:#$03AE),   // GREEK CAPITAL LETTER ETA WITH TONOS
    (Unicode:#$038A; Attr:laUpper; CaseCode:#$03AF),   // GREEK CAPITAL LETTER IOTA WITH TONOS
    (Unicode:#$038C; Attr:laUpper; CaseCode:#$03CC),   // GREEK CAPITAL LETTER OMICRON WITH TONOS
    (Unicode:#$038E; Attr:laUpper; CaseCode:#$03CD),   // GREEK CAPITAL LETTER UPSILON WITH TONOS
    (Unicode:#$038F; Attr:laUpper; CaseCode:#$03CE),   // GREEK CAPITAL LETTER OMEGA WITH TONOS
    (Unicode:#$0390; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    (Unicode:#$0391; Attr:laUpper; CaseCode:#$03B1),   // GREEK CAPITAL LETTER ALPHA
    (Unicode:#$0392; Attr:laUpper; CaseCode:#$03B2),   // GREEK CAPITAL LETTER BETA
    (Unicode:#$0393; Attr:laUpper; CaseCode:#$03B3),   // GREEK CAPITAL LETTER GAMMA
    (Unicode:#$0394; Attr:laUpper; CaseCode:#$03B4),   // GREEK CAPITAL LETTER DELTA
    (Unicode:#$0395; Attr:laUpper; CaseCode:#$03B5),   // GREEK CAPITAL LETTER EPSILON
    (Unicode:#$0396; Attr:laUpper; CaseCode:#$03B6),   // GREEK CAPITAL LETTER ZETA
    (Unicode:#$0397; Attr:laUpper; CaseCode:#$03B7),   // GREEK CAPITAL LETTER ETA
    (Unicode:#$0398; Attr:laUpper; CaseCode:#$03B8),   // GREEK CAPITAL LETTER THETA
    (Unicode:#$0399; Attr:laUpper; CaseCode:#$03B9),   // GREEK CAPITAL LETTER IOTA
    (Unicode:#$039A; Attr:laUpper; CaseCode:#$03BA),   // GREEK CAPITAL LETTER KAPPA
    (Unicode:#$039B; Attr:laUpper; CaseCode:#$03BB),   // GREEK CAPITAL LETTER LAMDA
    (Unicode:#$039C; Attr:laUpper; CaseCode:#$03BC),   // GREEK CAPITAL LETTER MU
    (Unicode:#$039D; Attr:laUpper; CaseCode:#$03BD),   // GREEK CAPITAL LETTER NU
    (Unicode:#$039E; Attr:laUpper; CaseCode:#$03BE),   // GREEK CAPITAL LETTER XI
    (Unicode:#$039F; Attr:laUpper; CaseCode:#$03BF),   // GREEK CAPITAL LETTER OMICRON
    (Unicode:#$03A0; Attr:laUpper; CaseCode:#$03C0),   // GREEK CAPITAL LETTER PI
    (Unicode:#$03A1; Attr:laUpper; CaseCode:#$03C1),   // GREEK CAPITAL LETTER RHO
    (Unicode:#$03A3; Attr:laUpper; CaseCode:#$03C3),   // GREEK CAPITAL LETTER SIGMA
    (Unicode:#$03A4; Attr:laUpper; CaseCode:#$03C4),   // GREEK CAPITAL LETTER TAU
    (Unicode:#$03A5; Attr:laUpper; CaseCode:#$03C5),   // GREEK CAPITAL LETTER UPSILON
    (Unicode:#$03A6; Attr:laUpper; CaseCode:#$03C6),   // GREEK CAPITAL LETTER PHI
    (Unicode:#$03A7; Attr:laUpper; CaseCode:#$03C7),   // GREEK CAPITAL LETTER CHI
    (Unicode:#$03A8; Attr:laUpper; CaseCode:#$03C8),   // GREEK CAPITAL LETTER PSI
    (Unicode:#$03A9; Attr:laUpper; CaseCode:#$03C9),   // GREEK CAPITAL LETTER OMEGA
    (Unicode:#$03AA; Attr:laUpper; CaseCode:#$03CA),   // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03AB; Attr:laUpper; CaseCode:#$03CB),   // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03AC; Attr:laLower; CaseCode:#$0386),   // GREEK SMALL LETTER ALPHA WITH TONOS
    (Unicode:#$03AD; Attr:laLower; CaseCode:#$0388),   // GREEK SMALL LETTER EPSILON WITH TONOS
    (Unicode:#$03AE; Attr:laLower; CaseCode:#$0389),   // GREEK SMALL LETTER ETA WITH TONOS
    (Unicode:#$03AF; Attr:laLower; CaseCode:#$038A),   // GREEK SMALL LETTER IOTA WITH TONOS
    (Unicode:#$03B0; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    (Unicode:#$03B1; Attr:laLower; CaseCode:#$0391),   // GREEK SMALL LETTER ALPHA
    (Unicode:#$03B2; Attr:laLower; CaseCode:#$0392),   // GREEK SMALL LETTER BETA
    (Unicode:#$03B3; Attr:laLower; CaseCode:#$0393),   // GREEK SMALL LETTER GAMMA
    (Unicode:#$03B4; Attr:laLower; CaseCode:#$0394),   // GREEK SMALL LETTER DELTA
    (Unicode:#$03B5; Attr:laLower; CaseCode:#$0395),   // GREEK SMALL LETTER EPSILON
    (Unicode:#$03B6; Attr:laLower; CaseCode:#$0396),   // GREEK SMALL LETTER ZETA
    (Unicode:#$03B7; Attr:laLower; CaseCode:#$0397),   // GREEK SMALL LETTER ETA
    (Unicode:#$03B8; Attr:laLower; CaseCode:#$0398),   // GREEK SMALL LETTER THETA
    (Unicode:#$03B9; Attr:laLower; CaseCode:#$0399),   // GREEK SMALL LETTER IOTA
    (Unicode:#$03BA; Attr:laLower; CaseCode:#$039A),   // GREEK SMALL LETTER KAPPA
    (Unicode:#$03BB; Attr:laLower; CaseCode:#$039B),   // GREEK SMALL LETTER LAMDA
    (Unicode:#$03BC; Attr:laLower; CaseCode:#$039C),   // GREEK SMALL LETTER MU
    (Unicode:#$03BD; Attr:laLower; CaseCode:#$039D),   // GREEK SMALL LETTER NU
    (Unicode:#$03BE; Attr:laLower; CaseCode:#$039E),   // GREEK SMALL LETTER XI
    (Unicode:#$03BF; Attr:laLower; CaseCode:#$039F),   // GREEK SMALL LETTER OMICRON
    (Unicode:#$03C0; Attr:laLower; CaseCode:#$03A0),   // GREEK SMALL LETTER PI
    (Unicode:#$03C1; Attr:laLower; CaseCode:#$03A1),   // GREEK SMALL LETTER RHO
    (Unicode:#$03C2; Attr:laLower; CaseCode:#$03A3),   // GREEK SMALL LETTER FINAL SIGMA
    (Unicode:#$03C3; Attr:laLower; CaseCode:#$03A3),   // GREEK SMALL LETTER SIGMA
    (Unicode:#$03C4; Attr:laLower; CaseCode:#$03A4),   // GREEK SMALL LETTER TAU
    (Unicode:#$03C5; Attr:laLower; CaseCode:#$03A5),   // GREEK SMALL LETTER UPSILON
    (Unicode:#$03C6; Attr:laLower; CaseCode:#$03A6),   // GREEK SMALL LETTER PHI
    (Unicode:#$03C7; Attr:laLower; CaseCode:#$03A7),   // GREEK SMALL LETTER CHI
    (Unicode:#$03C8; Attr:laLower; CaseCode:#$03A8),   // GREEK SMALL LETTER PSI
    (Unicode:#$03C9; Attr:laLower; CaseCode:#$03A9),   // GREEK SMALL LETTER OMEGA
    (Unicode:#$03CA; Attr:laLower; CaseCode:#$03AA),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03CB; Attr:laLower; CaseCode:#$03AB),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03CC; Attr:laLower; CaseCode:#$038C),   // GREEK SMALL LETTER OMICRON WITH TONOS
    (Unicode:#$03CD; Attr:laLower; CaseCode:#$038E),   // GREEK SMALL LETTER UPSILON WITH TONOS
    (Unicode:#$03CE; Attr:laLower; CaseCode:#$038F),   // GREEK SMALL LETTER OMEGA WITH TONOS
    (Unicode:#$03D0; Attr:laLower; CaseCode:#$0392),   // GREEK BETA SYMBOL
    (Unicode:#$03D1; Attr:laLower; CaseCode:#$0398),   // GREEK THETA SYMBOL
    (Unicode:#$03D2; Attr:laUpper; CaseCode:#$FFFF),   // GREEK UPSILON WITH HOOK SYMBOL
    (Unicode:#$03D3; Attr:laUpper; CaseCode:#$FFFF),   // GREEK UPSILON WITH ACUTE AND HOOK SYMBOL
    (Unicode:#$03D4; Attr:laUpper; CaseCode:#$FFFF),   // GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
    (Unicode:#$03D5; Attr:laLower; CaseCode:#$03A6),   // GREEK PHI SYMBOL
    (Unicode:#$03D6; Attr:laLower; CaseCode:#$03A0),   // GREEK PI SYMBOL
    (Unicode:#$03D7; Attr:laLower; CaseCode:#$FFFF),   // GREEK KAI SYMBOL
    (Unicode:#$03DA; Attr:laUpper; CaseCode:#$03DB),   // GREEK LETTER STIGMA
    (Unicode:#$03DB; Attr:laLower; CaseCode:#$03DA),   // GREEK SMALL LETTER STIGMA
    (Unicode:#$03DC; Attr:laUpper; CaseCode:#$03DD),   // GREEK LETTER DIGAMMA
    (Unicode:#$03DD; Attr:laLower; CaseCode:#$03DC),   // GREEK SMALL LETTER DIGAMMA
    (Unicode:#$03DE; Attr:laUpper; CaseCode:#$03DF),   // GREEK LETTER KOPPA
    (Unicode:#$03DF; Attr:laLower; CaseCode:#$03DE),   // GREEK SMALL LETTER KOPPA
    (Unicode:#$03E0; Attr:laUpper; CaseCode:#$03E1),   // GREEK LETTER SAMPI
    (Unicode:#$03E1; Attr:laLower; CaseCode:#$03E0),   // GREEK SMALL LETTER SAMPI
    (Unicode:#$03E2; Attr:laUpper; CaseCode:#$03E3),   // COPTIC CAPITAL LETTER SHEI
    (Unicode:#$03E3; Attr:laLower; CaseCode:#$03E2),   // COPTIC SMALL LETTER SHEI
    (Unicode:#$03E4; Attr:laUpper; CaseCode:#$03E5),   // COPTIC CAPITAL LETTER FEI
    (Unicode:#$03E5; Attr:laLower; CaseCode:#$03E4),   // COPTIC SMALL LETTER FEI
    (Unicode:#$03E6; Attr:laUpper; CaseCode:#$03E7),   // COPTIC CAPITAL LETTER KHEI
    (Unicode:#$03E7; Attr:laLower; CaseCode:#$03E6),   // COPTIC SMALL LETTER KHEI
    (Unicode:#$03E8; Attr:laUpper; CaseCode:#$03E9),   // COPTIC CAPITAL LETTER HORI
    (Unicode:#$03E9; Attr:laLower; CaseCode:#$03E8),   // COPTIC SMALL LETTER HORI
    (Unicode:#$03EA; Attr:laUpper; CaseCode:#$03EB),   // COPTIC CAPITAL LETTER GANGIA
    (Unicode:#$03EB; Attr:laLower; CaseCode:#$03EA),   // COPTIC SMALL LETTER GANGIA
    (Unicode:#$03EC; Attr:laUpper; CaseCode:#$03ED),   // COPTIC CAPITAL LETTER SHIMA
    (Unicode:#$03ED; Attr:laLower; CaseCode:#$03EC),   // COPTIC SMALL LETTER SHIMA
    (Unicode:#$03EE; Attr:laUpper; CaseCode:#$03EF),   // COPTIC CAPITAL LETTER DEI
    (Unicode:#$03EF; Attr:laLower; CaseCode:#$03EE),   // COPTIC SMALL LETTER DEI
    (Unicode:#$03F0; Attr:laLower; CaseCode:#$039A),   // GREEK KAPPA SYMBOL
    (Unicode:#$03F1; Attr:laLower; CaseCode:#$03A1),   // GREEK RHO SYMBOL
    (Unicode:#$03F2; Attr:laLower; CaseCode:#$03A3),   // GREEK LUNATE SIGMA SYMBOL
    (Unicode:#$03F3; Attr:laLower; CaseCode:#$FFFF),   // GREEK LETTER YOT
    (Unicode:#$03F4; Attr:laUpper; CaseCode:#$03B8),   // GREEK CAPITAL THETA SYMBOL
    (Unicode:#$03F5; Attr:laLower; CaseCode:#$0395),   // GREEK LUNATE EPSILON SYMBOL
    (Unicode:#$0400; Attr:laUpper; CaseCode:#$0450),   // CYRILLIC CAPITAL LETTER IE WITH GRAVE
    (Unicode:#$0401; Attr:laUpper; CaseCode:#$0451),   // CYRILLIC CAPITAL LETTER IO
    (Unicode:#$0402; Attr:laUpper; CaseCode:#$0452),   // CYRILLIC CAPITAL LETTER DJE
    (Unicode:#$0403; Attr:laUpper; CaseCode:#$0453),   // CYRILLIC CAPITAL LETTER GJE
    (Unicode:#$0404; Attr:laUpper; CaseCode:#$0454),   // CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (Unicode:#$0405; Attr:laUpper; CaseCode:#$0455),   // CYRILLIC CAPITAL LETTER DZE
    (Unicode:#$0406; Attr:laUpper; CaseCode:#$0456),   // CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (Unicode:#$0407; Attr:laUpper; CaseCode:#$0457),   // CYRILLIC CAPITAL LETTER YI
    (Unicode:#$0408; Attr:laUpper; CaseCode:#$0458),   // CYRILLIC CAPITAL LETTER JE
    (Unicode:#$0409; Attr:laUpper; CaseCode:#$0459),   // CYRILLIC CAPITAL LETTER LJE
    (Unicode:#$040A; Attr:laUpper; CaseCode:#$045A),   // CYRILLIC CAPITAL LETTER NJE
    (Unicode:#$040B; Attr:laUpper; CaseCode:#$045B),   // CYRILLIC CAPITAL LETTER TSHE
    (Unicode:#$040C; Attr:laUpper; CaseCode:#$045C),   // CYRILLIC CAPITAL LETTER KJE
    (Unicode:#$040D; Attr:laUpper; CaseCode:#$045D),   // CYRILLIC CAPITAL LETTER I WITH GRAVE
    (Unicode:#$040E; Attr:laUpper; CaseCode:#$045E),   // CYRILLIC CAPITAL LETTER SHORT U
    (Unicode:#$040F; Attr:laUpper; CaseCode:#$045F),   // CYRILLIC CAPITAL LETTER DZHE
    (Unicode:#$0410; Attr:laUpper; CaseCode:#$0430),   // CYRILLIC CAPITAL LETTER A
    (Unicode:#$0411; Attr:laUpper; CaseCode:#$0431),   // CYRILLIC CAPITAL LETTER BE
    (Unicode:#$0412; Attr:laUpper; CaseCode:#$0432),   // CYRILLIC CAPITAL LETTER VE
    (Unicode:#$0413; Attr:laUpper; CaseCode:#$0433),   // CYRILLIC CAPITAL LETTER GHE
    (Unicode:#$0414; Attr:laUpper; CaseCode:#$0434),   // CYRILLIC CAPITAL LETTER DE
    (Unicode:#$0415; Attr:laUpper; CaseCode:#$0435),   // CYRILLIC CAPITAL LETTER IE
    (Unicode:#$0416; Attr:laUpper; CaseCode:#$0436),   // CYRILLIC CAPITAL LETTER ZHE
    (Unicode:#$0417; Attr:laUpper; CaseCode:#$0437),   // CYRILLIC CAPITAL LETTER ZE
    (Unicode:#$0418; Attr:laUpper; CaseCode:#$0438),   // CYRILLIC CAPITAL LETTER I
    (Unicode:#$0419; Attr:laUpper; CaseCode:#$0439),   // CYRILLIC CAPITAL LETTER SHORT I
    (Unicode:#$041A; Attr:laUpper; CaseCode:#$043A),   // CYRILLIC CAPITAL LETTER KA
    (Unicode:#$041B; Attr:laUpper; CaseCode:#$043B),   // CYRILLIC CAPITAL LETTER EL
    (Unicode:#$041C; Attr:laUpper; CaseCode:#$043C),   // CYRILLIC CAPITAL LETTER EM
    (Unicode:#$041D; Attr:laUpper; CaseCode:#$043D),   // CYRILLIC CAPITAL LETTER EN
    (Unicode:#$041E; Attr:laUpper; CaseCode:#$043E),   // CYRILLIC CAPITAL LETTER O
    (Unicode:#$041F; Attr:laUpper; CaseCode:#$043F),   // CYRILLIC CAPITAL LETTER PE
    (Unicode:#$0420; Attr:laUpper; CaseCode:#$0440),   // CYRILLIC CAPITAL LETTER ER
    (Unicode:#$0421; Attr:laUpper; CaseCode:#$0441),   // CYRILLIC CAPITAL LETTER ES
    (Unicode:#$0422; Attr:laUpper; CaseCode:#$0442),   // CYRILLIC CAPITAL LETTER TE
    (Unicode:#$0423; Attr:laUpper; CaseCode:#$0443),   // CYRILLIC CAPITAL LETTER U
    (Unicode:#$0424; Attr:laUpper; CaseCode:#$0444),   // CYRILLIC CAPITAL LETTER EF
    (Unicode:#$0425; Attr:laUpper; CaseCode:#$0445),   // CYRILLIC CAPITAL LETTER HA
    (Unicode:#$0426; Attr:laUpper; CaseCode:#$0446),   // CYRILLIC CAPITAL LETTER TSE
    (Unicode:#$0427; Attr:laUpper; CaseCode:#$0447),   // CYRILLIC CAPITAL LETTER CHE
    (Unicode:#$0428; Attr:laUpper; CaseCode:#$0448),   // CYRILLIC CAPITAL LETTER SHA
    (Unicode:#$0429; Attr:laUpper; CaseCode:#$0449),   // CYRILLIC CAPITAL LETTER SHCHA
    (Unicode:#$042A; Attr:laUpper; CaseCode:#$044A),   // CYRILLIC CAPITAL LETTER HARD SIGN
    (Unicode:#$042B; Attr:laUpper; CaseCode:#$044B),   // CYRILLIC CAPITAL LETTER YERU
    (Unicode:#$042C; Attr:laUpper; CaseCode:#$044C),   // CYRILLIC CAPITAL LETTER SOFT SIGN
    (Unicode:#$042D; Attr:laUpper; CaseCode:#$044D),   // CYRILLIC CAPITAL LETTER E
    (Unicode:#$042E; Attr:laUpper; CaseCode:#$044E),   // CYRILLIC CAPITAL LETTER YU
    (Unicode:#$042F; Attr:laUpper; CaseCode:#$044F),   // CYRILLIC CAPITAL LETTER YA
    (Unicode:#$0430; Attr:laLower; CaseCode:#$0410),   // CYRILLIC SMALL LETTER A
    (Unicode:#$0431; Attr:laLower; CaseCode:#$0411),   // CYRILLIC SMALL LETTER BE
    (Unicode:#$0432; Attr:laLower; CaseCode:#$0412),   // CYRILLIC SMALL LETTER VE
    (Unicode:#$0433; Attr:laLower; CaseCode:#$0413),   // CYRILLIC SMALL LETTER GHE
    (Unicode:#$0434; Attr:laLower; CaseCode:#$0414),   // CYRILLIC SMALL LETTER DE
    (Unicode:#$0435; Attr:laLower; CaseCode:#$0415),   // CYRILLIC SMALL LETTER IE
    (Unicode:#$0436; Attr:laLower; CaseCode:#$0416),   // CYRILLIC SMALL LETTER ZHE
    (Unicode:#$0437; Attr:laLower; CaseCode:#$0417),   // CYRILLIC SMALL LETTER ZE
    (Unicode:#$0438; Attr:laLower; CaseCode:#$0418),   // CYRILLIC SMALL LETTER I
    (Unicode:#$0439; Attr:laLower; CaseCode:#$0419),   // CYRILLIC SMALL LETTER SHORT I
    (Unicode:#$043A; Attr:laLower; CaseCode:#$041A),   // CYRILLIC SMALL LETTER KA
    (Unicode:#$043B; Attr:laLower; CaseCode:#$041B),   // CYRILLIC SMALL LETTER EL
    (Unicode:#$043C; Attr:laLower; CaseCode:#$041C),   // CYRILLIC SMALL LETTER EM
    (Unicode:#$043D; Attr:laLower; CaseCode:#$041D),   // CYRILLIC SMALL LETTER EN
    (Unicode:#$043E; Attr:laLower; CaseCode:#$041E),   // CYRILLIC SMALL LETTER O
    (Unicode:#$043F; Attr:laLower; CaseCode:#$041F),   // CYRILLIC SMALL LETTER PE
    (Unicode:#$0440; Attr:laLower; CaseCode:#$0420),   // CYRILLIC SMALL LETTER ER
    (Unicode:#$0441; Attr:laLower; CaseCode:#$0421),   // CYRILLIC SMALL LETTER ES
    (Unicode:#$0442; Attr:laLower; CaseCode:#$0422),   // CYRILLIC SMALL LETTER TE
    (Unicode:#$0443; Attr:laLower; CaseCode:#$0423),   // CYRILLIC SMALL LETTER U
    (Unicode:#$0444; Attr:laLower; CaseCode:#$0424),   // CYRILLIC SMALL LETTER EF
    (Unicode:#$0445; Attr:laLower; CaseCode:#$0425),   // CYRILLIC SMALL LETTER HA
    (Unicode:#$0446; Attr:laLower; CaseCode:#$0426),   // CYRILLIC SMALL LETTER TSE
    (Unicode:#$0447; Attr:laLower; CaseCode:#$0427),   // CYRILLIC SMALL LETTER CHE
    (Unicode:#$0448; Attr:laLower; CaseCode:#$0428),   // CYRILLIC SMALL LETTER SHA
    (Unicode:#$0449; Attr:laLower; CaseCode:#$0429),   // CYRILLIC SMALL LETTER SHCHA
    (Unicode:#$044A; Attr:laLower; CaseCode:#$042A),   // CYRILLIC SMALL LETTER HARD SIGN
    (Unicode:#$044B; Attr:laLower; CaseCode:#$042B),   // CYRILLIC SMALL LETTER YERU
    (Unicode:#$044C; Attr:laLower; CaseCode:#$042C),   // CYRILLIC SMALL LETTER SOFT SIGN
    (Unicode:#$044D; Attr:laLower; CaseCode:#$042D),   // CYRILLIC SMALL LETTER E
    (Unicode:#$044E; Attr:laLower; CaseCode:#$042E),   // CYRILLIC SMALL LETTER YU
    (Unicode:#$044F; Attr:laLower; CaseCode:#$042F),   // CYRILLIC SMALL LETTER YA
    (Unicode:#$0450; Attr:laLower; CaseCode:#$0400),   // CYRILLIC SMALL LETTER IE WITH GRAVE
    (Unicode:#$0451; Attr:laLower; CaseCode:#$0401),   // CYRILLIC SMALL LETTER IO
    (Unicode:#$0452; Attr:laLower; CaseCode:#$0402),   // CYRILLIC SMALL LETTER DJE
    (Unicode:#$0453; Attr:laLower; CaseCode:#$0403),   // CYRILLIC SMALL LETTER GJE
    (Unicode:#$0454; Attr:laLower; CaseCode:#$0404),   // CYRILLIC SMALL LETTER UKRAINIAN IE
    (Unicode:#$0455; Attr:laLower; CaseCode:#$0405),   // CYRILLIC SMALL LETTER DZE
    (Unicode:#$0456; Attr:laLower; CaseCode:#$0406),   // CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (Unicode:#$0457; Attr:laLower; CaseCode:#$0407),   // CYRILLIC SMALL LETTER YI
    (Unicode:#$0458; Attr:laLower; CaseCode:#$0408),   // CYRILLIC SMALL LETTER JE
    (Unicode:#$0459; Attr:laLower; CaseCode:#$0409),   // CYRILLIC SMALL LETTER LJE
    (Unicode:#$045A; Attr:laLower; CaseCode:#$040A),   // CYRILLIC SMALL LETTER NJE
    (Unicode:#$045B; Attr:laLower; CaseCode:#$040B),   // CYRILLIC SMALL LETTER TSHE
    (Unicode:#$045C; Attr:laLower; CaseCode:#$040C),   // CYRILLIC SMALL LETTER KJE
    (Unicode:#$045D; Attr:laLower; CaseCode:#$040D),   // CYRILLIC SMALL LETTER I WITH GRAVE
    (Unicode:#$045E; Attr:laLower; CaseCode:#$040E),   // CYRILLIC SMALL LETTER SHORT U
    (Unicode:#$045F; Attr:laLower; CaseCode:#$040F),   // CYRILLIC SMALL LETTER DZHE
    (Unicode:#$0460; Attr:laUpper; CaseCode:#$0461),   // CYRILLIC CAPITAL LETTER OMEGA
    (Unicode:#$0461; Attr:laLower; CaseCode:#$0460),   // CYRILLIC SMALL LETTER OMEGA
    (Unicode:#$0462; Attr:laUpper; CaseCode:#$0463),   // CYRILLIC CAPITAL LETTER YAT
    (Unicode:#$0463; Attr:laLower; CaseCode:#$0462),   // CYRILLIC SMALL LETTER YAT
    (Unicode:#$0464; Attr:laUpper; CaseCode:#$0465),   // CYRILLIC CAPITAL LETTER IOTIFIED E
    (Unicode:#$0465; Attr:laLower; CaseCode:#$0464),   // CYRILLIC SMALL LETTER IOTIFIED E
    (Unicode:#$0466; Attr:laUpper; CaseCode:#$0467),   // CYRILLIC CAPITAL LETTER LITTLE YUS
    (Unicode:#$0467; Attr:laLower; CaseCode:#$0466),   // CYRILLIC SMALL LETTER LITTLE YUS
    (Unicode:#$0468; Attr:laUpper; CaseCode:#$0469),   // CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
    (Unicode:#$0469; Attr:laLower; CaseCode:#$0468),   // CYRILLIC SMALL LETTER IOTIFIED LITTLE YUS
    (Unicode:#$046A; Attr:laUpper; CaseCode:#$046B),   // CYRILLIC CAPITAL LETTER BIG YUS
    (Unicode:#$046B; Attr:laLower; CaseCode:#$046A),   // CYRILLIC SMALL LETTER BIG YUS
    (Unicode:#$046C; Attr:laUpper; CaseCode:#$046D),   // CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
    (Unicode:#$046D; Attr:laLower; CaseCode:#$046C),   // CYRILLIC SMALL LETTER IOTIFIED BIG YUS
    (Unicode:#$046E; Attr:laUpper; CaseCode:#$046F),   // CYRILLIC CAPITAL LETTER KSI
    (Unicode:#$046F; Attr:laLower; CaseCode:#$046E),   // CYRILLIC SMALL LETTER KSI
    (Unicode:#$0470; Attr:laUpper; CaseCode:#$0471),   // CYRILLIC CAPITAL LETTER PSI
    (Unicode:#$0471; Attr:laLower; CaseCode:#$0470),   // CYRILLIC SMALL LETTER PSI
    (Unicode:#$0472; Attr:laUpper; CaseCode:#$0473),   // CYRILLIC CAPITAL LETTER FITA
    (Unicode:#$0473; Attr:laLower; CaseCode:#$0472),   // CYRILLIC SMALL LETTER FITA
    (Unicode:#$0474; Attr:laUpper; CaseCode:#$0475),   // CYRILLIC CAPITAL LETTER IZHITSA
    (Unicode:#$0475; Attr:laLower; CaseCode:#$0474),   // CYRILLIC SMALL LETTER IZHITSA
    (Unicode:#$0476; Attr:laUpper; CaseCode:#$0477),   // CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$0477; Attr:laLower; CaseCode:#$0476),   // CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$0478; Attr:laUpper; CaseCode:#$0479),   // CYRILLIC CAPITAL LETTER UK
    (Unicode:#$0479; Attr:laLower; CaseCode:#$0478),   // CYRILLIC SMALL LETTER UK
    (Unicode:#$047A; Attr:laUpper; CaseCode:#$047B),   // CYRILLIC CAPITAL LETTER ROUND OMEGA
    (Unicode:#$047B; Attr:laLower; CaseCode:#$047A),   // CYRILLIC SMALL LETTER ROUND OMEGA
    (Unicode:#$047C; Attr:laUpper; CaseCode:#$047D),   // CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
    (Unicode:#$047D; Attr:laLower; CaseCode:#$047C),   // CYRILLIC SMALL LETTER OMEGA WITH TITLO
    (Unicode:#$047E; Attr:laUpper; CaseCode:#$047F),   // CYRILLIC CAPITAL LETTER OT
    (Unicode:#$047F; Attr:laLower; CaseCode:#$047E),   // CYRILLIC SMALL LETTER OT
    (Unicode:#$0480; Attr:laUpper; CaseCode:#$0481),   // CYRILLIC CAPITAL LETTER KOPPA
    (Unicode:#$0481; Attr:laLower; CaseCode:#$0480),   // CYRILLIC SMALL LETTER KOPPA
    (Unicode:#$048C; Attr:laUpper; CaseCode:#$048D),   // CYRILLIC CAPITAL LETTER SEMISOFT SIGN
    (Unicode:#$048D; Attr:laLower; CaseCode:#$048C),   // CYRILLIC SMALL LETTER SEMISOFT SIGN
    (Unicode:#$048E; Attr:laUpper; CaseCode:#$048F),   // CYRILLIC CAPITAL LETTER ER WITH TICK
    (Unicode:#$048F; Attr:laLower; CaseCode:#$048E),   // CYRILLIC SMALL LETTER ER WITH TICK
    (Unicode:#$0490; Attr:laUpper; CaseCode:#$0491),   // CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (Unicode:#$0491; Attr:laLower; CaseCode:#$0490),   // CYRILLIC SMALL LETTER GHE WITH UPTURN
    (Unicode:#$0492; Attr:laUpper; CaseCode:#$0493),   // CYRILLIC CAPITAL LETTER GHE WITH STROKE
    (Unicode:#$0493; Attr:laLower; CaseCode:#$0492),   // CYRILLIC SMALL LETTER GHE WITH STROKE
    (Unicode:#$0494; Attr:laUpper; CaseCode:#$0495),   // CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
    (Unicode:#$0495; Attr:laLower; CaseCode:#$0494),   // CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK
    (Unicode:#$0496; Attr:laUpper; CaseCode:#$0497),   // CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
    (Unicode:#$0497; Attr:laLower; CaseCode:#$0496),   // CYRILLIC SMALL LETTER ZHE WITH DESCENDER
    (Unicode:#$0498; Attr:laUpper; CaseCode:#$0499),   // CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
    (Unicode:#$0499; Attr:laLower; CaseCode:#$0498),   // CYRILLIC SMALL LETTER ZE WITH DESCENDER
    (Unicode:#$049A; Attr:laUpper; CaseCode:#$049B),   // CYRILLIC CAPITAL LETTER KA WITH DESCENDER
    (Unicode:#$049B; Attr:laLower; CaseCode:#$049A),   // CYRILLIC SMALL LETTER KA WITH DESCENDER
    (Unicode:#$049C; Attr:laUpper; CaseCode:#$049D),   // CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
    (Unicode:#$049D; Attr:laLower; CaseCode:#$049C),   // CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
    (Unicode:#$049E; Attr:laUpper; CaseCode:#$049F),   // CYRILLIC CAPITAL LETTER KA WITH STROKE
    (Unicode:#$049F; Attr:laLower; CaseCode:#$049E),   // CYRILLIC SMALL LETTER KA WITH STROKE
    (Unicode:#$04A0; Attr:laUpper; CaseCode:#$04A1),   // CYRILLIC CAPITAL LETTER BASHKIR KA
    (Unicode:#$04A1; Attr:laLower; CaseCode:#$04A0),   // CYRILLIC SMALL LETTER BASHKIR KA
    (Unicode:#$04A2; Attr:laUpper; CaseCode:#$04A3),   // CYRILLIC CAPITAL LETTER EN WITH DESCENDER
    (Unicode:#$04A3; Attr:laLower; CaseCode:#$04A2),   // CYRILLIC SMALL LETTER EN WITH DESCENDER
    (Unicode:#$04A4; Attr:laUpper; CaseCode:#$04A5),   // CYRILLIC CAPITAL LIGATURE EN GHE
    (Unicode:#$04A5; Attr:laLower; CaseCode:#$04A4),   // CYRILLIC SMALL LIGATURE EN GHE
    (Unicode:#$04A6; Attr:laUpper; CaseCode:#$04A7),   // CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
    (Unicode:#$04A7; Attr:laLower; CaseCode:#$04A6),   // CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK
    (Unicode:#$04A8; Attr:laUpper; CaseCode:#$04A9),   // CYRILLIC CAPITAL LETTER ABKHASIAN HA
    (Unicode:#$04A9; Attr:laLower; CaseCode:#$04A8),   // CYRILLIC SMALL LETTER ABKHASIAN HA
    (Unicode:#$04AA; Attr:laUpper; CaseCode:#$04AB),   // CYRILLIC CAPITAL LETTER ES WITH DESCENDER
    (Unicode:#$04AB; Attr:laLower; CaseCode:#$04AA),   // CYRILLIC SMALL LETTER ES WITH DESCENDER
    (Unicode:#$04AC; Attr:laUpper; CaseCode:#$04AD),   // CYRILLIC CAPITAL LETTER TE WITH DESCENDER
    (Unicode:#$04AD; Attr:laLower; CaseCode:#$04AC),   // CYRILLIC SMALL LETTER TE WITH DESCENDER
    (Unicode:#$04AE; Attr:laUpper; CaseCode:#$04AF),   // CYRILLIC CAPITAL LETTER STRAIGHT U
    (Unicode:#$04AF; Attr:laLower; CaseCode:#$04AE),   // CYRILLIC SMALL LETTER STRAIGHT U
    (Unicode:#$04B0; Attr:laUpper; CaseCode:#$04B1),   // CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
    (Unicode:#$04B1; Attr:laLower; CaseCode:#$04B0),   // CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
    (Unicode:#$04B2; Attr:laUpper; CaseCode:#$04B3),   // CYRILLIC CAPITAL LETTER HA WITH DESCENDER
    (Unicode:#$04B3; Attr:laLower; CaseCode:#$04B2),   // CYRILLIC SMALL LETTER HA WITH DESCENDER
    (Unicode:#$04B4; Attr:laUpper; CaseCode:#$04B5),   // CYRILLIC CAPITAL LIGATURE TE TSE
    (Unicode:#$04B5; Attr:laLower; CaseCode:#$04B4),   // CYRILLIC SMALL LIGATURE TE TSE
    (Unicode:#$04B6; Attr:laUpper; CaseCode:#$04B7),   // CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
    (Unicode:#$04B7; Attr:laLower; CaseCode:#$04B6),   // CYRILLIC SMALL LETTER CHE WITH DESCENDER
    (Unicode:#$04B8; Attr:laUpper; CaseCode:#$04B9),   // CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
    (Unicode:#$04B9; Attr:laLower; CaseCode:#$04B8),   // CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
    (Unicode:#$04BA; Attr:laUpper; CaseCode:#$04BB),   // CYRILLIC CAPITAL LETTER SHHA
    (Unicode:#$04BB; Attr:laLower; CaseCode:#$04BA),   // CYRILLIC SMALL LETTER SHHA
    (Unicode:#$04BC; Attr:laUpper; CaseCode:#$04BD),   // CYRILLIC CAPITAL LETTER ABKHASIAN CHE
    (Unicode:#$04BD; Attr:laLower; CaseCode:#$04BC),   // CYRILLIC SMALL LETTER ABKHASIAN CHE
    (Unicode:#$04BE; Attr:laUpper; CaseCode:#$04BF),   // CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
    (Unicode:#$04BF; Attr:laLower; CaseCode:#$04BE),   // CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER
    (Unicode:#$04C0; Attr:laUpper; CaseCode:#$FFFF),   // CYRILLIC LETTER PALOCHKA
    (Unicode:#$04C1; Attr:laUpper; CaseCode:#$04C2),   // CYRILLIC CAPITAL LETTER ZHE WITH BREVE
    (Unicode:#$04C2; Attr:laLower; CaseCode:#$04C1),   // CYRILLIC SMALL LETTER ZHE WITH BREVE
    (Unicode:#$04C3; Attr:laUpper; CaseCode:#$04C4),   // CYRILLIC CAPITAL LETTER KA WITH HOOK
    (Unicode:#$04C4; Attr:laLower; CaseCode:#$04C3),   // CYRILLIC SMALL LETTER KA WITH HOOK
    (Unicode:#$04C7; Attr:laUpper; CaseCode:#$04C8),   // CYRILLIC CAPITAL LETTER EN WITH HOOK
    (Unicode:#$04C8; Attr:laLower; CaseCode:#$04C7),   // CYRILLIC SMALL LETTER EN WITH HOOK
    (Unicode:#$04CB; Attr:laUpper; CaseCode:#$04CC),   // CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
    (Unicode:#$04CC; Attr:laLower; CaseCode:#$04CB),   // CYRILLIC SMALL LETTER KHAKASSIAN CHE
    (Unicode:#$04D0; Attr:laUpper; CaseCode:#$04D1),   // CYRILLIC CAPITAL LETTER A WITH BREVE
    (Unicode:#$04D1; Attr:laLower; CaseCode:#$04D0),   // CYRILLIC SMALL LETTER A WITH BREVE
    (Unicode:#$04D2; Attr:laUpper; CaseCode:#$04D3),   // CYRILLIC CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$04D3; Attr:laLower; CaseCode:#$04D2),   // CYRILLIC SMALL LETTER A WITH DIAERESIS
    (Unicode:#$04D4; Attr:laUpper; CaseCode:#$04D5),   // CYRILLIC CAPITAL LIGATURE A IE
    (Unicode:#$04D5; Attr:laLower; CaseCode:#$04D4),   // CYRILLIC SMALL LIGATURE A IE
    (Unicode:#$04D6; Attr:laUpper; CaseCode:#$04D7),   // CYRILLIC CAPITAL LETTER IE WITH BREVE
    (Unicode:#$04D7; Attr:laLower; CaseCode:#$04D6),   // CYRILLIC SMALL LETTER IE WITH BREVE
    (Unicode:#$04D8; Attr:laUpper; CaseCode:#$04D9),   // CYRILLIC CAPITAL LETTER SCHWA
    (Unicode:#$04D9; Attr:laLower; CaseCode:#$04D8),   // CYRILLIC SMALL LETTER SCHWA
    (Unicode:#$04DA; Attr:laUpper; CaseCode:#$04DB),   // CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DB; Attr:laLower; CaseCode:#$04DA),   // CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DC; Attr:laUpper; CaseCode:#$04DD),   // CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DD; Attr:laLower; CaseCode:#$04DC),   // CYRILLIC SMALL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DE; Attr:laUpper; CaseCode:#$04DF),   // CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
    (Unicode:#$04DF; Attr:laLower; CaseCode:#$04DE),   // CYRILLIC SMALL LETTER ZE WITH DIAERESIS
    (Unicode:#$04E0; Attr:laUpper; CaseCode:#$04E1),   // CYRILLIC CAPITAL LETTER ABKHASIAN DZE
    (Unicode:#$04E1; Attr:laLower; CaseCode:#$04E0),   // CYRILLIC SMALL LETTER ABKHASIAN DZE
    (Unicode:#$04E2; Attr:laUpper; CaseCode:#$04E3),   // CYRILLIC CAPITAL LETTER I WITH MACRON
    (Unicode:#$04E3; Attr:laLower; CaseCode:#$04E2),   // CYRILLIC SMALL LETTER I WITH MACRON
    (Unicode:#$04E4; Attr:laUpper; CaseCode:#$04E5),   // CYRILLIC CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$04E5; Attr:laLower; CaseCode:#$04E4),   // CYRILLIC SMALL LETTER I WITH DIAERESIS
    (Unicode:#$04E6; Attr:laUpper; CaseCode:#$04E7),   // CYRILLIC CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$04E7; Attr:laLower; CaseCode:#$04E6),   // CYRILLIC SMALL LETTER O WITH DIAERESIS
    (Unicode:#$04E8; Attr:laUpper; CaseCode:#$04E9),   // CYRILLIC CAPITAL LETTER BARRED O
    (Unicode:#$04E9; Attr:laLower; CaseCode:#$04E8),   // CYRILLIC SMALL LETTER BARRED O
    (Unicode:#$04EA; Attr:laUpper; CaseCode:#$04EB),   // CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EB; Attr:laLower; CaseCode:#$04EA),   // CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EC; Attr:laUpper; CaseCode:#$04ED),   // CYRILLIC CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$04ED; Attr:laLower; CaseCode:#$04EC),   // CYRILLIC SMALL LETTER E WITH DIAERESIS
    (Unicode:#$04EE; Attr:laUpper; CaseCode:#$04EF),   // CYRILLIC CAPITAL LETTER U WITH MACRON
    (Unicode:#$04EF; Attr:laLower; CaseCode:#$04EE),   // CYRILLIC SMALL LETTER U WITH MACRON
    (Unicode:#$04F0; Attr:laUpper; CaseCode:#$04F1),   // CYRILLIC CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$04F1; Attr:laLower; CaseCode:#$04F0),   // CYRILLIC SMALL LETTER U WITH DIAERESIS
    (Unicode:#$04F2; Attr:laUpper; CaseCode:#$04F3),   // CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F3; Attr:laLower; CaseCode:#$04F2),   // CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F4; Attr:laUpper; CaseCode:#$04F5),   // CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F5; Attr:laLower; CaseCode:#$04F4),   // CYRILLIC SMALL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F8; Attr:laUpper; CaseCode:#$04F9),   // CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
    (Unicode:#$04F9; Attr:laLower; CaseCode:#$04F8),   // CYRILLIC SMALL LETTER YERU WITH DIAERESIS
    (Unicode:#$0531; Attr:laUpper; CaseCode:#$0561),   // ARMENIAN CAPITAL LETTER AYB
    (Unicode:#$0532; Attr:laUpper; CaseCode:#$0562),   // ARMENIAN CAPITAL LETTER BEN
    (Unicode:#$0533; Attr:laUpper; CaseCode:#$0563),   // ARMENIAN CAPITAL LETTER GIM
    (Unicode:#$0534; Attr:laUpper; CaseCode:#$0564),   // ARMENIAN CAPITAL LETTER DA
    (Unicode:#$0535; Attr:laUpper; CaseCode:#$0565),   // ARMENIAN CAPITAL LETTER ECH
    (Unicode:#$0536; Attr:laUpper; CaseCode:#$0566),   // ARMENIAN CAPITAL LETTER ZA
    (Unicode:#$0537; Attr:laUpper; CaseCode:#$0567),   // ARMENIAN CAPITAL LETTER EH
    (Unicode:#$0538; Attr:laUpper; CaseCode:#$0568),   // ARMENIAN CAPITAL LETTER ET
    (Unicode:#$0539; Attr:laUpper; CaseCode:#$0569),   // ARMENIAN CAPITAL LETTER TO
    (Unicode:#$053A; Attr:laUpper; CaseCode:#$056A),   // ARMENIAN CAPITAL LETTER ZHE
    (Unicode:#$053B; Attr:laUpper; CaseCode:#$056B),   // ARMENIAN CAPITAL LETTER INI
    (Unicode:#$053C; Attr:laUpper; CaseCode:#$056C),   // ARMENIAN CAPITAL LETTER LIWN
    (Unicode:#$053D; Attr:laUpper; CaseCode:#$056D),   // ARMENIAN CAPITAL LETTER XEH
    (Unicode:#$053E; Attr:laUpper; CaseCode:#$056E),   // ARMENIAN CAPITAL LETTER CA
    (Unicode:#$053F; Attr:laUpper; CaseCode:#$056F),   // ARMENIAN CAPITAL LETTER KEN
    (Unicode:#$0540; Attr:laUpper; CaseCode:#$0570),   // ARMENIAN CAPITAL LETTER HO
    (Unicode:#$0541; Attr:laUpper; CaseCode:#$0571),   // ARMENIAN CAPITAL LETTER JA
    (Unicode:#$0542; Attr:laUpper; CaseCode:#$0572),   // ARMENIAN CAPITAL LETTER GHAD
    (Unicode:#$0543; Attr:laUpper; CaseCode:#$0573),   // ARMENIAN CAPITAL LETTER CHEH
    (Unicode:#$0544; Attr:laUpper; CaseCode:#$0574),   // ARMENIAN CAPITAL LETTER MEN
    (Unicode:#$0545; Attr:laUpper; CaseCode:#$0575),   // ARMENIAN CAPITAL LETTER YI
    (Unicode:#$0546; Attr:laUpper; CaseCode:#$0576),   // ARMENIAN CAPITAL LETTER NOW
    (Unicode:#$0547; Attr:laUpper; CaseCode:#$0577),   // ARMENIAN CAPITAL LETTER SHA
    (Unicode:#$0548; Attr:laUpper; CaseCode:#$0578),   // ARMENIAN CAPITAL LETTER VO
    (Unicode:#$0549; Attr:laUpper; CaseCode:#$0579),   // ARMENIAN CAPITAL LETTER CHA
    (Unicode:#$054A; Attr:laUpper; CaseCode:#$057A),   // ARMENIAN CAPITAL LETTER PEH
    (Unicode:#$054B; Attr:laUpper; CaseCode:#$057B),   // ARMENIAN CAPITAL LETTER JHEH
    (Unicode:#$054C; Attr:laUpper; CaseCode:#$057C),   // ARMENIAN CAPITAL LETTER RA
    (Unicode:#$054D; Attr:laUpper; CaseCode:#$057D),   // ARMENIAN CAPITAL LETTER SEH
    (Unicode:#$054E; Attr:laUpper; CaseCode:#$057E),   // ARMENIAN CAPITAL LETTER VEW
    (Unicode:#$054F; Attr:laUpper; CaseCode:#$057F),   // ARMENIAN CAPITAL LETTER TIWN
    (Unicode:#$0550; Attr:laUpper; CaseCode:#$0580),   // ARMENIAN CAPITAL LETTER REH
    (Unicode:#$0551; Attr:laUpper; CaseCode:#$0581),   // ARMENIAN CAPITAL LETTER CO
    (Unicode:#$0552; Attr:laUpper; CaseCode:#$0582),   // ARMENIAN CAPITAL LETTER YIWN
    (Unicode:#$0553; Attr:laUpper; CaseCode:#$0583),   // ARMENIAN CAPITAL LETTER PIWR
    (Unicode:#$0554; Attr:laUpper; CaseCode:#$0584),   // ARMENIAN CAPITAL LETTER KEH
    (Unicode:#$0555; Attr:laUpper; CaseCode:#$0585),   // ARMENIAN CAPITAL LETTER OH
    (Unicode:#$0556; Attr:laUpper; CaseCode:#$0586),   // ARMENIAN CAPITAL LETTER FEH
    (Unicode:#$0561; Attr:laLower; CaseCode:#$0531),   // ARMENIAN SMALL LETTER AYB
    (Unicode:#$0562; Attr:laLower; CaseCode:#$0532),   // ARMENIAN SMALL LETTER BEN
    (Unicode:#$0563; Attr:laLower; CaseCode:#$0533),   // ARMENIAN SMALL LETTER GIM
    (Unicode:#$0564; Attr:laLower; CaseCode:#$0534),   // ARMENIAN SMALL LETTER DA
    (Unicode:#$0565; Attr:laLower; CaseCode:#$0535),   // ARMENIAN SMALL LETTER ECH
    (Unicode:#$0566; Attr:laLower; CaseCode:#$0536),   // ARMENIAN SMALL LETTER ZA
    (Unicode:#$0567; Attr:laLower; CaseCode:#$0537),   // ARMENIAN SMALL LETTER EH
    (Unicode:#$0568; Attr:laLower; CaseCode:#$0538),   // ARMENIAN SMALL LETTER ET
    (Unicode:#$0569; Attr:laLower; CaseCode:#$0539),   // ARMENIAN SMALL LETTER TO
    (Unicode:#$056A; Attr:laLower; CaseCode:#$053A),   // ARMENIAN SMALL LETTER ZHE
    (Unicode:#$056B; Attr:laLower; CaseCode:#$053B),   // ARMENIAN SMALL LETTER INI
    (Unicode:#$056C; Attr:laLower; CaseCode:#$053C),   // ARMENIAN SMALL LETTER LIWN
    (Unicode:#$056D; Attr:laLower; CaseCode:#$053D),   // ARMENIAN SMALL LETTER XEH
    (Unicode:#$056E; Attr:laLower; CaseCode:#$053E),   // ARMENIAN SMALL LETTER CA
    (Unicode:#$056F; Attr:laLower; CaseCode:#$053F),   // ARMENIAN SMALL LETTER KEN
    (Unicode:#$0570; Attr:laLower; CaseCode:#$0540),   // ARMENIAN SMALL LETTER HO
    (Unicode:#$0571; Attr:laLower; CaseCode:#$0541),   // ARMENIAN SMALL LETTER JA
    (Unicode:#$0572; Attr:laLower; CaseCode:#$0542),   // ARMENIAN SMALL LETTER GHAD
    (Unicode:#$0573; Attr:laLower; CaseCode:#$0543),   // ARMENIAN SMALL LETTER CHEH
    (Unicode:#$0574; Attr:laLower; CaseCode:#$0544),   // ARMENIAN SMALL LETTER MEN
    (Unicode:#$0575; Attr:laLower; CaseCode:#$0545),   // ARMENIAN SMALL LETTER YI
    (Unicode:#$0576; Attr:laLower; CaseCode:#$0546),   // ARMENIAN SMALL LETTER NOW
    (Unicode:#$0577; Attr:laLower; CaseCode:#$0547),   // ARMENIAN SMALL LETTER SHA
    (Unicode:#$0578; Attr:laLower; CaseCode:#$0548),   // ARMENIAN SMALL LETTER VO
    (Unicode:#$0579; Attr:laLower; CaseCode:#$0549),   // ARMENIAN SMALL LETTER CHA
    (Unicode:#$057A; Attr:laLower; CaseCode:#$054A),   // ARMENIAN SMALL LETTER PEH
    (Unicode:#$057B; Attr:laLower; CaseCode:#$054B),   // ARMENIAN SMALL LETTER JHEH
    (Unicode:#$057C; Attr:laLower; CaseCode:#$054C),   // ARMENIAN SMALL LETTER RA
    (Unicode:#$057D; Attr:laLower; CaseCode:#$054D),   // ARMENIAN SMALL LETTER SEH
    (Unicode:#$057E; Attr:laLower; CaseCode:#$054E),   // ARMENIAN SMALL LETTER VEW
    (Unicode:#$057F; Attr:laLower; CaseCode:#$054F),   // ARMENIAN SMALL LETTER TIWN
    (Unicode:#$0580; Attr:laLower; CaseCode:#$0550),   // ARMENIAN SMALL LETTER REH
    (Unicode:#$0581; Attr:laLower; CaseCode:#$0551),   // ARMENIAN SMALL LETTER CO
    (Unicode:#$0582; Attr:laLower; CaseCode:#$0552),   // ARMENIAN SMALL LETTER YIWN
    (Unicode:#$0583; Attr:laLower; CaseCode:#$0553),   // ARMENIAN SMALL LETTER PIWR
    (Unicode:#$0584; Attr:laLower; CaseCode:#$0554),   // ARMENIAN SMALL LETTER KEH
    (Unicode:#$0585; Attr:laLower; CaseCode:#$0555),   // ARMENIAN SMALL LETTER OH
    (Unicode:#$0586; Attr:laLower; CaseCode:#$0556),   // ARMENIAN SMALL LETTER FEH
    (Unicode:#$0587; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE ECH YIWN
    (Unicode:#$10A0; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER AN
    (Unicode:#$10A1; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER BAN
    (Unicode:#$10A2; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER GAN
    (Unicode:#$10A3; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER DON
    (Unicode:#$10A4; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER EN
    (Unicode:#$10A5; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER VIN
    (Unicode:#$10A6; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER ZEN
    (Unicode:#$10A7; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER TAN
    (Unicode:#$10A8; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER IN
    (Unicode:#$10A9; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER KAN
    (Unicode:#$10AA; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER LAS
    (Unicode:#$10AB; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER MAN
    (Unicode:#$10AC; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER NAR
    (Unicode:#$10AD; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER ON
    (Unicode:#$10AE; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER PAR
    (Unicode:#$10AF; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER ZHAR
    (Unicode:#$10B0; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER RAE
    (Unicode:#$10B1; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER SAN
    (Unicode:#$10B2; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER TAR
    (Unicode:#$10B3; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER UN
    (Unicode:#$10B4; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER PHAR
    (Unicode:#$10B5; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER KHAR
    (Unicode:#$10B6; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER GHAN
    (Unicode:#$10B7; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER QAR
    (Unicode:#$10B8; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER SHIN
    (Unicode:#$10B9; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CHIN
    (Unicode:#$10BA; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CAN
    (Unicode:#$10BB; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER JIL
    (Unicode:#$10BC; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CIL
    (Unicode:#$10BD; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CHAR
    (Unicode:#$10BE; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER XAN
    (Unicode:#$10BF; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER JHAN
    (Unicode:#$10C0; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HAE
    (Unicode:#$10C1; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HE
    (Unicode:#$10C2; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HIE
    (Unicode:#$10C3; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER WE
    (Unicode:#$10C4; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HAR
    (Unicode:#$10C5; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HOE
    (Unicode:#$1E00; Attr:laUpper; CaseCode:#$1E01),   // LATIN CAPITAL LETTER A WITH RING BELOW
    (Unicode:#$1E01; Attr:laLower; CaseCode:#$1E00),   // LATIN SMALL LETTER A WITH RING BELOW
    (Unicode:#$1E02; Attr:laUpper; CaseCode:#$1E03),   // LATIN CAPITAL LETTER B WITH DOT ABOVE
    (Unicode:#$1E03; Attr:laLower; CaseCode:#$1E02),   // LATIN SMALL LETTER B WITH DOT ABOVE
    (Unicode:#$1E04; Attr:laUpper; CaseCode:#$1E05),   // LATIN CAPITAL LETTER B WITH DOT BELOW
    (Unicode:#$1E05; Attr:laLower; CaseCode:#$1E04),   // LATIN SMALL LETTER B WITH DOT BELOW
    (Unicode:#$1E06; Attr:laUpper; CaseCode:#$1E07),   // LATIN CAPITAL LETTER B WITH LINE BELOW
    (Unicode:#$1E07; Attr:laLower; CaseCode:#$1E06),   // LATIN SMALL LETTER B WITH LINE BELOW
    (Unicode:#$1E08; Attr:laUpper; CaseCode:#$1E09),   // LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E09; Attr:laLower; CaseCode:#$1E08),   // LATIN SMALL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E0A; Attr:laUpper; CaseCode:#$1E0B),   // LATIN CAPITAL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0B; Attr:laLower; CaseCode:#$1E0A),   // LATIN SMALL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0C; Attr:laUpper; CaseCode:#$1E0D),   // LATIN CAPITAL LETTER D WITH DOT BELOW
    (Unicode:#$1E0D; Attr:laLower; CaseCode:#$1E0C),   // LATIN SMALL LETTER D WITH DOT BELOW
    (Unicode:#$1E0E; Attr:laUpper; CaseCode:#$1E0F),   // LATIN CAPITAL LETTER D WITH LINE BELOW
    (Unicode:#$1E0F; Attr:laLower; CaseCode:#$1E0E),   // LATIN SMALL LETTER D WITH LINE BELOW
    (Unicode:#$1E10; Attr:laUpper; CaseCode:#$1E11),   // LATIN CAPITAL LETTER D WITH CEDILLA
    (Unicode:#$1E11; Attr:laLower; CaseCode:#$1E10),   // LATIN SMALL LETTER D WITH CEDILLA
    (Unicode:#$1E12; Attr:laUpper; CaseCode:#$1E13),   // LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E13; Attr:laLower; CaseCode:#$1E12),   // LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E14; Attr:laUpper; CaseCode:#$1E15),   // LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E15; Attr:laLower; CaseCode:#$1E14),   // LATIN SMALL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E16; Attr:laUpper; CaseCode:#$1E17),   // LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E17; Attr:laLower; CaseCode:#$1E16),   // LATIN SMALL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E18; Attr:laUpper; CaseCode:#$1E19),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E19; Attr:laLower; CaseCode:#$1E18),   // LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E1A; Attr:laUpper; CaseCode:#$1E1B),   // LATIN CAPITAL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1B; Attr:laLower; CaseCode:#$1E1A),   // LATIN SMALL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1C; Attr:laUpper; CaseCode:#$1E1D),   // LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1D; Attr:laLower; CaseCode:#$1E1C),   // LATIN SMALL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1E; Attr:laUpper; CaseCode:#$1E1F),   // LATIN CAPITAL LETTER F WITH DOT ABOVE
    (Unicode:#$1E1F; Attr:laLower; CaseCode:#$1E1E),   // LATIN SMALL LETTER F WITH DOT ABOVE
    (Unicode:#$1E20; Attr:laUpper; CaseCode:#$1E21),   // LATIN CAPITAL LETTER G WITH MACRON
    (Unicode:#$1E21; Attr:laLower; CaseCode:#$1E20),   // LATIN SMALL LETTER G WITH MACRON
    (Unicode:#$1E22; Attr:laUpper; CaseCode:#$1E23),   // LATIN CAPITAL LETTER H WITH DOT ABOVE
    (Unicode:#$1E23; Attr:laLower; CaseCode:#$1E22),   // LATIN SMALL LETTER H WITH DOT ABOVE
    (Unicode:#$1E24; Attr:laUpper; CaseCode:#$1E25),   // LATIN CAPITAL LETTER H WITH DOT BELOW
    (Unicode:#$1E25; Attr:laLower; CaseCode:#$1E24),   // LATIN SMALL LETTER H WITH DOT BELOW
    (Unicode:#$1E26; Attr:laUpper; CaseCode:#$1E27),   // LATIN CAPITAL LETTER H WITH DIAERESIS
    (Unicode:#$1E27; Attr:laLower; CaseCode:#$1E26),   // LATIN SMALL LETTER H WITH DIAERESIS
    (Unicode:#$1E28; Attr:laUpper; CaseCode:#$1E29),   // LATIN CAPITAL LETTER H WITH CEDILLA
    (Unicode:#$1E29; Attr:laLower; CaseCode:#$1E28),   // LATIN SMALL LETTER H WITH CEDILLA
    (Unicode:#$1E2A; Attr:laUpper; CaseCode:#$1E2B),   // LATIN CAPITAL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2B; Attr:laLower; CaseCode:#$1E2A),   // LATIN SMALL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2C; Attr:laUpper; CaseCode:#$1E2D),   // LATIN CAPITAL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2D; Attr:laLower; CaseCode:#$1E2C),   // LATIN SMALL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2E; Attr:laUpper; CaseCode:#$1E2F),   // LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E2F; Attr:laLower; CaseCode:#$1E2E),   // LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E30; Attr:laUpper; CaseCode:#$1E31),   // LATIN CAPITAL LETTER K WITH ACUTE
    (Unicode:#$1E31; Attr:laLower; CaseCode:#$1E30),   // LATIN SMALL LETTER K WITH ACUTE
    (Unicode:#$1E32; Attr:laUpper; CaseCode:#$1E33),   // LATIN CAPITAL LETTER K WITH DOT BELOW
    (Unicode:#$1E33; Attr:laLower; CaseCode:#$1E32),   // LATIN SMALL LETTER K WITH DOT BELOW
    (Unicode:#$1E34; Attr:laUpper; CaseCode:#$1E35),   // LATIN CAPITAL LETTER K WITH LINE BELOW
    (Unicode:#$1E35; Attr:laLower; CaseCode:#$1E34),   // LATIN SMALL LETTER K WITH LINE BELOW
    (Unicode:#$1E36; Attr:laUpper; CaseCode:#$1E37),   // LATIN CAPITAL LETTER L WITH DOT BELOW
    (Unicode:#$1E37; Attr:laLower; CaseCode:#$1E36),   // LATIN SMALL LETTER L WITH DOT BELOW
    (Unicode:#$1E38; Attr:laUpper; CaseCode:#$1E39),   // LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E39; Attr:laLower; CaseCode:#$1E38),   // LATIN SMALL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E3A; Attr:laUpper; CaseCode:#$1E3B),   // LATIN CAPITAL LETTER L WITH LINE BELOW
    (Unicode:#$1E3B; Attr:laLower; CaseCode:#$1E3A),   // LATIN SMALL LETTER L WITH LINE BELOW
    (Unicode:#$1E3C; Attr:laUpper; CaseCode:#$1E3D),   // LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3D; Attr:laLower; CaseCode:#$1E3C),   // LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3E; Attr:laUpper; CaseCode:#$1E3F),   // LATIN CAPITAL LETTER M WITH ACUTE
    (Unicode:#$1E3F; Attr:laLower; CaseCode:#$1E3E),   // LATIN SMALL LETTER M WITH ACUTE
    (Unicode:#$1E40; Attr:laUpper; CaseCode:#$1E41),   // LATIN CAPITAL LETTER M WITH DOT ABOVE
    (Unicode:#$1E41; Attr:laLower; CaseCode:#$1E40),   // LATIN SMALL LETTER M WITH DOT ABOVE
    (Unicode:#$1E42; Attr:laUpper; CaseCode:#$1E43),   // LATIN CAPITAL LETTER M WITH DOT BELOW
    (Unicode:#$1E43; Attr:laLower; CaseCode:#$1E42),   // LATIN SMALL LETTER M WITH DOT BELOW
    (Unicode:#$1E44; Attr:laUpper; CaseCode:#$1E45),   // LATIN CAPITAL LETTER N WITH DOT ABOVE
    (Unicode:#$1E45; Attr:laLower; CaseCode:#$1E44),   // LATIN SMALL LETTER N WITH DOT ABOVE
    (Unicode:#$1E46; Attr:laUpper; CaseCode:#$1E47),   // LATIN CAPITAL LETTER N WITH DOT BELOW
    (Unicode:#$1E47; Attr:laLower; CaseCode:#$1E46),   // LATIN SMALL LETTER N WITH DOT BELOW
    (Unicode:#$1E48; Attr:laUpper; CaseCode:#$1E49),   // LATIN CAPITAL LETTER N WITH LINE BELOW
    (Unicode:#$1E49; Attr:laLower; CaseCode:#$1E48),   // LATIN SMALL LETTER N WITH LINE BELOW
    (Unicode:#$1E4A; Attr:laUpper; CaseCode:#$1E4B),   // LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4B; Attr:laLower; CaseCode:#$1E4A),   // LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4C; Attr:laUpper; CaseCode:#$1E4D),   // LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4D; Attr:laLower; CaseCode:#$1E4C),   // LATIN SMALL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4E; Attr:laUpper; CaseCode:#$1E4F),   // LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E4F; Attr:laLower; CaseCode:#$1E4E),   // LATIN SMALL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E50; Attr:laUpper; CaseCode:#$1E51),   // LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E51; Attr:laLower; CaseCode:#$1E50),   // LATIN SMALL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E52; Attr:laUpper; CaseCode:#$1E53),   // LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E53; Attr:laLower; CaseCode:#$1E52),   // LATIN SMALL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E54; Attr:laUpper; CaseCode:#$1E55),   // LATIN CAPITAL LETTER P WITH ACUTE
    (Unicode:#$1E55; Attr:laLower; CaseCode:#$1E54),   // LATIN SMALL LETTER P WITH ACUTE
    (Unicode:#$1E56; Attr:laUpper; CaseCode:#$1E57),   // LATIN CAPITAL LETTER P WITH DOT ABOVE
    (Unicode:#$1E57; Attr:laLower; CaseCode:#$1E56),   // LATIN SMALL LETTER P WITH DOT ABOVE
    (Unicode:#$1E58; Attr:laUpper; CaseCode:#$1E59),   // LATIN CAPITAL LETTER R WITH DOT ABOVE
    (Unicode:#$1E59; Attr:laLower; CaseCode:#$1E58),   // LATIN SMALL LETTER R WITH DOT ABOVE
    (Unicode:#$1E5A; Attr:laUpper; CaseCode:#$1E5B),   // LATIN CAPITAL LETTER R WITH DOT BELOW
    (Unicode:#$1E5B; Attr:laLower; CaseCode:#$1E5A),   // LATIN SMALL LETTER R WITH DOT BELOW
    (Unicode:#$1E5C; Attr:laUpper; CaseCode:#$1E5D),   // LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5D; Attr:laLower; CaseCode:#$1E5C),   // LATIN SMALL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5E; Attr:laUpper; CaseCode:#$1E5F),   // LATIN CAPITAL LETTER R WITH LINE BELOW
    (Unicode:#$1E5F; Attr:laLower; CaseCode:#$1E5E),   // LATIN SMALL LETTER R WITH LINE BELOW
    (Unicode:#$1E60; Attr:laUpper; CaseCode:#$1E61),   // LATIN CAPITAL LETTER S WITH DOT ABOVE
    (Unicode:#$1E61; Attr:laLower; CaseCode:#$1E60),   // LATIN SMALL LETTER S WITH DOT ABOVE
    (Unicode:#$1E62; Attr:laUpper; CaseCode:#$1E63),   // LATIN CAPITAL LETTER S WITH DOT BELOW
    (Unicode:#$1E63; Attr:laLower; CaseCode:#$1E62),   // LATIN SMALL LETTER S WITH DOT BELOW
    (Unicode:#$1E64; Attr:laUpper; CaseCode:#$1E65),   // LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E65; Attr:laLower; CaseCode:#$1E64),   // LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E66; Attr:laUpper; CaseCode:#$1E67),   // LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E67; Attr:laLower; CaseCode:#$1E66),   // LATIN SMALL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E68; Attr:laUpper; CaseCode:#$1E69),   // LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E69; Attr:laLower; CaseCode:#$1E68),   // LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E6A; Attr:laUpper; CaseCode:#$1E6B),   // LATIN CAPITAL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6B; Attr:laLower; CaseCode:#$1E6A),   // LATIN SMALL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6C; Attr:laUpper; CaseCode:#$1E6D),   // LATIN CAPITAL LETTER T WITH DOT BELOW
    (Unicode:#$1E6D; Attr:laLower; CaseCode:#$1E6C),   // LATIN SMALL LETTER T WITH DOT BELOW
    (Unicode:#$1E6E; Attr:laUpper; CaseCode:#$1E6F),   // LATIN CAPITAL LETTER T WITH LINE BELOW
    (Unicode:#$1E6F; Attr:laLower; CaseCode:#$1E6E),   // LATIN SMALL LETTER T WITH LINE BELOW
    (Unicode:#$1E70; Attr:laUpper; CaseCode:#$1E71),   // LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E71; Attr:laLower; CaseCode:#$1E70),   // LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E72; Attr:laUpper; CaseCode:#$1E73),   // LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E73; Attr:laLower; CaseCode:#$1E72),   // LATIN SMALL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E74; Attr:laUpper; CaseCode:#$1E75),   // LATIN CAPITAL LETTER U WITH TILDE BELOW
    (Unicode:#$1E75; Attr:laLower; CaseCode:#$1E74),   // LATIN SMALL LETTER U WITH TILDE BELOW
    (Unicode:#$1E76; Attr:laUpper; CaseCode:#$1E77),   // LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E77; Attr:laLower; CaseCode:#$1E76),   // LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E78; Attr:laUpper; CaseCode:#$1E79),   // LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E79; Attr:laLower; CaseCode:#$1E78),   // LATIN SMALL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E7A; Attr:laUpper; CaseCode:#$1E7B),   // LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7B; Attr:laLower; CaseCode:#$1E7A),   // LATIN SMALL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7C; Attr:laUpper; CaseCode:#$1E7D),   // LATIN CAPITAL LETTER V WITH TILDE
    (Unicode:#$1E7D; Attr:laLower; CaseCode:#$1E7C),   // LATIN SMALL LETTER V WITH TILDE
    (Unicode:#$1E7E; Attr:laUpper; CaseCode:#$1E7F),   // LATIN CAPITAL LETTER V WITH DOT BELOW
    (Unicode:#$1E7F; Attr:laLower; CaseCode:#$1E7E),   // LATIN SMALL LETTER V WITH DOT BELOW
    (Unicode:#$1E80; Attr:laUpper; CaseCode:#$1E81),   // LATIN CAPITAL LETTER W WITH GRAVE
    (Unicode:#$1E81; Attr:laLower; CaseCode:#$1E80),   // LATIN SMALL LETTER W WITH GRAVE
    (Unicode:#$1E82; Attr:laUpper; CaseCode:#$1E83),   // LATIN CAPITAL LETTER W WITH ACUTE
    (Unicode:#$1E83; Attr:laLower; CaseCode:#$1E82),   // LATIN SMALL LETTER W WITH ACUTE
    (Unicode:#$1E84; Attr:laUpper; CaseCode:#$1E85),   // LATIN CAPITAL LETTER W WITH DIAERESIS
    (Unicode:#$1E85; Attr:laLower; CaseCode:#$1E84),   // LATIN SMALL LETTER W WITH DIAERESIS
    (Unicode:#$1E86; Attr:laUpper; CaseCode:#$1E87),   // LATIN CAPITAL LETTER W WITH DOT ABOVE
    (Unicode:#$1E87; Attr:laLower; CaseCode:#$1E86),   // LATIN SMALL LETTER W WITH DOT ABOVE
    (Unicode:#$1E88; Attr:laUpper; CaseCode:#$1E89),   // LATIN CAPITAL LETTER W WITH DOT BELOW
    (Unicode:#$1E89; Attr:laLower; CaseCode:#$1E88),   // LATIN SMALL LETTER W WITH DOT BELOW
    (Unicode:#$1E8A; Attr:laUpper; CaseCode:#$1E8B),   // LATIN CAPITAL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8B; Attr:laLower; CaseCode:#$1E8A),   // LATIN SMALL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8C; Attr:laUpper; CaseCode:#$1E8D),   // LATIN CAPITAL LETTER X WITH DIAERESIS
    (Unicode:#$1E8D; Attr:laLower; CaseCode:#$1E8C),   // LATIN SMALL LETTER X WITH DIAERESIS
    (Unicode:#$1E8E; Attr:laUpper; CaseCode:#$1E8F),   // LATIN CAPITAL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E8F; Attr:laLower; CaseCode:#$1E8E),   // LATIN SMALL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E90; Attr:laUpper; CaseCode:#$1E91),   // LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E91; Attr:laLower; CaseCode:#$1E90),   // LATIN SMALL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E92; Attr:laUpper; CaseCode:#$1E93),   // LATIN CAPITAL LETTER Z WITH DOT BELOW
    (Unicode:#$1E93; Attr:laLower; CaseCode:#$1E92),   // LATIN SMALL LETTER Z WITH DOT BELOW
    (Unicode:#$1E94; Attr:laUpper; CaseCode:#$1E95),   // LATIN CAPITAL LETTER Z WITH LINE BELOW
    (Unicode:#$1E95; Attr:laLower; CaseCode:#$1E94),   // LATIN SMALL LETTER Z WITH LINE BELOW
    (Unicode:#$1E96; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER H WITH LINE BELOW
    (Unicode:#$1E97; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER T WITH DIAERESIS
    (Unicode:#$1E98; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER W WITH RING ABOVE
    (Unicode:#$1E99; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Y WITH RING ABOVE
    (Unicode:#$1E9A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER A WITH RIGHT HALF RING
    (Unicode:#$1E9B; Attr:laLower; CaseCode:#$1E60),   // LATIN SMALL LETTER LONG S WITH DOT ABOVE
    (Unicode:#$1EA0; Attr:laUpper; CaseCode:#$1EA1),   // LATIN CAPITAL LETTER A WITH DOT BELOW
    (Unicode:#$1EA1; Attr:laLower; CaseCode:#$1EA0),   // LATIN SMALL LETTER A WITH DOT BELOW
    (Unicode:#$1EA2; Attr:laUpper; CaseCode:#$1EA3),   // LATIN CAPITAL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA3; Attr:laLower; CaseCode:#$1EA2),   // LATIN SMALL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA4; Attr:laUpper; CaseCode:#$1EA5),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA5; Attr:laLower; CaseCode:#$1EA4),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA6; Attr:laUpper; CaseCode:#$1EA7),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA7; Attr:laLower; CaseCode:#$1EA6),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA8; Attr:laUpper; CaseCode:#$1EA9),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EA9; Attr:laLower; CaseCode:#$1EA8),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EAA; Attr:laUpper; CaseCode:#$1EAB),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAB; Attr:laLower; CaseCode:#$1EAA),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAC; Attr:laUpper; CaseCode:#$1EAD),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAD; Attr:laLower; CaseCode:#$1EAC),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAE; Attr:laUpper; CaseCode:#$1EAF),   // LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EAF; Attr:laLower; CaseCode:#$1EAE),   // LATIN SMALL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EB0; Attr:laUpper; CaseCode:#$1EB1),   // LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB1; Attr:laLower; CaseCode:#$1EB0),   // LATIN SMALL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB2; Attr:laUpper; CaseCode:#$1EB3),   // LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB3; Attr:laLower; CaseCode:#$1EB2),   // LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB4; Attr:laUpper; CaseCode:#$1EB5),   // LATIN CAPITAL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB5; Attr:laLower; CaseCode:#$1EB4),   // LATIN SMALL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB6; Attr:laUpper; CaseCode:#$1EB7),   // LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB7; Attr:laLower; CaseCode:#$1EB6),   // LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB8; Attr:laUpper; CaseCode:#$1EB9),   // LATIN CAPITAL LETTER E WITH DOT BELOW
    (Unicode:#$1EB9; Attr:laLower; CaseCode:#$1EB8),   // LATIN SMALL LETTER E WITH DOT BELOW
    (Unicode:#$1EBA; Attr:laUpper; CaseCode:#$1EBB),   // LATIN CAPITAL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBB; Attr:laLower; CaseCode:#$1EBA),   // LATIN SMALL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBC; Attr:laUpper; CaseCode:#$1EBD),   // LATIN CAPITAL LETTER E WITH TILDE
    (Unicode:#$1EBD; Attr:laLower; CaseCode:#$1EBC),   // LATIN SMALL LETTER E WITH TILDE
    (Unicode:#$1EBE; Attr:laUpper; CaseCode:#$1EBF),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EBF; Attr:laLower; CaseCode:#$1EBE),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EC0; Attr:laUpper; CaseCode:#$1EC1),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC1; Attr:laLower; CaseCode:#$1EC0),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC2; Attr:laUpper; CaseCode:#$1EC3),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC3; Attr:laLower; CaseCode:#$1EC2),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC4; Attr:laUpper; CaseCode:#$1EC5),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC5; Attr:laLower; CaseCode:#$1EC4),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC6; Attr:laUpper; CaseCode:#$1EC7),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC7; Attr:laLower; CaseCode:#$1EC6),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC8; Attr:laUpper; CaseCode:#$1EC9),   // LATIN CAPITAL LETTER I WITH HOOK ABOVE
    (Unicode:#$1EC9; Attr:laLower; CaseCode:#$1EC8),   // LATIN SMALL LETTER I WITH HOOK ABOVE
    (Unicode:#$1ECA; Attr:laUpper; CaseCode:#$1ECB),   // LATIN CAPITAL LETTER I WITH DOT BELOW
    (Unicode:#$1ECB; Attr:laLower; CaseCode:#$1ECA),   // LATIN SMALL LETTER I WITH DOT BELOW
    (Unicode:#$1ECC; Attr:laUpper; CaseCode:#$1ECD),   // LATIN CAPITAL LETTER O WITH DOT BELOW
    (Unicode:#$1ECD; Attr:laLower; CaseCode:#$1ECC),   // LATIN SMALL LETTER O WITH DOT BELOW
    (Unicode:#$1ECE; Attr:laUpper; CaseCode:#$1ECF),   // LATIN CAPITAL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ECF; Attr:laLower; CaseCode:#$1ECE),   // LATIN SMALL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ED0; Attr:laUpper; CaseCode:#$1ED1),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED1; Attr:laLower; CaseCode:#$1ED0),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED2; Attr:laUpper; CaseCode:#$1ED3),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED3; Attr:laLower; CaseCode:#$1ED2),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED4; Attr:laUpper; CaseCode:#$1ED5),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED5; Attr:laLower; CaseCode:#$1ED4),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED6; Attr:laUpper; CaseCode:#$1ED7),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED7; Attr:laLower; CaseCode:#$1ED6),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED8; Attr:laUpper; CaseCode:#$1ED9),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1ED9; Attr:laLower; CaseCode:#$1ED8),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EDA; Attr:laUpper; CaseCode:#$1EDB),   // LATIN CAPITAL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDB; Attr:laLower; CaseCode:#$1EDA),   // LATIN SMALL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDC; Attr:laUpper; CaseCode:#$1EDD),   // LATIN CAPITAL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDD; Attr:laLower; CaseCode:#$1EDC),   // LATIN SMALL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDE; Attr:laUpper; CaseCode:#$1EDF),   // LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EDF; Attr:laLower; CaseCode:#$1EDE),   // LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EE0; Attr:laUpper; CaseCode:#$1EE1),   // LATIN CAPITAL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE1; Attr:laLower; CaseCode:#$1EE0),   // LATIN SMALL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE2; Attr:laUpper; CaseCode:#$1EE3),   // LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE3; Attr:laLower; CaseCode:#$1EE2),   // LATIN SMALL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE4; Attr:laUpper; CaseCode:#$1EE5),   // LATIN CAPITAL LETTER U WITH DOT BELOW
    (Unicode:#$1EE5; Attr:laLower; CaseCode:#$1EE4),   // LATIN SMALL LETTER U WITH DOT BELOW
    (Unicode:#$1EE6; Attr:laUpper; CaseCode:#$1EE7),   // LATIN CAPITAL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE7; Attr:laLower; CaseCode:#$1EE6),   // LATIN SMALL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE8; Attr:laUpper; CaseCode:#$1EE9),   // LATIN CAPITAL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EE9; Attr:laLower; CaseCode:#$1EE8),   // LATIN SMALL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EEA; Attr:laUpper; CaseCode:#$1EEB),   // LATIN CAPITAL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEB; Attr:laLower; CaseCode:#$1EEA),   // LATIN SMALL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEC; Attr:laUpper; CaseCode:#$1EED),   // LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EED; Attr:laLower; CaseCode:#$1EEC),   // LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EEE; Attr:laUpper; CaseCode:#$1EEF),   // LATIN CAPITAL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EEF; Attr:laLower; CaseCode:#$1EEE),   // LATIN SMALL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EF0; Attr:laUpper; CaseCode:#$1EF1),   // LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF1; Attr:laLower; CaseCode:#$1EF0),   // LATIN SMALL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF2; Attr:laUpper; CaseCode:#$1EF3),   // LATIN CAPITAL LETTER Y WITH GRAVE
    (Unicode:#$1EF3; Attr:laLower; CaseCode:#$1EF2),   // LATIN SMALL LETTER Y WITH GRAVE
    (Unicode:#$1EF4; Attr:laUpper; CaseCode:#$1EF5),   // LATIN CAPITAL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF5; Attr:laLower; CaseCode:#$1EF4),   // LATIN SMALL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF6; Attr:laUpper; CaseCode:#$1EF7),   // LATIN CAPITAL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF7; Attr:laLower; CaseCode:#$1EF6),   // LATIN SMALL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF8; Attr:laUpper; CaseCode:#$1EF9),   // LATIN CAPITAL LETTER Y WITH TILDE
    (Unicode:#$1EF9; Attr:laLower; CaseCode:#$1EF8),   // LATIN SMALL LETTER Y WITH TILDE
    (Unicode:#$1F00; Attr:laLower; CaseCode:#$1F08),   // GREEK SMALL LETTER ALPHA WITH PSILI
    (Unicode:#$1F01; Attr:laLower; CaseCode:#$1F09),   // GREEK SMALL LETTER ALPHA WITH DASIA
    (Unicode:#$1F02; Attr:laLower; CaseCode:#$1F0A),   // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F03; Attr:laLower; CaseCode:#$1F0B),   // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F04; Attr:laLower; CaseCode:#$1F0C),   // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F05; Attr:laLower; CaseCode:#$1F0D),   // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F06; Attr:laLower; CaseCode:#$1F0E),   // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F07; Attr:laLower; CaseCode:#$1F0F),   // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F08; Attr:laUpper; CaseCode:#$1F00),   // GREEK CAPITAL LETTER ALPHA WITH PSILI
    (Unicode:#$1F09; Attr:laUpper; CaseCode:#$1F01),   // GREEK CAPITAL LETTER ALPHA WITH DASIA
    (Unicode:#$1F0A; Attr:laUpper; CaseCode:#$1F02),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F0B; Attr:laUpper; CaseCode:#$1F03),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F0C; Attr:laUpper; CaseCode:#$1F04),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F0D; Attr:laUpper; CaseCode:#$1F05),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F0E; Attr:laUpper; CaseCode:#$1F06),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F0F; Attr:laUpper; CaseCode:#$1F07),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F10; Attr:laLower; CaseCode:#$1F18),   // GREEK SMALL LETTER EPSILON WITH PSILI
    (Unicode:#$1F11; Attr:laLower; CaseCode:#$1F19),   // GREEK SMALL LETTER EPSILON WITH DASIA
    (Unicode:#$1F12; Attr:laLower; CaseCode:#$1F1A),   // GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F13; Attr:laLower; CaseCode:#$1F1B),   // GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F14; Attr:laLower; CaseCode:#$1F1C),   // GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F15; Attr:laLower; CaseCode:#$1F1D),   // GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F18; Attr:laUpper; CaseCode:#$1F10),   // GREEK CAPITAL LETTER EPSILON WITH PSILI
    (Unicode:#$1F19; Attr:laUpper; CaseCode:#$1F11),   // GREEK CAPITAL LETTER EPSILON WITH DASIA
    (Unicode:#$1F1A; Attr:laUpper; CaseCode:#$1F12),   // GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F1B; Attr:laUpper; CaseCode:#$1F13),   // GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F1C; Attr:laUpper; CaseCode:#$1F14),   // GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F1D; Attr:laUpper; CaseCode:#$1F15),   // GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F20; Attr:laLower; CaseCode:#$1F28),   // GREEK SMALL LETTER ETA WITH PSILI
    (Unicode:#$1F21; Attr:laLower; CaseCode:#$1F29),   // GREEK SMALL LETTER ETA WITH DASIA
    (Unicode:#$1F22; Attr:laLower; CaseCode:#$1F2A),   // GREEK SMALL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F23; Attr:laLower; CaseCode:#$1F2B),   // GREEK SMALL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F24; Attr:laLower; CaseCode:#$1F2C),   // GREEK SMALL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F25; Attr:laLower; CaseCode:#$1F2D),   // GREEK SMALL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F26; Attr:laLower; CaseCode:#$1F2E),   // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F27; Attr:laLower; CaseCode:#$1F2F),   // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F28; Attr:laUpper; CaseCode:#$1F20),   // GREEK CAPITAL LETTER ETA WITH PSILI
    (Unicode:#$1F29; Attr:laUpper; CaseCode:#$1F21),   // GREEK CAPITAL LETTER ETA WITH DASIA
    (Unicode:#$1F2A; Attr:laUpper; CaseCode:#$1F22),   // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F2B; Attr:laUpper; CaseCode:#$1F23),   // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F2C; Attr:laUpper; CaseCode:#$1F24),   // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F2D; Attr:laUpper; CaseCode:#$1F25),   // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F2E; Attr:laUpper; CaseCode:#$1F26),   // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F2F; Attr:laUpper; CaseCode:#$1F27),   // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F30; Attr:laLower; CaseCode:#$1F38),   // GREEK SMALL LETTER IOTA WITH PSILI
    (Unicode:#$1F31; Attr:laLower; CaseCode:#$1F39),   // GREEK SMALL LETTER IOTA WITH DASIA
    (Unicode:#$1F32; Attr:laLower; CaseCode:#$1F3A),   // GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F33; Attr:laLower; CaseCode:#$1F3B),   // GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F34; Attr:laLower; CaseCode:#$1F3C),   // GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F35; Attr:laLower; CaseCode:#$1F3D),   // GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F36; Attr:laLower; CaseCode:#$1F3E),   // GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F37; Attr:laLower; CaseCode:#$1F3F),   // GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F38; Attr:laUpper; CaseCode:#$1F30),   // GREEK CAPITAL LETTER IOTA WITH PSILI
    (Unicode:#$1F39; Attr:laUpper; CaseCode:#$1F31),   // GREEK CAPITAL LETTER IOTA WITH DASIA
    (Unicode:#$1F3A; Attr:laUpper; CaseCode:#$1F32),   // GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F3B; Attr:laUpper; CaseCode:#$1F33),   // GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F3C; Attr:laUpper; CaseCode:#$1F34),   // GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F3D; Attr:laUpper; CaseCode:#$1F35),   // GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F3E; Attr:laUpper; CaseCode:#$1F36),   // GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F3F; Attr:laUpper; CaseCode:#$1F37),   // GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F40; Attr:laLower; CaseCode:#$1F48),   // GREEK SMALL LETTER OMICRON WITH PSILI
    (Unicode:#$1F41; Attr:laLower; CaseCode:#$1F49),   // GREEK SMALL LETTER OMICRON WITH DASIA
    (Unicode:#$1F42; Attr:laLower; CaseCode:#$1F4A),   // GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F43; Attr:laLower; CaseCode:#$1F4B),   // GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F44; Attr:laLower; CaseCode:#$1F4C),   // GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F45; Attr:laLower; CaseCode:#$1F4D),   // GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F48; Attr:laUpper; CaseCode:#$1F40),   // GREEK CAPITAL LETTER OMICRON WITH PSILI
    (Unicode:#$1F49; Attr:laUpper; CaseCode:#$1F41),   // GREEK CAPITAL LETTER OMICRON WITH DASIA
    (Unicode:#$1F4A; Attr:laUpper; CaseCode:#$1F42),   // GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F4B; Attr:laUpper; CaseCode:#$1F43),   // GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F4C; Attr:laUpper; CaseCode:#$1F44),   // GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F4D; Attr:laUpper; CaseCode:#$1F45),   // GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F50; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI
    (Unicode:#$1F51; Attr:laLower; CaseCode:#$1F59),   // GREEK SMALL LETTER UPSILON WITH DASIA
    (Unicode:#$1F52; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
    (Unicode:#$1F53; Attr:laLower; CaseCode:#$1F5B),   // GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F54; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
    (Unicode:#$1F55; Attr:laLower; CaseCode:#$1F5D),   // GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F56; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
    (Unicode:#$1F57; Attr:laLower; CaseCode:#$1F5F),   // GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F59; Attr:laUpper; CaseCode:#$1F51),   // GREEK CAPITAL LETTER UPSILON WITH DASIA
    (Unicode:#$1F5B; Attr:laUpper; CaseCode:#$1F53),   // GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F5D; Attr:laUpper; CaseCode:#$1F55),   // GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F5F; Attr:laUpper; CaseCode:#$1F57),   // GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F60; Attr:laLower; CaseCode:#$1F68),   // GREEK SMALL LETTER OMEGA WITH PSILI
    (Unicode:#$1F61; Attr:laLower; CaseCode:#$1F69),   // GREEK SMALL LETTER OMEGA WITH DASIA
    (Unicode:#$1F62; Attr:laLower; CaseCode:#$1F6A),   // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F63; Attr:laLower; CaseCode:#$1F6B),   // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F64; Attr:laLower; CaseCode:#$1F6C),   // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F65; Attr:laLower; CaseCode:#$1F6D),   // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F66; Attr:laLower; CaseCode:#$1F6E),   // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F67; Attr:laLower; CaseCode:#$1F6F),   // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F68; Attr:laUpper; CaseCode:#$1F60),   // GREEK CAPITAL LETTER OMEGA WITH PSILI
    (Unicode:#$1F69; Attr:laUpper; CaseCode:#$1F61),   // GREEK CAPITAL LETTER OMEGA WITH DASIA
    (Unicode:#$1F6A; Attr:laUpper; CaseCode:#$1F62),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F6B; Attr:laUpper; CaseCode:#$1F63),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F6C; Attr:laUpper; CaseCode:#$1F64),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F6D; Attr:laUpper; CaseCode:#$1F65),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F6E; Attr:laUpper; CaseCode:#$1F66),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F6F; Attr:laUpper; CaseCode:#$1F67),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F70; Attr:laLower; CaseCode:#$1FBA),   // GREEK SMALL LETTER ALPHA WITH VARIA
    (Unicode:#$1F71; Attr:laLower; CaseCode:#$1FBB),   // GREEK SMALL LETTER ALPHA WITH OXIA
    (Unicode:#$1F72; Attr:laLower; CaseCode:#$1FC8),   // GREEK SMALL LETTER EPSILON WITH VARIA
    (Unicode:#$1F73; Attr:laLower; CaseCode:#$1FC9),   // GREEK SMALL LETTER EPSILON WITH OXIA
    (Unicode:#$1F74; Attr:laLower; CaseCode:#$1FCA),   // GREEK SMALL LETTER ETA WITH VARIA
    (Unicode:#$1F75; Attr:laLower; CaseCode:#$1FCB),   // GREEK SMALL LETTER ETA WITH OXIA
    (Unicode:#$1F76; Attr:laLower; CaseCode:#$1FDA),   // GREEK SMALL LETTER IOTA WITH VARIA
    (Unicode:#$1F77; Attr:laLower; CaseCode:#$1FDB),   // GREEK SMALL LETTER IOTA WITH OXIA
    (Unicode:#$1F78; Attr:laLower; CaseCode:#$1FF8),   // GREEK SMALL LETTER OMICRON WITH VARIA
    (Unicode:#$1F79; Attr:laLower; CaseCode:#$1FF9),   // GREEK SMALL LETTER OMICRON WITH OXIA
    (Unicode:#$1F7A; Attr:laLower; CaseCode:#$1FEA),   // GREEK SMALL LETTER UPSILON WITH VARIA
    (Unicode:#$1F7B; Attr:laLower; CaseCode:#$1FEB),   // GREEK SMALL LETTER UPSILON WITH OXIA
    (Unicode:#$1F7C; Attr:laLower; CaseCode:#$1FFA),   // GREEK SMALL LETTER OMEGA WITH VARIA
    (Unicode:#$1F7D; Attr:laLower; CaseCode:#$1FFB),   // GREEK SMALL LETTER OMEGA WITH OXIA
    (Unicode:#$1F80; Attr:laLower; CaseCode:#$1F88),   // GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F81; Attr:laLower; CaseCode:#$1F89),   // GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F82; Attr:laLower; CaseCode:#$1F8A),   // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F83; Attr:laLower; CaseCode:#$1F8B),   // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F84; Attr:laLower; CaseCode:#$1F8C),   // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F85; Attr:laLower; CaseCode:#$1F8D),   // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F86; Attr:laLower; CaseCode:#$1F8E),   // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F87; Attr:laLower; CaseCode:#$1F8F),   // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F90; Attr:laLower; CaseCode:#$1F98),   // GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F91; Attr:laLower; CaseCode:#$1F99),   // GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F92; Attr:laLower; CaseCode:#$1F9A),   // GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F93; Attr:laLower; CaseCode:#$1F9B),   // GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F94; Attr:laLower; CaseCode:#$1F9C),   // GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F95; Attr:laLower; CaseCode:#$1F9D),   // GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F96; Attr:laLower; CaseCode:#$1F9E),   // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F97; Attr:laLower; CaseCode:#$1F9F),   // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA0; Attr:laLower; CaseCode:#$1FA8),   // GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1FA1; Attr:laLower; CaseCode:#$1FA9),   // GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1FA2; Attr:laLower; CaseCode:#$1FAA),   // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA3; Attr:laLower; CaseCode:#$1FAB),   // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA4; Attr:laLower; CaseCode:#$1FAC),   // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA5; Attr:laLower; CaseCode:#$1FAD),   // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA6; Attr:laLower; CaseCode:#$1FAE),   // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA7; Attr:laLower; CaseCode:#$1FAF),   // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FB0; Attr:laLower; CaseCode:#$1FB8),   // GREEK SMALL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB1; Attr:laLower; CaseCode:#$1FB9),   // GREEK SMALL LETTER ALPHA WITH MACRON
    (Unicode:#$1FB2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FB3; Attr:laLower; CaseCode:#$1FBC),   // GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
    (Unicode:#$1FB4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FB6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH PERISPOMENI
    (Unicode:#$1FB7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FB8; Attr:laUpper; CaseCode:#$1FB0),   // GREEK CAPITAL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB9; Attr:laUpper; CaseCode:#$1FB1),   // GREEK CAPITAL LETTER ALPHA WITH MACRON
    (Unicode:#$1FBA; Attr:laUpper; CaseCode:#$1F70),   // GREEK CAPITAL LETTER ALPHA WITH VARIA
    (Unicode:#$1FBB; Attr:laUpper; CaseCode:#$1F71),   // GREEK CAPITAL LETTER ALPHA WITH OXIA
    (Unicode:#$1FBE; Attr:laLower; CaseCode:#$0399),   // GREEK PROSGEGRAMMENI
    (Unicode:#$1FC2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FC3; Attr:laLower; CaseCode:#$1FCC),   // GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
    (Unicode:#$1FC4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FC6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH PERISPOMENI
    (Unicode:#$1FC7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FC8; Attr:laUpper; CaseCode:#$1F72),   // GREEK CAPITAL LETTER EPSILON WITH VARIA
    (Unicode:#$1FC9; Attr:laUpper; CaseCode:#$1F73),   // GREEK CAPITAL LETTER EPSILON WITH OXIA
    (Unicode:#$1FCA; Attr:laUpper; CaseCode:#$1F74),   // GREEK CAPITAL LETTER ETA WITH VARIA
    (Unicode:#$1FCB; Attr:laUpper; CaseCode:#$1F75),   // GREEK CAPITAL LETTER ETA WITH OXIA
    (Unicode:#$1FD0; Attr:laLower; CaseCode:#$1FD8),   // GREEK SMALL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD1; Attr:laLower; CaseCode:#$1FD9),   // GREEK SMALL LETTER IOTA WITH MACRON
    (Unicode:#$1FD2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
    (Unicode:#$1FD3; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
    (Unicode:#$1FD6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH PERISPOMENI
    (Unicode:#$1FD7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FD8; Attr:laUpper; CaseCode:#$1FD0),   // GREEK CAPITAL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD9; Attr:laUpper; CaseCode:#$1FD1),   // GREEK CAPITAL LETTER IOTA WITH MACRON
    (Unicode:#$1FDA; Attr:laUpper; CaseCode:#$1F76),   // GREEK CAPITAL LETTER IOTA WITH VARIA
    (Unicode:#$1FDB; Attr:laUpper; CaseCode:#$1F77),   // GREEK CAPITAL LETTER IOTA WITH OXIA
    (Unicode:#$1FE0; Attr:laLower; CaseCode:#$1FE8),   // GREEK SMALL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE1; Attr:laLower; CaseCode:#$1FE9),   // GREEK SMALL LETTER UPSILON WITH MACRON
    (Unicode:#$1FE2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
    (Unicode:#$1FE3; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
    (Unicode:#$1FE4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER RHO WITH PSILI
    (Unicode:#$1FE5; Attr:laLower; CaseCode:#$1FEC),   // GREEK SMALL LETTER RHO WITH DASIA
    (Unicode:#$1FE6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PERISPOMENI
    (Unicode:#$1FE7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FE8; Attr:laUpper; CaseCode:#$1FE0),   // GREEK CAPITAL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE9; Attr:laUpper; CaseCode:#$1FE1),   // GREEK CAPITAL LETTER UPSILON WITH MACRON
    (Unicode:#$1FEA; Attr:laUpper; CaseCode:#$1F7A),   // GREEK CAPITAL LETTER UPSILON WITH VARIA
    (Unicode:#$1FEB; Attr:laUpper; CaseCode:#$1F7B),   // GREEK CAPITAL LETTER UPSILON WITH OXIA
    (Unicode:#$1FEC; Attr:laUpper; CaseCode:#$1FE5),   // GREEK CAPITAL LETTER RHO WITH DASIA
    (Unicode:#$1FF2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FF3; Attr:laLower; CaseCode:#$1FFC),   // GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
    (Unicode:#$1FF4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FF6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH PERISPOMENI
    (Unicode:#$1FF7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FF8; Attr:laUpper; CaseCode:#$1F78),   // GREEK CAPITAL LETTER OMICRON WITH VARIA
    (Unicode:#$1FF9; Attr:laUpper; CaseCode:#$1F79),   // GREEK CAPITAL LETTER OMICRON WITH OXIA
    (Unicode:#$1FFA; Attr:laUpper; CaseCode:#$1F7C),   // GREEK CAPITAL LETTER OMEGA WITH VARIA
    (Unicode:#$1FFB; Attr:laUpper; CaseCode:#$1F7D),   // GREEK CAPITAL LETTER OMEGA WITH OXIA
    (Unicode:#$207F; Attr:laLower; CaseCode:#$FFFF),   // SUPERSCRIPT LATIN SMALL LETTER N
    (Unicode:#$2102; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL C
    (Unicode:#$2107; Attr:laUpper; CaseCode:#$FFFF),   // EULER CONSTANT
    (Unicode:#$210A; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL G
    (Unicode:#$210B; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL H
    (Unicode:#$210C; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL H
    (Unicode:#$210D; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL H
    (Unicode:#$210E; Attr:laLower; CaseCode:#$FFFF),   // PLANCK CONSTANT
    (Unicode:#$210F; Attr:laLower; CaseCode:#$FFFF),   // PLANCK CONSTANT OVER TWO PI
    (Unicode:#$2110; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL I
    (Unicode:#$2111; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL I
    (Unicode:#$2112; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL L
    (Unicode:#$2113; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL L
    (Unicode:#$2115; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL N
    (Unicode:#$2119; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL P
    (Unicode:#$211A; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL Q
    (Unicode:#$211B; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL R
    (Unicode:#$211C; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL R
    (Unicode:#$211D; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL R
    (Unicode:#$2124; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL Z
    (Unicode:#$2126; Attr:laUpper; CaseCode:#$03C9),   // OHM SIGN
    (Unicode:#$2128; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL Z
    (Unicode:#$212A; Attr:laUpper; CaseCode:#$006B),   // KELVIN SIGN
    (Unicode:#$212B; Attr:laUpper; CaseCode:#$00E5),   // ANGSTROM SIGN
    (Unicode:#$212C; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL B
    (Unicode:#$212D; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL C
    (Unicode:#$212F; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL E
    (Unicode:#$2130; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL E
    (Unicode:#$2131; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL F
    (Unicode:#$2133; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL M
    (Unicode:#$2134; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL O
    (Unicode:#$2139; Attr:laLower; CaseCode:#$FFFF),   // INFORMATION SOURCE
    (Unicode:#$FB00; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FF
    (Unicode:#$FB01; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FI
    (Unicode:#$FB02; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FL
    (Unicode:#$FB03; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FFI
    (Unicode:#$FB04; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FFL
    (Unicode:#$FB05; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE LONG S T
    (Unicode:#$FB06; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE ST
    (Unicode:#$FB13; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN NOW
    (Unicode:#$FB14; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN ECH
    (Unicode:#$FB15; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN INI
    (Unicode:#$FB16; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE VEW NOW
    (Unicode:#$FB17; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN XEH
    (Unicode:#$FF21; Attr:laUpper; CaseCode:#$FF41),   // FULLWIDTH LATIN CAPITAL LETTER A
    (Unicode:#$FF22; Attr:laUpper; CaseCode:#$FF42),   // FULLWIDTH LATIN CAPITAL LETTER B
    (Unicode:#$FF23; Attr:laUpper; CaseCode:#$FF43),   // FULLWIDTH LATIN CAPITAL LETTER C
    (Unicode:#$FF24; Attr:laUpper; CaseCode:#$FF44),   // FULLWIDTH LATIN CAPITAL LETTER D
    (Unicode:#$FF25; Attr:laUpper; CaseCode:#$FF45),   // FULLWIDTH LATIN CAPITAL LETTER E
    (Unicode:#$FF26; Attr:laUpper; CaseCode:#$FF46),   // FULLWIDTH LATIN CAPITAL LETTER F
    (Unicode:#$FF27; Attr:laUpper; CaseCode:#$FF47),   // FULLWIDTH LATIN CAPITAL LETTER G
    (Unicode:#$FF28; Attr:laUpper; CaseCode:#$FF48),   // FULLWIDTH LATIN CAPITAL LETTER H
    (Unicode:#$FF29; Attr:laUpper; CaseCode:#$FF49),   // FULLWIDTH LATIN CAPITAL LETTER I
    (Unicode:#$FF2A; Attr:laUpper; CaseCode:#$FF4A),   // FULLWIDTH LATIN CAPITAL LETTER J
    (Unicode:#$FF2B; Attr:laUpper; CaseCode:#$FF4B),   // FULLWIDTH LATIN CAPITAL LETTER K
    (Unicode:#$FF2C; Attr:laUpper; CaseCode:#$FF4C),   // FULLWIDTH LATIN CAPITAL LETTER L
    (Unicode:#$FF2D; Attr:laUpper; CaseCode:#$FF4D),   // FULLWIDTH LATIN CAPITAL LETTER M
    (Unicode:#$FF2E; Attr:laUpper; CaseCode:#$FF4E),   // FULLWIDTH LATIN CAPITAL LETTER N
    (Unicode:#$FF2F; Attr:laUpper; CaseCode:#$FF4F),   // FULLWIDTH LATIN CAPITAL LETTER O
    (Unicode:#$FF30; Attr:laUpper; CaseCode:#$FF50),   // FULLWIDTH LATIN CAPITAL LETTER P
    (Unicode:#$FF31; Attr:laUpper; CaseCode:#$FF51),   // FULLWIDTH LATIN CAPITAL LETTER Q
    (Unicode:#$FF32; Attr:laUpper; CaseCode:#$FF52),   // FULLWIDTH LATIN CAPITAL LETTER R
    (Unicode:#$FF33; Attr:laUpper; CaseCode:#$FF53),   // FULLWIDTH LATIN CAPITAL LETTER S
    (Unicode:#$FF34; Attr:laUpper; CaseCode:#$FF54),   // FULLWIDTH LATIN CAPITAL LETTER T
    (Unicode:#$FF35; Attr:laUpper; CaseCode:#$FF55),   // FULLWIDTH LATIN CAPITAL LETTER U
    (Unicode:#$FF36; Attr:laUpper; CaseCode:#$FF56),   // FULLWIDTH LATIN CAPITAL LETTER V
    (Unicode:#$FF37; Attr:laUpper; CaseCode:#$FF57),   // FULLWIDTH LATIN CAPITAL LETTER W
    (Unicode:#$FF38; Attr:laUpper; CaseCode:#$FF58),   // FULLWIDTH LATIN CAPITAL LETTER X
    (Unicode:#$FF39; Attr:laUpper; CaseCode:#$FF59),   // FULLWIDTH LATIN CAPITAL LETTER Y
    (Unicode:#$FF3A; Attr:laUpper; CaseCode:#$FF5A),   // FULLWIDTH LATIN CAPITAL LETTER Z
    (Unicode:#$FF41; Attr:laLower; CaseCode:#$FF21),   // FULLWIDTH LATIN SMALL LETTER A
    (Unicode:#$FF42; Attr:laLower; CaseCode:#$FF22),   // FULLWIDTH LATIN SMALL LETTER B
    (Unicode:#$FF43; Attr:laLower; CaseCode:#$FF23),   // FULLWIDTH LATIN SMALL LETTER C
    (Unicode:#$FF44; Attr:laLower; CaseCode:#$FF24),   // FULLWIDTH LATIN SMALL LETTER D
    (Unicode:#$FF45; Attr:laLower; CaseCode:#$FF25),   // FULLWIDTH LATIN SMALL LETTER E
    (Unicode:#$FF46; Attr:laLower; CaseCode:#$FF26),   // FULLWIDTH LATIN SMALL LETTER F
    (Unicode:#$FF47; Attr:laLower; CaseCode:#$FF27),   // FULLWIDTH LATIN SMALL LETTER G
    (Unicode:#$FF48; Attr:laLower; CaseCode:#$FF28),   // FULLWIDTH LATIN SMALL LETTER H
    (Unicode:#$FF49; Attr:laLower; CaseCode:#$FF29),   // FULLWIDTH LATIN SMALL LETTER I
    (Unicode:#$FF4A; Attr:laLower; CaseCode:#$FF2A),   // FULLWIDTH LATIN SMALL LETTER J
    (Unicode:#$FF4B; Attr:laLower; CaseCode:#$FF2B),   // FULLWIDTH LATIN SMALL LETTER K
    (Unicode:#$FF4C; Attr:laLower; CaseCode:#$FF2C),   // FULLWIDTH LATIN SMALL LETTER L
    (Unicode:#$FF4D; Attr:laLower; CaseCode:#$FF2D),   // FULLWIDTH LATIN SMALL LETTER M
    (Unicode:#$FF4E; Attr:laLower; CaseCode:#$FF2E),   // FULLWIDTH LATIN SMALL LETTER N
    (Unicode:#$FF4F; Attr:laLower; CaseCode:#$FF2F),   // FULLWIDTH LATIN SMALL LETTER O
    (Unicode:#$FF50; Attr:laLower; CaseCode:#$FF30),   // FULLWIDTH LATIN SMALL LETTER P
    (Unicode:#$FF51; Attr:laLower; CaseCode:#$FF31),   // FULLWIDTH LATIN SMALL LETTER Q
    (Unicode:#$FF52; Attr:laLower; CaseCode:#$FF32),   // FULLWIDTH LATIN SMALL LETTER R
    (Unicode:#$FF53; Attr:laLower; CaseCode:#$FF33),   // FULLWIDTH LATIN SMALL LETTER S
    (Unicode:#$FF54; Attr:laLower; CaseCode:#$FF34),   // FULLWIDTH LATIN SMALL LETTER T
    (Unicode:#$FF55; Attr:laLower; CaseCode:#$FF35),   // FULLWIDTH LATIN SMALL LETTER U
    (Unicode:#$FF56; Attr:laLower; CaseCode:#$FF36),   // FULLWIDTH LATIN SMALL LETTER V
    (Unicode:#$FF57; Attr:laLower; CaseCode:#$FF37),   // FULLWIDTH LATIN SMALL LETTER W
    (Unicode:#$FF58; Attr:laLower; CaseCode:#$FF38),   // FULLWIDTH LATIN SMALL LETTER X
    (Unicode:#$FF59; Attr:laLower; CaseCode:#$FF39),   // FULLWIDTH LATIN SMALL LETTER Y
    (Unicode:#$FF5A; Attr:laLower; CaseCode:#$FF3A)    // FULLWIDTH LATIN SMALL LETTER Z
    );

type
  TUnicodeTitleCaseLetterInfo = packed record
    Unicode : WideChar;
    Upper   : WideChar;
    Lower   : WideChar;
  end;
  PUnicodeTitleCaseLetterInfo = ^TUnicodeTitleCaseLetterInfo;

const
  // Derived from 'Lt' class
  UnicodeTitleCaseLetterEntries = 31;
  UnicodeTitleCaseLetterInfo : Array[0..UnicodeTitleCaseLetterEntries - 1] of TUnicodeTitleCaseLetterInfo = (
    (Unicode:#$01C5; Upper:#$01C4; Lower:#$01C6),   // LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
    (Unicode:#$01C8; Upper:#$01C7; Lower:#$01C9),   // LATIN CAPITAL LETTER L WITH SMALL LETTER J
    (Unicode:#$01CB; Upper:#$01CA; Lower:#$01CC),   // LATIN CAPITAL LETTER N WITH SMALL LETTER J
    (Unicode:#$01F2; Upper:#$01F1; Lower:#$01F3),   // LATIN CAPITAL LETTER D WITH SMALL LETTER Z
    (Unicode:#$1F88; Upper:#$FFFF; Lower:#$1F80),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F89; Upper:#$FFFF; Lower:#$1F81),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F8A; Upper:#$FFFF; Lower:#$1F82),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8B; Upper:#$FFFF; Lower:#$1F83),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8C; Upper:#$FFFF; Lower:#$1F84),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8D; Upper:#$FFFF; Lower:#$1F85),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8E; Upper:#$FFFF; Lower:#$1F86),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F8F; Upper:#$FFFF; Lower:#$1F87),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F98; Upper:#$FFFF; Lower:#$1F90),   // GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F99; Upper:#$FFFF; Lower:#$1F91),   // GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F9A; Upper:#$FFFF; Lower:#$1F92),   // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9B; Upper:#$FFFF; Lower:#$1F93),   // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9C; Upper:#$FFFF; Lower:#$1F94),   // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9D; Upper:#$FFFF; Lower:#$1F95),   // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9E; Upper:#$FFFF; Lower:#$1F96),   // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F9F; Upper:#$FFFF; Lower:#$1F97),   // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FA8; Upper:#$FFFF; Lower:#$1FA0),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1FA9; Upper:#$FFFF; Lower:#$1FA1),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1FAA; Upper:#$FFFF; Lower:#$1FA2),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAB; Upper:#$FFFF; Lower:#$1FA3),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAC; Upper:#$FFFF; Lower:#$1FA4),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAD; Upper:#$FFFF; Lower:#$1FA5),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAE; Upper:#$FFFF; Lower:#$1FA6),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FAF; Upper:#$FFFF; Lower:#$1FA7),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FBC; Upper:#$FFFF; Lower:#$1FB3),   // GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
    (Unicode:#$1FCC; Upper:#$FFFF; Lower:#$1FC3),   // GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
    (Unicode:#$1FFC; Upper:#$FFFF; Lower:#$1FF3)    // GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
    );

{##############################################################################}
// Private methods

{-------------------------------------------------------------------------------
  LocateFoldingLowerCase (Fundamentals)
-------------------------------------------------------------------------------}
function LocateFoldingLowerCase(const Ch: WideChar): WideString;
begin
  if Ch = #$0130 then
    Result := #$0069#$0307
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
  LocateFoldingUpperCase (Fundamentals)
-------------------------------------------------------------------------------}
function LocateFoldingUpperCase(const Ch: WideChar): WideString;
begin
  if Ord(Ch) < $00DF then
    Result := '' else
  if Ord(Ch) <= $0587 then
    case Ord(Ch) of
      $00DF : Result := #$0053#$0053;         // # LATIN SMALL LETTER SHARP S
      $0149 : Result := #$02BC#$004E;         // # LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
      $01F0 : Result := #$004A#$030C;         // # LATIN SMALL LETTER J WITH CARON
      $0390 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
      $03B0 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
      $0587 : Result := #$0535#$0552;         // # ARMENIAN SMALL LIGATURE ECH YIWN
    else
      Result := '';
    end else
  if Ord(Ch) < $1E96 then
    Result := '' else
  if Ord(Ch) <= $1FFC then
    case Ord(Ch) of
      $1E96 : Result := #$0048#$0331;         // # LATIN SMALL LETTER H WITH LINE BELOW
      $1E97 : Result := #$0054#$0308;         // # LATIN SMALL LETTER T WITH DIAERESIS
      $1E98 : Result := #$0057#$030A;         // # LATIN SMALL LETTER W WITH RING ABOVE
      $1E99 : Result := #$0059#$030A;         // # LATIN SMALL LETTER Y WITH RING ABOVE
      $1E9A : Result := #$0041#$02BE;         // # LATIN SMALL LETTER A WITH RIGHT HALF RING
      $1F50 : Result := #$03A5#$0313;         // # GREEK SMALL LETTER UPSILON WITH PSILI
      $1F52 : Result := #$03A5#$0313#$0300;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
      $1F54 : Result := #$03A5#$0313#$0301;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
      $1F56 : Result := #$03A5#$0313#$0342;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
      $1F80 : Result := #$1F08#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
      $1F81 : Result := #$1F09#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
      $1F82 : Result := #$1F0A#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
      $1F83 : Result := #$1F0B#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
      $1F84 : Result := #$1F0C#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
      $1F85 : Result := #$1F0D#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
      $1F86 : Result := #$1F0E#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
      $1F87 : Result := #$1F0F#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
      $1F88 : Result := #$1F08#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
      $1F89 : Result := #$1F09#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
      $1F8A : Result := #$1F0A#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
      $1F8B : Result := #$1F0B#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
      $1F8C : Result := #$1F0C#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
      $1F8D : Result := #$1F0D#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
      $1F8E : Result := #$1F0E#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
      $1F8F : Result := #$1F0F#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
      $1F90 : Result := #$1F28#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
      $1F91 : Result := #$1F29#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
      $1F92 : Result := #$1F2A#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
      $1F93 : Result := #$1F2B#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
      $1F94 : Result := #$1F2C#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
      $1F95 : Result := #$1F2D#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
      $1F96 : Result := #$1F2E#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
      $1F97 : Result := #$1F2F#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
      $1F98 : Result := #$1F28#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
      $1F99 : Result := #$1F29#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
      $1F9A : Result := #$1F2A#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
      $1F9B : Result := #$1F2B#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
      $1F9C : Result := #$1F2C#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
      $1F9D : Result := #$1F2D#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
      $1F9E : Result := #$1F2E#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
      $1F9F : Result := #$1F2F#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
      $1FA0 : Result := #$1F68#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
      $1FA1 : Result := #$1F69#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
      $1FA2 : Result := #$1F6A#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
      $1FA3 : Result := #$1F6B#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
      $1FA4 : Result := #$1F6C#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
      $1FA5 : Result := #$1F6D#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
      $1FA6 : Result := #$1F6E#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
      $1FA7 : Result := #$1F6F#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
      $1FA8 : Result := #$1F68#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
      $1FA9 : Result := #$1F69#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
      $1FAA : Result := #$1F6A#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
      $1FAB : Result := #$1F6B#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
      $1FAC : Result := #$1F6C#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
      $1FAD : Result := #$1F6D#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
      $1FAE : Result := #$1F6E#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
      $1FAF : Result := #$1F6F#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
      $1FB2 : Result := #$1FBA#$0399;         // # GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
      $1FB3 : Result := #$0391#$0399;         // # GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
      $1FB4 : Result := #$0386#$0399;         // # GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
      $1FB6 : Result := #$0391#$0342;         // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI
      $1FB7 : Result := #$0391#$0342#$0399;   // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FBC : Result := #$0391#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
      $1FC2 : Result := #$1FCA#$0399;         // # GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
      $1FC3 : Result := #$0397#$0399;         // # GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
      $1FC4 : Result := #$0389#$0399;         // # GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
      $1FC6 : Result := #$0397#$0342;         // # GREEK SMALL LETTER ETA WITH PERISPOMENI
      $1FC7 : Result := #$0397#$0342#$0399;   // # GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FCC : Result := #$0397#$0399;         // # GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
      $1FD2 : Result := #$0399#$0308#$0300;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
      $1FD3 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
      $1FD6 : Result := #$0399#$0342;         // # GREEK SMALL LETTER IOTA WITH PERISPOMENI
      $1FD7 : Result := #$0399#$0308#$0342;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
      $1FE2 : Result := #$03A5#$0308#$0300;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
      $1FE3 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
      $1FE4 : Result := #$03A1#$0313;         // # GREEK SMALL LETTER RHO WITH PSILI
      $1FE6 : Result := #$03A5#$0342;         // # GREEK SMALL LETTER UPSILON WITH PERISPOMENI
      $1FE7 : Result := #$03A5#$0308#$0342;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
      $1FF2 : Result := #$1FFA#$0399;         // # GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
      $1FF3 : Result := #$03A9#$0399;         // # GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
      $1FF4 : Result := #$038F#$0399;         // # GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
      $1FF6 : Result := #$03A9#$0342;         // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI
      $1FF7 : Result := #$03A9#$0342#$0399;   // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FFC : Result := #$03A9#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
    else
      Result := '';
    end else
  if Ord(Ch) < $FB00 then
    Result := '' else
  if Ord(Ch) <= $FB17 then
    case Ord(Ch) of
      $FB00 : Result := #$0046#$0046;         // # LATIN SMALL LIGATURE FF
      $FB01 : Result := #$0046#$0049;         // # LATIN SMALL LIGATURE FI
      $FB02 : Result := #$0046#$004C;         // # LATIN SMALL LIGATURE FL
      $FB03 : Result := #$0046#$0046#$0049;   // # LATIN SMALL LIGATURE FFI
      $FB04 : Result := #$0046#$0046#$004C;   // # LATIN SMALL LIGATURE FFL
      $FB05 : Result := #$0053#$0054;         // # LATIN SMALL LIGATURE LONG S T
      $FB06 : Result := #$0053#$0054;         // # LATIN SMALL LIGATURE ST
      $FB13 : Result := #$0544#$0546;         // # ARMENIAN SMALL LIGATURE MEN NOW
      $FB14 : Result := #$0544#$0535;         // # ARMENIAN SMALL LIGATURE MEN ECH
      $FB15 : Result := #$0544#$053B;         // # ARMENIAN SMALL LIGATURE MEN INI
      $FB16 : Result := #$054E#$0546;         // # ARMENIAN SMALL LIGATURE VEW NOW
      $FB17 : Result := #$0544#$053D;         // # ARMENIAN SMALL LIGATURE MEN XEH
    else
      Result := '';
    end
  else
    Result := '';
end;

{-------------------------------------------------------------------------------
  LocateLetterInfo (Fundamentals)
-------------------------------------------------------------------------------}
function LocateLetterInfo(const Ch: WideChar): Integer;
var
  L, H, I: Integer;
  D: WideChar;
begin
  // Binary search [Avg number of comparisons = Log2(UnicodeLetterEntries) = 10]
  L:= 0;
  H:= UnicodeLetterEntries - 1;
  repeat
    I:= (L + H) div 2;
    D:= UnicodeLetterInfo[I].Unicode;
    if D = Ch then
    begin
      Result:= I;
      exit; // -->
    end
    else
    if D > Ch then
    H:= I - 1
    else
    L:= I + 1;
  until L > H;
  Result:= -1;
end;

{-------------------------------------------------------------------------------
  LocateOtherLowerCase (Fundamentals)
-------------------------------------------------------------------------------}
function LocateOtherLowerCase(const Ch: WideChar): WideChar;
begin
  case Ord(Ch) of
    $2170..$217F: Result:= WideChar(Ord(Ch) - $2170 + $2160);    // # Nl  [16] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
    $24D0..$24E9: Result:= WideChar(Ord(Ch) - $24D0 + $24B6);    // # So  [26] CIRCLED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
  else
    Result:= #$0000;
  end;
end;

{-------------------------------------------------------------------------------
  LocateOtherUpperCase (Fundamentals)
-------------------------------------------------------------------------------}
function LocateOtherUpperCase(const Ch: WideChar): WideChar;
begin
  case Ord(Ch) of
    $2160..$216F: Result:= WideChar(Ord(Ch) - $2160 + $2170);    // # Nl  [16] ROMAN NUMERAL ONE..ROMAN NUMERAL ONE THOUSAND
    $24B6..$24CF: Result:= WideChar(Ord(Ch) - $24B6 + $24D0);    // # So  [26] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN CAPITAL LETTER Z
  else
    Result:= #$0000;
  end;
end;

{-------------------------------------------------------------------------------
  LocateTitleCaseLetterInfo (Fundamentals)
-------------------------------------------------------------------------------}
function LocateTitleCaseLetterInfo(const Ch: WideChar): Integer;
var
  I : Integer;
begin
  if (Ord(Ch) < $01C5) or (Ord(Ch) > $1FFC) then
  Result:= -1
  else if (Ord(Ch) > $01F2) and (Ord(Ch) < $1F88) then
  Result:= -1
  else
  begin
    for I:= 0 to UnicodeTitleCaseLetterEntries - 1 do
    begin
      if UnicodeTitleCaseLetterInfo[I].Unicode = Ch then
      begin
        Result:= I;
        exit; // -->
      end;
    end;
    Result:= -1;
  end;
end;

{##############################################################################}
// Public methods

{-------------------------------------------------------------------------------
  CharToWideChar (JCL)
-------------------------------------------------------------------------------}
function CharToWideChar(Ch: AnsiChar): WideChar;
var
  WS: WideString;
begin
  WS:= WideChar(Ch);
  Result:= WS[1];
end;

{-------------------------------------------------------------------------------
  WideCharToChar (JCL)
-------------------------------------------------------------------------------}
function WideCharToChar(Ch: WideChar): AnsiChar;
var
  S: WideString;
begin
  S:= Ch;
  Result:= AnsiChar(S[1]);
end;

{-------------------------------------------------------------------------------
  WideCompareText (CubicCore)
-------------------------------------------------------------------------------}
function WideCompareText(const S1, S2: WideString): SizeInt;
begin
  Result:= WideStrIComp(PWideChar(S1), PWideChar(S2));
end;

{-------------------------------------------------------------------------------
  WideCompareStr (CubicCore)
-------------------------------------------------------------------------------}
function WideCompareStr(const S1, S2: WideString): SizeInt;
begin
  Result:= WideStrComp(PWideChar(S1), PWideChar(S2));
end;

{-------------------------------------------------------------------------------
  WidePos (JCL)
-------------------------------------------------------------------------------}
function WidePos(const SubStr, Str: WideString): SizeInt;
var
  P: PWideChar;
begin
  P:= WidePPos(PWideChar(SubStr), PWideChar(Str));
  if P <> nil then
  Result:= P - PWideChar(Str) + 1
  else
  Result:= 0;
end;

{-------------------------------------------------------------------------------
  WidePPos (JCL)
-------------------------------------------------------------------------------}
function WidePPos(const SubStr, Str: PWideChar): PWideChar;
var
  P: PWideChar;
  I: SizeInt;
begin
  Result:= nil;
  if (Str = nil) or (SubStr = nil) or (Str^ = #0) or (SubStr^ = #0) then
  Exit;

  Result:= Str;
  while Result^ <> #0 do
  begin
    if Result^ <> SubStr^ then
    Inc(Result)
    else
    begin
      P:= Result + 1;
      I:= 1;
      while (P^ <> #0) and (P^ = SubStr[I]) do
      begin
        Inc(I);
        Inc(P);
      end;
      if SubStr[I] = #0 then
      Exit // -->
      else
      Inc(Result);
    end;
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  WideQuotedStr (JCL)
-------------------------------------------------------------------------------}
function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
var
  P, Src,
  Dest: PWideChar;
  AddCount: SizeInt;
begin
  AddCount:= 0;
  P:= WideStrScan(PWideChar(S), Quote);
  while P <> nil do
  begin
    Inc(P);
    Inc(AddCount);
    P:= WideStrScan(P, Quote);
  end;

  if AddCount = 0 then
    Result:= Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest:= PWideChar(Result);
    Dest^:= Quote;
    Inc(Dest);
    Src:= PWideChar(S);
    P:= WideStrScan(Src, Quote);
    repeat
      Inc(P);
      MoveWideChar(Src^, Dest^, P - Src);
      Inc(Dest, P - Src);
      Dest^:= Quote;
      Inc(Dest);
      Src:= P;
      P:= WideStrScan(Src, Quote);
    until P = nil;
    P:= WideStrEnd(Src);
    MoveWideChar(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^:= Quote;
  end;
end;

{-------------------------------------------------------------------------------
  WideStrScan (JCL)
-------------------------------------------------------------------------------}
function WideStrScan(const Str: PWideChar; Ch: WideChar): PWideChar;
begin
  Result:= Str;
  if Result <> nil then
  begin
    while (Result^ <> #0) and (Result^ <> Ch) do
    Inc(Result);

    if (Result^ = #0) and (Ch <> #0) then
    Result:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  WideStrScan (JCL)
    - Returns a pointer to first occurrence of a specified character in a string
      or nil if not found.
    - Note: this is just a binary search for the specified character and there's no
          check for a terminating null. Instead at most StrLen characters are
          searched. This makes this function extremly fast.
-------------------------------------------------------------------------------}
function WideStrScan(Str: PWideChar; Chr: WideChar; StrLen: SizeInt): PWideChar;
begin
  Result:= Str;
  while StrLen > 0 do
  begin
    if Result^ = Chr then
    Exit;

    Inc(Result);
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  WideStrNew (JCL)
    - Duplicates the given string (if not nil) and returns the address of the new string.
-------------------------------------------------------------------------------}
function WideStrNew(const Str: PWideChar): PWideChar;
var
  Size: SizeInt;
begin
  if Str = nil then
  Result:= nil
  else
  begin
    Size:= WideStrLen(Str) + 1;
    Result:= WideStrMove(WideStrAlloc(Size), Str, Size);
  end;
end;

{-------------------------------------------------------------------------------
  WideStrNew (JCL)
-------------------------------------------------------------------------------}
function WideStrNew(const Str: WideString): PWideChar;
begin
  Result:= WideStrNew(PWideChar(Str));
end;

{-------------------------------------------------------------------------------
  MoveWideChar (JCL)
-------------------------------------------------------------------------------}
procedure MoveWideChar(const Source; var Dest; Count: SizeInt);
begin
  Move(Source, Dest, Count * SizeOf(WideChar));
end;

{-------------------------------------------------------------------------------
  WideStrEnd (JCL)
-------------------------------------------------------------------------------}
function WideStrEnd(const Str: PWideChar): PWideChar;
begin
  Result:= Str;
  if Result <> nil then
  begin
    while Result^ <> #0 do
    Inc(Result);
  end;
end;

{-------------------------------------------------------------------------------
  WideStrLen (JCL)
-------------------------------------------------------------------------------}
function WideStrLen(const Str: PWideChar): SizeInt;
begin
  Result:= 0;
  if Str <> nil then
  begin
    while Str[Result] <> #0 do
    Inc(Result);
  end;
end;

{-------------------------------------------------------------------------------
  WideStrMove (JCL)
-------------------------------------------------------------------------------}
function WideStrMove(Dest: PWideChar; const Source: PWideChar; Count: SizeInt):
    PWideChar;
begin
  Result:= Dest;
  if Count > 0 then
  Move(Source^, Dest^, Count * SizeOf(WideChar));
end;

{-------------------------------------------------------------------------------
  WideStrAlloc (JCL)
-------------------------------------------------------------------------------}
function WideStrAlloc(WideSize: SizeInt): PWideChar;
begin
  WideSize := SizeOf(WideChar) * WideSize + SizeOf(SizeInt);
  Result := AllocMem(WideSize);
  SizeInt(Pointer(Result)^) := WideSize;
  Inc(Result, SizeOf(SizeInt) div SizeOf(WideChar));
end;

{-------------------------------------------------------------------------------
  SwapWordByteOrder JCL)
-------------------------------------------------------------------------------}
procedure SwapWordByteOrder(P: PWideChar; Len: SizeInt);
begin
  while Len > 0 do
  begin
    Dec(Len);
    P^:= WideChar((Word(P^) shr 8) or (Word(P^) shl 8));
    Inc(P);
  end;
end;

{-------------------------------------------------------------------------------
  WideExtractQuotedStr (JCL)
-------------------------------------------------------------------------------}
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
var
  P, Dest: PWideChar;
  DropCount: SizeInt;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then
    Exit;

  Inc(Src);
  DropCount := 1;
  P:= Src;
  Src:= WideStrScan(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then
    Break; // -->
    Inc(Src);
    Inc(DropCount);
    Src:= WideStrScan(Src, Quote);
  end;

  if Src = nil then
  Src:= WideStrEnd(P);

  if (Src - P) <= 1 then
  Exit; // -->

  if DropCount = 1 then
  SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest:= PWideChar(Result);
    Src:= WideStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then
      Break; // -->
      MoveWideChar(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P:= Src;
      Src:= WideStrScan(Src, Quote);
    end;

    if Src = nil then
    Src:= WideStrEnd(P);

    MoveWideChar(P^, Dest^, Src - P - 1);
  end;
end;

{-------------------------------------------------------------------------------
  WideStrComp (JCL)
    - Binary comparation of Str1 and Str2 with surrogate fix-up.
      Returns < 0 if Str1 is smaller in binary order than Str2.
      0 if both strings are equal and > 0 if Str1 is larger than Str2.

    - This code is based on an idea of Markus W. Scherer (IBM).
      Note: The surrogate fix-up is necessary because some single value code points have
        larger values than surrogates which are in UTF-32 actually larger.
-------------------------------------------------------------------------------}
  const
    // data used to bring UTF-16 coded strings into correct UTF-32 order for correct comparation
    UTF16Fixup: array [0..31] of Word = (
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      $2000, $F800, $F800, $F800, $F800
    );

function WideStrComp(const Str1, Str2: PWideChar): SizeInt;
var
  C1, C2: Word;
  Run1, Run2: PWideChar;
begin
  Run1:= Str1;
  Run2:= Str2;
  repeat
    C1:= Word(Run1^);
    C1:= Word(C1 or UTF16Fixup[C1 shr 11]);
    C2:= Word(Run2^);
    C2:= Word(C2 or UTF16Fixup[C2 shr 11]);

    // now C1 and C2 are in UTF-32-compatible order
    Result:= SizeInt(C1) - SizeInt(C2);

    if(Result <> 0) or (C1 = 0) or (C2 = 0) then
    Break; // -->

    Inc(Run1);
    Inc(Run2);
  until False;

  // If the strings have different lengths but the comparation returned equity so far
  // then adjust the result so that the longer string is marked as the larger one.
  if Result = 0 then
  Result:= (Run1 - Str1) - (Run2 - Str2);
end;

{-------------------------------------------------------------------------------
  WideStrIComp (JCL)
-------------------------------------------------------------------------------}
function WideStrIComp(const Str1, Str2: PWideChar): SizeInt;
var
  C1, C2: Word;
  Run1, Run2: PWideChar;
begin
  Run1:= Str1;
  Run2:= Str2;
  repeat
    C1:= Word(WideLowCase(WideChar(Run1^)));
    C1:= Word(C1 or UTF16Fixup[C1 shr 11]);
    C2:= Word(WideLowCase(WideChar(Run2^)));
    C2:= Word(C2 or UTF16Fixup[C2 shr 11]);

    // now C1 and C2 are in UTF-32-compatible order
    Result:= SizeInt(C1) - SizeInt(C2);

    if(Result <> 0) or (C1 = 0) or (C2 = 0) then
    Break; // -->

    Inc(Run1);
    Inc(Run2);
  until False;

  // If the strings have different lengths but the comparation returned equity so far
  // then adjust the result so that the longer string is marked as the larger one.
  if Result = 0 then
  Result:= (Run1 - Str1) - (Run2 - Str2);
end;

{-------------------------------------------------------------------------------
  WideStrCopy (JCL)
-------------------------------------------------------------------------------}
function WideStrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar;
var
  Src: PWideChar;
begin
  Result:= Dest;
  if Dest <> nil then
  begin
    Src:= Source;
    if Src <> nil then
    begin
      while Src^ <> #0 do
      begin
        Dest^:= Src^;
        Inc(Src);
        Inc(Dest);
      end;
    end;
    Dest^:= #0;
  end;
end;

{-------------------------------------------------------------------------------
  WideStrCat (JCL)
-------------------------------------------------------------------------------}
function WideStrCat(Dest: PWideChar; const Source: PWideChar): PWideChar;
begin
  Result:= Dest;
  WideStrCopy(WideStrEnd(Dest), Source);
end;

{-------------------------------------------------------------------------------
  WideStrBufSize (JCL)
    - Returns max number of wide characters that can be stored in a buffer
      allocated by StrAllocW.
-------------------------------------------------------------------------------}
function WideStrBufSize(const Str: PWideChar): SizeInt;
var
  P: PWideChar;
begin
  if Str <> nil then
  begin
    P:= Str;
    Dec(P, SizeOf(SizeInt) div SizeOf(WideChar));
    Result:= (PSizeInt(P)^ - SizeOf(SizeInt)) div SizeOf(WideChar);
  end
  else
    Result:= 0;
end;

{-------------------------------------------------------------------------------
  WideLowCase (Fundamentals)
-------------------------------------------------------------------------------}
function WideLowCase(const Ch: WideChar): WideChar;
var I : Integer;
    J : Integer;
    C : WideChar;
    P : PUnicodeLetterInfo;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
  begin
    if AnsiChar(Ord(Ch)) in ['A'..'Z'] then
    Result:= WideChar(Ord(Ch) + (Ord('a') - Ord('A')))
    else
    Result:= Ch;
  end
  else
  begin
    I:= LocateLetterInfo(Ch);
    if I >= 0 then
    begin
      P:= @UnicodeLetterInfo[I];
      if P^.Attr = laLower then
      Result := Ch
      else
      begin
        C:= P^.CaseCode;
        if C = #$FFFF then
        Result:= Ch
        else
        Result:= C;
      end;
    end
    else
    begin
      J:= LocateTitleCaseLetterInfo(Ch);
      if J >= 0 then
      begin
        C := UnicodeTitleCaseLetterInfo[J].Lower;
        if C = #$FFFF then
        Result := Ch
        else
        Result := C;
      end
      else
      begin
        C:= LocateOtherUpperCase(Ch);
        if C = #$0000 then
        Result:= Ch
        else
        Result:= C;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  WideIsSameStr (CubicCore)
-------------------------------------------------------------------------------}
function WideIsSameStr(const S1, S2: WideString): Boolean;
begin
  if Length(S1) <> Length(S2) then
  Result:= false
  else
  Result:= ccStrings.WideCompareStr(S1, S2) = 0;
end;

{-------------------------------------------------------------------------------
  WideIsSameText (CubicCore)
-------------------------------------------------------------------------------}
function WideIsSameText(const S1, S2: WideString): Boolean;
begin
  if Length(S1) <> Length(S2) then
  Result:= false
  else
  Result:= ccStrings.WideCompareText(S1, S2) = 0;
end;

{-------------------------------------------------------------------------------
  WideLowCaseFolding (Fundamentals)
-------------------------------------------------------------------------------}
function WideLowCaseFolding(const Ch: WideChar): WideString;
var
  R: WideChar;
begin
  R:= WideLowCase(Ch);
  if R = Ch then
  begin
    Result:= LocateFoldingLowerCase(Ch);
    if Result = '' then
    Result:= Ch;
  end
  else
  Result:= R;
end;

{-------------------------------------------------------------------------------
  WideLowerCase (Fundamentals)
-------------------------------------------------------------------------------}
function WideLowerCase(const S: WideString): WideString;
var
  I: Integer;
begin
  Result:= '';
  for I:= 1 to Length(S) do
  Result:= Result + WideLowCaseFolding(S[I]);
end;

{-------------------------------------------------------------------------------
  WideUpCase (Fundamentals)
-------------------------------------------------------------------------------}
function WideUpCase(const Ch: WideChar): WideChar;
var
  I: Integer;
  J: Integer;
  C: WideChar;
  P: TUnicodeLetterInfo;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
  begin
    if AnsiChar(Ord(Ch)) in ['a'..'z'] then
    Result:= WideChar(Ord(Ch) - (Ord('a') - Ord('A')))
    else
    Result:= Ch;
  end
  else
  begin
    I:= LocateLetterInfo(Ch);
    if I >= 0 then
    begin
      P:= UnicodeLetterInfo[I];
      if P.Attr = laUpper then
      Result:= Ch
      else
      begin
        C:= P.CaseCode;
        if C = #$FFFF then
        Result:= Ch
        else
        Result := C;
      end;
    end
    else
    begin
      J:= LocateTitleCaseLetterInfo(Ch);
      if J >= 0 then
      begin
        C:= UnicodeTitleCaseLetterInfo[J].Upper;
        if C = #$FFFF then
        Result:= Ch
        else
        Result:= C;
      end
      else
      begin
        C:= LocateOtherLowerCase(Ch);
        if C = #$0000 then
        Result:= Ch
        else
        Result := C;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  WideUpCaseFolding (Fundamentals)
-------------------------------------------------------------------------------}
function WideUpCaseFolding(const Ch: WideChar): WideString;
var
  R: WideChar;
begin
  R:= WideUpCase(Ch);
  if R = Ch then
  begin
    Result:= LocateFoldingUpperCase(Ch);
    if Result = '' then
    Result:= Ch;
  end
  else
  Result:= R;
end;

{-------------------------------------------------------------------------------
  WideUpperCase (Fundamentals)
-------------------------------------------------------------------------------}
function WideUpperCase(const S: WideString): WideString;
var
  I: Integer;
begin
  Result:= '';
  for I:= 1 to Length(S) do
  Result:= Result + WideUpCaseFolding(S[I]);
end;

{-------------------------------------------------------------------------------
  WideGetNextItem (CubicCore)
  - Returns an item from a character separated list. (item1,item2,item3...).
  - If separator is not found, AFromString is returned.
  - If ADeleteFromString = true, the found item is removed from AFromString.
  - If item is empty, ADefault is returned.
-------------------------------------------------------------------------------}
function WideGetNextItem(var AFromString: WideString; const ASeparator:
    WideChar; ADeleteFromString: Boolean = true; const ADefault: WideString =
    ''): WideString;
var
  i: Integer;
begin
  Result:= '';

  if AFromString = '' then
  Exit;

  i:= WidePos(ASeparator, AFromString);
  if i <> 0 then
  begin
    Result:= Copy(AFromString, 1, i-1);
    if ADeleteFromString then
    AFromString:= Copy(AFromString, i+1, Length(AFromString)-i);
  end
  else
  Result:= AFromString;

  if Result = '' then
  Result:= ADefault;
end;

{-------------------------------------------------------------------------------
  WideTrunkFromNull (CubicCore)
  - Truncates string from the first null character.
-------------------------------------------------------------------------------}
procedure WideTrunkFromNull(var AStr: WideString);
var
  i: Integer;
begin
  for i:= 1 to Length(AStr) do
  begin
    if AStr[i] = #0 then
    begin
      SetLength(AStr, i);
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  AnsiTrunkFromNull (CubicCore)
  - Truncates string from the first null character.
-------------------------------------------------------------------------------}
procedure AnsiTrunkFromNull(var AStr: AnsiString);
var
  i: Integer;
begin
  for i:= 1 to Length(AStr) do
  begin
    if AStr[i] = #0 then
    begin
      SetLength(AStr, i);
      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  WideReplaceChar (Fundamentals)
  - Replace specified character(s) with a string.
-------------------------------------------------------------------------------}
function WideReplaceChar(const Find: WideChar; const Replace: WideString; const
    AStr: WideString): WideString;
var
  C, L, M, I, R: Integer;
  P, Q: PWideChar;
begin
  // count number of chars to replace
  C:= WideCountChar(Find, AStr);
  if C = 0 then
  begin
    Result:= AStr;
    Exit;
  end;

  // count length of resulting string
  R:= Length(Replace);
  M:= Length(AStr);
  L:= M + (R - 1) * C;
  if L = 0 then
  begin
    Result:= '';
    Exit;
  end;

  SetLength(Result, L);
  P:= Pointer(AStr);
  Q:= Pointer(Result);

  // replace
  for I:= 1 to M do
  begin
    if P^ = Find then
    begin
      if R > 0 then
      begin
        Move(Pointer(Replace)^, Q^, Sizeof(WideChar) * R);
        Inc(Q, R);
      end;
      Inc(P);
    end
    else
    begin
      Q^:= P^;
      Inc(P);
      Inc(Q);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  WideCountChar (Fundamentals)
  - Returns the number of AChars found in AStr.
-------------------------------------------------------------------------------}
function WideCountChar(const AChar: WideChar; const AStr: WideString): Integer;
var
  Q: PWideChar;
  I: Integer;
begin
  Result:= 0;
  Q:= PWideChar(AStr);
  
  if not Assigned(Q) then
  Exit;

  for I:= 1 to Length(AStr) do
  begin
    if Q^ = AChar then
    Inc(Result);
    Inc(Q);
  end;
end;

{-------------------------------------------------------------------------------
  WideRectToStr (CubicCore)
  - Returns a text representation of TRect.
  - Format is: '[left,top,right,bottom]'
-------------------------------------------------------------------------------}
function WideRectToStr(ARect: TRect): WideString;
begin
  Result:= '[' + IntToStr(ARect.Left) + ',' +
           IntToStr(ARect.Top) + ',' +
           IntToStr(ARect.Right) + ',' +
           IntToStr(ARect.Bottom) + ']';
end;

{-------------------------------------------------------------------------------
  WidePointToStr (CubicCore)
  - Returns a text representation of TPoint.
  - Format is: '[X,Y]'
-------------------------------------------------------------------------------}
function WidePointToStr(APoint: TPoint): WideString;
begin
  Result:= '[' + IntToStr(APoint.X) + ',' +
           IntToStr(APoint.Y) + ']';
end;

{-------------------------------------------------------------------------------
  WideStrToRectDef (CubicCore)
  - Converts string to rect
  - If AStr is invalid format, ADefaultRect is returned.
  - AStr should be formated like so: '[left,top,right,bottom]'
-------------------------------------------------------------------------------}
function WideStrToRectDef(const AStr: WideString; ADefaultRect: TRect): TRect;
var
  i,c, index: Integer;
  startI, endI: Integer;
  char: AnsiChar;
begin
  Result:= ADefaultRect;
  c:= Length(AStr);
  index:= 0;
  startI:= 0;
  endI:= 0;
  for i:= 1 to c do
  begin
    char:= AnsiChar(AStr[i]);
    if index > 0 then
    begin
      if char in ['0'..'9'] then
      begin
        if startI = 0 then
        startI:= i;
        endI:= i;
      end
      else if char in [',', ']'] then
      begin
        case index of
          1: Result.Left:= StrToIntDef(Copy(AStr, startI, endI-startI+1), ADefaultRect.Left);
          2: Result.Top:= StrToIntDef(Copy(AStr, startI, endI-startI+1), ADefaultRect.Top);
          3: Result.Right:= StrToIntDef(Copy(AStr, startI, endI-startI+1), ADefaultRect.Right);
          4: Result.Bottom:= StrToIntDef(Copy(AStr, startI, endI-startI+1), ADefaultRect.Bottom);
        end;

        if index >= 4 then Exit;

        index:= index + 1;
        startI:= 0;
      end;
    end
    else if char = '[' then
    begin
      index:= 1;
    end
  end;
end;

{-------------------------------------------------------------------------------
  WideStrToPointDef (CubicCore)
  - Converts string to point
  - If AStr is invalid format, ADefaultPoint is returned.
  - AStr should be formated like so: '[X,Y]'
-------------------------------------------------------------------------------}
function WideStrToPointDef(const AStr: WideString; ADefaultPoint: TPoint):
    TPoint;
var
  i,c, index: Integer;
  startI, endI: Integer;
  char: AnsiChar;
begin
  Result:= ADefaultPoint;
  c:= Length(AStr);
  index:= 0;
  startI:= 0;
  endI:= 0;
  for i:= 1 to c do
  begin
    char:= AnsiChar(AStr[i]);
    if index > 0 then
    begin
      if char in ['0'..'9'] then
      begin
        if startI = 0 then
        startI:= i;
        endI:= i;
      end
      else if char in [',', ']'] then
      begin
        case index of
          1: Result.X:= StrToIntDef(Copy(AStr, startI, endI-startI+1), ADefaultPoint.X);
          2: Result.Y:= StrToIntDef(Copy(AStr, startI, endI-startI+1), ADefaultPoint.Y);
        end;

        if index >= 2 then Exit;

        index:= index + 1;
        startI:= 0;
      end;
    end
    else if char = '[' then
    begin
      index:= 1;
    end
  end;
end;

{-------------------------------------------------------------------------------
  WideStrDispose (JCL)
  - Release a string allocated with WideStrAlloc or WideStrNew.
-------------------------------------------------------------------------------}
procedure WideStrDispose(Str: PWideChar);
begin
  if Str <> nil then
  begin
    Dec(Str, SizeOf(SizeInt) div SizeOf(WideChar));
    FreeMem(Str);
  end;
end;

{-------------------------------------------------------------------------------
  WideStrDisposeAndNil (JCL)
  - Release a string allocated with WideStrAlloc or WideStrNew.
  - Sets Str to nil.
-------------------------------------------------------------------------------}
procedure WideStrDisposeAndNil(var Str: PWideChar);
var
  Buff: PWideChar;
begin
  Buff:= Str;
  Str:= nil;
  WideStrDispose(Buff);
end;

end.

