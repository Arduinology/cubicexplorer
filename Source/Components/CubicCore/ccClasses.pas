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
//  The Original Code is ccClasses.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccClasses;

{$INCLUDE Core.inc}

{==============================================================================}
interface

uses
  // CubicCore
  ccStrings, ccFileUtils, ccTypes, ccThreads,
  // System Units
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, RTLConsts;

type
{-------------------------------------------------------------------------------
  TCCStrings
-------------------------------------------------------------------------------}
  TWideFileOptionsType =
   (
    foAnsiFile,  // loads/writes an ANSI file
    foUTF8Encoding,
    foUnicodeLB  // reads/writes BOM_LSB_FIRST/BOM_MSB_FIRST
   );
  TWideFileOptions = set of TWideFileOptionsType;

  TCCStrings = class(TPersistent)
  private
    FDelimiter: WideChar;
    FQuoteChar: WideChar;
    FNameValueSeparator: WideChar;
    FLineSeparator: WideString;
    FUpdateCount: Integer;
    function GetCommaText: WideString;
    function GetDelimitedText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetDelimitedText(const Value: WideString);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
    function GetValueFromIndex(Index: Integer): WideString;
    procedure SetValueFromIndex(Index: Integer; const Value: WideString);
  protected
    fTag: Integer;
    procedure DefineProperties(Filer: TFiler); override;
    function ExtractName(const S: WideString): WideString;
    function GetP(Index: Integer): PWideString; virtual; abstract;
    function Get(Index: Integer): WideString;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual; abstract;
    procedure PutObject(Index: Integer; AObject: TObject); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: WideString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: WideString): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    function Add(const S: WideString): Integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(Strings: TCCStrings); overload; virtual;
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    function CreateAnsiStringList: TStrings;
    procedure AddStringsTo(Dest: TStrings); virtual;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TCCStrings): Boolean; {$IFDEF RTL200_UP}reintroduce; {$ENDIF RTL200_UP}overload;
    function Equals(Strings: TStrings): Boolean; {$IFDEF RTL200_UP}reintroduce; {$ENDIF RTL200_UP}overload;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetText: PWideChar; virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: WideString); virtual;
    procedure InsertObject(Index: Integer; const S: WideString;
      AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: TWideFileName; WideFileOptions:
        TWideFileOptions = []); virtual;
    procedure LoadFromStream(Stream: TStream;
      WideFileOptions: TWideFileOptions = []); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: TWideFileName;
      WideFileOptions: TWideFileOptions = []); virtual;
    procedure SaveToStream(Stream: TStream;
      WideFileOptions: TWideFileOptions = []); virtual;
    procedure SetText(Text: PWideChar); virtual;
    function GetDelimitedTextEx(ADelimiter, AQuoteChar: WideChar): WideString;
        virtual;
    procedure SetDelimitedTextEx(ADelimiter, AQuoteChar: WideChar; const Value: WideString);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: WideChar read FDelimiter write FDelimiter;
    property DelimitedText: WideString read GetDelimitedText write SetDelimitedText;
    property Names[Index: Integer]: WideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: WideChar read FQuoteChar write FQuoteChar;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: WideString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: WideChar read FNameValueSeparator write FNameValueSeparator;
    property LineSeparator: WideString read FLineSeparator write FLineSeparator;
    property PStrings[Index: Integer]: PWideString read GetP;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Tag: Integer read fTag write fTag;
    property Text: WideString read GetTextStr write SetTextStr;
  end;

{-------------------------------------------------------------------------------
  TCCStringsList
-------------------------------------------------------------------------------}
  TCCStringList = class;

  PWideStringItem = ^TWideStringItem;
  TWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  TCCWideStringListSortCompare = function(List: TCCStringList; Index1, Index2: Integer): Integer;

  TCCStringList = class(TCCStrings)
  private
    FList: TList;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    function GetItem(Index: Integer): PWideStringItem;
    procedure Changed; virtual;
    procedure Changing; virtual;
    function GetP(Index: Integer): PWideString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const Value: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: WideString): Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function AddObject(const S: WideString; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: WideString): Integer; override;
    procedure InsertObject(Index: Integer; const S: WideString;
      AObject: TObject); override;
    procedure Sort; virtual;
    function GetDelimitedTextEx(ADelimiter, AQuoteChar: WideChar): WideString;
        override;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

{-------------------------------------------------------------------------------
  TCCFileStream
-------------------------------------------------------------------------------}
  TCCFileStream = class(THandleStream)
  public
    constructor Create(const FileName: WideString; Mode: Word);
    destructor Destroy; override;
  end;

{-------------------------------------------------------------------------------
  TCCThreadStringList
-------------------------------------------------------------------------------}
  TCCThreadStringList = class(TCCStringList)
  protected
    fLock: TRTLCriticalSection;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Lock; virtual;
    procedure Unlock; virtual;
  end;

{==============================================================================}
implementation

{##############################################################################}
// TCCStrings

{-------------------------------------------------------------------------------
  Create an instance of TCCStrings
-------------------------------------------------------------------------------}
constructor TCCStrings.Create;
begin
  inherited Create;
  // FLineSeparator := WideChar($2028);
  {$IFDEF MSWINDOWS}
  FLineSeparator:= WideChar(13) + '' + WideChar(10); // compiler wants it this way
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  FLineSeparator:= WideChar(10);
  {$ENDIF UNIX}
  FNameValueSeparator:= '=';
  FDelimiter:= ',';
  FQuoteChar:= '"';
end;

{-------------------------------------------------------------------------------
  Add
-------------------------------------------------------------------------------}
function TCCStrings.Add(const S: WideString): Integer;
begin
  Result:= AddObject(S, nil);
end;

{-------------------------------------------------------------------------------
  Add Object
-------------------------------------------------------------------------------}
function TCCStrings.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result:= Count;
  InsertObject(Result, S, AObject);
end;

{-------------------------------------------------------------------------------
  Add Strings
-------------------------------------------------------------------------------}
procedure TCCStrings.AddStrings(Strings: TCCStrings);
var
  I: Integer;
begin
  for I:= 0 to Strings.Count - 1 do
  AddObject(Strings.GetP(I)^, Strings.Objects[I]);
end;

{-------------------------------------------------------------------------------
  Add Strings
-------------------------------------------------------------------------------}
procedure TCCStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I:= 0 to Strings.Count - 1 do
  AddObject(Strings.Strings[I], Strings.Objects[I]);
end;

{-------------------------------------------------------------------------------
  Add StringsTo
-------------------------------------------------------------------------------}
procedure TCCStrings.AddStringsTo(Dest: TStrings);
var
  I: Integer;
begin
  for I:= 0 to Count - 1 do
  Dest.AddObject(GetP(I)^, Objects[I]);
end;

{-------------------------------------------------------------------------------
  Append
-------------------------------------------------------------------------------}
procedure TCCStrings.Append(const S: WideString);
begin
  Add(S);
end;

{-------------------------------------------------------------------------------
  Assign
-------------------------------------------------------------------------------}
procedure TCCStrings.Assign(Source: TPersistent);
begin
  if Source is TCCStrings then
  begin
    BeginUpdate;
    try
      Clear;
      FDelimiter:= TCCStrings(Source).FDelimiter;
      FNameValueSeparator:= TCCStrings(Source).FNameValueSeparator;
      FQuoteChar:= TCCStrings(Source).FQuoteChar;
      AddStrings(TCCStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear;
      {$IFDEF RTL190_UP}
      FNameValueSeparator:= TStrings(Source).NameValueSeparator;
      FQuoteChar:= TStrings(Source).QuoteChar;
      FDelimiter:= TStrings(Source).Delimiter;
      {$ELSE ~RTL190_UP}
      {$IFDEF RTL150_UP}
      FNameValueSeparator:= CharToWideChar(TStrings(Source).NameValueSeparator);
      {$ENDIF RTL150_UP}
      FQuoteChar:= CharToWideChar(TStrings(Source).QuoteChar);
      FDelimiter:= CharToWideChar(TStrings(Source).Delimiter);
      {$ENDIF ~RTL190_UP}
      AddStrings(TStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

{-------------------------------------------------------------------------------
  AssignTo
-------------------------------------------------------------------------------}
procedure TCCStrings.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).BeginUpdate;
    try
      TStrings(Dest).Clear;
      {$IFDEF RTL190_UP}
      TStrings(Dest).NameValueSeparator:= NameValueSeparator;
      TStrings(Dest).QuoteChar:= QuoteChar;
      TStrings(Dest).Delimiter:= Delimiter;
      {$ELSE ~RTL190_UP}
      {$IFDEF RTL150_UP}
      TStrings(Dest).NameValueSeparator:= WideCharToChar(NameValueSeparator);
      {$ENDIF RTL150_UP}
      TStrings(Dest).QuoteChar:= WideCharToChar(QuoteChar);
      TStrings(Dest).Delimiter:= WideCharToChar(Delimiter);
      {$ENDIF ~RTL190_UP}
      for I:= 0 to Count - 1 do
      TStrings(Dest).AddObject(GetP(I)^, Objects[I]);
    finally
      TStrings(Dest).EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

{-------------------------------------------------------------------------------
  BeginUpdate
-------------------------------------------------------------------------------}
procedure TCCStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

{-------------------------------------------------------------------------------
  CompareStrings
-------------------------------------------------------------------------------}
function TCCStrings.CompareStrings(const S1, S2: WideString): Integer;
begin
  Result:= ccStrings.WideCompareText(S1, S2);
end;

{-------------------------------------------------------------------------------
  CreateAnsiStringList
-------------------------------------------------------------------------------}
function TCCStrings.CreateAnsiStringList: TStrings;
var
  i: Integer;
begin
  Result:= TStringList.Create;
  try
    Result.BeginUpdate;

    for i:= 0 to Count - 1 do
    Result.AddObject(GetP(i)^, Objects[i]);

    Result.EndUpdate;
  except
    Result.Free;
    raise;
  end;
end;

{-------------------------------------------------------------------------------
  DefineProperties
-------------------------------------------------------------------------------}
procedure TCCStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TCCStrings then
        Result := not Equals(TCCStrings(Filer.Ancestor))
    end
    else
      Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

{-------------------------------------------------------------------------------
  EndUpdate
-------------------------------------------------------------------------------}
procedure TCCStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  SetUpdateState(False);
end;

{-------------------------------------------------------------------------------
  Equals
-------------------------------------------------------------------------------}
function TCCStrings.Equals(Strings: TStrings): Boolean;
var
  i: Integer;
begin
  Result:= False;
  if Strings.Count = Count then
  begin
    for i:= 0 to Count - 1 do
    begin
      if Strings[i] <> PStrings[i]^ then
      Exit;
    end;
    Result:= True;
  end;
end;

{-------------------------------------------------------------------------------
  Equals
-------------------------------------------------------------------------------}
function TCCStrings.Equals(Strings: TCCStrings): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Strings.Count = Count then
  begin
    for i := 0 to Count - 1 do
    begin
      if Strings[i] <> PStrings[i]^ then
      Exit;
    end;
    Result:= True;
  end;
end;

{-------------------------------------------------------------------------------
  Exchange
-------------------------------------------------------------------------------}
procedure TCCStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString:= PStrings[Index1]^;
    TempObject:= Objects[Index1];
    PStrings[Index1]^:= PStrings[Index2]^;
    Objects[Index1]:= Objects[Index2];
    PStrings[Index2]^:= TempString;
    Objects[Index2]:= TempObject;
  finally
    EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  ExtractName
-------------------------------------------------------------------------------}
function TCCStrings.ExtractName(const S: WideString): WideString;
var
  Index: Integer;
begin
  Result:= S;
  Index:= WidePos(NameValueSeparator, Result);
  if Index <> 0 then
  SetLength(Result, Index - 1)
  else
  SetLength(Result, 0);
end;

{-------------------------------------------------------------------------------
  Get
-------------------------------------------------------------------------------}
function TCCStrings.Get(Index: Integer): WideString;
begin
  Result:= GetP(Index)^;
end;

{-------------------------------------------------------------------------------
  GetCapacity
-------------------------------------------------------------------------------}
function TCCStrings.GetCapacity: Integer;
begin
  Result:= Count;
end;

{-------------------------------------------------------------------------------
  GetCommaText
-------------------------------------------------------------------------------}
function TCCStrings.GetCommaText: WideString;
begin
  Result:= GetDelimitedTextEx(',', '"');
end;

{-------------------------------------------------------------------------------
  GetDelimitedText
-------------------------------------------------------------------------------}
function TCCStrings.GetDelimitedText: WideString;
begin
  Result:= GetDelimitedTextEx(FDelimiter, FQuoteChar);
end;

{-------------------------------------------------------------------------------
  GetDelimitedTextEx
-------------------------------------------------------------------------------}
function TCCStrings.GetDelimitedTextEx(ADelimiter, AQuoteChar: WideChar): WideString;
var
  S: WideString;
  P: PWideChar;
  I, Num: Integer;
begin
  Num:= GetCount;
  if (Num = 1) and (GetP(0)^ = '') then
  Result := AQuoteChar + '' + AQuoteChar // Compiler wants it this way
  else
  begin
    Result:= '';
    for I:= 0 to Count - 1 do
    begin
      S:= GetP(I)^;
      P:= PWideChar(S);
      while True do
      begin
        case P[0] of
          WideChar(0)..WideChar(32): Inc(P);
        else
          if (P[0] = AQuoteChar) or (P[0] = ADelimiter) then
          Inc(P)
          else
          Break;
        end;
      end;
      
      if P[0] <> WideChar(0) then
      S:= WideQuotedStr(S, AQuoteChar);

      Result:= Result + S + ADelimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

{-------------------------------------------------------------------------------
  GetName
-------------------------------------------------------------------------------}
function TCCStrings.GetName(Index: Integer): WideString;
var
  I: Integer;
begin
  Result:= GetP(Index)^;
  I:= WidePos(FNameValueSeparator, Result);
  if I > 0 then
  SetLength(Result, I - 1);
end;

{-------------------------------------------------------------------------------
  GetObject
-------------------------------------------------------------------------------}
function TCCStrings.GetObject(Index: Integer): TObject;
begin
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  GetText
-------------------------------------------------------------------------------}
function TCCStrings.GetText: PWideChar;
begin
  Result:= WideStrNew(GetTextStr);
end;

{-------------------------------------------------------------------------------
  GetTextStr
-------------------------------------------------------------------------------}
function TCCStrings.GetTextStr: WideString;
var
  I: Integer;
  Len, LL: Integer;
  P: PWideChar;
  W: PWideString;
begin
  Len:= 0;
  LL:= Length(LineSeparator);

  for I:= 0 to Count - 1 do
  Inc(Len, Length(GetP(I)^) + LL);

  SetLength(Result, Len);
  P:= PWideChar(Result);
  for I:= 0 to Count - 1 do
  begin
    W:= GetP(I);
    Len:= Length(W^);
    if Len > 0 then
    begin
      MoveWideChar(W^[1], P[0], Len);
      Inc(P, Len);
    end;
    if LL > 0 then
    begin
      MoveWideChar(FLineSeparator[1], P[0], LL);
      Inc(P, LL);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  GetValue
-------------------------------------------------------------------------------}
function TCCStrings.GetValue(const Name: WideString): WideString;
var
  Index: Integer;
begin
  Index:= IndexOfName(Name);
  if Index >= 0 then
  Result:= GetValueFromIndex(Index)
  else
  Result:= '';
end;

{-------------------------------------------------------------------------------
  GetValueFromIndex
-------------------------------------------------------------------------------}
function TCCStrings.GetValueFromIndex(Index: Integer): WideString;
var
  I: Integer;
begin
  Result:= GetP(Index)^;
  I:= WidePos(FNameValueSeparator, Result);
  if I > 0 then
  System.Delete(Result, 1, I)
  else
  Result := '';
end;

{-------------------------------------------------------------------------------
  IndexOf
-------------------------------------------------------------------------------}
function TCCStrings.IndexOf(const S: WideString): Integer;
begin
  for Result:= 0 to Count - 1 do
  begin
    if CompareStrings(GetP(Result)^, S) = 0 then
    Exit;
  end;
  Result:= -1;
end;

{-------------------------------------------------------------------------------
  IndexOfName
-------------------------------------------------------------------------------}
function TCCStrings.IndexOfName(const Name: WideString): Integer;
begin
  for Result:= 0 to Count - 1 do
  begin
    if CompareStrings(Names[Result], Name) = 0 then
    Exit;
  end;
  Result := -1;
end;

{-------------------------------------------------------------------------------
  IndexOfObject
-------------------------------------------------------------------------------}
function TCCStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result:= 0 to Count - 1 do
  begin
    if Objects[Result] = AObject then
    Exit;
  end;
  Result:= -1;
end;

{-------------------------------------------------------------------------------
  Insert
-------------------------------------------------------------------------------}
procedure TCCStrings.Insert(Index: Integer; const S: WideString);
begin
  InsertObject(Index, S, nil);
end;

{-------------------------------------------------------------------------------
  InsertObject
-------------------------------------------------------------------------------}
procedure TCCStrings.InsertObject(Index: Integer; const S: WideString; AObject: TObject);
begin
end;

{-------------------------------------------------------------------------------
  LoadFromFile
-------------------------------------------------------------------------------}
procedure TCCStrings.LoadFromFile(const FileName: TWideFileName;
    WideFileOptions: TWideFileOptions = []);
var
  Stream: TCCFileStream;
begin
  Stream:= TCCFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, WideFileOptions);
  finally
    Stream.Free;
  end;
end;

{-------------------------------------------------------------------------------
  LoadFromStream
-------------------------------------------------------------------------------}
procedure TCCStrings.LoadFromStream(Stream: TStream;
  WideFileOptions: TWideFileOptions = []);
var
  AnsiS: AnsiString;
  WideS: WideString;
  WC: WideChar;
begin
  BeginUpdate;
  try
    Clear;
    WC:= #0;
    Stream.Read(WC, SizeOf(WC));
    if (foAnsiFile in WideFileOptions) and (Hi(Word(WC)) <> 0) and (WC <> WideBOM_LSB_First) and (WC <> WideBOM_MSB_First) then
    begin
      Stream.Seek(-SizeOf(WC), soFromCurrent);
      SetLength(AnsiS, (Stream.Size - Stream.Position) div SizeOf(AnsiChar));
      Stream.Read(AnsiS[1], Length(AnsiS) * SizeOf(AnsiChar));
      if foUTF8Encoding in WideFileOptions then
      SetTextStr(UTF8Decode(AnsiS))
      else
      SetTextStr(WideString(AnsiS)); // explicit Unicode conversion
    end
    else
    begin
      if (WC <> WideBOM_LSB_First) and (WC <> WideBOM_MSB_First) then
      Stream.Seek(-SizeOf(WC), soFromCurrent);
      SetLength(WideS, (Stream.Size - Stream.Position + 1) div SizeOf(WideChar));
      Stream.Read(WideS[1], Length(WideS) * SizeOf(WideChar));
      if WC = WideBOM_MSB_First then
      SwapWordByteOrder(PWideChar(WideS), Length(WideS));
      SetTextStr(WideS);
    end;
  finally
    EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Move
-------------------------------------------------------------------------------}
procedure TCCStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString:= GetP(CurIndex)^;
      TempObject:= GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  ReadData
-------------------------------------------------------------------------------}
procedure TCCStrings.ReadData(Reader: TReader);
begin
  BeginUpdate;
  try
    Clear;
    Reader.ReadListBegin;
    while not Reader.EndOfList do
    begin
      if Reader.NextValue in [vaLString, vaString] then
      Add(Reader.ReadString)
      else
      Add(Reader.ReadWideString);
    end;
    Reader.ReadListEnd;
  finally
    EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  SaveToFile
-------------------------------------------------------------------------------}
procedure TCCStrings.SaveToFile(const FileName: TWideFileName;
    WideFileOptions: TWideFileOptions = []);
var
  Stream: TCCFileStream;
begin
  Stream:= TCCFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, WideFileOptions);
  finally
    Stream.Free;
  end;
end;

{-------------------------------------------------------------------------------
  SaveToStream
-------------------------------------------------------------------------------}
procedure TCCStrings.SaveToStream(Stream: TStream; WideFileOptions: TWideFileOptions = []);
var
  AnsiS: AnsiString;
  WideS: WideString;
  WC: WideChar;
begin
  if foAnsiFile in WideFileOptions then
  begin
    if foUTF8Encoding in WideFileOptions then
    AnsiS:= UTF8Encode(GetTextStr)
    else
    AnsiS:= AnsiString(GetTextStr); // explicit Unicode conversion
    Stream.Write(AnsiS[1], Length(AnsiS) * SizeOf(AnsiChar));
  end
  else
  begin
    if foUnicodeLB in WideFileOptions then
    begin
      WC:= WideBOM_LSB_First;
      Stream.Write(WC, SizeOf(WC));
    end;
    WideS:= GetTextStr;
    Stream.Write(WideS[1], Length(WideS) * SizeOf(WideChar));
  end;
end;

{-------------------------------------------------------------------------------
  SetCapacity
-------------------------------------------------------------------------------}
procedure TCCStrings.SetCapacity(NewCapacity: Integer);
begin
end;

{-------------------------------------------------------------------------------
  SetCommaText
-------------------------------------------------------------------------------}
procedure TCCStrings.SetCommaText(const Value: WideString);
begin
  SetDelimitedTextEx(',', '"', Value);
end;

{-------------------------------------------------------------------------------
  SetDelimitedText
-------------------------------------------------------------------------------}
procedure TCCStrings.SetDelimitedText(const Value: WideString);
begin
  SetDelimitedTextEx(Delimiter, QuoteChar, Value);
end;

{-------------------------------------------------------------------------------
  SetDelimitedTextEx
-------------------------------------------------------------------------------}
procedure TCCStrings.SetDelimitedTextEx(ADelimiter, AQuoteChar: WideChar;
  const Value: WideString);
var
  P, P1: PWideChar;
  S: WideString;

  procedure IgnoreWhiteSpace(var P: PWideChar);
  begin
    while True do
    begin
      case P^ of
        WideChar(1)..WideChar(32): Inc(P);
      else
        Break;
      end;
    end;
  end;

begin
  BeginUpdate;
  try
    Clear;
    P:= PWideChar(Value);
    IgnoreWhiteSpace(P);
    while P[0] <> WideChar(0) do
    begin
      if P[0] = AQuoteChar then
        S := WideExtractQuotedStr(P, AQuoteChar)
      else
      begin
        P1 := P;
        while (P[0] > WideChar(32)) and (P[0] <> ADelimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);

      IgnoreWhiteSpace(P);
      if P[0] = ADelimiter then
      begin
        Inc(P);
        IgnoreWhiteSpace(P);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Set Text
-------------------------------------------------------------------------------}
procedure TCCStrings.SetText(Text: PWideChar);
begin
  SetTextStr(Text);
end;

{-------------------------------------------------------------------------------
  Set TextStr
-------------------------------------------------------------------------------}
procedure TCCStrings.SetTextStr(const Value: WideString);
var
  P, Start: PWideChar;
  S: WideString;
  Len: Integer;
begin
  BeginUpdate;
  try
    Clear;
    if Value <> '' then
    begin
      P:= PWideChar(Value);
      if P <> nil then
      begin
        while P[0] <> WideChar(0) do
        begin
          Start:= P;
          while True do
          begin
            case P[0] of
              WideChar(0), WideChar(10), WideChar(13): Break;
            end;
            Inc(P);
          end;
          Len:= P - Start;
          if Len > 0 then
          begin
            SetString(S, Start, Len);
            AddObject(S, nil); // consumes most time
          end
          else
          AddObject('', nil);
          
          if P[0] = WideChar(13) then
          Inc(P);
          if P[0] = WideChar(10) then
          Inc(P);
        end;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  Set UpdateState
-------------------------------------------------------------------------------}
procedure TCCStrings.SetUpdateState(Updating: Boolean);
begin
end;

{-------------------------------------------------------------------------------
  Set Value
-------------------------------------------------------------------------------}
procedure TCCStrings.SetValue(const Name, Value: WideString);
var
  Index: Integer;
begin
  Index:= IndexOfName(Name);
  if Index >= 0 then
  SetValueFromIndex(Index, Value)
  else
  begin
    if Value <> '' then
    Add(Name + NameValueSeparator + Value);
  end;
end;

{-------------------------------------------------------------------------------
  Set ValueFromIndex
-------------------------------------------------------------------------------}
procedure TCCStrings.SetValueFromIndex(Index: Integer; const Value: WideString);
var
  S: WideString;
  I: Integer;
begin
  if Value = '' then
    Delete(Index)
  else
  begin
    if Index < 0 then
      Index:= Add('');
    S:= GetP(Index)^;
    I:= WidePos(NameValueSeparator, S);
    if I > 0 then
      System.Delete(S, I, MaxInt);
    S:= S + NameValueSeparator + Value;
    Put(Index, S);
  end;
end;

{-------------------------------------------------------------------------------
  WriteData
-------------------------------------------------------------------------------}
procedure TCCStrings.WriteData(Writer: TWriter);
var
  i: Integer;
begin
  Writer.WriteListBegin;
  for i:= 0 to Count - 1 do
  begin
    Writer.WriteWideString(GetP(i)^);
  end;
  Writer.WriteListEnd;
end;

{##############################################################################}
// TCCStringList

{-------------------------------------------------------------------------------
  Create an instance of TCCStringList
-------------------------------------------------------------------------------}
constructor TCCStringList.Create;
begin
  inherited Create;
  FList:= TList.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCCStringList
-------------------------------------------------------------------------------}
destructor TCCStringList.Destroy;
begin
  FOnChange:= nil;
  FOnChanging:= nil;
  FUpdateCount:= FUpdateCount + 1;
  Clear;
  FList.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Add Object
-------------------------------------------------------------------------------}
function TCCStringList.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result:= Count
  else if Duplicates <> dupAccept then
  begin
    if Find(S, Result) then
    begin
      case Duplicates of
        dupIgnore:
          Exit;
        dupError:
          raise EListError.CreateRes(@SDuplicateString);
      end;
    end;
  end;
  InsertObject(Result, S, AObject);
end;

{-------------------------------------------------------------------------------
  Changed
-------------------------------------------------------------------------------}
procedure TCCStringList.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{-------------------------------------------------------------------------------
  Changing
-------------------------------------------------------------------------------}
procedure TCCStringList.Changing;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCCStringList.Clear;
var
  i: Integer;
  Item: PWideStringItem;
begin
  if FUpdateCount = 0 then
  Changing;

  for i:= 0 to Count - 1 do
  begin
    Item:= PWideStringItem(FList[i]);
    Item.FString:= '';
    FreeMem(Item);
  end;
  FList.Clear;

  if FUpdateCount = 0 then
  Changed;
end;

{-------------------------------------------------------------------------------
  CompareStrings
-------------------------------------------------------------------------------}
function TCCStringList.CompareStrings(const S1, S2: WideString): Integer;
begin
  if CaseSensitive then
    Result:= ccStrings.WideCompareStr(S1, S2)
  else
    Result:= ccStrings.WideCompareText(S1, S2);
end;

{-------------------------------------------------------------------------------
  Delete
-------------------------------------------------------------------------------}
procedure TCCStringList.Delete(Index: Integer);
var
  Item: PWideStringItem;
begin
  if FUpdateCount = 0 then
  Changing;
  
  Item:= PWideStringItem(FList[Index]);
  FList.Delete(Index);
  Item.FString:= '';
  FreeMem(Item);

  if FUpdateCount = 0 then
  Changed;
end;

{-------------------------------------------------------------------------------
  Exchange
-------------------------------------------------------------------------------}
procedure TCCStringList.Exchange(Index1, Index2: Integer);
begin
  if FUpdateCount = 0 then
  Changing;
  
  FList.Exchange(Index1, Index2);

  if FUpdateCount = 0 then
  Changed;
end;

{-------------------------------------------------------------------------------
  Find
-------------------------------------------------------------------------------}
function TCCStringList.Find(const S: WideString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result:= False;
  if Sorted then
  begin
    L:= 0;
    H:= Count - 1;
    while L <= H do
    begin
      I:= (L + H) shr 1;
      C:= CompareStrings(GetItem(I).FString, S);
      if C < 0 then
        L:= I + 1
      else
      begin
        H:= I - 1;
        if C = 0 then
        begin
          Result:= True;
          if Duplicates <> dupAccept then
            L:= I;
        end;
      end;
    end;
    Index:= L;
  end
  else
  begin
    Index:= IndexOf(S);
    Result:= Index <> -1;
  end;
end;

{-------------------------------------------------------------------------------
  GetCapacity
-------------------------------------------------------------------------------}
function TCCStringList.GetCapacity: Integer;
begin
  Result:= FList.Capacity;
end;

{-------------------------------------------------------------------------------
  GetCount
-------------------------------------------------------------------------------}
function TCCStringList.GetCount: Integer;
begin
  Result:= FList.Count;
end;

{-------------------------------------------------------------------------------
  GetItem
-------------------------------------------------------------------------------}
function TCCStringList.GetItem(Index: Integer): PWideStringItem;
begin
  Result:= FList[Index];
end;

{-------------------------------------------------------------------------------
  GetObject
-------------------------------------------------------------------------------}
function TCCStringList.GetObject(Index: Integer): TObject;
begin
  Result:= GetItem(Index).FObject;
end;

{-------------------------------------------------------------------------------
  GetP
-------------------------------------------------------------------------------}
function TCCStringList.GetP(Index: Integer): PWideString;
begin
  Result:= Addr(GetItem(Index).FString);
end;

{-------------------------------------------------------------------------------
  IndexOf
-------------------------------------------------------------------------------}
function TCCStringList.IndexOf(const S: WideString): Integer;
begin
  if Sorted then
  begin
    Result:= -1;
    if not Find(S, Result) then
    Result:= -1;
  end
  else
  begin
    for Result:= 0 to Count - 1 do
    begin
      if CompareStrings(GetItem(Result).FString, S) = 0 then
      Exit;
    end;
    Result:= -1;
  end;
end;

{-------------------------------------------------------------------------------
  InsertObject
-------------------------------------------------------------------------------}
procedure TCCStringList.InsertObject(Index: Integer; const S: WideString;
  AObject: TObject);
var
  P: PWideStringItem;
begin
  if FUpdateCount = 0 then
  Changing;

  FList.Insert(Index, nil); // error check
  P:= AllocMem(SizeOf(TWideStringItem));
  FList[Index]:= P;

  Put(Index, S);
  if AObject <> nil then
  PutObject(Index, AObject);
  
  if FUpdateCount = 0 then
  Changed;
end;

{-------------------------------------------------------------------------------
  Put
-------------------------------------------------------------------------------}
procedure TCCStringList.Put(Index: Integer; const Value: WideString);
begin
  if FUpdateCount = 0 then
  Changing;

  GetItem(Index).FString:= Value;

  if FUpdateCount = 0 then
  Changed;
end;

{-------------------------------------------------------------------------------
  Put Object
-------------------------------------------------------------------------------}
procedure TCCStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if FUpdateCount = 0 then
  Changing;

  GetItem(Index).FObject:= AObject;

  if FUpdateCount = 0 then
  Changed;
end;

{-------------------------------------------------------------------------------
  Set Capacity
-------------------------------------------------------------------------------}
procedure TCCStringList.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity:= NewCapacity;
end;

{-------------------------------------------------------------------------------
  Set CaseSensitive
-------------------------------------------------------------------------------}
procedure TCCStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive:= Value;
    if Sorted then
    begin
      Sorted:= False;
      Sorted:= True; // re-sort
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set Sorted
-------------------------------------------------------------------------------}
procedure TCCStringList.SetSorted(Value: Boolean);
begin
  if Value <> FSorted then
  begin
    FSorted:= Value;
    if FSorted then
    begin
      FSorted:= False;
      Sort;
      FSorted:= True;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set UpdateState
-------------------------------------------------------------------------------}
procedure TCCStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
  Changing
  else
  Changed;
end;

{-------------------------------------------------------------------------------
  GetDelimitedTextEx
-------------------------------------------------------------------------------}
function TCCStringList.GetDelimitedTextEx(ADelimiter, AQuoteChar:
    WideChar): WideString;
var
  S: WideString;
  P: PWideChar;
  I, Num: Integer;
  {$IFDEF MSWINDOWS}
  list: TCCStringList;
  {$ENDIF}
begin
  Num:= GetCount;
  if (Num = 1) and (GetP(0)^ = '') then
  Result := AQuoteChar + '' + AQuoteChar // Compiler wants it this way
  else
  begin
    {$IFDEF MSWINDOWS}
    list:= TCCStringList.Create;
    try
    {$ENDIF}
      Result:= '';
      for I:= 0 to Count - 1 do
      begin
        S:= GetP(I)^;
        P:= PWideChar(S);
        while True do
        begin
          case P[0] of
            WideChar(0)..WideChar(32): Inc(P);
          else
            if (P[0] = AQuoteChar) or (P[0] = ADelimiter) then
            Inc(P)
            else
            Break;
          end;
        end;

        if P[0] <> WideChar(0) then
        S:= WideQuotedStr(S, AQuoteChar);

        {$IFDEF MSWINDOWS}
        list.Add(S + ADelimiter); // This is faster than concatenating with + sign.
        {$ELSE}
        Result:= Result + S + ADelimiter; // slow in Windows
        {$ENDIF}
      end;
    {$IFDEF MSWINDOWS}
    finally
      Result:= list.Text;
      list.Free;
    end;
    {$ENDIF}
    System.Delete(Result, Length(Result), 1);
  end;
end;

{-------------------------------------------------------------------------------
  Sort
-------------------------------------------------------------------------------}
  threadvar
    CustomSortList: TCCStringList;

  function DefaultSort(Item1, Item2: Pointer): Integer;
  begin
    Result:= CustomSortList.CompareStrings(PWideStringItem(Item1).FString,
                                           PWideStringItem(Item2).FString)
  end;

procedure TCCStringList.Sort;
var
  TempList: TCCStringList;
begin
  TempList:= CustomSortList;
  CustomSortList:= Self;
  try
    Changing;
    FList.Sort(DefaultSort);
    Changed;
  finally
    CustomSortList:= TempList;
  end;
end;

{##############################################################################}
// TCCFileStream

{-------------------------------------------------------------------------------
  Create an instance of TCCFileStream
-------------------------------------------------------------------------------}
constructor TCCFileStream.Create(const FileName: WideString; Mode: Word);
var
  CreateHandle: Integer;
  {$IFDEF DELPHI_7_UP}
  ErrorMessage: WideString;
  {$ENDIF}
begin
  if Mode = fmCreate then
  begin
    CreateHandle:= WideFileCreate(FileName);
    if CreateHandle < 0 then
    begin
      {$IFDEF DELPHI_7_UP}
      ErrorMessage:= WideSysErrorMessage(GetLastError);
      raise EFCreateError.CreateFmt(SFCreateErrorEx, [WideExpandFileName(FileName), ErrorMessage]);
      {$ELSE}
      raise EFCreateError.CreateFmt(SFCreateError, [WideExpandFileName(FileName)]);
      {$ENDIF}
    end;
  end
  else
  begin
    CreateHandle:= WideFileOpen(FileName, Mode);
    if CreateHandle < 0 then
    begin
      {$IFDEF DELPHI_7_UP}
      ErrorMessage:= WideSysErrorMessage(GetLastError);
      raise EFOpenError.CreateFmt(SFOpenErrorEx, [WideExpandFileName(FileName), ErrorMessage]);
      {$ELSE}
      raise EFOpenError.CreateFmt(SFOpenError, [WideExpandFileName(FileName)]);
      {$ENDIF}
    end;
  end;
  inherited Create(CreateHandle);
end;

{-------------------------------------------------------------------------------
  Destroy TCCFileStream
-------------------------------------------------------------------------------}
destructor TCCFileStream.Destroy;
begin
  if Handle >= 0 then
  FileClose(Handle);
end;

{##############################################################################}
// TCCThreadStringList

{-------------------------------------------------------------------------------
  Create an instance of TCCThreadStringList
-------------------------------------------------------------------------------}
constructor TCCThreadStringList.Create;
begin
  inherited Create;
  CreateCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  Destroy TCCThreadStringList
-------------------------------------------------------------------------------}
destructor TCCThreadStringList.Destroy;
begin
  DestroyCriticalSection(fLock);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Lock
-------------------------------------------------------------------------------}
procedure TCCThreadStringList.Lock;
begin
  EnterCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  Unlock
-------------------------------------------------------------------------------}
procedure TCCThreadStringList.Unlock;
begin
  LeaveCriticalSection(fLock);
end;

end.

