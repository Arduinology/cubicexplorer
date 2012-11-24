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
//  The Original Code is ccLog.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccLog;

interface

{$INCLUDE Core.inc}

uses
  // CubicCore
  ccContainers, ccThreads,
  // Fundamentals
  cTimers,
  // System Units
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes;

const
  IID_ICCLog: TGUID = '{DC06061D-FB9B-45E6-A489-B44D7B5A8EC8}';
  IID_ICCLogListener: TGUID = '{D0D25D86-1013-42FE-A0DD-DC268634572D}';

  // Log Notify (LNOTIFY)
  LNOTIFY_ADD = 0;
  LNOTIFY_CLEAR = 1;

  // Msg type (MTYPE)
  MTYPE_TICK    = -1;
  MTYPE_NORMAL  = 0;
  MTYPE_HINT    = 1;
  MTYPE_WARNING = 2;
  MTYPE_ERROR   = 3;

  MTYPE_STRS: array [MTYPE_TICK..MTYPE_ERROR] of string = ('Tick', 'Normal', 'Hint', 'Warning', 'Error');
  
type
  ICCLogListener = interface;

{-------------------------------------------------------------------------------
  ICCLog
-------------------------------------------------------------------------------}
  PLogItem = ^ALogItem;
  ALogItem = record
    ATick: Int64;
    ATime: TDateTime;
    AType: Integer;
    ASender: Pointer;
    AMsg: WideString;
  end;

  ICCLog = interface(IInterface)
  ['{DC06061D-FB9B-45E6-A489-B44D7B5A8EC8}']
    // Clear
    // - Clear all log messages.
    procedure Clear; stdcall;

    // GetCount
    // - Get log message count.
    function GetCount: Integer; stdcall;

    // GetEnabled
    // - Get log's enabled state.
    function GetEnabled: Boolean; stdcall;

    // GetItem
    // - Get log item from specified index.
    // - Returns false if AIndex is invalid and item was not found, else true.
    function GetItem(AIndex: Integer; out AItem: ALogItem): Boolean; stdcall;

    // Log
    // - Add new log message.
    // - Returns the tick of added msg.
    // - If AMsgType = MTYPE_TICK, no message will be added, only tick is returned.
    function Log(const AMsg: WideString; AMsgType: Integer; ASender: Pointer):
        Int64; stdcall;

    // RegisterListener
    // - Register listener.
    procedure RegisterListener(AListener: ICCLogListener); stdcall;
    
    // SetEnabled
    // - Set log's enabled state.
    // - If Value = false, the log will ignore Log calls.
    procedure SetEnabled(const Value: Boolean); stdcall;

    // UnregisterListerner
    // - Unregister listener.
    procedure UnregisterListener(AListener: ICCLogListener); stdcall;
  end;

{-------------------------------------------------------------------------------
  ICCLogListener
-------------------------------------------------------------------------------}


  ICCLogListener = interface(IInterface)
  ['{D0D25D86-1013-42FE-A0DD-DC268634572D}']
    // LogNotify
    // - This get's called on log change.
    // - ANotify specifies the type of change.
    // - On LNOTIFY_ADD, AMsg will contain the added message.
    // - On LNOTIFY_CLEAR, AMsg will contain a 'Log cleared' message.
    // - This method is called from main thread!
    procedure LogNotify(const ALog: ICCLog; const ANotify: Cardinal; const AMsg:
        ALogItem); stdcall;
  end;

{-------------------------------------------------------------------------------
  TCCLog
-------------------------------------------------------------------------------}
  TCCLog = class(TInterfacedObject, ICCLog)
  protected
    fEnabled: Boolean;
    fItems: TCCThreadList;
    fListeners: TInterfaceList;
    fNotify: Cardinal;
    fNotifyItem: ALogItem;
    procedure DoFreeItem(ASender: TCCList; AItem: PCCListItem); virtual;
    procedure DoSendNotify; virtual;
    function GetCount: Integer; virtual; stdcall;
    function GetEnabled: Boolean; virtual; stdcall;
    procedure SetEnabled(const Value: Boolean); virtual; stdcall;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual; stdcall;
    function GetItem(AIndex: Integer; out AItem: ALogItem): Boolean; virtual; stdcall;
    function Log(const AMsg: WideString; AMsgType: Integer; ASender: Pointer):
        Int64; virtual; stdcall;
    procedure RegisterListener(AListener: ICCLogListener); virtual; stdcall;
    procedure UnregisterListener(AListener: ICCLogListener); virtual; stdcall;
    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

{-------------------------------------------------------------------------------
  Public methods
-------------------------------------------------------------------------------}
  function GlobalLog: ICCLog;
  function GlobalLogMsg(const AMsg: WideString; AMsgType: Integer = 0; ASender:
    Pointer = nil): Int64;

implementation

var
  fGlobalLog: ICCLog;
  fGlobalLogLock: TRTLCriticalSection;

{##############################################################################}
// Public Methods

{-------------------------------------------------------------------------------
  GlobalLog
-------------------------------------------------------------------------------}
function GlobalLog: ICCLog;
begin
  EnterCriticalSection(fGlobalLogLock);
  try
    if not assigned(fGlobalLog) then
    fGlobalLog:= TCCLog.Create;
  finally
    LeaveCriticalSection(fGlobalLogLock);
  end;
  Result:= fGlobalLog;
end;

{-------------------------------------------------------------------------------
  GlobalLogMsg
  - Add message to global log.
  - Returns tick.
-------------------------------------------------------------------------------}
function GlobalLogMsg(const AMsg: WideString; AMsgType: Integer = 0; ASender:
    Pointer = nil): Int64;
begin
  Result:= GlobalLog.Log(AMsg, AMsgType, ASender);
end;

{##############################################################################}
// TCCLog

{-------------------------------------------------------------------------------
  Create an instance of TCCLog
-------------------------------------------------------------------------------}
constructor TCCLog.Create;
begin
  inherited Create;
  // create instances
  fItems:= TCCThreadList.Create;
  fItems.ItemDataSize:= SizeOf(ALogItem);
  fItems.OnFreeItem:= DoFreeItem;
  fItems.AutoLock:= false; // we do the locking ourselves.

  fListeners:= TInterfaceList.Create;
  // initialize values
  fEnabled:= true;
end;

{-------------------------------------------------------------------------------
  Destroy TCCLog
-------------------------------------------------------------------------------}
destructor TCCLog.Destroy;
begin
  // free instances
  fItems.Free;
  fListeners.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCCLog.Clear;
begin
  fItems.Lock;
  try
    fItems.Clear;
  finally
    fItems.UnLock;
  end;
  // notify
  fNotify:= LNOTIFY_CLEAR;
  StartTimer(fNotifyItem.ATick);
  fNotifyItem.ATime:= Now;
  fNotifyItem.AType:= MTYPE_NORMAL;
  fNotifyItem.ASender:= nil;
  fNotifyItem.AMsg:= 'Log cleared';
  TThread.Synchronize(nil, DoSendNotify);
end;

{-------------------------------------------------------------------------------
  Do Free Item
-------------------------------------------------------------------------------}
procedure TCCLog.DoFreeItem(ASender: TCCList; AItem: PCCListItem);
var
  data: PLogItem;
begin
  data:= ASender.GetItemData(AItem);
  data.AMsg:= '';
end;

{-------------------------------------------------------------------------------
  DoSendNotify
-------------------------------------------------------------------------------}
procedure TCCLog.DoSendNotify;
var
  i: Integer;
begin
  for i:= 0 to fListeners.Count - 1 do
  begin
    try
      (fListeners.Items[i] as ICCLogListener).LogNotify(Self, fNotify, fNotifyItem);
    except
      // catch exceptions
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get Count
-------------------------------------------------------------------------------}
function TCCLog.GetCount: Integer;
begin
  fItems.Lock;
  try
    Result:= fItems.Count;
  finally
    fItems.UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Get Enabled
-------------------------------------------------------------------------------}
function TCCLog.GetEnabled: Boolean;
begin
  fItems.Lock;
  try
    Result:= fEnabled;
  finally
    fItems.UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Get Item
  - Return true if item was found, else false.
-------------------------------------------------------------------------------}
function TCCLog.GetItem(AIndex: Integer; out AItem: ALogItem): Boolean;
var
  data: PLogItem;
begin
  fItems.Lock;
  try
    Result:= (AIndex >= 0) and (AIndex < fItems.Count);
    if Result then
    begin
      data:= fItems.GetItemData(fItems.Item[AIndex]);
      AItem.ATick:= data.ATick;
      AItem.ATime:= data.ATime;
      AItem.AMsg:= data.AMsg;
    end
    else
    begin
      AItem.ATick:= 0;
      AItem.ATime:= 0;
      AItem.AMsg:= '';
    end;
  finally
    fItems.UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Log
-------------------------------------------------------------------------------}
function TCCLog.Log(const AMsg: WideString; AMsgType: Integer; ASender:
    Pointer): Int64;
var
  data: PLogItem;
begin
  if AMsgType = MTYPE_TICK then
  begin
    StartTimer(Result);
    Exit;
  end;

  fItems.Lock;
  try
    if fEnabled then
    begin
      data:= fItems.GetItemData(fItems.AddItem);
      data.AMsg:= AMsg;
      data.ATime:= Now;
      data.AType:= AMsgType;
      data.ASender:= ASender;
      StartTimer(data.ATick);
      Result:= data.ATick;
    end;
  finally
    fItems.UnLock;
  end;
  // notify
  if fEnabled then
  begin
    fNotify:= LNOTIFY_ADD;
    fNotifyItem.ATick:= data.ATick;
    fNotifyItem.ATime:= data.ATime;
    fNotifyItem.AType:= data.AType;
    fNotifyItem.ASender:= data.ASender;
    fNotifyItem.AMsg:= data.AMsg;
    TThread.Synchronize(nil, DoSendNotify);
  end;
end;

{-------------------------------------------------------------------------------
  Register Listener
-------------------------------------------------------------------------------}
procedure TCCLog.RegisterListener(AListener: ICCLogListener);
begin
  if fListeners.IndexOf(AListener) < 0 then
  fListeners.Add(AListener);
end;

{-------------------------------------------------------------------------------
  Set Enabled
-------------------------------------------------------------------------------}
procedure TCCLog.SetEnabled(const Value: Boolean);
begin
  fItems.Lock;
  try
    fEnabled:= Value;
  finally
    fItems.UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Unregister Listener
-------------------------------------------------------------------------------}
procedure TCCLog.UnregisterListener(AListener: ICCLogListener);
begin
  fListeners.Remove(AListener);
end;

{==============================================================================}
initialization
  // Create GlobalLog lock
  CreateCriticalSection(fGlobalLogLock);

finalization
  // Free GlobalLog
  EnterCriticalSection(fGlobalLogLock);
  try
    if assigned(fGlobalLog) then
    begin
      fGlobalLog:= nil;  
    end;
  finally
    LeaveCriticalSection(fGlobalLogLock);
  end;
  DestroyCriticalSection(fGlobalLogLock);
end.
