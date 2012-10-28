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
//  The Original Code is ccThreads.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccThreads;

{$INCLUDE Core.inc}

{==============================================================================}
interface

uses
  // System Units
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes;

{==============================================================================}
const
  IID_ICCThreadLock: TGUID = '{5FDB53A2-2B69-43C7-886E-F64D13A3A790}';

  INFINITE = DWORD($FFFFFFFF);
  
type
  {$IFDEF MSWindows}
  TCCSignal = THandle;
  {$ELSE}
  TCCSignal = PRtlEvent;
  {$ENDIF}

{-------------------------------------------------------------------------------
  ICCThreadLock
-------------------------------------------------------------------------------}
  ICCThreadLock = interface(IInterface)
  ['{5FDB53A2-2B69-43C7-886E-F64D13A3A790}']
    procedure Lock; stdcall;
    procedure UnLock; stdcall;
  end;

{-------------------------------------------------------------------------------
  TCCBaseThread
-------------------------------------------------------------------------------}
  TCCBaseThread = class;
  
  TOnExecuteEvent = procedure(Sender: TCCBaseThread) of object;
  TOnMsgEvent = procedure(Sender: TCCBaseThread; Msg: TObject) of object;

  TCCBaseThread = class(TThread)
  private
    fData: TObject;
    fFinished: Boolean;
    fFreeDataOnDestroy: Boolean;
    fMsg: TObject;
    fMsgLock: TRTLCriticalSection;
    fName: AnsiString;
    fOnExecute: TOnExecuteEvent;
    fOnSyncedMessage: TOnMsgEvent;
  protected
    procedure DoSyncedMessage; virtual;
    procedure DoTerminate; override;
    procedure Execute; override;
    {$IFDEF DEBUG}{$IFDEF DELPHI}
    procedure SetName; virtual;
    {$ENDIF}{$ENDIF}
  public
    constructor Create(CreateSuspended: Boolean); virtual;
    destructor Destroy; override;
    procedure SendSyncedMessage(AMsg: TObject); virtual;
    property Data: TObject read fData write fData;
    property Finished: Boolean read fFinished;
    property FreeDataOnDestroy: Boolean read fFreeDataOnDestroy write
        fFreeDataOnDestroy;
    property Name: AnsiString read fName write fName;
    property Terminated;
  published
    property OnExecute: TOnExecuteEvent read fOnExecute write fOnExecute;
    property OnSyncedMessage: TOnMsgEvent read fOnSyncedMessage write
        fOnSyncedMessage;
  end;

{-------------------------------------------------------------------------------
  Public Methods
-------------------------------------------------------------------------------}
  function GetCurrentThreadID: Cardinal;
  procedure CreateCriticalSection(var ACriticalSection: TRTLCriticalSection);
  procedure DestroyCriticalSection(var ACriticalSection: TRTLCriticalSection);
  function CreateSignal: TCCSignal;
  procedure DestroySignal(Signal: TCCSignal);
  procedure SetSignal(Signal: TCCSignal);
  procedure ResetSignal(Signal: TCCSignal);
  procedure WaitForSignal(Signal: TCCSignal; AutoReset: Boolean = true; Timeout:
      Cardinal = INFINITE);

{==============================================================================}
implementation

{##############################################################################}
// Public methods

{-------------------------------------------------------------------------------
  GetCurrentThreadID
-------------------------------------------------------------------------------}
function GetCurrentThreadID: Cardinal;
begin
  {$IFDEF MSWindows}
  Result:= GetCurrentThreadID();
  {$ELSE}
  Result:= GetThreadID();
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  Create CriticalSection
-------------------------------------------------------------------------------}
procedure CreateCriticalSection(var ACriticalSection: TRTLCriticalSection);
begin
  {$IFDEF MSWindows}
  InitializeCriticalSection(ACriticalSection);
  {$ELSE}
  InitCriticalSection(ACriticalSection);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  Destroy CriticalSection
-------------------------------------------------------------------------------}
procedure DestroyCriticalSection(var ACriticalSection: TRTLCriticalSection);
begin
  {$IFDEF MSWindows}
  DeleteCriticalSection(ACriticalSection);
  {$ELSE}
  DoneCriticalsection(ACriticalSection);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  CreateSignal
-------------------------------------------------------------------------------}
function CreateSignal: TCCSignal;
begin
  {$IFDEF MSWindows}
  Result:= CreateEvent(nil, true, false, nil);
  {$ELSE}
  Result:= RtlEventCreate;
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  DestroySignal
-------------------------------------------------------------------------------}
procedure DestroySignal(Signal: TCCSignal);
begin
  {$IFDEF MSWindows}
  CloseHandle(Signal);
  {$ELSE}
  RtlEventDestroy(Signal);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  SetSignal
-------------------------------------------------------------------------------}
procedure SetSignal(Signal: TCCSignal);
begin
  {$IFDEF MSWindows}
  SetEvent(Signal);
  {$ELSE}
  RtlEventSetEvent(Signal);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  ResetSignal
-------------------------------------------------------------------------------}
procedure ResetSignal(Signal: TCCSignal);
begin
  {$IFDEF MSWindows}
  ResetEvent(Signal);
  {$ELSE}
  RtlEventResetEvent(Signal);
  {$ENDIF}
end;

{-------------------------------------------------------------------------------
  WaitForSignal (if Timeout = INFINITE, it waits until signal is set)
-------------------------------------------------------------------------------}
procedure WaitForSignal(Signal: TCCSignal; AutoReset: Boolean = true; Timeout:
    Cardinal = INFINITE);
begin
  if Timeout = INFINITE then
  begin
    {$IFDEF MSWindows}
    WaitForSingleObject(Signal, INFINITE);
    {$ELSE}
    RtlEventWaitFor(Signal);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF MSWindows}
    WaitForSingleObject(Signal, Timeout);
    {$ELSE}
    RtlEventWaitFor(Signal, Timeout);
    {$ENDIF} 
  end;
  if AutoReset then
  ResetSignal(Signal);
end;

{##############################################################################}
// TCCBaseThread

{-------------------------------------------------------------------------------
  Create an instance of TCCBaseThread
-------------------------------------------------------------------------------}
constructor TCCBaseThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  fFreeDataOnDestroy:= false;

  // Create MsgLock
  CreateCriticalSection(fMsgLock);
end;

{-------------------------------------------------------------------------------
  Destroy TCCBaseThread
-------------------------------------------------------------------------------}
destructor TCCBaseThread.Destroy;
begin
  if FreeDataOnDestroy and assigned(Data) then
  Data.Free;

  // Free MsgLock
  DestroyCriticalSection(fMsgLock);
  inherited;
end;

{-------------------------------------------------------------------------------
  DoSyncedMessage
-------------------------------------------------------------------------------}
procedure TCCBaseThread.DoSyncedMessage;
begin
  if assigned(fOnSyncedMessage) then
  fOnSyncedMessage(Self, fMsg);
end;

{-------------------------------------------------------------------------------
  DoTerminate
-------------------------------------------------------------------------------}
procedure TCCBaseThread.DoTerminate;
begin
  inherited;
  fFinished:= true;
end;

{-------------------------------------------------------------------------------
  Execute
-------------------------------------------------------------------------------}
procedure TCCBaseThread.Execute;
begin
  {$IFDEF DEBUG}{$IFDEF DELPHI}
  SetName;
  {$ENDIF}{$ENDIF}
  if Assigned(fOnExecute) then fOnExecute(Self);
end;

{-------------------------------------------------------------------------------
  Send SyncedMessage
-------------------------------------------------------------------------------}
procedure TCCBaseThread.SendSyncedMessage(AMsg: TObject);
begin
  // Do locking in case other threads call this method while the message is 
  // being processed.
  EnterCriticalSection(fMsgLock); 
  try
    fMsg:= AMsg;
    Self.Synchronize(DoSyncedMessage);
  finally
    LeaveCriticalSection(fMsgLock);
  end;
end;

{-------------------------------------------------------------------------------
  Set Name (name thread in Delphi's debugger)
-------------------------------------------------------------------------------}
{$IFDEF DEBUG}{$IFDEF DELPHI}
procedure TCCBaseThread.SetName;
type
  TThreadNameInfo = record
    FType: LongWord;      // must be 0x1000
    FName: PAnsiChar;     // pointer to name (in user address space)
    FThreadID: LongWord;  // thread ID (-1 indicates caller thread)
    FFlags: LongWord;     // reserved for future use, must be zero
  end;

var
  ThreadNameInfo: TThreadNameInfo;
begin
  if fName <> '' then
  begin
    ThreadNameInfo.FType:= $1000;
    ThreadNameInfo.FName:= PAnsiChar(fName);
    ThreadNameInfo.FThreadID:= $FFFFFFFF;
    ThreadNameInfo.FFlags:= 0;
    try
      RaiseException($406D1388, 0, SizeOf(ThreadNameInfo) div SizeOf(Longword), @ThreadNameInfo);
    except
    end;
  end;
end;
{$ENDIF}{$ENDIF}



end.
