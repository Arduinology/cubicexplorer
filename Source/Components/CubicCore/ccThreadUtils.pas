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
//  The Original Code is ccThreadUtils.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccThreadUtils;

{$INCLUDE Core.inc}

{==============================================================================}
interface

uses
  // CubicCore
  ccThreads, ccContainers,
  // Fundamentals
  cTimers,
  // System Units
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes;

{==============================================================================}

type
{-------------------------------------------------------------------------------
  TCCTaskPool
-------------------------------------------------------------------------------}
  TCCTaskPool = class;
  TCCTaskPoolThread = class;

  TCCTaskPoolExecuteEvent = procedure(Sender: TCCTaskPoolThread; AObject: TObject; AData: Pointer; ATag:
      Integer) of object;
  TCCTaskEvent = procedure(Sender, AObject: TObject; AData: Pointer; ATag:
      Integer) of object;
  TCCTaskPoolSyncMsgEvent = procedure(Sender: TCCTaskPoolThread; AObject: TObject; AData: Pointer; ATag,
      AMsg, AParam1, AParam2: Integer; var AResult: Integer) of object;

  PCCTaskPoolData = ^TCCTaskPoolData;
  TCCTaskPoolData = record
    fStartTick: Integer;
    fAutoFreeObject: Boolean;
    AObject: TObject;
    AData: Pointer;
    ATag: Integer;
    ATaskDoneEvent: TCCTaskEvent;
    AFreeTaskEvent: TCCTaskEvent;
    ATaskExecuteEvent: TCCTaskPoolExecuteEvent;
    ATaskSyncMsgEvent: TCCTaskPoolSyncMsgEvent;
  end;

  PCCTaskSyncMsg = ^TCCTaskSyncMsg;
  TCCTaskSyncMsg = record
    AMsg: Integer;
    AParam1: Integer;
    AParam2: Integer;
    AResult: Integer;
  end;

  TCCTaskPoolMgrThread = class(TCCBaseThread)
  protected
    fResultData: PCCTaskPoolData;
    fThreads: TThreadList;
    fWaitSignal: TCCSignal;
    procedure DoFreeTask; virtual;
    procedure DoTaskDone; virtual;
    procedure Execute; override;
    function StartNewTask(Data: PCCTaskPoolData): Boolean; virtual;
    procedure Wakeup; virtual;
  public
    Pool: TCCTaskPool;
    constructor Create(CreateSuspended: Boolean); override;
    destructor Destroy; override;
  end;
  
  TCCTaskPoolThread = class(TCCBaseThread)
  protected
    fDataLock: TRTLCriticalSection;
    fSyncMsg: TCCTaskSyncMsg;
    fTaskDone: Boolean;
    fTimeout: Cardinal;
    fWaitSignal: TCCSignal;
    procedure DoSyncMsg; virtual;
    procedure Execute; override;
    procedure Lock; virtual;
    procedure UnLock; virtual;
    procedure Wakeup; virtual;
    property TaskDone: Boolean read fTaskDone write fTaskDone;
    property Timeout: Cardinal read fTimeout write fTimeout;
  public
    Data: TCCTaskPoolData;
    TaskPool: TCCTaskPool;
    constructor Create(CreateSuspended: Boolean); override;
    destructor Destroy; override;
    procedure Abort;
    function IsAborted: Boolean;
    function SendSyncMessage(AMsg, AParam1, AParam2: Integer): Integer; virtual;
  end;

  TCCTaskPool = class(TObject)
  protected
    fKeepAliveTimeout: Integer;
    fLastAbort: Integer;
    fLastTagID: Integer;
    fLock: TRTLCriticalSection;
    fMaxThreadCount: Cardinal;
    fOnFreeTask: TCCTaskEvent;
    fOnTaskDone: TCCTaskEvent;
    fOnTaskExecute: TCCTaskPoolExecuteEvent;
    fOnTaskSyncMsg: TCCTaskPoolSyncMsgEvent;
    MgrThread: TCCTaskPoolMgrThread;
    fResultQue: TCCThreadList;
    fTaskQue: TCCThreadList;
    procedure DoFreeTask(Sender: TCCTaskPool; AObject: TObject; AData: Pointer;
        ATag: Integer); virtual;
    procedure DoTaskDone(Sender: TCCTaskPool; AObject: TObject; AData: Pointer;
        ATag: Integer); virtual;
    procedure DoTaskExecute(Sender: TCCTaskPoolThread; AObject: TObject; AData:
        Pointer; ATag: Integer); virtual;
    procedure DoTaskSyncMsg(Sender: TCCTaskPoolThread; AObject: TObject; AData:
        Pointer; ATag: Integer; AMsg, AParam1, AParam2: Integer; var AResult:
        Integer); virtual;
    procedure SetKeepAliveTimeout(const Value: Integer); virtual;
    procedure SetMaxThreadCount(const Value: Cardinal); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Abort; virtual;
    procedure AbortTasksWithTag(ATag: Integer); virtual;
    procedure AddTask(AData: Pointer; FirstInQue: Boolean = false; ATag: Integer =
        0); overload; virtual;
    procedure AddTask(AObject: TObject; AutoFreeObject: Boolean = true; FirstInQue:
        Boolean = false; ATag: Integer = 0); overload; virtual;
    procedure AddTask(AData: Pointer; AObject: TObject; AutoFreeObject: Boolean =
        true; FirstInQue: Boolean = false; ATag: Integer = 0; ATaskExecute:
        TCCTaskPoolExecuteEvent = nil; ATaskDoneEvent: TCCTaskEvent = nil;
        AFreeTaskEvent: TCCTaskEvent = nil; ATaskSyncMsg: TCCTaskPoolSyncMsgEvent =
        nil); overload; virtual;
    function GetUniqueTagID: Integer; virtual;
    procedure Lock; virtual;
    procedure UnLock; virtual;
    // KeepAliveTimeout
    // - Amount in millisecond to keep thread alive after last task is processed.
    // - If KeepAliveTimeout = 0, threads are killed right away after last task. (default)
    // - If KeepAliveTimeout = INFINITE, threads are kept alive forever.
    property KeepAliveTimeout: Integer read fKeepAliveTimeout write
        SetKeepAliveTimeout;
    property LastAbort: Integer read fLastAbort;
    // MaxThreadCount
    // - Created thread count will not exceed this.
    property MaxThreadCount: Cardinal read fMaxThreadCount write SetMaxThreadCount;
  published
    property OnFreeTask: TCCTaskEvent read fOnFreeTask write fOnFreeTask;
    property OnTaskDone: TCCTaskEvent read fOnTaskDone write fOnTaskDone;
    // OnTaskExecute
    // - This is where all processing is done.
    // - WARNING, this is called from separate thread!!!
    property OnTaskExecute: TCCTaskPoolExecuteEvent read fOnTaskExecute write fOnTaskExecute;
    property OnTaskSyncMsg: TCCTaskPoolSyncMsgEvent read fOnTaskSyncMsg write
        fOnTaskSyncMsg;
  end;

{-------------------------------------------------------------------------------
  Public Methods
-------------------------------------------------------------------------------}
  function GlobalTaskPool: TCCTaskPool;

{==============================================================================}
implementation

var
  fGlobalTaskPoolLock: TRTLCriticalSection;
  fGlobalTaskPool: TCCTaskPool;

{##############################################################################}
// Public Methods

{-------------------------------------------------------------------------------
  GlobalTaskPool
-------------------------------------------------------------------------------}
function GlobalTaskPool: TCCTaskPool;
begin
  EnterCriticalSection(fGlobalTaskPoolLock);
  try
    if not assigned(fGlobalTaskPool) then
    begin
      fGlobalTaskPool:= TCCTaskPool.Create;
      fGlobalTaskPool.KeepAliveTimeout:= 1000;
      fGlobalTaskPool.MaxThreadCount:= 10;
    end;
  finally
    LeaveCriticalSection(fGlobalTaskPoolLock);
  end;
  Result:= fGlobalTaskPool;
end;

{##############################################################################}
// TCCTaskPool

{-------------------------------------------------------------------------------
  Create an instance of TCCTaskPool
-------------------------------------------------------------------------------}
constructor TCCTaskPool.Create;
begin
  inherited;
  // Initilize values
  fKeepAliveTimeout:= 0;
  fMaxThreadCount:= 5;
  fLastAbort:= GetTick;
  fLastTagID:= 0;
  // create lock
  CreateCriticalSection(fLock);
  // create TaskQue
  fTaskQue:= TCCThreadList.Create;
  fTaskQue.ItemDataSize:= SizeOf(TCCTaskPoolData);
  fTaskQue.AutoLock:= false; // we'll do the locking here
  // create ResultQue
  fResultQue:= TCCThreadList.Create;
  fResultQue.ItemDataSize:= SizeOf(TCCTaskPoolData);
  fResultQue.AutoLock:= false; // we'll do the locking here

  // create manager thread
  MgrThread:= TCCTaskPoolMgrThread.Create(true);
  MgrThread.FreeOnTerminate:= false;
  MgrThread.Name:= 'TaskPool_Mgr';
  MgrThread.Pool:= Self;
  MgrThread.Resume;
end;

{-------------------------------------------------------------------------------
  Destroy TCCTaskPool
-------------------------------------------------------------------------------}
destructor TCCTaskPool.Destroy;
begin
  Abort;
  MgrThread.Terminate;
  MgrThread.Wakeup;
  MgrThread.WaitFor;
  MgrThread.Free;
  fTaskQue.Free;
  fResultQue.Free;
  DestroyCriticalSection(fLock);
  inherited;
end;

{-------------------------------------------------------------------------------
  Abort
-------------------------------------------------------------------------------}
procedure TCCTaskPool.Abort;
var
  t: Integer;
begin
  t:= GetTick;
  if t = fLastAbort then
  fLastAbort:= t + 1
  else
  fLastAbort:= t;
end;

{-------------------------------------------------------------------------------
  Abort Tasks With Tag
  - After this call, aborted tasks will not trigger OnTaskDone or OnTaskSyncMsg events.
  - WARNING!!! After thread finishes, OnFreeTask event will be triggered if it's assigned.
  - If you use GlobalTaskPool, do not use OnFreeTask events if there is a chance
    that the caller will be destroyed before GlobalTaskPool. Instead, use objects as data
    and set the AutoFreeObject to true.
-------------------------------------------------------------------------------}
procedure TCCTaskPool.AbortTasksWithTag(ATag: Integer);
var
  data: PCCTaskPoolData;
  item, tmp: PCCListItem;
  list: TList;
  i: Integer;
  thread: TCCTaskPoolThread;
begin
  // abort items from taskQue.
  fTaskQue.Lock;
  try
    item:= fTaskQue.GetFirst;
    while assigned(item) do
    begin
      data:= fTaskQue.GetItemData(item);
      if data.ATag = ATag then
      data.fStartTick:= 0;
      tmp:= item;
      item:= item.Next;
    end;
  finally
    fTaskQue.UnLock;
  end;

  // abort items from resultQue and running threads.
  // - keep the resultQue locked while looping threads because they do modify the que.
  fResultQue.Lock;
  try
    // loop running threads
    list:= MgrThread.fThreads.LockList;
    try
      for i:= 0 to list.Count-1 do
      begin
        thread:= TCCTaskPoolThread(list.Items[i]);
        thread.Lock;
        try
          if thread.Data.ATag = ATag then
          thread.Abort;
        finally
          thread.UnLock;
        end;
      end;
    finally
      MgrThread.fThreads.UnlockList;
    end;

    // loop ResultQue
    item:= fResultQue.GetFirst;
    while assigned(item) do
    begin
      data:= fResultQue.GetItemData(item);
      if data.ATag = ATag then
      data.fStartTick:= 0;
      item:= item.Next;
    end;
  finally
    fResultQue.UnLock;
  end;
  MgrThread.Wakeup;
end;

{-------------------------------------------------------------------------------
  AddTask
-------------------------------------------------------------------------------}
procedure TCCTaskPool.AddTask(AData: Pointer; FirstInQue: Boolean = false;
    ATag: Integer = 0);
begin
  AddTask(AData, nil, false, FirstInQue, ATag);
end;

{-------------------------------------------------------------------------------
  AddTask
-------------------------------------------------------------------------------}
procedure TCCTaskPool.AddTask(AObject: TObject; AutoFreeObject: Boolean = true;
    FirstInQue: Boolean = false; ATag: Integer = 0);
begin
  AddTask(nil, AObject, AutoFreeObject, FirstInQue, ATag);
end;

{-------------------------------------------------------------------------------
  AddTask
-------------------------------------------------------------------------------}
procedure TCCTaskPool.AddTask(AData: Pointer; AObject: TObject; AutoFreeObject:
    Boolean = true; FirstInQue: Boolean = false; ATag: Integer = 0;
    ATaskExecute: TCCTaskPoolExecuteEvent = nil; ATaskDoneEvent: TCCTaskEvent =
    nil; AFreeTaskEvent: TCCTaskEvent = nil; ATaskSyncMsg:
    TCCTaskPoolSyncMsgEvent = nil);
var
  data: PCCTaskPoolData;
begin
  fTaskQue.Lock;
  try
    // add item
    if FirstInQue then
    data:= fTaskQue.GetItemData(fTaskQue.InsertItem(nil, limFirst))
    else
    data:= fTaskQue.GetItemData(fTaskQue.AddItem);
    // assign values
    data.AObject:= AObject;
    data.AData:= AData;
    data.ATag:= ATag;
    data.fAutoFreeObject:= AutoFreeObject;
    data.fStartTick:= GetTick;
    // assign events
    data.ATaskDoneEvent:= ATaskDoneEvent;
    data.AFreeTaskEvent:= AFreeTaskEvent;
    data.ATaskExecuteEvent:= ATaskExecute;
    data.ATaskSyncMsgEvent:= ATaskSyncMsg;
  finally
    fTaskQue.UnLock;
  end;

  MgrThread.Wakeup;
end;

{-------------------------------------------------------------------------------
  Do FreeTask (Synchronized to main thread)
-------------------------------------------------------------------------------}
procedure TCCTaskPool.DoFreeTask(Sender: TCCTaskPool; AObject: TObject; AData:
    Pointer; ATag: Integer);
begin
  if Assigned(fOnFreeTask) then
  fOnFreeTask(Sender, AObject, AData, ATag);
end;

{-------------------------------------------------------------------------------
  Do TaskDone (Synchronized to main thread)
-------------------------------------------------------------------------------}
procedure TCCTaskPool.DoTaskDone(Sender: TCCTaskPool; AObject: TObject; AData:
    Pointer; ATag: Integer);
begin
  if Assigned(fOnTaskDone) then
  fOnTaskDone(Sender, AObject, AData, ATag);
end;

{-------------------------------------------------------------------------------
  Do TaskExecute (runs in a separate thread!!!)
-------------------------------------------------------------------------------}
procedure TCCTaskPool.DoTaskExecute(Sender: TCCTaskPoolThread; AObject: TObject;
    AData: Pointer; ATag: Integer);
begin
  if Assigned(fOnTaskExecute) then
  fOnTaskExecute(Sender, AObject, AData, ATag);
end;

{-------------------------------------------------------------------------------
  Do TaskSyncMsg
-------------------------------------------------------------------------------}
procedure TCCTaskPool.DoTaskSyncMsg(Sender: TCCTaskPoolThread; AObject:
    TObject; AData: Pointer; ATag: Integer; AMsg, AParam1, AParam2: Integer;
    var AResult: Integer);
begin
  if Assigned(fOnTaskSyncMsg) then
  fOnTaskSyncMsg(Sender, AObject, AData, ATag, AMsg, AParam1, AParam2, AResult);
end;

{-------------------------------------------------------------------------------
  GetUniqueTagID
-------------------------------------------------------------------------------}
function TCCTaskPool.GetUniqueTagID: Integer;
begin
  Lock;
  try
    Result:= GetTick;
    if fLastTagID = Result then
    Result:= Result + 1;
    fLastTagID:= Result;
  finally
    UnLock;
  end;
end;

{-------------------------------------------------------------------------------
  Lock
-------------------------------------------------------------------------------}
procedure TCCTaskPool.Lock;
begin
  EnterCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  Set KeepAliveTimeout
-------------------------------------------------------------------------------}
procedure TCCTaskPool.SetKeepAliveTimeout(const Value: Integer);
begin
  fKeepAliveTimeout:= Value;
end;

{-------------------------------------------------------------------------------
  Set MaxThreadCount
-------------------------------------------------------------------------------}
procedure TCCTaskPool.SetMaxThreadCount(const Value: Cardinal);
begin
  if Value > 0 then
  fMaxThreadCount:= Value
  else
  fMaxThreadCount:= 1;
end;

{-------------------------------------------------------------------------------
  UnLock
-------------------------------------------------------------------------------}
procedure TCCTaskPool.UnLock;
begin
  LeaveCriticalSection(fLock);
end;

{##############################################################################}
// TCCTaskPoolMgrThread

{-------------------------------------------------------------------------------
  Create an instance of TCCTaskPoolMgrThread
-------------------------------------------------------------------------------}
constructor TCCTaskPoolMgrThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  fWaitSignal:= CreateSignal;
  // create thread list
  fThreads:= TThreadList.Create;  
end;

{-------------------------------------------------------------------------------
  Destroy TCCTaskPoolMgrThread
-------------------------------------------------------------------------------}
destructor TCCTaskPoolMgrThread.Destroy;
begin
  fThreads.Free;
  DestroySignal(fWaitSignal);
  inherited;
end;

{-------------------------------------------------------------------------------
  Do FreeTask (synchronized to main thread)
-------------------------------------------------------------------------------}
procedure TCCTaskPoolMgrThread.DoFreeTask;
begin
  try
    if assigned(fResultData.AFreeTaskEvent) then
    fResultData.AFreeTaskEvent(Pool, fResultData.AObject, fResultData.AData, fResultData.ATag)
    else
    Pool.DoFreeTask(Pool, fResultData.AObject, fResultData.AData, fResultData.ATag);
  except
    // TODO: needs exception handling
  end;

  try
    if fResultData.fAutoFreeObject and assigned(fResultData.AObject) then
    fResultData.AObject.Free;
  except
    // TODO: needs exception handling
  end;
end;

{-------------------------------------------------------------------------------
  Do TaskDone (synchronized to main thread)
-------------------------------------------------------------------------------}
procedure TCCTaskPoolMgrThread.DoTaskDone;
begin
  try
    if assigned(fResultData.ATaskDoneEvent) then
    fResultData.ATaskDoneEvent(Pool, fResultData.AObject, fResultData.AData, fResultData.ATag)
    else
    Pool.DoTaskDone(Pool, fResultData.AObject, fResultData.AData, fResultData.ATag);
  except
    // TODO: needs exception handling
  end;
end;

{-------------------------------------------------------------------------------
  Execute
-------------------------------------------------------------------------------}
procedure TCCTaskPoolMgrThread.Execute;
var
  i: Integer;
  threadsTerminated: Boolean;
  item: PCCListItem;
  data: PCCTaskPoolData;
  thread: TCCTaskPoolThread;
  done: Boolean;
  list: TList;
begin
  inherited;
  done:= false;
  threadsTerminated:= false;
  repeat
    // start new tasks
    // - don't keep the que locked while starting thread
    // - we are the only ones who delete/modify items in TaskQue so it's safe
    //   to unlock the list.
    if not Self.Terminated then
    begin
      Pool.fTaskQue.Lock;
      item:= Pool.fTaskQue.GetFirst;
      Pool.fTaskQue.UnLock;
      while assigned(item) do
      begin
        data:= Pool.fTaskQue.GetItemData(item);
        // abort task
        if data.fStartTick < Pool.fLastAbort then
        begin
          // do free task
          fResultData:= data;
          Self.Synchronize(DoFreeTask);
          // delete task from TaskQue
          Pool.fTaskQue.Lock;
          Pool.fTaskQue.DeleteItem(item);
          Pool.fTaskQue.UnLock;
        end
        // start task
        else if StartNewTask(data) then
        begin
          // delete task from TaskQue
          Pool.fTaskQue.Lock;
          Pool.fTaskQue.DeleteItem(item);
          Pool.fTaskQue.UnLock;
        end
        else
        break; // all slots are full, go do something else.

        Pool.fTaskQue.Lock;
        item:= Pool.fTaskQue.GetFirst;
        Pool.fTaskQue.UnLock;
      end;
    end
    // terminating, free tasks
    else
    begin
      Pool.fTaskQue.Lock;
      try
        item:= Pool.fTaskQue.GetFirst;
        while assigned(item) do
        begin
          data:= Pool.fTaskQue.GetItemData(item);
          // DoFreeTask
          fResultData:= Pool.fResultQue.GetItemData(item);
          Self.Synchronize(DoFreeTask);
          // delete item from TaskQue
          Pool.fTaskQue.DeleteItem(item);
          item:= Pool.fTaskQue.GetFirst;
        end;
      finally
        Pool.fTaskQue.UnLock;
      end;
    end;
    
    // process results
    // - don't keep the que locked while processing result.
    // - we are the only ones who delete/modify items in ResultQue so it's safe
    //   to unlock the list.
    Pool.fResultQue.Lock;
    item:= Pool.fResultQue.GetFirst;
    Pool.fResultQue.UnLock;

    while assigned(item) do
    begin
      // do task done
      fResultData:= Pool.fResultQue.GetItemData(item);
      if fResultData.fStartTick >= Pool.fLastAbort then
      Self.Synchronize(DoTaskDone);

      // do free task
      Self.Synchronize(DoFreeTask);

      // delete processed item and get new
      Pool.fResultQue.Lock;
      Pool.fResultQue.DeleteItem(item);
      item:= Pool.fResultQue.GetFirst;
      Pool.fResultQue.UnLock;
    end;

    // wait for new tasks
    if not Self.Terminated then
    begin
      WaitForSignal(fWaitSignal);
    end
    // terminating, wait for threads to finish
    else
    begin
      // terminate threads
      if not threadsTerminated then
      begin
        
        list:= fThreads.LockList;
        try
          for i := 0 to list.Count - 1 do
          begin
            thread:= TCCTaskPoolThread(list.Items[i]);
            thread.Terminate;
            thread.Wakeup;
          end;
        finally
          done:= list.Count = 0;
          fThreads.UnlockList;
          threadsTerminated:= true;
        end;
      end
      // check if threads are finished
      else
      begin
        list:= fThreads.LockList;
        done:= list.Count = 0;
        fThreads.UnlockList;
      end;

      // check if TaskQue is empty
      Pool.fTaskQue.Lock;
      done:= done and (Pool.fTaskQue.Count = 0);
      Pool.fTaskQue.UnLock;

      // check if ResultQue is empty
      Pool.fResultQue.Lock;
      done:= done and (Pool.fResultQue.Count = 0);
      Pool.fResultQue.UnLock;

      if not done then
      WaitForSignal(fWaitSignal);
    end;
  until Self.Terminated and done;
end;

{-------------------------------------------------------------------------------
  Start New Task
-------------------------------------------------------------------------------}
function TCCTaskPoolMgrThread.StartNewTask(Data: PCCTaskPoolData): Boolean;
var
  i: Integer;
  list: TList;
  thread: TCCTaskPoolThread;
begin
  Result:= false;
  list:= fThreads.LockList;
  try
    // find finished thread and reuse it
    for i:= 0 to list.Count-1 do
    begin
      thread:= TCCTaskPoolThread(list.Items[i]);
      thread.Lock;
      try
        if thread.TaskDone and not thread.Terminated then
        begin
          thread.Data:= Data^;
          thread.TaskDone:= false;
          thread.Timeout:= Pool.KeepAliveTimeout;
          Result:= true;
          thread.Wakeup;
        end;
      finally
        thread.UnLock;
      end;

      if Result then
      break;
    end;

    // create new thread
    if not Result and (list.Count < Pool.MaxThreadCount) then
    begin
      thread:= TCCTaskPoolThread.Create(true);
      list.Add(thread);
      thread.Name:= 'Task';
      thread.FreeOnTerminate:= true;
      thread.TaskPool:= Pool;
      thread.Data:= Data^;
      thread.Timeout:= Pool.KeepAliveTimeout;
      thread.Resume;
      Result:= true;
    end;
  finally
    fThreads.UnlockList;
  end;
end;

{-------------------------------------------------------------------------------
  Wakeup
-------------------------------------------------------------------------------}
procedure TCCTaskPoolMgrThread.Wakeup;
begin
  SetSignal(fWaitSignal);
end;

{##############################################################################}
// TCCTaskPoolThread

{-------------------------------------------------------------------------------
  Create an instance of TCCTaskPoolThread
-------------------------------------------------------------------------------}
constructor TCCTaskPoolThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  fTaskDone:= false;
  fTimeout:= 1000;
  fWaitSignal:= CreateSignal;
  CreateCriticalSection(fDataLock);
end;

{-------------------------------------------------------------------------------
  Destroy TCCTaskPoolThread
-------------------------------------------------------------------------------}
destructor TCCTaskPoolThread.Destroy;
begin
  DestroySignal(fWaitSignal);
  DestroyCriticalSection(fDataLock);
  inherited;
end;

{-------------------------------------------------------------------------------
  Abort
-------------------------------------------------------------------------------}
procedure TCCTaskPoolThread.Abort;
begin
  // - if fStartTick is smaller than LastAbort, then TaskDone event won't happen.
  Data.fStartTick:= 0;
end;

{-------------------------------------------------------------------------------
  Execute
-------------------------------------------------------------------------------}
procedure TCCTaskPoolThread.Execute;
var
  done: Boolean;
  list: TList;
  poolData: PCCTaskPoolData;
begin
  inherited;
  done:= false;

  repeat
    // Execute task
    try
      if assigned(Data.ATaskExecuteEvent) then
      Data.ATaskExecuteEvent(Self, Data.AObject, Data.AData, Data.ATag)
      else
      TaskPool.DoTaskExecute(Self, Data.AObject, Data.AData, Data.ATag);
    except
    end;

    // Add data to result que
    TaskPool.fResultQue.Lock;
    try
      poolData:= TaskPool.fResultQue.GetItemData(TaskPool.fResultQue.AddItem);
      poolData^:= Data;
    finally
      TaskPool.fResultQue.UnLock;
    end;

    // TaskDone
    Lock;
    fTaskDone:= true;
    UnLock;
    TaskPool.MgrThread.Wakeup;

    // Wait
    if not Self.Terminated then
    WaitForSignal(fWaitSignal, true, fTimeout);

    // Terminate if there's no new task
    Lock;
    if fTaskDone then
    Self.Terminate;
    UnLock;
    
  until Self.Terminated;

  // remove self from threads list
  list:= TaskPool.MgrThread.fThreads.LockList;
  try
    list.Remove(Self);
  finally
    TaskPool.MgrThread.fThreads.UnlockList;
    TaskPool.MgrThread.Wakeup;
  end;
end;

{-------------------------------------------------------------------------------
  IsAborted
-------------------------------------------------------------------------------}
function TCCTaskPoolThread.IsAborted: Boolean;
begin
  Result:= Data.fStartTick < TaskPool.fLastAbort;
end;

{-------------------------------------------------------------------------------
  Lock
-------------------------------------------------------------------------------}
procedure TCCTaskPoolThread.Lock;
begin
  EnterCriticalSection(fDataLock);
end;

{-------------------------------------------------------------------------------
  DoSyncMsg
-------------------------------------------------------------------------------}
procedure TCCTaskPoolThread.DoSyncMsg;
begin
  try
    if assigned(Data.ATaskSyncMsgEvent) then
    Data.ATaskSyncMsgEvent(Self, Data.AObject, Data.AData, Data.ATag,
                           fSyncMsg.AMsg, fSyncMsg.AParam1, fSyncMsg.AParam2,
                           fSyncMsg.AResult)
    else
    TaskPool.DoTaskSyncMsg(Self, Data.AObject, Data.AData, Data.ATag,
                           fSyncMsg.AMsg, fSyncMsg.AParam1, fSyncMsg.AParam2,
                           fSyncMsg.AResult);
  except
    // TODO: needs exception handling
  end;
end;

{-------------------------------------------------------------------------------
  Send Synchronized Message
-------------------------------------------------------------------------------}
function TCCTaskPoolThread.SendSyncMessage(AMsg, AParam1, AParam2: Integer):
    Integer;
begin
  if not IsAborted then
  begin
    fSyncMsg.AMsg:= AMsg;
    fSyncMsg.AParam1:= AParam1;
    fSyncMsg.AParam2:= AParam2;
    fSyncMsg.AResult:= 0;
    // send
    Self.Synchronize(DoSyncMsg);
    // result
    Result:= fSyncMsg.AResult;
  end;
end;

{-------------------------------------------------------------------------------
  UnLock
-------------------------------------------------------------------------------}
procedure TCCTaskPoolThread.UnLock;
begin
  LeaveCriticalSection(fDataLock);
end;

{-------------------------------------------------------------------------------
  Wakeup
-------------------------------------------------------------------------------}
procedure TCCTaskPoolThread.Wakeup;
begin
  SetSignal(fWaitSignal);
end;

{##############################################################################}

initialization
  CreateCriticalSection(fGlobalTaskPoolLock);

finalization
  // free GlobalTaskPool
  EnterCriticalSection(fGlobalTaskPoolLock);
  try
    if assigned(fGlobalTaskPool) then
    begin
      fGlobalTaskPool.Free;
      fGlobalTaskPool:= nil;
    end;
  finally
    LeaveCriticalSection(fGlobalTaskPoolLock);
  end;
  DestroyCriticalSection(fGlobalTaskPoolLock);

end.

