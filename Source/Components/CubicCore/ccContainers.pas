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
//  The Original Code is ccContainers.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccContainers;

{$INCLUDE Core.inc}

{==============================================================================}
interface

uses
  // CubicCore
  ccThreads, ccTypes,
  // System Units
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes;

const
  IID_ICCTree: TGUID = '{EB396207-9A8F-4D5E-8FC2-7DF24CE7196F}';
  IID_ICCList: TGUID = '{5F2F3413-0734-4827-989A-892452296F50}';

type

{-------------------------------------------------------------------------------
  TCCTree
-------------------------------------------------------------------------------}
  PCCTreeItem = ^TCCTreeItem;
  TCCTreeItem = record
    // Info
    ChildCount: Integer;
    // Navigation
    Parent: PCCTreeItem;
    NextSibling: PCCTreeItem;
    PrevSibling: PCCTreeItem;
    FirstChild: PCCTreeItem;
    LastChild: PCCTreeItem;
    // Data
    CustomData: Pointer;
    Data: record end;
  end;

  TCCTree = class;

  TCCItemEvent = procedure(ASender: TCCTree; AItem: PCCTreeItem) of object;

  ICCTree = interface(IInterface)
  ['{EB396207-9A8F-4D5E-8FC2-7DF24CE7196F}']
    function AddItem(AParent: PCCTreeItem): PCCTreeItem; stdcall;
    procedure Clear; stdcall;
    procedure DeleteItem(AItem: PCCTreeItem); stdcall;
    function GetCount: Integer; stdcall;
    function GetFirst: PCCTreeItem; stdcall;
    function GetFirstChild(AParent: PCCTreeItem): PCCTreeItem; stdcall;
    function GetItem(AIndex: Integer): PCCTreeItem; stdcall;
    function GetItemData(AItem: PCCTreeItem): Pointer; stdcall;
    function GetRoot: PCCTreeItem; stdcall;
    function InsertItem(ARelative: PCCTreeItem; AInsertMode: TCCInsertMode):
        PCCTreeItem; stdcall;
    procedure SetItemDataSize(const Value: Cardinal); stdcall;
    procedure SetFreeItemEvent(const Value: TCCItemEvent); stdcall;
    function Validate(AItem: PCCTreeItem): Boolean; stdcall;
  end;
    
  TCCTree = class(TInterfacedObject, ICCTree)
  private
  protected
    fItemDataSize: Cardinal;
    fItems: TList;
    fItemSize: Cardinal;
    fOnFreeItem: TCCItemEvent;
    fRoot: PCCTreeItem;
    function AllocItem: PCCTreeItem; virtual;
    procedure FreeItem(AItem: PCCTreeItem); virtual;
    function GetCount: Integer; virtual; stdcall;
    function GetItem(AIndex: Integer): PCCTreeItem; virtual; stdcall;
    function GetRoot: PCCTreeItem; virtual; stdcall;
    procedure SetItemDataSize(const Value: Cardinal); virtual; stdcall;
    procedure SetFreeItemEvent(const Value: TCCItemEvent); virtual; stdcall;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddItem(AParent: PCCTreeItem): PCCTreeItem; virtual; stdcall;
    procedure Clear; virtual; stdcall;
    procedure DeleteItem(AItem: PCCTreeItem); virtual; stdcall;
    function GetFirst: PCCTreeItem; virtual; stdcall;
    function GetFirstChild(AParent: PCCTreeItem): PCCTreeItem; virtual; stdcall;
    function GetItemData(AItem: PCCTreeItem): Pointer; virtual; stdcall;
    function InsertItem(ARelative: PCCTreeItem; AInsertMode: TCCInsertMode): PCCTreeItem;
        virtual; stdcall;
    function Validate(AItem: PCCTreeItem): Boolean; virtual; stdcall;
    property Item[AIndex: Integer]: PCCTreeItem read GetItem;
    property Root: PCCTreeItem read GetRoot;
    property Count: Integer read GetCount;
    property ItemDataSize: Cardinal read fItemDataSize write SetItemDataSize;
    property OnFreeItem: TCCItemEvent read fOnFreeItem write SetFreeItemEvent;
  end;

{-------------------------------------------------------------------------------
  TCCThreadTree
-------------------------------------------------------------------------------}
  TCCThreadTree = class(TCCTree, ICCThreadLock)
  private
    fAutoLock: Boolean;
  protected
    fLock: TRTLCriticalSection;
    function GetItem(AIndex: Integer): PCCTreeItem; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function AddItem(AParent: PCCTreeItem): PCCTreeItem; override;
    procedure Clear; override;
    procedure DeleteItem(AItem: PCCTreeItem); override;
    function GetFirst: PCCTreeItem; override;
    function GetFirstChild(AParent: PCCTreeItem): PCCTreeItem; override;
    function InsertItem(ARelative: PCCTreeItem; AInsertMode: TCCInsertMode): PCCTreeItem;
        override;
    procedure Lock; virtual; stdcall;
    procedure UnLock; virtual; stdcall;
    function Validate(AItem: PCCTreeItem): Boolean; override;
  published
    property AutoLock: Boolean read fAutoLock write fAutoLock;
  end;

{-------------------------------------------------------------------------------
  TCCList
-------------------------------------------------------------------------------}
  TCCList = class;
  
  PCCListItem = ^TCCListItem;
  TCCListItem = record
    // Info
    Index: Integer;
    // Navigation
    Next: PCCListItem;
    Previous: PCCListItem;
    // Data
    CustomData: Pointer;
    Data: record end;
  end;

  TCCListInsertMode = (limBefore, limAfter, limFirst, limLast);

  TCCListItemEvent = procedure(ASender: TCCList; AItem: PCCListItem) of object;

  ICCList = interface(IInterface)
  ['{5F2F3413-0734-4827-989A-892452296F50}']
    function AddItem: PCCListItem; stdcall;
    procedure Clear; stdcall;
    function Delete(AIndex: Integer): Boolean; stdcall;
    procedure DeleteItem(AItem: PCCListItem); stdcall;
    function GetCount: Integer; stdcall;
    function GetFirst: PCCListItem; stdcall;
    function GetItem(AIndex: Integer): PCCListItem; stdcall;
    function GetItemData(AItem: PCCListItem): Pointer; stdcall;
    function GetLast: PCCListItem; stdcall;
    function IndexOf(AItem: PCCListItem): Integer; stdcall;
    function Insert(AIndex: Integer): PCCListItem; stdcall;
    function InsertItem(ARelative: PCCListItem; AInsertMode: TCCListInsertMode):
        PCCListItem; stdcall;
    procedure Move(CurIndex: Integer; NewIndex: Integer); stdcall;
    procedure SetFreeItemEvent(const Value: TCCListItemEvent); stdcall;
    procedure SetItemDataSize(const Value: Cardinal); stdcall;
    function Validate(AItem: PCCListItem): Boolean; stdcall;
  end;

  TCCList = class(TInterfacedObject, ICCList)
  private
  protected
    fCount: Integer;
    fFirstItem: PCCListItem;
    fItemDataSize: Cardinal;
    fItems: TList;
    fItemSize: Cardinal;
    fLastItem: PCCListItem;                       
    fOnFreeItem: TCCListItemEvent;
    function AllocItem: PCCListItem; virtual;
    procedure FreeItem(AItem: PCCListItem); virtual;
    function GetCount: Integer; virtual; stdcall;
    function GetItem(AIndex: Integer): PCCListItem; virtual; stdcall;
    procedure SetItemDataSize(const Value: Cardinal); virtual; stdcall;
    procedure SetFreeItemEvent(const Value: TCCListItemEvent); virtual; stdcall;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddItem: PCCListItem; virtual; stdcall;
    procedure Clear; virtual; stdcall;
    function Delete(AIndex: Integer): Boolean; virtual; stdcall;
    procedure DeleteItem(AItem: PCCListItem); virtual; stdcall;
    function GetFirst: PCCListItem; virtual; stdcall;
    function GetItemData(AItem: PCCListItem): Pointer; virtual; stdcall;
    function GetLast: PCCListItem; virtual; stdcall;
    function IndexOf(AItem: PCCListItem): Integer; virtual; stdcall;
    function Insert(AIndex: Integer): PCCListItem; virtual; stdcall;
    function InsertItem(ARelative: PCCListItem; AInsertMode: TCCListInsertMode):
        PCCListItem; virtual; stdcall;
    procedure Move(CurIndex: Integer; NewIndex: Integer); virtual; stdcall;
    function Validate(AItem: PCCListItem): Boolean; virtual; stdcall;
    property Count: Integer read GetCount;
    property Item[AIndex: Integer]: PCCListItem read GetItem;
    property ItemDataSize: Cardinal read fItemDataSize write SetItemDataSize;
    property OnFreeItem: TCCListItemEvent read fOnFreeItem write 
    SetFreeItemEvent;
  end;

{-------------------------------------------------------------------------------
  TCCThreadList
-------------------------------------------------------------------------------}
  TCCThreadList = class(TCCList, ICCThreadLock)
  private
    fAutoLock: Boolean;
  protected
    fLock: TRTLCriticalSection;
    function GetItem(AIndex: Integer): PCCListItem; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function AddItem: PCCListItem; override;
    procedure Clear; override;
    function Delete(AIndex: Integer): Boolean; override;
    procedure DeleteItem(AItem: PCCListItem); override;
    function GetFirst: PCCListItem; override;
    function GetLast: PCCListItem; override;
    function IndexOf(AItem: PCCListItem): Integer; override;
    function Insert(AIndex: Integer): PCCListItem; override;
    function InsertItem(ARelative: PCCListItem; AInsertMode: TCCListInsertMode):
        PCCListItem; override;
    procedure Lock; virtual; stdcall;
    procedure UnLock; virtual; stdcall;
    function Validate(AItem: PCCListItem): Boolean; override;
  published
    property AutoLock: Boolean read fAutoLock write fAutoLock;
  end;

{==============================================================================}
implementation

{##############################################################################}
// TCCTree

{-------------------------------------------------------------------------------
  Create an instance of TCCTree
-------------------------------------------------------------------------------}
constructor TCCTree.Create;
begin
  // Create fItems
  fItems:= TList.Create;
  // Init values
  fItemSize:= SizeOf(TCCTreeItem);
  fItemDataSize:= 0;
  // Create Root item
  fRoot:= AllocItem;
end;

{-------------------------------------------------------------------------------
  Destroy TCCTree
-------------------------------------------------------------------------------}
destructor TCCTree.Destroy;
begin
  Clear;
  // Free Item
  fItems.Free;
  // Free Root
  FreeItem(fRoot);
end;

{-------------------------------------------------------------------------------
  Allocate Item
-------------------------------------------------------------------------------}
function TCCTree.AllocItem: PCCTreeItem;
begin
  // MEMO, AllocMem initilizes memory to zero.
  Result:= AllocMem(fItemSize + fItemDataSize);
end;

{-------------------------------------------------------------------------------
  Free Item
-------------------------------------------------------------------------------}
procedure TCCTree.FreeItem(AItem: PCCTreeItem);
begin
  try
    if assigned(fOnFreeItem) and (AItem <> fRoot) then
    fOnFreeItem(Self, AItem);
  finally
    FreeMem(AItem);
  end;
end;

{-------------------------------------------------------------------------------
  Add Item
-------------------------------------------------------------------------------}
function TCCTree.AddItem(AParent: PCCTreeItem): PCCTreeItem;
begin
  Result:= AllocItem;
  fItems.Add(Result);
  // Set parent
  if AParent = nil then
  AParent:= fRoot;
  Result.Parent:= AParent;
  // Set siblings
  if assigned(AParent.LastChild) then
  begin
    AParent.LastChild.NextSibling:= Result;
    Result.PrevSibling:= AParent.LastChild;
  end;
  // Set AParent.Childs
  AParent.LastChild:= Result;
  if not assigned(AParent.FirstChild) then
  AParent.FirstChild:= Result;
  AParent.ChildCount:= AParent.ChildCount + 1;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCCTree.Clear;
var
  i: Integer;
begin
  for i:= 0 to FItems.Count-1 do
  begin
    FreeItem(fItems.Items[i]);
  end;
  fItems.Clear;
  fRoot.FirstChild:= nil;
  fRoot.LastChild:= nil;
  fRoot.ChildCount:= 0;
end;

{-------------------------------------------------------------------------------
  DeleteItem
-------------------------------------------------------------------------------}
procedure TCCTree.DeleteItem(AItem: PCCTreeItem);
var
  chItem, next: PCCTreeItem;
begin
  if AItem = fRoot then
  Exit; // --> Can't delete root

  // Delete children
  next:= AItem.FirstChild;
  while assigned(next) do
  begin
    chItem:= next;
    next:= chItem.NextSibling;
    DeleteItem(chItem);
  end;

  // Remove AItem from list
  fItems.Remove(AItem);
  // Refresh siblings
  if assigned(AItem.PrevSibling) then
  AItem.PrevSibling.NextSibling:= AItem.NextSibling;
  if assigned(AItem.NextSibling) then
  AItem.NextSibling.PrevSibling:= AItem.PrevSibling;
  // Refresh parent
  if AItem.Parent.FirstChild = AItem then
  AItem.Parent.FirstChild:= AItem.NextSibling;
  if AItem.Parent.LastChild = AItem then
  AItem.Parent.LastChild:= AItem.PrevSibling;
  AItem.Parent.ChildCount:= AItem.Parent.ChildCount - 1;
  // Free memory
  FreeItem(AItem);
end;

{-------------------------------------------------------------------------------
  Get Count
-------------------------------------------------------------------------------}
function TCCTree.GetCount: Integer;
begin
  Result:= fItems.Count;
end;

{-------------------------------------------------------------------------------
  Get First
-------------------------------------------------------------------------------}
function TCCTree.GetFirst: PCCTreeItem;
begin
  Result:= fRoot.FirstChild;
end;

{-------------------------------------------------------------------------------
  Get First Child
-------------------------------------------------------------------------------}
function TCCTree.GetFirstChild(AParent: PCCTreeItem): PCCTreeItem;
begin
  if assigned(AParent) then
  begin
    Result:= AParent.FirstChild;
  end
  else
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Get ItemData
-------------------------------------------------------------------------------}
function TCCTree.GetItemData(AItem: PCCTreeItem): Pointer;
begin
  if not assigned(AItem) or (fItemDataSize = 0) or (AItem = fRoot) then
  Result:= nil
  else
  Result:= @AItem.Data;
end;

{-------------------------------------------------------------------------------
  Get Item
-------------------------------------------------------------------------------}
function TCCTree.GetItem(AIndex: Integer): PCCTreeItem;
begin
  Result:= fItems.Items[AIndex];
end;

{-------------------------------------------------------------------------------
  Get Root
-------------------------------------------------------------------------------}
function TCCTree.GetRoot: PCCTreeItem;
begin
  Result:= fRoot;
end;

{-------------------------------------------------------------------------------
  InsertItem
-------------------------------------------------------------------------------}
function TCCTree.InsertItem(ARelative: PCCTreeItem; AInsertMode:
    TCCInsertMode): PCCTreeItem;
begin
  Result:= nil;
  if (ARelative = nil) and ((AInsertMode = imBefore) or (AInsertMode = imAfter)) then
  Exit; // --> Can't add Item in the same level as root

  Result:= AllocItem;
  fItems.Add(Result);

  if ARelative = nil then
  ARelative:= fRoot;
  // Last child of ARelative
  if AInsertMode = imLastChild then
  begin
    Result.Parent:= ARelative;
    if assigned(ARelative.LastChild) then
    ARelative.LastChild.NextSibling:= Result;
    Result.PrevSibling:= ARelative.LastChild;
    ARelative.LastChild:= Result;
    if not assigned(ARelative.FirstChild) then
    ARelative.FirstChild:= Result;
    ARelative.ChildCount:= ARelative.ChildCount + 1;
  end
  // First child of ARelative
  else if AInsertMode = imFirstChild then
  begin
    Result.Parent:= ARelative;
    if assigned(ARelative.FirstChild) then
    ARelative.FirstChild.PrevSibling:= Result;
    Result.NextSibling:= ARelative.FirstChild;
    ARelative.FirstChild:= Result;
    if not assigned(ARelative.LastChild) then
    ARelative.LastChild:= Result;
    ARelative.ChildCount:= ARelative.ChildCount + 1;
  end  
  // Before ARelative
  else if AInsertMode = imBefore then
  begin
    Result.Parent:= ARelative.Parent;
    if (Result.Parent.FirstChild = ARelative) then
    Result.Parent.FirstChild:= Result;
    Result.Parent.ChildCount:= Result.Parent.ChildCount + 1;

    Result.PrevSibling:= ARelative.PrevSibling;
    Result.NextSibling:= ARelative;
    if assigned(ARelative.PrevSibling) then
    ARelative.PrevSibling.NextSibling:= Result;
    ARelative.PrevSibling:= Result;
  end
  // After ARelative
  else if AInsertMode = imAfter then
  begin
    Result.Parent:= ARelative.Parent;
    if (Result.Parent.LastChild = ARelative) then
    Result.Parent.LastChild:= Result;
    Result.Parent.ChildCount:= Result.Parent.ChildCount + 1;

    Result.PrevSibling:= ARelative;
    Result.NextSibling:= ARelative.NextSibling;
    if assigned(ARelative.NextSibling) then
    ARelative.NextSibling.PrevSibling:= Result;
    ARelative.NextSibling:= Result;
  end;
end;

{-------------------------------------------------------------------------------
  Set ItemDataSize
-------------------------------------------------------------------------------}
procedure TCCTree.SetItemDataSize(const Value: Cardinal);
begin
  fItemDataSize:= Value;
end;

{-------------------------------------------------------------------------------
  Set OnFreeItem
-------------------------------------------------------------------------------}
procedure TCCTree.SetFreeItemEvent(const Value: TCCItemEvent);
begin
  fOnFreeItem:= Value;
end;

{-------------------------------------------------------------------------------
  Validate
-------------------------------------------------------------------------------}
function TCCTree.Validate(AItem: PCCTreeItem): Boolean;
begin
  Result:= fItems.IndexOf(AItem) > -1;
end;

{##############################################################################}
// TCCThreadTree

{-------------------------------------------------------------------------------
  Create an instance of TCCThreadTree
-------------------------------------------------------------------------------}
constructor TCCThreadTree.Create;
begin
  inherited;
  // Create Lock
  CreateCriticalSection(fLock);
  // Initialize values
  fAutoLock:= true;
end;

{-------------------------------------------------------------------------------
  Destroy TCCThreadTree
-------------------------------------------------------------------------------}
destructor TCCThreadTree.Destroy;
begin
  inherited;
  // Free Lock
  DestroyCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  Add Item
-------------------------------------------------------------------------------}
function TCCThreadTree.AddItem(AParent: PCCTreeItem): PCCTreeItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited AddItem(AParent);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited AddItem(AParent);
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCCThreadTree.Clear;
begin
  if AutoLock then
  begin
    Lock;
    try
      inherited;
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  inherited;
end;

{-------------------------------------------------------------------------------
  DeleteItem
-------------------------------------------------------------------------------}
procedure TCCThreadTree.DeleteItem(AItem: PCCTreeItem);
begin
  if AItem = fRoot then
  Exit; // --> Can't delete root

  if AutoLock then
  begin
    Lock;
    try
      inherited;
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  inherited;
end;

{-------------------------------------------------------------------------------
  Get First
-------------------------------------------------------------------------------}
function TCCThreadTree.GetFirst: PCCTreeItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= fRoot.FirstChild;
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= fRoot.FirstChild;
end;

{-------------------------------------------------------------------------------
  Get First Child
-------------------------------------------------------------------------------}
function TCCThreadTree.GetFirstChild(AParent: PCCTreeItem): PCCTreeItem;
begin
  if assigned(AParent) then
  begin
    if AutoLock then
    begin
      Lock;
      try
        Result:= AParent.FirstChild;
      finally
        if AutoLock then UnLock;
      end;
    end
    else
    Result:= AParent.FirstChild;
  end
  else
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Get Item
-------------------------------------------------------------------------------}
function TCCThreadTree.GetItem(AIndex: Integer): PCCTreeItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited GetItem(AIndex);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited GetItem(AIndex);
end;

{-------------------------------------------------------------------------------
  InsertItem
-------------------------------------------------------------------------------}
function TCCThreadTree.InsertItem(ARelative: PCCTreeItem; AInsertMode:
    TCCInsertMode): PCCTreeItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited InsertItem(ARelative, AInsertMode);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited InsertItem(ARelative, AInsertMode);
end;

{-------------------------------------------------------------------------------
  Lock
-------------------------------------------------------------------------------}
procedure TCCThreadTree.Lock;
begin
  EnterCriticalSection(fLock);  
end;

{-------------------------------------------------------------------------------
  UnLock
-------------------------------------------------------------------------------}
procedure TCCThreadTree.UnLock;
begin
  LeaveCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  Validate
-------------------------------------------------------------------------------}
function TCCThreadTree.Validate(AItem: PCCTreeItem): Boolean;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited Validate(AItem);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited Validate(AItem);
end;

{##############################################################################}
// TCCList

{-------------------------------------------------------------------------------
  Create an instance of TCCList
-------------------------------------------------------------------------------}
constructor TCCList.Create;
begin
  // Create fItems
  fItems:= TList.Create;
  // Init values
  fItemSize:= SizeOf(TCCListItem);
  fItemDataSize:= 0;
  fFirstItem:= nil;
  fLastItem:= nil;
  fCount:= 0;
end;

{-------------------------------------------------------------------------------
  Destroy TCCList
-------------------------------------------------------------------------------}
destructor TCCList.Destroy;
begin
  Clear;
  // Free fItems
  fItems.Free;
end;

{-------------------------------------------------------------------------------
  Add Item
-------------------------------------------------------------------------------}
function TCCList.AddItem: PCCListItem;
begin
  // Allocate item
  Result:= AllocItem;
  // Set Index
  Result.Index:= fCount;
  fCount:= fCount + 1;
  // Set siblings
  if assigned(fLastItem) then
  begin
    fLastItem.Next:= Result;
    Result.Previous:= fLastItem;
  end;
  // set First and Last items
  fLastItem:= Result;
  if not assigned(fFirstItem) then
  fFirstItem:= Result;
  // Add to fItems
  fItems.Add(Result);
end;

{-------------------------------------------------------------------------------
  Allocate Item
-------------------------------------------------------------------------------}
function TCCList.AllocItem: PCCListItem;
begin
  // MEMO, AllocMem initilizes memory to zero.
  Result:= AllocMem(fItemSize + fItemDataSize);
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCCList.Clear;
var
  item, next: PCCListItem;
begin
  // Free items
  next:= fFirstItem;
  while assigned(next) do
  begin
    item:= next;
    next:= item.Next;
    FreeItem(item);
  end;
  // Reset values
  fFirstItem:= nil;
  fLastItem:= nil;
  fCount:= 0;
  fItems.Clear;
end;

{-------------------------------------------------------------------------------
  Delete (Returns TRUE if successful, otherwise FALSE)
-------------------------------------------------------------------------------}
function TCCList.Delete(AIndex: Integer): Boolean;
begin
  Result:= (AIndex > -1) and (AIndex < fCount);
  if Result then
  begin
    DeleteItem(GetItem(AIndex));
  end;
end;

{-------------------------------------------------------------------------------
  DeleteItem
-------------------------------------------------------------------------------}
procedure TCCList.DeleteItem(AItem: PCCListItem);
var
  next: PCCListItem;
begin
  if assigned(AItem) then
  begin
    // Refresh siblings
    if assigned(AItem.Previous) then
    AItem.Previous.Next:= AItem.Next;
    if assigned(AItem.Next) then
    AItem.Next.Previous:= AItem.Previous;
    // Refresh indexes
    next:= AItem.Next;
    while assigned(next) do
    begin
      next.Index:= next.Index - 1;
      next:= next.Next;
    end;
    // Refresh count
    fCount:= fCount - 1;
    // Refresh First and Last items
    if fFirstItem = AItem then
    fFirstItem:= AItem.Next;
    if fLastItem = AItem then
    fLastItem:= AItem.Previous;
    // Remove from fItems
    fItems.Delete(AItem.Index);
    // Free memory
    FreeItem(AItem);
  end;
end;

{-------------------------------------------------------------------------------
  Free Item
-------------------------------------------------------------------------------}
procedure TCCList.FreeItem(AItem: PCCListItem);
begin
  try
    if assigned(fOnFreeItem) then
    fOnFreeItem(Self, AItem);
  finally
    FreeMem(AItem);
  end;
end;

{-------------------------------------------------------------------------------
  Get Count
-------------------------------------------------------------------------------}
function TCCList.GetCount: Integer;
begin
  Result:= fCount;
end;

{-------------------------------------------------------------------------------
  Get First
-------------------------------------------------------------------------------}
function TCCList.GetFirst: PCCListItem;
begin
  Result:= fFirstItem;
end;

{-------------------------------------------------------------------------------
  Get ItemData
-------------------------------------------------------------------------------}
function TCCList.GetItemData(AItem: PCCListItem): Pointer;
begin
  if assigned(AItem) and (fItemDataSize <> 0) then
  Result:= @AItem.Data
  else
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Get Item
-------------------------------------------------------------------------------}
function TCCList.GetItem(AIndex: Integer): PCCListItem;
begin
  Result:= fItems.Items[AIndex];
end;

{-------------------------------------------------------------------------------
  Get Last
-------------------------------------------------------------------------------}
function TCCList.GetLast: PCCListItem;
begin
  Result:= fLastItem;
end;

{-------------------------------------------------------------------------------
  IndexOf
-------------------------------------------------------------------------------}
function TCCList.IndexOf(AItem: PCCListItem): Integer;
begin
  if assigned(AItem) then
  Result:= AItem.Index
  else
  Result:= -1;
end;

{-------------------------------------------------------------------------------
  Insert  
-------------------------------------------------------------------------------}
function TCCList.Insert(AIndex: Integer): PCCListItem;
var
  item: PCCListItem;
begin
  if AIndex >= fCount then
  Result:= InsertItem(nil, limLast)
  else if (AIndex < 1) then
  Result:= InsertItem(nil, limFirst)
  else
  begin
    item:= GetItem(AIndex);
    Result:= InsertItem(item, limBefore);
  end;
end;

{-------------------------------------------------------------------------------
  InsertItem
-------------------------------------------------------------------------------}
function TCCList.InsertItem(ARelative: PCCListItem; AInsertMode:
    TCCListInsertMode): PCCListItem;
var
  next: PCCListItem;
begin
  // Allocate item
  Result:= AllocItem;
  // Setup insert mode
  if ARelative = nil then
  begin
    if AInsertMode = limBefore then
    AInsertMode:= limFirst
    else if AInsertMode = limAfter then
    AInsertMode:= limLast;
  end;

  // Insert before ARelative
  if AInsertMode = limBefore then
  begin
    // set navigation
    Result.Next:= ARelative;
    Result.Previous:= ARelative.Previous;
    if not assigned(Result.Previous) then
    fFirstItem:= Result
    else
    Result.Previous.Next:= Result;
    ARelative.Previous:= Result;
    // set indexes
    Result.Index:= ARelative.Index;
    next:= ARelative;
    while assigned(next) do
    begin
      next.Index:= next.Index + 1;
      next:= next.Next;
    end;
    // Insert to fItems
    fItems.Insert(Result.Index, Result);
  end
  // Insert after ARelative
  else if AInsertMode = limAfter then
  begin
    // set navigation
    Result.Previous:= ARelative;
    Result.Next:= ARelative.Next;
    if not assigned(Result.Next) then
    fLastItem:= Result
    else
    Result.Next.Previous:= Result;
    ARelative.Next:= Result;
    // set indexes
    Result.Index:= ARelative.Index + 1;
    next:= Result.Next;
    while assigned(next) do
    begin
      next.Index:= next.Index + 1;
      next:= next.Next;
    end;
    // Insert to fItems
    fItems.Insert(Result.Index, Result);
  end
  // Insert as FirstItem
  else if AInsertMode = limFirst then
  begin
    // set navigation
    Result.Next:= fFirstItem;
    fFirstItem:= Result;
    if not assigned(Result.Next) then
    fLastItem:= Result
    else
    Result.Next.Previous:= Result;
    // set indexes
    Result.Index:= 0;
    next:= Result.Next;
    while assigned(next) do
    begin
      next.Index:= next.Index + 1;
      next:= next.Next;
    end;
    // Insert to fItems
    fItems.Insert(Result.Index, Result);
  end
  // Insert as LastItem
  else
  begin
    // set navigation
    Result.Previous:= fLastItem;
    fLastItem:= Result;
    if not assigned(Result.Previous) then
    fFirstItem:= Result
    else
    Result.Previous.Next:= Result;
    // set indexes
    Result.Index:= fCount;
    // Add to fItems
    fItems.Add(Result);
  end;

  fCount:= fCount + 1;
end;

{-------------------------------------------------------------------------------
  Move
-------------------------------------------------------------------------------}
procedure TCCList.Move(CurIndex: Integer; NewIndex: Integer);
var
  cur, new: PCCListItem;
  i: Integer;
begin
  if (CurIndex <> NewIndex) then
  begin
    cur:= fItems.Items[CurIndex];
    new:= fItems.Items[NewIndex];
    fItems.Move(CurIndex, NewIndex);

    // change previous siblings
    if assigned(cur.Previous) then
    cur.Previous.Next:= cur.Next;
    if assigned(cur.Next) then
    cur.Next.Previous:= cur.Previous;

    // update siblings
    if CurIndex < NewIndex then
    begin
      cur.Previous:= new;
      cur.Next:= new.Next;      
      if assigned(new.Next) then
      new.Next.Previous:= cur;
      new.Next:= cur;

      // update indexes
      for i:= CurIndex to NewIndex do
      PCCListItem(fItems.Items[i]).Index:= i;
    end
    else
    begin
      if assigned(new.Previous) then
      new.Previous.Next:= cur;
      cur.Previous:= new.Previous;
      cur.Next:= new;
      new.Previous:= cur;

      // update indexes
      for i:= NewIndex to CurIndex do
      PCCListItem(fItems.Items[i]).Index:= i;
    end;

    // change first/last
    fFirstItem:= fItems.Items[0];
    fLastItem:= fItems.Items[fItems.Count-1];
  end;
end;

{-------------------------------------------------------------------------------
  Set ItemDataSize
-------------------------------------------------------------------------------}
procedure TCCList.SetItemDataSize(const Value: Cardinal);
begin
  fItemDataSize:= Value;
end;

{-------------------------------------------------------------------------------
  Set OnFreeItem
-------------------------------------------------------------------------------}
procedure TCCList.SetFreeItemEvent(const Value: TCCListItemEvent);
begin
  fOnFreeItem:= Value;
end;

{-------------------------------------------------------------------------------
  Validate (return TRUE if AItem belongs to this list, otherwise false)
-------------------------------------------------------------------------------}
function TCCList.Validate(AItem: PCCListItem): Boolean;
begin
  Result:= fItems.IndexOf(AItem) > -1;
end;

{##############################################################################}
// TCCThreadList

{-------------------------------------------------------------------------------
  Create an instance of TCCThreadList
-------------------------------------------------------------------------------}
constructor TCCThreadList.Create;
begin
  inherited;
  // Create Lock
  CreateCriticalSection(fLock);
  // Initialize values
  fAutoLock:= true;
end;

{-------------------------------------------------------------------------------
  Destroy TCCThreadList
-------------------------------------------------------------------------------}
destructor TCCThreadList.Destroy;
begin
  inherited;
  // Free Lock
  DestroyCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  Add Item
-------------------------------------------------------------------------------}
function TCCThreadList.AddItem: PCCListItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited AddItem;
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited AddItem;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCCThreadList.Clear;
begin
  if AutoLock then
  begin
    Lock;
    try
      inherited;
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  inherited;
end;

{-------------------------------------------------------------------------------
  Delete (Returns TRUE if successful, otherwise FALSE)
-------------------------------------------------------------------------------}
function TCCThreadList.Delete(AIndex: Integer): Boolean;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited Delete(AIndex);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited Delete(AIndex);
end;

{-------------------------------------------------------------------------------
  DeleteItem
-------------------------------------------------------------------------------}
procedure TCCThreadList.DeleteItem(AItem: PCCListItem);
begin
  if AutoLock then
  begin
    Lock;
    try
      inherited DeleteItem(AItem);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  inherited DeleteItem(AItem);
end;

{-------------------------------------------------------------------------------
  Get First
-------------------------------------------------------------------------------}
function TCCThreadList.GetFirst: PCCListItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited GetFirst;
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited GetFirst;
end;

{-------------------------------------------------------------------------------
  Get Item
-------------------------------------------------------------------------------}
function TCCThreadList.GetItem(AIndex: Integer): PCCListItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited GetItem(AIndex);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited GetItem(AIndex);
end;

{-------------------------------------------------------------------------------
  Get Last
-------------------------------------------------------------------------------}
function TCCThreadList.GetLast: PCCListItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited GetLast;
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited GetLast;
end;

{-------------------------------------------------------------------------------
  IndexOf
-------------------------------------------------------------------------------}
function TCCThreadList.IndexOf(AItem: PCCListItem): Integer;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited IndexOf(AItem);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited IndexOf(AItem);
end;

{-------------------------------------------------------------------------------
  Insert  
-------------------------------------------------------------------------------}
function TCCThreadList.Insert(AIndex: Integer): PCCListItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited Insert(AIndex);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited Insert(AIndex);
end;

{-------------------------------------------------------------------------------
  InsertItem
-------------------------------------------------------------------------------}
function TCCThreadList.InsertItem(ARelative: PCCListItem; AInsertMode:
    TCCListInsertMode): PCCListItem;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited InsertItem(ARelative, AInsertMode);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited InsertItem(ARelative, AInsertMode);
end;

{-------------------------------------------------------------------------------
  Lock
-------------------------------------------------------------------------------}
procedure TCCThreadList.Lock;
begin
  EnterCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  UnLock
-------------------------------------------------------------------------------}
procedure TCCThreadList.UnLock;
begin
  LeaveCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  Validate (return TRUE if AItem belongs to this list, otherwise false)
-------------------------------------------------------------------------------}
function TCCThreadList.Validate(AItem: PCCListItem): Boolean;
begin
  if AutoLock then
  begin
    Lock;
    try
      Result:= inherited Validate(AItem);
    finally
      if AutoLock then UnLock;
    end;
  end
  else
  Result:= inherited Validate(AItem);
end;

end.
