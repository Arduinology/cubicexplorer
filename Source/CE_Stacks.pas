unit CE_Stacks;

interface

uses
  // Tnt
  TntClasses, TntSysUtils,
  // VS Tools
  MPShellUtilities, MPCommonUtilities,
  // System Units
  SysUtils, Classes, Windows, Contnrs;


type
  TCEStackItem = class(TObject)
  private
    fStackName: WideString;
    function GetGroupCount: Integer;
    function GetGroupItems(Index: Integer): TTntStrings;
    function GetGroupName(Index: Integer): WideString;
    procedure SetGroupName(Index: Integer; const Value: WideString);
  protected
    fGroups: TTntStrings;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddGroup(AGroupName: WideString): Integer;
    procedure ClearGroups;
    procedure DeleteGroup(AIndex: Integer);
    procedure LoadFromFile(AFilePath: WideString);
    procedure LoadFromList(AStrings: TTntStrings);
    procedure SaveToFile(AFilePath: WideString);
    procedure SaveToList(AStrings: TTntStrings);
    property GroupCount: Integer read GetGroupCount;
    property GroupItems[Index: Integer]: TTntStrings read GetGroupItems;
    property GroupName[Index: Integer]: WideString read GetGroupName write
        SetGroupName;
    property StackName: WideString read fStackName write fStackName;
  published
  end;

  TCEStacks = class(TObject)
  private
    fStackDirPath: WideString;
    function GetCount: Integer;
    function GetItems(Index: Integer): TCEStackItem;
  protected
    fItems: TObjectList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddStack(AName: WideString): TCEStackItem;
    procedure LoadFromDir(ADirPath: WideString);
    procedure SaveToDir(ADirPath: WideString);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCEStackItem read GetItems;
    property StackDirPath: WideString read fStackDirPath write fStackDirPath;
  end;

var
  GlobalStacks: TCEStacks;

implementation

uses WideStrings;

{-------------------------------------------------------------------------------
  Create an instance of TCEStackItem
-------------------------------------------------------------------------------}
constructor TCEStackItem.Create;
begin
  inherited;
  fGroups:= TTntStringList.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCEStackItem
-------------------------------------------------------------------------------}
destructor TCEStackItem.Destroy;
begin
  ClearGroups;
  fGroups.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add Group
-------------------------------------------------------------------------------}
function TCEStackItem.AddGroup(AGroupName: WideString): Integer;
begin
  Result:= fGroups.AddObject(AGroupName, TTntStringList.Create);
end;

{-------------------------------------------------------------------------------
  Clear Groups
-------------------------------------------------------------------------------}
procedure TCEStackItem.ClearGroups;
var
  i: Integer;
begin
  for i:= 0 to fGroups.Count - 1 do
  fGroups.Objects[i].Free;
  fGroups.Clear;
end;

{-------------------------------------------------------------------------------
  Delete Group
-------------------------------------------------------------------------------}
procedure TCEStackItem.DeleteGroup(AIndex: Integer);
begin
  fGroups.Objects[AIndex].Free;
  fGroups.Delete(AIndex);
end;

{-------------------------------------------------------------------------------
  Get GroupCount
-------------------------------------------------------------------------------}
function TCEStackItem.GetGroupCount: Integer;
begin
  Result:= fGroups.Count;
end;

{-------------------------------------------------------------------------------
  Get GroupItems
-------------------------------------------------------------------------------}
function TCEStackItem.GetGroupItems(Index: Integer): TTntStrings;
begin
  Result:= TTntStrings(fGroups.Objects[Index]);
end;

{-------------------------------------------------------------------------------
  Get GroupName
-------------------------------------------------------------------------------}
function TCEStackItem.GetGroupName(Index: Integer): WideString;
begin
  Result:= fGroups.Strings[Index];
end;

{-------------------------------------------------------------------------------
  Load From File
-------------------------------------------------------------------------------}
procedure TCEStackItem.LoadFromFile(AFilePath: WideString);
var
  list: TTntStrings;
begin
  list:= TTntStringList.Create;
  try
    list.LoadFromFile(AFilePath);
    LoadFromList(list);
  finally
    list.Free;
  end;
end;                            

{-------------------------------------------------------------------------------
  Load From List
-------------------------------------------------------------------------------}
procedure TCEStackItem.LoadFromList(AStrings: TTntStrings);
var
  i, c: Integer;
  ws: WideString;
  items: TTntStrings;
begin
  ClearGroups;
  items:= nil;
  for i:= 0 to AStrings.Count - 1 do
  begin
    ws:= Trim(AStrings.Strings[i]);
    c:= Length(ws);
    if c > 0 then
    begin
      if c > 2 then
      begin
        // group
        if (ws[1] = '[') and (ws[c] = ']') then
        begin
          items:= GroupItems[AddGroup(Copy(ws, 2, c-2))];
        end
        // file item
        else if assigned(items) then
        items.Add(ws);
      end
      // file item
      else if assigned(items) then
      items.Add(ws);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Save To File
-------------------------------------------------------------------------------}
procedure TCEStackItem.SaveToFile(AFilePath: WideString);
var
  list: TTntStrings;
begin
  list:= TTntStringList.Create;
  try
    SaveToList(list);
    list.SaveToFile(AFilePath);
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Save To List
-------------------------------------------------------------------------------}
procedure TCEStackItem.SaveToList(AStrings: TTntStrings);
var
  i, i2: Integer;
  list: TTntStrings;
begin
  if not assigned(AStrings) then
  Exit;

  AStrings.Clear;
  for i:= 0 to GroupCount - 1 do
  begin
    list:= GroupItems[i];
    AStrings.Add('[' + GroupName[i] + ']');
    for i2:= 0 to list.Count - 1 do
    begin
      AStrings.Add(list.Strings[i2]);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set GroupName
-------------------------------------------------------------------------------}
procedure TCEStackItem.SetGroupName(Index: Integer; const Value: WideString);
begin
  fGroups.Strings[Index]:= Value;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEStacks
-------------------------------------------------------------------------------}
constructor TCEStacks.Create;
begin
  inherited;
  fItems:= TObjectList.Create(true);
end;

{-------------------------------------------------------------------------------
  Destroy TCEStacks
-------------------------------------------------------------------------------}
destructor TCEStacks.Destroy;
begin
  fItems.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  
-------------------------------------------------------------------------------}
function TCEStacks.AddStack(AName: WideString): TCEStackItem;
begin
  Result:= TCEStackItem.Create;
  Result.StackName:= AName;
  fItems.Add(Result);
end;

{-------------------------------------------------------------------------------
  Get Count
-------------------------------------------------------------------------------}
function TCEStacks.GetCount: Integer;
begin
  Result:= fItems.Count;
end;

{-------------------------------------------------------------------------------
  Get Items
-------------------------------------------------------------------------------}
function TCEStacks.GetItems(Index: Integer): TCEStackItem;
begin
  Result:= TCEStackItem(fItems.Items[Index]);
end;

{-------------------------------------------------------------------------------
  Load From Dir
-------------------------------------------------------------------------------}
procedure TCEStacks.LoadFromDir(ADirPath: WideString);
var
  sr: TSearchRecW;
  item: TCEStackItem;
begin
  ADirPath:= WideIncludeTrailingBackslash(ADirPath);
  if WideFindFirst(ADirPath + '*.stk', faAnyFile, sr) = 0 then
  begin
    repeat
      item:= AddStack(WideExtractFileName(sr.Name, true));
      try
        item.LoadFromFile(ADirPath + sr.Name);
      except
        // catch exceptions
      end;
    until WideFindNext(sr) <> 0;
    WideFindClose(sr);
  end;
end;

{-------------------------------------------------------------------------------
  Save To Dir
-------------------------------------------------------------------------------}
procedure TCEStacks.SaveToDir(ADirPath: WideString);
var
  item: TCEStackItem;
  i: Integer;
begin
  if ADirPath = '' then
  Exit;
  
  ADirPath:= WideIncludeTrailingPathDelimiter(ADirPath);

  // Create stack dir if it doesn't exist
  if not WideDirectoryExists(ADirPath) then
  WideCreateDir(ADirPath);

  for i:= 0 to Count - 1 do
  begin
    item:= Items[i];
    try
      item.SaveToFile(ADirPath + item.StackName + '.stk');
    //except
    finally
      // catch exceptions
    end;
  end;
end;

{##############################################################################}

initialization
  GlobalStacks:= TCEStacks.Create;

finalization
  FreeAndNil(GlobalStacks);

end.
