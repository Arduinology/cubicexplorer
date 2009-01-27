unit CE_FileSearch;

interface

uses
  // CE Units
  CE_BaseFileView, CE_FindFile,
  // Easy Listview
  EasyListview,
  // VSTools
  MPCommonObjects,  VirtualExplorerEasyListview, MPCommonUtilities,
  VirtualExplorerTree, MPShellUtilities,

  // Tnt Controls
  TntSysUtils,
  // Syn Edit

  // System Units
  Windows, Messages, Classes, SysUtils;

type
  TEasyListviewAccess = class(TEasyListview);

  TCEFileSearchView = class(TCECustomFileView)
  protected
    fPathIndex: Integer;
    procedure DoColumnClick(Button: TCommonMouseButton; ShiftState: TShiftState;
        const Column: TEasyColumn); override;
    procedure DoCustomColumnAdd; override;
    procedure DoCustomColumnCompare(Column: TExplorerColumn; Group: TEasyGroup;
        Item1, Item2: TExplorerItem; var CompareResult: Integer); override;
    procedure DoCustomColumnGetCaption(Column: TExplorerColumn; Item:
        TExplorerItem; var Caption: WideString); override;
    function DoItemCompare(Column: TEasyColumn; Group: TEasyGroup; Item1:
        TEasyItem; Item2: TEasyItem): Integer; override;
  public
    FindFile: TCEFindFile;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnFileMatch(Sender: TObject; Directory: WideString; FileName:
        WideString);
    property PathIndex: Integer read fPathIndex;
  end;


implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEFileSearchView.
-------------------------------------------------------------------------------}
constructor TCEFileSearchView.Create(AOwner: TComponent);
begin
  inherited;
  Self.View:= elsReport;
  Self.Sort.AutoSort:= false;
  FindFile:= TCEFindFile.Create;
  FindFile.OnFileMatch:= OnFileMatch;
  Self.BackGround.CaptionShow:= false;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEFileSearchView.
-------------------------------------------------------------------------------}
destructor TCEFileSearchView.Destroy;
begin
  FindFile.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Called on Column Click;
-------------------------------------------------------------------------------}
procedure TCEFileSearchView.DoColumnClick(Button: TCommonMouseButton;
    ShiftState: TShiftState; const Column: TEasyColumn);
begin
  inherited;
  Self.SortList;
end;

{*------------------------------------------------------------------------------
  Add Custom Column
-------------------------------------------------------------------------------}
procedure TCEFileSearchView.DoCustomColumnAdd;
var
  Column: TExplorerColumn;
begin
  Column := AddColumnProc;
  Column.Caption := 'Path';
  Column.Width := 120;
  Column.Alignment := taLeftJustify;
  Column.Visible := True;
  fPathIndex:= Column.Index;
end;

{*------------------------------------------------------------------------------
  Compare custom column
-------------------------------------------------------------------------------}
procedure TCEFileSearchView.DoCustomColumnCompare(Column: TExplorerColumn;
    Group: TEasyGroup; Item1, Item2: TExplorerItem; var CompareResult: Integer);
begin
  CompareResult:= WideCompareText(Item1.Captions[Column.Index], Item2.Captions[Column.Index]);
  if Column.SortDirection = esdDescending then
  CompareResult := -CompareResult;
end;

{*------------------------------------------------------------------------------
  Get custom column caption
-------------------------------------------------------------------------------}
procedure TCEFileSearchView.DoCustomColumnGetCaption(Column: TExplorerColumn;
    Item: TExplorerItem; var Caption: WideString);
var
  NS: TNamespace;
begin
  //Caption := IntToStr( Integer(Item))
  if Column.Index = fPathIndex then
    if Self.ValidateNamespace(Item, NS) then
    Caption := WideExtractFilePath(NS.NameParseAddress);
end;

{*------------------------------------------------------------------------------
  DoItemCompare
-------------------------------------------------------------------------------}
function TCEFileSearchView.DoItemCompare(Column: TEasyColumn; Group:
    TEasyGroup; Item1: TEasyItem; Item2: TEasyItem): Integer;
var
  ColumnIndex: Integer;
  b: Boolean;
begin
  ColumnIndex := -1;
  if Assigned(Column) and TExplorerColumn(Column).IsCustom then
    DoCustomColumnCompare(TExplorerColumn( Column), Group, TExplorerItem( Item1), TExplorerItem( Item2), Result)
  else
  if Assigned(OnItemCompare) then
    Result:= OnItemCompare(Self, Column, Group, Item1, Item2, b)
  else begin
    if Assigned(Column) then
      ColumnIndex := Column.Index;
    if assigned(TExplorerItem(Item2).Namespace) and assigned(TExplorerItem(Item1).Namespace) then
    begin
      Result := TExplorerItem(Item2).Namespace.ComparePIDL(TExplorerItem(Item1).Namespace.RelativePIDL, False, ColumnIndex);
      if (Result = 0) and (ColumnIndex > 0) then
        Result := TExplorerItem(Item2).Namespace.ComparePIDL(TExplorerItem(Item1).Namespace.RelativePIDL, False, 0);
      if (ColumnIndex > -1) and (Column.SortDirection = esdDescending) then
        Result := -Result
    end
    else
    Result:= 0;
  end
end;

{*------------------------------------------------------------------------------
  Get's called on search file match
-------------------------------------------------------------------------------}
procedure TCEFileSearchView.OnFileMatch(Sender: TObject; Directory: WideString;
    FileName: WideString);
var
  group: TEasyGroup;
  item: TExplorerItem;
begin
  if Self.Groups.Count = 0 then
  begin
    group:= Self.Groups.Add;
    //group.Caption:= Directory;
  end
  else
  begin
    group:= Self.Groups[Self.Groups.Count-1];
  end;

  Self.BeginUpdate;
  try
    item:= group.Items.InsertCustom(group.Items.Count,TExplorerItem) as TExplorerItem;
    item.Namespace:= TNamespace.CreateFromFileName(Directory + FileName);
  finally
    Self.EndUpdate(false);
  end;  
end;

end.
