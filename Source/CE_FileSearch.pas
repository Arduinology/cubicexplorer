//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
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
//  The Original Code is CE_FileSearch.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_FileSearch;

interface

uses
  // CE Units
  CE_BaseFileView, CE_FindFile, CE_Utils,
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
  Self.CellSizes.Report.Height:= SmallShellIconSize + 1;
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
  inherited;
end;

{*------------------------------------------------------------------------------
  Get custom column caption
-------------------------------------------------------------------------------}
procedure TCEFileSearchView.DoCustomColumnGetCaption(Column: TExplorerColumn;
    Item: TExplorerItem; var Caption: WideString);
var
  NS: TNamespace;
begin
  inherited;
  if Column.Index = fPathIndex then
  begin
    if Self.ValidateNamespace(Item, NS) then
    Caption := WideExtractFilePath(NS.NameParseAddress);
  end;
end;

{*------------------------------------------------------------------------------
  DoItemCompare
-------------------------------------------------------------------------------}
function TCEFileSearchView.DoItemCompare(Column: TEasyColumn; Group:
    TEasyGroup; Item1: TEasyItem; Item2: TEasyItem): Integer;
var
  ColIndex: Integer;
begin
  ColIndex:= 0;

  Result := 0;
  if Assigned(Column) then
  ColIndex:= Column.Index
  else
  ColIndex:= 0;

  if ColIndex = 3 then
  begin
    Result:= CompareFileTime(TExplorerItem(Item1).Namespace.LastWriteTimeRaw, TExplorerItem(Item2).Namespace.LastWriteTimeRaw);
  end
  else
  if ColIndex = 1 then
  begin
    if TExplorerItem(Item1).Namespace.SizeOfFileInt64 > TExplorerItem(Item2).Namespace.SizeOfFileInt64 then
      Result:= 1
    else
    if TExplorerItem(Item1).Namespace.SizeOfFileInt64 < TExplorerItem(Item2).Namespace.SizeOfFileInt64 then
      Result:= -1
  end
  else
  begin
    Result:= WideStrComp(PWidechar(Item1.Captions[ColIndex]), PWideChar(Item2.Captions[ColIndex]));
  end;

  if (ColIndex > 0) and (Result = 0) then
  Result:= WideStrComp(PWidechar(Item1.Captions[0]), PWideChar(Item2.Captions[0]));

  if Column.SortDirection = esdDescending then
  Result:= -Result;
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
