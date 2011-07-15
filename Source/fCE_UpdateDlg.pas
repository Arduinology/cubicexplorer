unit fCE_UpdateDlg;

interface

uses
  // CE Units
  CE_ArchiveTree,
  // VT
  VirtualTrees,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TntStdCtrls;

type

  TCEUpdateDlg = class(TForm)
    but_cancel: TTntButton;
    but_update: TTntButton;
    Panel1: TPanel;
    TntLabel1: TTntLabel;
    procedure but_updateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fDestinationDir: WideString;
    fFailed: Boolean;
  public
    ArchiveTree: TCEArchiveTree;
    procedure OpenArchive(AFilePath: WideString);
    property DestinationDir: WideString read fDestinationDir write fDestinationDir;
  published
    property Failed: Boolean read fFailed;
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  On FormCreate
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ArchiveTree:= TCEArchiveTree.Create(Self);
  ArchiveTree.Parent:= Panel1;
  ArchiveTree.Align:= alClient;
  ArchiveTree.CheckBoxSelection:= true;
  for i:= 1 to ArchiveTree.Header.Columns.Count - 1 do
  begin
    ArchiveTree.Header.Columns.Items[i].Options:= ArchiveTree.Header.Columns.Items[i].Options - [coVisible];
  end;
  ArchiveTree.Header.AutoSizeIndex:= 0;
  ArchiveTree.Header.Options:= ArchiveTree.Header.Options + [hoAutoResize] - [hoVisible];
  //ArchiveTree.TreeOptions.PaintOptions:= ArchiveTree.TreeOptions.PaintOptions - [toShowRoot];
end;

{-------------------------------------------------------------------------------
  Open Archive
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.OpenArchive(AFilePath: WideString);
begin
  ArchiveTree.OpenArchive(AFilePath);
  ArchiveTree.CheckAll;
  //ArchiveTree.FullExpand;
end;

{-------------------------------------------------------------------------------
  On but_update.Click
-------------------------------------------------------------------------------}
procedure TCEUpdateDlg.but_updateClick(Sender: TObject);
begin
  ArchiveTree.ExtractCheckedTo(DestinationDir, true);
end;


end.
