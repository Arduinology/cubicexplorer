unit fCE_CreateSymlink;

interface

uses
  // CE Units
  CE_ElevatedActions, CE_FileUtils,
  // Tnt
  TntStdCtrls, TntSysUtils, TntFileCtrl,
  // SpTBX
  SpTBXItem, SpTBXControls, SpTBXEditors,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCreateSymlinkDlg = class(TForm)
    but_cancel: TSpTBXButton;
    but_create: TSpTBXButton;
    edit_linkname: TSpTBXEdit;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    edit_targetpath: TSpTBXButtonEdit;
    procedure but_createClick(Sender: TObject);
    procedure edit_targetpathSubEditButton0Click(Sender: TObject);
  private
    { Private declarations }
  public
    ParentLinkFolderPath: WideString;
    { Public declarations }
  end;

procedure ShowCreateSymlinkDialog(AParentFolderPath: WideString; ALinkName:
    WideString; ATargetPath: WideString = '');

implementation

uses
  MPCommonUtilities, CE_LanguageEngine;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Show Create Symbolic link Dialog
-------------------------------------------------------------------------------}
procedure ShowCreateSymlinkDialog(AParentFolderPath: WideString; ALinkName:
    WideString; ATargetPath: WideString = '');
var
  dlg: TCreateSymlinkDlg;
begin
  dlg:= TCreateSymlinkDlg.Create(nil);
  try
    dlg.ParentLinkFolderPath:= AParentFolderPath;
    dlg.edit_linkname.Text:= ALinkName;
    dlg.edit_targetpath.Text:= ATargetPath;
    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On Create button click
-------------------------------------------------------------------------------}
procedure TCreateSymlinkDlg.but_createClick(Sender: TObject);
var
  ws: WideString;
begin
  if FileOrFolderExists(WideIncludeTrailingPathDelimiter(ParentLinkFolderPath) + edit_linkname.Text) then
  begin
    ws:= WideFormat(_('"%s" already exists. Please choose another name.'), [edit_linkname.Text]);
    WideMessageBox(0, _('Duplicate name'), ws, MB_ICONINFORMATION or MB_OK);
    Exit;
  end;
  
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    // Vista or Win7
    if Win32MajorVersion >= 6 then
    begin
      if Elevated_CreateJunction(WideIncludeTrailingPathDelimiter(ParentLinkFolderPath) + edit_linkname.Text,
                                 edit_targetpath.Text,
                                 Self.Handle) then
      begin
        Self.ModalResult:= mrOK;
        Self.CloseModal;
      end;
    end
    else if Win32MajorVersion > 4 then // 2000 and XP
    begin
      if CreateJunction(WideIncludeTrailingPathDelimiter(ParentLinkFolderPath) + edit_linkname.Text,
                     edit_targetpath.Text) then
      begin
        Self.ModalResult:= mrOK;
        Self.CloseModal;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  On target path edit button click
-------------------------------------------------------------------------------}
procedure TCreateSymlinkDlg.edit_targetpathSubEditButton0Click(Sender: TObject);
var
  ws: WideString;
begin
  if edit_targetpath.Text <> '' then
  ws:= edit_targetpath.Text
  else
  ws:= ParentLinkFolderPath;
  
  if WideSelectDirectory('Select target folder', '', ws) then
  begin
    edit_targetpath.Text:= ws;
  end;
end;

end.
