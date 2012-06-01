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
//  The Original Code is fCE_OptionsPage_Display_FileView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_OptionsPage_Display_FileView;

interface

uses
  // CubicCore
  ccClasses, ccStrings,
  // CE Units
  fCE_OptionsDialog, fCE_OptionsCustomPage, CE_LanguageEngine, 
  // Tnt
  TntStdCtrls, TntComCtrls, TntSysUtils,
  // Virtual Trees
  VirtualTrees,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, TntExtCtrls;

type
  PExtListData = ^AExtListData;
  AExtListData = record
    fExtensions: WideString;
    fBold: Boolean;
    fItalic: Boolean;
    fUnderline: Boolean;
    fColor: TColor;
  end;

  TCE_OptionsPage_Display_FileView = class(TCEOptionsCustomPage)
    check_selectprev: TTntCheckBox;
    check_autoselect: TTntCheckBox;
    check_autosize_liststyle: TTntCheckBox;
    check_sortfoldersfirst: TTntCheckBox;
    check_infotips: TTntCheckBox;
    check_singleclick: TTntCheckBox;
    check_perfolder: TTntCheckBox;
    TntPageControl1: TTntPageControl;
    sheet_options: TTntTabSheet;
    sheet_colors: TTntTabSheet;
    check_browse_zip: TTntCheckBox;
    list_exts: TVirtualStringTree;
    but_add: TTntButton;
    but_delete: TTntButton;
    check_ext_bold: TTntCheckBox;
    check_ext_italic: TTntCheckBox;
    check_ext_underline: TTntCheckBox;
    color_extension: TColorBox;
    check_extension_colors: TTntCheckBox;
    TntLabel2: TTntLabel;
    sheet_display: TTntTabSheet;
    TntLabel1: TTntLabel;
    combo_sizeformat: TTntComboBox;
    color_background: TColorBox;
    TntLabel3: TTntLabel;
    TntGroupBox1: TTntGroupBox;
    TntLabel14: TTntLabel;
    but_font: TTntButton;
    color_font: TColorBox;
    panel_font: TTntPanel;
    FontDlg: TFontDialog;
    check_gridlines: TTntCheckBox;
    check_fullrowselect: TTntCheckBox;
    check_remember_thumbs: TTntCheckBox;
    procedure HandleChange(Sender: TObject);
    procedure list_extsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure list_extsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure list_extsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure but_addClick(Sender: TObject);
    procedure list_extsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure HandleExtColorChange(Sender: TObject);
    procedure but_deleteClick(Sender: TObject);
    procedure list_extsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure list_extsEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure FontDlgApply(Sender: TObject; Wnd: HWND);
    procedure color_fontSelect(Sender: TObject);
    procedure but_fontClick(Sender: TObject);
    procedure color_backgroundSelect(Sender: TObject);
  private
    { Private declarations }
  protected
    fExtValuesChanging: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

var
  CE_OptionsPage_Display_FileView: TCE_OptionsPage_Display_FileView;

implementation

uses
  fCE_FileView, VirtualExplorerEasyListview;

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Display
-------------------------------------------------------------------------------}
constructor TCE_OptionsPage_Display_FileView.Create(AOwner: TComponent);
begin
  inherited;
  PageName:= _('Fileview');
  PageTitle:= _('Fileview Settings');
  PagePath:= 'Display/Fileview';
  ImageIndex:= 5;
  combo_sizeformat.Items.Add(_('Default'));
  combo_sizeformat.Items.Add(_('Explorer'));
  combo_sizeformat.Items.Add(_('Actual'));
  combo_sizeformat.Items.Add(_('Disk Usage'));
  combo_sizeformat.Items.Add(_('Text'));

  list_exts.NodeDataSize:= SizeOf(AExtListData);

  fExtValuesChanging:= false;
end;

{-------------------------------------------------------------------------------
  On FontDlg.Apply
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.FontDlgApply(Sender: TObject;
  Wnd: HWND);
begin
  panel_font.Font.Assign(FontDlg.Font);
  panel_font.Caption:= FontDlg.Font.Name + ' ' + IntToStr(FontDlg.Font.Size) + 'pt';
  ApplySettings;
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.ApplySettings;

  procedure WriteExtensionColors;
  var
    node: PVirtualNode;
    data: PExtListData;
    value: WideString;
  begin
    node:= list_exts.GetFirst;
    value:= '';
    while assigned(node) do
    begin
      data:= list_exts.GetNodeData(node);
      if value <> '' then
      value:= value + '|';
      
      value:= value + WideReplaceChar(',', ';', data.fExtensions) + ',' +  // exts
              IntToStr(data.fColor) + ',' +                        // color
              BoolToStr(data.fBold) + ',' +                        // bold
              BoolToStr(data.fItalic) + ',' +                      // italic
              BoolToStr(data.fUnderline);                          // underline

      node:= list_exts.GetNext(node);
    end;
    GlobalFileViewSettings.ExtensionColors:= value;
  end;

begin
  GlobalFileViewSettings.BeginUpdate;
  try
    // options
    GlobalFileViewSettings.FullRowSelect:= check_fullrowselect.Checked;
    GlobalFileViewSettings.SelectPreviousFolder:= check_selectprev.Checked;
    GlobalFileViewSettings.AutoSelectFirstItem:= check_autoselect.Checked;
    GlobalFileViewSettings.AutosizeListViewStyle:= check_autosize_liststyle.Checked;
    GlobalFileViewSettings.SortFolderFirstAlways:= check_sortfoldersfirst.Checked;
    GlobalFileViewSettings.ShowInfoTips:= check_infotips.Checked;
    GlobalFileViewSettings.SingleClickBrowse:= check_singleclick.Checked;
    GlobalFileViewSettings.PerFolderSettings:= check_perfolder.Checked;
    GlobalFileViewSettings.ShowGridLines:= check_gridlines.Checked;
    GlobalFileViewSettings.BrowseZipFolders:= check_browse_zip.Checked;
    GlobalFileViewSettings.FileSizeFormat:= TVirtualFileSizeFormat(combo_sizeformat.ItemIndex);
    GlobalFileViewSettings.Thumbnails.UseStorage:= check_remember_thumbs.Checked;

    // display
    GlobalFileViewSettings.BackgroundColor:= color_background.Selected;
    GlobalFileViewSettings.Font.Assign(panel_font.Font);

    // colors
    GlobalFileViewSettings.ExtensionColorsEnabled:= check_extension_colors.Checked;
    WriteExtensionColors;
  finally
    GlobalFileViewSettings.EndUpdate(true);
  end;
end;

{-------------------------------------------------------------------------------
  Handle Change
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.HandleChange(Sender: TObject);
begin
  // enables the Apply button.
  inherited;
end;

{-------------------------------------------------------------------------------
  HandleExtColorChange
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.HandleExtColorChange(
  Sender: TObject);
var
  data: PExtListData;
begin
  if fExtValuesChanging then
  Exit;
  
  if Assigned(list_exts.FocusedNode) then
  begin
    data:= list_exts.GetNodeData(list_exts.FocusedNode);
    data.fBold:= check_ext_bold.Checked;
    data.fItalic:= check_ext_italic.Checked;
    data.fUnderline:= check_ext_underline.Checked;
    data.fColor:= color_extension.Selected;
  end;
  HandleChange(Sender);
end;

{-------------------------------------------------------------------------------
  On but_add.Click
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.but_addClick(Sender: TObject);
var
  node: PVirtualNode;
begin
  node:= list_exts.AddChild(nil);
  list_exts.Selected[node]:= true;
  list_exts.FocusedNode:= node;
  list_exts.EditNode(node, 0);
end;

{-------------------------------------------------------------------------------
  On but_delete.Click
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.but_deleteClick(Sender: TObject);
begin
  list_exts.DeleteSelectedNodes;
  HandleChange(Sender);
end;

{-------------------------------------------------------------------------------
  On but_font.Click
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.but_fontClick(Sender: TObject);
begin
  FontDlg.Font.Assign(panel_font.Font);
  if FontDlg.Execute then
  begin
    panel_font.Font.Assign(FontDlg.Font);
    panel_font.Caption:= FontDlg.Font.Name + ' ' + IntToStr(FontDlg.Font.Size) + 'pt';
  end;
end;

{-------------------------------------------------------------------------------
  On color_font.Select
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.color_fontSelect(Sender: TObject);
begin
  panel_font.Font.Color:= color_font.Selected;
  color_extension.DefaultColorColor:= panel_font.Font.Color;
  HandleChange(Sender);
end;

{-------------------------------------------------------------------------------
  On color_background.Select
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.color_backgroundSelect(
  Sender: TObject);
begin
  panel_font.Color:= color_background.Selected;
  HandleChange(Sender);
end;

{-------------------------------------------------------------------------------
  On list_exts.FreeNode
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.list_extsFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  data: PExtListData;
begin
  data:= Sender.GetNodeData(Node);
  data.fExtensions:= '';
end;

{-------------------------------------------------------------------------------
  On list_exts.EditCancelled
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.list_extsEditCancelled(
  Sender: TBaseVirtualTree; Column: TColumnIndex);
var
  data: PExtListData;
begin
  if assigned(Sender.FocusedNode) then
  begin
    data:= Sender.GetNodeData(Sender.FocusedNode);
    if data.fExtensions = '' then
    Sender.DeleteNode(Sender.FocusedNode);
  end;
end;

{-------------------------------------------------------------------------------
  On list_exts.Edited
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.list_extsEdited(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  data: PExtListData;
begin
  data:= Sender.GetNodeData(Node);
  if data.fExtensions = '' then
  Sender.DeleteNode(Node);
end;

{-------------------------------------------------------------------------------
  On list_exts.FocusChanged
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.list_extsFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  data: PExtListData;
begin
  if assigned(Node) then
  begin
    data:= Sender.GetNodeData(Node);
    fExtValuesChanging:= true;
    try
      check_ext_bold.Checked:= data.fBold;
      check_ext_italic.Checked:= data.fItalic;
      check_ext_underline.Checked:= data.fUnderline;
      color_extension.Selected:= data.fColor;
    finally
      fExtValuesChanging:= false;
    end;
  end;

  but_delete.Enabled:= assigned(Node);
  check_ext_bold.Enabled:= but_delete.Enabled;
  check_ext_italic.Enabled:= but_delete.Enabled;
  check_ext_underline.Enabled:= but_delete.Enabled;
  color_extension.Enabled:= but_delete.Enabled;
end;

{-------------------------------------------------------------------------------
  On list_exts.GetText
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.list_extsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  data: PExtListData;
begin
  data:= Sender.GetNodeData(Node);
  CellText:= data.fExtensions;
end;

{-------------------------------------------------------------------------------
  On list_exts.NewText
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.list_extsNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);
var
  data: PExtListData;
begin
  if NewText <> '' then
  begin
    data:= Sender.GetNodeData(Node);
    data.fExtensions:= WideReplaceChar(' ', '', NewText);
  end
  else
  begin
    list_exts.DeleteNode(Node);
  end;
  HandleChange(Sender);
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TCE_OptionsPage_Display_FileView.RefreshSettings;

  procedure ReadExtensionColors;
  var
    list: TCCStringList;
    i: Integer;
    ws, value: WideString;
    data: PExtListData;
    node: PVirtualNode;
  begin
    list_exts.Clear;
    list:= TCCStringList.Create;
    try
      list.Delimiter:= '|';
      list.DelimitedText:= GlobalFileViewSettings.ExtensionColors;
      for i:= 0 to list.Count - 1 do
      begin
        ws:= list.Strings[i];
        value:= WideGetNextItem(ws, ',');
        value:= WideReplaceChar(';', ',', value);
        if value <> '' then
        begin
          node:= list_exts.AddChild(nil);
          data:= list_exts.GetNodeData(node);

          data.fExtensions:= value;
          data.fColor:= StrToIntDef(WideGetNextItem(ws, ','), clWindowText); // color
          data.fBold:= StrToBoolDef(WideGetNextItem(ws, ','), false);  // bold
          data.fItalic:= StrToBoolDef(WideGetNextItem(ws, ','), false);  // italic
          data.fUnderline:= StrToBoolDef(WideGetNextItem(ws, ','), false);  // underline
        end;
      end;
    finally
      list.Free;
    end;
  end;

begin
  // options
  check_fullrowselect.Checked:= GlobalFileViewSettings.FullRowSelect;
  check_selectprev.Checked:= GlobalFileViewSettings.SelectPreviousFolder;
  check_autoselect.Checked:= GlobalFileViewSettings.AutoSelectFirstItem;
  check_autosize_liststyle.Checked:= GlobalFileViewSettings.AutosizeListViewStyle;
  check_sortfoldersfirst.Checked:= GlobalFileViewSettings.SortFolderFirstAlways;
  check_infotips.Checked:= GlobalFileViewSettings.ShowInfoTips;
  check_singleclick.Checked:= GlobalFileViewSettings.SingleClickBrowse;
  check_perfolder.Checked:= GlobalFileViewSettings.PerFolderSettings;
  check_gridlines.Checked:= GlobalFileViewSettings.ShowGridLines;
  check_browse_zip.Checked:= GlobalFileViewSettings.BrowseZipFolders;
  combo_sizeformat.ItemIndex:= Ord(GlobalFileViewSettings.FileSizeFormat);
  check_remember_thumbs.Checked:= GlobalFileViewSettings.Thumbnails.UseStorage;

  // display
  color_background.Handle; // <-- hack around bug in TColorBox (http://qc.embarcadero.com/wc/qcmain.aspx?d=85895).
  color_background.Selected:= GlobalFileViewSettings.BackgroundColor;
  panel_font.Color:= GlobalFileViewSettings.BackgroundColor;
  panel_font.Font.Assign(GlobalFileViewSettings.Font);
  panel_font.Caption:= GlobalFileViewSettings.Font.Name + ' ' + IntToStr(GlobalFileViewSettings.Font.Size) + 'pt';
  color_font.Handle;
  color_font.Selected:= panel_font.Font.Color;
  color_extension.DefaultColorColor:= color_font.Selected;

  // colors
  check_extension_colors.Checked:= GlobalFileViewSettings.ExtensionColorsEnabled;
  ReadExtensionColors;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCE_OptionsPage_Display_FileView);

finalization

end.
