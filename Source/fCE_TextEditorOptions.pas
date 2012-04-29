unit fCE_TextEditorOptions;

interface

uses
  // SynEdit
  SynEdit, 
  // Tnt
  TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, ExtCtrls,
  SynEditMiscClasses, MPShellUtilities, Contnrs, TntExtCtrls;
{==============================================================================}

type
  TCETextEditorExportOption = (eoInlineCSSOnCopy, eoInlineCSSOnExport, eoUseBackground, eoConvertTabsToSpaces);
  TCETextEditorExportOptions = set of TCETextEditorExportOption;

  TCETextEditorSettings = class(TPersistent)
  protected
    fBackgroundColor: TColor;
    fEditorList: TObjectList;
    fEnablePlayback: Boolean;
    fExportBackgroundColor: TColor;
    fExportOptions: TCETextEditorExportOptions;
    fExportWrapperClass: String;
    fExtraLineSpacing: Integer;
    fFont: TFont;
    fGutter: TSynGutter;
    fInsertCaret: TSynEditCaretType;
    fMaxUndo: Integer;
    fOptions: TSynEditorOptions;
    fOverwriteCaret: TSynEditCaretType;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    fTabWidth: Integer;
    procedure SetFont(const Value: TFont); virtual;
    procedure SetGutter(const Value: TSynGutter); virtual;
    procedure SetOptions(const Value: TSynEditorOptions); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure AssignTo(Dest : TPersistent); override;
    function GetOptionFlag(AOption: TSynEditorOption): Boolean; virtual;
    procedure RegisterEditor(AEditor: TPersistent); virtual;
    procedure SetOptionFlag(AOption: TSynEditorOption; AValue: Boolean); virtual;
    procedure UnRegisterEditor(AEditor: TPersistent); virtual;
    procedure UpdateEditors; virtual;
  published
    property BackgroundColor: TColor read fBackgroundColor write fBackgroundColor;
    property EnablePlayback: Boolean read fEnablePlayback write fEnablePlayback;
    property ExportBackgroundColor: TColor read fExportBackgroundColor write
        fExportBackgroundColor;
    property ExportOptions: TCETextEditorExportOptions read fExportOptions write
        fExportOptions;
    property ExportWrapperClass: String read fExportWrapperClass write
        fExportWrapperClass;
    property ExtraLineSpacing: Integer read fExtraLineSpacing write
        fExtraLineSpacing;
    property Font: TFont read fFont write SetFont;
    property Gutter: TSynGutter read fGutter write SetGutter;
    property InsertCaret: TSynEditCaretType read fInsertCaret write fInsertCaret;
    property MaxUndo: Integer read fMaxUndo write fMaxUndo;
    property Options: TSynEditorOptions read fOptions write SetOptions;
    property OverwriteCaret: TSynEditCaretType read fOverwriteCaret write
        fOverwriteCaret;
    property RightEdge: Integer read fRightEdge write fRightEdge;
    property RightEdgeColor: TColor read fRightEdgeColor write fRightEdgeColor;
    property TabWidth: Integer read fTabWidth write fTabWidth;
  end;

{-------------------------------------------------------------------------------
  TCETextEditorOptionsForm
-------------------------------------------------------------------------------}
  TCETextEditorOptionsForm = class(TTntForm)
    PageControl: TTntPageControl;
    but_apply: TTntButton;
    but_cancel: TTntButton;
    but_ok: TTntButton;
    sheet_display: TTntTabSheet;
    sheet_options: TTntTabSheet;
    group_right_edge: TTntGroupBox;
    TntLabel2: TTntLabel;
    edit_right_edge: TTntEdit;
    TntLabel3: TTntLabel;
    color_right_edge: TColorBox;
    group_spacing: TTntGroupBox;
    TntLabel4: TTntLabel;
    edit_extra_lines: TTntEdit;
    edit_tab_width: TTntEdit;
    TntLabel5: TTntLabel;
    TntGroupBox1: TTntGroupBox;
    but_font: TTntButton;
    sheet_gutter: TTntTabSheet;
    TntLabel1: TTntLabel;
    check_gutter_visible: TTntCheckBox;
    check_gutter_line_numbers: TTntCheckBox;
    check_gutter_start_at_zero: TTntCheckBox;
    check_gutter_show_leading_zeros: TTntCheckBox;
    color_gutter: TColorBox;
    group_gutter_font: TTntGroupBox;
    check_gutter_font: TTntCheckBox;
    but_gutter_font: TTntButton;
    group_caret: TTntGroupBox;
    TntLabel7: TTntLabel;
    combo_insert_caret: TTntComboBox;
    combo_override_caret: TTntComboBox;
    TntLabel8: TTntLabel;
    check_auto_indent: TTntCheckBox;
    check_enhance_home_key: TTntCheckBox;
    check_enhance_end_key: TTntCheckBox;
    check_group_undo: TTntCheckBox;
    check_half_page_scroll: TTntCheckBox;
    check_right_mouse_moves_cursor: TTntCheckBox;
    check_scroll_past_eol: TTntCheckBox;
    check_scroll_past_eof: TTntCheckBox;
    check_show_scroll_hint: TTntCheckBox;
    check_show_special_chars: TTntCheckBox;
    check_smart_tabs: TTntCheckBox;
    check_smart_tab_delete: TTntCheckBox;
    check_tab_indent: TTntCheckBox;
    check_tabs_to_spaces: TTntCheckBox;
    check_trim_trailing_spaces: TTntCheckBox;
    TntLabel9: TTntLabel;
    color_background: TColorBox;
    FontDlg: TFontDialog;
    group_gutter_gradient: TTntGroupBox;
    color_gutter_gradient_end_color: TColorBox;
    TntLabel10: TTntLabel;
    color_gutter_gradient_start_color: TColorBox;
    TntLabel6: TTntLabel;
    check_gutter_gradient: TTntCheckBox;
    TntLabel11: TTntLabel;
    edit_gutter_gradient_steps: TTntEdit;
    group_border: TTntGroupBox;
    TntLabel12: TTntLabel;
    color_gutter_border_color: TColorBox;
    TntLabel13: TTntLabel;
    combo_gutter_border_style: TTntComboBox;
    TntLabel14: TTntLabel;
    color_font: TColorBox;
    panel_font: TTntPanel;
    TntLabel15: TTntLabel;
    color_gutter_font: TColorBox;
    panel_gutter_font: TTntPanel;
    sheet_export: TTntTabSheet;
    check_inline_css_on_copy: TTntCheckBox;
    check_inline_css_on_export: TTntCheckBox;
    check_export_use_background: TTntCheckBox;
    TntLabel16: TTntLabel;
    edit_export_wrapper_class: TTntEdit;
    color_export_background_color: TColorBox;
    check_export_tabs_to_spaces: TTntCheckBox;
    procedure TntFormCreate(Sender: TObject);
    procedure but_applyClick(Sender: TObject);
    procedure but_fontClick(Sender: TObject);
    procedure but_gutter_fontClick(Sender: TObject);
    procedure color_gutter_fontChange(Sender: TObject);
    procedure color_fontChange(Sender: TObject);
    procedure FontDlgApply(Sender: TObject; Wnd: HWND);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  protected
    fSettings: TCETextEditorSettings;
  public
    procedure AssignValuesFrom(AFrom: TCETextEditorSettings); virtual;
    procedure AssignValuesTo(ATo: TCETextEditorSettings); virtual;
    { Public declarations }
  published
    property Settings: TCETextEditorSettings read fSettings write fSettings;
  end;

{-------------------------------------------------------------------------------
  Global options
-------------------------------------------------------------------------------}
procedure ShowTextEditorOptions(ASettings: TCETextEditorSettings);
function GlobalTextEditorSettings: TCETextEditorSettings;

{==============================================================================}
implementation

uses
  fCE_TextEditor, CE_LanguageEngine;

var
  fGlobalTextEditorSettings: TCETextEditorSettings;
  fTextEditorOptionsForm: TCETextEditorOptionsForm;

{$R *.dfm}

{##############################################################################}
// Global options

{-------------------------------------------------------------------------------
  GlobalTextEditorSettings
-------------------------------------------------------------------------------}
function GlobalTextEditorSettings: TCETextEditorSettings;
begin
  if not assigned(fGlobalTextEditorSettings) then
  fGlobalTextEditorSettings:= TCETextEditorSettings.Create;
  Result:= fGlobalTextEditorSettings;
end;

{-------------------------------------------------------------------------------
  ShowTextEditorOptions
-------------------------------------------------------------------------------}
procedure ShowTextEditorOptions(ASettings: TCETextEditorSettings);
begin
  if not assigned(fTextEditorOptionsForm) then
  begin
    fTextEditorOptionsForm:= TCETextEditorOptionsForm.Create(Application.MainForm);
  end;
  fTextEditorOptionsForm.Settings:= ASettings;
  fTextEditorOptionsForm.AssignValuesFrom(ASettings);
  fTextEditorOptionsForm.ShowModal;
end;

{##############################################################################}
// TCETextEditorOptionsForm

{-------------------------------------------------------------------------------
  On TCETextEditorOptionsForm.Create
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.TntFormCreate(Sender: TObject);
begin
  CEGlobalTranslator.TranslateComponent(Self);
  combo_insert_caret.Items.Strings[0]:= _('Vertical Line');
  combo_insert_caret.Items.Strings[1]:= _('Horizontal Line');
  combo_insert_caret.Items.Strings[2]:= _('Half Block');
  combo_insert_caret.Items.Strings[3]:= _('Block');
  combo_override_caret.Items.Assign(combo_insert_caret.Items);
end;

{-------------------------------------------------------------------------------
  Assign Values From
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.AssignValuesFrom(AFrom:
    TCETextEditorSettings);
begin
  if not assigned(AFrom) then
  Exit;
  // right edge
  edit_right_edge.Text:= IntToStr(AFrom.RightEdge);
  color_right_edge.Selected:= AFrom.RightEdgeColor;
  // spacing
  edit_extra_lines.Text:= IntToStr(AFrom.ExtraLineSpacing);
  edit_tab_width.Text:= IntToStr(AFrom.TabWidth);
  // font
  panel_font.Font.Assign(AFrom.Font);
  panel_font.Caption:= AFrom.Font.Name + ' ' + IntToStr(AFrom.Font.Size) + 'pt';
  color_font.Selected:= AFrom.Font.Color;
  // caret
  combo_insert_caret.ItemIndex:= Ord(AFrom.InsertCaret);
  combo_override_caret.ItemIndex:= Ord(AFrom.OverwriteCaret);
  // background color
  color_background.Selected:= AFrom.BackgroundColor;
  panel_font.Color:= AFrom.BackgroundColor;
  // gutter options
  check_gutter_visible.Checked:= AFrom.Gutter.Visible;
  check_gutter_line_numbers.Checked:= AFrom.Gutter.ShowLineNumbers;
  check_gutter_start_at_zero.Checked:= AFrom.Gutter.ZeroStart;
  check_gutter_show_leading_zeros.Checked:= AFrom.Gutter.LeadingZeros;
  // gutter border
  color_gutter_border_color.Selected:= AFrom.Gutter.BorderColor;
  combo_gutter_border_style.ItemIndex:= Ord(AFrom.Gutter.BorderStyle);
  // gutter gradient
  check_gutter_gradient.Checked:= AFrom.Gutter.Gradient;
  color_gutter_gradient_start_color.Selected:= AFrom.Gutter.GradientStartColor;
  color_gutter_gradient_end_color.Selected:= AFrom.Gutter.GradientEndColor;
  edit_gutter_gradient_steps.Text:= IntToStr(AFrom.Gutter.GradientSteps);
  // gutter font
  check_gutter_font.Checked:= AFrom.Gutter.UseFontStyle;
  panel_gutter_font.Font.Assign(AFrom.Gutter.Font);
  panel_gutter_font.Caption:= AFrom.Gutter.Font.Name + ' ' + IntToStr(AFrom.Gutter.Font.Size) + 'pt';
  // gutter color
  color_gutter.Selected:= AFrom.Gutter.Color;
  panel_gutter_font.Color:= AFrom.Gutter.Color;
  // options
  check_auto_indent.Checked:= eoAutoIndent in AFrom.Options;
  check_enhance_home_key.Checked:= eoEnhanceHomeKey in AFrom.Options;
  check_enhance_end_key.Checked:= eoEnhanceEndKey in AFrom.Options;
  check_group_undo.Checked:= eoGroupUndo in AFrom.Options;
  check_half_page_scroll.Checked:= eoHalfPageScroll in AFrom.Options;
  check_right_mouse_moves_cursor.Checked:= eoRightMouseMovesCursor in AFrom.Options;
  check_scroll_past_eol.Checked:= eoScrollPastEol in AFrom.Options;
  check_scroll_past_eof.Checked:= eoScrollPastEof in AFrom.Options;
  check_show_scroll_hint.Checked:= eoShowScrollHint in AFrom.Options;
  check_show_special_chars.Checked:= eoShowSpecialChars in AFrom.Options;
  check_smart_tabs.Checked:= eoSmartTabs in AFrom.Options;
  check_smart_tab_delete.Checked:= eoSmartTabDelete in AFrom.Options;
  check_tab_indent.Checked:= eoTabIndent in AFrom.Options;
  check_tabs_to_spaces.Checked:= eoTabsToSpaces in AFrom.Options;
  check_trim_trailing_spaces.Checked:= eoTrimTrailingSpaces in AFrom.Options;
  // export
  check_export_use_background.Checked:= eoUseBackground in AFrom.ExportOptions;
  color_export_background_color.Selected:= AFrom.ExportBackgroundColor;
  edit_export_wrapper_class.Text:= AFrom.ExportWrapperClass;
  check_inline_css_on_copy.Checked:= eoInlineCSSOnCopy in AFrom.ExportOptions;
  check_inline_css_on_export.Checked:= eoInlineCSSOnExport in AFrom.ExportOptions;
  check_export_tabs_to_spaces.Checked:= eoConvertTabsToSpaces in AFrom.ExportOptions;
end;

{-------------------------------------------------------------------------------
  Assign Values To
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.AssignValuesTo(ATo: TCETextEditorSettings);
var
  tmpOptions: TSynEditorOptions;
  tmpExportOptions: TCETextEditorExportOptions;

  procedure SetOptionFlag(AOption: TSynEditorOption; AValue: Boolean);
  begin
    if aValue then
    Include(tmpOptions, aOption)
    else
    Exclude(tmpOptions, aOption);
  end;

  procedure SetExportFlag(AOption: TCETextEditorExportOption; AValue: Boolean);
  begin
    if aValue then
    Include(tmpExportOptions, aOption)
    else
    Exclude(tmpExportOptions, aOption);
  end;

begin
  if not assigned(ATo) then
  Exit;
  // right edge
  ATo.RightEdge:= StrToIntDef(edit_right_edge.Text, ATo.RightEdge);
  ATo.RightEdgeColor:= color_right_edge.Selected;
  // spacing
  ATo.ExtraLineSpacing:= StrToIntDef(edit_extra_lines.Text, ATo.ExtraLineSpacing);
  ATo.TabWidth:= StrToIntDef(edit_tab_width.Text, ATo.TabWidth);
  // font
  ATo.Font.Assign(panel_font.Font);
  ATo.Font.Color:= color_font.Selected;
  // caret
  ATo.InsertCaret:= TSynEditCaretType(combo_insert_caret.ItemIndex);
  ATo.OverwriteCaret:= TSynEditCaretType(combo_override_caret.ItemIndex);
  // background color
  ATo.BackgroundColor:= color_background.Selected;
  // gutter options
  ATo.Gutter.Visible:= check_gutter_visible.Checked;
  ATo.Gutter.ShowLineNumbers:= check_gutter_line_numbers.Checked;
  ATo.Gutter.ZeroStart:= check_gutter_start_at_zero.Checked;
  ATo.Gutter.LeadingZeros:= check_gutter_show_leading_zeros.Checked;
  // gutter border
  ATo.Gutter.BorderColor:= color_gutter_border_color.Selected;
  ATo.Gutter.BorderStyle:= TSynGutterBorderStyle(combo_gutter_border_style.ItemIndex);
  // gutter gradient
  ATo.Gutter.Gradient:= check_gutter_gradient.Checked;
  ATo.Gutter.GradientStartColor:= color_gutter_gradient_start_color.Selected;
  ATo.Gutter.GradientEndColor:= color_gutter_gradient_end_color.Selected;
  ATo.Gutter.GradientSteps:= StrToIntDef(edit_gutter_gradient_steps.Text, ATo.Gutter.GradientSteps);
  // gutter font
  ATo.Gutter.UseFontStyle:= check_gutter_font.Checked;
  ATo.Gutter.Font.Assign(panel_gutter_font.Font);
  // gutter color
  ATo.Gutter.Color:= color_gutter.Selected;
  // options
  tmpOptions:= ATo.Options;
  SetOptionFlag(eoAutoIndent, check_auto_indent.Checked);
  SetOptionFlag(eoEnhanceHomeKey, check_enhance_home_key.Checked);
  SetOptionFlag(eoEnhanceEndKey, check_enhance_end_key.Checked);
  SetOptionFlag(eoGroupUndo, check_group_undo.Checked);
  SetOptionFlag(eoHalfPageScroll, check_half_page_scroll.Checked);
  SetOptionFlag(eoRightMouseMovesCursor, check_right_mouse_moves_cursor.Checked);
  SetOptionFlag(eoScrollPastEol, check_scroll_past_eol.Checked);
  SetOptionFlag(eoScrollPastEof, check_scroll_past_eof.Checked);
  SetOptionFlag(eoShowScrollHint, check_show_scroll_hint.Checked);
  SetOptionFlag(eoShowSpecialChars, check_show_special_chars.Checked);
  SetOptionFlag(eoSmartTabs, check_smart_tabs.Checked);
  SetOptionFlag(eoSmartTabDelete, check_smart_tab_delete.Checked);
  SetOptionFlag(eoTabIndent, check_tab_indent.Checked);
  SetOptionFlag(eoTabsToSpaces, check_tabs_to_spaces.Checked);
  SetOptionFlag(eoTrimTrailingSpaces, check_trim_trailing_spaces.Checked);
  ATo.Options:= tmpOptions;
  // export
  tmpExportOptions:= ATo.ExportOptions;
  SetExportFlag(eoInlineCSSOnCopy, check_inline_css_on_copy.Checked);
  SetExportFlag(eoInlineCSSOnExport, check_inline_css_on_export.Checked);
  SetExportFlag(eoUseBackground, check_export_use_background.Checked);
  SetExportFlag(eoConvertTabsToSpaces, check_export_tabs_to_spaces.Checked);
  ATo.ExportOptions:= tmpExportOptions;
  ATo.ExportBackgroundColor:= color_export_background_color.Selected;
  ATo.ExportWrapperClass:= edit_export_wrapper_class.Text;
end;

{-------------------------------------------------------------------------------
  On but_apply.Click
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.but_applyClick(Sender: TObject);
begin
  if assigned(fSettings) then
  begin
    AssignValuesTo(fSettings);
    fSettings.UpdateEditors;
  end;
end;

{-------------------------------------------------------------------------------
  On but_font.Click
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.but_fontClick(Sender: TObject);
begin
  FontDlg.Tag:= 1;
  FontDlg.Font.Assign(panel_font.Font);
  if FontDlg.Execute then
  begin
    panel_font.Font.Assign(FontDlg.Font);
    panel_font.Caption:= FontDlg.Font.Name + ' ' + IntToStr(FontDlg.Font.Size) + 'pt';
  end;
end;

{-------------------------------------------------------------------------------
  On but_gutter_font.Click
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.but_gutter_fontClick(Sender: TObject);
begin
  FontDlg.Tag:= 2;
  FontDlg.Font.Assign(panel_gutter_font.Font);
  if FontDlg.Execute then
  begin
    panel_gutter_font.Font.Assign(FontDlg.Font);
    panel_gutter_font.Caption:= FontDlg.Font.Name + ' ' + IntToStr(FontDlg.Font.Size) + 'pt';
  end;
end;

{-------------------------------------------------------------------------------
  On color_gutter_font.Change
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.color_gutter_fontChange(Sender: TObject);
begin
  panel_gutter_font.Font.Color:= color_gutter_font.Selected;
  panel_gutter_font.Color:= color_gutter.Selected;
end;

{-------------------------------------------------------------------------------
  color_fontChange
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.color_fontChange(Sender: TObject);
begin
  panel_font.Font.Color:= color_font.Selected;
  panel_font.Color:= color_background.Selected;
end;

{-------------------------------------------------------------------------------
  On FontDlg.Apply
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.FontDlgApply(Sender: TObject; Wnd: HWND);
begin
  if FontDlg.Tag = 1 then
  begin
    panel_font.Font.Assign(FontDlg.Font);
    panel_font.Caption:= FontDlg.Font.Name + ' ' + IntToStr(FontDlg.Font.Size) + 'pt';
  end
  else if FontDlg.Tag = 2 then
  begin
    panel_gutter_font.Font.Assign(FontDlg.Font);
    panel_gutter_font.Caption:= FontDlg.Font.Name + ' ' + IntToStr(FontDlg.Font.Size) + 'pt';
  end
  else
  Exit;
  
  but_applyClick(Sender);
end;

{-------------------------------------------------------------------------------
  On FormClose
-------------------------------------------------------------------------------}
procedure TCETextEditorOptionsForm.FormClose(Sender: TObject; var Action:
    TCloseAction);
begin
  Action:= caFree;
  if fTextEditorOptionsForm = Self then
  fTextEditorOptionsForm:= nil;
end;

{##############################################################################}
// TCETextEditorSettings

{-------------------------------------------------------------------------------
  TCETextEditorSettings
-------------------------------------------------------------------------------}
constructor TCETextEditorSettings.Create;
begin
  inherited Create;
  // create editor list
  fEditorList:= TObjectList.Create(false);
  // initialize values
  fFont:= TFont.Create;
  fFont.Name:= 'Courier New';
  fFont.Size:= 10;
  fGutter:= TSynGutter.Create;
  fGutter.Font.Size:= 10;
  fGutter.AutoSize:= true;
  fGutter.ShowLineNumbers:= true;
  fBackgroundColor:= clWindow;
  fExtraLineSpacing:= 0;
  fInsertCaret:= ctVerticalLine;
  fOverwriteCaret:= ctBlock;
  fMaxUndo:= 1024;
  fOptions:= [
    eoAltSetsColumnMode,       //Holding down the Alt Key will put the selection mode into columnar format
    eoAutoIndent,              //Will indent the caret on new lines with the same amount of leading white space as the preceding line
    //eoAutoSizeMaxScrollWidth,  //Automatically resizes the MaxScrollWidth property when inserting text
    //eoDisableScrollArrows,     //Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoDragDropEditing,         //Allows you to select a block of text and drag it within the document to another location
    //eoDropFiles,               //Allows the editor accept OLE file drops
    eoEnhanceHomeKey,          //enhances home key positioning, similar to visual studio
    //eoEnhanceEndKey,           //enhances End key positioning, similar to JDeveloper
    //eoGroupUndo,               //When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    //eoHalfPageScroll,          //When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars,      //if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoKeepCaretX,              //When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    //eoNoCaret,                 //Makes it so the caret is never visible
    //eoNoSelection,             //Disables selecting text
    //eoRightMouseMovesCursor,   //When clicking with the right mouse for a popup menu, move the cursor to that location
    //eoScrollByOneLess,         //Forces scrolling to be one less
    //eoScrollHintFollows,       //The scroll hint follows the mouse when scrolling vertically
    //eoScrollPastEof,           //Allows the cursor to go past the end of file marker
    eoScrollPastEol,           //Allows the cursor to go past the last character into the white space at the end of a line
    eoShowScrollHint,          //Shows a hint of the visible line numbers when scrolling vertically
    //eoShowSpecialChars,        //Shows the special Characters
    eoSmartTabDelete,          //similar to Smart Tabs, but when you delete characters
    //eoSmartTabs,               //When tabbing, the cursor will go to the next non-white space character of the previous line
    //eoSpecialLineDefaultFg,    //disables the foreground text color override when using the OnSpecialLineColor event
    eoTabIndent,               //When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    //eoTabsToSpaces,            //Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces       //Spaces at the end of lines will be trimmed and not saved
  ];
  fRightEdge:= 80;
  fRightEdgeColor:= clSilver;
  fTabWidth:= 2;
  fExportOptions:= [eoInlineCSSOnCopy, eoConvertTabsToSpaces];
  fExportBackgroundColor:= clWindow;
  fExportWrapperClass:= 'ce_export';
  fEnablePlayback:= false;
end;

{-------------------------------------------------------------------------------
  TCETextEditorSettings
-------------------------------------------------------------------------------}
destructor TCETextEditorSettings.Destroy;
begin
  fFont.Free;
  fGutter.Free;
  fEditorList.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Assign
-------------------------------------------------------------------------------}
procedure TCETextEditorSettings.Assign(Source: TPersistent);
begin
  if not assigned(Source) then
  Exit;

  if Source is TCETextEditor then
  begin
    Source:= TCETextEditor(Source).SynMemo;
  end;

  if (Source is TCustomSynEdit) then
  begin
    Self.Font.Assign(TCustomSynEdit(Source).Font);
    Self.Gutter.Assign(TCustomSynEdit(Source).Gutter);
    Self.BackgroundColor := TCustomSynEdit(Source).Color;
    Self.Options := TCustomSynEdit(Source).Options;
    Self.ExtraLineSpacing := TCustomSynEdit(Source).ExtraLineSpacing;
    Self.InsertCaret := TCustomSynEdit(Source).InsertCaret;
    Self.OverwriteCaret := TCustomSynEdit(Source).OverwriteCaret;
    Self.MaxUndo := TCustomSynEdit(Source).MaxUndo;
    Self.RightEdge := TCustomSynEdit(Source).RightEdge;
    Self.RightEdgeColor := TCustomSynEdit(Source).RightEdgeColor;
    Self.TabWidth := TCustomSynEdit(Source).TabWidth;
  end;
end;

{-------------------------------------------------------------------------------
  AssignTo
-------------------------------------------------------------------------------}
procedure TCETextEditorSettings.AssignTo(Dest: TPersistent);
begin
  if not assigned(Dest) then
  Exit;

  if Dest is TCETextEditor then
  begin
    Dest:= TCETextEditor(Dest).SynMemo;
  end;

  if (Dest is TCustomSynEdit) then
  begin
    TCustomSynEdit(Dest).Font.Assign(Self.Font);
    TCustomSynEdit(Dest).Gutter.Assign(Self.Gutter);
    TCustomSynEdit(Dest).Color:= Self.BackgroundColor;
    TCustomSynEdit(Dest).Options:= Self.Options;
    TCustomSynEdit(Dest).ExtraLineSpacing:= Self.ExtraLineSpacing;
    TCustomSynEdit(Dest).InsertCaret:= Self.InsertCaret;
    TCustomSynEdit(Dest).OverwriteCaret:= Self.OverwriteCaret;
    TCustomSynEdit(Dest).MaxUndo:= Self.MaxUndo;
    TCustomSynEdit(Dest).RightEdge:= Self.RightEdge;
    TCustomSynEdit(Dest).RightEdgeColor:= Self.RightEdgeColor;
    TCustomSynEdit(Dest).TabWidth:= Self.TabWidth;
  end;
end;

{-------------------------------------------------------------------------------
  Get Option Flag
-------------------------------------------------------------------------------}
function TCETextEditorSettings.GetOptionFlag(AOption: TSynEditorOption):
    Boolean;
begin
  Result:= AOption in fOptions;
end;

{-------------------------------------------------------------------------------
  Register Editor
-------------------------------------------------------------------------------}
procedure TCETextEditorSettings.RegisterEditor(AEditor: TPersistent);
begin
  if fEditorList.IndexOf(AEditor) = -1 then
  fEditorList.Add(AEditor);
end;

{-------------------------------------------------------------------------------
  Set Font
-------------------------------------------------------------------------------}
procedure TCETextEditorSettings.SetFont(const Value: TFont);
begin
  fFont.Assign(Value);
end;

{-------------------------------------------------------------------------------
  Se Gutter
-------------------------------------------------------------------------------}
procedure TCETextEditorSettings.SetGutter(const Value: TSynGutter);
begin
  fGutter.Assign(Value);
end;

{-------------------------------------------------------------------------------
  Set OptionFlag
-------------------------------------------------------------------------------}
procedure TCETextEditorSettings.SetOptionFlag(AOption: TSynEditorOption;
    AValue: Boolean);
begin
  if AValue then
  Include(fOptions, AOption)
  else
  Exclude(fOptions, AOption);
end;

{-------------------------------------------------------------------------------
  SetOptions
-------------------------------------------------------------------------------}
procedure TCETextEditorSettings.SetOptions(const Value: TSynEditorOptions);
begin
  fOptions:= Value;
end;

{-------------------------------------------------------------------------------
  UnRegister Editor
-------------------------------------------------------------------------------}
procedure TCETextEditorSettings.UnRegisterEditor(AEditor: TPersistent);
begin
  fEditorList.Remove(AEditor);
end;

{-------------------------------------------------------------------------------
  UpdateEditors
-------------------------------------------------------------------------------}
procedure TCETextEditorSettings.UpdateEditors;
var
  i: Integer;
begin
  for i:= 0 to fEditorList.Count - 1 do
  begin
    AssignTo(TPersistent(fEditorList.Items[i]));
  end;
end;

{##############################################################################}

initialization

finalization
  if assigned(fGlobalTextEditorSettings) then
  FreeAndNil(fGlobalTextEditorSettings);

end.
