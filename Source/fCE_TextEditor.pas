unit fCE_TextEditor;

interface

uses
  // CE
  CE_ToolbarEditorItems, CE_Toolbar, CE_SynExporters, fCE_TextEditorOptions,
  // SynEdit
  SynEditRegexSearch, SynEditMiscClasses, SynEditSearch, SynEdit, SynMemo,
  SynEditTypes, SynUnicode,
  // SynEdit Highlighters
  SynEditHighlighter, SynHighlighterPas,
  SynHighlighterCobol, SynHighlighterFortran, SynHighlighterURI,
  SynHighlighterRC, SynHighlighterInno, SynHighlighterIni, SynHighlighterDfm,
  SynHighlighterAsm, SynHighlighterUNIXShellScript, SynHighlighterRuby,
  SynHighlighterTclTk, SynHighlighterPython, SynHighlighterPerl,
  SynHighlighterBat, SynHighlighterXML, SynHighlighterVBScript,
  SynHighlighterPHP, SynHighlighterJScript, SynHighlighterHtml,
  SynHighlighterCSS, SynHighlighterCS, SynHighlighterJava, SynHighlighterVB,
  SynHighlighterSQL, SynHighlighterCpp, SynHighlighterMulti,
  // SpTBX
  SpTBXItem, TB2Toolbar, TB2Dock, TB2Item, SpTBXEditors, SpTBXSkins,
  // Tnt
  TntActnList, TntForms,
  // Png
  PngImageList,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ImgList, ExtCtrls, ActnList, StrUtils, Menus, SynURIOpener, StdCtrls;

type
{-------------------------------------------------------------------------------
  TCETextEditor
-------------------------------------------------------------------------------}
  TCETextEditor = class(TTntForm)
    TopDock: TSpTBXDock;
    toolbar_main: TCEToolbar;
    StatusBar: TSpTBXStatusBar;
    SynMemo: TSynMemo;
    LeftDock: TSpTBXDock;
    RightDock: TSpTBXDock;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    but_new: TSpTBXItem;
    but_open: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    but_save: TSpTBXItem;
    but_saveas: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    but_close: TSpTBXItem;
    but_reload: TSpTBXItem;
    label_status: TSpTBXLabelItem;
    DockBottom: TSpTBXDock;
    toolbar_find: TCEToolbar;
    but_close_search: TSpTBXItem;
    PngImageList1: TPngImageList;
    but_search_next: TSpTBXItem;
    but_search_prev: TSpTBXItem;
    label_find: TSpTBXLabelItem;
    but_search_options: TSpTBXSubmenuItem;
    check_case_sensitive: TSpTBXItem;
    check_whole_word: TSpTBXItem;
    check_selected_text_only: TSpTBXItem;
    check_regex: TSpTBXItem;
    but_undo: TSpTBXItem;
    but_redo: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    but_copy: TSpTBXItem;
    but_cut: TSpTBXItem;
    but_paste: TSpTBXItem;
    but_delete: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    but_select_all: TSpTBXItem;
    ActionList: TTntActionList;
    act_new: TTntAction;
    act_open: TTntAction;
    act_reload: TTntAction;
    act_save: TTntAction;
    act_saveas: TTntAction;
    act_close: TTntAction;
    act_undo: TTntAction;
    act_redo: TTntAction;
    act_copy: TTntAction;
    act_cut: TTntAction;
    act_paste: TTntAction;
    act_delete: TTntAction;
    act_select_all: TTntAction;
    act_find: TTntAction;
    act_find_next: TTntAction;
    act_find_previous: TTntAction;
    act_replace: TTntAction;
    SpTBXSubmenuItem3: TSpTBXSubmenuItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    toolbar_replace: TCEToolbar;
    but_replace_close: TSpTBXItem;
    label_replace: TSpTBXLabelItem;
    but_replace: TSpTBXItem;
    but_replace_all: TSpTBXItem;
    CEToolbarFixedSpacerItem1: TCEToolbarFixedSpacerItem;
    label_position: TSpTBXLabelItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    label_stats: TSpTBXLabelItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    but_insertmode: TSpTBXItem;
    CEToolbarFixedSpacerItem2: TCEToolbarFixedSpacerItem;
    CEToolbarFixedSpacerItem3: TCEToolbarFixedSpacerItem;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    label_modified: TSpTBXLabelItem;
    sep_modified: TSpTBXSeparatorItem;
    StatusTimer: TTimer;
    NormalSearch: TSynEditSearch;
    RegexSearch: TSynEditRegexSearch;
    check_wrap_around: TSpTBXItem;
    SpTBXSubmenuItem4: TSpTBXSubmenuItem;
    act_wordwrap: TTntAction;
    SpTBXItem6: TSpTBXItem;
    SpTBXSeparatorItem11: TSpTBXSeparatorItem;
    label_format: TSpTBXLabelItem;
    sub_highlighter: TSpTBXSubmenuItem;
    SynCppSyn1: TSynCppSyn;
    SynPasSyn1: TSynPasSyn;
    SynSQLSyn1: TSynSQLSyn;
    SynVBSyn1: TSynVBSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynCSSyn1: TSynCSSyn;
    SynCssSyn1: TSynCssSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynVBScriptSyn1: TSynVBScriptSyn;
    SynXMLSyn1: TSynXMLSyn;
    SynBatSyn1: TSynBatSyn;
    SynPerlSyn1: TSynPerlSyn;
    SynPythonSyn1: TSynPythonSyn;
    SynTclTkSyn1: TSynTclTkSyn;
    SynRubySyn1: TSynRubySyn;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
    SynAsmSyn1: TSynAsmSyn;
    SynDfmSyn1: TSynDfmSyn;
    SynIniSyn1: TSynIniSyn;
    SynInnoSyn1: TSynInnoSyn;
    SynRCSyn1: TSynRCSyn;
    SynURISyn1: TSynURISyn;
    SynFortranSyn1: TSynFortranSyn;
    SynCobolSyn1: TSynCobolSyn;
    SynMultiHighlighter: TSynMultiSyn;
    SpTBXSeparatorItem12: TSpTBXSeparatorItem;
    SpTBXItem7: TSpTBXItem;
    act_export_html: TTntAction;
    act_copy_as_html: TTntAction;
    SpTBXSeparatorItem13: TSpTBXSeparatorItem;
    SpTBXItem8: TSpTBXItem;
    act_options: TTntAction;
    SpTBXSeparatorItem14: TSpTBXSeparatorItem;
    SpTBXItem9: TSpTBXItem;
    act_special_chars: TTntAction;
    SpTBXItem10: TSpTBXItem;
    EditorPopupMenu: TSpTBXPopupMenu;
    SpTBXItem11: TSpTBXItem;
    SpTBXItem12: TSpTBXItem;
    SpTBXItem13: TSpTBXItem;
    SpTBXSeparatorItem15: TSpTBXSeparatorItem;
    SpTBXItem14: TSpTBXItem;
    SpTBXItem15: TSpTBXItem;
    SpTBXSeparatorItem16: TSpTBXSeparatorItem;
    act_bookmark_1: TTntAction;
    act_bookmark_2: TTntAction;
    act_bookmark_3: TTntAction;
    act_bookmark_4: TTntAction;
    act_bookmark_5: TTntAction;
    act_bookmark_6: TTntAction;
    act_bookmark_7: TTntAction;
    act_bookmark_8: TTntAction;
    act_bookmark_9: TTntAction;
    SpTBXSubmenuItem5: TSpTBXSubmenuItem;
    SpTBXItem16: TSpTBXItem;
    SpTBXItem17: TSpTBXItem;
    SpTBXItem18: TSpTBXItem;
    SpTBXItem19: TSpTBXItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXItem22: TSpTBXItem;
    SpTBXItem23: TSpTBXItem;
    SpTBXItem24: TSpTBXItem;
    toolbar_bookmarks: TSpTBXToolbar;
    SpTBXSeparatorItem17: TSpTBXSeparatorItem;
    SpTBXItem26: TSpTBXItem;
    SpTBXItem27: TSpTBXItem;
    SpTBXItem28: TSpTBXItem;
    SpTBXItem29: TSpTBXItem;
    SpTBXItem30: TSpTBXItem;
    SpTBXItem31: TSpTBXItem;
    SpTBXItem32: TSpTBXItem;
    SpTBXItem33: TSpTBXItem;
    SpTBXItem34: TSpTBXItem;
    act_show_statusbar: TTntAction;
    act_playback_enabled: TTntAction;
    act_show_bookmark_toolbar: TTntAction;
    SpTBXItem25: TSpTBXItem;
    SpTBXItem35: TSpTBXItem;
    SpTBXSeparatorItem18: TSpTBXSeparatorItem;
    SpTBXItem36: TSpTBXItem;
    SpTBXSubmenuItem6: TSpTBXSubmenuItem;
    act_replace_selected: TTntAction;
    act_replace_all: TTntAction;
    URIOpener: TSynURIOpener;
    edit_find: TSpTBXEdit;
    TBControlItem1: TTBControlItem;
    edit_replace: TSpTBXEdit;
    TBControlItem2: TTBControlItem;
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure edit_findKeyPress(Sender: TObject; var Key: Char);
    procedure editChange(Sender: TObject);
    procedure edit_replaceKeyPress(Sender: TObject; var Key: Char);
    procedure StatusTimerTimer(Sender: TObject);
    procedure SynMemoChange(Sender: TObject);
    procedure SynMemoKeyPress(Sender: TObject; var Key: WideChar);
    procedure SynMemoStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    { Private declarations }
  protected
    fActiveFileName: WideString;
    fActiveFilePath: WideString;
    fActiveHighlighter: Integer;
    fOnActiveFileChange: TNotifyEvent;
    fOnEnablePlaybackChanged: TNotifyEvent;
    fPosStr: WideString;
    fSettings: TCETextEditorSettings;
    fStatsStr: WideString;
    procedure DoActiveFileChange;
    procedure DoHighlighterClick(Sender: TObject); virtual;
    procedure DoHighlighterMenuPopup(Sender: TTBCustomItem; FromLink: Boolean);
        virtual;
    procedure SetActiveHighlighter(const Value: Integer);
    procedure UpdateCaption; virtual;
  public
    Highlighters: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ChangeStatus(AStatus: WideString); virtual;
    procedure Close; virtual;
    function Find(AText: WideString; ABackward: Boolean = false): Boolean; virtual;
    function FindNext: Boolean; virtual;
    function FindPrevious: Boolean; virtual;
    function OpenFile(AFilePath: WideString): Boolean; virtual;
    procedure PopuplateHighlighters(ASubMenu: TTBCustomItem); virtual;
    procedure Reload; virtual;
    function Save(ASaveAs: Boolean = false): Boolean; virtual;
    function SaveFile(AFilePath: WideString): Boolean; virtual;
    procedure ChangeAutoHighlighter; virtual;
    procedure ExportToHTML(AToClipboard: Boolean; ASelectedOnly: Boolean;
        AFilePath: WideString = ''; ATitle: WideString = ''); virtual;
    procedure ReplaceAll(AFind, AReplace: WideString); virtual;
    procedure ShowSearchReplace(AShowReplace: Boolean = false); virtual;
    function CanClose: Boolean; virtual;
    procedure HideSearchReplace(AHideReplace: Boolean = false); virtual;
    procedure UpdateStats; virtual;
    property ActiveFileName: WideString read fActiveFileName;
    property ActiveFilePath: WideString read fActiveFilePath;
    property ActiveHighlighter: Integer read fActiveHighlighter write
        SetActiveHighlighter;
    { Public declarations }
  published
    property Settings: TCETextEditorSettings read fSettings write fSettings;
    property OnActiveFileChange: TNotifyEvent read fOnActiveFileChange write
        fOnActiveFileChange;
    property OnEnablePlaybackChanged: TNotifyEvent read fOnEnablePlaybackChanged
        write fOnEnablePlaybackChanged;
  end;

{-------------------------------------------------------------------------------
  Public Methods
-------------------------------------------------------------------------------}
function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;

implementation

uses
  TntDialogs, ccFileUtils, CE_LanguageEngine, CE_VistaFuncs;

{$R *.dfm}

{##############################################################################}
// Public Methods

{-------------------------------------------------------------------------------
  Get Highlighter From File Extension
-------------------------------------------------------------------------------}
function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;
var
  ExtLen: integer;
  i, j: integer;
  Highlighter: TSynCustomHighlighter;
  Filter: string;
begin
  Extension:= LowerCase(Extension);
  ExtLen:= Length(Extension);
  if Assigned(AHighlighters) and (ExtLen > 0) then
  begin
    for i:= 0 to AHighlighters.Count - 1 do
    begin
      if (AHighlighters.Objects[i] is TSynCustomHighlighter) then
      begin
        Highlighter:= TSynCustomHighlighter(AHighlighters.Objects[i]);
        Filter:= LowerCase(Highlighter.DefaultFilter);
        j:= Pos('|', Filter);
        if j > 0 then
        begin
          Delete(Filter, 1, j);
          j:= Pos(Extension, Filter);
          if (j > 0) and
             ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';')) then
          begin
            Result:= Highlighter;
            Exit;
          end;
        end;
      end;
    end;
  end;
  Result:= nil;
end;

{##############################################################################}
// TCETextEditor

{-------------------------------------------------------------------------------
  Create an instance of TCETextEditor
-------------------------------------------------------------------------------}
constructor TCETextEditor.Create(AOwner: TComponent);
var
  IV, IV2: TTBItemViewer;
  i: Integer;
  Highlighter: TSynCustomHighlighter;                                            
begin
  inherited Create(AOwner);

  // initialize values
  check_wrap_around.Checked:= true;
  toolbar_find.Visible:= false;
  toolbar_replace.Visible:= false;
  fActiveHighlighter:= -2;
  fStatsStr:= _('Length: %d Lines: %d');
  fPosStr:= _('Line: %d Char: %d Sel: %d'); 
  // Assign Settings
  fSettings:= GlobalTextEditorSettings;
  GlobalTextEditorSettings.RegisterEditor(Self);
  GlobalTextEditorSettings.AssignTo(Self);

  // create Highlighters list
  Highlighters:= TStringList.Create;
  for i := Self.ComponentCount - 1 downto 0 do
  begin
    if (Self.Components[i] is TSynCustomHighlighter) then
    begin
      Highlighter:= Self.Components[i] as TSynCustomHighlighter;
      // only one highlighter for each language
      if (Highlighters.IndexOf(Highlighter.GetLanguageName) = -1) and
         (Highlighter <> SynMultiHighlighter) then
      Highlighters.AddObject(Highlighter.GetLanguageName, Highlighter);
    end;
  end;
  Highlighters.Sort;

  // add multi highlighter to top of the Highlighter list
  Highlighters.InsertObject(0, _('HTML with CSS, JS or PHP'), SynMultiHighlighter);

  // populate highlighters menu
  PopuplateHighlighters(sub_highlighter);

  // translate
  CEGlobalTranslator.TranslateComponent(Self);
//  label_modified.Caption:= _(label_modified.Caption);

  // update items
  UpdateStats;
  SynMemoStatusChange(Self, [scAll]);
  UpdateCaption;
end;

{-------------------------------------------------------------------------------
  Destroy TCETextEditor
-------------------------------------------------------------------------------}
destructor TCETextEditor.Destroy;
begin
  GlobalTextEditorSettings.UnRegisterEditor(Self);
  // Free instances
  Highlighters.Free;
  
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  On Action Execute
-------------------------------------------------------------------------------}
procedure TCETextEditor.ActionExecute(Sender: TObject);
var
  openDlg: TTntOpenDialog;
  saveDlg: TTntSaveDialog;
  book: TBufferCoord;
  bookIndex: Integer;
begin
  case TComponent(Sender).Tag of
    // New
    101: Close;
    // Open
    102: begin
        openDlg:= TTntOpenDialog.Create(nil);
        try
          if openDlg.Execute then
          OpenFile(openDlg.FileName);
        finally
          openDlg.Free;
        end;
      end;
    // Reload
    103: Reload;
    // Save
    104: Save;
    // Save As
    105: begin
        Save(true);
      end;
    // Close
    106: Close;
    // Export as HTML
    107: begin
      saveDlg:= TTntSaveDialog.Create(nil);
      try
        saveDlg.DefaultExt:= 'html';
        saveDlg.Filter:= _('HTML Documents')+' (*.htm; *.html)|*.htm;*.html|'+ _('All types') + ' (*.*)|*.*';
        if saveDlg.Execute then
        begin
          ExportToHTML(false, false, saveDlg.FileName, WideExtractFileName(ActiveFilePath));
        end;
      finally
        saveDlg.Free;
      end;
    end;
    // Undo
    201: begin
      if SynMemo.Focused then
      SynMemo.Undo
      else if edit_find.Focused then
      edit_find.Undo
      else if edit_replace.Focused then
      edit_replace.Undo;
    end;
    // Redo
    202: if SynMemo.Focused then SynMemo.Redo;
    // Copy
    203: begin
      if SynMemo.Focused then
      SynMemo.CopyToClipboard
      else if edit_find.Focused then
      edit_find.CopyToClipboard
      else if edit_replace.Focused then
      edit_replace.CopyToClipboard;
    end;
    // Cut
    204: begin
      if SynMemo.Focused then
      SynMemo.CutToClipboard
      else if edit_find.Focused then
      edit_find.CutToClipboard
      else if edit_replace.Focused then
      edit_replace.CutToClipboard;
    end;
    // Paste
    205: begin
      if SynMemo.Focused then
      SynMemo.PasteFromClipboard
      else if edit_find.Focused then
      edit_find.PasteFromClipboard
      else if edit_replace.Focused then
      edit_replace.PasteFromClipboard;
    end;
    // Delete
    206: begin
      if SynMemo.Focused then
      SynMemo.ClearSelection
      else if edit_find.Focused then
      edit_find.ClearSelection
      else if edit_replace.Focused then
      edit_replace.ClearSelection;
    end;
    // Select All
    207: begin
      if SynMemo.Focused then
      SynMemo.SelectAll
      else if edit_find.Focused then
      edit_find.SelectAll
      else if edit_replace.Focused then
      edit_replace.SelectAll;
    end;
    // Copy as HTML
    208: begin
      ExportToHTML(true, true);
    end;
    // Find
    301: ShowSearchReplace;
    // Find Next
    302: FindNext;
    // Find Previous
    303: FindPrevious;
    // Replace
    304: ShowSearchReplace(true);
    // Replace Selected
    305: begin
      if (edit_replace.Text <> '') and (SynMemo.SelLength > 0) then
      SynMemo.SelText:= edit_replace.Text;
    end;
    // Replace All
    306: ReplaceAll(edit_find.Text, edit_replace.Text);
    // Close Find
    307: HideSearchReplace;
    // Close Replace
    308: HideSearchReplace(true);
    // Word Wrap
    401: SynMemo.WordWrap:= not SynMemo.WordWrap;
    // Options
    402: ShowTextEditorOptions(GlobalTextEditorSettings);
    // Special Characters
    403: begin
      Settings.SetOptionFlag(eoShowSpecialChars, not Settings.GetOptionFlag(eoShowSpecialChars));
      Settings.UpdateEditors;
    end;
    // Enable Playback
    404: begin
      Settings.EnablePlayback:= not Settings.EnablePlayback;
      if assigned(fOnEnablePlaybackChanged) then
      fOnEnablePlaybackChanged(Self);
    end;
    // Show Bookmarks
    405: toolbar_bookmarks.Visible:= not toolbar_bookmarks.Visible;
    // Show Statusbar
    406: StatusBar.Visible:= not StatusBar.Visible;
    // Bookmarks
    501..510: begin
      bookIndex:= TComponent(Sender).Tag - 500;
      if SynMemo.GetBookMark(bookIndex, book.Char, book.Line) then
      begin
        if (SynMemo.CaretX = book.Char) and (SynMemo.CaretY = book.Line) then
        SynMemo.ClearBookMark(bookIndex)
        else
        SynMemo.CaretXY:= book;
      end
      else
      SynMemo.SetBookMark(bookIndex, SynMemo.CaretX, SynMemo.CaretY);
    end;
    // InsertMode
    1001: SynMemo.InsertMode:= not SynMemo.InsertMode;
  end;
end;

{-------------------------------------------------------------------------------
  On Action Update
-------------------------------------------------------------------------------}
procedure TCETextEditor.ActionUpdate(Sender: TObject);
var
  book: TBufferCoord;
begin
  if Sender is TTntAction then
  begin
    case TTntAction(Sender).Tag of
      // New
      101:;
      // Open
      102:;
      // Reload
      103: TTntAction(Sender).Enabled:= fActiveFilePath <> '';
      // Save
      104: TTntAction(Sender).Enabled:= SynMemo.Modified;
      // Save As
      105: TTntAction(Sender).Enabled:= true;
      // Close
      106:;
      // Undo
      201: TTntAction(Sender).Enabled:= SynMemo.CanUndo or edit_find.Focused or edit_replace.Focused;
      // Redo
      202: TTntAction(Sender).Enabled:= SynMemo.CanRedo;
      // Copy
      203: TTntAction(Sender).Enabled:= (SynMemo.SelLength > 0) or  edit_find.Focused or edit_replace.Focused;
      // Cut
      204: TTntAction(Sender).Enabled:= (SynMemo.SelLength > 0) or edit_find.Focused or edit_replace.Focused;
      // Paste
      205: TTntAction(Sender).Enabled:= SynMemo.CanPaste or edit_find.Focused or edit_replace.Focused;
      // Delete
      206: TTntAction(Sender).Enabled:= (SynMemo.SelLength > 0) or edit_find.Focused or edit_replace.Focused;
      // Select All
      207: TTntAction(Sender).Enabled:= true;
      // Copy as HTML
      208: TTntAction(Sender).Enabled:= SynMemo.SelAvail;
      // Find
      301:;
      // Find Next
      302:;
      // Find Previous
      303:;
      // Replace
      304: begin
        TTntAction(Sender).Checked:= toolbar_replace.Visible;
      end;
      // Replace Selected
      305: TTntAction(Sender).Enabled:= (SynMemo.SelLength > 0) and (edit_replace.Text <> '');
      // Replace All
      306: TTntAction(Sender).Enabled:= (edit_find.Text <> '') and
                                        (edit_replace.Text <> '') and
                                        toolbar_find.Visible;
      // Word Wrap
      401: TTntAction(Sender).Checked:= SynMemo.WordWrap;
      // Show Special Characters
      403: TTntAction(Sender).Checked:= Settings.GetOptionFlag(eoShowSpecialChars);
      // Enable Playback
      404: TTntAction(Sender).Checked:= Settings.EnablePlayback;
      // Show Bookmarks
      405: TTntAction(Sender).Checked:= toolbar_bookmarks.Visible;
      // Show Statusbar
      406: TTntAction(Sender).Checked:= StatusBar.Visible;
      // Bookmarks
      501..510: begin
        TTntAction(Sender).Checked:= SynMemo.GetBookMark(TComponent(Sender).Tag - 500, book.Char, book.Line);
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  ChangeStatus
-------------------------------------------------------------------------------}
procedure TCETextEditor.ChangeStatus(AStatus: WideString);
begin
  StatusTimer.Enabled:= false;
  label_status.Caption:= AStatus;
  label_status.Enabled:= true;
  StatusTimer.Enabled:= true;
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCETextEditor.Close;
begin
  fActiveFilePath:= '';
  SynMemo.Clear;
  UpdateStats;
  StatusTimerTimer(self);
  UpdateCaption;
  DoActiveFileChange;
end;

{-------------------------------------------------------------------------------
  Do Highlighter Click
-------------------------------------------------------------------------------}
procedure TCETextEditor.DoHighlighterClick(Sender: TObject);
var
  item: TSpTBXItem;
begin
  item:= TSpTBXItem(Sender);
  ActiveHighlighter:= item.Tag;
end;

{-------------------------------------------------------------------------------
  Find
-------------------------------------------------------------------------------}
function TCETextEditor.Find(AText: WideString; ABackward: Boolean = false):
    Boolean;
var
  opts: TSynSearchOptions;
begin
  Result:= false;
  if AText = '' then
  Exit;
  
  // search engine
  if check_regex.Checked then
  SynMemo.SearchEngine:= RegexSearch
  else
  SynMemo.SearchEngine:= NormalSearch;

  // options
  opts:= [];
  if check_case_sensitive.Checked then
  Include(opts, ssoMatchCase);
  if check_whole_word.Checked then
  Include(opts, ssoWholeWord);
  if check_selected_text_only.Checked then
  Include(opts, ssoSelectedOnly);
  if ABackward then
  Include(opts, ssoBackwards);
  
  Result:= SynMemo.SearchReplace(AText, '', opts) <> 0;
  if not Result then
  begin
    // wrap around
    if check_wrap_around.Checked then
    begin
      Include(opts, ssoEntireScope);
      Result:= SynMemo.SearchReplace(AText, '', opts) <> 0;
      if Result then
      ChangeStatus(_('Wrapped around'));
    end;

    if not Result then
    ChangeStatus(_('Not found'));
  end
  else
  begin
    ChangeStatus(_('Found'));
  end;

  if not ABackward and Result then // move caret in front of the selection
  SynMemo.SetCaretAndSelection(SynMemo.BlockBegin, SynMemo.BlockBegin, SynMemo.BlockEnd);
end;

{-------------------------------------------------------------------------------
  On Find Next
-------------------------------------------------------------------------------}
function TCETextEditor.FindNext: Boolean;
begin
  if (not check_selected_text_only.Checked and SynMemo.SelAvail) then
  begin
    // move caret at the end of selection if needed.
    if (check_case_sensitive.Checked and (SynMemo.SelText = edit_find.Text)) or
       (not check_case_sensitive.Checked and (WideCompareText(SynMemo.SelText, edit_find.Text) = 0)) then
    begin
      SynMemo.SetCaretAndSelection(SynMemo.BlockEnd, SynMemo.BlockBegin, SynMemo.BlockEnd);
    end;
  end;

  Result:= Find(edit_find.Text);
end;

{-------------------------------------------------------------------------------
  Find Previous
-------------------------------------------------------------------------------}
function TCETextEditor.FindPrevious: Boolean;
begin
  if (not check_selected_text_only.Checked and SynMemo.SelAvail) then
  begin
    // move caret at the beginning of selection if needed.
    if (check_case_sensitive.Checked and (SynMemo.SelText = edit_find.Text)) or
       (not check_case_sensitive.Checked and (WideCompareText(SynMemo.SelText, edit_find.Text) = 0)) then
    begin
      SynMemo.SetCaretAndSelection(SynMemo.BlockBegin, SynMemo.BlockBegin, SynMemo.BlockEnd);
    end;
  end;
  
  Result:= Find(edit_find.Text, true);
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
function TCETextEditor.OpenFile(AFilePath: WideString): Boolean;
var
  ws, ws2: WideString;
begin
  Result:= false;
  fActiveFilePath:= '';
  if WideFileExists(AFilePath) then
  begin
    try
      SynMemo.Modified:= false;
      SynMemo.Lines.LoadFromFile(AFilePath);
      fActiveFilePath:= AFilePath;
      Result:= true;
      StatusTimerTimer(self);
    except
      on E: Exception do
      begin
        ChangeStatus(_('Failed to open file!'));
        ws:= _('Error');
        ws2:= E.Message;
        MessageBoxW(0, PWideChar(ws2), PWideChar(ws), MB_ICONERROR or MB_OK);
      end;
    end;

    if fActiveHighlighter = -2 then
    ChangeAutoHighlighter;
    
    UpdateStats;
    SynMemoStatusChange(Self, [scAll]);
  end;

  UpdateCaption;
  DoActiveFileChange;
end;

{-------------------------------------------------------------------------------
  Popuplate Highlighters
-------------------------------------------------------------------------------}
procedure TCETextEditor.PopuplateHighlighters(ASubMenu: TTBCustomItem);
var
  i: integer;
  Highlighter: TSynCustomHighlighter;
  item: TSpTBXItem;
begin
  // add highlighters to sub menu
  if assigned(ASubMenu) then
  begin
    ASubMenu.OnPopup:= DoHighlighterMenuPopup;
    ASubMenu.Clear;
    // add None item
    item:= TSpTBXItem.Create(ASubMenu);
    item.Caption:= _('None');
    item.Tag:= -1;
    item.RadioItem:= true;
    item.GroupIndex:= 1;
    item.OnClick:= DoHighlighterClick;
    ASubMenu.Add(item);
    // add Automatic item
    item:= TSpTBXItem.Create(ASubMenu);
    item.Caption:= _('Automatic');
    item.Tag:= -2;
    item.RadioItem:= true;
    item.GroupIndex:= 1;
    item.OnClick:= DoHighlighterClick;
    ASubMenu.Add(item);
    // add Separator
    ASubMenu.Add(TSpTBXSeparatorItem.Create(ASubMenu));
    // add highlighters
    for i:= 0 to Highlighters.Count - 1 do
    begin
      item:= TSpTBXItem.Create(ASubMenu);
      Highlighter:= TSynCustomHighlighter(Highlighters.Objects[i]);
      if Highlighter <> SynMultiHighlighter then
      item.Caption:= Highlighter.GetFriendlyLanguageName
      else
      item.Caption:= Highlighters.Strings[i];
      item.Tag:= i;
      item.RadioItem:= true;
      item.GroupIndex:= 1;
      item.OnClick:= DoHighlighterClick;
      ASubMenu.Add(item);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Reload
-------------------------------------------------------------------------------}
procedure TCETextEditor.Reload;
var
  oldPos: TBufferCoord;
  topLine: Integer;
begin
  if fActiveFilePath <> '' then
  begin
    oldPos:= SynMemo.CaretXY;
    topLine:= SynMemo.TopLine;
    if OpenFile(fActiveFilePath) then
    begin
      SynMemo.SetCaretAndSelection(oldPos, oldPos, oldPos);
      SynMemo.TopLine:= topLine;
      ChangeStatus(_('Reloaded from disk'));
    end
    else
    ChangeStatus(_('Reload failed!'));
  end;
end;

{-------------------------------------------------------------------------------
  Save
-------------------------------------------------------------------------------}
function TCETextEditor.Save(ASaveAs: Boolean = false): Boolean;
var
  saveDlg: TTntSaveDialog;
begin
  Result:= false;
  // save
  if not ASaveAs and WideFileExists(fActiveFilePath) then
  begin
    Result:= SaveFile(fActiveFilePath);
  end
  // save as
  else
  begin
    saveDlg:= TTntSaveDialog.Create(nil);
    try
      if saveDlg.Execute then
      Result:= SaveFile(saveDlg.FileName);
    finally
      saveDlg.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  SaveFile
-------------------------------------------------------------------------------}
function TCETextEditor.SaveFile(AFilePath: WideString): Boolean;
var
  ws, ws2: WideString;
begin
  Result:= false;
  try
    SynMemo.Lines.SaveToFile(AFilePath);
    SynMemo.Modified:= false;
    fActiveFilePath:= AFilePath;
    Result:= true;
    ChangeStatus(WideFormat(_('Saved to "%s"'), [WideExtractFileName(AFilePath)]));
  except
    on E: Exception do
    begin
      ws:= _('Error');
      ws2:= E.Message;
      MessageBoxW(0, PWideChar(ws2), PWideChar(ws), MB_ICONERROR or MB_OK);
      ChangeStatus(_('Failed to save file!'));
    end;
  end;
  UpdateStats;
  UpdateCaption;
  DoActiveFileChange;  
end;

{*------------------------------------------------------------------------------
  Change AutoHighlighter
-------------------------------------------------------------------------------}
procedure TCETextEditor.ChangeAutoHighlighter;
var
  ExtLen: integer;
  i, j, i2, i3: integer;
  Highlighter: TSynCustomHighlighter;
  Filter: string;
  Extension: String;
  item: TSpTBXItem;
begin
  if ActiveFilePath = '' then
  Exit;
  
  Extension:= ExtractFileExt(ActiveFilePath);
  Extension:= LowerCase(Extension);
  ExtLen:= Length(Extension);
  
  if (ExtLen > 0) then
  begin
    i3:= -1;
    for i:= 0 to Highlighters.Count - 1 do
    begin
      if (Highlighters.Objects[i] is TSynCustomHighlighter) then
      begin
        Highlighter:= TSynCustomHighlighter(Highlighters.Objects[i]);
        Filter:= LowerCase(Highlighter.DefaultFilter);
        j:= Pos('|', Filter);
        if j > 0 then
        begin
          Delete(Filter, 1, j);
          j:= Pos(Extension, Filter);
          if (j > 0) and ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';')) then
          begin
            // assign highglighter
            SynMemo.Highlighter:= Highlighter;
            Exit;
          end
          else if Pos('*.*', Filter) > 0 then
          i3:= i;
        end;
      end;
    end;

    // assign highglighter that supports all files
    if i3 > -1 then
    begin
      Highlighter:= TSynCustomHighlighter(Highlighters.Objects[i3]);
      SynMemo.Highlighter:= Highlighter;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  DoHighlighterMenuPopup
-------------------------------------------------------------------------------}
procedure TCETextEditor.DoHighlighterMenuPopup(Sender: TTBCustomItem; FromLink:
    Boolean);
var
  i, i2: Integer;
  item: TSpTBXItem;
begin
  // change active highlighter's font style to Bold
  i2:= Highlighters.IndexOfObject(SynMemo.Highlighter);
  for i:= 0 to Sender.Count - 1 do
  begin
    if Sender.Items[i] is TSpTBXItem then
    begin
      item:= TSpTBXItem(Sender.Items[i]);
      item.Checked:= item.Tag = fActiveHighlighter;
      
      if (i2 = item.Tag) then
      item.FontSettings.Style:= [fsBold]
      else
      item.FontSettings.Style:= [];
    end;
  end;
end;

{-------------------------------------------------------------------------------
  ExportToHTML
-------------------------------------------------------------------------------}
procedure TCETextEditor.ExportToHTML(AToClipboard: Boolean; ASelectedOnly:
    Boolean; AFilePath: WideString = ''; ATitle: WideString = '');
var
  exporter: TCESynExporterHTML;
  lines: TUnicodeStrings;
  spaces: String;
  i: Integer;
begin
  Assert(Assigned(fSettings), 'TCETextEditor: Settings needs to be assigned');
  
  exporter:= TCESynExporterHTML.Create(Self);
  try
    exporter.Highlighter:= SynMemo.Highlighter;
    exporter.Encoding:= seUTF8;
    exporter.ExportAsText:= true;
    exporter.UseBackground:= eoUseBackground in Settings.ExportOptions;
    exporter.Color:= Settings.ExportBackgroundColor;
    exporter.WrapperDivClass:= Settings.ExportWrapperClass;

    if AToClipboard then
    begin
      exporter.IncludeHTMLWrapper:= false;
      exporter.UseInlineCSS:= eoInlineCSSOnCopy in Settings.ExportOptions;
    end
    else
    begin
      exporter.HTMLTitle:= ATitle;
      exporter.IncludeHTMLWrapper:= true;
      exporter.UseInlineCSS:= eoInlineCSSOnExport in Settings.ExportOptions;
      if ASelectedOnly then
      exporter.ExportRange(SynMemo.Lines, SynMemo.BlockBegin, SynMemo.BlockEnd)
      else
      exporter.ExportAll(SynMemo.Lines);
    end;

    //  tabs to spaces export
    if eoConvertTabsToSpaces in Settings.ExportOptions then
    begin
      lines:= TUnicodeStringList.Create;
      try
        // make space string
        spaces:= StringOfChar(#32, SynMemo.TabWidth);
        // replace tabs with space string
        if ASelectedOnly then
        lines.Text:= ReplaceStr(SynMemo.SelText, #9, spaces)
        else
        lines.Text:= ReplaceStr(SynMemo.Lines.Text, #9, spaces);
        // export
        exporter.ExportAll(lines);
      finally
        lines.Free;
      end;
    end
    // normal export
    else
    begin
      if ASelectedOnly then
      exporter.ExportRange(SynMemo.Lines, SynMemo.BlockBegin, SynMemo.BlockEnd)
      else
      exporter.ExportAll(SynMemo.Lines);
    end;

    // save/copy to clipboard
    if AToClipboard then
    exporter.CopyToClipboard
    else
    exporter.SaveToFile(AFilePath);
  finally
    exporter.Free;
  end;
end;

{-------------------------------------------------------------------------------
  ReplaceAll
-------------------------------------------------------------------------------}
procedure TCETextEditor.ReplaceAll(AFind, AReplace: WideString);
var
  opts: TSynSearchOptions;
  i: Integer;
  xy: TBufferCoord;
  dispXY: TDisplayCoord;
begin
  if (AFind <> '') and (AReplace <> '') then
  begin
    // search engine
    if check_regex.Checked then
    SynMemo.SearchEngine:= RegexSearch
    else
    SynMemo.SearchEngine:= NormalSearch;
    
    // options
    opts:= [ssoEntireScope, ssoReplaceAll];
    if check_case_sensitive.Checked then
    Include(opts, ssoMatchCase);
    if check_whole_word.Checked then
    Include(opts, ssoWholeWord);
    if check_selected_text_only.Checked then
    Include(opts, ssoSelectedOnly);

    xy:= SynMemo.CaretXY;
    dispXY.Row:= SynMemo.TopLine;
    dispXY.Column:= SynMemo.LeftChar;

    i:= SynMemo.SearchReplace(AFind, AReplace, opts);

    SynMemo.CaretXY:= xy;
    SynMemo.TopLine:= dispXY.Row;
    SynMemo.LeftChar:= dispXY.Column;

    ChangeStatus(WideFormat(_('Replaced %d texts.'),[i]));
  end;
end;

{-------------------------------------------------------------------------------
  Set ActiveHighlighter
-------------------------------------------------------------------------------}
procedure TCETextEditor.SetActiveHighlighter(const Value: Integer);
begin
  // validate Value
  if (Value < -2) or (Value >= Highlighters.Count) then
  fActiveHighlighter:= -1
  else
  fActiveHighlighter:= Value;

  // manual highlighter
  if fActiveHighlighter > -1 then
  begin
    SynMemo.Highlighter:= TSynCustomHighlighter(Highlighters.Objects[fActiveHighlighter]);
  end
  // automatic highlighter
  else if fActiveHighlighter = -2 then
  begin
    ChangeAutoHighlighter;
  end
  // no highlighter
  else
  begin
    SynMemo.Highlighter:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  Show SearchReplace
-------------------------------------------------------------------------------}
procedure TCETextEditor.ShowSearchReplace(AShowReplace: Boolean = false);
var
  IV, IV2: TTBItemViewer;
begin
  if AShowReplace then
  begin
    toolbar_replace.Visible:= true;
    toolbar_find.Visible:= true;
  end
  else
  toolbar_find.Visible:= true;

  // resize find and replace labels
  if toolbar_replace.Visible then
  begin
    IV:= toolbar_find.View.Find(label_find);
    IV2:= toolbar_replace.View.Find(label_replace);
    if assigned(IV) and assigned(IV2) then
    begin
      label_find.MinWidth:= IV2.BoundsRect.Right - IV2.BoundsRect.Left;
      label_replace.MinWidth:= IV.BoundsRect.Right - IV.BoundsRect.Left;
    end;
  end
  else
  begin
    label_find.MinWidth:= 0;
  end;

  // resize replace edit
  if toolbar_replace.Visible then
  begin
    if edit_replace.Width <> edit_find.Width then
    begin
      edit_replace.Width:= edit_find.Width;
      toolbar_replace.RightAlignItems;
    end;
  end;

  // set focus
  if AShowReplace and toolbar_replace.Visible then
  edit_replace.SetFocus
  else if not AShowReplace and toolbar_find.Visible then
  edit_find.SetFocus;
end;

{-------------------------------------------------------------------------------
  On StatusTimer.Timer
-------------------------------------------------------------------------------}
procedure TCETextEditor.StatusTimerTimer(Sender: TObject);
begin
  StatusTimer.Enabled:= false;
  if fActiveFilePath <> '' then
  begin
    if SynMemo.Modified then
    label_status.Caption:= '*' + WideExtractFileName(fActiveFilePath)
    else
    label_status.Caption:= WideExtractFileName(fActiveFilePath);
    label_status.Enabled:= SynMemo.Modified;
  end
  else
  begin
    if SynMemo.Modified then
    label_status.Caption:= '*' + _('New text file')
    else
    label_status.Caption:= _('New text file');
    label_status.Enabled:= SynMemo.Modified;
  end;
end;

{-------------------------------------------------------------------------------
  On SynMemo.Change
-------------------------------------------------------------------------------}
procedure TCETextEditor.SynMemoChange(Sender: TObject);
begin
  // Update stats
  UpdateStats;
end;

{-------------------------------------------------------------------------------
  On SynMemo.StatusChange
-------------------------------------------------------------------------------}
procedure TCETextEditor.SynMemoStatusChange(Sender: TObject; Changes:
    TSynStatusChanges);
var
  i: Integer;
  len: Int64;
begin
  // Selection changed
  if (scSelection in Changes) or (scCaretX in Changes) or (scCaretY in Changes) or (scAll in Changes) then
  begin
    // position label
    label_position.Caption:= WideFormat(fPosStr,
      [SynMemo.CaretXY.Line, SynMemo.CaretXY.Char, SynMemo.SelLength]);
  end;

  // Insert Mode
  if (scInsertMode in Changes) or (scAll in Changes) then
  begin
    if SynMemo.InsertMode then
    but_insertmode.Caption:= _('Insert')
    else
    but_insertmode.Caption:= _('Override');
  end;
  
end;

{-------------------------------------------------------------------------------
  Can Close
-------------------------------------------------------------------------------}
function TCETextEditor.CanClose: Boolean;
var
  s,s2: WideString;
  r: Integer;
  i: Integer;
begin
  Result:= false;
  if SynMemo.Modified then
  begin
    if not Visible then
    Show;

    s:= WideFormat(_('The text in %s has changed.'), [fActiveFileName]);
    s2:= _('Do you want to save changes before closing?');
    r:= TaskDialog(Self.Handle,
                   _('Save before closing?'),
                   s,
                   s2,
                   TD_ICON_WARNING,
                   TD_BUTTON_YES + TD_BUTTON_NO + TD_BUTTON_CANCEL);

    if r = TD_RESULT_YES then
    begin
      if not Save then
      Exit;
    end
    else if r = TD_RESULT_CANCEL then
    begin
      Exit;
    end;
  end;
  
  Result:= true;
end;

{-------------------------------------------------------------------------------
  DoActiveFileChange
-------------------------------------------------------------------------------}
procedure TCETextEditor.DoActiveFileChange;
begin
  if Assigned(fOnActiveFileChange) then fOnActiveFileChange(Self);
end;

{-------------------------------------------------------------------------------
  On edit_find.KeyPress
-------------------------------------------------------------------------------}
procedure TCETextEditor.edit_findKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #9 then
  begin
    Key:= #0;
    if toolbar_replace.Visible then
    edit_replace.SetFocus;
  end
  else if Key = #13 then
  begin
    Key:= #0;
    FindNext;
    if not Settings.FindNextWithEnter then
    SynMemo.SetFocus;
  end
  else if Key = #27 then
  begin
    Key:= #0;
    HideSearchReplace;
  end;
end;

{-------------------------------------------------------------------------------
  On edit_replace.KeyPress
-------------------------------------------------------------------------------}
procedure TCETextEditor.edit_replaceKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #9 then
  begin
    Key:= #0;
    SynMemo.SetFocus;
  end
  else if Key = #13 then
  begin
    Key:= #0;
    act_replace_selected.Execute;
  end
  else if Key = #27 then
  begin
    Key:= #0;
    HideSearchReplace(true);
  end;
end;

{-------------------------------------------------------------------------------
  On editChange
-------------------------------------------------------------------------------}
procedure TCETextEditor.editChange(Sender: TObject);
begin
  act_replace_selected.Enabled:= (SynMemo.SelLength > 0) and (edit_replace.Text <> '');
  act_replace_all.Enabled:= (edit_find.Text <> '') and (edit_replace.Text <> '') and toolbar_find.Visible;
end;

{-------------------------------------------------------------------------------
  HideSearchReplace
-------------------------------------------------------------------------------}
procedure TCETextEditor.HideSearchReplace(AHideReplace: Boolean = false);
begin
  if AHideReplace then
  toolbar_replace.Visible:= false;
  toolbar_find.Visible:= false;
  SynMemo.SetFocus;
end;

{-------------------------------------------------------------------------------
  On SynMemo.KeyPress
-------------------------------------------------------------------------------}
procedure TCETextEditor.SynMemoKeyPress(Sender: TObject; var Key: WideChar);
begin
  if Settings.CloseWithEsc and (Key = #27) then
  act_close.Execute;
end;

{-------------------------------------------------------------------------------
  UpdateCaption
-------------------------------------------------------------------------------}
procedure TCETextEditor.UpdateCaption;
begin
  if ActiveFilePath <> '' then
  begin
    Caption:= ActiveFilePath;
    fActiveFileName:= WideExtractFileName(ActiveFilePath);
  end
  else
  begin
    fActiveFileName:= _('New text file');
    Caption:= 'CubicNotepad';
  end;
end;

{-------------------------------------------------------------------------------
  UpdateStats
-------------------------------------------------------------------------------}
procedure TCETextEditor.UpdateStats;
var
  i: Integer;
  len: Int64;
begin
  // stats label
  len:= 0;
  if SynMemo.Lines.Count > 0 then
  begin
    for i:= 0 to SynMemo.Lines.Count-1 do
    begin
      len:= len + Length(SynMemo.Lines.Strings[i]);
    end;
    len:= len + ((SynMemo.Lines.Count-1) * 2); // <-- add line breaks to the count
  end;

  label_stats.Caption:= WideFormat(fStatsStr, [len, SynMemo.Lines.Count]);

  // modified label
  if SynMemo.Modified <> label_modified.Visible then
  begin
    label_modified.Visible:= SynMemo.Modified;
    sep_modified.Visible:= label_modified.Visible;
    if not StatusTimer.Enabled then
    StatusTimerTimer(self);
  end;

  // format
  case SynMemo.Lines.SaveFormat of
    sfUTF16LSB: label_format.Caption:= 'UTF-16 Little-endian';
    sfUTF16MSB: label_format.Caption:= 'UTF-16 Big-endian';
    sfUTF8: label_format.Caption:= 'UTF-8';
    sfAnsi: label_format.Caption:= 'ANSI';
  end;
end;

end.

