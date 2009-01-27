unit fCE_TextEditor;

interface

uses
  // CE Units
  CE_GlobalCtrl, fCE_TabPage,
  // Toolbar2000
  TB2Dock, TB2Toolbar, TB2Item,
  // TBX
  TBX, TBXDkPanels,
  // SpTBXLib
  SpTBXControls, SpTBXDkPanels, SpTBXItem,
  // Syn Edit
  SynEdit, SynEditOptionsDialog, SynURIOpener, SynEditRegexSearch,
  SynEditMiscClasses, SynEditSearch, SynEditTypes,
  // Syn Highlighters
  SynHighlighterURI, SynHighlighterDfm, SynHighlighterPerl, SynHighlighterJava,
  SynHighlighterPas, SynHighlighterIni, SynHighlighterBat, SynHighlighterXML,
  SynHighlighterJScript, SynHighlighterPHP, SynHighlighterHtml,
  SynHighlighterCSS, SynEditHighlighter, SynHighlighterCpp,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ExtCtrls, ActnList, TntActnList, StdCtrls, TntStdCtrls, TntSysUtils,
  TntDialogs, TntClasses;

type
  TCEActiveFileChangeEvent = procedure(Sender: TObject; FilePath: WideString) of object;

  TCETextEditor = class(TCECustomTabPage)
    TopDock: TSpTBXDock;
    MainToolbar: TSpTBXToolbar;
    Editor: TSynEdit;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    SpTBXSubmenuItem3: TSpTBXSubmenuItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXItem6: TSpTBXItem;
    SpTBXItem7: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SpTBXItem8: TSpTBXItem;
    SpTBXItem9: TSpTBXItem;
    SpTBXItem10: TSpTBXItem;
    SpTBXItem11: TSpTBXItem;
    SpTBXItem12: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    SpTBXItem13: TSpTBXItem;
    SpTBXItem17: TSpTBXItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SpTBXItem19: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    ActionList: TTntActionList;
    SpTBXSubmenuItem4: TSpTBXSubmenuItem;
    SpTBXItem20: TSpTBXItem;
    text_file_new: TTntAction;
    text_edit_undo: TTntAction;
    text_file_open: TTntAction;
    text_file_save: TTntAction;
    text_file_saveas: TTntAction;
    text_file_close: TTntAction;
    text_edit_redo: TTntAction;
    text_edit_delete: TTntAction;
    text_edit_selall: TTntAction;
    text_edit_search: TTntAction;
    text_edit_findnext: TTntAction;
    text_edit_findprev: TTntAction;
    text_edit_paste: TTntAction;
    text_edit_cut: TTntAction;
    text_edit_copy: TTntAction;
    text_format_wrap: TTntAction;
    text_format_options: TTntAction;
    text_view_toolbar: TTntAction;
    SpTBXItem21: TSpTBXItem;
    text_file_reload: TTntAction;
    StatusBar: TSpTBXStatusBar;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    label_input: TSpTBXLabelItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    label_modified: TSpTBXLabelItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    label_path: TSpTBXLabelItem;
    text_view_statusbar: TTntAction;
    SpTBXItem22: TSpTBXItem;
    FindPanel: TSpTBXPanel;
    SpTBXLabel1: TSpTBXLabel;
    SearchMemo: TTntMemo;
    SpTBXLabel2: TSpTBXLabel;
    ReplaceMemo: TTntMemo;
    SpTBXGroupBox1: TSpTBXGroupBox;
    opt_check1: TSpTBXCheckBox;
    opt_check2: TSpTBXCheckBox;
    opt_check3: TSpTBXCheckBox;
    opt_check4: TSpTBXCheckBox;
    opt_check5: TSpTBXCheckBox;
    SpTBXButton1: TSpTBXButton;
    SpTBXButton3: TSpTBXButton;
    SpTBXButton5: TSpTBXButton;
    SpTBXItem14: TSpTBXItem;
    SpTBXItem15: TSpTBXItem;
    SynEditSearch: TSynEditSearch;
    SynEditRegexSearch: TSynEditRegexSearch;
    opt_radio: TSpTBXRadioGroup;
    SynCppSyn1: TSynCppSyn;
    SynCssSyn1: TSynCssSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynXMLSyn1: TSynXMLSyn;
    SynBatSyn1: TSynBatSyn;
    SynIniSyn1: TSynIniSyn;
    SynPasSyn1: TSynPasSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynPerlSyn1: TSynPerlSyn;
    SynDfmSyn1: TSynDfmSyn;
    highlighterSubmenu: TSpTBXSubmenuItem;
    SpTBXItem16: TSpTBXItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    SpTBXItem18: TSpTBXItem;
    SynURISyn1: TSynURISyn;
    SynURIOpener1: TSynURIOpener;
    procedure text_file_Execute(Sender: TObject);
    procedure text_file_Update(Sender: TObject);
    procedure text_edit_Execute(Sender: TObject);
    procedure text_edit_Update(Sender: TObject);
    procedure text_format_Execute(Sender: TObject);
    procedure text_format_Update(Sender: TObject);
    procedure text_view_Execute(Sender: TObject);
    procedure text_view_Update(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SpTBXButton1Click(Sender: TObject);
    procedure EditorReplaceText(Sender: TObject; const ASearch,
      AReplace: WideString; Line, Column: Integer;
      var Action: TSynReplaceAction);
    procedure SpTBXButton3Click(Sender: TObject);
    procedure SpTBXButton5Click(Sender: TObject);
    procedure SpTBXItem16Click(Sender: TObject);
  private
    fActiveFile: WideString;
    fOnActiveFileChange: TCEActiveFileChangeEvent;
    fOnModifiedChange: TNotifyEvent;
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure SetActiveFile(const Value: WideString);
  protected
    procedure ActiveFileChange;
    procedure ModifiedChange;
  public
    Highlighters: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CloseDocument: Boolean;
    procedure SetAutoHighlighter;
    procedure NewDocument;
    procedure OpenDocument(FilePath: WideString = '');
    procedure PopuplateHighlighters;
    procedure ReloadDocument;
    procedure SaveDocument;
    procedure SaveDocumentAs;
    procedure ShowOptions;
    property ActiveFile: WideString read fActiveFile write SetActiveFile;
  published
    property OnActiveFileChange: TCEActiveFileChangeEvent read fOnActiveFileChange write
        fOnActiveFileChange;
    property OnModifiedChange: TNotifyEvent read fOnModifiedChange write
        fOnModifiedChange;
  end;

function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;


implementation


function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;
var
  ExtLen: integer;
  i, j: integer;
  Highlighter: TSynCustomHighlighter;
  Filter: string;
begin
  Extension := LowerCase(Extension);
  ExtLen := Length(Extension);
  if Assigned(AHighlighters) and (ExtLen > 0) then begin
    for i := 0 to AHighlighters.Count - 1 do begin
      if not (AHighlighters.Objects[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[i]);
      Filter := LowerCase(Highlighter.DefaultFilter);
      j := Pos('|', Filter);
      if j > 0 then begin
        Delete(Filter, 1, j);
        j := Pos(Extension, Filter);
        if (j > 0) and
           ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';'))
        then begin
          Result := Highlighter;
          exit;
        end;
      end;
    end;
  end;
  Result := nil;
end;

{=== File Action IDs ===}
// 101 = New
// 102 = Open
// 103 = Save
// 104 = Save As
// 105 = Close
// 106 = Reload
{=== Edit Action IDs ===}
// 201 = Undo
// 202 = Redo
// 203 = Copy
// 204 = Cut
// 205 = Paste
// 206 = Delete
// 207 = Select All
// 208 = Search and Replace
// 209 = Find Next
// 210 = Find Previous
{=== Format Action IDs ===}
// 301 = Word Wrap
// 302 = Editor Options
{=== View Action IDs ===}
// 401 = Show Toolbar
// 402 = Show statusbar

{$R *.dfm}


constructor TCETextEditor.Create(AOwner: TComponent);
begin
  inherited;
  ActiveFile:= 'Untitled';

  Highlighters:= TStringList.Create;
  Highlighters.Sorted:= true;

  PopuplateHighlighters;
  GlobalFocusCtrl.CtrlList.Add(Editor);
end;

destructor TCETextEditor.Destroy;
begin
  CloseDocument;
  Highlighters.Free;
  inherited;
end;

procedure TCETextEditor.ActiveFileChange;
begin
  if csDestroying in self.ComponentState then
  Exit;
  if Assigned(fOnActiveFileChange) then fOnActiveFileChange(Self, ActiveFile);
end;

function TCETextEditor.CloseDocument: Boolean;
var
  s: String;
  r: Integer;
  i: Integer;
begin
  Result:= false;
  if Editor.Modified then
  begin
    s:= 'The text in the ' + WideExtractFileName(ActiveFile) + ' file has changed.' ;
    s:= s + #13#10#13#10'Do you want to save the changes?';
    r:= MessageBox(0, PChar(s), 'Save before closing?', MB_ICONWARNING or MB_YESNOCANCEL);
    if r = idYes then
    SaveDocument
    else if r = idCancel then
    Exit;
  end;

  if csDestroying in self.ComponentState then
  Exit;  

  Editor.Clear;
  ActiveFile:= 'Untitled';
  Result:= true;
  for i:= 0 to highlighterSubmenu.Count - 1 do
  begin
    if highlighterSubmenu.Items[i] is TSpTBXItem then
    TSpTBXItem(highlighterSubmenu.Items[i]).CaptionGlow:= gldNone;
  end;
end;

{ TSearchReplaceDemoForm }

procedure TCETextEditor.NewDocument;
begin
  if CloseDocument then
  begin
    ActiveFile:= 'Untitled';
  end;
end;

procedure TCETextEditor.OpenDocument(FilePath: WideString = '');
var
  open1: TTntOpenDialog;
  S: TTntFileStream;
begin
  if FilePath = '' then
  begin
    open1:= TTntOpenDialog.Create(self);
    if open1.Execute then
    begin
      if CloseDocument then
      begin
        s:= TTntFileStream.Create(open1.FileName, fmOpenRead);
        try
          Editor.Lines.LoadFromStream(s);
          ActiveFile:= open1.FileName;
        finally
          s.Free;
        end;
      end;
    end;
    open1.Free;
  end
  else
  begin
    if CloseDocument then
    begin
      s:= TTntFileStream.Create(FilePath, fmOpenRead);
      try
        Editor.Lines.LoadFromStream(s);
        ActiveFile:= FilePath;
      finally
        s.Free;
      end;
    end;
  end;
end;

procedure TCETextEditor.ReloadDocument;
begin
  OpenDocument(ActiveFile);
end;

procedure TCETextEditor.SaveDocument;
var
  S: TTntFileStream;
begin
  if WideFileExists(ActiveFile) then
  begin
    s:= TTntFileStream.Create(ActiveFile, fmCreate);
    try
      Editor.Lines.SaveToStream(s);
    finally
      s.Free;
    end;
    Editor.Modified:= false;
  end
  else
  begin
    SaveDocumentAs;
  end;
end;

procedure TCETextEditor.SaveDocumentAs;
var
  save1: TTntSaveDialog;
  s: TTntFileStream;
begin
  save1:= TTntSaveDialog.Create(self);
  if save1.Execute then
  begin
    s:= TTntFileStream.Create(save1.FileName, fmCreate);
    try
      Editor.Lines.SaveToStream(s);
      ActiveFile:= save1.FileName;
    finally
      s.Free;
    end;
    Editor.Modified:= false;
  end;
  save1.Free;
end;

procedure TCETextEditor.SetActiveFile(const Value: WideString);
begin
  fActiveFile:= Value;
  label_path.Caption:= Value;
  ActiveFileChange;
  if SpTBXItem18.Checked then
  begin
    SetAutoHighlighter;
  end;
end;

procedure TCETextEditor.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  //if (scCaretX in Changes) or (scCaretY in Changes) then
  //label_pos.Caption:= 'Ln: ' + IntToStr(Editor.CaretY) + ' | ' + 'Col: ' + IntToStr(Editor.CaretX);
  if (scInsertMode in Changes) then
  begin
    if Editor.InsertMode then
    label_input.Caption:= 'Insert'
    else
    label_input.Caption:= 'Override';
  end;
  if (scModified in Changes) then
  begin
    if Editor.Modified then
    label_modified.Caption:= 'Modified'
    else
    label_modified.Caption:= '';
    ModifiedChange;
  end;
end;

procedure TCETextEditor.ShowOptions;
var
  cont: TSynEditorOptionsContainer;
  dlg: TSynEditOptionsDialog;
begin
  cont:= TSynEditorOptionsContainer.Create(self);
  dlg:= TSynEditOptionsDialog.Create(self);
  try
    cont.Assign(Editor);
    if dlg.Execute(cont) then
    begin
      cont.AssignTo(Editor);
    end;
  finally
    dlg.Free;
    cont.Free;
  end;
end;

procedure TCETextEditor.text_file_Execute(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  case act.Tag of
    101: NewDocument;
    102: OpenDocument;
    103: SaveDocument;
    104: SaveDocumentAs;
    105: CloseDocument;
    106: ReloadDocument;
  end;
end;

procedure TCETextEditor.text_file_Update(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  act.Enabled:= true;
  case act.Tag of
    103: act.Enabled:= Editor.Modified;
    106: act.Enabled:= WideFileExists(fActiveFile);
  end;
end;

procedure TCETextEditor.text_edit_Execute(Sender: TObject);
var
  act: TTntAction;
  r: TRect;
begin
  act:= TTntAction(Sender);
  case act.Tag of
    201: Editor.Undo;
    202: Editor.Redo;
    203: Editor.CopyToClipboard;
    204: Editor.CutToClipboard;
    205: Editor.PasteFromClipboard;
    206: Editor.ClearSelection;
    207: Editor.SelectAll;
    208: begin
           if not FindPanel.Visible then
           begin
            r:= self.ClientRect;
            if StatusBar.Visible then
            r.Bottom:= StatusBar.BoundsRect.Top;
            r.Top:= r.Bottom - FindPanel.Height;
            FindPanel.BoundsRect:= r;
            //FindPanel.Realign;
           end;
           FindPanel.Visible:= not FindPanel.Visible;
           if FindPanel.Visible then
           SearchMemo.SetFocus;
         end;
    209: DoSearchReplaceText(false, false);
    210: DoSearchReplaceText(false, true);
  end;
end;

procedure TCETextEditor.text_edit_Update(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  act.Enabled:= true;
  case act.Tag of
    201: act.Enabled:= Editor.CanUndo;
    202: act.Enabled:= Editor.CanRedo;
    205: act.Enabled:= Editor.CanPaste;
    208: act.Checked:= FindPanel.Visible;
    //208..211: act.Enabled:= false;
  end;
end;

procedure TCETextEditor.text_format_Execute(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  case act.Tag of
    301: Editor.WordWrap:= not Editor.WordWrap;
    302: ShowOptions;
  end;
end;

procedure TCETextEditor.text_format_Update(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  act.Enabled:= true;
  case act.Tag of
    301: act.Checked:= Editor.WordWrap;
  end;
end;

procedure TCETextEditor.text_view_Execute(Sender: TObject);
var
  act: TTntAction;
  r: TRect;
begin
  act:= TTntAction(Sender);
  case act.Tag of
    402: begin
           if not StatusBar.Visible then
           begin
             if FindPanel.Visible then
             begin
               r:= FindPanel.BoundsRect;
               r.Top:= r.Bottom;
               r.Bottom:= r.Top + StatusBar.Height;
               StatusBar.BoundsRect:= r;
             end;
           end;
           StatusBar.Visible:= not StatusBar.Visible;
         end;
  end;
end;

procedure TCETextEditor.text_view_Update(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  act.Enabled:= true;
  case act.Tag of
    402: act.Checked:= StatusBar.Visible;  
  end;
end;

procedure TCETextEditor.DoSearchReplaceText(AReplace: boolean; ABackwards:
    boolean);
var
  Options: TSynSearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if opt_check1.Checked then
  Include(Options, ssoMatchCase);
  if opt_check2.Checked then
  Include(Options, ssoWholeWord);
  if not opt_check3.Checked then
  Include(Options, ssoEntireScope);
  if opt_check4.Checked then
  Include(Options, ssoSelectedOnly);
  if ABackwards then
  Include(Options, ssoBackwards);

  if opt_check5.Checked then
  Editor.SearchEngine:= SynEditRegexSearch
  else
  Editor.SearchEngine:= SynEditSearch;

  if Editor.SearchReplace(SearchMemo.Text, ReplaceMemo.Text, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    if ssoBackwards in Options then
      Editor.BlockEnd := Editor.BlockBegin
    else
      Editor.BlockBegin := Editor.BlockEnd;
    Editor.CaretXY := Editor.BlockBegin;
  end;
end;

var
  ReplaceAll: Boolean;

procedure TCETextEditor.EditorReplaceText(Sender: TObject; const ASearch,
  AReplace: WideString; Line, Column: Integer; var Action: TSynReplaceAction);
var
  s: String;
  r: Integer;
begin
  if ASearch = AReplace then
    Action := raSkip
  else if ReplaceAll then
  begin
    Action:= raReplaceAll;
  end
  else
  begin
    s:= 'Replace this occurence of "' + ASearch + '"';
    r:= MessageBox(0, PChar(s), 'Replace?', MB_ICONQUESTION or MB_YESNOCANCEL);
    case r of
      idYes: Action:= raReplace;
      idNo: Action:= raSkip;
      idCancel: Action:= raCancel;
    end;
  end;
end;

procedure TCETextEditor.ModifiedChange;
begin
  if Assigned(fOnModifiedChange) then fOnModifiedChange(Self);
end;


procedure TCETextEditor.SpTBXButton1Click(Sender: TObject);
begin
  DoSearchReplaceText(false, (opt_radio.ItemIndex = 1));
end;

procedure TCETextEditor.SpTBXButton3Click(Sender: TObject);
begin
  ReplaceAll:= false;
  DoSearchReplaceText(true, (opt_radio.ItemIndex = 1));
end;

procedure TCETextEditor.SpTBXButton5Click(Sender: TObject);
begin
  ReplaceAll:= true;
  DoSearchReplaceText(true, (opt_radio.ItemIndex = 1));
end;

procedure TCETextEditor.SpTBXItem16Click(Sender: TObject);
var
  item: TSpTBXItem;
  i: Integer;
begin
  item:= TSpTBXItem(Sender);
  if item.Tag > -1 then
  begin
    Editor.Highlighter:= TSynCustomHighlighter(Highlighters.Objects[item.Tag]);
    for i:= 0 to highlighterSubmenu.Count - 1 do
    begin
      if highlighterSubmenu.Items[i] is TSpTBXItem then
      TSpTBXItem(highlighterSubmenu.Items[i]).CaptionGlow:= gldNone;
    end;
  end
  else if item.Tag = -2 then
  begin
    SetAutoHighlighter;
  end
  else
  begin
    Editor.Highlighter:= nil;
    for i:= 0 to highlighterSubmenu.Count - 1 do
    begin
      if highlighterSubmenu.Items[i] is TSpTBXItem then
      TSpTBXItem(highlighterSubmenu.Items[i]).CaptionGlow:= gldNone;
    end;
  end;
  item.Checked:= true;
end;

procedure TCETextEditor.SetAutoHighlighter;
var
  ExtLen: integer;
  i, j, i2, i3: integer;
  Highlighter: TSynCustomHighlighter;
  Filter: string;
  Extension: String;
  item: TSpTBXItem;
begin
  Extension:= ExtractFileExt(ActiveFile);
  Extension := LowerCase(Extension);
  ExtLen := Length(Extension);
  if (ExtLen > 0) then
  begin
    i3:= -1;
    for i := 0 to Highlighters.Count - 1 do
    begin
      if not (Highlighters.Objects[i] is TSynCustomHighlighter) then
      Continue;
      Highlighter := TSynCustomHighlighter(Highlighters.Objects[i]);
      Filter := LowerCase(Highlighter.DefaultFilter);
      j := Pos('|', Filter);
      if j > 0 then begin
        Delete(Filter, 1, j);
        j := Pos(Extension, Filter);
        if (j > 0) and ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';')) then
        begin
          Editor.Highlighter:= Highlighter;
          for i2:= 0 to highlighterSubmenu.Count - 1 do
          begin
            if highlighterSubmenu.Items[i2] is TSpTBXItem then
            begin
              item:= TSpTBXItem(highlighterSubmenu.Items[i2]);
              if item.Tag = i then
              item.CaptionGlow:= gldBottomRight
              else
              item.CaptionGlow:= gldNone;
            end;
          end;            
          Exit;
        end
        else if Pos('*.*', Filter) > 0 then
        i3:= i;     
      end;
    end;

    if i3 > -1 then
    begin
      Highlighter := TSynCustomHighlighter(Highlighters.Objects[i3]);
      Editor.Highlighter:= Highlighter;
      for i2:= 0 to highlighterSubmenu.Count - 1 do
      begin
        if highlighterSubmenu.Items[i2] is TSpTBXItem then
        begin
          item:= TSpTBXItem(highlighterSubmenu.Items[i2]);
          if item.Tag = i3 then
          item.CaptionGlow:= gldBottomRight
          else
          item.CaptionGlow:= gldNone;
        end;
      end;

    end;

  end;
end;

procedure TCETextEditor.PopuplateHighlighters;
var
  i: integer;
  Highlighter: TSynCustomHighlighter;
  item: TSpTBXItem;
begin
  for i := Self.ComponentCount - 1 downto 0 do
  begin
    if not (Self.Components[i] is TSynCustomHighlighter) then
    Continue;
    Highlighter:= Self.Components[i] as TSynCustomHighlighter;
      // only one highlighter for each language
    if Highlighters.IndexOf(Highlighter.GetLanguageName) = -1 then
    Highlighters.AddObject(Highlighter.GetLanguageName, Highlighter);
  end;
  Highlighters.Sort;
  for i:= 0 to Highlighters.Count - 1 do
  begin
    item:= TSpTBXItem.Create(self);
    item.Caption:= Highlighters.Strings[i];
    item.Tag:= i;
    item.RadioItem:= true;
    item.GroupIndex:= 1;
    item.OnClick:= SpTBXItem16Click;
    highlighterSubmenu.Add(item);
  end;
end;

end.
