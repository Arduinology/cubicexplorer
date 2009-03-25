unit dCE_Actions;

interface

uses
  // CE Units
  CE_GlobalCtrl,  dCE_Images, CE_TBActions, CE_Utils, CE_LanguageEngine,
  CE_SpTabBar,
  // PngControls
  PngImageList,
  // EasyListview & VSTools
  EasyListview, VirtualExplorerTree, MPCommonUtilities, MPShellUtilities,
  VirtualExplorerEasyListview, MPCommonObjects,
  // Tnt
  TntActnList, TntClipbrd, TntSysUtils, TntWindows, TntClasses, TntSystem,
  TntMenus, 
  // JVCL
  JvDockControlForm,
  // SpTBX
  SpTBXSkins,
  // GraphicEx
  GraphicEx,
  // System Units
  SysUtils, Classes, ActnList, ImgList, Controls, Windows, ExtCtrls, Forms,
  ShellAPI, AppEvnts, Messages, ShlObj, Clipbrd, Menus;

const
  MK_XBUTTON1 = $20;
  MK_XBUTTON2 = $40;

  WM_SingleInstance = WM_USER + 1;
  WM_MakeVisible = WM_USER + 100;

type
  TCustomVirtualExplorerEasyListviewHack = class(TCustomVirtualExplorerEasyListview);

  TCEActions = class(TDataModule)
    ActionList: TTntActionList;
    act_gen_exit: TTntAction;
    UpdateTimer: TTimer;
    act_navi_folderup: TTntAction;
    act_navi_refresh: TTntAction;
    act_view_folders: TTntAction;
    act_view_bookmark: TTntAction;
    act_view_large: TTntAction;
    act_view_small: TTntAction;
    act_view_list: TTntAction;
    act_view_details: TTntAction;
    act_view_thumbs: TTntAction;
    act_view_tiles: TTntAction;
    act_view_filmstrip: TTntAction;
    act_edit_copy: TTntAction;
    act_edit_cut: TTntAction;
    act_edit_paste: TTntAction;
    act_edit_delete: TTntAction;
    act_edit_selall: TTntAction;
    act_edit_invertsel: TTntAction;
    act_edit_properties: TTntAction;
    act_edit_rename: TTntAction;
    act_quick_none: TTntAction;
    act_quick_auto: TTntAction;
    act_quick_text: TTntAction;
    act_quick_image: TTntAction;
    act_quick_hex: TTntAction;
    act_help_home: TTntAction;
    act_help_forum: TTntAction;
    act_help_about: TTntAction;
    act_view_quickview: TTntAction;
    act_edit_duplicate: TTntAction;
    act_navi_texteditor: TTntAction;
    act_navi_filesearch: TTntAction;
    ApplicationEvents: TApplicationEvents;
    act_tools_mapdrive: TTntAction;
    act_tools_disconnectdrive: TTntAction;
    act_navi_back: TCEToolbarAction;
    act_navi_forward: TCEToolbarAction;
    act_edit_newfile: TCEToolbarAction;
    act_edit_copypath: TCEToolbarAction;
    act_view_showhints: TTntAction;
    act_tools_showcustomizer: TTntAction;
    act_view_hiddenfiles: TTntAction;
    act_edit_newfolder: TTntAction;
    act_view_showheaderalways: TTntAction;
    act_tools_emptytrash: TCEToolbarAction;
    act_tools_cmd: TTntAction;
    act_view_filters: TTntAction;
    act_view_statusbar: TTntAction;
    act_view_fullscreen: TTntAction;
    BackgroundCMItems_up: TTntPopupMenu;
    BackgroundCMItems_down: TTntPopupMenu;
    View1: TTntMenuItem;
    LargeIcons1: TTntMenuItem;
    SmallIcons1: TTntMenuItem;
    List1: TTntMenuItem;
    Details1: TTntMenuItem;
    iles1: TTntMenuItem;
    humbnails1: TTntMenuItem;
    Filmstrip1: TTntMenuItem;
    N1: TTntMenuItem;
    Refresh1: TTntMenuItem;
    N2: TTntMenuItem;
    Paste1: TTntMenuItem;
    Properties1: TTntMenuItem;
    CopyPath1: TTntMenuItem;
    N3: TTntMenuItem;
    act_view_showextensions: TTntAction;
    N4: TTntMenuItem;
    act_edit_paste_shortcut: TTntAction;
    PasteShortcut1: TTntMenuItem;
    act_help_poedit_form: TTntAction;
    act_view_loadskin: TTntAction;
    act_tools_showoptions: TTntAction;
    act_sessions_save: TTntAction;
    act_sessions_saveas: TTntAction;
    act_sessions_properties: TTntAction;
    act_sessions_delete: TTntAction;
    act_view_dropstack: TTntAction;
    MenuItem_ArragneBy: TTntMenuItem;
    act_view_arrangeby: TCEToolbarAction;
    act_view_viewstyle: TCEToolbarAction;
    act_view_groupby: TCEToolbarAction;
    MenuItem_GroupBy: TTntMenuItem;
    act_navi_scrollleft: TTntAction;
    act_navi_scrollright: TTntAction;
    act_view_alwaysontop: TTntAction;
    act_tabs_closetab: TTntAction;
    act_tabs_closeothertabs: TTntAction;
    act_tabs_addtab: TTntAction;
    act_tabs_duplicatetab: TTntAction;
    act_tabs_closeonleft: TTntAction;
    act_tabs_closeonright: TTntAction;
    act_gen_menu: TCEToolbarAction;
    act_navi_quickview: TTntAction;
    procedure ActionExecute(Sender: TObject);
    procedure ApplicationEventsActivate(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure BackgroundCMItems_upPopup(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure DoAssigneByClick(Sender: TObject);
    procedure DoGroupByClick(Sender: TObject);
  public
    procedure AssignCustomToolbarActions;
    procedure UpdateAll;
    { Public declarations }
  end;

// Global
procedure ExecuteCEAction(ActionID: Integer);
procedure UpdateCEAction(ActionID: Integer; TargetAction: TTntAction);

// Action Executes
procedure ExecuteGeneralCategory(ActionID: Integer);
procedure ExecuteEditCategory(ActionID: Integer);
procedure ExecuteSessionsCategory(ActionID: Integer);
procedure ExecuteTabsCategory(ActionID: Integer);
procedure ExecuteHelpCategory(ActionID: Integer);
procedure ExecuteNavigationCategory(ActionID: Integer);
procedure ExecuteQuickviewCategory(ActionID: Integer);
procedure ExecuteBookmarksCategory(ActionID: Integer);

// Action Updates
procedure UpdateGeneralCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateEditCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateSessionsCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateToolsCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateHelpCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateNavigationCategory(ActionID: Integer; TargetAction: TTntAction);
procedure UpdateQuickOptionsCategory(ActionID: Integer; TargetAction:
    TTntAction);
procedure UpdateBookmarksCategory(ActionID: Integer; TargetAction: TTntAction);

procedure MouseAction(var Msg: tagMSG; var Handled: Boolean);

procedure OpenFileInTab(FilePath: WideString; SelectTab: Boolean = true;
    ActivateApp: Boolean = false);

procedure OpenFolderInTab(Sender: TObject; PIDL: PItemIDList; SelectTab:
    Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean =
    false); overload;

procedure OpenFolderInTab(Sender: TObject; FilePath: WideString; SelectTab:
    Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean =
    false); overload;

function IsSingleInstance: Boolean;

procedure ExecuteQuickOptionsCategory(ActionID: Integer);

procedure UpdateQuickviewCategory(ActionID: Integer; TargetAction: TTntAction);

function HandleCmdParams(Str: WideString): Boolean;

procedure ExecuteViewCategory(ActionID: Integer);

procedure UpdateViewCategory(ActionID: Integer; TargetAction: TTntAction);

procedure UpdateAllActions;

procedure HandleInputMessage(var Msg : TMessage; var Handled: Boolean);

procedure UpdateTabsCategory(ActionID: Integer; TargetAction: TTntAction);

procedure ExecuteToolsCategory(ActionID: Integer);

var
  CEActions: TCEActions;

implementation

{$R *.dfm}

uses
  Main, fCE_FolderPanel, fCE_QuickViewPanel, fCE_BookmarkPanel,
  fCE_TextEditor, fCE_FileView, CE_FileView, CE_QuickView,
  CE_Bookmarks, CE_BookmarkTree, fCE_AboutBox, fCE_FileSearch,
  CE_ToolbarButtons, fCE_ToolbarCustomizer, fCE_TabPage, fCE_FiltersPanel,
  fCE_PoEditor, fCE_OptionsDialog, CE_Sessions, fCE_SessionDlg, fCE_StackPanel,
  CE_BaseFileView, fCE_QuickViewTab;

{##############################################################################}

{*------------------------------------------------------------------------------
  On Module Create
-------------------------------------------------------------------------------}
procedure TCEActions.DataModuleCreate(Sender: TObject);
begin
  AssignCustomToolbarActions;
end;

{*------------------------------------------------------------------------------
  On Module Destroy
-------------------------------------------------------------------------------}
procedure TCEActions.DataModuleDestroy(Sender: TObject);
begin
  //
end;

{*------------------------------------------------------------------------------
  Assign Custom Toolbar Actions
-------------------------------------------------------------------------------}
procedure TCEActions.AssignCustomToolbarActions;
begin
  act_navi_back.ItemClass:= TCEFileViewBackButton;
  act_navi_forward.ItemClass:= TCEFileViewForwardButton;
  act_edit_newfile.ItemClass:= TCENewFileButton;
  act_edit_copypath.ItemClass:= TCEFileViewCopyPathButton;
  act_tools_emptytrash.ItemClass:= TCEEmptyTrashButton;
  act_view_arrangeby.ItemClass:= TCEArrangeByButton;
  act_view_viewstyle.ItemClass:= TCEViewStyleButton;
  act_view_groupby.ItemClass:= TCEGroupByButton;
  act_gen_menu.ItemClass:= TCEMainMenuButton;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Execute action (ALL CATEGORIES)
-------------------------------------------------------------------------------}
procedure TCEActions.ActionExecute(Sender: TObject);
var
  act: TTntAction;
begin
  act:= TTntAction(Sender);
  ExecuteCEAction(act.Tag);
  UpdateTimerTimer(Sender);
end;

{*------------------------------------------------------------------------------
  Get's called on Action Update timer (UPDATE ALL CATEGORIES)
-------------------------------------------------------------------------------}
procedure TCEActions.UpdateTimerTimer(Sender: TObject);
begin
  UpdateAll;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Execute Action.
-------------------------------------------------------------------------------}
procedure ExecuteCEAction(ActionID: Integer);
begin
  case ActionID of
    100..199: ExecuteGeneralCategory(ActionID);
    200..299: ExecuteEditCategory(ActionID);
    300..399: ExecuteViewCategory(ActionID);
    400..499: ExecuteToolsCategory(ActionID);
    500..599: ExecuteHelpCategory(ActionID);
    600..659: ExecuteNavigationCategory(ActionID);
    660..699: ExecuteTabsCategory(ActionID);
    700..799: ExecuteQuickviewCategory(ActionID);
    800..849: ExecuteBookmarksCategory(ActionID);
    850..899: ExecuteSessionsCategory(ActionID);
    900..999: ExecuteQuickOptionsCategory(ActionID);
  end;
end;

{*------------------------------------------------------------------------------
  Update Action
-------------------------------------------------------------------------------}
procedure UpdateCEAction(ActionID: Integer; TargetAction: TTntAction);
begin
  case ActionID of
    100..199: UpdateGeneralCategory(ActionID, TargetAction);
    200..299: UpdateEditCategory(ActionID, TargetAction);
    300..399: UpdateViewCategory(ActionID, TargetAction);
    400..499: UpdateToolsCategory(ActionID, TargetAction);
    500..599: UpdateHelpCategory(ActionID, TargetAction);
    600..659: UpdateNavigationCategory(ActionID, TargetAction);
    660..699: UpdateTabsCategory(ActionID, TargetAction);
    700..799: UpdateQuickviewCategory(ActionID, TargetAction);
    800..849: UpdateBookmarksCategory(ActionID, TargetAction);
    850..899: UpdateSessionsCategory(ActionID, TargetAction);
    900..999: UpdateQuickOptionsCategory(ActionID, TargetAction);
  end;
end;

{##############################################################################}


{*------------------------------------------------------------------------------
  File Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteGeneralCategory(ActionID: Integer);
begin
  case ActionID of
    100: Application.MainForm.Close; // Exit
    //101: MainForm.SingleInstance:= not MainForm.SingleInstance;
    // TODO: fix this
    //102: CESettings.MainFormSettings.RememberTabs:= not CESettings.MainFormSettings.RememberTabs;
  end;
end;

{*------------------------------------------------------------------------------
  File Category Update
-------------------------------------------------------------------------------}
procedure UpdateGeneralCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
//  case ActionID of
//    //101: TargetAction.Checked:= MainForm.SingleInstance;
//    // TODO: fix this
//    //102: TargetAction.Checked:= CESettings.MainFormSettings.RememberTabs;
//  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Edit Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteEditCategory(ActionID: Integer);
var
  fileview: TCEFileView;
  NS: TNamespace;
begin
  fileview:= nil;
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  fileview:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;

  if CEFolderPanel.FolderTree.Focused then
  begin
    case ActionID of
      201: CEFolderPanel.FolderTree.CopyToClipBoard;
      202: CEFolderPanel.FolderTree.CutToClipBoard;
      203: CEFolderPanel.FolderTree.PasteFromClipboard;
      204: CEFolderPanel.FolderTree.DeleteSelectedNodes;
      208: if CEFolderPanel.FolderTree.FocusedNode <> nil then
           CEFolderPanel.FolderTree.EditNode(CEFolderPanel.FolderTree.FocusedNode, -1);
      213: CEFolderPanel.FolderTree.PasteShortcutFromClipboard;
    end;
  end
  else if assigned(fileview) then
  begin
    if fileview.Focused then
    begin
      case ActionID of
        201: fileview.CopyToClipboard;
        202: fileview.CutToClipboard;
        203: fileview.PasteFromClipboard;
        204: fileview.SelectedFilesDelete;
        208: if fileview.Selection.FocusedItem <> nil then
             fileview.Selection.FocusedItem.Edit;
        209: begin
               fileview.CopyToClipboard;
               fileview.PasteFromClipboard;
             end;
        213: fileview.PasteShortcutFromClipboard;
      end;
    end;
  end;

  if assigned(fileview) then
  begin
    case ActionID of
      205: fileview.Selection.SelectAll;
      206: fileview.Selection.Invert;
      207: begin
             if fileview.ValidateNamespace(fileview.Selection.First, NS) then
             NS.ShowPropertySheetMulti(fileview.SelectedToNamespaceArray, false)
             else
             fileview.RootFolderNamespace.ShowPropertySheet;
           end;
      211: begin
             if fileview.Selection.Count > 1 then
             TntClipboard.AsText:= fileview.SelectedPaths.Text
             else if fileview.Selection.Count = 1 then
             TntClipboard.AsText:= fileview.SelectedPath
             else
             TntClipboard.AsText:= IncludeTrailingBackslashW(fileview.RootFolderNamespace.NameForParsing);
           end;
      212: begin
             fileview.CreateNewFolder;
           end;
    end;
  end;

end;

{*------------------------------------------------------------------------------
  Edit Category Update
-------------------------------------------------------------------------------}
procedure UpdateEditCategory(ActionID: Integer; TargetAction: TTntAction);
var
  fileview: TCEFileView;
  NS: TNamespace;
begin
  TargetAction.Enabled:= false;
  fileview:= nil;
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  fileview:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView;

  if CEFolderPanel.FolderTree.Focused then
  begin
    case ActionID of
      201,202,204: begin
        TargetAction.Enabled:= CEFolderPanel.FolderTree.SelectedCount > 0;
      end;
      203,213: TargetAction.Enabled:= ClipboardContainsShellFormats;
      208: begin
             if CEFolderPanel.FolderTree.FocusedNode <> nil then
             begin
               if CEFolderPanel.FolderTree.ValidateNamespace(CEFolderPanel.FolderTree.FocusedNode, NS) then
               TargetAction.Enabled:= NS.CanRename
               else
               TargetAction.Enabled:= false
             end;
           end;      
    end;
  end
  else if assigned(fileview) then
  begin
    if fileview.Focused then
    begin
      case ActionID of
        201,202,204,209:
          TargetAction.Enabled:=  fileview.Selection.Count > 0;
        203,213: TargetAction.Enabled:= ClipboardContainsShellFormats;
        208: begin
               if fileview.Selection.FocusedItem <> nil then
               begin
                 if fileview.ValidateNamespace(fileview.Selection.FocusedItem, NS) then
                 TargetAction.Enabled:= NS.CanRename
                 else
                 TargetAction.Enabled:= false
               end;
             end;
      end;
    end;
  end;

  if assigned(fileview) then
  begin
    case ActionID of
      205,206,207,210,211,212: TargetAction.Enabled:= true;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  View Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteViewCategory(ActionID: Integer);
begin
  case ActionID of
    300: MainForm.StatusBar.Visible:= not MainForm.StatusBar.Visible;
    301: if GetFormVisible(CEFolderPanel) then HideDockForm(CEFolderPanel) else ShowDockForm(CEFolderPanel);
    302: if GetFormVisible(CEBookmarkPanel) then HideDockForm(CEBookmarkPanel) else ShowDockForm(CEBookmarkPanel);
    303: if GetFormVisible(CEQuickviewPanel) then HideDockForm(CEQuickviewPanel) else ShowDockForm(CEQuickviewPanel);
    304: if GetFormVisible(CEFiltersPanel) then HideDockForm(CEFiltersPanel) else ShowDockForm(CEFiltersPanel);
    305: if GetFormVisible(CEStackPanel) then HideDockForm(CEStackPanel) else ShowDockForm(CEStackPanel);
    330: MainForm.ShowHint:= not MainForm.ShowHint;
    332: begin
      GlobalFileViewSettings.HiddenFiles:= not GlobalFileViewSettings.HiddenFiles;
      CEFolderPanel.FolderTree.HiddenFiles:= GlobalFileViewSettings.HiddenFiles;
    end;
    333: GlobalFileViewSettings.ShowHeaderAlways:= not GlobalFileViewSettings.ShowHeaderAlways;
    334: GlobalFileViewSettings.ShowExtensions:= not GlobalFileViewSettings.ShowExtensions;
    335: if MainForm.FormStyle = fsStayOnTop then
         MainForm.FormStyle:= fsNormal
         else
         MainForm.FormStyle:= fsStayOnTop;
    351..357: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
              begin
                TCEFileViewPage(GlobalPathCtrl.ActivePage).ViewStyle:= TEasyListStyle(Ord(ActionID - 351));
              end;
    370: MainForm.Fullscreen:= not MainForm.Fullscreen;
    371: MainForm.OpenSkin;
  end;
end;

{*------------------------------------------------------------------------------
  View Category Update
-------------------------------------------------------------------------------}
procedure UpdateViewCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
  case ActionID of
    300: TargetAction.Checked:= MainForm.StatusBar.Visible;
    301: TargetAction.Checked:= CEFolderPanel.IsVisible;
    302: TargetAction.Checked:= CEBookmarkPanel.IsVisible;
    303: TargetAction.Checked:= CEQuickviewPanel.IsVisible;
    304: TargetAction.Checked:= CEFiltersPanel.IsVisible;
    305: TargetAction.Checked:= CEStackPanel.IsVisible;
    330: TargetAction.Checked:= MainForm.ShowHint;
    332: TargetAction.Checked:= GlobalFileViewSettings.HiddenFiles;
    333: TargetAction.Checked:= GlobalFileViewSettings.ShowHeaderAlways;
    334: TargetAction.Checked:= GlobalFileViewSettings.ShowExtensions;
    335: TargetAction.Checked:= MainForm.FormStyle = fsStayOnTop;
    351..357: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
              begin
                if ActionID = (Ord(TCEFileViewPage(GlobalPathCtrl.ActivePage).ViewStyle) + 351) then
                begin
                  TargetAction.Checked:= true;
                  CEActions.act_view_viewstyle.ImageIndex:= TargetAction.ImageIndex;
                end
                else
                TargetAction.Checked:= false;
              end
              else
              begin
                TargetAction.Enabled:= false;
                TargetAction.Checked:= false;
              end;
    370: TargetAction.Checked:= MainForm.Fullscreen;
    372..374: TargetAction.Enabled:= GlobalPathCtrl.ActivePage is TCEFileViewPage;
  end;
end;



{##############################################################################}

{*------------------------------------------------------------------------------
  Tools Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteToolsCategory(ActionID: Integer);
var
  ws: WideString;
begin
  case ActionID of
    401: ShowCustomizer(MainForm);
    402: ShowOptionsDialog;
    451: WNetConnectionDialog(MainForm.Handle, RESOURCETYPE_DISK);
    452: WNetDisconnectDialog(MainForm.Handle, RESOURCETYPE_DISK);
    453: EmptyRecycleBin;
    454: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           ws:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.NameForParsing;
           if not WideDirectoryExists(ws) then
           ws:= '';
           Tnt_ShellExecuteW(0,'open','cmd.exe','',PWideChar(ws), SW_SHOW);
         end;
  end;
end;

{*------------------------------------------------------------------------------
  Tools Category Update
-------------------------------------------------------------------------------}
procedure UpdateToolsCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
  case ActionID of
    453: begin

         end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Help Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteHelpCategory(ActionID: Integer);
var
  form: TCEPoEditorForm;
begin
  case ActionID of
    501: ShellExecute(0,'open','http://www.cubicreality.com','','',SW_NORMAL);
    502: ShellExecute(0,'open','http://www.cubicreality.com/forum','','',SW_NORMAL);
    503: ShowAboutBox;
    504: begin
      form:= TCEPoEditorForm.CreateNew(MainForm,0);
      form.PoEditor.LocaleDir:= exePath + 'Locale\';
      form.PoEditor.OnTranslateUI:= MainForm.TranslateUI;
      if WideFileExists(exePath + 'Locale\default.pot') then
      form.PoEditor.POTFile.LoadFromFile(exePath + 'Locale\default.pot');
      form.PoEditor.TabControl.ActiveTabIndex:= 0;
      form.PoEditor.POT_Rev:= GetFileVersionBuild(WideParamStr(0));
      form.PoEditor.ActiveLanguage:= MainForm.ActiveLanguage;
      form.Show;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Help Category Update
-------------------------------------------------------------------------------}
procedure UpdateHelpCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Navigation Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteNavigationCategory(ActionID: Integer);
var
  page: TCECustomTabPage;
  ws,ext: WideString;
  ns: TNamespace;
  item: TEasyItem;
  editor: TCETextEditorPage;
  quickview: TCEQuickViewPage;
begin
  case ActionID of
    601: begin
           ExecuteTabsCategory(663);
         end;
    602: begin
           MainForm.TabSet.CloseSelectedTab;
         end;
    603: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.GoBackInHistory;
         end;
    604: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.GoForwardInHistory;
         end;
    605: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.GoFolderUp;
         end;
    606: begin
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Rebuild;
           CEFolderPanel.FolderTree.Refresh;
           MainForm.DriveToolbar.Populate;
           MainForm.StatusBar.UpdateLabels(true);
         end;
    607: begin
           ExecuteTabsCategory(664);
         end;
    608: MainForm.TabSet.ScrollLeft;
    609: MainForm.TabSet.ScrollRight;
    650: begin
           GlobalFileViewSettings.AssignFromActivePage;
           ws:= '';
           if (GlobalPathCtrl.ActivePage is TCEFileViewPage) then
           begin
             item:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Selection.First;
             if TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end
           else if (GlobalPathCtrl.ActivePage is TCEFileSearchPage) then
           begin
             item:= TCEFileSearchPage(GlobalPathCtrl.ActivePage).Results.Selection.First;
             if TCEFileSearchPage(GlobalPathCtrl.ActivePage).Results.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end;

           editor:= TCETextEditorPage(MainForm.TabSet.AddTab(TCETextEditorPage, MainForm.TabSet.NewTabSelect).Page);
           if (ws <> '') then
           begin
            ext:= WideUpperCase(WideExtractFileExt(ws));
            case QuickViewSettings.GetViewType(ext) of
              qvImage, qvVideo:
              else
              begin
                if (ext <> '.EXE') and (ext <> '.DLL') then
                editor.OpenDocument(ws);
              end;
            end;
           end;
         end;
    651: begin
           GlobalFileViewSettings.AssignFromActivePage;
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           ws:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.NameForParsing;
           page:= TCECustomTabPage(MainForm.TabSet.AddTab(TCEFileSearchPage, MainForm.TabSet.NewTabSelect).Page);
           if WideDirectoryExists(ws) then
           begin
             TCEFileSearchPage(page).DestinationEdit.Text:= ws;
           end;
         end;
    652: begin
           GlobalFileViewSettings.AssignFromActivePage;
           ws:= '';
           if GlobalPathCtrl.ActivePage is TCEFileViewPage then
           begin
             item:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.Selection.First;
             if TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end
           else if (GlobalPathCtrl.ActivePage is TCEFileSearchPage) then
           begin
             item:= TCEFileSearchPage(GlobalPathCtrl.ActivePage).Results.Selection.First;
             if TCEFileSearchPage(GlobalPathCtrl.ActivePage).Results.ValidateNamespace(item, ns) then
             begin
               if NS.FileSystem and not NS.Folder then
               begin
                 ws:= NS.NameForParsing;
               end;
             end;
           end;
           
           quickview:= TCEQuickViewPage(MainForm.TabSet.AddTab(TCEQuickViewPage, MainForm.TabSet.NewTabSelect).Page);
           if ws <> '' then
           quickview.OpenFile(ws);
         end;
  end;
end;

{*------------------------------------------------------------------------------
  Navigation Category  Update
-------------------------------------------------------------------------------}
procedure UpdateNavigationCategory(ActionID: Integer; TargetAction: TTntAction);
var
  L,R: Boolean;
begin
  case ActionID of
    //602: TargetAction.Enabled:= MainForm.TabSet.TabCount > 1;
    603: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.History.HasBackItems
         else
         TargetAction.Enabled:= false;
    604: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.History.HasNextItems
         else
         TargetAction.Enabled:= false;
    605: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= (TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.Parent <> nil)
         else
         TargetAction.Enabled:= false;
    606,607: if GlobalPathCtrl.ActivePage is TCEFileViewPage then
         TargetAction.Enabled:= True
         else
         TargetAction.Enabled:= false;
    608, 609: begin
      MainForm.TabSet.ScrollState(L, R);
      if L or R then
      begin
        if not CEActions.act_navi_scrollleft.Visible then
        begin
          CEActions.act_navi_scrollleft.Visible:= true;
          CEActions.act_navi_scrollright.Visible:= true;
        end;
        CEActions.act_navi_scrollleft.Enabled:= L;
        CEActions.act_navi_scrollright.Enabled:= R;
      end
      else
      begin
        if CEActions.act_navi_scrollleft.Visible then
        begin
          CEActions.act_navi_scrollleft.Visible:= false;
          CEActions.act_navi_scrollright.Visible:= false;
        end;
      end;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Tabs Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteTabsCategory(ActionID: Integer);
var
  pidl: PItemIDList;
begin
  if not assigned(MainForm.TabSet.ActivePopupTab) then
  begin
    MainForm.TabSet.ActivePopupTab:= MainForm.TabSet.GetActiveTab;
  end;

  case ActionID of
    661: MainForm.TabSet.CloseTab(MainForm.TabSet.ActivePopupTab);
    662: MainForm.TabSet.CloseAllTabs(MainForm.TabSet.ActivePopupTab);
    663: begin
           GlobalFileViewSettings.AssignFromActivePage;
           if MainForm.TabSet.NewTabType = 1 then // Clone active tab
           begin
             if GlobalPathCtrl.ActivePage is TCEFileViewPage then
             begin
              if MainForm.TabSet.Toolbar.GetTabsCount(true) = 0 then
              pidl:= DrivesFolder.AbsolutePIDL
              else
              pidl:= TCEFileViewPage(GlobalPathCtrl.ActivePage).FileView.RootFolderNamespace.AbsolutePIDL
             end
             else
             pidl:= nil;
             OpenFolderInTab(nil, pidl, MainForm.TabSet.NewTabSelect, true, true);
           end
           else if MainForm.TabSet.NewTabType = 3 then // Open custom path
           begin
             OpenFolderInTab(nil, MainForm.TabSet.NewTabNamespace.AbsolutePIDL, MainForm.TabSet.NewTabSelect, true, true);
           end
           else // Open desktop
           begin
             OpenFolderInTab(nil, nil, MainForm.TabSet.NewTabSelect, true, true);
           end;
         end;
    664: begin
           if assigned(MainForm.TabSet.ActivePopupTab) then
           begin
             if MainForm.TabSet.ActivePopupTab.Page is TCEFileViewPage then
             OpenFolderInTab(MainForm.TabSet.ActivePopupTab,
                             TCEFileViewPage(MainForm.TabSet.ActivePopupTab.Page).FileView.RootFolderNamespace.AbsolutePIDL, MainForm.TabSet.NewTabSelect, true, true);
           end;
         end;
    665: MainForm.TabSet.CloseTabsOnLeft(MainForm.TabSet.ActivePopupTab);
    666: MainForm.TabSet.CloseTabsOnRight(MainForm.TabSet.ActivePopupTab);
  end;

  MainForm.TabSet.ActivePopupTab:= nil; // popup has closed so set this to nil
end;

{*------------------------------------------------------------------------------
  Tabs Category  Update
-------------------------------------------------------------------------------}
procedure UpdateTabsCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  case ActionID of
    661..662,664: TargetAction.Enabled:= assigned(MainForm.TabSet.ActivePopupTab) or (MainForm.TabSet.GetActiveTab <> nil);
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Quickview Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteQuickviewCategory(ActionID: Integer);
begin
  case ActionID of
    701: CEQuickviewPanel.Viewer.ViewType:= qvNone;
    702: CEQuickviewPanel.Viewer.ViewType:= qvAuto;
    703: CEQuickviewPanel.Viewer.ViewType:= qvMemo;
    704: CEQuickviewPanel.Viewer.ViewType:= qvImage;
    705: CEQuickviewPanel.Viewer.ViewType:= qvHex;
  end;
end;

{*------------------------------------------------------------------------------
  Quickview Category Update
-------------------------------------------------------------------------------}
procedure UpdateQuickviewCategory(ActionID: Integer; TargetAction: TTntAction);
var
  i: Integer;
begin
  i:= Ord(CEQuickviewPanel.Viewer.ViewType) + 701;
  if ActionID = i then
  begin
    if not TargetAction.Checked then
    TargetAction.Checked:= true;
  end
  else
  TargetAction.Checked:= false;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Bookmarks Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteBookmarksCategory(ActionID: Integer);
begin
/// TODO: What's going on here?

//
//  case ActionID of
//    801: MainForm.BookmarkTree.InsertBookmark(MainForm.BookmarkTree.FocusedNode,amAddChildLast,nil, 'Category', true);
//    802: begin
//           if GlobalPathCtrl.ActiveFileView <> nil then
//           begin
//             MainForm.BookmarkTree.InsertBookmark(MainForm.BookmarkTree.FocusedNode,
//                                                  amAddChildLast,
//                                                  PIDLMgr.CopyPIDL(GlobalPathCtrl.ActiveFileView.RootFolderNamespace.AbsolutePIDL));
//           end;
//         end;
//    803: MainForm.BookmarkTree.EditNode(MainForm.BookmarkTree.FocusedNode, 0);
//    804: begin
//           MainForm.BookmarkTree.SafeDeleteSelectedNodes;
//         end;
//  end;
end;

{*------------------------------------------------------------------------------
  Bookmarks Category Update
-------------------------------------------------------------------------------}
procedure UpdateBookmarksCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
end;


{##############################################################################}

{*------------------------------------------------------------------------------
  Session Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteSessionsCategory(ActionID: Integer);
begin
  case ActionID of
    851: GlobalSessions.SaveToSession(GlobalSessions.ActiveSession);
    852: NewSession;
    853: EditSession(GlobalSessions.ActiveSession);
    854: begin
      if (MessageBox(0, 'Are you sure you want to delete active session?', 'Delete Session?', MB_ICONQUESTION or MB_YESNO) = idYes) then
      GlobalSessions.DeleteSession(GlobalSessions.ActiveSession);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Session Category Update
-------------------------------------------------------------------------------}
procedure UpdateSessionsCategory(ActionID: Integer; TargetAction: TTntAction);
begin
  TargetAction.Enabled:= true;
  case ActionID of
    854: TargetAction.Enabled:= not GlobalSessions.IsDefaultSession;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Mouse Input actions
-------------------------------------------------------------------------------}
procedure MouseAction(var Msg: tagMSG; var Handled: Boolean);
var
  Shift: Integer;
begin  
  if (Msg.message = 523) or (Msg.message = 525) then // Navigation buttons
  begin
    Shift:= Lo(Msg.wParam);
    if Shift = MK_XBUTTON1 then  // back
    ExecuteNavigationCategory(603)
    else if Shift = MK_XBUTTON2 then // forward
    ExecuteNavigationCategory(604);
  end
end;

{*------------------------------------------------------------------------------
  Open file in a new tab
-------------------------------------------------------------------------------}
procedure OpenFileInTab(FilePath: WideString; SelectTab: Boolean = true;
    ActivateApp: Boolean = false);
var
  editor: TCETextEditorPage;
  quickview: TCEQuickViewPage;
  filetype: TCEQuickViewType;
  ext: WideString;
begin
  if WideFileExists(FilePath) then
  begin
    GlobalFileViewSettings.AssignFromActivePage;

    ext:= WideExtractFileExt(FilePath);
    filetype:= QuickViewSettings.GetViewType(ext);
    case filetype of
      qvImage, qvVideo: begin
        quickview:= TCEQuickViewPage(MainForm.TabSet.AddTab(TCEQuickViewPage, SelectTab).Page);
        quickview.OpenFile(FilePath);
      end;
      else
      begin
        editor:= TCETextEditorPage(MainForm.TabSet.AddTab(TCETextEditorPage, SelectTab).Page);
        editor.OpenDocument(FilePath);
      end;
    end;

    if ActivateApp then
    MainForm.MakeVisible;
  end;
end;

{*------------------------------------------------------------------------------
  Open Folder in a new tab
-------------------------------------------------------------------------------}
procedure OpenFolderInTab(Sender: TObject; FilePath: WideString; SelectTab:
    Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false);
var
  PIDL: PItemIDList;
begin
  PIDL:= PathToPIDL(FilePath);
  OpenFolderInTab(Sender, PIDL, SelectTab, ActivateApp, ForceNewTab);
end;

{*------------------------------------------------------------------------------
  Open Folder in a new tab
-------------------------------------------------------------------------------}
procedure OpenFolderInTab(Sender: TObject; PIDL: PItemIDList; SelectTab:
    Boolean = true; ActivateApp: Boolean = false; ForceNewTab: Boolean = false);
var
  page: TCEFileViewPage;
  item: TCESpTabItem;
  i: Integer;
begin
  item:= nil;
  GlobalFileViewSettings.AssignFromActivePage;
  if MainForm.TabSet.ReuseTabs and not ForceNewTab then
  begin
    for i:= 0 to MainForm.TabSet.Items.Count -1 do
    begin
      if MainForm.TabSet.Items.Items[i] is TCESpTabItem then
      begin
        item:= TCESpTabItem(MainForm.TabSet.Items.Items[i]);
        if item.Page is TCEFileViewPage then
        begin
          page:= TCEFileViewPage(item.Page);
          if ILIsEqual(PIDL, page.FileView.RootFolderNamespace.AbsolutePIDL) then
          begin
            MainForm.TabSet.SelectTab(item);
            if ActivateApp then
            MainForm.MakeVisible;
            break;
          end
          else
          item:= nil;
        end
        else
        item:= nil;
      end;
    end;
  end;

  if not assigned(item) then
  begin
    item:= MainForm.TabSet.AddTab(TCEFileViewPage, false, false);
    if assigned(item) then
    begin
      page:= TCEFileViewPage(item.Page);
      page.FileView.fChangeHistory:= false;
      page.FileView.Selection.ClearAll;
      page.FileView.RootFolderCustomPIDL:= PIDL;
      page.UpdateCaption;
      if page.FileView.Selection.First <> nil then
      page.FileView.Selection.First.MakeVisible(emvMiddle);
      page.FileView.ClearHistory;
      page.FileView.History.Add(TNamespace.Create(PIDLMgr.CopyPIDL(PIDL),nil),true);
      page.FileView.fChangeHistory:= true;
      page.Active:= true;
      if SelectTab or (MainForm.TabSet.Toolbar.GetTabsCount(true) = 1) then
      MainForm.TabSet.SelectTab(item);
      if ActivateApp then
      MainForm.MakeVisible;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Is Single Instance query
-------------------------------------------------------------------------------}
function IsSingleInstance: Boolean;
begin
  Result:= MainForm.SingleInstance;
  if Result then
  Application.BringToFront;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Quick Options Category Execute
-------------------------------------------------------------------------------}
procedure ExecuteQuickOptionsCategory(ActionID: Integer);
begin
  //
end;

{*------------------------------------------------------------------------------
  Quick Options Category Update
-------------------------------------------------------------------------------}
procedure UpdateQuickOptionsCategory(ActionID: Integer; TargetAction:
    TTntAction);
begin
  //
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Handle Command Line input
-------------------------------------------------------------------------------}
function HandleCmdParams(Str: WideString): Boolean;
var
  list: TTntStrings;
  i: Integer;
  pidl: PItemIDList;
  path: WideString;
  TabOpened: Boolean;
  IsShortcut: Boolean;
  NS: TNamespace;
begin
  Result:= false;
  list:= TTntStringList.Create;
  try
    list.Delimiter:= ',';
    list.StrictDelimiter:= true;
    list.DelimitedText:= Str;
    
    i:= 0;
    TabOpened:= false;
    IsShortcut:= false;
    while i < list.Count do
    begin
      if IsSameText(list.Strings[i], '/idlist') and not TabOpened then
      begin
        i:= i + 1;
        if i < list.Count then
        begin
          pidl:= StringToPIDL(list.Strings[i]);
          if pidl <> nil then
          begin
            OpenFolderInTab(nil, pidl, true, true);
            Result:= true;
            TabOpened:= true;
          end;
        end;
        IsShortcut:= false;
      end
      else if IsSameText(list.Strings[i], '/n') then
      begin
        TabOpened:= false;
        IsShortcut:= false;
      end
      else if IsSameText(list.Strings[i], '/link') then
      begin
        TabOpened:= false;
        IsShortcut:= true;
      end
      else
      begin
        path:= list.Strings[i];
        
        if IsShortcut then
        begin
          if not WideFileExists(path) then
          path:= DecodeRelativePath(path);
          if WideFileExists(path) then
          begin
            NS:= TNamespace.CreateFromFileName(path);
            try
              if NS.Link then
              begin
                OpenFolderInTab(nil, NS.ShellLink.TargetIDList, true, true);
                Result:= true;
                TabOpened:= true;
              end;
            finally
              NS.Free;
            end;
          end;
          IsShortcut:= false;
        end;

        if not TabOpened then
        begin
          ReplaceSystemVariablePath(path);
          path:= IncludeTrailingBackslashW(path);

          if not DirExistsVET(path, false) then
          path:= DecodeRelativePath(path);

          if DirExistsVET(path, false) then
          begin
            OpenFolderInTab(nil, path, true, true);
            Result:= true;
            TabOpened:= true;
          end
        end;
      end;
      i:= i + 1;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Update All Actions
-------------------------------------------------------------------------------}
procedure UpdateAllActions;
begin
  if assigned(CEActions) then
  CEActions.UpdateAll
end;

{-------------------------------------------------------------------------------
  Handle Input message
-------------------------------------------------------------------------------}
procedure HandleInputMessage(var Msg : TMessage; var Handled: Boolean);
var
  ws: WideString;
begin
  case msg.Msg of
    // Single instance question
    WM_SingleInstance: begin
      if IsSingleInstance then
      Msg.Result:= 0
      else
      Msg.Result:= -1;
      Handled:= true;
    end;
    // Copy Data for command line parameters
    WM_COPYDATA: begin
      ws:= PWideChar(TWMCopyData(Msg).CopyDataStruct.lpData);
      HandleCmdParams(ws);
      Handled:= true;
    end;
    // Make CE Visible
    WM_MakeVisible: begin
      MainForm.MakeVisible;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On Application Activate
-------------------------------------------------------------------------------}
procedure TCEActions.ApplicationEventsActivate(Sender: TObject);
begin
  UpdateAll;
end;

{*------------------------------------------------------------------------------
  Application Messages
-------------------------------------------------------------------------------}
procedure TCEActions.ApplicationEventsMessage(var Msg: tagMSG;
  var Handled: Boolean);
begin
  case Msg.message of
    512..525: MouseAction(Msg, Handled);  
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On BackgroundCMItems_up Popup
-------------------------------------------------------------------------------}
procedure TCEActions.BackgroundCMItems_upPopup(Sender: TObject);
var
  item: TMenuItem;
  page: TCEFileViewPage;
  col: TEasyColumn;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  begin
    MenuItem_ArragneBy.Clear;
    page:= TCEFileViewPage(GlobalPathCtrl.ActivePage);
    col:= page.FileView.Header.FirstVisibleColumn;
    while assigned(col) do
    begin
      item:= BackgroundCMItems_up.CreateMenuItem;
      item.Caption:= col.Caption;
      item.OnClick:= DoAssigneByClick;
      item.Tag:= Integer(col);
      item.RadioItem:= true;
      if col.SortDirection <> esdNone then
      item.Checked:= true;
      MenuItem_ArragneBy.Add(item);
      col:= page.FileView.Header.NextVisibleColumn(col);
    end;
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= '-';
    item.Tag:= 0;
    MenuItem_ArragneBy.Add(item);

    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= _('More...');
    item.Tag:= -1;
    item.OnClick:= DoAssigneByClick;
    MenuItem_ArragneBy.Add(item);

    // Group By
      // Sho in Groups
    MenuItem_GroupBy.Clear;
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= _('Show in Groups');
    item.Checked:= page.FileView.Grouped;
    item.Tag:= -2;
    item.OnClick:= DoGroupByClick;
    MenuItem_GroupBy.Add(item);
      // separator
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= '-';
    item.Tag:= 0;
    MenuItem_GroupBy.Add(item);
      // group by items
    col:= page.FileView.Header.FirstVisibleColumn;
    while assigned(col) do
    begin
      item:= BackgroundCMItems_up.CreateMenuItem;
      item.Caption:= col.Caption;
      item.OnClick:= DoGroupByClick;
      item.Tag:= Integer(col);
      item.RadioItem:= true;
      item.Checked:= page.FileView.GroupingColumn = col.Index;
      MenuItem_GroupBy.Add(item);
      col:= page.FileView.Header.NextVisibleColumn(col);
    end;
      // separator
    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= '-';
    item.Tag:= 0;
    MenuItem_GroupBy.Add(item);

    item:= BackgroundCMItems_up.CreateMenuItem;
    item.Caption:= _('More...');
    item.Tag:= -1;
    item.OnClick:= DoGroupByClick;
    MenuItem_GroupBy.Add(item);
  end;
end;

{-------------------------------------------------------------------------------
  Do AssigneBy Click
-------------------------------------------------------------------------------}
procedure TCEActions.DoAssigneByClick(Sender: TObject);
var
  item: TMenuItem;
  col, tmpCol: TEasyColumn;
  view: TCECustomFileView;
begin
  item:= TMenuItem(Sender);
  if item.Tag = -1 then
  begin
    if GlobalPathCtrl.ActivePage is TCEFileViewPage then
    begin
      TCEFileViewPage(GlobalPathCtrl.ActivePage).ShowHeaderSelector;
    end;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    view:= TCECustomFileView(col.OwnerListview);
    view.BeginUpdate;
    try
      tmpCol:= view.Header.FirstColumn;
      while assigned(tmpCol) do
      begin
        if tmpCol <> col then
        tmpCol.SortDirection:= esdNone
        else
        tmpCol.SortDirection:= esdAscending;
        tmpCol:= view.Header.NextColumn(tmpCol);
      end;
    finally
      view.EndUpdate(true);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Do GroupBy Click
-------------------------------------------------------------------------------}
procedure TCEActions.DoGroupByClick(Sender: TObject);
var
  item: TMenuItem;
  col: TEasyColumn;
  page: TCEFileViewPage;
begin
  if GlobalPathCtrl.ActivePage is TCEFileViewPage then
  page:= TCEFileViewPage(GlobalPathCtrl.ActivePage)
  else
  Exit;
  
  item:= TMenuItem(Sender);
  if item.Tag = -2 then
  begin
    page.FileView.Grouped:= not page.FileView.Grouped;
  end
  else if item.Tag = -1 then
  begin
    page.ShowHeaderSelector;
  end
  else
  begin
    col:= TEasyColumn(item.Tag);
    if assigned(col) then
    page.FileView.GroupingColumn:= col.Index;
  end;
end;

{-------------------------------------------------------------------------------
  Update All actions
-------------------------------------------------------------------------------}
procedure TCEActions.UpdateAll;
var
  act: TTntAction;
  i: Integer;
begin
  for i:= 0 to ActionList.ActionCount - 1 do
  begin
    act:= TTntAction(ActionList.Actions[i]);
    UpdateCEAction(act.Tag, act);
  end;
end;

end.
