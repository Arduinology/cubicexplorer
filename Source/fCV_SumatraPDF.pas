unit fCV_SumatraPDF;

interface

uses
  // CubicCore
  ccFileUtils,
  // CubicExplorer
  CE_Toolbar, CE_Utils,
  // Tnt
  TntRegistry, TntActnList, TntDialogs, TntForms,
  // SpTBX
  TB2Dock, TB2Toolbar, SpTBXItem, TB2Item,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ActnList, StdCtrls, TntStdCtrls;

type
  TSumatraPDF = class(TTntForm)
    TopDock: TSpTBXDock;
    ActionList: TTntActionList;
    act_file_open: TTntAction;
    act_file_close: TTntAction;
    act_file_properties: TTntAction;
    toolbar_menu: TCEToolbar;
    sub_file: TSpTBXSubmenuItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    sub_view: TSpTBXSubmenuItem;
    act_view_single_page: TTntAction;
    act_view_facing: TTntAction;
    act_view_book_view: TTntAction;
    act_view_continuosly: TTntAction;
    act_view_rotate_left: TTntAction;
    act_view_rotate_right: TTntAction;
    act_view_presentation: TTntAction;
    act_view_fullscreen: TTntAction;
    act_view_bookmarks: TTntAction;
    SpTBXItem6: TSpTBXItem;
    SpTBXItem7: TSpTBXItem;
    SpTBXItem8: TSpTBXItem;
    SpTBXItem9: TSpTBXItem;
    SpTBXItem10: TSpTBXItem;
    SpTBXItem11: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SpTBXItem12: TSpTBXItem;
    act_view_select_all: TTntAction;
    act_view_copy_selection: TTntAction;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SpTBXItem13: TSpTBXItem;
    SpTBXItem14: TSpTBXItem;
    act_view_settings: TTntAction;
    act_goto_next_page: TTntAction;
    act_goto_prev_page: TTntAction;
    act_goto_first_page: TTntAction;
    act_goto_last_page: TTntAction;
    act_goto_page: TTntAction;
    act_goto_back: TTntAction;
    act_goto_forward: TTntAction;
    act_goto_find: TTntAction;
    sub_goto: TSpTBXSubmenuItem;
    SpTBXItem18: TSpTBXItem;
    SpTBXItem19: TSpTBXItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXItem22: TSpTBXItem;
    SpTBXItem23: TSpTBXItem;
    SpTBXItem24: TSpTBXItem;
    SpTBXItem25: TSpTBXItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    act_zoom_fit_page: TTntAction;
    act_zoom_actual_size: TTntAction;
    act_zoom_fit_width: TTntAction;
    act_zoom_fit_content: TTntAction;
    act_zoom_custom: TTntAction;
    act_zoom_6400: TTntAction;
    act_zoom_3200: TTntAction;
    act_zoom_1600: TTntAction;
    act_zoom_800: TTntAction;
    act_zoom_400: TTntAction;
    act_zoom_200: TTntAction;
    act_zoom_150: TTntAction;
    act_zoom_125: TTntAction;
    act_zoom_100: TTntAction;
    act_zoom_50: TTntAction;
    act_zoom_25: TTntAction;
    act_zoom_12_5: TTntAction;
    act_zoom_8_33: TTntAction;
    sub_zoom: TSpTBXSubmenuItem;
    SpTBXItem16: TSpTBXItem;
    SpTBXItem17: TSpTBXItem;
    SpTBXItem26: TSpTBXItem;
    SpTBXItem27: TSpTBXItem;
    SpTBXItem28: TSpTBXItem;
    SpTBXItem29: TSpTBXItem;
    SpTBXItem30: TSpTBXItem;
    SpTBXItem31: TSpTBXItem;
    SpTBXItem32: TSpTBXItem;
    SpTBXItem33: TSpTBXItem;
    SpTBXItem34: TSpTBXItem;
    SpTBXItem35: TSpTBXItem;
    SpTBXItem37: TSpTBXItem;
    SpTBXItem38: TSpTBXItem;
    SpTBXItem39: TSpTBXItem;
    SpTBXItem40: TSpTBXItem;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    SpTBXItem41: TSpTBXItem;
    act_file_save_as: TTntAction;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    label_status: TTntLabel;
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
  private
    { Private declarations }
  protected
    fActiveFileName: WideString;
    fActiveFilePath: WideString;
    fBookmarksWindow: HWND;
    fExePath: WideString;
    fLastError: WideString;
    fOnActiveFileChange: TNotifyEvent;
    fOnCloseClick: TNotifyEvent;
    fOnError: TNotifyEvent;
    fPluginMode: Boolean;
    fProcessInfo: TProcessInformation;
    fRestrict: Boolean;
    fShowBookmarks: Boolean;
    fSumatraWindow: HWND;
    fTimeout: Integer;
    fTimer: TTimer;
    procedure DoError(const AErrorMsg: WideString); virtual;
    function GetExePath: WideString; virtual;
    function GetShowBookmarks: Boolean; virtual;
    function InternalOpen(const AFilePath: WideString; APluginMode: Boolean = true;
        ARestrictedMode: Boolean = false): Boolean; virtual;
    procedure OnTimer(Sender: TObject); virtual;
    procedure Resize; override;
    procedure SetShowBookmarks(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Close; virtual;
    function OpenFile(const AFilePath: WideString): Boolean; virtual;
    property ActiveFileName: WideString read fActiveFileName;
    property ActiveFilePath: WideString read fActiveFilePath;
    property LastError: WideString read fLastError;
    property PluginMode: Boolean read fPluginMode write fPluginMode;
    property Restrict: Boolean read fRestrict write fRestrict;
    property SumatraWindow: HWND read fSumatraWindow;
    property Timeout: Integer read fTimeout write fTimeout;
    { Public declarations }
  published
    property ExePath: WideString read fExePath write fExePath;
    property ShowBookmarks: Boolean read GetShowBookmarks write SetShowBookmarks;
    property OnActiveFileChange: TNotifyEvent read fOnActiveFileChange write
        fOnActiveFileChange;
    property OnCloseClick: TNotifyEvent read fOnCloseClick write fOnCloseClick;
    property OnError: TNotifyEvent read fOnError write fOnError;
  end;

const
  IDM_OPEN                         = 400;
  IDM_CLOSE                        = 401;
  IDM_SAVEAS                       = 402;
  IDM_PRINT                        = 403;
  IDM_EXIT                         = 405;
  IDM_REFRESH                      = 406;
  IDM_SAVEAS_BOOKMARK              = 407;
  IDM_SEND_BY_EMAIL                = 408;
  IDM_PROPERTIES                   = 409;
  IDM_VIEW_SINGLE_PAGE             = 410;
  IDM_VIEW_FACING                  = 411;
  IDM_VIEW_BOOK                    = 412;
  IDM_VIEW_CONTINUOUS              = 413;
  IDM_VIEW_ROTATE_LEFT             = 415;
  IDM_VIEW_ROTATE_RIGHT            = 416;
  IDM_VIEW_BOOKMARKS               = 417;
  IDM_VIEW_PRESENTATION_MODE       = 418;
  IDM_VIEW_SHOW_HIDE_TOOLBAR       = 419;
  IDM_COPY_SELECTION               = 420;
  IDM_VIEW_FULLSCREEN              = 421;
  IDM_SELECT_ALL                   = 422;
  IDM_COPY_IMAGE                   = 427;
  IDM_COPY_LINK_TARGET             = 428;
  IDM_COPY_COMMENT                 = 429;
  IDM_GOTO_NEXT_PAGE               = 430;
  IDM_GOTO_PREV_PAGE               = 431;
  IDM_GOTO_FIRST_PAGE              = 432;
  IDM_GOTO_LAST_PAGE               = 433;
  IDM_GOTO_PAGE                    = 434;
  IDM_SETTINGS                     = 552;


//#define IDM_OPEN                        400
//#define IDM_CLOSE                       401
//#define IDM_SAVEAS                      402
//#define IDM_PRINT                       403
//#define IDM_EXIT                        405
//#define IDM_REFRESH                     406
//#define IDM_SAVEAS_BOOKMARK             407
//#define IDM_SEND_BY_EMAIL               408
//#define IDM_PROPERTIES                  409
//#define IDM_VIEW_SINGLE_PAGE            410
//#define IDM_VIEW_FACING                 411
//#define IDM_VIEW_BOOK                   412
//#define IDM_VIEW_CONTINUOUS             413
//#define IDM_VIEW_ROTATE_LEFT            415
//#define IDM_VIEW_ROTATE_RIGHT           416
//#define IDM_VIEW_BOOKMARKS              417
//#define IDM_VIEW_PRESENTATION_MODE      418
//#define IDM_VIEW_SHOW_HIDE_TOOLBAR      419
//#define IDM_COPY_SELECTION              420
//#define IDM_VIEW_FULLSCREEN             421
//#define IDM_SELECT_ALL                  422
//#define IDM_COPY_IMAGE                  427
//#define IDM_COPY_LINK_TARGET            428
//#define IDM_COPY_COMMENT                429
//#define IDM_GOTO_NEXT_PAGE              430
//#define IDM_GOTO_PREV_PAGE              431
//#define IDM_GOTO_FIRST_PAGE             432
//#define IDM_GOTO_LAST_PAGE              433
//#define IDM_GOTO_PAGE                   434
//#define IDM_FIND_FIRST                  435
//#define IDM_FIND_NEXT                   436
//#define IDM_FIND_PREV                   437
//#define IDM_FIND_MATCH                  438
//#define IDM_ZOOM_FIT_PAGE               440
//#define IDM_ZOOM_ACTUAL_SIZE            441
//#define IDM_ZOOM_FIT_WIDTH              442
//#define IDM_ZOOM_6400                   443
//#define IDM_ZOOM_3200                   444
//#define IDM_ZOOM_1600                   445
//#define IDM_ZOOM_800                    446
//#define IDM_ZOOM_400                    447
//#define IDM_ZOOM_200                    448
//#define IDM_ZOOM_150                    449
//#define IDM_ZOOM_125                    450
//#define IDM_ZOOM_100                    451
//#define IDM_ZOOM_50                     452
//#define IDM_ZOOM_25                     453
//#define IDM_ZOOM_12_5                   454
//#define IDM_ZOOM_8_33                   455
//#define IDM_ZOOM_FIT_CONTENT            456
//#define IDM_ZOOM_CUSTOM                 457
//#define IDM_CONTRIBUTE_TRANSLATION      460
//#define IDM_VIEW_WITH_ACROBAT           470
//#define IDM_VIEW_WITH_FOXIT             471
//#define IDM_VIEW_WITH_PDF_XCHANGE       472
//#define IDM_VIEW_WITH_XPS_VIEWER        473
//#define IDM_VIEW_WITH_HTML_HELP         474
//#define IDM_OPEN_SELECTED_DOCUMENT      480
//#define IDM_PIN_SELECTED_DOCUMENT       481
//#define IDM_FORGET_SELECTED_DOCUMENT    482
//#define IDM_FILE_HISTORY_FIRST          510
//#define IDM_FILE_HISTORY_LAST           519
//#define IDM_VISIT_WEBSITE               550
//#define IDM_ABOUT                       551
//#define IDM_SETTINGS                    552
//#define IDM_CHANGE_LANGUAGE             553
//#define IDM_CHECK_UPDATE                554
//#define IDM_MANUAL                      555
//#define IDM_MOVE_FRAME_FOCUS            557
//#define IDM_GOTO_NAV_BACK               558
//#define IDM_GOTO_NAV_FORWARD            559
//#define IDM_FAV_ADD                     560
//#define IDM_FAV_DEL                     561
//#define IDM_FAV_TOGGLE                  562
//#define IDM_RENAME_FILE                 580
//#define IDM_FIND_NEXT_SEL               581
//#define IDM_FIND_PREV_SEL               582
//#define IDM_DEBUG_SHOW_LINKS            590
//#define IDM_DEBUG_GDI_RENDERER          591
//#define IDM_DEBUG_CRASH_ME              592
//#define IDM_LOAD_MOBI_SAMPLE            593
//#define IDM_DEBUG_EBOOK_UI              594
//#define IDM_FAV_FIRST                   600
//#define IDM_FAV_LAST                    800

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Show Properties Dlg
-------------------------------------------------------------------------------}
function Enum_ShowPropertiesDlg(Wnd: HWND; AProcessID: Integer): Boolean;
    stdcall;
var
  pPid: DWORD;
  classN: array [0..254] of Char;
begin
  Result:= true;
  GetWindowThreadProcessId(Wnd, pPid);
  if pPid = AProcessID then
  begin
    // get class name
    FillChar(classN, 255, #0);
    GetClassName(Wnd, classN, 255);
    if classN = 'SUMATRA_PDF_PROPERTIES' then
    begin
      ShowWindow(Wnd, SW_SHOW);
      // set on top
      SetWindowPos(Wnd, HWND_TOPMOST, 0,0,0,0,SWP_NOMOVE or SWP_NOSIZE);
      Result:= false; // stop enumeration
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Find Bookmarks Window
-------------------------------------------------------------------------------}
function Enum_FindBookmarksWnd(Wnd: HWND; AOutput: PCardinal): Boolean;
    stdcall;
var
  s: array [0..254] of Char;
begin
  Result:= true;
  // get class name
  FillChar(s, 255, #0);
  GetClassName(Wnd, s, 255);
  if s = 'SysTreeView32' then
  begin
    // get title
    FillChar(s, 255, #0);
    GetWindowText(Wnd, s, 255);
    if s = 'TOC' then
    begin
      AOutput^:= Wnd;
      Result:= false; // stop enumeration
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Find Sumatra Window
-------------------------------------------------------------------------------}
function Enum_FindSumatraWnd(Wnd: HWND; ACaller: TSumatraPDF): Boolean;
    stdcall;
var
  pPid: DWORD;
  s: array [0..254] of Char;
begin
  Result:= true;

  GetWindowThreadProcessId(Wnd, pPid);
  if pPid = ACaller.fProcessInfo.dwProcessId then
  begin
    // get class name
    FillChar(s, 255, #0);
    GetClassName(Wnd, s, 255);
    if s = 'SUMATRA_PDF_FRAME' then
    begin
      ACaller.fSumatraWindow:= Wnd;
      Result:= false; // stop enumeration
    end;
  end;
end;

{##############################################################################}
// TSumatraPDF

{-------------------------------------------------------------------------------
  Create an instance of TSumatraPDF
-------------------------------------------------------------------------------}
constructor TSumatraPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // init values
  FillChar(fProcessInfo, SizeOf(TProcessInformation), #0);
  fSumatraWindow:= 0;
  fBookmarksWindow:= 0;

  fTimeout:= 5000; // 5s
  fExePath:= '';
  fShowBookmarks:= true;
  fActiveFilePath:= '';
  fPluginMode:= false;
  fRestrict:= false;
  
  // create timer
  fTimer:= TTimer.Create(nil);
  fTimer.Enabled:= false;
  fTimer.Interval:= 1000;
  fTimer.OnTimer:= OnTimer;  
end;

{-------------------------------------------------------------------------------
  Destroy TSumatraPDF
-------------------------------------------------------------------------------}
destructor TSumatraPDF.Destroy;
begin
  Close;
  fTimer.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TSumatraPDF.Close;
var
  exitCode: Cardinal;
begin
  fTimer.Enabled:= false;

  // get bookmarks status before closing
  if fBookmarksWindow <> 0 then
  fShowBookmarks:= IsWindowVisible(fBookmarksWindow);

  // terminate process
  if fProcessInfo.hProcess <> 0 then
  begin
    // send close message
    SendMessage(fSumatraWindow, WM_CLOSE, 0, 0);
    
    // wait 5s, then force close
    if WaitForSingleObject(fProcessInfo.hProcess, 5000) = WAIT_TIMEOUT then
    begin
      GetExitCodeProcess(fProcessInfo.hProcess, exitCode);
      TerminateProcess(fProcessInfo.hProcess, exitCode);
    end;
  end;
  
  // clear values
  label_status.Caption:= 'Closed';
  FillChar(fProcessInfo, SizeOf(TProcessInformation), #0);
  fSumatraWindow:= 0;
  fBookmarksWindow:= 0;
  fActiveFilePath:= '';
end;

{-------------------------------------------------------------------------------
  DoError
-------------------------------------------------------------------------------}
procedure TSumatraPDF.DoError(const AErrorMsg: WideString);
begin
  fLastError:= AErrorMsg;
  label_status.Caption:= fLastError;
  if assigned(fOnError) then
  fOnError(Self);
end;

{-------------------------------------------------------------------------------
  Internal Open
-------------------------------------------------------------------------------}
function TSumatraPDF.InternalOpen(const AFilePath: WideString; APluginMode:
    Boolean = true; ARestrictedMode: Boolean = false): Boolean;
var
  StartInfo  : TStartupInfoW;
  dir, params, cmd, exe: WideString;
  exitCode: Cardinal;
  t: Integer;
  reg: TTntRegistry;
  h: HWND;
  style: Integer;
begin
  Result:= false;

  // get exe path
  exe:= GetExePath;

  // exit if exe is not found
  if not WideFileExists(exe) then
  begin
    DoError('SumatraPDF not found, please install it first!');
    Exit;
  end;

  // init values
  dir:= WideExtractFileDir(AFilePath);
  FillChar(StartInfo,SizeOf(TStartupInfo),#0);
  FillChar(fProcessInfo, SizeOf(TProcessInformation), #0);
  StartInfo.cb:= SizeOf(TStartupInfo);
  StartInfo.dwFlags:= STARTF_USESHOWWINDOW;
  StartInfo.wShowWindow:= SW_HIDE;

  // parameters
    // -restrict
  if ARestrictedMode then
  params:= '-restrict '
  else
  params:= '';
    // -plugin
  if APluginMode then
  params:= params + '-plugin ' + IntToStr(Self.Handle) + ' ';
    // -page
  params:= params + '-page 1 ';
    // -zoom and view
  params:= params + '-zoom "fit width" -view "continuous single page"';
    // file
  params:= params + '"' + AFilePath + '"';

  // commandline
  cmd:= '"' + exe + '" ' + params;

  // create process
  if CreateProcessW(PWideChar(exe),
                    PWideChar(cmd),
                    nil,
                    nil,
                    false,
                    NORMAL_PRIORITY_CLASS,
                    nil,
                    PWideChar(dir),
                    StartInfo,
                    fProcessInfo) then
  begin
    // wait for the process to start
    t:= GetTickCount;
    repeat
      if fProcessInfo.hProcess = 0 then
      Exit;
      
      // exit if process is terminated
      if WaitForSingleObject(fProcessInfo.hProcess, 100) <> WAIT_TIMEOUT then
      Exit;

      // find window
      if APluginMode then
      fSumatraWindow:= FindWindowEx(Self.Handle, 0, 'SUMATRA_PDF_FRAME', nil)
      else
      EnumWindows(@Enum_FindSumatraWnd, LongInt(Self));

      // time out if window is not found
      if (fSumatraWindow = 0) and ((GetTickCount - t) > fTimeout) then
      begin
        // terminate process
        Close;
        DoError('Error: SumatraPDF failed to start!');
        Exit;
      end;
    until (fSumatraWindow <> 0);

    // find bookmarks window
    fBookmarksWindow:= 0;
    EnumChildWindows(fSumatraWindow, @Enum_FindBookmarksWnd, Longint(@fBookmarksWindow));

    // Setup window
    if not APluginMode then
    begin
      // set window style
      style:= GetWindowLong(fSumatraWindow, GWL_STYLE);
      style:= style and not WS_POPUP and not WS_BORDER and not WS_CAPTION and not WS_THICKFRAME;
      style:= style or WS_CHILD;
      SetWindowLong(fSumatraWindow, GWL_STYLE, style);
      // set parent
      Windows.SetParent(fSumatraWindow, Self.Handle);
      // set position/visibility
      Windows.MoveWindow(fSumatraWindow, ClientRect.Left, ClientRect.Top+toolbar_menu.Height,
                 ClientRect.Left + ClientWidth, ClientRect.Top + ClientHeight-toolbar_menu.Height, true);
      Windows.ShowWindow(fSumatraWindow, SW_SHOW);
      
      UpdateWindow(fSumatraWindow);
    end;

    // set position
    SetWindowPos(fSumatraWindow, HWND_TOP,
                 ClientRect.Left, ClientRect.Top+toolbar_menu.Height,
                 ClientRect.Left + ClientWidth, ClientRect.Top + ClientHeight-toolbar_menu.Height,
                 SWP_FRAMECHANGED or SWP_SHOWWINDOW or SWP_NOACTIVATE);

    // set bookmark visibility
    SetShowBookmarks(fShowBookmarks);

    // enable timer
    fTimer.Enabled:= true;

    // success
    Result:= true;
  end
  else
  begin
    MessageBox(0, PChar(SysErrorMessage(GetLastError)), 'Error', MB_ICONERROR or MB_OK);
  end;
end;

{-------------------------------------------------------------------------------
  OnTimer
-------------------------------------------------------------------------------}
procedure TSumatraPDF.OnTimer(Sender: TObject);
begin
  // check if sumatra still exists
  if not IsWindow(fSumatraWindow) then
  begin
    Close;
    DoError('Error: SumatraPDF.exe terminated unexpectedly!');
    Exit;
  end;
end;

{-------------------------------------------------------------------------------
  Open File
-------------------------------------------------------------------------------}
function TSumatraPDF.OpenFile(const AFilePath: WideString): Boolean;
var
  h: HWND;
begin
  Result:= false;
  // close previous instance
  Close;
  h:= GetActiveWindow;
  try
    label_status.Caption:= 'Opening...';
    Application.ProcessMessages;
    if InternalOpen(AFilePath, fPluginMode, fRestrict) then
    begin
      Result:= true;
      fActiveFilePath:= AFilePath;
      fActiveFileName:= WideExtractFileName(AFilePath);
      if assigned(fOnActiveFileChange) then
      fOnActiveFileChange(Self);
      label_status.Caption:= '';
    end;
  finally
    SetActiveWindow(h);
  end;
end;

{-------------------------------------------------------------------------------
  Resize
-------------------------------------------------------------------------------}
procedure TSumatraPDF.Resize;
begin
  inherited;
  if fSumatraWindow <> 0 then
  begin
    SetWindowPos(fSumatraWindow, HWND_TOP,
                 ClientRect.Left, ClientRect.Top+toolbar_menu.Height,
                 ClientRect.Left + ClientWidth, ClientRect.Top + ClientHeight-toolbar_menu.Height,
                 SWP_SHOWWINDOW or SWP_NOACTIVATE);
  end;
end;

{-------------------------------------------------------------------------------
  Get/Set ShowBookmarks
-------------------------------------------------------------------------------}
function TSumatraPDF.GetShowBookmarks: Boolean;
begin
  if fBookmarksWindow <> 0 then
  fShowBookmarks:= IsWindowVisible(fBookmarksWindow);

  Result:= fShowBookmarks;
end;
procedure TSumatraPDF.SetShowBookmarks(const Value: Boolean);
begin
  fShowBookmarks:= Value;
  if fBookmarksWindow <> 0 then
  begin
    if IsWindowVisible(fBookmarksWindow) <> fShowBookmarks then
    SendMessage(fSumatraWindow, WM_COMMAND, IDM_VIEW_BOOKMARKS, 0);
  end;
end;

{-------------------------------------------------------------------------------
  On Action Execute
-------------------------------------------------------------------------------}
procedure TSumatraPDF.ActionExecute(Sender: TObject);
var
  dlg: TTntOpenDialog;
begin
  if (fSumatraWindow = 0) and (TTntAction(Sender).Tag <> IDM_OPEN) and
     (TTntAction(Sender).Tag <> IDM_CLOSE) then
  exit;

  case TTntAction(Sender).Tag of
    // open
    IDM_OPEN: begin
      dlg:= TTntOpenDialog.Create(nil);
      try
        if fActiveFilePath <> '' then
        dlg.FileName:= fActiveFilePath;
        if dlg.Execute then
        OpenFile(dlg.FileName);
      finally
        dlg.Free;
      end;
    end;
    // close
    IDM_CLOSE: begin
      if assigned(fOnCloseClick) then
      fOnCloseClick(Self);
    end;
    // properties
    IDM_PROPERTIES: begin
      SendMessage(fSumatraWindow, WM_COMMAND, IDM_PROPERTIES, 0);
      EnumWindows(@Enum_ShowPropertiesDlg, fProcessInfo.dwProcessId);
    end;
    // bookmarks
    IDM_VIEW_BOOKMARKS: ShowBookmarks:= not TTntAction(Sender).Checked;
    else
      SendMessage(fSumatraWindow, WM_COMMAND, TTntAction(Sender).Tag, 0);
  end;
end;

{-------------------------------------------------------------------------------
  On Action Update
-------------------------------------------------------------------------------}
procedure TSumatraPDF.ActionUpdate(Sender: TObject);
begin
  TTntAction(Sender).Enabled:= fSumatraWindow <> 0;
  
  if fSumatraWindow = 0 then
  exit;

  case TTntAction(Sender).Tag of
    IDM_VIEW_BOOKMARKS: TTntAction(Sender).Checked:= ShowBookmarks;
  end;
end;

{-------------------------------------------------------------------------------
  Get Exe Path
-------------------------------------------------------------------------------}
function TSumatraPDF.GetExePath: WideString;
var
  reg: TTntRegistry;
begin
  Result:= '';
  // use user provided path
  if WideFileExists(fExePath) then
  Result:= fExePath
  // try to find SumatraPDF.exe
  else
  begin
    // look from settings folder first
    if WideFileExists(SettingsDirPath + 'Plugins\SumatraPDF.exe') then
    Result:= SettingsDirPath + 'Plugins\SumatraPDF.exe'
    // then install folder
    else if WideFileExists(AppDirPath + 'Plugins\SumatraPDF.exe') then
    Result:= AppDirPath + 'Plugins\SumatraPDF.exe'
    else if WideFileExists(AppDirPath + 'Sumatra.PDF.exe') then
    Result:= AppDirPath + 'Sumatra.PDF.exe'
    // then see if sumatra is installed in the system
    else
    begin
      reg:= TTntRegistry.Create;
      try
        // installed for current user
        reg.RootKey:= HKEY_CURRENT_USER;
        if reg.OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\App Paths\SumatraPDF.exe') then
        Result:= reg.ReadString('');
        // installed for all users
        if Result = '' then
        begin
          reg.RootKey:= HKEY_LOCAL_MACHINE;
          if reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\SumatraPDF.exe') then
          Result:= reg.ReadString('');
        end;
      finally
        reg.Free;
      end;
    end;
  end;
end;

end.

