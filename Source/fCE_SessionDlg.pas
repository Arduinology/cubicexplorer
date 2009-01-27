unit fCE_SessionDlg;

interface

uses
  // CE Units
  CE_LanguageEngine, CE_Sessions,
  // Tnt
  TntStdCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TCESessionEditDlg = class(TForm)
    TntLabel1: TTntLabel;
    TntGroupBox1: TTntGroupBox;
    check_bookmarks: TTntCheckBox;
    check_tabs: TTntCheckBox;
    check_layouts: TTntCheckBox;
    check_autosave: TTntCheckBox;
    TntButton1: TTntButton;
    TntButton2: TTntButton;
    combo_sessionname: TTntComboBox;
    check_onstartup: TTntCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure combo_sessionnameChange(Sender: TObject);
  private
    fNewSessionMode: Boolean;
    procedure SetNewSessionMode(const Value: Boolean);
    { Private declarations }
  public
    procedure AssignToSession(Session: TCESession);
    procedure AssignFromSession(Session: TCESession);
    property NewSessionMode: Boolean read fNewSessionMode write SetNewSessionMode;
    { Public declarations }
  end;

procedure NewSession;
procedure EditSession(SessionName: WideString);

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create new session
-------------------------------------------------------------------------------}
procedure NewSession;

  function ValidName(AName: WideString): Boolean;
  begin
    Result:= AName <> '';
  end;

var
  dlg: TCESessionEditDlg;
  i: Integer;

  procedure CreateNew;
  var
    s: TCESession;
  begin
    if dlg.ShowModal = mrOK then
    begin
      s:= GlobalSessions.FindSession(dlg.combo_sessionname.Text);
      if s <> nil then
      begin
        if MessageBox(0, 'Session with same name already exists! Do you want to override it?', 'Override existing session?', MB_ICONWARNING or MB_YESNO) = idYes then
        begin
          dlg.AssignToSession(s);
          GlobalSessions.SaveToSession(s);
          s.SaveSettings(false);
          GlobalSessions.ActiveSession:= s.SessionName;
        end
        else
        CreateNew;
      end
      else if ValidName(dlg.combo_sessionname.Text) then
      begin
        s:= GlobalSessions.CreateSession(dlg.combo_sessionname.Text);
        dlg.AssignToSession(s);
        GlobalSessions.SaveToSession(s);
        s.SaveSettings(false);
        GlobalSessions.ActiveSession:= s.SessionName;
      end
      else
      begin
        MessageBox(0, 'Session name can''t be empty.', 'Invalid session name!', MB_ICONINFORMATION or MB_OK);
        CreateNew;
      end;
    end
  end;

begin
  dlg:= TCESessionEditDlg.Create(nil);
  try
    for i:= 0 to GlobalSessions.SessionCount - 1 do
    dlg.combo_sessionname.Items.Add(GlobalSessions.Session[i].SessionName);

    dlg.NewSessionMode:= true;
    CreateNew;
  finally
    dlg.Release;
  end;
end;

{-------------------------------------------------------------------------------
  Edit session
-------------------------------------------------------------------------------}
procedure EditSession(SessionName: WideString);
var
  dlg: TCESessionEditDlg;
  s: TCESession;
  i: Integer;
begin
  s:= GlobalSessions.FindSession(SessionName);
  if assigned(s) then
  begin
    dlg:= TCESessionEditDlg.Create(nil);
    try
      for i:= 0 to GlobalSessions.SessionCount - 1 do
      dlg.combo_sessionname.Items.Add(GlobalSessions.Session[i].SessionName);
      dlg.NewSessionMode:= false;

      dlg.AssignFromSession(s);
      if dlg.ShowModal = mrOK then
      begin
        s:= GlobalSessions.FindSession(dlg.combo_sessionname.Text);
        dlg.AssignToSession(s);
        s.SaveSettings(true);
      end;
    finally
      dlg.Release;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  On Create
-------------------------------------------------------------------------------}
procedure TCESessionEditDlg.FormCreate(Sender: TObject);
begin
  CEGlobalTranslator.TranslateComponent(Self);
end;

{-------------------------------------------------------------------------------
  Assign From session
-------------------------------------------------------------------------------}
procedure TCESessionEditDlg.AssignFromSession(Session: TCESession);
var
  sit: TCESessionItemTypes;
begin
  if not fNewSessionMode then
  combo_sessionname.ItemIndex:= combo_sessionname.Items.IndexOf(Session.SessionName)
  else
  combo_sessionname.Text:= Session.SessionName;
  check_autosave.Checked:= Session.AutoSave;
  // Item types
  sit:= Session.SessionItemTypes;
  check_tabs.Checked:= sitTabs in sit;
  check_bookmarks.Checked:= sitBookmarks in sit;
  check_layouts.Checked:= sitLayouts in sit;
  // On startup
  check_onstartup.Checked:= WideCompareText(GlobalSessions.StartupSession, Session.SessionName) = 0;
end;

{-------------------------------------------------------------------------------
  Assign to session
-------------------------------------------------------------------------------}
procedure TCESessionEditDlg.AssignToSession(Session: TCESession);
var
  sit: TCESessionItemTypes;
begin
  Session.SessionName:= combo_sessionname.Text;
  Session.AutoSave:= check_autosave.Checked;
  // Item types
  sit:= [];
  if check_tabs.Checked then
  include(sit, sitTabs);
  if check_bookmarks.Checked then
  include(sit, sitBookmarks);
  if check_layouts.Checked then
  include(sit, sitLayouts);
  Session.SessionItemTypes:= sit;
  // On startup
  if check_onstartup.Checked then
  GlobalSessions.StartupSession:= Session.SessionName
  else if WideCompareText(GlobalSessions.StartupSession, Session.SessionName) = 0 then
  GlobalSessions.StartupSession:= '';
end;

{-------------------------------------------------------------------------------
  On combo_sessionname Change
-------------------------------------------------------------------------------}
procedure TCESessionEditDlg.combo_sessionnameChange(Sender: TObject);
var
  s: TCESession;
begin
  if not fNewSessionMode then
  begin
    s:= GlobalSessions.FindSession(combo_sessionname.Text);
    AssignFromSession(s);
  end;
end;

{-------------------------------------------------------------------------------
  Set NewSessionMode
-------------------------------------------------------------------------------}
procedure TCESessionEditDlg.SetNewSessionMode(const Value: Boolean);
begin
  fNewSessionMode:= Value;
  if fNewSessionMode then
  begin
    Caption:= _('New Session');
    combo_sessionname.Style:= csSimple;
  end
  else
  begin
    Caption:= _('Edit Session');
    combo_sessionname.Style:= csDropDownList;
  end;
end;

end.
