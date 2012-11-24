unit fCE_PluginLog;

interface

uses
  // CubicCore
  ccLog,
  // CE
  CE_Plugins, CE_PluginsIntf,
  // TNT
  TntStdCtrls,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TCEPluginLog = class(TForm, ICCLogListener)
    memo_log: TMemo;
    panel_bottom: TPanel;
    check_enable: TTntCheckBox;
    but_close: TTntButton;
    procedure but_closeClick(Sender: TObject);
    procedure check_enableClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  protected
    fLog: ICCLog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LogNotify(const ALog: ICCLog; const ANotify: Cardinal; const AMsg:
        ALogItem); virtual; stdcall;
  end;

procedure ShowPluginLog;

var
  CEPluginLog: TCEPluginLog;

implementation

uses
  Main;

{$R *.dfm}

{-------------------------------------------------------------------------------
  ShowPluginLog
-------------------------------------------------------------------------------}
procedure ShowPluginLog;
begin
  if not assigned(CEPluginLog) then
  begin
    CEPluginLog:= TCEPluginLog.Create(MainForm);
  end;
  CEPluginLog.Show;
  CEPluginLog.BringToFront;
end;

{##############################################################################}
// TCEPluginLog

{-------------------------------------------------------------------------------
  Create an instance of TCEPluginLog
-------------------------------------------------------------------------------}
constructor TCEPluginLog.Create(AOwner: TComponent);
var
  acc: ICEPluginHostAccess;
begin
  inherited Create(AOwner);
  fLog:= TCCLog.Create;
  fLog.RegisterListener(Self);

  acc:= GlobalPluginHost as ICEPluginHostAccess;
  acc.SetLogger(fLog);
  acc:= nil;  
end;

{-------------------------------------------------------------------------------
  Destroy TCEPluginLog
-------------------------------------------------------------------------------}
destructor TCEPluginLog.Destroy;
var
  acc: ICEPluginHostAccess;
begin
  acc:= GlobalPluginHost as ICEPluginHostAccess;
  acc.SetLogger(nil);
  acc:= nil;

  fLog:= nil;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  but_closeClick
-------------------------------------------------------------------------------}
procedure TCEPluginLog.but_closeClick(Sender: TObject);
begin
  Self.Close;
end;

{-------------------------------------------------------------------------------
  check_enableClick
-------------------------------------------------------------------------------}
procedure TCEPluginLog.check_enableClick(Sender: TObject);
var
  acc: ICEPluginHostAccess;
begin
  if check_enable.Enabled then
  begin
    acc:= GlobalPluginHost as ICEPluginHostAccess;
    if check_enable.Checked then
    acc.SetLogger(GlobalLog)
    else
    acc.SetLogger(nil);
    acc:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  FormClose
-------------------------------------------------------------------------------}
procedure TCEPluginLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
  CEPluginLog:= nil;
end;

{-------------------------------------------------------------------------------
  LogNotify
-------------------------------------------------------------------------------}
procedure TCEPluginLog.LogNotify(const ALog: ICCLog; const ANotify: Cardinal;
    const AMsg: ALogItem);
var
  intf: IInterface;
  ws: WideString;
  plug: ICEPlugin;
begin
  ws:= '';
  if assigned(AMsg.ASender) then
  begin
    intf:= IInterface(AMsg.ASender);
    if supports(intf, IID_ICEPlugin, plug) then
    ws:= GUIDToString(plug.GetPluginID) + ': ';
  end;

  if ANotify = LNOTIFY_ADD then
  begin
    if AMsg.AType = MTYPE_NORMAL then
    memo_log.Lines.Add(ws + AMsg.AMsg)
    else
    memo_log.Lines.Add('[' + MTYPE_STRS[AMsg.AType] + '] ' + ws + AMsg.AMsg)
  end
  else if ANotify = LNOTIFY_CLEAR then
  memo_log.Lines.Clear;
end;

end.
