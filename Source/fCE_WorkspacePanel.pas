unit fCE_WorkspacePanel;

interface

uses
  // CE Units
  fCE_DockableForm, CE_FileView,
  // SpTBX
  TB2Dock, SpTBXItem,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms;

const
  WM_ActivateWorkspace = WM_USER + 155;

type
  TCEWorkspacePanel = class(TCECustomDockableForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure WMActivateWorkspace(var Message: TMessage); message
        WM_ActivateWorkspace;
  public
    FileView: TCEFileView;
    procedure DoFormHide; override;
    procedure DoFormShow; override;
  end;

var
  CEWorkspacePanel: TCEWorkspacePanel;

implementation

uses
  CE_GlobalCtrl, MPShellUtilities, MPCommonUtilities, EasyListview,
  CE_VistaFuncs, CE_Utils;

{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called when form gets hidden.
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoFormHide;
begin
  inherited;
  FileView.Active:= false;
end;

{*------------------------------------------------------------------------------
  Get's called when form gets shown.
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.DoFormShow;
begin
  inherited;
  Application.ProcessMessages;
  PostMessage(Handle, WM_ActivateWorkspace, 1,0);
end;

{-------------------------------------------------------------------------------
  On TCEWorkspacePanel Create
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.FormCreate(Sender: TObject);
begin
  FileView:= TCEFileView.Create(Self);
  FileView.Parent:= self;
  FileView.Align:= alClient;
  FileView.Themed:= false;
  FileView.BorderStyle:= bsNone;
  FileView.BoundsRect:= Rect(0,0, self.ClientWidth, self.ClientHeight);
  FileView.FileObjects:= [foFolders,
                          foNonFolders];
  FileView.DragManager.MouseButton:= [cmbLeft,cmbRight];
  FileView.Selection.MouseButton:= [cmbLeft,cmbRight];
  FileView.ParentShowHint:= true;
  FileView.HintType:= ehtText;
  FileView.PaintInfoGroup.BandThickness:= 2;
  FileView.PaintInfoGroup.BandColor:= clWindowText;
  FileView.PaintInfoGroup.BandColorFade:= clWindow;
  FileView.CompressedFile.Hilight:= false;
  FileView.EncryptedFile.Hilight:= false;
  FileView.GroupFont.Style:= [fsBold];
  FileView.TranslateHeader:= false;
  FileView.TabOrder:= 1;



  GlobalFocusCtrl.CtrlList.Add(FileView);
  FileView.OnMouseWheel:= GlobalFocusCtrl.DoMouseWheel;
  SetDesktopIconFonts(FileView.Font);

end;

{-------------------------------------------------------------------------------
  On TCEWorkspacePanel Destroy
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.FormDestroy(Sender: TObject);
begin
  //
end;

{-------------------------------------------------------------------------------
  Activate Workspace
-------------------------------------------------------------------------------}
procedure TCEWorkspacePanel.WMActivateWorkspace(var Message: TMessage);
begin
  inherited;
  FileView.Active:= Message.WParam = 1;
end;

end.
