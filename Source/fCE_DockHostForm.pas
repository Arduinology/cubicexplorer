unit fCE_DockHostForm;

interface

uses
  // CE Units
  CE_Layout,
  // JVCL
  JvDockControlForm,
  // Toolbar2000
  TB2Dock,
  // SpTBXLib
  SpTBXItem,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TControlAccess = class(TControl);
  TCEDockHostForm = class(TForm)
    CenterPanel: TPanel;
    PageHostPanel: TPanel;
    TopPageToolDock: TSpTBXDock;
    BottomPageToolDock: TSpTBXDock;
    LeftPageToolDock: TSpTBXDock;
    RightPageToolDock: TSpTBXDock;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    DockServer: TJvDockServer;
  end;

implementation

{$R *.dfm}

{*------------------------------------------------------------------------------
  Get's called when TCEDockHostForm is created.
-------------------------------------------------------------------------------}
procedure TCEDockHostForm.FormCreate(Sender: TObject);
begin
  DockServer:= TJvDockServer.Create(Self);
  DockServer.DockStyle:= CEDockStyle;
  DockServer.TopDockPanel.Parent:= CenterPanel;
  DockServer.TopSplitter.Parent:= CenterPanel;
  DockServer.BottomDockPanel.Parent:= CenterPanel;
  DockServer.BottomSplitter.Parent:= CenterPanel;

  DockServer.LeftSplitter.Width:= 5;
  DockServer.LeftSplitter.Cursor:= crSizeWE;
  DockServer.RightSplitter.Width:= 5;
  DockServer.RightSplitter.Cursor:= crSizeWE;
  DockServer.TopSplitter.Height:= 5;
  DockServer.TopSplitter.Cursor:= crSizeNS;
  DockServer.BottomSplitter.Height:= 5;
  DockServer.BottomSplitter.Cursor:= crSizeNS;

  TControlAccess(DockServer.LeftDockPanel).ParentColor:= true;
  TControlAccess(DockServer.TopDockPanel).ParentColor:= true;
  TControlAccess(DockServer.RightDockPanel).ParentColor:= true;
  TControlAccess(DockServer.BottomDockPanel).ParentColor:= true;
end;

{*------------------------------------------------------------------------------
  Get's called when TCEDockHostForm is destroyed.
-------------------------------------------------------------------------------}
procedure TCEDockHostForm.FormDestroy(Sender: TObject);
begin
  DockServer.Free;
end;

end.
