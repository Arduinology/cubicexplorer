unit fCE_AboutBox;

interface

uses
  // CE Units
  CE_VistaFuncs, CE_LanguageEngine,
  // PNG Controls
  pngimage,
  // Syn Edit
  SynEditHighlighter, SynHighlighterURI, SynURIOpener, SynEdit, SynMemo,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, TntStdCtrls;

type
  TCEAboutBox = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SynMemo1: TSynMemo;
    SynURIOpener1: TSynURIOpener;
    SynURISyn1: TSynURISyn;
    but_close: TTntButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure but_closeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public

  end;

procedure ShowAboutBox;

implementation

uses
  JclFileUtils, Main;
  
{$R *.dfm}

{*------------------------------------------------------------------------------
  Show about dialog
-------------------------------------------------------------------------------}
procedure ShowAboutBox;
var
  box: TCEAboutBox;
begin
  box:= TCEAboutBox.Create(MainForm);
  box.Show;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  close button execute
-------------------------------------------------------------------------------}
procedure TCEAboutBox.but_closeClick(Sender: TObject);
begin
  PostMessage(self.Handle, WM_CLOSE,0,0);
end;

{*------------------------------------------------------------------------------
  Get's called on Form close
-------------------------------------------------------------------------------}
procedure TCEAboutBox.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

{*------------------------------------------------------------------------------
  Get's called when TCEAboutBox is created.
-------------------------------------------------------------------------------}
procedure TCEAboutBox.FormCreate(Sender: TObject);
var
  ver: TJclFileVersionInfo;
begin
  SetVistaFont(Font);
  ver:= TJclFileVersionInfo.Create(Application.ExeName);
  Label2.Caption:= _('Version:') + ' ' + ver.FileVersion;
  Label1.Caption:= ver.ProductName + ' ' + ver.ProductVersion;
  ver.Free;
  CEGlobalTranslator.TranslateComponent(Self);
end;

{*------------------------------------------------------------------------------
  Get's called when TCEAboutBox is shown.
-------------------------------------------------------------------------------}
procedure TCEAboutBox.FormShow(Sender: TObject);
begin
  SynMemo1.SetFocus;
end;

end.
