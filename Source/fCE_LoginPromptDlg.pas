unit fCE_LoginPromptDlg;

interface

uses
  // Tnt
  TntStdCtrls, TntForms,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TCELoginPromptDlg = class(TTntForm)
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    edit_username: TTntEdit;
    edit_password: TTntEdit;
    check_save: TTntCheckBox;
    label_warning: TTntLabel;
    but_ok: TTntButton;
    but_cancel: TTntButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  CE_LanguageEngine;

{$R *.dfm}

{-------------------------------------------------------------------------------
  On Form Create
-------------------------------------------------------------------------------}
procedure TCELoginPromptDlg.FormCreate(Sender: TObject);
begin
  // Translate
  CEGlobalTranslator.TranslateComponent(Self);
end;

{-------------------------------------------------------------------------------
  On Form KeyPress
-------------------------------------------------------------------------------}
procedure TCELoginPromptDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) then
  but_cancel.Click;
end;

end.
