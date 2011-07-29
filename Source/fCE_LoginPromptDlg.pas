unit fCE_LoginPromptDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls;

type
  TCELoginPromptDlg = class(TForm)
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    edit_username: TTntEdit;
    edit_password: TTntEdit;
    check_save: TTntCheckBox;
    label_warning: TTntLabel;
    but_ok: TTntButton;
    but_cancel: TTntButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  CE_LanguageEngine;

{$R *.dfm}

procedure TCELoginPromptDlg.FormCreate(Sender: TObject);
begin
  // Translate
  CEGlobalTranslator.TranslateComponent(Self); 
end;

end.
