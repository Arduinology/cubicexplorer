unit fCE_OptionsCustomPage;

interface

uses
  // CE Units
  CE_SettingsIntf,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs;

type
  TCEOptionsCustomPageClass = class of TCEOptionsCustomPage;
  TCEOptionsCustomPage = class(TFrame, ICESettingsHandler)
    procedure HandleChange(Sender: TObject);
  private
    fOptionsDialog: TForm;
  protected
    fImageIndex: Integer;
    fPageName: WideString;
    fPagePath: WideString;
    fPageTitle: WideString;
  public
    procedure ApplySettings; virtual;
    procedure LoadFromStorage(Storage: ICESettingsStorage); virtual; stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); virtual; stdcall;
    procedure SetModified;
    property ImageIndex: Integer read fImageIndex write fImageIndex;
    property OptionsDialog: TForm read fOptionsDialog write fOptionsDialog;
    property PageName: WideString read fPageName write fPageName;
    property PagePath: WideString read fPagePath write fPagePath;
    property PageTitle: WideString read fPageTitle write fPageTitle;
  end;

implementation

{$R *.dfm}

uses
  fCE_OptionsDialog;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.ApplySettings;
begin
  // Override from descendant
end;

{-------------------------------------------------------------------------------
  Handle Change
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.HandleChange(Sender: TObject);
begin
  SetModified;
end;

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.LoadFromStorage(Storage: ICESettingsStorage);
begin
  // Override from descendant
end;

{-------------------------------------------------------------------------------
  Save To Storage
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.SaveToStorage(Storage: ICESettingsStorage);
begin
  // Override from descendant
end;

{-------------------------------------------------------------------------------
  SetModified
-------------------------------------------------------------------------------}
procedure TCEOptionsCustomPage.SetModified;
begin
  if assigned(fOptionsDialog) then
  TCEOptionsDialog(fOptionsDialog).Modified:= true;
end;

end.
