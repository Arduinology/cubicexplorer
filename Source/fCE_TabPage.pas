unit fCE_TabPage;

interface

uses
  // CE Units
  CE_GlobalCtrl, CE_VistaFuncs, CE_SettingsIntf,
  // SpTBX
  SpTBXTabs,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShlObj, Contnrs;

type
  TCECustomTabPageClass = class of TCECustomTabPage;
  TCECustomTabPage = class(TFrame, ICEPathChangeHandler, ICESettingsHandler)
  private
    fActive: Boolean;
    fImageIndex: Integer;
    fImages: TImageList;
    fLayout: String;
    fTabCaption: WideString;
    procedure SetTabCaption(const Value: WideString);
  protected
    fTabItem: TSpTBXTabItem;
    procedure GlobalActivePageChange(OldPage, NewPage: TComponent); virtual;
        stdcall;
    procedure GlobalContentChange(Sender: TObject); virtual; stdcall;
    procedure GlobalFocusChanged(Sender: TObject; NewPath: WideString); virtual;
        stdcall;
    procedure GlobalPathChanged(Sender: TObject; NewPath: WideString); virtual;
        stdcall;
    procedure GlobalPIDLChanged(Sender: TObject; NewPIDL: PItemIDList); virtual;
        stdcall;
    procedure SetActive(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HidePage; virtual;
    procedure LoadFromStorage(Storage: ICESettingsStorage); virtual; stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); virtual; stdcall;
    procedure SelectPage; virtual;
    function TabClosing: Boolean; virtual;
    procedure UpdateCaption; virtual;
    property Active: Boolean read fActive write SetActive;
    property ImageIndex: Integer read fImageIndex write fImageIndex;
    property Images: TImageList read fImages write fImages;
    property Layout: String read fLayout write fLayout;
    property TabCaption: WideString read fTabCaption write SetTabCaption;
    property TabItem: TSpTBXTabItem read fTabItem;
  end;

  TCEClassList = class(TObject)
  protected
    ClassList: TClassList;
    NameList: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetClass(AName: String): TCECustomTabPageClass;
    function GetName(AClass: TCECustomTabPageClass): string;
    procedure RegisterClass(AName: String; AClass: TCECustomTabPageClass);
    procedure UnRegisterClass(AClass: TCECustomTabPageClass);
  end;

function TabPageClassList: TCEClassList;

var
  fTabPageClassList: TCEClassList;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Get TabPageClassList
-------------------------------------------------------------------------------}
function TabPageClassList: TCEClassList;
begin
  if fTabPageClassList = nil then
  fTabPageClassList:= TCEClassList.Create;
  Result:= fTabPageClassList;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Get's called when TCECustomTabPage is created.
-------------------------------------------------------------------------------}
constructor TCECustomTabPage.Create(AOwner: TComponent);
begin
  inherited;
  SetVistaFont(Font);
  fImageIndex:= -1;
  fActive:= false;
  Layout:= 'CustomPage';
end;

{*------------------------------------------------------------------------------
  Get's called when TCECustomTabPage is destoyed.
-------------------------------------------------------------------------------}
destructor TCECustomTabPage.Destroy;
begin
  inherited;
end;

{*------------------------------------------------------------------------------
  Get's called when Active page has changed
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalActivePageChange(OldPage, NewPage: TComponent);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when global content has changed
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalContentChange(Sender: TObject);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Global focus has changed
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalFocusChanged(Sender: TObject; NewPath:
    WideString);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (String)
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalPathChanged(Sender: TObject; NewPath:
    WideString);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Get's called when Global path has changed (PIDL)
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.GlobalPIDLChanged(Sender: TObject; NewPIDL:
    PItemIDList);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Hide page
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.HidePage;
begin
  if Visible then
  Visible:= false;
end;

{-------------------------------------------------------------------------------
  Load From Storage
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.LoadFromStorage(Storage: ICESettingsStorage);
begin
  // Override from descendant
end;

{-------------------------------------------------------------------------------
  Save to Storage
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.SaveToStorage(Storage: ICESettingsStorage);
begin
  // Override from descendant
end;

{*------------------------------------------------------------------------------
  Select page
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.SelectPage;
begin
  // Override from descendant.
end;

{*------------------------------------------------------------------------------
  Set Active Value
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.SetActive(const Value: Boolean);
begin
  fActive:= Value;
end;

{*------------------------------------------------------------------------------
  Get's called when tab is about to be closed.
    -If returns true the tab is closed.
-------------------------------------------------------------------------------}
function TCECustomTabPage.TabClosing: Boolean;
begin
  Result:= true;
end;

{*------------------------------------------------------------------------------
  Update tab caption
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.UpdateCaption;
begin
  TabCaption:= 'CustomPage';
end;

{*------------------------------------------------------------------------------
  Set Tab Caption
-------------------------------------------------------------------------------}
procedure TCECustomTabPage.SetTabCaption(const Value: WideString);
begin
  fTabCaption:= Value;
  TabItem.Caption:= Value;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEClassList
-------------------------------------------------------------------------------}
constructor TCEClassList.Create;
begin
  inherited;
  ClassList:= TClassList.Create;
  NameList:= TStringList.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCEClassList
-------------------------------------------------------------------------------}
destructor TCEClassList.Destroy;
begin
  ClassList.Free;
  NameList.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEClassList.Clear;
begin
  ClassList.Clear;
  NameList.Clear;
end;

{-------------------------------------------------------------------------------
  Get Class
-------------------------------------------------------------------------------}
function TCEClassList.GetClass(AName: String): TCECustomTabPageClass;
var
  i: Integer;
begin
  i:= NameList.IndexOf(AName);
  if i > -1 then
  Result:= TCECustomTabPageClass(ClassList.Items[i])
  else
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Get Name
-------------------------------------------------------------------------------}
function TCEClassList.GetName(AClass: TCECustomTabPageClass): string;
var
  i: Integer;
begin
  i:= ClassList.IndexOf(AClass);
  if i > -1 then
  Result:= NameList.Strings[i]
  else
  Result:= '';
end;

{-------------------------------------------------------------------------------
  RegisterClass
-------------------------------------------------------------------------------}
procedure TCEClassList.RegisterClass(AName: String; AClass:
    TCECustomTabPageClass);
begin
  NameList.Add(AName);
  ClassList.Add(AClass);
end;

{-------------------------------------------------------------------------------
  RegisterClass
-------------------------------------------------------------------------------}
procedure TCEClassList.UnRegisterClass(AClass: TCECustomTabPageClass);
var
  i: Integer;
begin
  i:= ClassList.IndexOf(AClass);
  if i > -1 then
  begin
    ClassList.Delete(i);
    NameList.Delete(i);
  end;
end;

{##############################################################################}

initialization

finalization
  FreeAndNil(fTabPageClassList);

end.
