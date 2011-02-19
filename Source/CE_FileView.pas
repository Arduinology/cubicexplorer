//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
//                                                                                            
//  The contents of this file are subject to the Mozilla Public License                       
//  Version 1.1 (the "License"); you may not use this file except in                          
//  compliance with the License. You may obtain a copy of the License at                      
//  http://www.mozilla.org/MPL/                                                               
//                                                                                            
//  Software distributed under the License is distributed on an "AS IS"
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
//  License for the specific language governing rights and limitations                        
//  under the License.                                                                        
//                                                                                            
//  The Original Code is CE_FileView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_FileView;

interface

uses
  // CE Units
  CE_Consts, CE_BaseFileView, CE_Toolbar, CE_VistaFuncs, CE_LanguageEngine,
  // Jvcl
  JvBalloonHint, JvSimpleXML,
  // VSTools
  MPCommonObjects, MPCommonUtilities, MPShellUtilities,
  EasyListview, VirtualExplorerEasyListview, VirtualResources,
  VirtualExplorerTree,  VirtualShellHistory,
  VirtualShellNewMenu, VirtualShellNotifier,
  // SpTBX, TB2K
  SpTBXItem, TB2Item,
  // Tnt Controls
  TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Forms,
  Graphics, Menus, ShellAPI, ShlObj, Math;

type

  TCEColSetting = record
    Index: Integer;
    Position: Integer;
    Width: Integer;
    Sort: TEasySortDirection;
  end;

  PCEColSettings = ^TCEColSettings;
  TCEColSettings = array of TCEColSetting;

  PCEGroupBySetting = ^TCEGroupBySetting;
  TCEGroupBySetting = record
    Index: Integer;
    Enabled: Boolean;
  end;

  TCEFileView = class(TCECustomFileView)
  private
    fAutosizeListViewStyle: Boolean;
    fAutoSelectFirstItem: Boolean;
    fOnViewStyleChange: TNotifyEvent;
    fSelectPreviousFolder: Boolean;
    fTranslateHeader: Boolean;
    fUseKernelNotification: Boolean;
    fCellWidth: Integer;
    fColumnIndex: Integer;
    fRightMouseButton_IsDown: Boolean;
    fLeftMouseButton_IsDown: Boolean;
    fLeftMouseButton_RockerClicks: Integer;
    fRightMouseButton_RockerClicks: Integer;
    fUseMouseRocker: Boolean;
    procedure SetAutosizeListViewStyle(const Value: Boolean);
    procedure SetUseKernelNotification(const Value: Boolean);
  protected
    procedure DoCustomColumnAdd; override;
    procedure DoEnumFinished; override;
    procedure DoEnumFolder(const Namespace: TNamespace; var AllowAsChild: Boolean);
        override;
    procedure DoKeyAction(var CharCode: Word; var Shift: TShiftState; var
        DoDefault: Boolean); override;
    procedure DoRootChange; override;
    procedure DoRootChanging(const NewRoot: TRootFolder; Namespace: TNamespace; var
        Allow: Boolean); override;
    procedure DoRootRebuild; override;
    procedure HandleDblClick(Button: TCommonMouseButton; Msg: TWMMouse); override;
    procedure HandleMouseDown(Button: TCommonMouseButton; Msg: TWMMouse); override;
    procedure HandleMouseUp(Button: TCommonMouseButton; Msg: TWMMouse); override;
    procedure HistoryChange(Sender: TBaseVirtualShellPersistent; ItemIndex:
        Integer; ChangeType: TVSHChangeType);
    procedure SetNotifyFolder(Namespace: TNamespace);
    procedure SetView(Value: TEasyListStyle); override;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KillFocus;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    property ColumnIndex: Integer read fColumnIndex write fColumnIndex;
  public
    fChangeHistory: Boolean;
    History: TVirtualShellHistory;
    ShellNewMenu: TVirtualShellNewMenu;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearHistory;
    procedure CreateNewFolder;
    procedure GoBackInHistory;
    procedure GoFolderUp;
    procedure GoForwardInHistory;
    procedure OnCreateNewFile(Sender: TMenu; const NewMenuItem:
        TVirtualShellNewItem; var Path, FileName: WideString; var Allow: Boolean);
    procedure PasteShortcutFromClipboard;
    procedure SetFocus; override;
    property AutosizeListViewStyle: Boolean read fAutosizeListViewStyle write
        SetAutosizeListViewStyle;
    property TranslateHeader: Boolean read fTranslateHeader write fTranslateHeader;
    property UseMouseRocker: Boolean read fUseMouseRocker write fUseMouseRocker;
    property LeftMouseButton_IsDown: Boolean read fLeftMouseButton_IsDown;
    property LeftMouseButton_RockerClicks: Integer read
        fLeftMouseButton_RockerClicks;
    property RightMouseButton_IsDown: Boolean read fRightMouseButton_IsDown;
    property RightMouseButton_RockerClicks: Integer read
        fRightMouseButton_RockerClicks;
  published
    property AutoSelectFirstItem: Boolean read fAutoSelectFirstItem write
        fAutoSelectFirstItem;
    property SelectPreviousFolder: Boolean read fSelectPreviousFolder write
        fSelectPreviousFolder;
    property UseKernelNotification: Boolean read fUseKernelNotification write
        SetUseKernelNotification;
    property OnViewStyleChange: TNotifyEvent read fOnViewStyleChange write
        fOnViewStyleChange;
  end;

  TCEColumnSettings = class(TPersistent)
  private
    function GetControlPanel: string;
    function GetDefault: string;
    function GetMyComputer: string;
    function GetNetwork: string;
    procedure SetControlPanel(const Value: string);
    procedure SetDefault(const Value: string);
    procedure SetMyComputer(const Value: string);
    procedure SetNetwork(const Value: string);
  public
    DefaultColSettings: TCEColSettings;
    MyComputerColSettings: TCEColSettings;
    NetworkColSettings: TCEColSettings;
    ControlPanelColSettings: TCEColSettings;
  published
    property ControlPanel: string read GetControlPanel write SetControlPanel;
    property Default: string read GetDefault write SetDefault;
    property MyComputer: string read GetMyComputer write SetMyComputer;
    property Network: string read GetNetwork write SetNetwork;
  end;

type
  TCEGroupBySettings = class(TPersistent)
  private
    function GetControlPanel: string;
    function GetDefault: string;
    function GetMyComputer: string;
    function GetNetwork: string;
    procedure SetControlPanel(const Value: string);
    procedure SetDefault(const Value: string);
    procedure SetMyComputer(const Value: string);
    procedure SetNetwork(const Value: string);
  public
    DefaultGroupBySettings: TCEGroupBySetting;
    MyComputerGroupBySettings: TCEGroupBySetting;
    NetworkGroupBySettings: TCEGroupBySetting;
    ControlPanelGroupBySettings: TCEGroupBySetting;
    function GroupBySettingsToString(GroupBySetting: TCEGroupBySetting): string;
    procedure StringToGroupBySettings(AString: String; var GroupBySetting:
        TCEGroupBySetting);
  published
    property ControlPanel: string read GetControlPanel write SetControlPanel;
    property Default: string read GetDefault write SetDefault;
    property MyComputer: string read GetMyComputer write SetMyComputer;
    property Network: string read GetNetwork write SetNetwork;
  end;

  TCEFilmstripSettings = class(TPersistent)
  private
    fThumbPos: TAlign;
    fThumbSize: Integer;
    fThumbStyle: TEasyListStyle;
  published
    property ThumbPos: TAlign read fThumbPos write fThumbPos;
    property ThumbSize: Integer read fThumbSize write fThumbSize;
    property ThumbStyle: TEasyListStyle read fThumbStyle write fThumbStyle;
  end;

function ColSettingsToString(ColSettings: TCEColSettings): string;

procedure StringToColSettings(AString: String; var ColSettings: TCEColSettings);

implementation

uses
  CE_GlobalCtrl, CE_Utils, dCE_Actions;

{##############################################################################}

{-------------------------------------------------------------------------------
  Column Settings to string
-------------------------------------------------------------------------------}
function ColSettingsToString(ColSettings: TCEColSettings): string;
var
  i: Integer;
  col: TCEColSetting;
begin
  Result:= '';
  for i:= 0 to Length(ColSettings) - 1 do
  begin
    col:= ColSettings[i];
    if i > 0 then
    Result:= Result + '|';
    Result:= Result + IntToStr(col.Index) + ','
             + IntToStr(col.Position) + ','
             + IntToStr(col.Width) + ','
             + IntToStr(Ord(col.Sort));
  end;
end;

{-------------------------------------------------------------------------------
  String to Column Settings
-------------------------------------------------------------------------------}
procedure StringToColSettings(AString: String; var ColSettings: TCEColSettings);
var
  i: Integer;
  colListSet, colList: TStrings;
begin
  colListSet:= TStringList.Create;
  colList:= TStringList.Create;
  try
    colListSet.Delimiter:= '|';
    colList.Delimiter:= ',';

    colListSet.DelimitedText:= AString;
    SetLength(ColSettings, colListSet.Count);

    for i:= 0 to colListSet.Count - 1 do
    begin
      colList.DelimitedText:= colListSet.Strings[i];
      if colList.Count >= 3 then
      begin
        ColSettings[i].Index:= StrToInt(colList.Strings[0]);
        ColSettings[i].Position:= StrToInt(colList.Strings[1]);
        ColSettings[i].Width:= StrToInt(colList.Strings[2]);
        if colList.Count = 4 then
        ColSettings[i].Sort:= TEasySortDirection(StrToInt(colList.Strings[3]));
      end;
    end;

  finally
    colListSet.Free;
    colList.Free;
  end;    
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEFileView
-------------------------------------------------------------------------------}
constructor TCEFileView.Create(AOwner: TComponent);
begin
  inherited;
  fCellWidth:= 0;
  fChangeHistory:= false;
  fTranslateHeader:= false;
  History:= TVirtualShellHistory.Create(self);
  History.Add(TNamespace.Create(nil,nil),true); // Add Desktop to history
  //History.ItemIndex:= -1;
  History.OnChange:= HistoryChange;
  fChangeHistory:= true;

  ShellNewMenu:= TVirtualShellNewMenu.Create(self);
  ShellNewMenu.NewFolderItem:= true;
  //ShellNewMenu.NewShortcutItem:= true;
  ShellNewMenu.OnCreateNewFile:= OnCreateNewFile;
  //self.PopupMenu:= ShellNewMenu;
  UsePNGAlpha:= true;
  fUseKernelNotification:= true;
  ChangeNotifier.RegisterKernelChangeNotify(Self,AllKernelNotifiers);
  fSelectPreviousFolder:= true;
  fAutoSelectFirstItem:= true;
  fAutosizeListViewStyle:= true;
  // Default CellSizes
  Self.CellSizes.SmallIcon.Height:= SmallShellIconSize + 1;
  Self.CellSizes.List.Height:= SmallShellIconSize + 1;
  Self.CellSizes.Report.Height:= SmallShellIconSize + 1;
  Self.Selection.FirstItemFocus:= false;

  UseMouseRocker:= true;
end;

{-------------------------------------------------------------------------------
  Destroy an instance of TCEFileView.
-------------------------------------------------------------------------------}
destructor TCEFileView.Destroy;
begin
  fChangeHistory:= false;
  if fUseKernelNotification then
  ChangeNotifier.UnRegisterKernelChangeNotify(Self);
  //ClearHistory;
  //History.Free;
  //ShellNewMenu.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Clear History
-------------------------------------------------------------------------------}
procedure TCEFileView.ClearHistory;
begin
  fChangeHistory:= false;
  try
    History.Clear;
  finally
    fChangeHistory:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Get's called on mouse dbl click.
-------------------------------------------------------------------------------}
procedure TCEFileView.HandleDblClick(Button: TCommonMouseButton; Msg: TWMMouse);
var
  WindowPt: TPoint;
  item: TEasyItem;
  HitInfo: TEasyItemHitTestInfoSet;
  GoBack: Boolean;
begin
  GoBack:= false;
  if Self.EditManager.Editing then
  Exit;
  
  WindowPt:= Self.Scrollbars.MapWindowToView(Msg.Pos);
  if (Button = cmbLeft) and (WindowPt.Y >= 0) then
  begin
    item:= self.Groups.ItemByPoint(WindowPt);
    GoBack:= item = nil;

    if not GoBack then
    begin
      item.HitTestAt(WindowPt, HitInfo);
      case View of
        elsReport: GoBack:= HitInfo = [];
        elsTile: GoBack:= not (ehtOnIcon in HitInfo) and not (ehtOnText in HitInfo) and not (ehtOnClickSelectBounds in HitInfo);
        else
        GoBack:= not (ehtOnIcon in HitInfo) and not (ehtOnText in HitInfo);
      end;
    end;
  end;

  if GoBack then
  self.BrowseToPrevLevel
  else
  inherited;
end;

{-------------------------------------------------------------------------------
  Get's called when new file is created from ShellNewMenu.
-------------------------------------------------------------------------------}
procedure TCEFileView.OnCreateNewFile(Sender: TMenu; const NewMenuItem:
    TVirtualShellNewItem; var Path, FileName: WideString; var Allow: Boolean);
var
  Handle: THandle;
  TemplateFound: Boolean;
  NewFileSourcePath: WideString;
  Skip: Boolean;
  NewFileTargetPath: WideString;
  ASCIOpen: string;
  NS: TNamespace;
  item: TEasyItem;
  tmpNS: TNamespace;
begin
  NewFileTargetPath:= self.RootFolderNamespace.NameForParsing;

  if WideDirectoryExists(NewFileTargetPath) then
  begin
    try
      NewFileTargetPath := WideIncludeTrailingBackslash(NewFileTargetPath);
      if FileName = '' then
      begin
        if NewMenuItem.NewShellKind <> nmk_Folder then
          NewFileTargetPath := UniqueFileName(NewFileTargetPath + S_NEW + NewMenuItem.FileType + NewMenuItem.Extension)
        else
          NewFileTargetPath := UniqueDirName(NewFileTargetPath + S_NEW + NewMenuItem.FileType)
      end else
      begin
        if NewMenuItem.NewShellKind <> nmk_Folder then
          NewFileTargetPath := NewFileTargetPath + WideStripExt(FileName) + NewMenuItem.Extension
        else
          NewFileTargetPath := NewFileTargetPath + FileName
      end;

      Skip := False;
      if ShellNewMenu.WarnOnOverwrite then
      begin
        if FileExists(NewFileTargetPath) then
          Skip := WideMessageBox(Application.Handle, S_WARNING, S_OVERWRITE_EXISTING_FILE, MB_OKCANCEL or MB_ICONWARNING) = IDCANCEL
      end;
      if not Skip then
      begin
        case NewMenuItem.NewShellKind of
          nmk_Null:
            begin
               Handle := FileCreate(NewFileTargetPath);
               if Handle <> INVALID_HANDLE_VALUE then
               begin
                 FileClose(Handle);
               end;

            end;
          nmk_FileName:
            begin
              TemplateFound := False;
              { This is where the template should be }
              if Assigned(TemplatesFolder) then
              begin
                NewFileSourcePath := TemplatesFolder.NameParseAddress + '\' + NewMenuItem.NewShellKindStr;
                TemplateFound := WideFileExists(NewFileSourcePath);
              end;

              {Look from common templates folder}
              if not TemplateFound then
              begin
                tmpNS:= CreateSpecialNamespace($002d);
                if assigned(tmpNS) then
                begin
                  NewFileSourcePath := tmpNS.NameParseAddress + '\' + NewMenuItem.NewShellKindStr;
                  TemplateFound := WideFileExists(NewFileSourcePath);
                  tmpNS.Free;
                end;
              end;

              {NEW: Some Programs like WinRAR store the templates elsewhere (like in their own programdirectory)}
              {So check if NewShellKindStr points directly to a template and - if yes - use it ...}
              if not TemplateFound then
              begin
                NewFileSourcePath := NewMenuItem.NewShellKindStr;
                TemplateFound := FileExists(NewFileSourcePath);
              end; 

              { Microsoft can't seem to even get its applications to follow the rules   }
              { Some Templates are in the old ShellNew folder in the Windows directory. }
              if not TemplateFound then
              begin
                NewFileSourcePath := WindowsDirectory + '\ShellNew\' + NewMenuItem.NewShellKindStr;
                TemplateFound := FileExists(NewFileSourcePath);
              end;
              if TemplateFound then
              begin
                WideCopyFile(NewFileSourcePath, NewFileTargetPath, True);
                //SHChangeNotify(SHCNE_CREATE, SHCNF_PATH, PChar(NewFileTargetPath), nil);
              end;
            end;
          nmk_Data:
            begin
              Handle := FileCreate(NewFileTargetPath);
              if Handle <> INVALID_HANDLE_VALUE then
              try
                FileWrite(Handle, NewMenuItem.Data^, NewMenuItem.DataSize)
              finally
                FileClose(Handle);
                //SHChangeNotify(SHCNE_CREATE, SHCNF_PATH, PChar(NewFileTargetPath), nil);
              end
            end;
          nmk_Command:
            begin
              if NewMenuItem.IsBriefcase then
              { The Briefcase is a special case that we need to be careful with     }
              begin
                { Strip the *.bfc extension from the file name }
                NewFileTargetPath := WideStripExt(NewFileTargetPath);
                { This is a bit of a hack.  The true Command string in Win2k looks  }
                { like:                                                             }
                {  %SystemRoot%\system32\rundll32.exe %SystemRoot%\system32\syncui.dll,Briefcase_Create %2!d! %1 }
                { This is undocumented as the the parameters but in Win2k and XP if }
                { You pass a path of the new Briefcase for %1 and then set %2 to a  }
                { number > 0 then a Briefcase will be created in the passed folder. }
                { In Win9x the param are reversed and the string is filled in:      }
                { c:\Windows\System\rundll32.exe c:\Windows\System\\syncui.dll,Briefcase_Create %1!d! %2 }
                { In this OS it is not necessary to change the %1 to 1? Oh well     }
                { Undocumented Shell fun at its best.                               }
                Path := SystemDirectory + S_RUNDLL32;
                if not FileExists(Path) then
                  Path := WindowsDirectory + S_RUNDLL32;
                ASCIOpen := S_OPEN;
                WideShellExecute(Application.Handle, ASCIOpen, Path,
                  S_BRIEFCASE_HACK_STRING + NewFileTargetPath,'', SW_SHOW)
              end else
                { Being lazy here, this way I don't have to try to parse arguments  }
                WinExec(PChar(NewMenuItem.NewShellKindStr), SW_SHOW);
            end;
          nmk_Folder:
            begin
              WideCreateDir(NewFileTargetPath);
            end;
          nmk_Shortcut:
            begin
              NewFileTargetPath := ExtractFilePath(NewFileTargetPath);
              NewFileTargetPath := 'rundll32.exe appwiz.cpl,NewLinkHere ' + NewFileTargetPath;
              WinExec(PChar(String(NewFileTargetPath)), SW_SHOW);
            end;
        end;

        if WideFileExists(NewFileTargetPath) or WideDirectoryExists(NewFileTargetPath) then
        begin
          if not Self.Focused then
          Self.SetFocus;

          NS:= TNamespace.CreateFromFileName(NewFileTargetPath);
          item:= self.AddCustomItem(nil,NS,true);
          Self.Selection.SelectRange(item,item,false,true);
          Self.Selection.FocusedItem:= item;
          self.Refresh;
          item.Edit;
        end;

      end;
    finally

    end;
  end;
  Allow:= false;
end;

{-------------------------------------------------------------------------------
  Set View Mode
-------------------------------------------------------------------------------}
procedure TCEFileView.SetView(Value: TEasyListStyle);
var
  i: Integer;
begin
  inherited;
  if Value = elsList then
  begin
    if AutosizeListViewStyle then
    begin
      fCellWidth:= 0;
      for i:= 0 to Self.ItemCount - 1 do
      begin
        fCellWidth:= Max(fCellWidth, Canvas.TextWidth(Self.Items.Items[i].Caption));
      end;
      if fCellWidth > 0 then
      Self.CellSizes.List.Width:= fCellWidth + 32;
      fCellWidth:= 0;
    end
    else
    begin
      Self.CellSizes.List.RestoreDefaults;
    end;
  end;

  if assigned(fOnViewStyleChange) then
  fOnViewStyleChange(self);
end;

{-------------------------------------------------------------------------------
  Create new folder
-------------------------------------------------------------------------------}
procedure TCEFileView.CreateNewFolder;
var
  path: WideString;
  NS: TNamespace;
  item: TEasyItem;
begin
  if WideDirectoryExists(RootFolderNamespace.NameForParsing) then
  begin
   path:= UniqueDirName(WideIncludeTrailingBackslash(RootFolderNamespace.NameForParsing) + _('New Folder'));
   WideCreateDir(path);
   if WideDirectoryExists(path) then
   begin
     if not Self.Focused then
     Self.SetFocus;
     NS:= TNamespace.CreateFromFileName(path);
     item:= AddCustomItem(nil,NS,true);
     Self.Selection.SelectRange(item,item,false,true);
     Self.Selection.FocusedItem:= item;
     //Refresh;
     item.Edit;
   end;
  end;
end;

{-------------------------------------------------------------------------------
  Do CustomColumnAdd
-------------------------------------------------------------------------------}
procedure TCEFileView.DoCustomColumnAdd;
var
  i: Integer;
  col: TEasyColumn;
begin
  inherited;
  if fTranslateHeader then
  begin
    for i:= 0 to Self.Header.Columns.Count-1 do
    begin
      col:= Self.Header.Columns[i];
      col.Caption:= _(col.Caption);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  DoEnumFinished
-------------------------------------------------------------------------------}
procedure TCEFileView.DoEnumFinished;
begin
  if (View = elsList) and AutosizeListViewStyle then
  begin
    if fCellWidth > 0 then
    CellSizes.List.Width:= fCellWidth + 32
    else
    CellSizes.List.RestoreDefaults;
    fCellWidth:= 0;
  end;

  inherited;
end;

{-------------------------------------------------------------------------------
  DoEnumFolder
-------------------------------------------------------------------------------}
procedure TCEFileView.DoEnumFolder(const Namespace: TNamespace; var
    AllowAsChild: Boolean);
begin
  if (View = elsList) and AutosizeListViewStyle then
  begin
    fCellWidth:= Max(fCellWidth, Canvas.TextWidth(Namespace.NameInFolder));
  end;
  
  inherited;
end;

{-------------------------------------------------------------------------------
  Handle Key Action
-------------------------------------------------------------------------------}
procedure TCEFileView.DoKeyAction(var CharCode: Word; var Shift: TShiftState;
    var DoDefault: Boolean);
begin
  if DoDefault then
  begin
    DoDefault:= True;
    case CharCode of
      VK_BACK: begin
        GoFolderUp;
        DoDefault:= false;
      end;
      VK_F2,
      VK_F5,
      VK_DELETE: DoDefault:= Shift <> [];
      Ord('C'), Ord('c'): begin
        if ssCtrl in Shift then
        DoDefault:= false;
      end;
      Ord('X'), Ord('x'): begin
        if ssCtrl in Shift then
        DoDefault:= false;
      end;
      Ord('V'), Ord('v'): begin
        if ssCtrl in Shift then
        DoDefault:= false;
      end;
      Ord('A'), Ord('a'): begin
        if (Shift = [ssShift,ssCtrl]) or (Shift = [ssCtrl]) then
        DoDefault:= false;
      end;
      VK_LEFT, VK_RIGHT:
      begin
        if Shift = [ssAlt] then
        DoDefault:= false;
      end;
      VK_INSERT:
      begin
        if (ssShift in Shift) or (ssCtrl in Shift) then
        DoDefault:= false;
      end;
    end
  end;

  inherited DoKeyAction(CharCode, Shift, DoDefault);
end;

{-------------------------------------------------------------------------------
  Do Root Change
-------------------------------------------------------------------------------}
procedure TCEFileView.DoRootChange;
begin
  inherited;
  // TODO: Redesign history feature
  if fChangeHistory then
  begin
    fChangeHistory:= false;
    if History.ItemIndex > -1 then
    begin
      if RootFolderNamespace.ComparePIDL(History.Items[History.ItemIndex].AbsolutePIDL, true) <> 0 then
      History.Add(RootFolderNamespace, false, true);
    end;    
    fChangeHistory:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Root changing
-------------------------------------------------------------------------------}
procedure TCEFileView.DoRootChanging(const NewRoot: TRootFolder; Namespace:
    TNamespace; var Allow: Boolean);
begin
  inherited;
  SetNotifyFolder(Namespace);
end;

{-------------------------------------------------------------------------------
  Root rebuild
-------------------------------------------------------------------------------}
procedure TCEFileView.DoRootRebuild;
begin
  inherited;
  if AutoSelectFirstItem then
  Selection.SelectFirst;
end;

{-------------------------------------------------------------------------------
  Go Back In History
-------------------------------------------------------------------------------}
procedure TCEFileView.GoBackInHistory;
var
  oldIndex: Integer;
  item: TExplorerItem;
begin
  if SelectPreviousFolder then
  begin
    oldIndex:= History.ItemIndex;
    BeginUpdate;
    try
      History.Back;
      Application.ProcessMessages;
      if (oldIndex > History.ItemIndex) and (oldIndex > -1) and (oldIndex < History.Count) then
      begin
        Selection.ClearAll;
        item:= FindItemByPIDL(History.Items[oldIndex].AbsolutePIDL);
        if assigned(item) then
        begin
          item.MakeVisible(emvTop);
          item.Focused:= true;
          item.Selected:= true;
        end;
      end;
    finally
      EndUpdate;
    end;
  end
  else
  begin
    History.Back;
  end;
end;

{-------------------------------------------------------------------------------
  Go folder up
-------------------------------------------------------------------------------}
procedure TCEFileView.GoFolderUp;
var
  oldPIDL: PItemIDList;
  item: TExplorerItem;
begin
  if SelectPreviousFolder then
  begin
    oldPIDL:= PIDLMgr.CopyPIDL(Self.RootFolderNamespace.AbsolutePIDL);
    BeginUpdate;
    try
      BrowseToPrevLevel;
      Application.ProcessMessages;
      Selection.ClearAll;
      item:= FindItemByPIDL(oldPIDL);
      if assigned(item) then
      begin
        item.MakeVisible(emvTop);
        item.Focused:= true;
        item.Selected:= true;
      end;
    finally
      PIDLMgr.FreeAndNilPIDL(oldPIDL);
      EndUpdate;
    end;
  end
  else
  begin
    BrowseToPrevLevel;
  end;
end;

{-------------------------------------------------------------------------------
  Go Forward in history
-------------------------------------------------------------------------------}
procedure TCEFileView.GoForwardInHistory;
begin
  History.Next;
end;

{-------------------------------------------------------------------------------
  Handle Mouse Down events.
-------------------------------------------------------------------------------}
procedure TCEFileView.HandleMouseDown(Button: TCommonMouseButton; Msg:
    TWMMouse);
begin
  Inherited;
  
  if Button = cmbLeft then
  fLeftMouseButton_IsDown:= true;
  if Button = cmbRight then
  fRightMouseButton_IsDown:= true;

  // Mouse Rocker Navigation
  if UseMouseRocker then
  begin
    if (Button = cmbLeft) and fRightMouseButton_IsDown then
    begin
      GoBackInHistory;
      fLeftMouseButton_RockerClicks:= fLeftMouseButton_RockerClicks + 1;
    end
    else if (Button = cmbRight) and fLeftMouseButton_IsDown then
    begin
      GoForwardInHistory;
      fRightMouseButton_RockerClicks:= fRightMouseButton_RockerClicks + 1;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Mouse Up
-------------------------------------------------------------------------------}
procedure TCEFileView.HandleMouseUp(Button: TCommonMouseButton; Msg: TWMMouse);
begin
  Inherited;
  if Button = cmbLeft then
  begin
    fLeftMouseButton_IsDown:= false;
    if fLeftMouseButton_IsDown then
    fLeftMouseButton_RockerClicks:= 0;
  end
  else if Button = cmbRight then
  begin
    fRightMouseButton_IsDown:= false;
    if fRightMouseButton_IsDown then
    fRightMouseButton_RockerClicks:= 0;
  end;
end;

{-------------------------------------------------------------------------------
  On History change
-------------------------------------------------------------------------------}
procedure TCEFileView.HistoryChange(Sender: TBaseVirtualShellPersistent;
    ItemIndex: Integer; ChangeType: TVSHChangeType);
begin
  if fChangeHistory then
  begin
    if (ChangeType = hctSelected) and (ItemIndex > -1) then
    begin
      fChangeHistory:= false;
      if assigned(History.Items[ItemIndex]) then
      BrowseToByPIDL(History.Items[ItemIndex].AbsolutePIDL, false);
      fChangeHistory:= true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Paste Shortcut From Clipboard
-------------------------------------------------------------------------------}
procedure TCEFileView.PasteShortcutFromClipboard;
var
  NSA: TNamespaceArray;
begin
  Cursor := crHourglass;
  try
    if Assigned(RootFolderNamespace) then
    begin
      SetLength(NSA, 1);
      NSA[0] := RootFolderNamespace;
      RootFolderNamespace.Paste(Self, NSA, True);
    end;
  finally
    Cursor:= crDefault;
  end
end;

{-------------------------------------------------------------------------------
  Set AutosizeListViewStyle
-------------------------------------------------------------------------------}
procedure TCEFileView.SetAutosizeListViewStyle(const Value: Boolean);
var
  i: Integer;
begin
  if Value <> fAutosizeListViewStyle then
  begin
    fAutosizeListViewStyle:= Value;
    if Self.View = elsList then
    begin
      if not fAutosizeListViewStyle then
      Self.CellSizes.List.RestoreDefaults
      else
      begin
        fCellWidth:= 0;
        for i:= 0 to Self.ItemCount - 1 do
        begin
          fCellWidth:= Max(fCellWidth, Canvas.TextWidth(Self.Items.Items[i].Caption));
        end;
        if fCellWidth > 0 then
        Self.CellSizes.List.Width:= fCellWidth + 32;
        fCellWidth:= 0;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set Focus
-------------------------------------------------------------------------------}
procedure TCEFileView.SetFocus;
begin
  if not GetRealVisibility(self) then Exit;
  inherited;
  UpdateAllActions;
end;

{-------------------------------------------------------------------------------
  Set Notify Folder
-------------------------------------------------------------------------------}
procedure TCEFileView.SetNotifyFolder(Namespace: TNamespace);
var
  useKernel: Boolean;
begin
  if assigned(Namespace) then
  begin
    if fUseKernelNotification then
    begin
      useKernel:= not Namespace.IsNetworkNeighborhoodChild;
      if useKernel then
      useKernel:= WideGetDriveType(WideIncludeTrailingBackslash(Namespace.NameForParsing)) = DRIVE_FIXED;

      if useKernel then
      ChangeNotifier.NotifyWatchFolder(Self, Namespace.NameForParsing)
      else
      ChangeNotifier.NotifyWatchFolder(Self, '');
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Set UseKernelNotification
-------------------------------------------------------------------------------}
procedure TCEFileView.SetUseKernelNotification(const Value: Boolean);
begin
  if Value <> fUseKernelNotification then
  begin
    fUseKernelNotification:= Value;
    if fUseKernelNotification then
    begin
      ChangeNotifier.RegisterKernelChangeNotify(Self,AllKernelNotifiers);
    end
    else
    begin
      ChangeNotifier.UnRegisterKernelChangeNotify(Self);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  WM_KillFocus
-------------------------------------------------------------------------------}
procedure TCEFileView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  //Self.ShowInactive:= true;
end;

{-------------------------------------------------------------------------------
  WM_SetFocus
-------------------------------------------------------------------------------}
procedure TCEFileView.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  //Self.ShowInactive:= false;
end;



{-------------------------------------------------------------------------------
  Get/Set Default
-------------------------------------------------------------------------------}
function TCEColumnSettings.GetDefault: string;
begin
  Result:= ColSettingsToString(DefaultColSettings);
end;
procedure TCEColumnSettings.SetDefault(const Value: string);
begin
  StringToColSettings(Value, DefaultColSettings);
end;

{-------------------------------------------------------------------------------
  Get/Set MyComputer
-------------------------------------------------------------------------------}
function TCEColumnSettings.GetMyComputer: string;
begin
  Result:= ColSettingsToString(MyComputerColSettings);
end;
procedure TCEColumnSettings.SetMyComputer(const Value: string);
begin
  StringToColSettings(Value, MyComputerColSettings);
end;

{-------------------------------------------------------------------------------
  Get/Set Network
-------------------------------------------------------------------------------}
function TCEColumnSettings.GetNetwork: string;
begin
  Result:= ColSettingsToString(NetworkColSettings);
end;
procedure TCEColumnSettings.SetNetwork(const Value: string);
begin
  StringToColSettings(Value, NetworkColSettings);
end;

{-------------------------------------------------------------------------------
  Get/Set ControlPanel
-------------------------------------------------------------------------------}
function TCEColumnSettings.GetControlPanel: string;
begin
  Result:= ColSettingsToString(ControlPanelColSettings);
end;
procedure TCEColumnSettings.SetControlPanel(const Value: string);
begin
  StringToColSettings(Value, ControlPanelColSettings);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Column Settings to string
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GroupBySettingsToString(GroupBySetting:
    TCEGroupBySetting): string;
begin
  Result:= IntToStr(GroupBySetting.Index) + ',' + BoolToStr(GroupBySetting.Enabled, true);
end;

{-------------------------------------------------------------------------------
  String to Column Settings
-------------------------------------------------------------------------------}
procedure TCEGroupBySettings.StringToGroupBySettings(AString: String; var
    GroupBySetting: TCEGroupBySetting);
var
  groupList: TStrings;
begin
  groupList:= TStringList.Create;
  try
    groupList.CommaText:= AString;
    if groupList.Count > 1 then
    begin
      GroupBySetting.Index:= StrToIntDef(groupList.Strings[0], 0);
      GroupBySetting.Enabled:= StrToBoolDef(groupList.Strings[1], false);
    end;
  finally
    groupList.Free;
  end;    
end;

{-------------------------------------------------------------------------------
  Get/Set Default
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GetDefault: string;
begin
  Result:= GroupBySettingsToString(DefaultGroupBySettings);
end;

procedure TCEGroupBySettings.SetDefault(const Value: string);
begin
  StringToGroupBySettings(Value, DefaultGroupBySettings);
end;

{-------------------------------------------------------------------------------
  Get/Set MyComputer
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GetMyComputer: string;
begin
  Result:= GroupBySettingsToString(MyComputerGroupBySettings);
end;

procedure TCEGroupBySettings.SetMyComputer(const Value: string);
begin
  StringToGroupBySettings(Value, MyComputerGroupBySettings);
end;

{-------------------------------------------------------------------------------
  Get/Set Network
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GetNetwork: string;
begin
  Result:= GroupBySettingsToString(NetworkGroupBySettings);
end;

procedure TCEGroupBySettings.SetNetwork(const Value: string);
begin
  StringToGroupBySettings(Value, NetworkGroupBySettings);
end;

{-------------------------------------------------------------------------------
  Get/Set ControlPanel
-------------------------------------------------------------------------------}
function TCEGroupBySettings.GetControlPanel: string;
begin
  Result:= GroupBySettingsToString(ControlPanelGroupBySettings);
end;

procedure TCEGroupBySettings.SetControlPanel(const Value: string);
begin
  StringToGroupBySettings(Value, ControlPanelGroupBySettings);
end;

end.
