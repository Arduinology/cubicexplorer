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
//  The Original Code is CE_Layout.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_Layout;

interface

uses
  // CE Units
  CE_Utils, CE_DockInfo,
  // Toolbar200
  TB2Toolbar, TB2Dock, TB2ToolWindow,
  // SpTBX
  SpTBXItem, SpTBXTabs,
  // TNT
  TntSysUtils, TntClasses,
  // JVCL
  JvAppStorage, JvDockVSNetStyle, JvDockInfo, JvDockControlForm,
  JvDockSupportProc, JvDockGlobals, JvAppXMLStorage, JvSimpleXML,
  // mbTBX
  CEJvDockVSNetStyleTBX,
  // System Units
  Classes, Contnrs, Windows, SysUtils, Forms, TB2Item, Messages, Controls;

type
  TCEToolbarDockType = (tdtBoth, tdtInner, tdtOuter);

  TCELayoutItems = class(TComponentList)
  public
    procedure PopulateMenuItem(RootItem: TTBCustomItem);
  end;

  TCEToolbarDocks = class(TObject)
  protected
  public
    OuterDocks: TComponentList;
    InnerDocks: TComponentList;
    constructor Create; overload;
    destructor Destroy; override;
    function Add(ADock: TTBDock; InnerDock: Boolean = true): Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    function FindDockNamed(AName: String): TTBDock; overload;
    function FindDockNamed(AName: String; out IsInner: Boolean): TTBDock; overload;
    function FindInnerDockNamed(AName: String): TTBDock;
    function FindOuterDockNamed(AName: String): TTBDock;
    function IsInnerDock(ADock: TTBDock): Boolean;
    function IsOuterDock(ADock: TTBDock): Boolean;
  end;

  TCELayoutController = class(TComponent)
  private
    fAutoSave: Boolean;
    fCurrentLayout: String;
    fFilePath: WideString;
    procedure InitSelf;
  protected
    CurrentFormLayout: string;
    CurrentInnerToolbarLayout: string;
    CurrentOuterToolbarLayout: string;
  public
    Layouts: TStrings;
    AppStorage: TJvAppXMLFileStorage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilePath: WideString);
    procedure LoadLayout(LayoutName: String; LoadInnerToolbarLayout,
        LoadOuterToolbarLayout, LoadFormLayout: Boolean);
    procedure LoadToolbarLayout;
    procedure SaveCurrentLayout;
    procedure SaveLayout(LayoutName: String; SaveInnerToolbarLayout,
        SaveOuterToolbarLayout, SaveFormsLayout: Boolean);
    procedure SaveToFile(AFilePath: WideString = '');
    procedure SaveToolbarLayout;
    property AutoSave: Boolean read fAutoSave write fAutoSave;
    property CurrentLayout: String read fCurrentLayout write fCurrentLayout;
    property FilePath: WideString read fFilePath write fFilePath;
  end;


  procedure LoadToolbars(AppStorage: TJvCustomAppStorage; AppStoragePath: String
      = ''; DockType: TCEToolbarDockType = tdtBoth);
  procedure SaveToolbars(AppStorage: TJvCustomAppStorage; AppStoragePath: String
      = ''; DockType: TCEToolbarDockType = tdtBoth);
  procedure LoadDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath:
      String = ''; HandleMainForm: Boolean = false);
  procedure SaveDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath:
      String = '');
  procedure SaveToolbarItemLayout(AppStorage: TJvAppXMLFileStorage);
  procedure LoadToolbarItemLayout(AppStorage: TJvAppXMLFileStorage);
  procedure CE_DoFloatForm(DockForm: TControl);
  procedure CE_DoFloatAllForm;

procedure BeginToolbarCustomize;

procedure EndToolbarCustomize;

var
  DefaultLayoutName: String = 'Default';
  CELayoutItems: TCELayoutItems;
  CEToolbarDocks: TCEToolbarDocks;
  CEDockStyle: TJvDockVSNetStyleTBX;

implementation

uses
  fCE_DockHostForm, fCE_ToolbarCustomizer, Main, fCE_DockableForm, CE_SpTabBar,
  CE_StatusBar;

{*------------------------------------------------------------------------------
  Create an instance of TCELayoutController
-------------------------------------------------------------------------------}
constructor TCELayoutController.Create(AOwner: TComponent);
begin
  inherited;
  Layouts:= TStringList.Create;
  AppStorage:= TJvAppXMLFileStorage.Create(nil);
  AppStorage.AutoFlush:= false;
  AppStorage.AutoReload:= false;
  AppStorage.FlushOnDestroy:= false;
  AppStorage.Path:= 'Layouts';
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCELayoutController
-------------------------------------------------------------------------------}
destructor TCELayoutController.Destroy;
begin
  AppStorage.Free;
  Layouts.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Initialize layouts.
-------------------------------------------------------------------------------}
procedure TCELayoutController.InitSelf;
var
  node, chNode, chNode2: TJvSimpleXmlElem;
  i: Integer;
begin
  Layouts.BeginUpdate;
  Layouts.Clear;
  try
    node:= AppStorage.Xml.Root.Items.ItemNamed['Layouts'];
    if node <> nil then
    begin
      for i:= 0 to node.Items.Count-1 do
      begin
        Layouts.Add(node.Items.Item[i].Name);
      end;

      // Make sure older layout files work
      if Layouts.IndexOf(DefaultLayoutName) = -1 then
      begin
        chNode:= node.Items.ItemNamed['FileView'];
        if assigned(chNode) then
        begin
          chNode2:= node.Items.Add(DefaultLayoutName);
          chNode2.Assign(chNode);
          chNode2.Name:= DefaultLayoutName;
        end;
      end;
    end;

  finally
    Layouts.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Load layout configuration from file
-------------------------------------------------------------------------------}
procedure TCELayoutController.LoadFromFile(AFilePath: WideString);
var
  s: TStream;
begin
  fFilePath:= AFilePath;
  AppStorage.BeginUpdate;
  try
    if WideFileExists(fFilePath) then
    begin
      s:= TTntFileStream.Create(fFilePath, fmOpenRead or fmShareDenyNone);
      try
        s.Position:= 0;
        AppStorage.Xml.LoadFromStream(s);
      finally
        s.Free;
      end;
    end
    else
    begin
      AppStorage.Xml.LoadFromResourceName(hInstance, 'DEFAULT_LAYOUT_DATA');
    end;
  finally
    AppStorage.EndUpdate;
    InitSelf;
  end;
end;

{*------------------------------------------------------------------------------
  Save layout configuration to file
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveToFile(AFilePath: WideString = '');
var
  s: TStream;
begin
  try
    if AFilePath <> '' then
    fFilePath:= AFilePath;
    s:= TTntFileStream.Create(fFilePath, fmCreate);
    try
      s.Position:= 0;
      AppStorage.Xml.SaveToStream(s);
    finally
      s.Free;
    end;
  except
  end;
end;

{*------------------------------------------------------------------------------
  Load layout
-------------------------------------------------------------------------------}
procedure TCELayoutController.LoadLayout(LayoutName: String;
    LoadInnerToolbarLayout, LoadOuterToolbarLayout, LoadFormLayout: Boolean);
var
  s: String;
begin
  if Layouts.IndexOf(LayoutName) <> -1 then
  begin
    fCurrentLayout:= LayoutName;
    MainForm.BeginUIUpdate;
    try
      // Load Form Layout
      if LoadFormLayout then
      s:= LayoutName
      else
      s:= DefaultLayoutName;
      if s <> CurrentFormLayout then
      begin
        LoadDockedForms(AppStorage, s);
        CurrentFormLayout:= s;
      end;

      // Load Toolbar Layout
      if LoadInnerToolbarLayout and LoadOuterToolbarLayout then
      begin
        LoadToolbars(AppStorage, LayoutName, tdtBoth);
        CurrentInnerToolbarLayout:= LayoutName;
        CurrentOuterToolbarLayout:= LayoutName;
      end
      else if (not LoadInnerToolbarLayout) and (not LoadOuterToolbarLayout) then
      begin
        LoadToolbars(AppStorage, DefaultLayoutName, tdtBoth);
        CurrentInnerToolbarLayout:= DefaultLayoutName;
        CurrentOuterToolbarLayout:= DefaultLayoutName;
      end
      else
      begin
        // Inner Toolbar Layout
        if LoadInnerToolbarLayout then
        s:= LayoutName
        else
        s:= DefaultLayoutName;
        if s <> CurrentInnerToolbarLayout then
        begin
          LoadToolbars(AppStorage, s, tdtInner);
          CurrentInnerToolbarLayout:= s;
        end;
        // Outer Toolbar Layout
        if LoadOuterToolbarLayout then
        s:= LayoutName
        else
        s:= DefaultLayoutName;
        if s <> CurrentOuterToolbarLayout then
        begin
          LoadToolbars(AppStorage, s, tdtOuter);
          CurrentOuterToolbarLayout:= s;
        end;
      end;
    finally
      MainForm.EndUIUpdate;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Save Layout
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveLayout(LayoutName: String;
    SaveInnerToolbarLayout, SaveOuterToolbarLayout, SaveFormsLayout: Boolean);
var
  s: String;
begin
  if SaveFormsLayout then
  s:= LayoutName
  else
  s:= DefaultLayoutName;
  SaveDockedForms(AppStorage, s);

  if SaveInnerToolbarLayout and SaveOuterToolbarLayout then
  begin
    SaveToolbars(AppStorage, LayoutName, tdtBoth);
  end
  else if (not SaveInnerToolbarLayout) and (not SaveOuterToolbarLayout) then
  begin
    SaveToolbars(AppStorage, DefaultLayoutName, tdtBoth);
  end
  else
  begin
    if SaveInnerToolbarLayout then
    s:= LayoutName
    else
    s:= DefaultLayoutName;
    SaveToolbars(AppStorage, s, tdtInner);

    if SaveOuterToolbarLayout then
    s:= LayoutName
    else
    s:= DefaultLayoutName;
    SaveToolbars(AppStorage, s, tdtOuter);
  end;

  InitSelf;
  if fAutoSave then
  SaveToFile;
end;

{*------------------------------------------------------------------------------
  Load Toolbars Items
-------------------------------------------------------------------------------}
procedure TCELayoutController.LoadToolbarLayout;
begin
  LoadToolbarItemLayout(AppStorage);
end;

{-------------------------------------------------------------------------------
  Save Current Layout
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveCurrentLayout;
begin
  SaveDockedForms(AppStorage, CurrentFormLayout);
  if CurrentInnerToolbarLayout <> CurrentOuterToolbarLayout then
  begin
    SaveToolbars(AppStorage, CurrentInnerToolbarLayout, tdtInner);
    SaveToolbars(AppStorage, CurrentOuterToolbarLayout, tdtOuter);
  end
  else
  begin
    SaveToolbars(AppStorage, CurrentInnerToolbarLayout, tdtBoth);
  end;
end;

{*------------------------------------------------------------------------------
  Saves Toolbars items
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveToolbarLayout;
begin
  SaveToolbarItemLayout(AppStorage);
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEToolbarDocks
-------------------------------------------------------------------------------}
constructor TCEToolbarDocks.Create;
begin
  inherited Create;
  InnerDocks:= TComponentList.Create(false);
  OuterDocks:= TComponentList.Create(false);
end;

{-------------------------------------------------------------------------------
  Destroy TCEToolbarDocks
-------------------------------------------------------------------------------}
destructor TCEToolbarDocks.Destroy;
begin
  InnerDocks.Free;
  OuterDocks.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add
-------------------------------------------------------------------------------}
function TCEToolbarDocks.Add(ADock: TTBDock; InnerDock: Boolean = true):
    Integer;
begin
  if InnerDock then
  InnerDocks.Add(ADock)
  else
  OuterDocks.Add(ADock);
end;

{*------------------------------------------------------------------------------
  Begin Update
-------------------------------------------------------------------------------}
procedure TCEToolbarDocks.BeginUpdate;
var
  i: Integer;
begin
  for i:= 0 to InnerDocks.Count-1 do
  begin
    TTBDock(InnerDocks.Items[i]).BeginUpdate;
  end;
  for i:= 0 to OuterDocks.Count-1 do
  begin
    TTBDock(OuterDocks.Items[i]).BeginUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  End Update
-------------------------------------------------------------------------------}
procedure TCEToolbarDocks.EndUpdate;
var
  i: Integer;
begin
  for i:= 0 to InnerDocks.Count-1 do
  begin
    TTBDock(InnerDocks.Items[i]).EndUpdate;
  end;
  for i:= 0 to OuterDocks.Count-1 do
  begin
    TTBDock(OuterDocks.Items[i]).EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Find Dock by it's name
-------------------------------------------------------------------------------}
function TCEToolbarDocks.FindDockNamed(AName: String): TTBDock;
var
  i: Integer;
begin
  Result:= nil;
  // Outer Docks
  for i:= 0 to OuterDocks.Count - 1 do
  begin
    if CompareText(AName, OuterDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(OuterDocks.Items[i]);
      Break;
    end;
  end;
  // Inner Docks
  for i:= 0 to InnerDocks.Count - 1 do
  begin
    if CompareText(AName, InnerDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(InnerDocks.Items[i]);
      Break;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Find Dock by it's name
-------------------------------------------------------------------------------}
function TCEToolbarDocks.FindDockNamed(AName: String; out IsInner: Boolean):
    TTBDock;
var
  i: Integer;
begin
  Result:= nil;
  IsInner:= false;
  // Outer Docks
  for i:= 0 to OuterDocks.Count - 1 do
  begin
    if CompareText(AName, OuterDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(OuterDocks.Items[i]);
      Break;
    end;
  end;
  // Inner Docks
  for i:= 0 to InnerDocks.Count - 1 do
  begin
    if CompareText(AName, InnerDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(InnerDocks.Items[i]);
      IsInner:= true;
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Find Inner Dock Named
-------------------------------------------------------------------------------}
function TCEToolbarDocks.FindInnerDockNamed(AName: String): TTBDock;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to InnerDocks.Count - 1 do
  begin
    if CompareText(AName, InnerDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(InnerDocks.Items[i]);
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Find Outer Dock Named
-------------------------------------------------------------------------------}
function TCEToolbarDocks.FindOuterDockNamed(AName: String): TTBDock;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to OuterDocks.Count - 1 do
  begin
    if CompareText(AName, OuterDocks.Items[i].Name) = 0 then
    begin
      Result:= TTBDock(OuterDocks.Items[i]);
      Break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Is Inner Dock
-------------------------------------------------------------------------------}
function TCEToolbarDocks.IsInnerDock(ADock: TTBDock): Boolean;
begin
  Result:= InnerDocks.IndexOf(ADock) > -1;
end;

{-------------------------------------------------------------------------------
  Is Outer Dock
-------------------------------------------------------------------------------}
function TCEToolbarDocks.IsOuterDock(ADock: TTBDock): Boolean;
begin
  Result:= OuterDocks.IndexOf(ADock) > -1;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Load toolbars from AppStorage.
-------------------------------------------------------------------------------}
procedure LoadToolbars(AppStorage: TJvCustomAppStorage; AppStoragePath: String
    = ''; DockType: TCEToolbarDockType = tdtBoth);
var
  OldPath: String;
  RootPath: String;
  i: Integer;
  s: String;
  dockablewindow: TTBCustomDockableWindow;
  dock: TTBDock;
  tabset: TCESpTabSet;
  statusbar: TCEStatusBar;
  isInnerDock: Boolean;
begin
  if not assigned(AppStorage) then
  Exit;
  OldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, AppStoragePath, 'Toolbars']);
    RootPath:= AppStorage.Path;
    CEToolbarDocks.BeginUpdate;
    for i:= 0 to CELayoutItems.Count - 1 do
    begin
      if CELayoutItems.Items[i] is TTBCustomDockableWindow then
      begin
        dockablewindow:= TTBCustomDockableWindow(CELayoutItems.Items[i]);
        dockablewindow.BeginUpdate;
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, dockablewindow.Name]);
        try
          // Read toolbar properties
          s:= AppStorage.ReadString('Dock', 'TopToolDock');
          if s <> '' then
          begin
            case DockType of
              tdtBoth: dock:= CEToolbarDocks.FindDockNamed(s);
              tdtInner: dock:= CEToolbarDocks.FindInnerDockNamed(s);
              tdtOuter: dock:= CEToolbarDocks.FindOuterDockNamed(s);
            end;
            if assigned(dock) then
            begin
              dockablewindow.CurrentDock:= dock;
              dockablewindow.DockPos:= AppStorage.ReadInteger('DockPos', dockablewindow.DockPos);
              dockablewindow.DockRow:= AppStorage.ReadInteger('DockRow', dockablewindow.DockRow);
              dockablewindow.Visible:= AppStorage.ReadBoolean('Visible', dockablewindow.Visible);
            end;
          end
          else
          begin
            dockablewindow.Visible:= AppStorage.ReadBoolean('Visible', dockablewindow.Visible);
            dockablewindow.Floating:= true;
          end;
        finally
          dockablewindow.EndUpdate;
          AppStorage.Path:= RootPath;
        end;
      end
      else if (CELayoutItems.Items[i] is TCESpTabSet) and (DockType <> tdtInner) then
      begin
        tabset:= TCESpTabSet(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, tabset.Name]);
        try
          tabset.Visible:= AppStorage.ReadBoolean('Visible', tabset.Visible);
        finally
          AppStorage.Path:= RootPath;
        end;
      end
      else if (CELayoutItems.Items[i] is TCEStatusBar) and (DockType <> tdtInner) then
      begin
        statusbar:= TCEStatusBar(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, statusbar.Name]);
        try
          statusbar.Visible:= AppStorage.ReadBoolean('Visible', statusbar.Visible);
        finally
          AppStorage.Path:= RootPath;
        end;
      end;
    end;
  finally
    CEToolbarDocks.EndUpdate;
    AppStorage.Path:= OldPath;
  end;
end;

{*------------------------------------------------------------------------------
  Save toolbars from CELayoutItems to AppStorage.
-------------------------------------------------------------------------------}
procedure SaveToolbars(AppStorage: TJvCustomAppStorage; AppStoragePath: String
    = ''; DockType: TCEToolbarDockType = tdtBoth);
var
  OldPath: String;
  RootPath: String;
  i: Integer;
  dockablewindow: TTBCustomDockableWindow;
  tabset: TCESpTabSet;
  statusbar: TCEStatusBar;
begin
  if not assigned(AppStorage) then
  Exit;

  AppStorage.BeginUpdate;
  OldPath:= AppStorage.Path;
  try
    AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, AppStoragePath, 'Toolbars']);
    RootPath:= AppStorage.Path;
    for i:= 0 to CELayoutItems.Count - 1 do
    begin
      if CELayoutItems.Items[i] is TTBCustomDockableWindow then
      begin
        dockablewindow:= TTBCustomDockableWindow(CELayoutItems.Items[i]);
        if (DockType = tdtBoth) or
           ((DockType = tdtInner) and CEToolbarDocks.IsInnerDock(dockablewindow.CurrentDock)) or
           ((DockType = tdtOuter) and CEToolbarDocks.IsOuterDock(dockablewindow.CurrentDock)) then
        begin
          AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, dockablewindow.Name]);

          // Write toolbar properties
          AppStorage.WriteInteger('DockPos', dockablewindow.DockPos);
          AppStorage.WriteInteger('DockRow', dockablewindow.DockRow);
          if not dockablewindow.Floating then
          AppStorage.WriteString('Dock', dockablewindow.CurrentDock.Name)
          else
          AppStorage.WriteString('Dock', '');
          AppStorage.WriteBoolean('Visible', dockablewindow.Visible);

          AppStorage.Path:= RootPath;
        end;
      end
      else if (CELayoutItems.Items[i] is TCESpTabSet) and (DockType <> tdtInner) then
      begin
        tabset:= TCESpTabSet(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, tabset.Name]);
        try
          AppStorage.WriteBoolean('Visible', tabset.Visible);
        finally
          AppStorage.Path:= RootPath;
        end;
      end
      else if (CELayoutItems.Items[i] is TCEStatusBar) and (DockType <> tdtInner) then
      begin
        statusbar:= TCEStatusBar(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, statusbar.Name]);
        try
          AppStorage.WriteBoolean('Visible', statusbar.Visible);
        finally
          AppStorage.Path:= RootPath;
        end;
      end;
    end;
  finally
    AppStorage.Path:= OldPath;
    AppStorage.EndUpdate;
  end;
end;



{*------------------------------------------------------------------------------
  Load Dockable Forms
-------------------------------------------------------------------------------}
procedure LoadDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath:
    String = ''; HandleMainForm: Boolean = false);
var
  DockInfoTree: TCEDockInfoTree;
  form: TCustomForm;
  i: Integer;
  r: TRect;
begin
  if not assigned(AppStorage) then
  Exit;
  
  BeginDockLoading;
  try
    CE_DoFloatAllForm;
    HideAllPopupPanel(nil);

    DockInfoTree:= TCEDockInfoTree.Create(TJvDockInfoZone);
    DockInfoTree.HandleMainForm:= HandleMainForm;

    try
      DockInfoTree.AppStorage:= AppStorage;
      DockInfoTree.AppStoragePath:= AppStoragePath;
      try
        DockInfoTree.ReadInfoFromAppStorage;
      finally
      end;
    finally
      DockInfoTree.Free;
    end;

    for I := 0 to Screen.CustomFormCount - 1 do
    begin
      form:= Screen.CustomForms[I];
      if form is TCECustomDockableForm then
      begin
        if form.Parent = MainForm.DockHostForm.CenterPanel then
        begin
          r:= form.BoundsRect;
          form.ManualFloat(r);
        end;
      end;

      if (form is TCECustomDockableForm) or (form is TJvDockTabHostForm) or (form is TCEDockHostForm) then
      begin
        if form.Visible then
        Windows.ShowWindow(form.Handle, SW_SHOW)
        else
        Windows.ShowWindow(form.Handle, SW_HIDE);
      end;
    end;
  finally
    EndDockLoading;
  end;
end;

{*------------------------------------------------------------------------------
  Save Dockable Forms
-------------------------------------------------------------------------------}
procedure SaveDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath: String = '');
var
  DockInfoTree: TCEDockInfoTree;
  i: Integer;
  form: TCustomForm;
begin
  if not assigned(AppStorage) then
  Exit;
  AppStorage.BeginUpdate;
  try
    HideAllPopupPanel(nil);
    DockInfoTree := TCEDockInfoTree.Create(TJvDockInfoZone);
    try
      for I := 0 to Screen.CustomFormCount - 1 do
      begin
        form:= Screen.CustomForms[I];
        if ((form.Parent = nil) or (form is TCEDockHostForm)) and ((FindDockClient(form) <> nil) or (FindDockServer(form) <> nil)) then
        begin
          DockInfoTree.CreateZoneAndAddInfoFromApp(form);
        end;
      end;
      DockInfoTree.AppStorage:= AppStorage;
      DockInfoTree.AppStoragePath:= AppStoragePath;
      DockInfoTree.WriteInfoToAppStorage;
    finally
      DockInfoTree.Free;
    end;
  finally
    AppStorage.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Save Toolbar item layout
-------------------------------------------------------------------------------}
procedure SaveToolbarItemLayout(AppStorage: TJvAppXMLFileStorage);
var
  i: Integer;
  toolbar: TSpTBXToolbar;
  elem,rootElem: TJvSimpleXMLElem;
begin
  AppStorage.BeginUpdate;
  try
    rootElem:= AppStorage.Xml.Root.Items.ItemNamed['ToolbarItems'];
    if not assigned(rootElem) then
    rootElem:= AppStorage.Xml.Root.Items.Add('ToolbarItems');
    rootElem.Items.Clear;
    for i:= 0 to CELayoutItems.Count - 1 do
    begin
      if CELayoutItems.Items[i] is TSpTBXToolbar then
      begin
        toolbar:= TSpTBXToolbar(CELayoutItems.Items[i]);
        if toolbar.Customizable then
        begin
          elem:= rootElem.Items.Add(toolbar.Name);
          SaveToolbarItems(toolbar,elem);
        end;
      end
      else if CELayoutItems.Items[i] is TCESpTabSet then
      begin
        toolbar:= TCESpTabSet(CELayoutItems.Items[i]).Toolbar;
        if toolbar.Customizable then
        begin
          elem:= rootElem.Items.Add(toolbar.Name);
          SaveToolbarItems(toolbar,elem);
        end;
      end;
    end;
  finally
    AppStorage.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Load toolbar item layout
-------------------------------------------------------------------------------}
procedure LoadToolbarItemLayout(AppStorage: TJvAppXMLFileStorage);
var
  i: Integer;
  toolbar: TSpTBXToolbar;
  elem,rootElem: TJvSimpleXMLElem;
begin
  CEToolbarDocks.BeginUpdate;
  try
    rootElem:= AppStorage.Xml.Root.Items.ItemNamed['ToolbarItems'];
    if assigned(rootElem) then
    begin
      for i:= 0 to CELayoutItems.Count - 1 do
      begin
        if CELayoutItems.Items[i] is TSpTBXToolbar then
        begin
          toolbar:= TSpTBXToolbar(CELayoutItems.Items[i]);
          elem:= rootElem.Items.ItemNamed[toolbar.Name];
          if assigned(elem) then
          begin
            LoadToolbarItems(toolbar,elem);
          end;
        end
        else if CELayoutItems.Items[i] is TCESpTabSet then
        begin
          toolbar:= TCESpTabSet(CELayoutItems.Items[i]).Toolbar;
          elem:= rootElem.Items.ItemNamed[toolbar.Name];
          if assigned(elem) then
          begin
            LoadToolbarItems(toolbar,elem);
          end;
        end;     
      end;
    end;
  finally
    CEToolbarDocks.EndUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  Do Float Form
-------------------------------------------------------------------------------}
procedure CE_DoFloatForm(DockForm: TControl);
var
  I: TAlign;
  J: Integer;
  ADockServer: TJvDockServer;
  //  ARect: TRect;
  Channel: TJvDockVSChannel;
begin
  if DockForm is TJvDockableForm then
  begin
    with TJvDockableForm(DockForm).DockableControl do
    begin
      for J := DockClientCount - 1 downto 0 do
        CE_DoFloatForm(DockClients[J]);

      DockForm.ManualDock(MainForm.DockHostForm.CenterPanel);
    end;
  end
  else
  begin
    ADockServer := FindDockServer(DockForm);
    if ADockServer <> nil then
    begin
      for I := alTop to alRight do
      begin
        if Assigned(ADockServer.DockPanelWithAlign[I]) then
        begin
          for J := ADockServer.DockPanelWithAlign[I].DockClientCount - 1 downto 0 do
          begin
            CE_DoFloatForm(ADockServer.DockPanelWithAlign[I].DockClients[J]);
          end;

          if ADockServer.DockPanelWithAlign[I] is TJvDockVSNETPanel then
          begin
            with TJvDockVSNETPanel(ADockServer.DockPanelWithAlign[I]).VSChannel do
            begin
              RemoveAllBlock;
              HidePopupPanel(ActiveDockForm);
            end;
          end;
        end;
      end;
    end
    else
    begin
      if DockForm.HostDockSite <> nil then
      begin
        if (DockForm.HostDockSite.Parent is TJvDockableForm) and (DockForm.HostDockSite.DockClientCount <= 2) then
        PostMessage(DockForm.HostDockSite.Parent.Handle, WM_CLOSE, 0, 0);
      end;

      Channel := RetrieveChannel(DockForm.HostDockSite);
      if Assigned(Channel) then
      begin
        Channel.RemoveDockControl(TWinControl(DockForm));
        DockForm.Dock(MainForm.DockHostForm.CenterPanel, Bounds(DockForm.Left, DockForm.Top, DockForm.UndockWidth, DockForm.UndockHeight));
      end
      else
      begin
        //if DockForm.Parent <> nil then
        DockForm.ManualDock(MainForm.DockHostForm.CenterPanel);
      end;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Do Float AllForm
-------------------------------------------------------------------------------}
procedure CE_DoFloatAllForm;
var
  I: Integer;
  TempList: TList;
begin
  TempList := TList.Create;
  try
    for I := 0 to Screen.CustomFormCount - 1 do
      if not (Screen.CustomForms[I] is TJvDockableForm) and
        (Assigned(FinddockClient(Screen.CustomForms[I])) or
         Assigned(FinddockServer(Screen.CustomForms[I]))) then
        TempList.Add(Screen.CustomForms[I]);

    for I := 0 to TempList.Count - 1 do
      CE_DoFloatForm(TempList[I]);
  finally
    TempList.Free;
  end;
  FreeAllDockableForm;
end;

{-------------------------------------------------------------------------------
  Begin Toolbar Customize
-------------------------------------------------------------------------------}
procedure BeginToolbarCustomize;
var
  i: Integer;
  toolbar: TSpTBXToolbar;
begin
  for i:= 0 to CELayoutItems.Count-1 do
  begin
    if CELayoutItems.Items[i] is TSpTBXTabSet then
    toolbar:= TSpTBXTabSet(CELayoutItems.Items[i]).Toolbar
    else if CELayoutItems.Items[i] is TSpTBXToolbar then
    toolbar:= TSpTBXToolbar(CELayoutItems.Items[i])
    else
    toolbar:= nil;

    if assigned(toolbar) then
    begin
      toolbar.BeginCustomize;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  End Toolbar Customize
-------------------------------------------------------------------------------}
procedure EndToolbarCustomize;
var
  i: Integer;
  toolbar: TSpTBXToolbar;
begin
  for i:= 0 to CELayoutItems.Count-1 do
  begin
    if CELayoutItems.Items[i] is TSpTBXTabSet then
    toolbar:= TSpTBXTabSet(CELayoutItems.Items[i]).Toolbar
    else if CELayoutItems.Items[i] is TSpTBXToolbar then
    toolbar:= TSpTBXToolbar(CELayoutItems.Items[i])
    else
    toolbar:= nil;

    if assigned(toolbar) then
    begin
      toolbar.EndCustomize;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Add toolbar toggles to menuitem
-------------------------------------------------------------------------------}
procedure TCELayoutItems.PopulateMenuItem(RootItem: TTBCustomItem);
var
  i: Integer;
  comp: TControl;
  item: TSpTBXItem;
begin
  for i:= 0 to Count - 1 do
  begin
    if (CELayoutItems.Items[i] is TTBCustomDockableWindow) or (CELayoutItems.Items[i] is TCESpTabSet) then
    begin
      comp:= TControl(Items[i]);
      item:= TSpTBXItem.Create(RootItem);
      RootItem.Add(item);
      item.Control:= comp;
      if comp is TSpTBXToolbar then
      item.Caption:= TSpTBXToolbar(comp).Caption
      else if comp is TSpTBXToolWindow then
      item.Caption:= TSpTBXToolWindow(comp).Caption
      else if comp is TCESpTabSet then
      item.Caption:= TCESpTabSet(comp).Toolbar.Caption;
    end;
  end;
end;

{##############################################################################}

initialization
  CELayoutItems:= TCELayoutItems.Create(false);
  CEToolbarDocks:= TCEToolbarDocks.Create;
  CEDockStyle:= TJvDockVSNetStyleTBX.Create(nil);

finalization
  FreeAndNil(CEToolbarDocks);
  FreeAndNil(CELayoutItems);
  FreeAndNil(CEDockStyle);
  
end.
