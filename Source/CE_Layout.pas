unit CE_Layout;

interface

uses
  // CE Units
  CE_Utils, CE_DockInfo,
  // Toolbar200
  TB2Toolbar, TB2Dock, TB2ToolWindow,
  // SpTBX
  SpTBXItem,
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

  TCELayoutItems = class(TComponentList)
  public
    procedure PopulateMenuItem(RootItem: TTBCustomItem);
  end;

  TCEToolbarDocks = class(TComponentList)
  protected
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    function FindDockNamed(AName: String): TTBDock;
  end;

  TCELayoutController = class(TComponent)
  private
    fAutoSave: Boolean;
    fCurrentLayout: String;
    fFilePath: WideString;
    procedure InitSelf;
  public
    Layouts: TStrings;
    AppStorage: TJvAppXMLFileStorage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilePath: WideString);
    procedure LoadLayout(LayoutName: String);
    procedure LoadToolbarLayout;
    procedure SaveLayout(LayoutName: String);
    procedure SaveToFile(AFilePath: WideString = '');
    procedure SaveToolbarLayout;
    property AutoSave: Boolean read fAutoSave write fAutoSave;
    property CurrentLayout: String read fCurrentLayout write fCurrentLayout;
    property FilePath: WideString read fFilePath write fFilePath;
  end;


  procedure LoadToolbars(AppStorage: TJvCustomAppStorage; AppStoragePath: String = '');
  procedure SaveToolbars(AppStorage: TJvCustomAppStorage; AppStoragePath: String = '');
  procedure LoadDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath:
      String = ''; HandleMainForm: Boolean = false);
  procedure SaveDockedForms(AppStorage: TJvCustomAppStorage; AppStoragePath:
      String = '');
  procedure SaveToolbarItemLayout(AppStorage: TJvAppXMLFileStorage);
  procedure LoadToolbarItemLayout(AppStorage: TJvAppXMLFileStorage);
  procedure CE_DoFloatForm(DockForm: TControl);
  procedure CE_DoFloatAllForm;

var
  CELayoutItems: TCELayoutItems;
  CEToolbarDocks: TCEToolbarDocks;
  CEDockStyle: TJvDockVSNetStyleTBX;

implementation

uses
  fCE_DockHostForm, fCE_ToolbarCustomizer, Main, fCE_DockableForm, CE_SpTabBar;

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
  node: TJvSimpleXmlElem;
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
procedure TCELayoutController.LoadLayout(LayoutName: String);
begin
  if Layouts.IndexOf(LayoutName) <> -1 then
  begin
    fCurrentLayout:= LayoutName;
    MainForm.BeginUIUpdate;
    try
      LoadToolbars(AppStorage, LayoutName);
      LoadDockedForms(AppStorage, LayoutName);
    finally
      MainForm.EndUIUpdate;
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Save Layout
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveLayout(LayoutName: String);
begin
  SaveDockedForms(AppStorage, LayoutName);
  SaveToolbars(AppStorage, LayoutName);
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

{*------------------------------------------------------------------------------
  Saves Toolbars items
-------------------------------------------------------------------------------}
procedure TCELayoutController.SaveToolbarLayout;
begin
  SaveToolbarItemLayout(AppStorage);
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Begin Update
-------------------------------------------------------------------------------}
procedure TCEToolbarDocks.BeginUpdate;
var
  i: Integer;
begin
  for i:= 0 to Count-1 do
  begin
    TTBDock(Self.Items[i]).BeginUpdate;
  end;
end;

{*------------------------------------------------------------------------------
  End Update
-------------------------------------------------------------------------------}
procedure TCEToolbarDocks.EndUpdate;
var
  i: Integer;
begin
  for i:= 0 to Count-1 do
  begin
    TTBDock(Self.Items[i]).EndUpdate;
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
  for i:= 0 to self.Count - 1 do
  begin
    if CompareText(AName, self.GetItems(i).Name) = 0 then
    begin
      Result:= TTBDock(Self.GetItems(i));
      Break;
    end;
  end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Load toolbars from AppStorage.
-------------------------------------------------------------------------------}
procedure LoadToolbars(AppStorage: TJvCustomAppStorage; AppStoragePath: String = '');
var
  OldPath: String;
  RootPath: String;
  i: Integer;
  s: String;
  dockablewindow: TTBCustomDockableWindow;
  tabset: TCESpTabSet;
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
            dockablewindow.CurrentDock:= CEToolbarDocks.FindDockNamed(s);
            dockablewindow.DockPos:= AppStorage.ReadInteger('DockPos', dockablewindow.DockPos);
            dockablewindow.DockRow:= AppStorage.ReadInteger('DockRow', dockablewindow.DockRow);
            dockablewindow.Visible:= AppStorage.ReadBoolean('Visible', dockablewindow.Visible);
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
      else if CELayoutItems.Items[i] is TCESpTabSet then
      begin
        tabset:= TCESpTabSet(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, tabset.Name]);
        try
          tabset.Visible:= AppStorage.ReadBoolean('Visible', tabset.Visible);
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
procedure SaveToolbars(AppStorage: TJvCustomAppStorage; AppStoragePath: String = '');
var
  OldPath: String;
  RootPath: String;
  i: Integer;
  dockablewindow: TTBCustomDockableWindow;
  tabset: TCESpTabSet;
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
      end
      else if CELayoutItems.Items[i] is TCESpTabSet then
      begin
        tabset:= TCESpTabSet(CELayoutItems.Items[i]);
        AppStorage.Path:= AppStorage.ConcatPaths([AppStorage.Path, tabset.Name]);
        try
          AppStorage.WriteBoolean('Visible', tabset.Visible);
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
  CEToolbarDocks:= TCEToolbarDocks.Create(false);
  CEDockStyle:= TJvDockVSNetStyleTBX.Create(nil);

finalization
  FreeAndNil(CEToolbarDocks);
  FreeAndNil(CELayoutItems);
  FreeAndNil(CEDockStyle);
  
end.
