unit fCE_OptionsPage_Plugins;

interface

uses
  // CC
  ccStrings, ccFileUtils,
  // CE
  CE_LanguageEngine, fCE_OptionsCustomPage, fCE_OptionsDialog, dCE_Images,
  CE_Plugins, CE_PluginsIntf, fCE_PluginSettingsForm,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SpTBXTabs, TB2Item, SpTBXItem, ComCtrls, TntComCtrls, VirtualTrees,
  SpTBXControls, ExtCtrls, TntExtCtrls, StdCtrls, TntStdCtrls, ImagingComponents;

type
  PPlugData = ^APlugData;
  APlugData = record
    isPluginNode: Boolean;
    PluginType: Integer;
    Name: WideString;
    Data: TCEPluginData;
  end;

  TCEOptionsPage_Plugins = class(TCEOptionsCustomPage)
    TntPageControl1: TTntPageControl;
    sheet_libraries: TTntTabSheet;
    sheet_formats: TTntTabSheet;
    tree_libraries: TVirtualStringTree;
    label_library_name: TSpTBXLabel;
    TntTabSheet1: TTntTabSheet;
    tree_plugins: TVirtualStringTree;
    label_plugin_name: TSpTBXLabel;
    VirtualStringTree2: TVirtualStringTree;
    but_plugin_settings: TTntButton;
    memo_plugin_desc: TTntMemo;
    label_plugin_version: TTntLabel;
    Image1: TImage;
    procedure tree_plug_FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tree_plug_GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure tree_plug_GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tree_plugPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure tree_pluginsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure but_plugin_settingsClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure PopulateLibrariesTree;
    procedure PopulatePluginsTree;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplySettings; override;
    procedure RefreshSettings; override;
    { Public declarations }
  end;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------------
  Create an instance of TCEOptionsPage_Plugins
-------------------------------------------------------------------------------}
constructor TCEOptionsPage_Plugins.Create(AOwner: TComponent);
begin
  inherited;
  // init options page
  PageName:= _('Plugins');
  PageTitle:= _('Plugins');
  PagePath:= 'Plugins';
  ImageIndex:= 5;
  PageListPosition:= 4;

  // init values
  tree_libraries.NodeDataSize:= SizeOf(APlugData);
  tree_plugins.NodeDataSize:= SizeOf(APlugData);
end;

{-------------------------------------------------------------------------------
  Apply Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.ApplySettings;
begin
  // 
end;

{-------------------------------------------------------------------------------
  Refresh Settings
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.RefreshSettings;
begin
  PopulatePluginsTree;
  PopulateLibrariesTree;
end;

{-------------------------------------------------------------------------------
  PopulateLibrariesTree
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.PopulateLibrariesTree;

  function FindNodeByPath(const APath: WideString): PVirtualNode;
  var
    d: PPlugData;
  begin
    Result:= tree_libraries.GetFirst;
    while assigned(Result) do
    begin
      d:= tree_libraries.GetNodeData(Result);
      if WideIsSameText(APath, d.Data.PluginPath) then
      Exit;
      Result:= tree_libraries.GetNextSibling(Result);
    end;
    Result:= nil;
  end;

var
  i: Integer;
  parentNode, node: PVirtualNode;
  plugin: TCEPluginData;
  data: PPlugData;
begin
  tree_libraries.BeginUpdate;
  try
    tree_libraries.Clear;

    for i:= 0 to GlobalPluginHost.GetPluginDataCount-1 do
    begin
      if GlobalPluginHost.GetPluginData(i, plugin) then
      begin
        // get library node
        parentNode:= FindNodeByPath(plugin.PluginPath);
        if not assigned(parentNode) then
        begin
          parentNode:= tree_libraries.AddChild(nil);
          parentNode.CheckType:= ctCheckBox;
          data:= tree_libraries.GetNodeData(parentNode);
          data.isPluginNode:= false;
          data.Name:= WideExtractFileName(plugin.PluginPath);
          data.Data.PluginPath:= plugin.PluginPath;
        end;

        // add plugin node
        node:= tree_libraries.AddChild(parentNode);
        node.CheckType:= ctCheckBox;
        data:= tree_libraries.GetNodeData(node);
        data.isPluginNode:= true;
        data.Name:= plugin.Info.GetPluginName;
        data.Data:= plugin;
      end;
    end;

    tree_libraries.FullExpand;
  finally
    tree_libraries.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  PopulatePluginsTree
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.PopulatePluginsTree;

  function FindNodeByType(APluginType: Integer): PVirtualNode;
  var
    d: PPlugData;
  begin
    Result:= tree_plugins.GetFirst;
    while assigned(Result) do
    begin
      d:= tree_plugins.GetNodeData(Result);
      if APluginType =  d.PluginType then
      Exit;
      Result:= tree_plugins.GetNextSibling(Result);
    end;
    Result:= nil;
  end;

var
  i: Integer;
  parentNode: PVirtualNode;
  plugin: TCEPluginData;
  data: PPlugData;
begin
  tree_plugins.BeginUpdate;
  try
    tree_plugins.Clear;

    for i:= 0 to GlobalPluginHost.GetPluginDataCount-1 do
    begin
      if GlobalPluginHost.GetPluginData(i, plugin) then
      begin
        // get parent node
        parentNode:= FindNodeByType(plugin.Info.GetPluginType);
        // create parent node if needed
        if not assigned(parentNode) then
        begin
          parentNode:= tree_plugins.AddChild(nil);
          data:= tree_plugins.GetNodeData(parentNode);
          data.isPluginNode:= false;
          data.PluginType:= plugin.Info.GetPluginType;
          data.Name:= PluginTypeToStr(data.PluginType);
          data.Data.PluginPath:= '';
        end;

        // add plugin node
        data:= tree_plugins.GetNodeData(tree_plugins.AddChild(parentNode));
        data.isPluginNode:= true;
        data.PluginType:= plugin.Info.GetPluginType;
        data.Name:= plugin.Info.GetPluginName;
        data.Data:= plugin;
      end;
    end;

    tree_plugins.FullExpand;
  finally
    tree_plugins.EndUpdate;
  end;
end;

{-------------------------------------------------------------------------------
  On tree_*.FreeNode
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.tree_plug_FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  data: PPlugData;
begin
  data:= Sender.GetNodeData(Node);
  data.Name:= '';
  data.Data.Info:= nil;
  data.Data.Factory:= nil;
  data.Data.Settings:= nil;
  data.Data.PluginPath:= '';
end;

{-------------------------------------------------------------------------------
  On tree_*.PaintText
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.tree_plugPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  data: PPlugData;
begin
  data:= Sender.GetNodeData(Node);
  if not data.isPluginNode then
  TargetCanvas.Font.Style:= [fsBold]
  else
  TargetCanvas.Font.Style:= [];
end;

{-------------------------------------------------------------------------------
  On tree_*.GetHint
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.tree_plug_GetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
var
  data: PPlugData;
begin
  data:= Sender.GetNodeData(Node);
  if not data.isPluginNode then
  HintText:= data.Data.PluginPath
  else if assigned(data.Data.Info) then
  HintText:= data.Data.Info.GetPluginDescription;
end;

{-------------------------------------------------------------------------------
  On tree_*.GetText
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.tree_plug_GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  data: PPlugData;
begin
  data:= Sender.GetNodeData(Node);
  CellText:= data.Name;
end;

{-------------------------------------------------------------------------------
  On tree_plugins.FocusChanged
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.tree_pluginsFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  data: PPlugData;
begin
  data:= Sender.GetNodeData(Node);
  if assigned(data) and data.isPluginNode  then
  begin
    label_plugin_name.Caption:= data.Name;
    memo_plugin_desc.Text:= data.Data.Info.GetPluginDescription;
    label_plugin_version.Caption:= 'Version: ' +  IntToStr(data.Data.Info.GetPluginVersion) + '  ';
    label_plugin_name.Visible:= true;
    label_plugin_version.Visible:= true;
    memo_plugin_desc.Visible:= true;
    but_plugin_settings.Visible:= true;
  end
  else
  begin
    label_plugin_name.Visible:= false;
    label_plugin_version.Visible:= false;
    memo_plugin_desc.Visible:= false;
    but_plugin_settings.Visible:= false;
  end;
end;

{-------------------------------------------------------------------------------
  On but_plugin_settings.Click
-------------------------------------------------------------------------------}
procedure TCEOptionsPage_Plugins.but_plugin_settingsClick(Sender: TObject);
var
  data: PPlugData;
begin
  data:= tree_plugins.GetNodeData(tree_plugins.FocusedNode);
  if assigned(data) then
  begin
    ShowPluginSettings(GlobalPluginHost, data.Data.Info.GetPluginID);
  end;
end;

{##############################################################################}

initialization
  RegisterOptionsPageClass(TCEOptionsPage_Plugins);

end.
