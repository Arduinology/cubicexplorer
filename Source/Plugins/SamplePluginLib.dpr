library SamplePluginLib;

uses
  CE_PluginsIntf,
  SamplePlugin in 'SamplePlugin.pas',
  fSamplePlugin_Settings in 'fSamplePlugin_Settings.pas' {SamplePluginSettings_form: TFrame};

{-------------------------------------------------------------------------------
  CreatePluginFactory
-------------------------------------------------------------------------------}
function CreatePluginFactory(AInterfaceVersion: Integer; out APluginFactory: ICEPluginFactory): Boolean;
    stdcall;
begin
  Result:= false;
  if AInterfaceVersion = CE_PluginInterfaceVersion then
  begin
    try
      APluginFactory:= TSamplePluginFactory.Create;
      Result:= assigned(APluginFactory);
    except
      APluginFactory:= nil;
    end;
  end;
end;


{$R *.res}

exports CreatePluginFactory;

begin
end.
