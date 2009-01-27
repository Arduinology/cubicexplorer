unit CE_VideoPlayer;

interface

uses
  // CE Units
  CE_VideoEngine, CE_VideoPlayerUI,
  // TNT Controls
  TntSysUtils, TntClasses,
  // VSTools
  MPShellUtilities,
  // Graphics32
  GR32, GR32_Image, GR32_Layers, GR32_Resamplers,
  // System Units
  ExtCtrls, Windows, Messages, SysUtils, Controls, Classes, Graphics;

type

  TCEVideoPlayer = class(TPanel)
  private
    fCurrentFile: WideString;
    infoLayer: TBitmapLayer;
    thumbLayer: TBitmapLayer;
  protected
    procedure OnBackImageMouseDown(Sender: TObject; Button: TMouseButton; Shift:
        TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure OnFullscreenClick(Sender: TObject);
    procedure OnMutedChanged(Sender: TObject);
    procedure OnPauseClick(Sender: TObject);
    procedure OnPlayClick(Sender: TObject);
    procedure OnProgress(Sender: TObject);
    procedure OnProgressBarClicked(Sender: TObject; Position: Integer);
    procedure OnStopClick(Sender: TObject);
    procedure OnVideoPanelResize(Sender: TObject);
    procedure OnVolumeChanged(Sender: TObject);
    procedure ResizeThumbLayer;
  public
    BackImage: TImage32;
    DSEngine: TCEDSPlayer;
    Controller: TCEVideoPlayerController;
    VideoPanel: TPanel;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseFile;
    procedure DrawBackground;
    procedure OpenFile(FilePath: WideString);
    property CurrentFile: WideString read fCurrentFile write fCurrentFile;
  end;

implementation

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEVideoPlayer
-------------------------------------------------------------------------------}
constructor TCEVideoPlayer.Create(AOwner: TComponent);
begin
  inherited;
  Self.BevelInner:= bvNone;
  Self.BevelOuter:= bvNone;
  Self.BorderWidth:= 0;
  Self.Color:= clBlack;
  // Init Controller
  Controller:= TCEVideoPlayerController.Create(self);
  Controller.Parent:= self;
  Controller.Align:= alBottom;
  Controller.Height:= 50;
  Controller.Color:= clGray;
  Controller.OnPlayClick:= OnPlayClick;
  Controller.OnPauseClick:= OnPauseClick;
  Controller.OnStopClick:= OnStopClick;
  Controller.OnMutedChanged:= OnMutedChanged;
  Controller.OnVolumeChanged:= onVolumeChanged;
  Controller.OnProgressBarClick:= OnProgressBarClicked;
  Controller.OnFullscreenClick:= OnFullscreenClick;
  // Init VideoPanel
  VideoPanel:= TPanel.Create(self);
  VideoPanel.Parent:= self;
  VideoPanel.Align:= alClient;
  VideoPanel.BevelInner:= bvNone;
  VideoPanel.BevelOuter:= bvNone;
  VideoPanel.Color:= $333333;
  // Init BackImage
  BackImage:= TImage32.Create(self);
  BackImage.Parent:= self;
  BackImage.Align:= alClient;
  BackImage.Color:= $333333;
  BackImage.OnMouseDown:= OnBackImageMouseDown;
  thumbLayer:= TBitmapLayer.Create(BackImage.Layers);
  thumbLayer.Bitmap.MasterAlpha:= 150;
  thumbLayer.Bitmap.DrawMode:= dmBlend;
  thumbLayer.Bitmap.Resampler:= TLinearResampler.Create(thumbLayer.Bitmap);
  infoLayer:= TBitmapLayer.Create(BackImage.Layers);
  // Init DSEngine
  DSEngine:= TCEDSPlayer.Create(self);
  DSEngine.VideoPanel:= BackImage;
  DSEngine.OnProgress:= OnProgress;
  DSEngine.Loop:= true;
  BackImage.OnResize:= OnVideoPanelResize;
end;

{*------------------------------------------------------------------------------
  Destroy TCEVideoPlayer
-------------------------------------------------------------------------------}
destructor TCEVideoPlayer.Destroy;
begin
  DSEngine.ClearGraph;
  inherited;
end;

{*------------------------------------------------------------------------------
  Handle Fullscreen click
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnFullscreenClick(Sender: TObject);
begin
  DSEngine.Fullscreen:= true;
end;

{*------------------------------------------------------------------------------
  Handle Muted changed
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnMutedChanged(Sender: TObject);
begin
  if Controller.Muted then
  DSEngine.Volume:= 0
  else
  DSEngine.Volume:= Controller.Volume;
end;

{*------------------------------------------------------------------------------
  Handle Pause click
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnPauseClick(Sender: TObject);
begin
  DSEngine.Pause;
end;

{*------------------------------------------------------------------------------
  Handle Play click
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnPlayClick(Sender: TObject);
begin
  if Controller.Muted then
  DSEngine.Volume:= 0
  else
  DSEngine.Volume:= Controller.Volume;
  
  if DSEngine.IsLoaded then
  DSEngine.Play
  else       
  DSEngine.OpenFile(fCurrentFile)
end;

{*------------------------------------------------------------------------------
  Get's called on video progress
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnProgress(Sender: TObject);
begin
  if DSEngine.IsLoaded then
  begin
    if infoLayer.Visible and DSEngine.HasVideo then
    begin
      infoLayer.Visible:= false;
      thumbLayer.Visible:= false;
      BackImage.Color:= clBlack;
      BackImage.Bitmap.SetSize(0,0);
    end;
    Controller.TimeString:= DSEngine.PositionString + ' / ' + DSEngine.DurationString;
    Controller.MaxPosition:= DSEngine.Duration;
    Controller.Position:= DSEngine.Position;
  end
  else
  begin
    Controller.TimeString:= '';
    Controller.Position:= 0;
    if not infoLayer.Visible then
    begin
      infoLayer.Visible:= true;
      thumbLayer.Visible:= true;
      BackImage.Color:= $333333;
    end;
  end;

  if DSEngine.IsPlaying then
  Controller.Status:= vpsPlaying
  else if DSEngine.IsPaused then
  Controller.Status:= vpsPaused
  else if not DSEngine.IsLoaded then
  Controller.Status:= vpsStopped;

  Controller.Draw;
end;

{*------------------------------------------------------------------------------
  Handle ProgressBar click
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnProgressBarClicked(Sender: TObject; Position:
    Integer);
begin
  Caption:= IntTostr(Position);
  Controller.Position:= Position;
  Controller.Draw;
  DSEngine.Position:= Position;
end;

{*------------------------------------------------------------------------------
  Handle Stop click
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnStopClick(Sender: TObject);
begin
  DSEngine.ClearGraph;
end;

{*------------------------------------------------------------------------------
  Handle Volume changed
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnVolumeChanged(Sender: TObject);
begin
  DSEngine.Volume:= Controller.Volume;
end;

{*------------------------------------------------------------------------------
  Resize video
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnVideoPanelResize(Sender: TObject);
begin
  DSEngine.ResizeVideo;
  ResizeThumbLayer;
end;

{*------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OpenFile(FilePath: WideString);
begin
  if fCurrentFile = FilePath then
  Exit;
  if not WideFileExists(FilePath) then
  Exit;
  
  DSEngine.ClearGraph;
  fCurrentFile:= FilePath;
  Controller.PlayingTitle:= WideExtractFileName(FilePath);
  Controller.TimeString:= '';
  Controller.Draw;

  DrawBackground;
end;

{*------------------------------------------------------------------------------
  CloseFile
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.CloseFile;
begin
  DSEngine.ClearGraph;
  fCurrentFile:= '';
  Controller.PlayingTitle:= '';
  Controller.TimeString:= '';
  Controller.Draw;
  DrawBackground;
end;

{*------------------------------------------------------------------------------
  Draw Background
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.DrawBackground;
var
  NS: TNamespace;
  bitmap: TBitmap;
  ws: WideString;
  list: TTntStrings;
  i, tmpI: Integer;
  w: Integer;
  Bits: HBITMAP;
begin
  if not FileExists(fCurrentFile) then
  begin
    BackImage.BeginUpdate;
    infoLayer.Bitmap.SetSize(0,0);
    thumbLayer.Bitmap.SetSize(0,0);
    BackImage.EndUpdate;
    BackImage.Changed;
    Exit;
  end;

  BackImage.BeginUpdate;

  NS:= TNamespace.CreateFromFileName(fCurrentFile);
  try
    // Draw Image
    if Assigned(NS.ExtractImage.ExtractImageInterface) then
    begin
      NS.ExtractImage.ColorDepth:= 32;
      NS.ExtractImage.Width:= 200;
      NS.ExtractImage.Height:= 200;
      NS.ExtractImage.ImagePath;
      if Succeeded(NS.ExtractImage.ExtractImageInterface.Extract(Bits)) then
      begin
        Bitmap:= TBitmap.Create;
        try
          Bitmap.PixelFormat := pf32Bit;
          Bitmap.Handle := Bits;
          thumbLayer.Bitmap.SetSizeFrom(Bitmap);
          thumbLayer.Bitmap.Clear($FF333333);
          thumbLayer.Bitmap.Canvas.Draw(0,0,Bitmap);
          thumbLayer.Bitmap.ResetAlpha;
          ResizeThumbLayer;
        finally
          Bitmap.Free;
        end;
      end
      else
        thumbLayer.Bitmap.SetSize(0,0);
    end
    else
    begin
      thumbLayer.Bitmap.SetSize(0,0);
    end;
    // Draw Info
    list:= TTntStringList.Create;
    infoLayer.Bitmap.BeginUpdate;
    try
      infoLayer.Bitmap.Font.Name:= 'Arial';
      infoLayer.Bitmap.Font.Size:= 8;
      ws:= NS.InfoTip;
      list.Text:= ws;
      w:= 0;
      for i:= 0 to list.Count - 1 do
      begin
        tmpI:= infoLayer.Bitmap.TextWidthW(list.Strings[i]);
        if tmpI > w then
        w:= tmpI;
      end;
      infoLayer.Bitmap.Font.Size:= 10;
      ws:= WideExtractFileName(NS.NameForParsing);
      tmpI:= infoLayer.Bitmap.TextWidthW(ws);
      if tmpI > w then
      w:= tmpI;

      infoLayer.Bitmap.Width:= w + 20;
      infoLayer.Bitmap.Height:= 20 + (list.Count * 13);
      infoLayer.Bitmap.Clear($aaFFFFFF);

      
      infoLayer.Bitmap.RenderTextW(4,2, ws,3,clBlack32);

      infoLayer.Bitmap.Font.Size:= 8;
      for i:= 0 to list.Count - 1 do
      infoLayer.Bitmap.RenderTextW(4,20 + (i*13),list.Strings[i],3,clBlack32);
      infoLayer.Bitmap.DrawMode:= dmBlend;

      infoLayer.Bitmap.FrameRectS(infoLayer.Bitmap.BoundsRect, clBlack32);
      infoLayer.Location:= FloatRect(5,
                                     5,
                                     infoLayer.Bitmap.Width + 5,
                                     infoLayer.Bitmap.Height + 5);
    finally
      infoLayer.Bitmap.EndUpdate;
      list.Free;
    end;
  finally
    NS.Free;
    BackImage.EndUpdate;
    BackImage.Changed;
  end;
end;

{*------------------------------------------------------------------------------
  Resize ThumbLayer
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.ResizeThumbLayer;
var
  f: TFloatRect;
  ar, ar2: Real;
  s: Real;
begin
  if (thumbLayer.Bitmap.Width = 0) or (thumbLayer.Bitmap.Height = 0) then
  Exit;

  ar:= thumbLayer.Bitmap.Width / thumbLayer.Bitmap.Height;
  ar2:= BackImage.Width / BackImage.Height;
  if ar >= ar2 then
  begin
    f.Left:= 0;
    f.Right:= BackImage.Width;
    s:= BackImage.Width / ar;
    f.Top:= (BackImage.Height - s) / 2;
    f.Bottom:= f.Top + s
  end
  else
  begin
    f.Top:= 0;
    f.Bottom:= BackImage.Height;
    s:= BackImage.Height * ar;
    f.Left:= (BackImage.Width - s) / 2;
    f.Right:= f.Left + s
  end;
  thumbLayer.Location:= f;
end;

{*------------------------------------------------------------------------------
  OnBackImageMouseDown
-------------------------------------------------------------------------------}
procedure TCEVideoPlayer.OnBackImageMouseDown(Sender: TObject; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if not DSEngine.IsLoaded then
  begin
    if Shift = [ssLeft] then
    OnPlayClick(self);
  end;
end;


end.
