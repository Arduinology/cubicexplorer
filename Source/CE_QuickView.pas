unit CE_QuickView;

interface

uses
  // CE Units
  CE_Consts, CE_Utils, CE_GifAnim, CE_VideoPlayer, CE_VistaFuncs,
  CE_LanguageEngine, CE_SettingsIntf,
  // Tnt Controls
  TntStdCtrls, TntSysUtils, TntClasses,
  // GraphicEx
  GraphicEx,
  //Graphics32
  GR32, GR32_Image, GR32_Resamplers, GR32_RangeBars,
  // JVCL
  //JvGIFCtrl, JvSimpleXML, JvAppStorage,
  // VSTools
  VirtualThumbnails,
  // System Units
  Windows, Classes, Controls, SysUtils, ExtCtrls, Forms, Graphics, StdCtrls;

type
  TCEQuickViewType = (qvNone, qvAuto, qvMemo, qvImage, qvHex, qvVideo);

  TCEQuickView = class(TPanel, ICESettingsHandler)
  private
    fActive: Boolean;
    fFilePath: WideString;
    fMemo: TTntMemo;
    fImage: TImgView32;
    fViewType: TCEQuickViewType;
    fGifImage: TCEGifImage;
    fMuted: Boolean;
    fVideoPlayer: TCEVideoPlayer;
    fUseThumbImage: Boolean;
    fVolume: Byte;
    procedure DoMemoKeyPress(Sender: TObject; var Key: Char);
    procedure SetActive(const Value: Boolean);
    procedure SetViewType(const Value: TCEQuickViewType);
  protected
    procedure LoadFromStorage(Storage: ICESettingsStorage); stdcall;
    procedure SaveToStorage(Storage: ICESettingsStorage); stdcall;
  public
    MemoExts: TStrings;
    ImageExts: TStrings;
    HexExts: TStrings;
    VideoExts: TStrings;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AutoLoadFile(AFilePath: WideString): TCEQuickViewType;
    function CloseFile: Boolean;
    function GetViewType(Ext: String): TCEQuickViewType;
    procedure LoadFile(AFilePath: WideString);
    procedure LoadFileToMemo(AFilePath: WideString);
    procedure LoadFileToImage(AFilePath: WideString);
    procedure LoadFileToHexView(AFilePath: WideString);
    procedure LoadFileToGifImage(AFilePath: WideString);
    procedure LoadFileToVideoPlayer(AFilePath: WideString);
    property Active: Boolean read fActive write SetActive;
    property FilePath: WideString read fFilePath;
    property Muted: Boolean read fMuted write fMuted;
    property UseThumbImage: Boolean read fUseThumbImage write fUseThumbImage;
    property ViewType: TCEQuickViewType read fViewType write SetViewType;
    property Volume: Byte read fVolume write fVolume;
  end;

const
  DefaultMemoExts = 'txt'#13'ini'#13'bat'#13'html'#13'htm'#13'pas'#13'css'#13'xml'#13'log'#13'for'#13'php'#13'py';
  DefaultHexExts = '';
  DefaultVideoExts = 'avi'#13'wmv'#13'mp4'#13'mpg'#13'mpeg'#13'ogg'#13'ogm'#13'mkv'#13'dvr-ms'#13'mp3'#13'vob'#13'wav';

implementation

{*------------------------------------------------------------------------------
  Create instance of Object
-------------------------------------------------------------------------------}
constructor TCEQuickView.Create(AOwner: TComponent);
begin
  inherited;
  SetVistaFont(Font);
  Caption:= _('No file loaded.');
  Color:= clWindow;
  fViewType:= qvAuto;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  MemoExts:= TStringList.Create;
  MemoExts.Delimiter:= ',';
  MemoExts.Text:= DefaultMemoExts;

  ImageExts:= TStringList.Create;
  ImageExts.Delimiter:= ',';
  GraphicEx.FileFormatList.GetExtensionList(ImageExts);

  HexExts:= TStringList.Create;
  HexExts.Delimiter:= ',';
  HexExts.Text:= DefaultHexExts;

  VideoExts:= TStringList.Create;
  VideoExts.Delimiter:= ',';
  VideoExts.Text:= DefaultVideoExts;

  fActive:= false;
end;

{*------------------------------------------------------------------------------
  Destroy object.
-------------------------------------------------------------------------------}
destructor TCEQuickView.Destroy;
begin
  CloseFile;
  MemoExts.Free;
  ImageExts.Free;
  HexExts.Free;
  VideoExts.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Choose view type based on EXT and load file.
-------------------------------------------------------------------------------}
function TCEQuickView.AutoLoadFile(AFilePath: WideString): TCEQuickViewType;
var
  ext: String;
begin
  ext:= ExtractFileExt(AFilePath);
  Result:= GetViewType(ext);
  case Result of
    qvNone: CloseFile;
    qvMemo: LoadFileToMemo(AFilePath);
    qvImage: begin
               if (ext = '.gif') or (ext = '.GIF') then
               LoadFileToGifImage(AFilePath)
               else
               LoadFileToImage(AFilePath);
             end;
    qvHex: LoadFileToHexView(AFilePath);
    qvVideo: LoadFileToVideoPlayer(AFilePath);
  end;
end;

{*------------------------------------------------------------------------------
  Load File
-------------------------------------------------------------------------------}
procedure TCEQuickView.LoadFile(AFilePath: WideString);
var
  ext: String;
begin
  fFilePath:= AFilePath;

  if not fActive then
  Exit;
  
  fFilePath:= '';

  if not WideFileExists(AFilePath) then
  begin
    CloseFile;
    Exit;
  end;

  fFilePath:= AFilePath;
  case fViewType of
    qvNone: CloseFile;
    qvAuto: AutoLoadFile(AFilePath);
    qvMemo: LoadFileToMemo(AFilePath);
    qvImage: begin
               ext:= ExtractFileExt(AFilePath);
               if (ext = '.gif') or (ext = '.GIF') then
               LoadFileToGifImage(AFilePath)
               else
               LoadFileToImage(AFilePath);
             end;
    qvHex: LoadFileToHexView(AFilePath);
    qvVideo: LoadFileToVideoPlayer(AFilePath); 
  end;
end;

{*------------------------------------------------------------------------------
  Load file to Memo
-------------------------------------------------------------------------------}
procedure TCEQuickView.LoadFileToMemo(AFilePath: WideString);
var
  Stream: TStream;
begin
  if not CloseFile then Exit;
  Caption:= _('Loading...');
  Repaint;

  try
    Stream := TTntFileStream.Create(AFilePath, fmOpenReadWrite or fmShareDenyNone);
  except
    on E: Exception do
    begin
      Caption:= E.Message;
      Exit;
    end;
  end;

  try
    fMemo:= TTntMemo.Create(nil);
    fMemo.Visible:= false;
    fMemo.Parent:= self;
    fMemo.OnKeyPress:= DoMemoKeyPress;
    fMemo.BorderStyle:= bsNone;
    fMemo.BevelInner:= bvNone;
    fMemo.BevelOuter:= bvNone;
    fMemo.ScrollBars:= ssBoth;
    fMemo.WordWrap:= false;
    fMemo.Align:= alClient;
    Stream.Position := 0;
    fMemo.Lines.LoadFromStream(Stream);
    fMemo.Visible:= true;
    Caption:= '';
  finally
    Stream.Free;
  end;
end;

{*------------------------------------------------------------------------------
  Load file to Image
-------------------------------------------------------------------------------}
procedure TCEQuickView.LoadFileToImage(AFilePath: WideString);

  function HasAlpha(bitmap: TBitmap32): Boolean;
  var
    y,x: Integer;
    row: PColor32Array;
  begin
    Result:= false;
    for y:= 0 to bitmap.Height - 1 do
    begin
      row:= bitmap.ScanLine[y];
      for x:= 0 to bitmap.Width - 1 do
      begin
        if ((row[x] shr 24) > 0) then
        begin
          Result:= true;
          exit;
        end;
      end;
    end;
  end;

var
  b: TBitmap;
  W,H: Integer;
  ext: WideString;
  png: TPNGGraphic;
  s: TStream;
begin
  if not CloseFile then Exit;
  fImage:= TImgView32.Create(nil);
  fImage.Color:= clWindow;
  fImage.Visible:= false;
  fImage.Parent:= self;
  fImage.ScrollBars.Visibility:= svAuto;
  fImage.ScrollBars.Style:= rbsMac;
  fImage.ScaleMode:= smOptimalScaled;
  fImage.Bitmap.Resampler:= TLinearResampler.Create(fImage.Bitmap);
  //fImage.Bitmap.DrawMode:= dmBlend;
  fImage.Align:= alClient;
  Caption:= _('Loading...');
  Repaint;

  fImage.Bitmap.BeginUpdate;
  try
    ext:= WideExtractFileExt(AFilePath);
    if (ext = '.png') or (ext = '.PNG') then
    begin
      png:= TPNGGraphic.Create;
      s:= TTntFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
      try
        s.Position:= 0;
        png.LoadFromStream(s);
        if png.ImageProperties.HasAlpha then
        fImage.Bitmap.DrawMode:= dmBlend
        else
        fImage.Bitmap.DrawMode:= dmOpaque;
        fImage.Bitmap.Assign(png);
      finally
        png.Free;
        s.Free;
      end;
    end
    else
    begin
      if fUseThumbImage then
      begin
        B:= TBitmap.Create;
        B.Canvas.Lock;
        SpMakeThumbFromFile(AFilePath,
                            B,
                            Width,
                            Height,
                            clWhite,
                            false,
                            false,
                            false,
                            W,
                            H);
        B.Canvas.Unlock;
        fImage.Bitmap.SetSize(B.Width,B.Height);
        fImage.Bitmap.Clear($00000000);
        fImage.Bitmap.Draw(fImage.Bitmap.BoundsRect,fImage.Bitmap.BoundsRect,B.Canvas.Handle);
        B.Free;
      end
      else
      begin
        fImage.Bitmap.LoadFromFile(AFilePath);
      end;
    end;
  except
    fImage.Bitmap.SetSize(150,40);
    fImage.Bitmap.Clear(clWhite32);
    fImage.Bitmap.Textout(fImage.Bitmap.BoundsRect, DT_CENTER+DT_VCENTER+DT_SINGLELINE, 'Format not supported');
  end;

  fImage.Bitmap.EndUpdate;

  fImage.Visible:= true;
  Caption:= '';
end;

{*------------------------------------------------------------------------------
  Load file to HexViewer
-------------------------------------------------------------------------------}
procedure TCEQuickView.LoadFileToHexView(AFilePath: WideString);
begin
//  if not CloseFile then Exit;
//  fHexView:= TATBinHex.Create(nil);
//  fHexView.Visible:= false;
//  fHexView.Parent:= self;
//  fHexView.Align:= alClient;
//  fHexView.BorderStyle:= bsNone;
//  fHexView.Mode:= vbmodeHex;
//  Caption:= _('Loading...');
//  Repaint;
//  try
//    fHexView.Open(AFilePath);
//  finally
//  end;
//  fHexView.Visible:= true;
//  Caption:= '';
end;

{*------------------------------------------------------------------------------
  Close file.
-------------------------------------------------------------------------------}
function TCEQuickView.CloseFile: Boolean;
begin
  Result:= false;
  if assigned(fMemo) then
  FreeAndNil(fMemo);
  if assigned(fImage) then
  FreeAndNil(fImage);
//  if assigned(fHexView) then
//  FreeAndNil(fHexView);
  if assigned(fGifImage) then
  FreeAndNil(fGifImage);
  if assigned(fVideoPlayer) then
  begin
    if fVideoPlayer.DSEngine.IsLoaded then
    Exit;
    Volume:= fVideoPlayer.Controller.Volume;
    Muted:= fVideoPlayer.Controller.Muted;
    FreeAndNil(fVideoPlayer);
  end;
  Caption:= _('No file loaded.');
  Result:= true;
end;

{*------------------------------------------------------------------------------
  Get View type from Extension
-------------------------------------------------------------------------------}
function TCEQuickView.GetViewType(Ext: String): TCEQuickViewType;
var
  s: String;
begin
  Result:= qvNone;
  if Length(Ext) > 0 then
  begin
    if Ext[1] = '.' then
    s:= Copy(Ext,2,Length(Ext));
  end;
  if ImageExts.IndexOf(s) > -1 then
  begin
    Result:= qvImage;
  end
  else if MemoExts.IndexOf(s) > -1 then
  begin
    Result:= qvMemo;
  end
  else if VideoExts.IndexOf(s) > -1 then
  begin
    Result:= qvVideo;
  end
  else if HexExts.IndexOf(s) > -1 then
  begin
    Result:= qvHex;
  end;
end;

{*------------------------------------------------------------------------------
  Load file to Gif Image
-------------------------------------------------------------------------------}
procedure TCEQuickView.LoadFileToGifImage(AFilePath: WideString);
begin
  if not CloseFile then Exit;
  fGifImage:= TCEGifImage.Create(nil);
  fGifImage.Visible:= false;
  fGifImage.Parent:= self;
  fGifImage.Color:= clWindow;
  fGifImage.BitmapAlign:= baCenter;
  fGifImage.Bitmap.Resampler:= TLinearResampler.Create(fGifImage.Bitmap);
  fGifImage.ScaleMode:= smOptimal;
  fGifImage.Align:= alClient;
  Caption:= _('Loading...');
  Repaint;
  try
    if fGifImage.LoadFromFile(AFilePath) then
    fGifImage.Animate:= true;
  except
    fGifImage.Bitmap.SetSize(150,40);
    fGifImage.Bitmap.Clear(clWhite32);
    fGifImage.Bitmap.Textout(fGifImage.Bitmap.BoundsRect, DT_CENTER+DT_VCENTER+DT_SINGLELINE, 'Format not supported');
  end;
  fGifImage.Visible:= true;
  Caption:= '';
end;

{*------------------------------------------------------------------------------
  Load file to VideoPlayer
-------------------------------------------------------------------------------}
procedure TCEQuickView.LoadFileToVideoPlayer(AFilePath: WideString);
begin
  if assigned(fVideoPlayer) then
  begin
    if not fVideoPlayer.DSEngine.IsLoaded then
    fVideoPlayer.OpenFile(AFilePath);
    Exit;
  end
  else if CloseFile then
  begin
    fVideoPlayer:= TCEVideoPlayer.Create(nil);
    fVideoPlayer.Parent:= self;
    fVideoPlayer.Align:= alClient;
    fVideoPlayer.Controller.Volume:= Volume;
    fVideoPlayer.Controller.Muted:= Muted;
    fVideoPlayer.OpenFile(AFilePath);
  end;
end;

{*------------------------------------------------------------------------------
  Set view type.
-------------------------------------------------------------------------------}
procedure TCEQuickView.SetViewType(const Value: TCEQuickViewType);
begin
  if Value = fViewType then
  Exit;

  fViewType:= Value;
  LoadFile(fFilePath);
end;

{*------------------------------------------------------------------------------
  Set Active value.
-------------------------------------------------------------------------------}
procedure TCEQuickView.SetActive(const Value: Boolean);
begin
  if fActive = Value then
  Exit;
  fActive:= Value;

  if not Value then
  CloseFile
  else
  begin
    LoadFile(FilePath);
  end;
end;

{*------------------------------------------------------------------------------
  Handle memo key press
-------------------------------------------------------------------------------}
procedure TCEQuickView.DoMemoKeyPress(Sender: TObject; var Key: Char);
begin
  if assigned(fMemo) then
  begin
    if Ord(Key) = 1 then
    begin
      Key:= #0;
      fMemo.SelectAll;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Load from storage
-------------------------------------------------------------------------------}
procedure TCEQuickView.LoadFromStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('QuickView');
  try
    fVolume:= Storage.ReadInteger('Volume',200);
    fMuted:= Storage.ReadBoolean('Muted',false);
  finally
    Storage.ClosePath;
  end;
end;

{-------------------------------------------------------------------------------
  Save to storage
-------------------------------------------------------------------------------}
procedure TCEQuickView.SaveToStorage(Storage: ICESettingsStorage);
begin
  Storage.OpenPath('QuickView');
  try
    if assigned(fVideoPlayer) then
    begin
      Volume:= fVideoPlayer.Controller.Volume;
      Muted:= fVideoPlayer.Controller.Muted;
    end;
    Storage.WriteInteger('Volume',fVolume);
    Storage.WriteBoolean('Muted',fMuted);
  finally
    Storage.ClosePath;
  end;
end;

end.
