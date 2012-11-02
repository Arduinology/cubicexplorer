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
//  The Original Code is CV_MediaPlayerEngines.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CV_MediaPlayerEngines;

{==============================================================================}
interface

uses
  // CubicCore
  ccTypes, ccFileUtils, ccClasses, ccStrings,
  // CubicViewer
  CV_MediaPlayer, CV_ImageView,
  // CubicExplorer
  fCE_TextEditor,
  // DirectShow
  DirectShow9,
  // WMP
  WMPLib_TLB,
  // GraphicEx
  GraphicEx,
  // TNT
  TntRegistry,
  // System Units
  ActiveX, SysUtils, Types, Windows, ExtCtrls, Controls, Messages, Classes,
  StdCtrls, fCV_SumatraPDF;

{==============================================================================}
const
  ID_CVDSEngine: TGUID          = '{31617FF4-C7B6-4FBD-A3AF-961FCE4954B3}';
  ID_CVWmpEngine: TGUID         = '{B41DD245-09F2-4DA8-AB8D-47751BA82D03}';
  ID_CVImageEngine: TGUID       = '{621F4159-A3C0-4A88-B257-43E296E8BFA6}';
  ID_CVTextEngine: TGUID        = '{4C7CB4CE-0761-4A87-8C16-FEDFFF24CBC2}';
  ID_CVPDFEngine: TGUID         = '{72D40CD7-D19A-41F1-A71A-DC4F4F1C3C0A}';

type

{-------------------------------------------------------------------------------
  TCVDSEngine (DirectShow engine)
-------------------------------------------------------------------------------}
  TCVDSEngine = class(TCVCustomMediaEngine, ICVMediaEngineControl,
      ICVMediaEngineSeeking, ICVMediaEngineVideo, ICVMediaEngineAudio)
  protected
    Graph: IGraphBuilder;
    MediaControl: IMediaControl;
    VWin: IVideoWindow;
    BasicVideo: IBasicVideo;
    BasicAudio: IBasicAudio;
    fErrorMsg: WideString;
    fMessageHandle: HWND;
    fTitle: WideString;
    fVideoRect: TRect;
    fVideoSize: TPoint;
    fVolume: Integer;
    MediaSeeking: IMediaSeeking;
    MediaEventEx: IMediaEventEx;
    procedure ClearGraph;
    procedure InitGraph;
    procedure ResizeVideo; virtual;
    procedure UpdateTitle; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close; override; stdcall;
    function GetDuration: Int64; virtual; stdcall;
    // GetID
    // - Return unique TGuid
    function GetID: TGUID; override; stdcall;
    function GetPosition: Int64; virtual; stdcall;
    function GetStatus: TCVMediaPlayerStatus; override; stdcall;
    function GetStatusText: WideString; override; stdcall;
    // GetSupportedExtensions
    // - Return comma separated list of file extensions that are supported.
    // - Example: jpg,gif,avi
    // - Use * to support all extensions.
    function GetSupportedExtensions: WideString; override; stdcall;
    // GetTitle
    // - Return the title of currently loaded media
    function GetTitle: WideString; override; stdcall;
    // GetVideoSize
    // - Should return the video size (actual not resized).
    function GetVideoSize: TPoint; virtual; stdcall;
    // GetVolume
    // - Range should be from 0 to 100.
    function GetVolume: Integer; virtual; stdcall;
    // HasAudio
    // - Return true if currently loaded file has audio
    function HasAudio: Boolean; virtual; stdcall;
    // HasControl
    // - Return true if currently loaded file has audio
    function HasControl: Boolean; virtual; stdcall;
    // HasSeeking
    // - Return true if currently loaded file supports seeking
    function HasSeeking: Boolean; virtual; stdcall;
    // HasVideo
    // - Return true if currently loaded file has video
    function HasVideo: Boolean; virtual; stdcall;
    function OpenFile(AFilePath: WideString): Boolean; override; stdcall;
    procedure Pause; virtual; stdcall;
    procedure Play; virtual; stdcall;
    procedure SetBounds(ARect: TRect); override; stdcall;
    procedure SetParentWindow(AParentWindow: HWND); override; stdcall;
    // SetPosition
    // - Set position in milliseconds
    procedure SetPosition(APosition: Int64); virtual; stdcall;
    // SetVolume
    // - Range should be from 0 to 100.
    procedure SetVolume(AVolume: Integer); virtual; stdcall;
    procedure Stop; virtual; stdcall;
    procedure WinProc(var msg: TMessage);
  end;

{-------------------------------------------------------------------------------
  TCVWMPEngine (WMP engine)
-------------------------------------------------------------------------------}
  TCVWMPEngine = class(TCVCustomMediaEngine, ICVMediaEngineControl,
    ICVMediaEngineSeeking, ICVMediaEngineVideo, ICVMediaEngineAudio)
  protected
    fErrorMsg: WideString;
    fIsDone: Boolean;
    fTitle: WideString;
    fVolume: Integer;
    fWMP: TWindowsMediaPlayer;
    procedure InternalCreateWMP; virtual;
    procedure InternalDestroyWMP; virtual;
    procedure WMPError(Sender: TObject); virtual;
    procedure WMPStatusChange(Sender: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close; override; stdcall;
    function GetDuration: Int64; virtual; stdcall;
    // GetID
    // - Return unique TGuid
    function GetID: TGUID; override; stdcall;
    function GetPosition: Int64; virtual; stdcall;
    function GetStatusText: WideString; override; stdcall;
    // GetSupportedExtensions
    // - Return comma separated list of file extensions that are supported.
    // - Example: jpg,gif,avi
    // - Use * to support all extensions.
    function GetSupportedExtensions: WideString; override; stdcall;
    // GetTitle
    // - Return the title of currently loaded media
    function GetTitle: WideString; override; stdcall;
    // GetVideoSize
    // - Should return the video size (actual not resized).
    function GetVideoSize: TPoint; virtual; stdcall;
    // GetVolume
    // - Range should be from 0 to 100.
    function GetVolume: Integer; virtual; stdcall;
    // HasAudio
    // - Return true if currently loaded file has audio
    function HasAudio: Boolean; virtual; stdcall;
    // HasControl
    // - Return true if currently loaded file has audio
    function HasControl: Boolean; virtual; stdcall;
    // HasSeeking
    // - Return true if currently loaded file supports seeking
    function HasSeeking: Boolean; virtual; stdcall;
    // HasVideo
    // - Return true if currently loaded file has video
    function HasVideo: Boolean; virtual; stdcall;
    function OpenFile(AFilePath: WideString): Boolean; override; stdcall;
    procedure Pause; virtual; stdcall;
    procedure Play; virtual; stdcall;
    procedure SetBounds(ARect: TRect); override; stdcall;
    procedure SetParentWindow(AParentWindow: HWND); override; stdcall;
    procedure SetPosition(APosition: Int64); virtual; stdcall;
    // SetVolume
    // - Range should be from 0 to 100.
    procedure SetVolume(AVolume: Integer); virtual; stdcall;
    procedure Stop; virtual; stdcall;
  end;

{-------------------------------------------------------------------------------
  TCVImageEngine (ImageViewer engine)
-------------------------------------------------------------------------------}
  TCVImageEngine = class(TCVCustomMediaEngine, ICVMediaEngineControl,
    ICVMediaEngineStill)
  protected
    fImageView: TCVImageViewPanel;
    fPlayInterval: Cardinal;
    fPlayTimer: TTimer;
    fPlayTimerTick: Cardinal;
    fTaskTag: Integer;
    procedure HandleLoadError(Sender: TObject); virtual;
    procedure HandlePlayTimer(Sender: TObject); virtual;
    procedure InternalCreateImageView; virtual;
  public
    destructor Destroy; override;
    procedure Close; override; stdcall;
    constructor Create; override;
    // GetID
    // - Return unique TGuid
    function GetID: TGUID; override; stdcall;
    // HasControl
    // - Return TRUE if currently loaded file supports play/pause/stop.
    // - Return FALSE with still media like images.
    function HasControl: Boolean; virtual; stdcall;
    // IsStill
    // - Return true if currentrly loaded media is still (image or similar).
    function IsStill: Boolean; virtual; stdcall;
    function OpenFile(AFilePath: WideString): Boolean; override; stdcall;
    // Pause
    procedure Pause; virtual; stdcall;
    // Play
    procedure Play; virtual; stdcall;
    procedure SetBounds(ARect: TRect); override; stdcall;
    // SetFocus
    procedure SetFocus; override; stdcall;
    procedure SetParentWindow(AParentWindow: HWND); override; stdcall;
    // SetSlideshowInterval
    // - Engines have to implement their own timer to "play" stills.
    // - They should use the interval set in here as the "media length".
    // - Timer should start when Play is called. When the timer finishes,
    // Status should be changed to mpsDone.
    procedure SetSlideshowInterval(AInterval: Integer); virtual; stdcall;
    // Stop
    procedure Stop; virtual; stdcall;
  end;

  TCVTextEngine = class(TCVCustomMediaEngine, ICVMediaEngineControl,
      ICVMediaEngineStill, ICVMediaEngineEditor)
  protected
    fCloseEventHandler: TNotifyEvent;
    fEditor: TCETextEditor;
    fPlayInterval: Cardinal;
    fPlayTimer: TTimer;
    fPlayTimerTick: Cardinal;
    // GetPlaybackEnabled
    // - Return True if the media player should show playback controls (play, pause, next file... etc.).
    function GetPlaybackEnabled: Boolean; override; stdcall;
    procedure HandleActiveFileChange(Sender: TObject); virtual;
    procedure HandleEnablePlaybackChanged(Sender: TObject); virtual;
    procedure HandlePlayTimer(Sender: TObject); virtual;
    procedure InternalCreateEditor; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CanClose: Boolean; virtual; stdcall;
    procedure Close; override; stdcall;
    // GetID
    // - Return unique TGuid
    function GetID: TGUID; override; stdcall;
    // GetTitle
    // - Return the title of currently loaded media
    function GetTitle: WideString; override; stdcall;
    // HasControl
    // - Return TRUE if currently loaded file supports play/pause/stop.
    // - Return FALSE with still media like images.
    function HasControl: Boolean; virtual; stdcall;
    // IsStill
    // - Return true if currentrly loaded media is still (image or similar).
    function IsStill: Boolean; virtual; stdcall;
    function OpenFile(AFilePath: WideString): Boolean; override; stdcall;
    // Pause
    procedure Pause; virtual; stdcall;
    // Play
    procedure Play; virtual; stdcall;
    procedure SetBounds(ARect: TRect); override; stdcall;
    // SetEditorCloseEvent
    // - MediaPlayer will call this to set a Close event handler.
    // - If the engine want's to close itself, it can call the AHandler.
    procedure SetEditorCloseEvent(AHandler: TNotifyEvent); virtual; stdcall;
    // SetFocus
    procedure SetFocus; override; stdcall;
    procedure SetParentWindow(AParentWindow: HWND); override; stdcall;
    // SetSlideshowInterval
    // - Engines have to implement their own timer to "play" stills.
    // - They should use the interval set in here as the "media length".
    // - Timer should start when Play is called. When the timer finishes,
    // Status should be changed to mpsDone.
    procedure SetSlideshowInterval(AInterval: Integer); virtual; stdcall;
    // Stop
    procedure Stop; virtual; stdcall;
    property PlayInterval: Cardinal read fPlayInterval write fPlayInterval;
  end;

{-------------------------------------------------------------------------------
  TCVSumatraEngine
-------------------------------------------------------------------------------}
  TCVSumatraEngine = class(TCVCustomMediaEngine, ICVMediaEngineControl,
      ICVMediaEngineStill)
  protected
    fSumatra: TSumatraPDF;
    procedure DoError(Sender: TObject); virtual;
    function GetPlaybackEnabled: Boolean; override; stdcall;
    procedure HandleActiveFileChange(Sender: TObject); virtual;
    procedure InternalCreateSumatraPDF; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Close; override; stdcall;
    function GetID: TGUID; override; stdcall;
    function GetStatusText: WideString; override; stdcall;
    // GetTitle
    // - Return the title of currently loaded media
    function GetTitle: WideString; override; stdcall;
    function HasControl: Boolean; virtual; stdcall;
    function IsStill: Boolean; virtual; stdcall;
    function OpenFile(AFilePath: WideString): Boolean; override; stdcall;
    procedure Pause; virtual; stdcall;
    procedure Play; virtual; stdcall;
    procedure SetBounds(ARect: TRect); override; stdcall;
    // SetFocus
    procedure SetFocus; override; stdcall;
    procedure SetParentWindow(AParentWindow: HWND); override; stdcall;
    procedure SetSlideshowInterval(AInterval: Integer); virtual; stdcall;
    procedure Stop; virtual; stdcall;
  published
  end;

const
  WM_MediaEventNotify = WM_USER + 1;

{==============================================================================}
implementation

uses
  Graphics, GR32_Resamplers, GR32_LowLevel, CE_LanguageEngine, Forms,
  fCE_TextEditorOptions, CE_Utils;

{##############################################################################}
// TCVDSEngine

{-------------------------------------------------------------------------------
  Create
-------------------------------------------------------------------------------}
constructor TCVDSEngine.Create;
begin
  inherited;
  fMessageHandle:= Classes.AllocateHWND(WinProc);
  fVolume:= 0;
  fTitle:= '';
  fErrorMsg:= '';
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCVDSEngine.Destroy;
begin
  ClearGraph;
  Classes.DeallocateHWnd(fMessageHandle);
  inherited;
end;

{*------------------------------------------------------------------------------
  Initialize graph
-------------------------------------------------------------------------------}
procedure TCVDSEngine.InitGraph;
begin
  CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC,IID_IGraphBuilder, Graph);
  if assigned(Graph) then
  begin
    Graph.QueryInterface(IID_IMediaControl, MediaControl);
    Graph.QueryInterface(IID_IMediaSeeking, MediaSeeking);
    Graph.QueryInterface(IID_IMediaEventEx, MediaEventEx);
    Graph.QueryInterface(IID_IVideoWindow, VWin);
    Graph.QueryInterface(IID_IBasicVideo2, BasicVideo);
    Graph.QueryInterface(IID_IBasicAudio, BasicAudio);

    if assigned(MediaEventEx) then
    MediaEventEx.SetNotifyWindow(fMessageHandle, WM_MediaEventNotify, 0);
  end;
end;

{-------------------------------------------------------------------------------
  ClearGraph
-------------------------------------------------------------------------------}
procedure TCVDSEngine.ClearGraph;
begin

  if assigned(MediaControl) then
  begin
    MediaControl.Stop;
    MediaControl:= nil;
  end;
  if assigned(MediaSeeking) then
  MediaSeeking:= nil;
  if assigned(MediaEventEx) then
  MediaEventEx:= nil;
  if assigned(VWin) then
  VWin:= nil;
  if assigned(BasicVideo) then
  BasicVideo:= nil;
  if assigned(BasicAudio) then
  BasicAudio:= nil;
  if assigned(Graph) then
  Graph:= nil;

  fVideoRect:= Rect(0,0,0,0);
  fVideoSize:= Point(0,0);
  fTitle:= '';
  ChangeStatus(mpsClosed);
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCVDSEngine.Close;
begin
  ClearGraph;
end;

{-------------------------------------------------------------------------------
  Get Duration
-------------------------------------------------------------------------------}
function TCVDSEngine.GetDuration: Int64;
var
  i: Int64;
begin
  Result:= 0;
  if assigned(MediaSeeking) then
  begin
    MediaSeeking.GetDuration(i);
    if i > 0 then
    Result:= i div 10000;
  end;
end;

{-------------------------------------------------------------------------------
  GetID
-------------------------------------------------------------------------------}
function TCVDSEngine.GetID: TGUID;
begin
  Result:= ID_CVDSEngine;
end;

{-------------------------------------------------------------------------------
  Get Position
-------------------------------------------------------------------------------}
function TCVDSEngine.GetPosition: Int64;
var
  i: Int64;
begin
  Result:= 0;
  if (fStatus <> mpsClosed) and (fStatus <> mpsDone) and assigned(MediaSeeking) then
  begin
    MediaSeeking.GetCurrentPosition(i);
    if i > 0 then
    Result:= i div 10000;
  end;
end;

{-------------------------------------------------------------------------------
  Get Status
-------------------------------------------------------------------------------}
function TCVDSEngine.GetStatus: TCVMediaPlayerStatus;
begin
  Result:= inherited GetStatus;
end;

{-------------------------------------------------------------------------------
  Get StatusText
-------------------------------------------------------------------------------}
function TCVDSEngine.GetStatusText: WideString;
begin
  if fStatus = mpsError then
  Result:= fErrorMsg
  else
  Result:= inherited GetStatusText;
end;

{-------------------------------------------------------------------------------
  Get SupportedExtensions
-------------------------------------------------------------------------------}
function TCVDSEngine.GetSupportedExtensions: WideString;
begin
  Result:= '*';
end;

{-------------------------------------------------------------------------------
  Get Title
-------------------------------------------------------------------------------}
function TCVDSEngine.GetTitle: WideString;
begin
  Result:= fTitle;
end;

{-------------------------------------------------------------------------------
   Get VideoSize
-------------------------------------------------------------------------------}
function TCVDSEngine.GetVideoSize: TPoint;
begin
  Result:= fVideoSize;
end;

{-------------------------------------------------------------------------------
  GetVolume (range 0-100)
-------------------------------------------------------------------------------}
function TCVDSEngine.GetVolume: Integer;
begin
  Result:= fVolume;
end;

{-------------------------------------------------------------------------------
  HasAudio
-------------------------------------------------------------------------------}
function TCVDSEngine.HasAudio: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  HasControl
-------------------------------------------------------------------------------}
function TCVDSEngine.HasControl: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  HasSeeking
-------------------------------------------------------------------------------}
function TCVDSEngine.HasSeeking: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  HasVideo
-------------------------------------------------------------------------------}
function TCVDSEngine.HasVideo: Boolean;
begin
  Result:= (fVideoSize.X > 0) and (fVideoSize.Y > 0);
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
function TCVDSEngine.OpenFile(AFilePath: WideString): Boolean;
var
  hr: Integer;
  s: String;
  c: Integer;
begin
  
  Result:= false;
  // Re-initilize Graph
  ClearGraph;
  InitGraph;

  if not assigned(Graph) then // Nothing to do, Graph failed
  Exit;

  // Build Graph
  ChangeStatus(mpsOpening);
  hr:= Graph.RenderFile(PWideChar(AFilePath),nil);
  if hr <> S_OK then
  begin
    // Render failed, clear graph
    ClearGraph;
    
    SetLength(s, 255);
    AMGetErrorText(hr, PChar(s), 255);
    AnsiTrunkFromNull(s);
    fErrorMsg:= s;
    ChangeStatus(mpsError);
    
    Exit;
  end
  else
  begin
    UpdateTitle;
    //fTitle:= WideExtractFileName(AFilePath);
    Result:= true;
  end;


  // Initialize VideoWindow
  if assigned(VWin) and (fParentWindow <> 0) then
  begin
    VWin.put_Owner(fParentWindow);
    VWin.put_WindowStyle(WS_CHILD+WS_CLIPSIBLINGS);
    VWIn.put_MessageDrain(fParentWindow);
    ResizeVideo;
  end;

  // Set Volume
  SetVolume(fVolume);

  // Play
  Play;
end;

{-------------------------------------------------------------------------------
  Pause
-------------------------------------------------------------------------------}
procedure TCVDSEngine.Pause;
begin
  if assigned(MediaControl) then
  begin
    MediaControl.Pause;
    ChangeStatus(mpsPaused);
  end;
end;

{-------------------------------------------------------------------------------
  Play
-------------------------------------------------------------------------------}
procedure TCVDSEngine.Play;
begin
  if assigned(MediaControl) then
  begin
    MediaControl.Run;
    UpdateTitle;
    ChangeStatus(mpsPlaying);
  end;
end;

{-------------------------------------------------------------------------------
  Stop
-------------------------------------------------------------------------------}
procedure TCVDSEngine.Stop;
begin
  if assigned(MediaControl) then
  begin
    MediaControl.Stop;
    SetPosition(0);
    ChangeStatus(mpsStopped);
  end;
end;

{*------------------------------------------------------------------------------
  Resize video
-------------------------------------------------------------------------------}
procedure TCVDSEngine.ResizeVideo;
var
  AR: Extended;
  tmpI: Integer;
  BoundsSize: TPoint;
begin
  if not assigned(Graph) then
  Exit;
  
  if assigned(VWin) then
  begin
    // Get Video size
    if not assigned(BasicVideo) then
    Graph.QueryInterface(IID_IBasicVideo, BasicVideo);
    if assigned(BasicVideo) then
    begin
      fVideoSize.X:= 0;
      fVideoSize.Y:= 0;
      BasicVideo.GetVideoSize(fVideoSize.X, fVideoSize.Y)
    end
    else
    begin
      Exit;
    end;

    if (fVideoSize.X <= 0) or (fVideoSize.Y <= 0) then
    begin
      // Nothing to do
      Exit;
    end;

    // Calculate video AR and bounds size
    AR:= fVideoSize.X / fVideoSize.Y;
    BoundsSize.X:= fBoundsRect.Right-fBoundsRect.Left;
    BoundsSize.Y:= fBoundsRect.Bottom-fBoundsRect.Top;

    // Calculate fVideoRect
    if AR >= (BoundsSize.X / BoundsSize.Y) then
    begin
      fVideoRect.Left:= fBoundsRect.Left;
      fVideoRect.Right:= fBoundsRect.Right;
      tmpI:= Round(BoundsSize.X / AR);
      fVideoRect.Top:= fBoundsRect.Top + Round((BoundsSize.Y - tmpI) / 2);
      fVideoRect.Bottom:= fVideoRect.Top + tmpI;
    end
    else
    begin
      fVideoRect.Top:= fBoundsRect.Top;
      fVideoRect.Bottom:= fBoundsRect.Bottom;
      tmpI:= Round(BoundsSize.Y * AR);
      fVideoRect.Left:= fBoundsRect.Left + Round((BoundsSize.X - tmpI) / 2);
      fVideoRect.Right:= fVideoRect.Left + tmpI;
    end;

    // Set Video Position
    VWin.SetWindowPosition(fVideoRect.Left,
                           fVideoRect.Top,
                           fVideoRect.Right-fVideoRect.Left,
                           fVideoRect.Bottom-fVideoRect.Top);
  end;
end;

{-------------------------------------------------------------------------------
  Set Bounds
-------------------------------------------------------------------------------}
procedure TCVDSEngine.SetBounds(ARect: TRect);
begin
  inherited;
  ResizeVideo;
end;

{-------------------------------------------------------------------------------
  Set Parent Window
-------------------------------------------------------------------------------}
procedure TCVDSEngine.SetParentWindow(AParentWindow: HWND);
begin
  inherited;

  // Initialize VideoWindow
  if assigned(VWin) and (fParentWindow <> 0) then
  begin
    VWin.put_Owner(fParentWindow);
    VWin.put_WindowStyle(WS_CHILD+WS_CLIPSIBLINGS);
    VWIn.put_MessageDrain(0);
    ResizeVideo;
  end;
end;

{-------------------------------------------------------------------------------
  Set Position (in milliseconds)
-------------------------------------------------------------------------------}
procedure TCVDSEngine.SetPosition(APosition: Int64);
var
  i,i2: Int64;
begin
  if assigned(MediaSeeking) then
  begin
    i:= APosition * 10000; // millisecond -> 100-nanosecond.
    i2:= 0;
    MediaSeeking.SetPositions(i, AM_SEEKING_AbsolutePositioning , i2, AM_SEEKING_NoPositioning);
  end;
end;

{-------------------------------------------------------------------------------
  SetVolume (range 0-100)
-------------------------------------------------------------------------------}
procedure TCVDSEngine.SetVolume(AVolume: Integer);
var
  i: Integer;
begin                                                                
  fVolume:= AVolume;
  if assigned(BasicAudio) then
  begin
    i:= 100 * fVolume;
    if i > 0 then
    i:= Round(1085.73*ln(i)) - 10000 // convert to log
    else
    i:= -10000;
    BasicAudio.put_Volume(i);
  end;
end;

{-------------------------------------------------------------------------------
  Update Title
-------------------------------------------------------------------------------}
procedure TCVDSEngine.UpdateTitle;
var
  content: IAMMediaContent;
  ws: PWideChar;
begin
  if assigned(Graph) then
  begin
    if succeeded(Graph.QueryInterface(IID_IAMMediaContent, content)) then
    begin
      if succeeded(content.get_Title(ws)) then
      begin
        SysFreeString(ws);
        fTitle:= ws;
      end;
      content:= nil;                                                
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Graph messages
-------------------------------------------------------------------------------}
procedure TCVDSEngine.WinProc(var msg: TMessage);
var
  code, param1, param2: Integer;
begin
  if msg.Msg = WM_MediaEventNotify then
  begin
    if assigned(MediaEventEx) then
    begin
      while MediaEventEx.GetEvent(code,param1,param2, 0) = S_OK do
      begin
        case code of
          EC_COMPLETE: begin
            if assigned(MediaControl) then
            begin
              MediaControl.Stop;
              SetPosition(0);
            end;
            ChangeStatus(mpsDone);
          end;
        end;
      end;
    end;
  end
  else
  Msg.Result:= DefWindowProc(fMessageHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

{##############################################################################}
// TCVWMPEngine

{-------------------------------------------------------------------------------
  Create
-------------------------------------------------------------------------------}
constructor TCVWMPEngine.Create;
begin
  inherited;
  fVolume:= 75;
  fIsDone:= false;
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCVWMPEngine.Destroy;
begin
  fStatusChangedEvent:= nil;
  InternalDestroyWMP;
  inherited;
end;

{-------------------------------------------------------------------------------
  Internal Create WMP
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.InternalCreateWMP;
begin
  if not assigned(fWMP) then
  begin
    fWMP:= TWindowsMediaPlayer.Create(nil);
    fWMP.uiMode:= 'none';
    fWMP.stretchToFit:= true;
    fWMP.Align:= alClient;
    if assigned(fWMP.settings) then
    fWMP.settings.enableErrorDialogs:= false;
    fWMP.OnStatusChange:= WMPStatusChange;
    fWMP.OnError:= WMPError;
    
    if fParentWindow <> 0 then
    fWMP.ParentWindow:= fParentWindow;
    fWMP.BoundsRect:= fBoundsRect;
    if assigned(fWMP.settings) then
    fWMP.settings.volume:= fVolume;
  end;
end;

{-------------------------------------------------------------------------------
  Internal Destroy WMP
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.InternalDestroyWMP;
begin
  if assigned(fWMP) then
  begin
    fWMP.close;
    fWMP.Free;
    fWMP:= nil;
  end;
  fTitle:= '';
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.Close;
begin
  InternalDestroyWMP;
  ChangeStatus(mpsClosed);  
end;

{-------------------------------------------------------------------------------
  Get Duration
-------------------------------------------------------------------------------}
function TCVWMPEngine.GetDuration: Int64;
begin
  if (fStatus <> mpsClosed) and assigned(fWMP) and assigned(fWMP.currentMedia)then
  Result:= Round(fWMP.currentMedia.duration * 1000)
  else
  Result:= 0;
end;

{-------------------------------------------------------------------------------
  GetID
-------------------------------------------------------------------------------}
function TCVWMPEngine.GetID: TGUID;
begin
  Result:= ID_CVWmpEngine;
end;

{-------------------------------------------------------------------------------
  Get Position
-------------------------------------------------------------------------------}
function TCVWMPEngine.GetPosition: Int64;
begin
  if (fStatus <> mpsClosed) and assigned(fWMP) and assigned(fWMP.controls) then
  Result:= Round(fWMP.controls.currentPosition * 1000)
  else
  Result:= 0;
end;

{-------------------------------------------------------------------------------
  Get StatusText
-------------------------------------------------------------------------------}
function TCVWMPEngine.GetStatusText: WideString;
begin
  case fStatus of
    mpsClosed: Result:= 'Closed';
    mpsOpening: Result:= 'Opening';
    mpsDone: Result:= 'Done';
    mpsError: begin
      Result:= fErrorMsg;
    end
    else
    begin
      if assigned(fWMP) then     
      Result:= fWMP.status
      else
      Result:= inherited GetStatusText;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Get SupportedExtensions
-------------------------------------------------------------------------------}
function TCVWMPEngine.GetSupportedExtensions: WideString;
begin
  Result:= '*';
end;

{-------------------------------------------------------------------------------
  Get Title
-------------------------------------------------------------------------------}
function TCVWMPEngine.GetTitle: WideString;
begin
  Result:= fTitle;
end;

{-------------------------------------------------------------------------------
   Get VideoSize
-------------------------------------------------------------------------------}
function TCVWMPEngine.GetVideoSize: TPoint;
begin
  if assigned(fWMP) and assigned(fWMP.currentMedia) then
  begin
    Result:= Point(fWMP.currentMedia.imageSourceWidth,
                   fWMP.currentMedia.imageSourceHeight);
  end
  else
  Result:= Point(0,0);
end;

{-------------------------------------------------------------------------------
  GetVolume (range 0-100)
-------------------------------------------------------------------------------}
function TCVWMPEngine.GetVolume: Integer;
begin
  Result:= fVolume;
end;

{-------------------------------------------------------------------------------
  HasAudio
-------------------------------------------------------------------------------}
function TCVWMPEngine.HasAudio: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  HasControl
-------------------------------------------------------------------------------}
function TCVWMPEngine.HasControl: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  HasSeeking
-------------------------------------------------------------------------------}
function TCVWMPEngine.HasSeeking: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  HasVideo
-------------------------------------------------------------------------------}
function TCVWMPEngine.HasVideo: Boolean;
begin
  if assigned(fWMP) and assigned(fWMP.currentMedia) then
  begin
    Result:= (fWMP.currentMedia.imageSourceWidth > 0) and
             (fWMP.currentMedia.imageSourceHeight > 0);
  end
  else
  Result:= false;
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
function TCVWMPEngine.OpenFile(AFilePath: WideString): Boolean;
begin
  Result:= false;
  InternalCreateWMP;
  if assigned(fWMP) then
  begin
    fTitle:= WideExtractFileName(AFilePath);
    //ChangeStatus(mpsOpening);
    fWMP.URL:= AFilePath;
    Result:= true;
  end;
end;

{-------------------------------------------------------------------------------
  Pause
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.Pause;
begin
  if assigned(fWMP) and assigned(fWMP.controls) then
  fWMP.controls.pause;
end;

{-------------------------------------------------------------------------------
  Play
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.Play;
begin
  if assigned(fWMP) and assigned(fWMP.controls) then
  fWMP.controls.play;
end;

{-------------------------------------------------------------------------------
  Stop
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.Stop;
begin
  if assigned(fWMP) and assigned(fWMP.controls) then
  fWMP.controls.stop;
end;

{-------------------------------------------------------------------------------
  Set Bounds
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.SetBounds(ARect: TRect);
begin
  inherited;
  if assigned(fWMP) then
  fWMP.BoundsRect:= ARect;
end;

{-------------------------------------------------------------------------------
  Set Parent Window
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.SetParentWindow(AParentWindow: HWND);
begin
  inherited;
  if assigned(fWMP) then
  fWMP.ParentWindow:= AParentWindow;
end;

{-------------------------------------------------------------------------------
  Set Position
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.SetPosition(APosition: Int64);
begin
  if assigned(fWMP) and assigned(fWMP.controls) then
  fWMP.controls.currentPosition:= APosition / 1000;
end;

{-------------------------------------------------------------------------------
  SetVolume (range 0-100)
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.SetVolume(AVolume: Integer);
begin
  fVolume:= AVolume;
  if assigned(fWMP) and assigned(fWMP.settings) then
  fWMP.settings.volume:= fVolume;
end;

{-------------------------------------------------------------------------------
  On WMP Error
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.WMPError(Sender: TObject);
begin
  if assigned(fWMP) then
  begin
    if fWMP.Error.errorCount > 0 then
    fErrorMsg:= fWMP.Error.Item[fWMP.Error.errorCount-1].errorDescription; // get latest error
    fWMP.Error.clearErrorQueue;
  end
  else
  fErrorMsg:= 'Error';
  
  ChangeStatus(mpsError);
end;

{-------------------------------------------------------------------------------
  On WMP Status Change
-------------------------------------------------------------------------------}
procedure TCVWMPEngine.WMPStatusChange(Sender: TObject);
begin
  if assigned(fWMP) then
  begin
    case fWMP.playState of
      wmppsStopped    : begin
        if not fIsDone then
        ChangeStatus(mpsStopped)
        else
        ChangeStatus(mpsDone);
        fIsDone:= false;
      end;
      wmppsPlaying    : begin
        fIsDone:= false;
        if assigned(fWMP.currentMedia) then
        fTitle:= fWMP.currentMedia.name;
        ChangeStatus(mpsPlaying);
      end;
      wmppsPaused     : ChangeStatus(mpsPaused);
      wmppsBuffering  : ChangeStatus(mpsBuffering);
      wmppsMediaEnded : fIsDone:= true;
    end;
  end;
end;

{##############################################################################}
// TCVImageEngine

{-------------------------------------------------------------------------------
  Create an instance of TCVImageEngine
-------------------------------------------------------------------------------}
constructor TCVImageEngine.Create;
begin
  inherited;
  fTaskTag:= GetTickCount;
  fPlayInterval:= 3000;
  // create PlayTimer
  fPlayTimer:= TTimer.Create(nil);
  fPlayTimer.Enabled:= false;
  fPlayTimer.Interval:= fPlayInterval;
  fPlayTimer.OnTimer:= HandlePlayTimer;
  fPlayTimerTick:= 0;
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCVImageEngine.Destroy;
begin
  fPlayTimer.Enabled:= false;
  FreeAndNil(fPlayTimer);
  
  if assigned(fImageView) then
  FreeAndNil(fImageView);
  inherited;
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCVImageEngine.Close;
begin
  fPlayTimer.Enabled:= false;
  if assigned(fImageView) then
  FreeAndNil(fImageView);
  ChangeStatus(mpsClosed);
end;

{-------------------------------------------------------------------------------
  GetID
-------------------------------------------------------------------------------}
function TCVImageEngine.GetID: TGUID;
begin
  Result:= ID_CVImageEngine;
end;

{-------------------------------------------------------------------------------
  HandleLoadError
-------------------------------------------------------------------------------}
procedure TCVImageEngine.HandleLoadError(Sender: TObject);
begin
  ChangeStatus(mpsError);
end;

{-------------------------------------------------------------------------------
  Handle PlayTimer
-------------------------------------------------------------------------------}
procedure TCVImageEngine.HandlePlayTimer(Sender: TObject);
begin
  // reset PlayTimer.Interval (pause will change it, here we change it back)
  fPlayTimer.Enabled:= false;
  fPlayTimer.Interval:= fPlayInterval;
  fPlayTimerTick:= GetTickCount;
  ChangeStatus(mpsDone);
end;

{-------------------------------------------------------------------------------
  Has Control
-------------------------------------------------------------------------------}
function TCVImageEngine.HasControl: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  Internal Create ImageView
-------------------------------------------------------------------------------}
procedure TCVImageEngine.InternalCreateImageView;
var
  kernel: TKernelResampler;
begin
  if not assigned(fImageView) then
  begin
    fImageView:= TCVImageViewPanel.Create(nil);
    fImageView.View.AdaptiveZoom:= true;
    fImageView.View.ScaleDownResampler:= irDraft;
    fImageView.View.ScaleUpResampler:= irDraft;
    fImageView.View.UseThumbnailPreview:= true;
    fImageView.View.LoadOptimalSize:= true;
    fImageView.View.OnLoadError:= HandleLoadError;
    fImageView.View.Color:= $00333333;
    fImageView.View.Font.Color:= clWhite;
    fImageView.View.LoadErrorMsg:= _('Format not supported');
    if fParentWindow <> 0 then
    begin
      fImageView.ParentWindow:= fParentWindow;
      fImageView.BoundsRect:= fBoundsRect;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Is Still
-------------------------------------------------------------------------------}
function TCVImageEngine.IsStill: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
function TCVImageEngine.OpenFile(AFilePath: WideString): Boolean;
begin
  Result:= true;
  InternalCreateImageView;

  ChangeStatus(mpsOpening);
  fImageView.View.OpenFile(AFilePath);
  ChangeStatus(mpsStopped);
end;

{-------------------------------------------------------------------------------
  Pause
-------------------------------------------------------------------------------}
procedure TCVImageEngine.Pause;
var
  t: Integer;
begin
  if fStatus = mpsPlaying then
  begin
    fPlayTimer.Enabled:= false;
    t:= fPlayInterval - (GetTickCount - fPlayTimerTick);
    if t > 0 then
    fPlayTimer.Interval:= t
    else
    fPlayTimer.Interval:= 0;
    
    ChangeStatus(mpsPaused);
  end;
end;

{-------------------------------------------------------------------------------
  Play
-------------------------------------------------------------------------------}
procedure TCVImageEngine.Play;
begin
  // start PlayTimer
  if fPlayInterval > 0 then
  begin
    fPlayTimer.Enabled:= true;
    fPlayTimerTick:= GetTickCount;
    ChangeStatus(mpsPlaying);
  end;
end;

{-------------------------------------------------------------------------------
  Set Bounds
-------------------------------------------------------------------------------}
procedure TCVImageEngine.SetBounds(ARect: TRect);
begin
  inherited;
  if assigned(fImageView) then
  fImageView.BoundsRect:= fBoundsRect;
end;

{-------------------------------------------------------------------------------
  SetFocus
-------------------------------------------------------------------------------}
procedure TCVImageEngine.SetFocus;
begin
  if assigned(fImageView) then
  begin
    Windows.SetFocus(fImageView.View.Handle);
  end;
end;

{-------------------------------------------------------------------------------
  Set Parent Window
-------------------------------------------------------------------------------}
procedure TCVImageEngine.SetParentWindow(AParentWindow: HWND);
begin
  inherited;
  if assigned(fImageView) and (fParentWindow <> 0) then
  begin
    fImageView.ParentWindow:= fParentWindow;
    fImageView.BoundsRect:= fBoundsRect;
  end;
end;

{-------------------------------------------------------------------------------
  SetSlideshowInterval
-------------------------------------------------------------------------------}
procedure TCVImageEngine.SetSlideshowInterval(AInterval: Integer);
begin
  fPlayInterval:= AInterval;
  fPlayTimer.Interval:= fPlayInterval;
end;

{-------------------------------------------------------------------------------
  Stop
-------------------------------------------------------------------------------}
procedure TCVImageEngine.Stop;
begin
  if (fStatus = mpsPlaying) or (fStatus = mpsPaused) then
  begin
    fPlayTimer.Enabled:= false;
    fPlayTimer.Interval:= fPlayInterval;
    ChangeStatus(mpsStopped);
  end;
end;

{##############################################################################}
// TCVTextEngine

{-------------------------------------------------------------------------------
  Create
-------------------------------------------------------------------------------}
constructor TCVTextEngine.Create;
begin
  inherited;
  // initilize values
  fPlayInterval:= 3000;
  PlaybackEnabled:= false;
  // create PlayTimer
  fPlayTimer:= TTimer.Create(nil);
  fPlayTimer.Enabled:= false;
  fPlayTimer.Interval:= fPlayInterval;
  fPlayTimer.OnTimer:= HandlePlayTimer;
  fPlayTimerTick:= 0;
end;

{-------------------------------------------------------------------------------
  Destroy
-------------------------------------------------------------------------------}
destructor TCVTextEngine.Destroy;
begin
  fPlayTimer.Enabled:= false;
  FreeAndNil(fPlayTimer);
  
  if assigned(fEditor) then
  FreeAndNil(fEditor);
  inherited;
end;

{-------------------------------------------------------------------------------
  CanClose
-------------------------------------------------------------------------------}
function TCVTextEngine.CanClose: Boolean;
begin
  Result:= fEditor.CanClose;
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCVTextEngine.Close;
begin
  fPlayTimer.Enabled:= false;
  if assigned(fEditor) then
  FreeAndNil(fEditor);
  ChangeStatus(mpsClosed);
end;

{-------------------------------------------------------------------------------
  GetID
-------------------------------------------------------------------------------}
function TCVTextEngine.GetID: TGUID;
begin
  Result:= ID_CVTextEngine;
end;

{-------------------------------------------------------------------------------
  GetPlaybackEnabled
-------------------------------------------------------------------------------}
function TCVTextEngine.GetPlaybackEnabled: Boolean;
begin
  Result:= GlobalTextEditorSettings.EnablePlayback;
end;

{-------------------------------------------------------------------------------
  Get Title
-------------------------------------------------------------------------------}
function TCVTextEngine.GetTitle: WideString;
begin
  if assigned(fEditor) then
  Result:= fEditor.ActiveFileName
  else
  Result:= '';
end;

{-------------------------------------------------------------------------------
  Handle ActiveFileChange
-------------------------------------------------------------------------------}
procedure TCVTextEngine.HandleActiveFileChange(Sender: TObject);
begin
  DoTitleChanged;
end;

{-------------------------------------------------------------------------------
  Handle EnablePlaybackChanged
-------------------------------------------------------------------------------}
procedure TCVTextEngine.HandleEnablePlaybackChanged(Sender: TObject);
begin
  ChangeStatus(fStatus);
end;

{-------------------------------------------------------------------------------
  Handle PlayTimer
-------------------------------------------------------------------------------}
procedure TCVTextEngine.HandlePlayTimer(Sender: TObject);
begin
  // reset PlayTimer.Interval (pause will change it, here we change it back)
  fPlayTimer.Interval:= fPlayInterval;
  fPlayTimerTick:= GetTickCount;
  ChangeStatus(mpsDone);
end;

{-------------------------------------------------------------------------------
  Has Control
-------------------------------------------------------------------------------}
function TCVTextEngine.HasControl: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  Internal Create ImageView
-------------------------------------------------------------------------------}
procedure TCVTextEngine.InternalCreateEditor;
begin
  if not assigned(fEditor) then
  begin
    fEditor:= TCETextEditor.Create(nil);
    fEditor.OnEnablePlaybackChanged:= HandleEnablePlaybackChanged;
    fEditor.OnActiveFileChange:= HandleActiveFileChange;
    if assigned(fCloseEventHandler) then
    fEditor.act_close.OnExecute:= fCloseEventHandler;
      
    fEditor.BorderStyle:= bsNone;
    if fParentWindow <> 0 then
    begin
      fEditor.ParentWindow:= fParentWindow;
      fEditor.BoundsRect:= fBoundsRect;
      fEditor.Visible:= true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Is Still
-------------------------------------------------------------------------------}
function TCVTextEngine.IsStill: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
function TCVTextEngine.OpenFile(AFilePath: WideString): Boolean;
begin
  Result:= true;
  InternalCreateEditor;

  ChangeStatus(mpsOpening);
  fEditor.OpenFile(AFilePath);
  ChangeStatus(mpsStopped);
end;

{-------------------------------------------------------------------------------
  Pause
-------------------------------------------------------------------------------}
procedure TCVTextEngine.Pause;
var
  t: Integer;
begin
  if fStatus = mpsPlaying then
  begin
    fPlayTimer.Enabled:= false;
    t:= fPlayInterval - (GetTickCount - fPlayTimerTick);
    if t > 0 then
    fPlayTimer.Interval:= t
    else
    fPlayTimer.Interval:= 0;
    
    ChangeStatus(mpsPaused);
  end;
end;

{-------------------------------------------------------------------------------
  Play
-------------------------------------------------------------------------------}
procedure TCVTextEngine.Play;
begin
  // start PlayTimer
  if fPlayInterval > 0 then
  begin
    fPlayTimer.Enabled:= true;
    fPlayTimerTick:= GetTickCount;
    ChangeStatus(mpsPlaying);
  end;
end;

{-------------------------------------------------------------------------------
  Set Bounds
-------------------------------------------------------------------------------}
procedure TCVTextEngine.SetBounds(ARect: TRect);
begin
  inherited;
  if assigned(fEditor) then
  fEditor.BoundsRect:= fBoundsRect;
end;

{-------------------------------------------------------------------------------
  SetEditorCloseEvent
-------------------------------------------------------------------------------}
procedure TCVTextEngine.SetEditorCloseEvent(AHandler: TNotifyEvent);
begin
  fCloseEventHandler:= AHandler;
  if assigned(fEditor) and assigned(fCloseEventHandler) then
  fEditor.act_close.OnExecute:= fCloseEventHandler;
end;

{-------------------------------------------------------------------------------
  SetFocus
-------------------------------------------------------------------------------}
procedure TCVTextEngine.SetFocus;
begin
  if assigned(fEditor) then
  begin
    fEditor.SynMemo.SetFocus;
  end;
end;

{-------------------------------------------------------------------------------
  Set Parent Window
-------------------------------------------------------------------------------}
procedure TCVTextEngine.SetParentWindow(AParentWindow: HWND);
begin
  inherited;
  if assigned(fEditor) and (fParentWindow <> 0) then
  begin
    fEditor.ParentWindow:= fParentWindow;
    fEditor.BoundsRect:= fBoundsRect;
  end;
end;

{-------------------------------------------------------------------------------
  SetSlideshowInterval
-------------------------------------------------------------------------------}
procedure TCVTextEngine.SetSlideshowInterval(AInterval: Integer);
begin
  fPlayInterval:= AInterval;
  fPlayTimer.Interval:= fPlayInterval;
end;

{-------------------------------------------------------------------------------
  Stop
-------------------------------------------------------------------------------}
procedure TCVTextEngine.Stop;
begin
  if (fStatus = mpsPlaying) or (fStatus = mpsPaused) then
  begin
    fPlayTimer.Enabled:= false;
    fPlayTimer.Interval:= fPlayInterval;
    ChangeStatus(mpsStopped);
  end;
end;

{##############################################################################}
// TCVSumatraEngine

{-------------------------------------------------------------------------------
  Create an instance of TCVSumatraEngine
-------------------------------------------------------------------------------}
constructor TCVSumatraEngine.Create;
begin
  inherited;

  // supported:
  // cbr,cbz,chm,djvu,mobi,pdf,xps


end;

{-------------------------------------------------------------------------------
  Destroy TCVSumatraEngine
-------------------------------------------------------------------------------}
destructor TCVSumatraEngine.Destroy;
begin
  if assigned(fSumatra) then
  FreeAndNil(fSumatra);

  inherited;
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.Close;
begin
  if assigned(fSumatra) then
  begin
    fSumatra.Close;
    FreeAndNil(fSumatra);
  end;
  ChangeStatus(mpsClosed);
end;

{-------------------------------------------------------------------------------
  DoError
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.DoError(Sender: TObject);
begin
  ChangeStatus(mpsError);
end;

{-------------------------------------------------------------------------------
  GetID
-------------------------------------------------------------------------------}
function TCVSumatraEngine.GetID: TGUID;
begin
  Result:= ID_CVPDFEngine;
end;                                               

{-------------------------------------------------------------------------------
  GetPlaybackEnabled
-------------------------------------------------------------------------------}
function TCVSumatraEngine.GetPlaybackEnabled: Boolean;
begin
  Result:= false;
end;

{-------------------------------------------------------------------------------
  Get StatusText
-------------------------------------------------------------------------------}
function TCVSumatraEngine.GetStatusText: WideString;
begin
  inherited GetStatusText;
  if fStatus = mpsError then
  Result:= fSumatra.LastError;
end;

{-------------------------------------------------------------------------------
  Get Title
-------------------------------------------------------------------------------}
function TCVSumatraEngine.GetTitle: WideString;
begin
  if assigned(fSumatra) then
  Result:= fSumatra.ActiveFileName
  else
  Result:= '';
end;

{-------------------------------------------------------------------------------
  Handle ActiveFileChange
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.HandleActiveFileChange(Sender: TObject);
begin
  DoTitleChanged;
end;

{-------------------------------------------------------------------------------
  Has Control
-------------------------------------------------------------------------------}
function TCVSumatraEngine.HasControl: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  IsStill
-------------------------------------------------------------------------------}
function TCVSumatraEngine.IsStill: Boolean;
begin
  Result:= true;
end;

{-------------------------------------------------------------------------------
  Internal Create SumatraPDF
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.InternalCreateSumatraPDF;
begin
  if not assigned(fSumatra) then
  begin
    fSumatra:= TSumatraPDF.Create(nil);
    fSumatra.OnError:= DoError;
    fSumatra.OnActiveFileChange:= HandleActiveFileChange;
    fSumatra.BorderStyle:= bsNone;
    if fParentWindow <> 0 then
    begin
      fSumatra.ParentWindow:= fParentWindow;
      fSumatra.BoundsRect:= fBoundsRect;
      fSumatra.Visible:= true;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
function TCVSumatraEngine.OpenFile(AFilePath: WideString): Boolean;
begin
  Result:= false;
  InternalCreateSumatraPDF;
  if assigned(fSumatra) then
  Result:= fSumatra.OpenFile(AFilePath);
end;

{-------------------------------------------------------------------------------
  Pause
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.Pause;
begin
  // TODO -cMM: TCVSumatraEngine.Pause default body inserted
end;

{-------------------------------------------------------------------------------
  Play
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.Play;
begin
  // TODO -cMM: TCVSumatraEngine.Play default body inserted
end;

{-------------------------------------------------------------------------------
  Set Bounds
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.SetBounds(ARect: TRect);
begin
  inherited;
  if assigned(fSumatra) then
  fSumatra.BoundsRect:= ARect;
end;

{-------------------------------------------------------------------------------
  SetFocus
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.SetFocus;
begin
  if assigned(fSumatra) then
  Windows.SetFocus(fSumatra.SumatraWindow);
end;

{-------------------------------------------------------------------------------
  Set Parent Window
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.SetParentWindow(AParentWindow: HWND);
begin
  inherited;
  if assigned(fSumatra) then
  fSumatra.ParentWindow:= AParentWindow;
end;

{-------------------------------------------------------------------------------
  SetSlideshowInterval
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.SetSlideshowInterval(AInterval: Integer);
begin
  // TODO -cMM: TCVSumatraEngine.SetSlideshowInterval default body inserted
end;

{-------------------------------------------------------------------------------
  Stop
-------------------------------------------------------------------------------}
procedure TCVSumatraEngine.Stop;
begin
  // TODO -cMM: TCVSumatraEngine.Stop default body inserted
end;

end.

