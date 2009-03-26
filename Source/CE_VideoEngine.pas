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
//  The Original Code is CE_VideoEngine.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_VideoEngine;

interface

uses
  // DirectX
  DirectShow9,
  TntSysUtils,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActiveX;

type

  TCEDSPlayer = class(TComponent)
  private
    fDuration: Int64;
    fDurationString: String;
    fHasVideo: Boolean;
    fIsPaused: Boolean;
    fIsPlaying: Boolean;
    fLoop: Boolean;
    fMessageHandle: HWND;
    fOnProgress: TNotifyEvent;
    fPosition: Int64;
    fPositionString: string;
    fProgressTimer: TTimer;
    fVideoPanel: TWinControl;
    fVolume: Byte;
    function GetFullscreen: boolean;
    procedure SetPosition(const Value: Int64);
  protected
    procedure Progress(Sender: TObject);
    procedure SetVolume(const Value: Byte);
  public
    { Public declarations }
    Graph: IGraphBuilder;
    MediaControl: IMediaControl;
    VWin: IVideoWindow;
    BasicVideo: IBasicVideo;
    BasicAudio: IBasicAudio;
    MediaSeeking: IMediaSeeking;
    MediaEventEx: IMediaEventEx;
    constructor Create(AOwner: TComponent); override;
    //FilterGraph2: IFilterGraph2;
    destructor Destroy; override;
    procedure ClearGraph;
    procedure SetFullscreen(const Value: Boolean);
    procedure InitGraph;
    function IsLoaded: Boolean;
    procedure Play;
    function OpenFile(AFilePath: WideString): Integer;
    procedure Pause;
    procedure ResizeVideo;
    procedure Stop;
    procedure WinProc(var msg: TMessage);
    property Duration: Int64 read fDuration;
    property DurationString: String read fDurationString;
    property Fullscreen: boolean read GetFullscreen write SetFullscreen;
    property HasVideo: Boolean read fHasVideo;
    property IsPaused: Boolean read fIsPaused;
    property IsPlaying: Boolean read fIsPlaying;
    property Loop: Boolean read fLoop write fLoop;
    property Position: Int64 read fPosition write SetPosition;
    property PositionString: string read fPositionString;
    property VideoPanel: TWinControl read fVideoPanel write fVideoPanel;
    property Volume: Byte read fVolume write SetVolume;
  published
    property OnProgress: TNotifyEvent read fOnProgress write fOnProgress;
  end;

  function SecToTime(Sec: Integer): string;

const
  WM_ShellNotify = WM_USER + 1;

implementation

{*------------------------------------------------------------------------------
  Second To Time
-------------------------------------------------------------------------------}
function SecToTime(Sec: Integer): string;
var
   H, M, S: string;
   ZH, ZM, ZS: Integer;
begin
   ZH:= Sec div 3600;
   ZM:= Sec div 60 - ZH * 60;
   ZS:= Sec - (ZH * 3600 + ZM * 60) ;
   H:= IntToStr(ZH);
   M:= IntToStr(ZM);
   S:= IntToStr(ZS);
   if ZS < 10 then
   S:= '0'+S;

   if h = '0' then
   begin
     Result := M + ':' + S
   end
   else
   begin
     if ZM < 10 then
     M:= '0'+M;
     Result := H + ':' + M + ':' + S;
   end;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create an instance of TCEDSPlayer
-------------------------------------------------------------------------------}
constructor TCEDSPlayer.Create(AOwner: TComponent);
begin
  inherited;
  fProgressTimer:= TTimer.Create(self);
  fProgressTimer.Enabled:= false;
  fProgressTimer.Interval:= 250;
  fProgressTimer.OnTimer:= Progress;
  fMessageHandle:= Classes.AllocateHWND(WinProc);
  fLoop:= false;
end;

{*------------------------------------------------------------------------------
  Destroy an instance of TCEDSPlayer
-------------------------------------------------------------------------------}
destructor TCEDSPlayer.Destroy;
begin
  FreeAndNil(fProgressTimer);
  ClearGraph;
  Classes.DeallocateHWnd(fMessageHandle);
  inherited;
end;

{*------------------------------------------------------------------------------
  ClearGraph
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.ClearGraph;
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
  
  if assigned(fProgressTimer) then
  begin
    fProgressTimer.Enabled:= false;
    Progress(self);
  end;
end;

{*------------------------------------------------------------------------------
  Initialize graph
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.InitGraph;
begin
  CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC,IID_IGraphBuilder, Graph);
  Graph.QueryInterface(IID_IMediaControl, MediaControl);
  Graph.QueryInterface(IID_IMediaSeeking, MediaSeeking);
  Graph.QueryInterface(IID_IMediaEventEx, MediaEventEx);
  Graph.QueryInterface(IID_IVideoWindow, VWin);
  Graph.QueryInterface(IID_IBasicVideo2, BasicVideo);
  Graph.QueryInterface(IID_IBasicAudio, BasicAudio);

  if assigned(MediaEventEx) then
  begin
    MediaEventEx.SetNotifyWindow(fMessageHandle, WM_ShellNotify, 0);
  end;
end;

{*------------------------------------------------------------------------------
  Open File
-------------------------------------------------------------------------------}
function TCEDSPlayer.OpenFile(AFilePath: WideString): Integer;
begin
  Result:= E_FAIL;
  
  if not WideFileExists(AFilePath) then Exit;
  ClearGraph;
  InitGraph;
  // Build Graph
  Result:= Graph.RenderFile(PWideChar(AFilePath),nil);

  if Result <> S_OK then
  begin
    ClearGraph;
    Exit;
  end;
  fHasVideo:= false;
  // Initialize VideoWindow
  if assigned(VWin) and assigned(fVideoPanel) then
  begin
    VWin.put_Owner(fVideoPanel.Handle);
    VWin.put_WindowStyle(WS_CHILD+WS_CLIPSIBLINGS);
    VWIn.put_MessageDrain(fMessageHandle);
    ResizeVideo;
  end;
  // Set volume
  SetVolume(fVolume);
  // Run Graph
  if assigned(MediaControl) then
  MediaControl.Run;

  Progress(self);  

  fProgressTimer.Enabled:= true;
end;

{*------------------------------------------------------------------------------
  Play
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.Play;
begin
  if assigned(MediaControl) then
  MediaControl.Run;
  Progress(self);
end;

{*------------------------------------------------------------------------------
  Stop
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.Stop;
begin
  if assigned(MediaControl) then
  MediaControl.Stop;
  Progress(self);
end;

{*------------------------------------------------------------------------------
  Pause
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.Pause;
begin
  if assigned(MediaControl) then
  MediaControl.Pause;
  Progress(self);
end;

{*------------------------------------------------------------------------------
  IsLoaded
-------------------------------------------------------------------------------}
function TCEDSPlayer.IsLoaded: Boolean;
begin
  Result:= Assigned(Graph);
end;

{*------------------------------------------------------------------------------
  Set video to fullscreen
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.SetFullscreen(const Value: Boolean);
begin
  if assigned(VWin) then
  begin
    VWin.put_FullScreenMode(Value);
  end;
end;

{*------------------------------------------------------------------------------
  Get video fullscreen
-------------------------------------------------------------------------------}
function TCEDSPlayer.GetFullscreen: boolean;
var
  b: LongBool;
begin
  Result:= false;
  if assigned(VWin) then
  begin
    VWin.get_FullScreenMode(b);
    Result:= b;
  end;
end;

{*------------------------------------------------------------------------------
  Resize video
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.ResizeVideo;
var
  VideoRect: TRect;
  VideoSize: TPoint;
  AR: Extended;
  tmpI: Integer;
begin
  fHasVideo:= false;
  
  if not assigned(Graph) then
  Exit;
  
  if assigned(VWin) and assigned(fVideoPanel) then
  begin
    if not assigned(BasicVideo) then
    Graph.QueryInterface(IID_IBasicVideo, BasicVideo);
    
    if assigned(BasicVideo) then
    begin
      VideoSize.X:= 0;
      VideoSize.Y:= 0;
      BasicVideo.GetVideoSize(VideoSize.X, VideoSize.Y)
    end
    else
    begin
      Exit;
    end;

    if (VideoSize.X <= 0) or (VideoSize.Y <= 0) then
    begin
      Exit;
    end;

    fHasVideo:= true;

    AR:= VideoSize.X / VideoSize.Y;

    if AR >= (fVideoPanel.ClientWidth / fVideoPanel.ClientHeight) then
    begin
      VideoRect.Left:= 0;
      VideoRect.Right:= fVideoPanel.ClientWidth;
      tmpI:= Round(fVideoPanel.ClientWidth / AR);
      VideoRect.Top:= Round((fVideoPanel.ClientHeight - tmpI) / 2);
      VideoRect.Bottom:= tmpI;
    end
    else
    begin
      VideoRect.Top:= 0;
      VideoRect.Bottom:= fVideoPanel.ClientHeight;
      tmpI:= Round(fVideoPanel.ClientHeight * AR);
      VideoRect.Left:= Round((fVideoPanel.ClientWidth - tmpI) / 2);
      VideoRect.Right:= tmpI;
    end;

    VWin.SetWindowPosition(VideoRect.Left,
                           VideoRect.Top,
                           VideoRect.Right,
                           VideoRect.Bottom);
  end;
end;

{*------------------------------------------------------------------------------
  Call progress event
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.Progress(Sender: TObject);
var
  fs: TFilterState;
  i: Int64;
begin
  if assigned(MediaControl) then
  begin
    MediaControl.GetState(1000,fs);
    case fs of
      State_Stopped: begin
                       fIsPlaying:= false;
                       fIsPaused:= false;
                     end;
      State_Paused:  begin
                       fIsPlaying:= false;
                       fIsPaused:= true;
                     end;
      State_Running: begin
                       fIsPlaying:= true;
                       fIsPaused:= false;
                     end;
    end;
  end
  else
  begin
    fIsPlaying:= false;
    fIsPaused:= false;
  end;

  if assigned(MediaSeeking) then
  begin
    MediaSeeking.GetDuration(i);
    fDuration:= i div 10000;
    MediaSeeking.GetCurrentPosition(i);
    fPosition:= i div 10000;
    fDurationString:= SecToTime(fDuration div 1000);
    fPositionString:= SecToTime(fPosition div 1000);
  end
  else
  begin
    fDuration:= 0;
    fPosition:= 0;
    fDurationString:= '0:00';
    fPositionString:= '0:00';
  end;
  if Assigned(fOnProgress) then fOnProgress(Self);
end;

{*------------------------------------------------------------------------------
  Set playback position
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.SetPosition(const Value: Int64);
var
  i,i2: Int64;
begin
  if assigned(MediaSeeking) then
  begin
    i:= Value * 10000;
    i2:= 0;
    MediaSeeking.SetPositions(i, AM_SEEKING_AbsolutePositioning , i2, AM_SEEKING_NoPositioning);
    Progress(self);
  end;
end;

{*------------------------------------------------------------------------------
  Set Volume
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.SetVolume(const Value: Byte);
var
  i: Integer;
begin
  fVolume:= Value;
  
  if not assigned(Graph) then
  Exit;
  
  if not assigned(BasicAudio) then
  Graph.QueryInterface(IID_IBasicAudio, BasicAudio);

  if assigned(BasicAudio) then
  begin
    i:= Round((10000 / 255) * (Value));
    if i > 0 then
    i:= Round(1085.73*ln(i)) - 10000
    else
    i:= -10000;
    BasicAudio.put_Volume(i);
  end;
end;

{*------------------------------------------------------------------------------
  Mouse and Keyboard input from video window
-------------------------------------------------------------------------------}
procedure TCEDSPlayer.WinProc(var msg: TMessage);
var
  code, param1, param2: Integer;
begin
  if msg.Msg = WM_LBUTTONDBLCLK then
  begin
    Fullscreen:= not Fullscreen;
  end
  else if msg.Msg = WM_KEYDOWN then
  begin
    if (msg.WParam = VK_ESCAPE) and Fullscreen then
    Fullscreen:= false;
  end;
  if msg.Msg = WM_ShellNotify then
  begin
    if assigned(MediaEventEx) then
    begin
      while MediaEventEx.GetEvent(code,param1,param2, 0) = S_OK do
      begin
        case code of
          EC_COMPLETE: begin
            if fLoop then
            Position:= 0
            else
            Stop;
          end;
        end;
      end;
    end;
  end
  else
  Msg.Result:= DefWindowProc(fMessageHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.
