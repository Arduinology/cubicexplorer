unit CE_VideoPlayerUI;

interface

uses
  // Graphics32
  GR32, GR32_Image,
  // GraphicEx
  GraphicEx,
  // System Units
  Classes, Windows, Messages, SysUtils, Graphics, ExtCtrls, Controls;

type
  TCEVideoPlayerStatus = (vpsPlaying, vpsPaused, vpsStopped);
  TCEVPProgressEvent = procedure(Sender: TObject; Position: Integer) of object;

  TCEVideoPlayerController = class(TImage32)
  private
    fBorderColor: TColor32;
    fControlsBackBit: TBitmap32;
    fFullScreenButtonBit: TBitmap32;
    fMaxPosition: Integer;
    fMinPosition: Integer;
    fUnMutedButtonBit: TBitmap32;
    fMuted: Boolean;
    fMutedButtonBit: TBitmap32;
    fOnFullscreenClick: TNotifyEvent;
    fOnMutedChanged: TNotifyEvent;
    fOnPauseClick: TNotifyEvent;
    fOnPlayClick: TNotifyEvent;
    fOnProgressBarClick: TCEVPProgressEvent;
    fOnProgressBarMouseMove: TCEVPProgressEvent;
    fOnStopClick: TNotifyEvent;
    fOnVolumeChanged: TNotifyEvent;
    fPauseButtonBit: TBitmap32;
    fPlayButtonBit: TBitmap32;
    fPlayingTitle: WideString;
    fPosition: Integer;
    fProgressBarBit: TBitmap32;
    fStatus: TCEVideoPlayerStatus;
    fStopButtonBit: TBitmap32;
    fTimeString: string;
    fVolume: Byte;
    fVolumeBit: TBitmap32;
    fVolumeChanging: Boolean;
    fVolumeFullBit: TBitmap32;
  protected
    PlayButtonRect: TRect;
    PauseButtonRect: TRect;
    StopButtonRect: TRect;
    PlayingTitleRect: TRect;
    TimeStringRect: TRect;
    VolumeRampRect: TRect;
    MuteButtonRect: TRect;
    FullscreenButtonRect: TRect;
    ProgressBarRect: TRect;
    ControlsRect: TRect;
    function CalcRects: TRect;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        overload; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        overload; override;
    procedure UpdateCursor(MousePos: TPoint);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw;
    procedure Init;
    procedure Resize; override;
    property BorderColor: TColor32 read fBorderColor write fBorderColor;
    property ControlsBackBit: TBitmap32 read fControlsBackBit;
    property FullScreenButtonBit: TBitmap32 read fFullScreenButtonBit;
    property MaxPosition: Integer read fMaxPosition write fMaxPosition;
    property MinPosition: Integer read fMinPosition write fMinPosition;
    property UnMutedButtonBit: TBitmap32 read fUnMutedButtonBit;
    property Muted: Boolean read fMuted write fMuted;
    property MutedButtonBit: TBitmap32 read fMutedButtonBit;
    property PauseButtonBit: TBitmap32 read fPauseButtonBit;
    property PlayButtonBit: TBitmap32 read fPlayButtonBit;
    property PlayingTitle: WideString read fPlayingTitle write fPlayingTitle;
    property Position: Integer read fPosition write fPosition;
    property ProgressBarBit: TBitmap32 read fProgressBarBit;
    property Status: TCEVideoPlayerStatus read fStatus write fStatus;
    property StopButtonBit: TBitmap32 read fStopButtonBit;
    property TimeString: string read fTimeString write fTimeString;
    property Volume: Byte read fVolume write fVolume;
    property VolumeBit: TBitmap32 read fVolumeBit;
    property VolumeFullBit: TBitmap32 read fVolumeFullBit;
  published
    property OnFullscreenClick: TNotifyEvent read fOnFullscreenClick write
        fOnFullscreenClick;
    property OnMutedChanged: TNotifyEvent read fOnMutedChanged write
        fOnMutedChanged;
    property OnPauseClick: TNotifyEvent read fOnPauseClick write fOnPauseClick;
    property OnPlayClick: TNotifyEvent read fOnPlayClick write fOnPlayClick;
    property OnProgressBarClick: TCEVPProgressEvent read fOnProgressBarClick
        write fOnProgressBarClick;
    property OnProgressBarMouseMove: TCEVPProgressEvent read
        fOnProgressBarMouseMove write fOnProgressBarMouseMove;
    property OnStopClick: TNotifyEvent read fOnStopClick write fOnStopClick;
    property OnVolumeChanged: TNotifyEvent read fOnVolumeChanged write
        fOnVolumeChanged;
  end;

implementation

{*------------------------------------------------------------------------------
  Create an instance of TCEVideoPlayerController
-------------------------------------------------------------------------------}
constructor TCEVideoPlayerController.Create(AOwner: TComponent);
begin
  inherited;
  fProgressBarBit:= TBitmap32.Create;
  fControlsBackBit:= TBitmap32.Create;
  fPlayButtonBit:= TBitmap32.Create;
  fPauseButtonBit:= TBitmap32.Create;
  fStopButtonBit:= TBitmap32.Create;
  fFullScreenButtonBit:= TBitmap32.Create;
  fUnMutedButtonBit:= TBitmap32.Create;
  fMutedButtonBit:= TBitmap32.Create;
  fVolumeBit:= TBitmap32.Create;
  fVolumeFullBit:= TBitmap32.Create;
  Init;
end;

{*------------------------------------------------------------------------------
  Destroy TCEVideoPlayerController
-------------------------------------------------------------------------------}
destructor TCEVideoPlayerController.Destroy;
begin
  fProgressBarBit.Free;
  fControlsBackBit.Free;
  fPlayButtonBit.Free;
  fPauseButtonBit.Free;  
  fStopButtonBit.Free;
  fFullScreenButtonBit.Free;
  fUnMutedButtonBit.Free;
  fMutedButtonBit.Free;
  fVolumeBit.Free;
  fVolumeFullBit.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Initialize
-------------------------------------------------------------------------------}
procedure TCEVideoPlayerController.Init;
begin
  fStatus:= vpsStopped;
  fBorderColor:= clBlack32;
  // Load Bitmaps
  Bitmap.Font.Name:= 'Arial';
  Bitmap.Font.Size:= 8;
  fProgressBarBit.LoadFromResourceName(hInstance, 'AV_UI_PROGRESSBAR');
  fControlsBackBit.LoadFromResourceName(hInstance, 'AV_UI_CONTROLSBACK');
  fPlayButtonBit.LoadFromResourceName(hInstance, 'AV_UI_PLAYBUTTON');
  fPauseButtonBit.LoadFromResourceName(hInstance, 'AV_UI_PAUSEBUTTON');
  fStopButtonBit.LoadFromResourceName(hInstance, 'AV_UI_STOPBUTTON');
  fFullScreenButtonBit.LoadFromResourceName(hInstance, 'AV_UI_FULLSCREENBUTTON');
  fUnMutedButtonBit.LoadFromResourceName(hInstance, 'AV_UI_UNMUTEDBUTTON');
  fMutedButtonBit.LoadFromResourceName(hInstance, 'AV_UI_MUTEDBUTTON');
  fVolumeBit.LoadFromResourceName(hInstance, 'AV_UI_VOLUME');
  fVolumeFullBit.LoadFromResourceName(hInstance, 'AV_UI_VOLUMEFULL');
end;

{*------------------------------------------------------------------------------
  Calculate Rects (Returns full area rect)
-------------------------------------------------------------------------------}
function TCEVideoPlayerController.CalcRects: TRect;
var
  x,y,x2,tmpX: Integer;
begin
  x:= 0;
  x2:= Bitmap.Width;
// border line Y
  y:= 1;
// Progress bar
  ProgressBarRect:= Rect(0, y, Bitmap.Width, y+fProgressBarBit.Height);
  y:= y + fProgressBarBit.Height;
// border line Y
  y:= y + 1;
// Controls area
  ControlsRect:= Rect(0, y, Bitmap.Width, y+fControlsBackBit.Height);
  y:= y + fControlsBackBit.Height;
// Play Button
  if fStatus <> vpsPlaying then
  begin
    PlayButtonRect:= Rect(0, ControlsRect.Top, fPlayButtonBit.Width, ControlsRect.Bottom);
    x:= x + fPlayButtonBit.Width;
    PauseButtonRect:= Rect(0,0,0,0);
  end
// Pause Button
  else
  begin
    PauseButtonRect:= Rect(0, ControlsRect.Top, fPauseButtonBit.Width, ControlsRect.Bottom);
    x:= x + fPauseButtonBit.Width;
    PlayButtonRect:= Rect(0,0,0,0);
  end;
// border line X
  x:= x + 1;
// Stop Button
  if fStatus <> vpsStopped then
  begin
    StopButtonRect:= Rect(x, ControlsRect.Top, x+fStopButtonBit.Width, ControlsRect.Bottom);
    x:= x + fStopButtonBit.Width;
  end;
// border line X
  x:= x + 1;
// Fullscreen button
  FullscreenButtonRect:= Rect(x2-fFullScreenButtonBit.Width,
                              ControlsRect.Top,
                              x2,
                              ControlsRect.Bottom);
  x2:= x2 - fFullScreenButtonBit.Width;
// border line X
  x2:= x2 - 1;
// Mute/Volume
  if not fMuted then
  tmpX:= x2-fUnMutedButtonBit.Width-fVolumeBit.Width-1
  else
  tmpX:= x2-fUnMutedButtonBit.Width-1;

  if tmpX > x then
  begin
    if not fMuted then
    begin
      MuteButtonRect:= Rect(x2-fUnMutedButtonBit.Width,
                            ControlsRect.Top,
                            x2,
                            ControlsRect.Bottom);
      x2:= x2 - fUnMutedButtonBit.Width;
      VolumeRampRect:= Rect(x2-fVolumeBit.Width,
                            ControlsRect.Top,
                            x2,
                            ControlsRect.Bottom);
      x2:= x2 - fVolumeBit.Width;
    end
    else
    begin
      MuteButtonRect:= Rect(x2-fMutedButtonBit.Width,
                            ControlsRect.Top,
                            x2,
                            ControlsRect.Bottom);
      x2:= x2 - fMutedButtonBit.Width;                         
      VolumeRampRect:= Rect(0,0,0,0);
    end;
  // border line X
    x2:= x2 - 1;

  // Time String
    x2:= x2 - 2; // some space before text
    tmpX:= Bitmap.TextWidth(fTimeString);
    if (x2 - tmpX) > x then
    begin
      TimeStringRect:= Rect(x2 - tmpX,
                            ControlsRect.Top,
                            x2,
                            ControlsRect.Bottom);
      x2:= x2 - tmpX;
    // Playing Title
      x:= x + 2; // some space before text
      if x2 > x+10 then
      begin
        PlayingTitleRect:= Rect(x,
                                ControlsRect.Top,
                                x2,
                                ControlsRect.Bottom);
      end
      else
      begin
        PlayingTitleRect:= Rect(0,0,0,0);
      end;
    end
    else
    begin
      TimeStringRect:= Rect(0,0,0,0);
      PlayingTitleRect:= Rect(0,0,0,0);
    end;
  end
  else
  begin
    MuteButtonRect:= Rect(0,0,0,0);
    VolumeRampRect:= Rect(0,0,0,0);
  end;

// border line Y
  y:= y + 1;
// Total area
  Result:= Rect(0, 0, Bitmap.Width, y); 
end;

{*------------------------------------------------------------------------------
  Draw
-------------------------------------------------------------------------------}
procedure TCEVideoPlayerController.Draw;
var
  r,r2: TRect;
  s: Real;
  i,i2: Integer;
begin
  CalcRects;

  Bitmap.BeginUpdate;
  try
    Bitmap.Clear(clGray32);
    // border line Y
    Bitmap.LineS(0,0,Bitmap.Width,0,fBorderColor);
    // progress bar
    r:= ProgressBarRect;
    i:= r.Right - r.Left;
    i2:= fMaxPosition - fMinPosition;
    if (i2 <> 0) then
    s:= i / i2
    else
    s:= 0;
    r.Right:= r.Left + Round(s * fPosition);
    Bitmap.Draw(r, fProgressBarBit.BoundsRect, fProgressBarBit);
    // borderline X
    if r.Right > 0 then
    Bitmap.VertLineS(r.Right, r.Top, r.Bottom, fBorderColor);
    // border line Y
    Bitmap.LineS(0,ProgressBarRect.Bottom,Bitmap.Width,ProgressBarRect.Bottom,fBorderColor);
    // controls back
    Bitmap.Draw(ControlsRect, fControlsBackBit.BoundsRect, fControlsBackBit);
      // play button
      if fStatus <> vpsPlaying then
      begin
        Bitmap.Draw(PlayButtonRect, fPlayButtonBit.BoundsRect, fPlayButtonBit);
        // borderline X
        Bitmap.VertLineS(PlayButtonRect.Right, ControlsRect.Top, ControlsRect.Bottom, fBorderColor);
      end
      else
      begin
        Bitmap.Draw(PauseButtonRect, fPauseButtonBit.BoundsRect, fPauseButtonBit);
        // borderline X
        Bitmap.VertLineS(PauseButtonRect.Right, ControlsRect.Top, ControlsRect.Bottom, fBorderColor);
      end;
      // stop button
      if fStatus <> vpsStopped then
      begin
        Bitmap.Draw(StopButtonRect, fStopButtonBit.BoundsRect, fStopButtonBit);
        // borderline X
        Bitmap.VertLineS(StopButtonRect.Right, ControlsRect.Top, ControlsRect.Bottom, fBorderColor);
      end;

      // Fullscreen button
      Bitmap.Draw(FullscreenButtonRect, fFullScreenButtonBit.BoundsRect, fFullScreenButtonBit);
      Bitmap.VertLineS(FullscreenButtonRect.Left-1, ControlsRect.Top, ControlsRect.Bottom, fBorderColor);

      // Mute/Volume
      if not fMuted then
      begin
        Bitmap.Draw(MuteButtonRect, fUnMutedButtonBit.BoundsRect, fUnMutedButtonBit);
        Bitmap.Draw(VolumeRampRect, fVolumeBit.BoundsRect, fVolumeBit);
        s:= (VolumeRampRect.Right - VolumeRampRect.Left) / 255;
        r:= VolumeRampRect;
        r2:= fVolumeFullBit.BoundsRect;
        r2.Right:= Round(s * fVolume);
        r.Right:= r.Left + r2.Right;
        Bitmap.Draw(r, r2, fVolumeFullBit);
        Bitmap.VertLineS(VolumeRampRect.Left-1, ControlsRect.Top, ControlsRect.Bottom, fBorderColor);
      end
      else
      begin
        Bitmap.Draw(MuteButtonRect, fMutedButtonBit.BoundsRect, fMutedButtonBit);
        Bitmap.VertLineS(MuteButtonRect.Left-1, ControlsRect.Top, ControlsRect.Bottom, fBorderColor);
      end;

      // Time String
      Bitmap.Textout(TimeStringRect, DT_VCENTER or DT_SINGLELINE, fTimeString);
      // Playing Title
      Bitmap.TextoutW(PlayingTitleRect, DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS, fPlayingTitle);

    // border line Y
    Bitmap.Line(0,ControlsRect.Bottom,Bitmap.Width,ControlsRect.Bottom,fBorderColor);
  finally
    Bitmap.EndUpdate;
    Bitmap.Changed;
  end;
end;

{*------------------------------------------------------------------------------
  Handle Resize
-------------------------------------------------------------------------------}
procedure TCEVideoPlayerController.Resize;
begin
  inherited;
  Height:= CalcRects.Bottom;  
  SetupBitmap;
  Draw;
end;

{*------------------------------------------------------------------------------
  Handle MouseDown
-------------------------------------------------------------------------------}
procedure TCEVideoPlayerController.MouseDown(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
  s: Real;
begin
  inherited;

  MousePos:= Point(X, Y);

  if Shift = [ssLeft] then
  begin
    // Progress bar click
    if PtInRect(ProgressBarRect, MousePos) then
    begin
      s:= (fMaxPosition - fMinPosition) / (ProgressBarRect.Right - ProgressBarRect.Left);
      if assigned(fOnProgressBarClick) then fOnProgressBarClick(self, Round(s*(X-ProgressBarRect.Left)));
      Exit;
    end;

    // Play button click
    if fStatus <> vpsPlaying then
    begin
      if PtInRect(PlayButtonRect, MousePos) then
      begin
        if assigned(fOnPlayClick) then fOnPlayClick(self);
        UpdateCursor(MousePos);
        Exit;
      end;
    end
    // Pause button click
    else
    begin
      if PtInRect(PauseButtonRect, MousePos) then
      begin
        if assigned(fOnPauseClick) then fOnPauseClick(self);
        UpdateCursor(MousePos);
        Exit;
      end;
    end;
    // Stop button click
    if PtInRect(StopButtonRect, MousePos) then
    begin
      if assigned(fOnStopClick) then fOnStopClick(self);
      UpdateCursor(MousePos);
      Exit;
    end;
    // Fullscreen button click
    if PtInRect(FullscreenButtonRect, MousePos) then
    begin
      if assigned(fOnFullscreenClick) then fOnFullscreenClick(self);
      UpdateCursor(MousePos);
      Exit;
    end;
    // Mute Button click
    if PtInRect(MuteButtonRect, MousePos) then
    begin
      Muted:= not Muted;
      Draw;
      if assigned(fOnMutedChanged) then fOnMutedChanged(self);
      Exit;
    end;
    // Volume Ramp click
    if PtInRect(VolumeRampRect, MousePos) then
    begin
      fVolumeChanging:= true;
      s:= 255 / (VolumeRampRect.Right - VolumeRampRect.Left);
      Volume:= Round(s * (MousePos.X - VolumeRampRect.Left));
      Draw;
      if assigned(fOnVolumeChanged) then fOnVolumeChanged(self);
      Exit;
    end;
  end;
end;

procedure TCEVideoPlayerController.MouseLeave;
begin
  inherited;
  if Cursor <> crDefault then
  Cursor:= crDefault;
end;

{*------------------------------------------------------------------------------
  Handle MouseMove
-------------------------------------------------------------------------------}
procedure TCEVideoPlayerController.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  MousePos: TPoint;
  s: Real;
  i: Integer;
begin
  inherited;

  MousePos:= Point(X, Y);

  if Shift = [ssLeft] then
  begin
    // Volume Ramp
    if PtInRect(VolumeRampRect, MousePos) then
    begin
      s:= 255 / (VolumeRampRect.Right - VolumeRampRect.Left);
      i:= Round(s * (MousePos.X - VolumeRampRect.Left));
      if i <> fVolume then
      begin
        Volume:= i;
        Draw;
        if assigned(fOnVolumeChanged) then fOnVolumeChanged(self);
      end;
      Exit;
    end
    else if fVolumeChanging and PtInRect(MuteButtonRect, MousePos) then
    begin
      if Volume < 255 then
      begin
        Volume:= 255;
        Draw;
        if assigned(fOnVolumeChanged) then fOnVolumeChanged(self);
      end;
      Exit;
    end;
  end;

  // Progress Bar
  if PtInRect(ProgressBarRect, MousePos) then
  begin
    s:= (fMaxPosition - fMinPosition) / (ProgressBarRect.Right - ProgressBarRect.Left);
    if assigned(fOnProgressBarMouseMove) then fOnProgressBarMouseMove(self, Round(s*(X-ProgressBarRect.Left)));
    Exit;
  end;

  UpdateCursor(MousePos);
end;

{*------------------------------------------------------------------------------
  Handle Mouse Up
-------------------------------------------------------------------------------}
procedure TCEVideoPlayerController.MouseUp(Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer);
begin
  inherited;
  fVolumeChanging:= false;
end;

{*------------------------------------------------------------------------------
 Update Cursor 
-------------------------------------------------------------------------------}
procedure TCEVideoPlayerController.UpdateCursor(MousePos: TPoint);
begin
  if PtInRect(PlayButtonRect, MousePos) or
     PtInRect(PauseButtonRect, MousePos) or
     ((fStatus <> vpsStopped) and PtInRect(StopButtonRect, MousePos)) or
     PtInRect(MuteButtonRect, MousePos) or
     PtInRect(FullscreenButtonRect, MousePos) then
  begin
    if Cursor <> crHandPoint then
    Cursor:= crHandPoint;
  end
  else if PtInRect(VolumeRampRect, MousePos) then
  begin
    if Cursor <> crDefault then
    Cursor:= crDefault;
  end
  else     
  begin
    if Cursor <> crDefault then
    Cursor:= crDefault;
  end;
end;

end.
