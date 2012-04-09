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
//  The Original Code is fCE_QuickView.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit fCE_QuickView;

interface

uses
  // CE
  CE_Toolbar, CE_SpTBXItems, CE_FilePreview,
  // CV
  CV_MediaPlayer, CV_MediaPlayerEngines,
  // SpTBX
  TB2Dock, SpTBXItem, TB2Toolbar, SpTBXControls, TB2Item, SpTBXDkPanels,
  SpTBXTabs,
  // System Units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, ImgList, PngImageList, ComCtrls, ExtCtrls;

type
  TCEQuickViewForm = class;

{-------------------------------------------------------------------------------
  TCEQuickView
-------------------------------------------------------------------------------}
  TCEQuickView = class(TFrame)
    Dock_Bottom: TSpTBXDock;
    toolbar_seekbar: TCEToolbar;
    toolbar_controls: TCEToolbar;
    PngImageList1: TPngImageList;
    panel_content: TPanel;
    tabs_playlist: TSpTBXTabControl;
    tab_playlist: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    tab_filelist: TSpTBXTabItem;
    SpTBXTabSheet2: TSpTBXTabSheet;
    splitter_left: TSpTBXSplitter;
    splitter_right: TSpTBXSplitter;
  private
    { Private declarations }
  protected
    fActive: Boolean;
    fActiveFilePath: WideString;
    fControlItems: TComponentList;
    fHostWindow: HWND;
    fLastMute: Boolean;
    fLastPlaylistVisible: Boolean;
    fLastVolume: Integer;
    fMediaPlayer: TCVMediaPlayer;
    Preview: TCEFilePreview;
    procedure AssignTo(Dest: TPersistent); override;
    function CreateDetachedQuickView: TCEQuickViewForm; virtual;
    procedure CreateEmbededMediaPlayer; virtual;
    procedure HandleControlClick(Sender: TObject); virtual;
    procedure HandlePreviewClick(Sender: TObject); virtual;
    procedure HandleSeekbarAfterChange(Sender: TObject); virtual;
    procedure HandleShowHintQuery(Sender: TObject; var AHint: String; var
        AShowHint: Boolean); virtual;
    procedure HandleVolumeAfterChange(Sender: TObject); virtual;
    procedure HandleVolumeChange(Sender: TObject); virtual;
    procedure SetActive(const Value: Boolean);
    procedure SetActiveFilePath(const Value: WideString);
    procedure UpdateControlStates(Sender: TObject); virtual;
    procedure UpdateSeekbarState(Sender: TObject; APosition: Int64; ADuration:
        Int64); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AttachMediaPlayer(AMediaPlayer: TCVMediaPlayer); virtual;
    procedure Close; virtual;
    procedure Detach; virtual;
    procedure OpenFile(AFilePath: WideString); virtual;
    procedure OpenFileInMediaPlayer(AFilePath: WideString); virtual;
    procedure PopulateControlsToolbar(AToolbar: TCEToolbar); virtual;
    procedure PopulateSeekToolbar(AToolbar: TCEToolbar); virtual;
    property Active: Boolean read fActive write SetActive;
    property ActiveFilePath: WideString read fActiveFilePath write
        SetActiveFilePath;
    property HostWindow: HWND read fHostWindow write fHostWindow;
    { Public declarations }
  end;

{-------------------------------------------------------------------------------
  TCEQuickViewForm
    - This is the detached QuickView window
-------------------------------------------------------------------------------}
  TCEQuickViewForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoClose(var Action: TCloseAction); override;
  public
    QuickView: TCEQuickView;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
  end;

{-------------------------------------------------------------------------------
  TCEQuickViewSettings
-------------------------------------------------------------------------------}
  TCEMediaPlayerType = (mptAuto, mptWMP, mptDirectShow);

  TCEQuickViewSettings = class(TPersistent)
  private
    fRememberInnerToolbarLayout: Boolean;
    fRememberOuterToolbarLayout: Boolean;
    fRememberPanelLayout: Boolean;
  protected
    fDirectShowExtensions: WideString;
    fImageExtensions: WideString;
    fMediaPlayer: TCEMediaPlayerType;
    fTextExtensions: WideString;
    fMediaExtensions: WideString;
    fWMPExtensions: WideString;
    procedure SetDirectShowExtensions(const Value: WideString); virtual;
    procedure SetImageExtensions(const Value: WideString); virtual;
    procedure SetMediaExtensions(const Value: WideString); virtual;
    procedure SetTextExtensions(const Value: WideString); virtual;
    procedure SetWMPExtensions(const Value: WideString); virtual;
  public
    constructor Create; virtual;
    function CreateMediaEngine(AExtension: WideString): ICVMediaEngine; virtual;
    function IsSupported(AExtension: WideString): Boolean; virtual;
  published
    property DirectShowExtensions: WideString read fDirectShowExtensions write
        SetDirectShowExtensions;
    property ImageExtensions: WideString read fImageExtensions write
        SetImageExtensions;
    property MediaPlayer: TCEMediaPlayerType read fMediaPlayer write fMediaPlayer;
    property TextExtensions: WideString read fTextExtensions write
        SetTextExtensions;
    property RememberInnerToolbarLayout: Boolean read fRememberInnerToolbarLayout
        write fRememberInnerToolbarLayout;
    property RememberOuterToolbarLayout: Boolean read fRememberOuterToolbarLayout
        write fRememberOuterToolbarLayout;
    property RememberPanelLayout: Boolean read fRememberPanelLayout write
        fRememberPanelLayout;
    property MediaExtensions: WideString read fMediaExtensions write
        SetMediaExtensions;
    property WMPExtensions: WideString read fWMPExtensions write SetWMPExtensions;
  end;

var
  GlobalQuickViewSettings: TCEQuickViewSettings;

implementation

uses
  Math, CE_AppSettings, ccFileUtils, ccClasses;

{$R *.dfm}

{##############################################################################}
// TCEQuickView

{-------------------------------------------------------------------------------
  Create an instance of TCEQuickView
-------------------------------------------------------------------------------}
constructor TCEQuickView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // initilize values
  fLastMute:= false;
  fLastVolume:= 100;
  fLastPlaylistVisible:= false;

  Preview:= TCEFilePreview.Create(Self);
  Preview.Parent:= panel_content;
  Preview.Align:= alClient;
  Preview.OnClick:= HandlePreviewClick;

  fControlItems:= TComponentList.Create(false);
  PopulateControlsToolbar(toolbar_controls);
  PopulateSeekToolbar(toolbar_seekbar);
  UpdateControlStates(Self);
end;

{-------------------------------------------------------------------------------
  Destroy TCEQuickView
-------------------------------------------------------------------------------}
destructor TCEQuickView.Destroy;
begin
  if assigned(fMediaPlayer) then
  begin
    fMediaPlayer.Free;
    fMediaPlayer:= nil;
  end;
  fControlItems.Free;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  AssignTo
-------------------------------------------------------------------------------}
procedure TCEQuickView.AssignTo(Dest: TPersistent);
var
  quick: TCEQuickView;
begin
  if Dest is TCEQuickView then
  begin
    quick:= TCEQuickView(Dest);
    quick.fLastVolume:= fLastVolume;
    quick.fLastMute:= fLastMute;    
    quick.fLastPlaylistVisible:= fLastPlaylistVisible;
    if assigned(quick.fMediaPlayer) then
    begin
      quick.fMediaPlayer.Volume:= fLastVolume;
      quick.fMediaPlayer.Mute:= fLastMute;
    end;
    quick.UpdateControlStates(Self);
  end;
end;

{-------------------------------------------------------------------------------
  Attach MediaPlayer
-------------------------------------------------------------------------------}
procedure TCEQuickView.AttachMediaPlayer(AMediaPlayer: TCVMediaPlayer);
begin
  if assigned(AMediaPlayer) then
  begin
    // free previous instance of MediaPlayerWnd
    Close;
  
    // attach
    fMediaPlayer:= AMediaPlayer;
    fMediaPlayer.Parent:= panel_content;
    fLastVolume:= fMediaPlayer.Volume;
    fLastMute:= fMediaPlayer.Mute;
    fMediaPlayer.OnPositionChange:= UpdateSeekbarState;
    fMediaPlayer.OnStatusChanged:= UpdateControlStates;
  end;
end;

{-------------------------------------------------------------------------------
  Close
-------------------------------------------------------------------------------}
procedure TCEQuickView.Close;
begin
  if assigned(fMediaPlayer) then
  begin
    fMediaPlayer.Free;
    fMediaPlayer:= nil;
  end;
  Preview.Clear;
end;

{-------------------------------------------------------------------------------
  Create Detached QuickView
-------------------------------------------------------------------------------}
function TCEQuickView.CreateDetachedQuickView: TCEQuickViewForm;
var
  p: TPoint;
begin
  // create form
  Result:= TCEQuickViewForm.CreateNew(Self);
  // size
  p:= Self.ClientToScreen(Point(0,0));
  Result.ClientHeight:= Self.ClientHeight;
  Result.ClientWidth:= Self.ClientWidth;
  Windows.SetWindowPos(Result.Handle, HWND_TOP,
                       p.X+5, p.Y+5,
                       0, 0,
                       SWP_NOSIZE);
  // show
  Result.Show;
  Result.Resize;
end;

{-------------------------------------------------------------------------------
  Create EmbededMediaPlayer
-------------------------------------------------------------------------------}
procedure TCEQuickView.CreateEmbededMediaPlayer;
begin
  if not assigned(fMediaPlayer) then
  begin
    fMediaPlayer:= TCVMediaPlayer.Create(nil);
    fMediaPlayer.Parent:= panel_content;
    fMediaPlayer.Align:= alClient;
    fMediaPlayer.Volume:= fLastVolume;
    fMediaPlayer.Mute:= fLastMute;
    fMediaPlayer.BringToFront;
    fMediaPlayer.OnPositionChange:= UpdateSeekbarState;
    fMediaPlayer.OnStatusChanged:= UpdateControlStates;
  end;
end;

{-------------------------------------------------------------------------------
  Detach
-------------------------------------------------------------------------------}
procedure TCEQuickView.Detach;
var
  form: TCEQuickViewForm;
begin
  form:= CreateDetachedQuickView;
  form.QuickView.AttachMediaPlayer(fMediaPlayer);
  form.QuickView.Assign(Self);
  fMediaPlayer:= nil;
  UpdateControlStates(Self);
end;

{-------------------------------------------------------------------------------
  Handle ControlClick
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleControlClick(Sender: TObject);
begin
  if assigned(fMediaPlayer) then
  begin
    if Sender is TSpTBXItem then
    begin
      case TSpTBXItem(Sender).Tag of
        1: begin
          if fMediaPlayer.GetStatus = mpsPlaying then
          fMediaPlayer.Pause
          else
          fMediaPlayer.Play;
        end;
        2: fMediaPlayer.Stop;
        8: begin
          fLastPlaylistVisible:= not fLastPlaylistVisible;
        end;
        9: begin
          fLastMute:= not fLastMute;
          fMediaPlayer.Mute:= fLastMute;
        end;
      end;
      UpdateControlStates(Self);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle SeekbarAfterChange
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleSeekbarAfterChange(Sender: TObject);
begin
  if assigned(fMediaPlayer) then
  fMediaPlayer.SetPosition(Round((fMediaPlayer.GetDuration / MAXWORD) * TCETrackBar(Sender).Position));
end;

{-------------------------------------------------------------------------------
  Handle ShowHintQuery
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleShowHintQuery(Sender: TObject; var AHint: String; var AShowHint: Boolean);
var
  track: TCETrackBar;
  pos: Int64;
begin
  if Sender is TCETrackBar then
  begin
    track:= TCETrackBar(Sender);
    // Seekbar
    if track.Tag = 20 then
    begin
      if assigned(fMediaPlayer) then
      begin
        pos:= Round((fMediaPlayer.GetDuration / track.Max) * track.Position);

        // millisecond to second
        if pos > 0 then
        pos:= pos div 1000;

        AHint:= SecToTime(pos);
        AShowHint:= true;
      end
      else
      AShowHint:= false;
    end
    // Volume
    else if track.Tag = 10 then
    begin
      AHint:= AHint + '%';
      AShowHint:= true;
    end
  end;
end;

{-------------------------------------------------------------------------------
 Handle VolumeAfterChange
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleVolumeAfterChange(Sender: TObject);
begin
  UpdateControlStates(Self);
end;

{-------------------------------------------------------------------------------
  Handle VolumeChange
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandleVolumeChange(Sender: TObject);
var
  i: Integer;
begin
  if TCETrackBar(Sender).Changing then
  begin
    fLastVolume:= TCETrackBar(Sender).Position;
    fLastMute:= fLastVolume = 0;    
    TCETrackBar(Sender).SelEnd:= fLastVolume;  
    if assigned(fMediaPlayer) then
    begin
      fMediaPlayer.Volume:= fLastVolume;
      fMediaPlayer.Mute:= fLastMute;
    end;

    // update mute buttons  
    for i:= 0 to fControlItems.Count - 1 do
    begin
      if fControlItems.Items[i].Tag = 9 then
      begin
        if TSpTBXItem(fControlItems.Items[i]).Checked <> fLastMute then
        begin
          TSpTBXItem(fControlItems.Items[i]).Checked:= fLastMute;
          if fLastMute then
          TSpTBXItem(fControlItems.Items[i]).ImageIndex:= 9
          else
          TSpTBXItem(fControlItems.Items[i]).ImageIndex:= 8;

          // repaint the toolbar manually because the mute button doesn't get
          //   repainted properly always when using the volume trackbar.
          if TSpTBXItem(fControlItems.Items[i]).Owner is TSpTBXToolbar then
          TSpTBXToolbar(TSpTBXItem(fControlItems.Items[i]).Owner).Repaint;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  OpenFile
-------------------------------------------------------------------------------}
procedure TCEQuickView.OpenFile(AFilePath: WideString);
var
  status: TCVMediaPlayerStatus;
begin
  Preview.OpenFile(AFilePath);
  if assigned(fMediaPlayer) then
  begin
    status:= fMediaPlayer.GetStatus;
    if (status = mpsClosed) or (status = mpsDone) or (status = mpsError) or (status = mpsStopped)  then
    Close;
  end;
  UpdateControlStates(Self);
end;

{-------------------------------------------------------------------------------
  Handle Preview Click
-------------------------------------------------------------------------------}
procedure TCEQuickView.HandlePreviewClick(Sender: TObject);
begin
  OpenFileInMediaPlayer(fActiveFilePath);
  fMediaPlayer.SetFocus;
end;

{-------------------------------------------------------------------------------
  Open File In MediaPlayer
-------------------------------------------------------------------------------}
procedure TCEQuickView.OpenFileInMediaPlayer(AFilePath: WideString);
var
  Ext: WideString;
begin
  CreateEmbededMediaPlayer;
  Ext:= WideLowerCase(WideExtractFileExt(AFilePath));


  fMediaPlayer.Engine:= GlobalQuickViewSettings.CreateMediaEngine(Ext);
  fMediaPlayer.OpenFile(AFilePath);
  UpdateControlStates(Self);
end;

{-------------------------------------------------------------------------------
  Populate Controls Toolbar
-------------------------------------------------------------------------------}
procedure TCEQuickView.PopulateControlsToolbar(AToolbar: TCEToolbar);
var
  item: TSpTBXItem;
  status: TSpTBXLabelItem;
  track: TCETrackBar;
begin
  if assigned(AToolbar) then
  AToolbar.Items.Clear
  else
  Exit;

  // play (1)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 1;
  item.Caption:= 'Play';
  item.ImageIndex:= 0;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // stop (2)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 2;
  item.Caption:= 'Stop';
  item.ImageIndex:= 2;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // previous (3)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 3;
  item.Caption:= 'Previous';
  item.ImageIndex:= 3;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // next (4)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 4;
  item.Caption:= 'Next';
  item.ImageIndex:= 4;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);

  // dynamic spacer
  AToolbar.Items.Add(TCEToolbarDynamicSpacerItem.Create(AToolbar));

  // previous file (5)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 5;
  item.Caption:= 'Previous File';
  item.ImageIndex:= 5;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // next file (6)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 6;
  item.Caption:= 'Next File';
  item.ImageIndex:= 6;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // status text (7)
//  status:= TSpTBXLabelItem.Create(AToolbar);
//  status.Tag:= 7;
//  status.Caption:= 'Closed';
//  AToolbar.Items.Add(status);
//  fControlItems.Add(status);
  
  // playlist (8)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 8;
  item.Caption:= 'Playlist';
  item.ImageIndex:= 7;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // mute (9)
  item:= TSpTBXItem.Create(AToolbar);
  item.Tag:= 9;
  item.Caption:= 'Mute';
  item.ImageIndex:= 8;
  item.OnClick:= HandleControlClick;
  AToolbar.Items.Add(item);
  fControlItems.Add(item);
  // volume (10)
  track:= TCETrackBar.Create(AToolbar);
  track.Tag:= 10;
  track.Parent:= AToolbar;
  track.ShowFocusRect:= false;
  track.ShowPositionHint:= true;
  track.Height:= 21;
  track.Width:= 64;
  track.ChannelSize:= 8;
  track.ThumbLength:= 18;
  track.Max:= 100;
  track.PageSize:= 10;
  track.TickStyle:= tsNone;
  track.TickMarks:= tmxCenter;
  track.OnChange:= HandleVolumeChange;
  track.OnAfterChange:= HandleVolumeAfterChange;
  track.OnShowHintQueryEvent:= HandleShowHintQuery;
  fControlItems.Add(track);
end;

{-------------------------------------------------------------------------------
  Populate Seek Toolbar
-------------------------------------------------------------------------------}
procedure TCEQuickView.PopulateSeekToolbar(AToolbar: TCEToolbar);
var
  track: TCETrackBar;
  spacer: TCECustomToolbarSpacerItem;
  time: TSpTBXLabelItem;
  item: TSpTBXItem;
begin
  if assigned(AToolbar) then
  AToolbar.Items.Clear
  else
  Exit;

  // seekbar (20)
  track:= TCETrackBar.Create(AToolbar);
  track.Tag:= 20;
  track.Parent:= AToolbar;
  track.ShowFocusRect:= false;
  track.ShowPositionHint:= true;
  track.Height:= 21;
  track.ChannelSize:= 8;
  track.ThumbLength:= 18;
  track.Max:= MAXWORD;
  track.PageSize:= MAXWORD div 20;
  track.TickStyle:= tsNone;
  track.TickMarks:= tmxCenter;
  track.OnAfterChange:= HandleSeekbarAfterChange;
  track.OnShowHintQueryEvent:= HandleShowHintQuery;
  fControlItems.Add(track);
  // stretcher
  AToolbar.Items.Add(TCEToolbarStretcherItem.Create(AToolbar));

  // time label (21)
  time:= TSpTBXLabelItem.Create(AToolbar);
  time.Tag:= 21;
  time.Caption:= '00:00 / 00:00';
  fControlItems.Add(time);
  AToolbar.Items.Add(time);
end;

{-------------------------------------------------------------------------------
  Set Active
-------------------------------------------------------------------------------}
procedure TCEQuickView.SetActive(const Value: Boolean);
begin
  if fActive <> Value then
  begin
    fActive:= Value;
    if fActive then
    OpenFile(fActiveFilePath)
    else
    Close;
  end;
end;

{-------------------------------------------------------------------------------
  Set ActiveFilePath
-------------------------------------------------------------------------------}
procedure TCEQuickView.SetActiveFilePath(const Value: WideString);
begin
  fActiveFilePath:= Value;
  if fActive then
  begin
    if WideFileExists(fActiveFilePath) then
    OpenFile(fActiveFilePath)
    else if not assigned(fMediaPlayer) then
    Close;
  end;
end;

{-------------------------------------------------------------------------------
  Update Control States
  - Here we hide/show and enable/disable buttons and toolbars according to
  current media player state.
-------------------------------------------------------------------------------}
procedure TCEQuickView.UpdateControlStates(Sender: TObject);
var
  i: Integer;
  item: TSpTBXItem;
  status: TCVMediaPlayerStatus;
  loaded: Boolean;
begin
  // loaded, show toolbar(s)
  if assigned(fMediaPlayer) then
  begin
    status:= fMediaPlayer.GetStatus;
    loaded:= (status = mpsPlaying) or (status = mpsPaused) or (status = mpsStopped) or (status = mpsDone);
    for i:= 0 to fControlItems.Count - 1 do
    begin
      case fControlItems.Items[i].Tag of
        // play/pause
        1: begin
          item:= TSpTBXItem(fControlItems.Items[i]);
          item.Enabled:= loaded;
          if (status = mpsPlaying) then
          item.ImageIndex:= 1
          else
          item.ImageIndex:= 0;
        end;
        // stop
        2: begin
          TSpTBXItem(fControlItems.Items[i]).Enabled:= (status = mpsPlaying) or (status = mpsPaused);
        end;
        // previous
        3: begin
        
        end;
        // next
        4: begin

        end;
        // previous file
        5: begin

        end;
        // next file
        6: begin

        end;
        // status text
        7: begin

        end;
        // playlist
        8: begin
          TSpTBXItem(fControlItems.Items[i]).Checked:= fLastPlaylistVisible;
        end;
        // mute
        9: begin
          if fMediaPlayer.HasAudio then
          begin
            TSpTBXItem(fControlItems.Items[i]).Visible:= true;
            TSpTBXItem(fControlItems.Items[i]).Checked:= fLastMute;
            if fLastMute then
            TSpTBXItem(fControlItems.Items[i]).ImageIndex:= 9
            else
            TSpTBXItem(fControlItems.Items[i]).ImageIndex:= 8;
          end
          else
          TSpTBXItem(fControlItems.Items[i]).Visible:= false;
        end;
        // volume
        10: begin
          if fMediaPlayer.HasAudio then
          begin
            TCETrackBar(fControlItems.Items[i]).Visible:= true;
            TCETrackBar(fControlItems.Items[i]).Position:= fLastVolume;
            if not fLastMute then
            TCETrackBar(fControlItems.Items[i]).SelEnd:= fLastVolume
            else
            TCETrackBar(fControlItems.Items[i]).SelEnd:= 0;
          end
          else
          TCETrackBar(fControlItems.Items[i]).Visible:= false;
        end;
        // time label
        21: begin
          if status = mpsError then
          begin
            TSpTBXLabelItem(fControlItems.Items[i]).Caption:= 'Error';
            TSpTBXLabelItem(fControlItems.Items[i]).Hint:= fMediaPlayer.GetStatusText;
          end;
        end;
      end;
    end;
    // Show toolbars
    if not toolbar_controls.Visible then
    begin
      toolbar_controls.Visible:= true;
      toolbar_controls.Invalidate;
    end;

    if not toolbar_seekbar.Visible then
    begin
      toolbar_seekbar.Visible:= fMediaPlayer.HasSeeking;
      toolbar_seekbar.Invalidate;
    end
    else
    toolbar_seekbar.Visible:= fMediaPlayer.HasSeeking;

    // Show Playlist
    if tabs_playlist.Visible <> fLastPlaylistVisible then
    begin
      tabs_playlist.Visible:= fLastPlaylistVisible;
      if fLastPlaylistVisible then
      begin
        if tabs_playlist.Align = alRight then
        begin
          splitter_left.Visible:= false;
          splitter_right.Visible:= true;
          splitter_right.Left:= tabs_playlist.Left - splitter_right.Width;
        end
        else if tabs_playlist.Align = alLeft then
        begin
          splitter_left.Visible:= true;
          splitter_right.Visible:= false;
          splitter_left.Left:= tabs_playlist.Width;
        end;
      end
      else
      begin
        splitter_left.Visible:= false;
        splitter_right.Visible:= false;
      end;
    end;
  end
  // not loaded, hide everything
  else
  begin
    toolbar_controls.Visible:= false;
    toolbar_seekbar.Visible:= false;
    tabs_playlist.Visible:= false;
    splitter_left.Visible:= false;
    splitter_right.Visible:= false;
  end;
end;

{-------------------------------------------------------------------------------
  Update Seekbar State
-------------------------------------------------------------------------------}
procedure TCEQuickView.UpdateSeekbarState(Sender: TObject; APosition: Int64;
    ADuration: Int64);
var
  status: TCVMediaPlayerStatus;
  i, posI: Integer;
  track: TCETrackBar;
  s: String;
begin
  if not assigned(fMediaPlayer) then
  Exit;

  if (ADuration > 0) then
  posI:= Min(Round((MAXWORD / ADuration) * APosition), MAXWORD)
  else
  posI:= 0;

  // milliseconds to seconds
  if APosition > 0 then
  APosition:= APosition div 1000;
  if ADuration > 0 then
  ADuration:= ADuration div 1000;
  s:= SecToTime(APosition) + ' / ' + SecToTime(ADuration);
  status:= fMediaPlayer.GetStatus;

  for i:= 0 to fControlItems.Count - 1 do
  begin
    // Change seekbar positions
    if (fControlItems.Items[i] is TCETrackBar) and (fControlItems.Items[i].Tag = 20) then
    begin
      track:= TCETrackBar(fControlItems.Items[i]);
      track.SelEnd:= posI;
      if (not track.MouseInThumb) and (not track.Changing) then
      track.Position:= posI;
    end
    else if (fControlItems.Items[i] is TSpTBXLabelItem) and (fControlItems.Items[i].Tag = 21) then
    begin
      TSpTBXLabelItem(fControlItems.Items[i]).Hint:= fMediaPlayer.GetStatusText;
      if (status = mpsPlaying) or (status = mpsPaused) or (status = mpsStopped) or (status = mpsDone) then
      TSpTBXLabelItem(fControlItems.Items[i]).Caption:= s
      else if status <> mpsError then
      TSpTBXLabelItem(fControlItems.Items[i]).Caption:= fMediaPlayer.GetStatusText
      else
      begin
        TSpTBXLabelItem(fControlItems.Items[i]).Caption:= 'Error';
      end;
    end;     
  end;
end;

{##############################################################################}
// TCEQuickViewForm

{-------------------------------------------------------------------------------
  Create an instance of TCEQuickViewForm
-------------------------------------------------------------------------------}
constructor TCEQuickViewForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
  QuickView:= TCEQuickView.Create(Self);
  QuickView.Parent:= Self;
  QuickView.HostWindow:= Self.Handle;
  QuickView.Align:= alClient;
end;

{-------------------------------------------------------------------------------
  CreateParams
-------------------------------------------------------------------------------}
procedure TCEQuickViewForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent:= GetDesktopWindow;
end;                                                           

{-------------------------------------------------------------------------------
  Do Close
-------------------------------------------------------------------------------}
procedure TCEQuickViewForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action:= caFree;
end;

{##############################################################################}
// TCEQuickViewSettings

{-------------------------------------------------------------------------------
  Create instance of TCEQuickViewSettings
-------------------------------------------------------------------------------}
constructor TCEQuickViewSettings.Create;
begin
  inherited;
  // TODO: make the extension settings dynamic for plugin support.
  fImageExtensions:= 'bmp,ico,wmf,emf,jfif,jpg,jpe,jpeg,rle,dib,win,vst,vda,tga,icb,tiff,tif,fax,eps,pcx,pcc,scr,rpf,rla,sgi,rgba,rgb,bw,psd,pdd,ppm,pgm,pbm,cel,pic,pcd,cut,psp,png,gif';
  fTextExtensions:= 'txt,ini,bat,html,htm,pas,css,xml,log,for,php,py,csv';
  fMediaExtensions:= 'avi,wmv,mp4,mpg,mpeg,ogg,ogm,mkv,dvr-ms,mp3,vob,wav,flv';
  fWMPExtensions:= fMediaExtensions;
  fDirectShowExtensions:= fMediaExtensions;
  fMediaPlayer:= mptWMP;
end;

{-------------------------------------------------------------------------------
  CreateMediaEngine
-------------------------------------------------------------------------------}
function TCEQuickViewSettings.CreateMediaEngine(AExtension: WideString):
    ICVMediaEngine;
var
  list: TCCStringList;
begin
  Result:= nil;

  if AExtension = '' then
  Exit;

  if (AExtension[1] = '.') then
  Delete(AExtension, 1, 1);

  list:= TCCStringList.Create;
  try
    list.Delimiter:= ',';
    // images
    list.DelimitedText:= fImageExtensions;
    if list.IndexOf(AExtension) > -1 then
    Result:= TCVImageEngine.Create;
    // text
    list.DelimitedText:= fTextExtensions;
    if list.IndexOf(AExtension) > -1 then
    Result:= TCVMemoEngine.Create;
    // media
    if fMediaPlayer <> mptAuto then
    begin
      list.DelimitedText:= fMediaExtensions;
      if list.IndexOf(AExtension) > -1 then
      begin
        if fMediaPlayer = mptWMP then
        Result:= TCVWMPEngine.Create
        else if fMediaPlayer = mptDirectShow then
        Result:= TCVDSEngine.Create;
      end;
    end
    else
    begin
      // wmp
      list.DelimitedText:= fWMPExtensions;
      if list.IndexOf(AExtension) > -1 then
      Result:= TCVWMPEngine.Create;

      // directshow
      if not assigned(Result) then
      begin
        list.DelimitedText:= fDirectShowExtensions;
        if list.IndexOf(AExtension) > -1 then
        Result:= TCVDSEngine.Create;
      end;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  IsSupported
-------------------------------------------------------------------------------}
function TCEQuickViewSettings.IsSupported(AExtension: WideString): Boolean;
var
  list: TCCStringList;
begin
  Result:= false;
  
  if AExtension = '' then
  Exit;

  if (AExtension[1] = '.') then
  Delete(AExtension, 1, 1);

  list:= TCCStringList.Create;
  try
    list.Delimiter:= ',';
    
    if fMediaPlayer = mptAuto then
    list.DelimitedText:= fImageExtensions +','+ fTextExtensions +','+ fWMPExtensions +','+ fDirectShowExtensions
    else
    list.DelimitedText:= fImageExtensions +','+ fTextExtensions +','+ fMediaExtensions;
    
    Result:= list.IndexOf(AExtension) > -1;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Set DirectShow Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetDirectShowExtensions(const Value: WideString);
begin
  fDirectShowExtensions:= WideLowerCase(Value);
end;

{-------------------------------------------------------------------------------
  Set Image Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetImageExtensions(const Value: WideString);
begin
  fImageExtensions:= WideLowerCase(Value);
end;

{-------------------------------------------------------------------------------
  Set Media Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetMediaExtensions(const Value: WideString);
begin
  fMediaExtensions:= WideLowerCase(Value);
end;

{-------------------------------------------------------------------------------
  Set Text Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetTextExtensions(const Value: WideString);
begin
  fTextExtensions:= WideLowerCase(Value);
end;

{-------------------------------------------------------------------------------
  Set WMP Extensions
-------------------------------------------------------------------------------}
procedure TCEQuickViewSettings.SetWMPExtensions(const Value: WideString);
begin
  fWMPExtensions:= WideLowerCase(Value);
end;

{##############################################################################}

initialization
  GlobalQuickViewSettings:= TCEQuickViewSettings.Create;
  GlobalAppSettings.AddItem('QuickView', GlobalQuickViewSettings, true);

finalization
  FreeAndNil(GlobalQuickViewSettings);

end.
