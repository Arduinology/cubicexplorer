unit CE_InfoBar;

interface

uses
  // SpTBX
  SpTBXSkins, SpTBXItem, SpTBXControls,
  // TNT
  TntGraphics, TntClasses,
  // VSTools
  MPShellUtilities, MPShellTypes, MPCommonObjects,
  // System Units
  ExtCtrls, Classes, Controls, Windows, Graphics, Messages, Contnrs,
  Forms, Math, SysUtils, ShlObj, ActiveX, UxTheme, Themes, GraphicEx;

type
  TCEIconType = (itSmallIcon, itLargeIcon, itExtraLargeIcon, itJumboIcon);

  TCEInfoBar = class;

  TCEThumbLoadThread = class(TThread)
  protected
    InfoBar: TCEInfoBar;
    procedure SyncFinished; virtual;
    procedure Execute; override;
  public
    PIDL: PItemIDList;
    Thumbnail: TBitmap;
    ThumbnailFound: Boolean;
    ThumbnailSize: TSize;
  end;

  TCEInfoBar = class(TCustomControl)
  private
    fAutoRefreshThumbnail: Boolean;
    fCalculateHiddenItems: Boolean;
    fRowHeight: Integer;
    fShowFolderItemCount: Boolean;
    fZoomLevel: Integer;
    procedure SetZoomLevel(const Value: Integer);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd);
  protected
    fIcon: TBitmap;
    fIconRect: TRect;
    fIconType: TCEIconType;
    fInfoList: TObjectList;
    fInfoRect: TRect;
    fLatestNS: TNamespace;
    fLatestThread: TCEThumbLoadThread;
    fThumbnail: TBitmap;
    fThumbnailFound: Boolean;
    procedure BuildInfoList; virtual;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawIcon; virtual;
    procedure RunThumbnailThread; virtual;
    procedure ThreadFinished(AThread: TCEThumbLoadThread); virtual;
  public
    fResizedThumbnail: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddInfoItem(AText: WideString; APrefix: WideString): Integer; virtual;
    procedure Clear;
    procedure LoadFromPIDL(APIDL: PItemIDList); virtual;
    procedure Paint; override;
    procedure RefreshThumbnail;
    procedure Resize; override;
    property AutoRefreshThumbnail: Boolean read fAutoRefreshThumbnail write
        fAutoRefreshThumbnail;
    property CalculateHiddenItems: Boolean read fCalculateHiddenItems write
        fCalculateHiddenItems;
    property Font;
    property RowHeight: Integer read fRowHeight write fRowHeight;
    property ShowFolderItemCount: Boolean read fShowFolderItemCount write
        fShowFolderItemCount;
    property ZoomLevel: Integer read fZoomLevel write SetZoomLevel;
  end;

  TCustomControlHack = class(TCustomControl);

  TCEInfoItem = class(TObject)
  private    
    fPrefix: WideString;
  public
    fText: WideString;
    ItemRect: TRect;
    constructor Create;
    destructor Destroy; override;
    property Prefix: WideString read fPrefix write fPrefix;
    property Text: WideString read fText write fText;
  end;



implementation

uses
  CE_VistaFuncs;

{-------------------------------------------------------------------------------
  Create an instance of TCEInfoBar
-------------------------------------------------------------------------------}
constructor TCEInfoBar.Create(AOwner: TComponent);
var
  ACanvas: TControlCanvas;
begin
  inherited;
  SetDesktopIconFonts(Canvas.Font);
  fZoomLevel:= 1;
  fRowHeight:= 18;
  Height:= fRowHeight * fZoomLevel;
  fIcon:= TBitmap.Create;
  fThumbnail:= TBitmap.Create;
  fInfoList:= TObjectList.Create(true);
  fAutoRefreshThumbnail:= false;
  fShowFolderItemCount:= true;
  fCalculateHiddenItems:= false;
end;

{-------------------------------------------------------------------------------
  Destroy TCEInfoBar
-------------------------------------------------------------------------------}
destructor TCEInfoBar.Destroy;
begin
  if assigned(fLatestNS) then
  fLatestNS.Free;
  fIcon.Free;
  fThumbnail.Free;
  fInfoList.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add new Info Item
-------------------------------------------------------------------------------}
function TCEInfoBar.AddInfoItem(AText: WideString; APrefix: WideString):
    Integer;
var
  item: TCEInfoItem;
begin
  item:= TCEInfoItem.Create;
  item.Text:= AText;
  item.Prefix:= APrefix;
  Result:= fInfoList.Add(item);
end;

{-------------------------------------------------------------------------------
  Build InfoList
-------------------------------------------------------------------------------}
procedure TCEInfoBar.BuildInfoList;
var
  EnumList: IEnumIDList;
  NewItem: PItemIDList;
  Dummy: Cardinal;
  Flags: Cardinal;
  list: TTntStrings;
  i: Integer;
begin
  list:= TTntStringList.Create;
  list.NameValueSeparator:= ':';
  try
    fInfoList.Clear;
    // Add Name
    AddInfoItem(fLatestNS.NameNormal, '');
    // Add Folder Item Count
    if fShowFolderItemCount and fLatestNS.Folder then
    begin
      i:= 0;
      if CalculateHiddenItems then
      Flags:= SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN
      else
      Flags:= SHCONTF_FOLDERS or SHCONTF_NONFOLDERS;
      

      if fLatestNS.ShellFolder.EnumObjects(0, Flags, EnumList) = S_OK then
      begin
        while EnumList.Next(1, NewItem, Dummy) = S_OK do
        begin
          i:= i + 1;
          PIDLMgr.FreePIDL(NewItem);
        end;
      end;
      AddInfoItem(IntToStr(i) + ' ' + 'Item(s)', '');
    end;
    // Add Info Query items
    list.Text:= fLatestNS.InfoTip;
    for i:= list.Count - 1 downto 0 do
    begin
      if list.Names[i] = '' then
      AddInfoItem('', list.Strings[i])
      else
      AddInfoItem(Trim(list.ValueFromIndex[i]), list.Names[i]);
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Handle CanResize
-------------------------------------------------------------------------------}
function TCEInfoBar.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result:= (NewHeight mod fRowHeight) = 0;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Clear;
begin
  if assigned(fLatestThread) then
  begin
    fLatestThread.Terminate;
  end;
  fIcon.SetSize(0,0);
  fThumbnail.SetSize(0,0);
  if assigned(fLatestNS) then
  FreeAndNil(fLatestNS);
  fThumbnailFound:= false;
  fInfoList.Clear;
  Paint;
end;

{-------------------------------------------------------------------------------
  Handle CreateParams
-------------------------------------------------------------------------------}
procedure TCEInfoBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if not (csDesigning in ComponentState) then begin
    with Params do
      Style := Style or WS_CLIPCHILDREN;
    with Params.WindowClass do
      Style := Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

{-------------------------------------------------------------------------------
  Draw Icon
-------------------------------------------------------------------------------}
procedure TCEInfoBar.DrawIcon;
var
  bit: TBitmap;
begin
  if assigned(fLatestNS) then
  begin
    fIcon.SetSize(fIconRect.Right - fIconRect.Left, fIconRect.Bottom - fIconRect.Top);

    // Clear background
    fIcon.Canvas.Brush.Color:= clWindow;
    fIcon.Canvas.FillRect(fIcon.Canvas.ClipRect);

    // Draw icon
    bit:= TBitmap.Create;
    try
      case fIconType of
        itSmallIcon: SmallSysImages.GetBitmap(fLatestNS.GetIconIndex(false, icSmall), bit);
        itLargeIcon: LargeSysImages.GetBitmap(fLatestNS.GetIconIndex(false, icLarge), bit);
        itExtraLargeIcon: ExtraLargeSysImages.GetBitmap(fLatestNS.GetIconIndex(false, icLarge), bit);
        itJumboIcon: JumboSysImages.GetBitmap(fLatestNS.GetIconIndex(false, icLarge), bit);
      end;
      Stretch(fIcon.Width, fIcon.Height, sfLanczos3, 0, bit, fIcon);
    finally
      bit.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Load From PIDL
-------------------------------------------------------------------------------}
procedure TCEInfoBar.LoadFromPIDL(APIDL: PItemIDList);
begin
  if csDestroying in Self.ComponentState then
  exit;

  if assigned(fLatestNS) then
  begin
    if PIDLMgr.EqualPIDL(APIDL, fLatestNS.AbsolutePIDL) then
    begin
      BuildInfoList; // Refresh infolist
      Paint;
      Exit;
    end;
  end;

  // Terminate previous thread if present
  if assigned(fLatestThread) then
  begin
    fLatestThread.Terminate;
  end;
  
  if assigned(APIDL) then
  begin
    // Create namespace
    if assigned(fLatestNS) then
    fLatestNS.Free;
    fLatestNS:= TNamespace.Create(PIDLMgr.CopyPIDL(APIDL), nil);
    fLatestNS.FreePIDLOnDestroy:= true;

    // Build InfoList
    BuildInfoList;
    
    // Draw normal Icon first
    fThumbnailFound:= false;
    Resize;
    DrawIcon;
    Paint;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Paint
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Paint;
var
  Flags: Integer;
  l,t,i, row, col_num, col: Integer;
  r: TRect;
  item: TCEInfoItem;
  ws: WideString;
  size, prefix_size: TSize;
begin
  if csDestroying in Self.ComponentState then
  Exit;

  Canvas.Lock;
  try
    // Paint Background
    Canvas.Brush.Color:= clWindow;
    Canvas.FillRect(Canvas.ClipRect);

    // Paint Thumbnail
    if fThumbnailFound and (fZoomLevel > 1) then
    begin
      l:= Round(((fIconRect.Right - fIconRect.Left) - fThumbnail.Width) / 2) + fIconRect.Left;
      t:= Round(((fIconRect.Bottom - fIconRect.Top) - fThumbnail.Height) / 2) + fIconRect.Top;
      Canvas.Draw(l, t, fThumbnail);
    end
    else
    Canvas.Draw(fIconRect.Left, fIconRect.Top, fIcon);

    // Paint Info Items
    if fInfoList.Count > 0 then
    begin
      r:= fInfoRect;
      if fZoomLevel = 1 then // Single row
      begin
        // Name
        Canvas.Font.Style:= [];
        Canvas.Font.Color:= clWindowText;
        item:= TCEInfoItem(fInfoList.Items[0]);
        size:= WideCanvasTextExtent(Canvas, item.Text);
        SpDrawXPText(Canvas, item.Text, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);

        r.Left:= r.Left + size.cx + 4;

        // Other infos
        Canvas.Font.Color:= clGrayText;
        ws:= '';
        for i:= 1 to fInfoList.Count - 1 do
        begin
          item:= TCEInfoItem(fInfoList.Items[i]);
          if item.Text <> '' then
          begin
            ws:= ws + ' | ' + item.Text;
          end
          else if item.Prefix <> '' then
          begin
            ws:= ws + ' | ' + item.Prefix;
          end;
        end;
        SpDrawXPText(Canvas, ws, r, DT_END_ELLIPSIS  or DT_SINGLELINE or DT_VCENTER);
      end
      else // Multiple rows
      begin
        // Name
        r.Bottom:= r.Top + RowHeight-2;
        Canvas.Font.Style:= [fsBold];
        Canvas.Font.Color:= clWindowText;
        item:= TCEInfoItem(fInfoList.Items[0]);
        size:= WideCanvasTextExtent(Canvas, item.Text);
        SpDrawXPText(Canvas, item.Text, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);

        Canvas.Font.Style:= [];
        r.Left:= fInfoRect.Left;
        r.Top:= r.Bottom;
        r.Bottom:= r.Top + RowHeight - 3;
        row:= 2;
        col:= 1;
        col_num:= Ceil((fInfoList.Count - 1) / (fZoomLevel - 1));
        if col_num < 1 then
        col_num:= 1;

        for i:= 1 to fInfoList.Count - 1 do
        begin
          item:= TCEInfoItem(fInfoList.Items[i]);
          if (item.Text <> '') or (item.Prefix <> '') then
          begin
            size:= WideCanvasTextExtent(Canvas, item.Text);
            if item.Prefix <> '' then
            begin
              prefix_size:= WideCanvasTextExtent(Canvas, item.Prefix + ':');
              prefix_size.cx:= prefix_size.cx + 2;
            end
            else
            prefix_size.cx:= 0;
            
            if (((r.Right - r.Left) < (size.cx + prefix_size.cx)) and (row < ZoomLevel) and (col_num > 1))
               or ((col > col_num) and (row <= ZoomLevel)) then
            begin
              row:= row + 1;
              col:= 1;
              r.Left:= fInfoRect.Left;
              r.Top:= r.Bottom;
              r.Bottom:= r.Top + RowHeight - 3;
            end;

            Canvas.Font.Color:= clGrayText;
            if prefix_size.cx > 0 then
            begin
              if size.cx > 0 then
              SpDrawXPText(Canvas, item.Prefix + ':', r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER)
              else
              SpDrawXPText(Canvas, item.Prefix, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);
              r.Left:= r.Left + prefix_size.cx;
            end;
            Canvas.Font.Color:= clWindowText;
            if size.cx > 0 then
            begin
              SpDrawXPText(Canvas, item.Text, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);
            end;
            r.Left:= r.Left + size.cx + 10;
            col:= col + 1;
          end;
        end;
      end;
    end;
  finally
    Canvas.Unlock;
  end;
end;

{-------------------------------------------------------------------------------
  Refresh Thumbnail
-------------------------------------------------------------------------------}
procedure TCEInfoBar.RefreshThumbnail;
begin
  if fThumbnailFound and (ZoomLevel > 1) and fResizedThumbnail then
  RunThumbnailThread;
end;

{-------------------------------------------------------------------------------
  Handle Resize
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Resize;
var
  old_level: Integer;
  w, h: Integer;
  bit: TBitmap;
  ar: Single;
begin
  inherited;
  if csDestroying in Self.ComponentState then
  Exit;

  old_level:= fZoomLevel;

  // Calculate Zoom Level
  if Height > fRowHeight then
  fZoomLevel:= Height div fRowHeight
  else
  fZoomLevel:= 1;

  // Calculate icon rect
  if fZoomLevel = 1 then
  begin
    fIconType:= itSmallIcon;
    fIconRect:= Rect(3, 1, Height+1, Height-1);
  end
  else
  begin
    if ZoomLevel = 2 then
    fIconType:= itLargeIcon
    else
    fIconType:= itExtraLargeIcon;
    fIconRect:= Rect(3,3,Height-3,Height-3);
  end;

  // Calculate info rect
  fInfoRect:= fIconRect;
  fInfoRect.Left:= fIconRect.Right + 4;
  fInfoRect.Right:= Width - 3;

  // Resize thumbnail
  if fThumbnailFound then
  begin
    w:= fIconRect.Right - fIconRect.Left;
    h:= fIconRect.Bottom - fIconRect.Top;
    if fThumbnail.Width > fThumbnail.Height then
    begin
      ar:= fThumbnail.Height / fThumbnail.Width;
      h:= Round(w * ar);
    end
    else
    begin
      ar:= fThumbnail.Width / fThumbnail.Height;
      w:= Round(h * ar);
    end;
    
    if (fThumbnail.Width <> w) or (fThumbnail.Height <> h) then
    begin

      
      bit:= TBitmap.Create;
      try
        Stretch(w, h, sfLanczos3, 0, fThumbnail, bit);
        fThumbnail.Assign(bit);
        fResizedThumbnail:= true;
      finally
        bit.Free;
      end;
      if AutoRefreshThumbnail then
      RunThumbnailThread;
    end;
  end
  else if (fZoomLevel > 1) then
  begin
    RunThumbnailThread;
  end;

  // Draw Icon
  if old_level <> fZoomLevel then
  begin
    DrawIcon;
    Paint;
  end;
end;

{-------------------------------------------------------------------------------
  Run Thumbnail Thread
-------------------------------------------------------------------------------}
procedure TCEInfoBar.RunThumbnailThread;
begin
  if assigned(fLatestNS) then
  begin
    if CompareText(fLatestNS.Extension, '.ico') = 0 then
    Exit;
    
    fLatestThread:= TCEThumbLoadThread.Create(true);
    fLatestThread.FreeOnTerminate:= true;
    fLatestThread.InfoBar:= Self;
    fLatestThread.PIDL:= PIDLMgr.CopyPIDL(fLatestNS.AbsolutePIDL);
    fLatestThread.ThumbnailSize.cx:= fIconRect.Right - fIconRect.Left;
    fLatestThread.ThumbnailSize.cy:= fIconRect.Bottom - fIconRect.Top;
    fLatestThread.Resume;
  end;
end;

{-------------------------------------------------------------------------------
  Set Zoom Level
-------------------------------------------------------------------------------}
procedure TCEInfoBar.SetZoomLevel(const Value: Integer);
begin
  if Value > 0 then
  begin
    fZoomLevel:= Value;
    Height:= fRowHeight * fZoomLevel;
  end;
end;

{-------------------------------------------------------------------------------
  Get's called when thread has finished
-------------------------------------------------------------------------------}
procedure TCEInfoBar.ThreadFinished(AThread: TCEThumbLoadThread);
begin
  if AThread = fLatestThread then
  begin
    fThumbnailFound:= not AThread.Terminated and AThread.ThumbnailFound;
    if fThumbnailFound then
    begin
      fThumbnail.Assign(AThread.Thumbnail);
      fResizedThumbnail:= false;
      Paint;
    end;
    fLatestThread:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  Handle WM_EraseBkgnd message (Don't erase background)
-------------------------------------------------------------------------------}
procedure TCEInfoBar.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result:= 1;
end;

{##############################################################################}
// TCEThumbLoadThread

{-------------------------------------------------------------------------------
  Execute ThumbLoadThread
-------------------------------------------------------------------------------}
procedure TCEThumbLoadThread.Execute;
var
  Bits: HBITMAP;
  ns: TNamespace;
begin
  CoInitialize(nil); // <- needed for video thumbnails
  Thumbnail:= TBitmap.Create;
  Thumbnail.PixelFormat:= pf32bit;
  Thumbnail.Transparent:= true;
  ns:= TNamespace.Create(PIDL, nil);
  ns.FreePIDLOnDestroy:= true;
  ThumbnailFound:= false;
  try
    // Extract Image
    if Assigned(ns.ExtractImage.ExtractImageInterface) then
    begin
      ns.ExtractImage.Width:= ThumbnailSize.cx;
      ns.ExtractImage.Height:= ThumbnailSize.cy;
      ns.ExtractImage.ColorDepth:= 32;
      ns.ExtractImage.Flags:= IEIFLAG_SCREEN or IEIFLAG_QUALITY;
      ns.ExtractImage.ImagePath;
      if Succeeded(ns.ExtractImage.ExtractImageInterface.Extract(Bits)) then
      begin
        Thumbnail.Handle:= Bits;
        ThumbnailFound:= true;
      end;
    end;
    // Synchronize end results
    Synchronize(SyncFinished);
  finally
    Thumbnail.Free;
    ns.Free;
  end;
end;

{-------------------------------------------------------------------------------
  SyncFinished
-------------------------------------------------------------------------------}
procedure TCEThumbLoadThread.SyncFinished;
begin
  InfoBar.ThreadFinished(Self);
end;

{##############################################################################}
// TCEInfoItem

{-------------------------------------------------------------------------------
  Create an instance of TCEInfoItem
-------------------------------------------------------------------------------}
constructor TCEInfoItem.Create;
begin
  inherited;
end;

{-------------------------------------------------------------------------------
  Destroy TCEInfoItem
-------------------------------------------------------------------------------}
destructor TCEInfoItem.Destroy;
begin
  inherited;
end;

end.
