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
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd);
  protected
    fBuffer: TBitmap;
    fIconHeight: Integer;
    fIconRect: TRect;
    fIconType: TCEIconType;
    fIconWidth: Integer;
    fInfoList: TObjectList;
    fInfoRect: TRect;
    fLatestNS: TNamespace;
    fLatestThread: TCEThumbLoadThread;
    fThumbHeight: Integer;
    fThumbnailBuffer: TBitmap;
    fThumbnailFound: Boolean;
    fThumbWidth: Integer;
    procedure BuildInfoList; virtual;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DrawBuffer; virtual;
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
  fRowHeight:= 18;
  fThumbnailBuffer:= TBitmap.Create;
  fBuffer:= TBitmap.Create;
  fInfoList:= TObjectList.Create(true);
  fAutoRefreshThumbnail:= false;
  fShowFolderItemCount:= true;
  fCalculateHiddenItems:= false;
  Resize;
end;

{-------------------------------------------------------------------------------
  Destroy TCEInfoBar
-------------------------------------------------------------------------------}
destructor TCEInfoBar.Destroy;
begin
  if assigned(fLatestNS) then
  fLatestNS.Free;
  fThumbnailBuffer.Free;
  fBuffer.Free;
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
var
  row_count: Integer;
  s: Single;
begin
  Result:= NewHeight > fRowHeight;
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
  fThumbnailBuffer.SetSize(0,0);
  if assigned(fLatestNS) then
  FreeAndNil(fLatestNS);
  fThumbnailFound:= false;
  fInfoList.Clear;
  DrawBuffer;
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
  Draw Buffer
-------------------------------------------------------------------------------}
procedure TCEInfoBar.DrawBuffer;

  procedure DrawIcon(ACanvas: TCanvas; ARect: TRect);
  var
    bit: TBitmap;
    l,t,w,h: Integer;
  begin
    if assigned(fLatestNS) then
    begin
      bit:= TBitmap.Create;
      try
        // get system icon
        case fIconType of
          itSmallIcon: SmallSysImages.GetBitmap(fLatestNS.GetIconIndex(false, icSmall), bit);
          itLargeIcon: LargeSysImages.GetBitmap(fLatestNS.GetIconIndex(false, icLarge), bit);
          itExtraLargeIcon: ExtraLargeSysImages.GetBitmap(fLatestNS.GetIconIndex(false, icLarge), bit);
          itJumboIcon: JumboSysImages.GetBitmap(fLatestNS.GetIconIndex(false, icLarge), bit);
        end;

        l:= ARect.Left;
        t:= ARect.Top;
        w:= ARect.Right - ARect.Left;
        h:= ARect.Bottom - ARect.Top;

        SetStretchBltMode(ACanvas.Handle, HALFTONE);
        SetBrushOrgEx(ACanvas.Handle, 0,0, nil);
        StretchBlt(ACanvas.Handle, l,t,w,h, bit.Canvas.Handle, 0,0,bit.Width,bit.Height, SRCCOPY);
      finally
        bit.Free;
      end;
    end;
  end;

  procedure DrawThumbnail(ACanvas: TCanvas; ARect: TRect);
  var
    ar: Single;
    l,t,w,h: Integer;
    bit: TBitmap;
  begin
    if (fThumbnailBuffer.Width = 0) or (fThumbnailBuffer.Height = 0) then
    Exit;
    // Resize thumbnail
    fThumbWidth:= ARect.Right - ARect.Left;
    fThumbHeight:= ARect.Bottom - ARect.Top;
    if fThumbnailBuffer.Width > fThumbnailBuffer.Height then
    begin
      ar:= fThumbnailBuffer.Height / fThumbnailBuffer.Width;
      fThumbHeight:= Round(fThumbWidth * ar);
    end
    else
    begin
      ar:= fThumbnailBuffer.Width / fThumbnailBuffer.Height;
      fThumbWidth:= Round(fThumbHeight * ar);
    end;
    fResizedThumbnail:= (fThumbWidth <> fThumbnailBuffer.Width) or (fThumbHeight <> fThumbnailBuffer.Height);

    w:= ARect.Right - ARect.Left;
    h:= ARect.Bottom - ARect.Top;
    l:= Round((w - fThumbWidth) / 2) + ARect.Left;
    t:= Round((h - fThumbHeight) / 2) + ARect.Top;

    SetStretchBltMode(ACanvas.Handle, HALFTONE);
    SetBrushOrgEx(ACanvas.Handle, 0,0, nil);
    StretchBlt(ACanvas.Handle, l,t,fThumbWidth,fThumbHeight, fThumbnailBuffer.Canvas.Handle, 0,0,fThumbnailBuffer.Width,fThumbnailBuffer.Height, SRCCOPY);
  end;

var
  Flags: Integer;
  l,t,i, row, col_num, row_num, col: Integer;
  r: TRect;
  item: TCEInfoItem;
  ws: WideString;
  size, prefix_size: TSize;
begin
  if csDestroying in Self.ComponentState then
  Exit;

  fBuffer.Canvas.Lock;
  try
    // Set buffer size
    fBuffer.SetSize(Width,Height);

    // Paint Background
    fBuffer.Canvas.Brush.Color:= clWindow;
    fBuffer.Canvas.FillRect(Rect(0,0, Width, Height));

    r:= fInfoRect;

    if fInfoList.Count > 0 then
    begin
      // Paint Thumbnail/Icon
      if fThumbnailFound and (Height > 16) then
      begin
        DrawThumbnail(fBuffer.Canvas, fIconRect);
      end
      else
      begin
        DrawIcon(fBuffer.Canvas, fIconRect);
      end;

      // Single row
      if Height < (RowHeight * 2) then 
      begin
        // Name
        fBuffer.Canvas.Font.Style:= [];
        fBuffer.Canvas.Font.Color:= clWindowText;
        item:= TCEInfoItem(fInfoList.Items[0]);
        size:= WideCanvasTextExtent(fBuffer.Canvas, item.Text);
        SpDrawXPText(fBuffer.Canvas, item.Text, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);

        r.Left:= r.Left + size.cx + 4;

        // Other infos
        fBuffer.Canvas.Font.Color:= clGrayText;
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
        SpDrawXPText(fBuffer.Canvas, ws, r, DT_END_ELLIPSIS  or DT_SINGLELINE or DT_VCENTER);
      end
      // Multiple rows
      else 
      begin
        // Name
        r.Bottom:= r.Top + RowHeight-2;
        fBuffer.Canvas.Font.Style:= [fsBold];
        fBuffer.Canvas.Font.Color:= clWindowText;
        item:= TCEInfoItem(fInfoList.Items[0]);
        size:= WideCanvasTextExtent(fBuffer.Canvas, item.Text);
        SpDrawXPText(fBuffer.Canvas, item.Text, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);

        // Other Infos
        fBuffer.Canvas.Font.Style:= [];
        r.Left:= fInfoRect.Left;
        r.Top:= r.Bottom;
        r.Bottom:= r.Top + RowHeight - 3;
        row:= 2;
        row_num:= Max(1,Height div RowHeight);
        col:= 1;
        col_num:= Ceil((fInfoList.Count - 1) / row_num);
        if col_num < 1 then
        col_num:= 1;

        for i:= 1 to fInfoList.Count - 1 do
        begin
          item:= TCEInfoItem(fInfoList.Items[i]);
          if (item.Text <> '') or (item.Prefix <> '') then
          begin
            // get text sizes
            size:= WideCanvasTextExtent(fBuffer.Canvas, item.Text);
            if item.Prefix <> '' then
            begin
              prefix_size:= WideCanvasTextExtent(fBuffer.Canvas, item.Prefix + ':');
              prefix_size.cx:= prefix_size.cx + 2;
            end
            else
            prefix_size.cx:= 0;

            // go to row
            if (((r.Right - r.Left) < (size.cx + prefix_size.cx)) and (row < row_num) and (col_num > 1))
               or ((col > col_num) and (row <= row_num)) then
            begin
              if ((fInfoRect.Bottom - fInfoRect.Top) + Max(prefix_size.cy, size.cy)) >= (((row+1) * RowHeight)) then
              begin
                row:= row + 1;
                col:= 1;
                r.Left:= fInfoRect.Left;
                r.Top:= r.Bottom;
                r.Bottom:= r.Top + RowHeight - 3;
              end;
            end;

            // draw text
            fBuffer.Canvas.Font.Color:= clGrayText;
            if prefix_size.cx > 0 then
            begin
              if size.cx > 0 then
              SpDrawXPText(fBuffer.Canvas, item.Prefix + ':', r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER)
              else
              SpDrawXPText(fBuffer.Canvas, item.Prefix, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);
              r.Left:= r.Left + prefix_size.cx;
            end;
            fBuffer.Canvas.Font.Color:= clWindowText;
            if size.cx > 0 then
            begin
              SpDrawXPText(fBuffer.Canvas, item.Text, r, DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER);
            end;
            r.Left:= r.Left + size.cx + 10;
            col:= col + 1;
          end;
        end;
      end;
    end
    else
    begin
      fBuffer.Canvas.Font.Color:= clGrayText;
      r.Left:= 10;
      r.Top:= 10;
      SpDrawXPText(fBuffer.Canvas, 'No Selection', r, DT_END_ELLIPSIS);
    end;
  finally
    fBuffer.Canvas.Unlock;
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
    DrawBuffer;
    Self.Repaint;

    RunThumbnailThread;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Paint
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Paint;
begin
  if csDestroying in Self.ComponentState then
  Exit;

  BitBlt(Canvas.Handle, 0, 0, Width, Height, fBuffer.Canvas.Handle, 0, 0, SRCCOPY);
end;

{-------------------------------------------------------------------------------
  Refresh Thumbnail
-------------------------------------------------------------------------------}
procedure TCEInfoBar.RefreshThumbnail;
var
  ar: Single;
begin
  if (Height > 16) and ((fThumbHeight > fThumbnailBuffer.Height) or (fThumbWidth > fThumbnailBuffer.Width)) then
  RunThumbnailThread;
end;

{-------------------------------------------------------------------------------
  Handle Resize
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Resize;
var
  old_height: Integer;
  w, h: Integer;
  bit: TBitmap;
  ar: Single;
begin
  inherited;
  if csDestroying in Self.ComponentState then
  Exit;

  // Calculate icon rect
  if Height < 22 then
  begin
    fIconType:= itSmallIcon;
    fIconRect:= Rect(3, 1, Height+1, Height+1);
  end
  else
  begin
    if (Height > 22) and (Height < 39) then
    fIconType:= itLargeIcon
    else if (Height > 38) and (Height < 55) then
    fIconType:= itExtraLargeIcon
    else
    fIconType:= itJumboIcon;
    fIconRect:= Rect(3,3, Height-3,Height-3);
  end;
  fIconWidth:= fIconRect.Right - fIconRect.Left;
  fIconHeight:= fIconRect.Bottom - fIconRect.Top;

  // Calculate info rect
  fInfoRect:= fIconRect;
  fInfoRect.Left:= fIconRect.Right + 4;
  fInfoRect.Right:= Width - 3;

  DrawBuffer;

//  if AutoRefreshThumbnail and fResizedThumbnail then
//  RunThumbnailThread;
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
  Get's called when thread has finished
-------------------------------------------------------------------------------}
procedure TCEInfoBar.ThreadFinished(AThread: TCEThumbLoadThread);
begin
  if AThread = fLatestThread then
  begin
    fThumbnailFound:= not AThread.Terminated and AThread.ThumbnailFound;
    if fThumbnailFound then
    begin
      fThumbnailBuffer.SetSize(AThread.Thumbnail.Width, AThread.Thumbnail.Height);
      fThumbnailBuffer.Canvas.Brush.Color:= clWindow;
      fThumbnailBuffer.Canvas.FillRect(Rect(0,0,fThumbnailBuffer.Width,fThumbnailBuffer.Height));
      fThumbnailBuffer.Canvas.Draw(0,0,AThread.Thumbnail);
      DrawBuffer;
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
