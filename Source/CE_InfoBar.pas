unit CE_InfoBar;

interface

uses
  // CE Units
  CE_LanguageEngine,
  // SpTBX
  SpTBXSkins,
  // TNT
  TntGraphics,
  // VSTools
  MPShellUtilities, MPShellTypes, MPCommonObjects,
  // System Units
  ExtCtrls, Classes, Controls, Windows, Graphics, Messages, Contnrs,
  Forms, Math, SysUtils, ShlObj, ActiveX;

type
  TCustomControlHack = class(TCustomControl);
  
  TCEInfoItem = class(TObject)
  private    
    fOwner: TCustomControl;
    fTextHeight: Integer;
    fTextWidth: Integer;
    procedure SetText(const Value: WideString);
  public
    fText: WideString;
    ItemRect: TRect;
    constructor Create;
    destructor Destroy; override;
    property Owner: TCustomControl read fOwner write fOwner;
    property Text: WideString read fText write SetText;
    property TextHeight: Integer read fTextHeight;
    property TextWidth: Integer read fTextWidth;
  end;

  TCEThumbImage = class;

  TCEInfoBar = class(TCustomControl)
  private
    fCalculateHiddenItems: Boolean;
    fThumbImageRect, fInfoItemsRect: TRect;
    fRowHeight, fRowNum: Integer;
    fColWidth, fColNum: Integer;
    fCurrentNamespace: TNamespace;
    fInfoList: TObjectList;
    procedure SetCalculateHiddenItems(const Value: Boolean);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    fDefaultImage: TBitmap;
    procedure CalcPositions; virtual;
    procedure OnThumbImageChanged(Sender: TObject);
  public
    ThumbImage: TCEThumbImage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddInfoItem(AText: WideString): Integer;
    procedure Clear;
    function LoadInfoFrom(APIDL: PItemIDList): Boolean;
    procedure Paint; override;
    procedure Resize; override;
    property CalculateHiddenItems: Boolean read fCalculateHiddenItems write
        SetCalculateHiddenItems;
    property Canvas;
    property Font;
    property CurrentNamespace: TNamespace read fCurrentNamespace;
    property InfoList: TObjectList read fInfoList;
  end;

  TCEThumbLoadThread = class(TThread)
  protected
    ThumbImage: TCEThumbImage;
    procedure Execute; override;
    procedure SyncFinished; virtual;
  public
    PIDL: PItemIDList;
    Thumbnail: TBitmap;
    ThumbnailFound: Boolean;
    ThumbnailSize: TSize;
  end;

  TThumbImageStatus = (tisDefault, tisLoading, tisThumbnail);

  TCEThumbImage = class(TObject)
  private
    fOnChanged: TNotifyEvent;
    fStatus: TThumbImageStatus;
  protected
    fLatestThread: TCEThumbLoadThread;
    fThumbnail: TBitmap;
    procedure DoChanged; virtual;
    procedure ThreadFinished(AThread: TCEThumbLoadThread); virtual;
  public
    ThumbnailSize: TSize;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure OpenFile(AFilePath: WideString; NoStatusChange: Boolean = false);
    procedure OpenPIDL(APIDL: PItemIDList; NoStatusChange: Boolean = false);
  published
    property Status: TThumbImageStatus read fStatus;
    property Thumbnail: TBitmap read fThumbnail;
    property OnChanged: TNotifyEvent read fOnChanged write fOnChanged;
  end;



implementation

{##############################################################################}
// TCEInfoBar

{-------------------------------------------------------------------------------
  Create an instance of TCEInfoBar
-------------------------------------------------------------------------------}
constructor TCEInfoBar.Create(AOwner: TComponent);
begin
  inherited;
  fInfoList:= TObjectList.Create(true);
  ThumbImage:= TCEThumbImage.Create;
  ThumbImage.OnChanged:= OnThumbImageChanged;
  fDefaultImage:= TBitmap.Create;
  fCalculateHiddenItems:= false;
end;

{-------------------------------------------------------------------------------
  Destroy TCEInfoBar
-------------------------------------------------------------------------------}
destructor TCEInfoBar.Destroy;
begin
  Clear;
  InfoList.Free;
  ThumbImage.Free;
  fDefaultImage.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Add new Info Item
-------------------------------------------------------------------------------}
function TCEInfoBar.AddInfoItem(AText: WideString): Integer;
var
  item: TCEInfoItem;
begin
  item:= TCEInfoItem.Create;
  item.Owner:= Self;
  item.Text:= AText;
  Result:= InfoList.Add(item);
end;

{-------------------------------------------------------------------------------
  Calculate draw positions
-------------------------------------------------------------------------------}
procedure TCEInfoBar.CalcPositions;
var
  i: Integer;
begin
  fThumbImageRect:= Rect(4,4,Self.Height - 4, Self.Height - 4);
  fInfoItemsRect:= Rect(fThumbImageRect.Right + 4, fThumbImageRect.Top,
                        Self.Width - 4, Self.Height - 4);
  
  fRowHeight:= 0;
  if InfoList.Count > 0 then
  begin
    // Calc row height and number of rows
    for i:= 0 to InfoList.Count - 1 do
    begin
      if TCEInfoItem(InfoList.Items[i]).TextHeight > fRowHeight then
      fRowHeight:= TCEInfoItem(InfoList.Items[i]).TextHeight;
    end;
    fRowHeight:= fRowHeight + 2;
    fRowNum:= Trunc((fInfoItemsRect.Bottom - fInfoItemsRect.Top) / fRowHeight);

    // Calc column width and number of cols
    fColNum:= Ceil(InfoList.Count / fRowNum);
    fColWidth:= Trunc((fInfoItemsRect.Right - fInfoItemsRect.Left) / fColNum);
  end
  else
  begin
    fRowNum:= 0;
    fColNum:= 0;
    fColWidth:= 0;
  end;
  ThumbImage.ThumbnailSize.cx:= fThumbImageRect.Right - fThumbImageRect.Left;
  ThumbImage.ThumbnailSize.cy:= fThumbImageRect.Bottom - fThumbImageRect.Top;
end;

{-------------------------------------------------------------------------------
  Clear All
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Clear;
begin
  ThumbImage.Clear;
  InfoList.Clear;
  FreeAndNil(fCurrentNamespace);
end;

{-------------------------------------------------------------------------------
  Load Info from Namespace
-------------------------------------------------------------------------------}
function TCEInfoBar.LoadInfoFrom(APIDL: PItemIDList): Boolean;
var
  EnumList: IEnumIDList;
  NewItem: PItemIDList;
  i: Integer;
  Dummy: Cardinal;
  Flags: Cardinal;
begin
  Clear;

  Result:= assigned(APIDL);
  if Result then
  begin
    fCurrentNamespace:= TNamespace.Create(PIDLMgr.CopyPIDL(APIDL), nil);

    // Thumbnail
    fDefaultImage.SetSize(0,0);
    LargeSysImages.GetBitmap(fCurrentNamespace.GetIconIndex(false, icLarge), fDefaultImage);
    //if fCurrentNamespace.OffLine then
    ThumbImage.OpenPIDL(APIDL);
    // Name
    AddInfoItem(fCurrentNamespace.NameNormal);

    // Size
    if fCurrentNamespace.FileSystem and not fCurrentNamespace.Folder then
    begin
      AddInfoItem(_('Size') + ': ' + fCurrentNamespace.SizeOfFileKB);
      AddInfoItem(_('Type') + ': ' + fCurrentNamespace.FileType);
      AddInfoItem(_('Modified') + ': ' + fCurrentNamespace.LastWriteTime);
      AddInfoItem(_('Created') + ': ' + fCurrentNamespace.CreationTime);
    end
    else if fCurrentNamespace.Folder then
    begin
      i:= 0;
      if CalculateHiddenItems then
      Flags:= SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN
      else
      Flags:= SHCONTF_FOLDERS or SHCONTF_NONFOLDERS;
      

      if fCurrentNamespace.ShellFolder.EnumObjects(0, Flags, EnumList) = S_OK then
      begin
        while EnumList.Next(1, NewItem, Dummy) = S_OK do
        begin
          i:= i + 1;
          PIDLMgr.FreePIDL(NewItem);
        end;
      end;
      AddInfoItem(IntToStr(i) + ' ' + _('Item(s)'));
    end;
    
    CalcPositions;
    Paint;
  end;
end;

procedure TCEInfoBar.OnThumbImageChanged(Sender: TObject);
begin
  Paint;
end;

{-------------------------------------------------------------------------------
  Handle Paint
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Paint;
var
  i: Integer;
  curRow, curCol: Integer;
  item: TCEInfoItem;
begin
  if csDestroying in  Self.ComponentState then
  Exit;
  
  //inherited;
  Canvas.Lock;
  try
    // Paint background
    Canvas.Brush.Color:= clWhite;
    Canvas.FillRect(Rect(0,0, Self.Width, Self.Height));

    // No need to continue if there's no item loaded
    if not assigned(fCurrentNamespace) then
    Exit;

    // Paint Thumbnail
    if ThumbImage.Status = tisThumbnail then
    Canvas.StretchDraw(fThumbImageRect, ThumbImage.Thumbnail)
    else
    Canvas.StretchDraw(fThumbImageRect, fDefaultImage);
//    LargeSysImages.Draw(Canvas, fThumbImageRect.Left, fThumbImageRect.Top,
//                        CurrentNamespace.GetIconIndex(false, icLarge));
    // Paint InfoList
    if InfoList.Count > 0 then
    begin
      // Paint Items
      curRow:= 0;
      curCol:= 0;
      Canvas.Brush.Style:= bsClear;
      for i:= 0 to InfoList.Count - 1 do
      begin
        item:= TCEInfoItem(InfoList.Items[i]);
        item.ItemRect.Left:= fInfoItemsRect.Left + (curCol * fColWidth);
        item.ItemRect.Top:= fInfoItemsRect.Top + (curRow * fRowHeight);
        item.ItemRect.Right:= item.ItemRect.Left + fColWidth - 2;
        item.ItemRect.Bottom:= item.ItemRect.Top + fRowHeight;

        SpDrawXPText(Canvas, item.Text, item.ItemRect, DT_END_ELLIPSIS);

        if (curRow + 1) > (fRowNum-1) then
        begin
          curRow:= 0;
          curCol:= curCol + 1;
        end
        else
        begin
          curRow:= curRow + 1;
        end;
      end;
    end;
    
  finally
    Canvas.Unlock;
  end;
end;

{-------------------------------------------------------------------------------
  Handle Resize
-------------------------------------------------------------------------------}
procedure TCEInfoBar.Resize;
var
  ReloadThumbnail: Boolean;
  oldSize: TSize;
begin
  inherited;
  if csDestroying in  Self.ComponentState then
  Exit;
    
  oldSize:= ThumbImage.ThumbnailSize;
  CalcPositions;

  ReloadThumbnail:= (oldSize.cx <> ThumbImage.ThumbnailSize.cx) or
                    (oldSize.cy <> ThumbImage.ThumbnailSize.cy);

  if assigned(fCurrentNamespace) and ReloadThumbnail then
  begin
    if ThumbImage.Status = tisThumbnail then
    ThumbImage.OpenPIDL(fCurrentNamespace.AbsolutePIDL, true);
  end;
end;

{-------------------------------------------------------------------------------
  Set CalculateHiddenItems
-------------------------------------------------------------------------------}
procedure TCEInfoBar.SetCalculateHiddenItems(const Value: Boolean);
var
  pidl: PItemIDList;
begin
  if fCalculateHiddenItems <> Value then
  begin
    fCalculateHiddenItems:= Value;
    if assigned(fCurrentNamespace) then
    begin
      if fCurrentNamespace.FileSystem and fCurrentNamespace.Folder then
      begin
        pidl:= PIDLMgr.CopyPIDL(fCurrentNamespace.AbsolutePIDL);
        try
          LoadInfoFrom(pidl);
        finally
          PIDLMgr.FreePIDL(pidl);
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle WM_EraseBkgnd message
-------------------------------------------------------------------------------}
procedure TCEInfoBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result:= 0;
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

{-------------------------------------------------------------------------------
  Set Text
-------------------------------------------------------------------------------}
procedure TCEInfoItem.SetText(const Value: WideString);
var
  size: TSize;
begin
  // TODO: Is this really necessary?
  if fText <> Value then
  begin
    fText:= Value;
    size:= WideCanvasTextExtent(TCustomControlHack(Owner).Canvas, fText);
    fTextWidth:= size.cx;
    fTextHeight:= size.cy;
  end;
end;

{##############################################################################}
// TCEThumbImage

{-------------------------------------------------------------------------------
  Create an instance of TCEThumbImage
-------------------------------------------------------------------------------}
constructor TCEThumbImage.Create;
begin
  inherited;
  ThumbnailSize.cx:= 32;
  ThumbnailSize.cy:= 32;
  fThumbnail:= TBitmap.Create;
  fStatus:= tisDefault;
end;

{-------------------------------------------------------------------------------
  Destroy TCEThumbImage
-------------------------------------------------------------------------------}
destructor TCEThumbImage.Destroy;
begin
  fThumbnail.Free;
  inherited;
end;

{-------------------------------------------------------------------------------
  Clear
-------------------------------------------------------------------------------}
procedure TCEThumbImage.Clear;
begin
  if assigned(fLatestThread) then
  begin
    fLatestThread.Terminate;
  end;
  fStatus:= tisDefault;
  DoChanged;
end;

{-------------------------------------------------------------------------------
  Open File
-------------------------------------------------------------------------------}
procedure TCEThumbImage.OpenFile(AFilePath: WideString; NoStatusChange: Boolean
    = false);
var
  ns: TNamespace;
begin
  ns:= TNamespace.CreateFromFileName(AFilePath);
  try
    OpenPIDL(ns.AbsolutePIDL, NoStatusChange);
  finally
    ns.Free;
  end;
end;

{-------------------------------------------------------------------------------
  OpenPIDL
-------------------------------------------------------------------------------}
procedure TCEThumbImage.OpenPIDL(APIDL: PItemIDList; NoStatusChange: Boolean =
    false);
begin
  // Terminate previous thread if present
  if assigned(fLatestThread) then
  begin
    fLatestThread.Terminate;
  end;

  // Create thread
  fLatestThread:= TCEThumbLoadThread.Create(true);
  fLatestThread.FreeOnTerminate:= true;
  fLatestThread.ThumbImage:= Self;
  fLatestThread.PIDL:= PIDLMgr.CopyPIDL(APIDL);
  fLatestThread.ThumbnailSize.cx:= ThumbnailSize.cx;
  fLatestThread.ThumbnailSize.cy:= ThumbnailSize.cy;

  // Change status
  if not NoStatusChange then
  fStatus:= tisLoading;
  
  DoChanged;

  // Run thread
  fLatestThread.Resume;
end;

{-------------------------------------------------------------------------------
  Get's called when thread has finished
-------------------------------------------------------------------------------}
procedure TCEThumbImage.ThreadFinished(AThread: TCEThumbLoadThread);
begin
  if AThread = fLatestThread then
  begin
    if AThread.Terminated or not AThread.ThumbnailFound then
    fStatus:= tisDefault
    else
    begin
      fThumbnail.Assign(AThread.Thumbnail);
      fStatus:= tisThumbnail;
    end;
    DoChanged;

    fLatestThread:= nil;
  end;
end;

{-------------------------------------------------------------------------------
  Do Changed event
-------------------------------------------------------------------------------}
procedure TCEThumbImage.DoChanged;
begin
  if Assigned(fOnChanged) then fOnChanged(Self);
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
  CoInitialize(nil);
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
  ThumbImage.ThreadFinished(Self);
end;

end.
