unit dCE_Images;

interface

uses
  // PNG Controls
  PngImageList,
  // System Units
  SysUtils, Classes, ImgList, Controls, GR32_Image;

type
  TCE_Images = class(TDataModule)
    MediumIcons: TPngImageList;
    SmallIcons: TPngImageList;
    BookmarkImages: TPngImageList;
    MiscImages: TPngImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CE_Images: TCE_Images;

implementation

{$R *.dfm}

end.
