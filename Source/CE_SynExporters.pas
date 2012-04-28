unit CE_SynExporters;

interface

uses
  // SynEdit
  SynEditExport, SynEditHighlighter, SynUnicode, SynEditMiscProcs,
  // System Units
  Windows, Graphics, Classes, SysUtils;

type
{-------------------------------------------------------------------------------
  TCESynExporterHTML
-------------------------------------------------------------------------------}
  TCESynExporterHTML = class(TSynCustomExporter)
  private
  protected
    fDefaultBackgroundColor: TColor;
    fDefaultFontColor: TColor;
    fHTMLTitle: WideString;
    fIncludeHTMLWrapper: Boolean;
    fSpanIsOpen: Boolean;
    fUseInlineCSS: Boolean;
    fWrapperDivClass: String;
    function AttriToCSS(Attri: TSynHighlighterAttributes; UniqueAttriName: string):
        string; virtual;
    function AttriToCSSCallback(Highlighter: TSynCustomHighlighter; Attri:
        TSynHighlighterAttributes; UniqueAttriName: string; Params: array of
        Pointer): Boolean; virtual;
    function ColorToHTML(AColor: TColor): string; virtual;
    procedure FormatAfterLastAttribute; override;
    procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: boolean;
        FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: boolean;
        FontStylesChanged: TFontStyles); override;
    procedure FormatBeforeFirstAttribute(BackgroundChanged, ForegroundChanged:
        boolean; FontStylesChanged: TFontStyles); override;
    procedure FormatNewLine; override;
    function GetFooter: UnicodeString; override;
    function GetFormatName: string; override;
    function GetHeader: UnicodeString; override;
    function GetStyleName(Highlighter: TSynCustomHighlighter; Attri:
        TSynHighlighterAttributes): string; virtual;
    function MakeValidName(Name: string): string; virtual;
    function ReplaceReservedChar(AChar: WideChar): UnicodeString; override;
    function StyleNameCallback(Highlighter: TSynCustomHighlighter; Attri:
        TSynHighlighterAttributes; UniqueAttriName: string; Params: array of
        Pointer): Boolean; virtual;
    function UseBom: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    function SupportedEncodings: TSynEncodings; override;
  published
    property HTMLTitle: WideString read fHTMLTitle write fHTMLTitle;
    property IncludeHTMLWrapper: Boolean read fIncludeHTMLWrapper write
        fIncludeHTMLWrapper;
    property UseInlineCSS: Boolean read fUseInlineCSS write fUseInlineCSS;
    property WrapperDivClass: String read fWrapperDivClass write fWrapperDivClass;
  end;
  

implementation

{-------------------------------------------------------------------------------
  Create an instance of TCESynExporterHTML
-------------------------------------------------------------------------------}
constructor TCESynExporterHTML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fIncludeHTMLWrapper:= false;
  fUseInlineCSS:= true;
  fHTMLTitle:= 'Untitled';
end;

{-------------------------------------------------------------------------------
  AttriToCSS
-------------------------------------------------------------------------------}
function TCESynExporterHTML.AttriToCSS(Attri: TSynHighlighterAttributes;
    UniqueAttriName: string): string;
var
  StyleName: string;
begin
  // name
  Result:= '.' + MakeValidName(UniqueAttriName) + ' {';
  // background color
  if (Attri.Background <> clNone) then
  Result:= Result + 'background-color: ' + ColorToHTML(Attri.Background) + '; ';
  // font color
  if Attri.Foreground <> clNone then
  Result:= Result + 'color: ' + ColorToHTML(Attri.Foreground) + '; ';
  // font styles
  if fsBold in Attri.Style then
  Result:= Result + 'font-weight: bold; ';
  if fsItalic in Attri.Style then
  Result:= Result + 'font-style: italic; ';
  if fsUnderline in Attri.Style then
   Result:= Result + 'text-decoration: underline; ';
  if fsStrikeOut in Attri.Style then
  Result:= Result + 'text-decoration: line-through; ';

  Result:= Trim(Result) + '}';
end;

{-------------------------------------------------------------------------------
  AttriToCSSCallback
-------------------------------------------------------------------------------}
function TCESynExporterHTML.AttriToCSSCallback(Highlighter:
    TSynCustomHighlighter; Attri: TSynHighlighterAttributes; UniqueAttriName:
    string; Params: array of Pointer): Boolean;
var
  Styles: ^string;
begin
  Styles:= Params[0];
  Styles^:= Styles^ + AttriToCSS(Attri, UniqueAttriName) + #13#10;
  Result:= True;
end;

{-------------------------------------------------------------------------------
  ColorToHTML
-------------------------------------------------------------------------------}
function TCESynExporterHTML.ColorToHTML(AColor: TColor): string;
var
  RGBColor: longint;
  RGBValue: byte;
const
  Digits: array[0..15] of Char = '0123456789ABCDEF';
begin
  RGBColor:= ColorToRGB(AColor);
  Result:= '#000000';
  // red
  RGBValue:= GetRValue(RGBColor);
  if RGBValue > 0 then
  begin
    Result[2]:= Digits[RGBValue shr  4];
    Result[3]:= Digits[RGBValue and 15];
  end;
  // green
  RGBValue:= GetGValue(RGBColor);
  if RGBValue > 0 then
  begin
    Result[4]:= Digits[RGBValue shr  4];
    Result[5]:= Digits[RGBValue and 15];
  end;
  // blue
  RGBValue:= GetBValue(RGBColor);
  if RGBValue > 0 then
  begin
    Result[6]:= Digits[RGBValue shr  4];
    Result[7]:= Digits[RGBValue and 15];
  end;
end;

{-------------------------------------------------------------------------------
  FormatAfterLastAttribute
-------------------------------------------------------------------------------}
procedure TCESynExporterHTML.FormatAfterLastAttribute;
begin
  // do nothing
end;

{-------------------------------------------------------------------------------
  FormatAttributeDone
-------------------------------------------------------------------------------}
procedure TCESynExporterHTML.FormatAttributeDone(BackgroundChanged,
    ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
begin
  if BackgroundChanged or ForegroundChanged or (FontStylesChanged <> []) then
  begin
    if fSpanIsOpen then
    AddData('</span>');
  end;
end;

{-------------------------------------------------------------------------------
  FormatAttributeInit
-------------------------------------------------------------------------------}
procedure TCESynExporterHTML.FormatAttributeInit(BackgroundChanged,
    ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
var
  style: String;
begin
  if fUseInlineCSS then
  begin
    if BackgroundChanged or ForegroundChanged or (FontStylesChanged <> []) then
    begin
      style:= '';
      // background
      if (Self.fLastBG <> clNone) and (Self.fLastBG <> fDefaultBackgroundColor)  then
      style:= style + 'background-color: ' + ColorToHTML(Self.fLastBG) + '; ';
      // font color
      if (Self.fLastFG <> clNone) and (Self.fLastFG <> fDefaultFontColor) then
      style:= style + 'color: ' + ColorToHTML(Self.fLastFG) + '; ';
      // font styles
      if fsBold in Self.fLastStyle then
      style:= style + 'font-weight: bold; ';
      if fsItalic in Self.fLastStyle then
      style:= style + 'font-style: italic; ';
      if fsUnderline in Self.fLastStyle then
      style:= style + 'text-decoration: underline; ';
      if fsStrikeOut in Self.fLastStyle then
      style:= style + 'text-decoration: line-through; ';

      style:= Trim(style);

      fSpanIsOpen:= style <> '';
      if fSpanIsOpen then
      AddData('<span style="' + style + '">');
    end;
  end
  else
  begin
    fSpanIsOpen:= true;
    style:= GetStyleName(Highlighter, Highlighter.GetTokenAttribute);
    AddData(Format('<span class="%s">', [style]));
  end;
end;

{-------------------------------------------------------------------------------
  FormatBeforeFirstAttribute
-------------------------------------------------------------------------------}
procedure TCESynExporterHTML.FormatBeforeFirstAttribute(BackgroundChanged,
    ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
var
  s: String;
begin
  // init colors
  fDefaultFontColor:= ColorToRGB(fFont.Color);
  fDefaultBackgroundColor:= ColorToRGB(Color);

  s:= '';

  if fUseInlineCSS then
  begin
    // base font color
    s:= s + '<span style="color: ' + ColorToHTML(fDefaultFontColor) + '">';
  end;

  AddData(s);
  
  FormatAttributeInit(BackgroundChanged, ForegroundChanged, FontStylesChanged);
end;

{-------------------------------------------------------------------------------
  Format NewLine
-------------------------------------------------------------------------------}
procedure TCESynExporterHTML.FormatNewLine;
begin
  AddNewLine;
end;

{-------------------------------------------------------------------------------
  Get Footer                               
-------------------------------------------------------------------------------}
function TCESynExporterHTML.GetFooter: UnicodeString;
begin
  if fUseInlineCSS or fSpanIsOpen then
  Result:= '</span>';

  if (WrapperDivClass <> '') or UseBackground then
  Result:= Result + '</code></pre></div>'#13#10
  else
  Result:= Result + '</code></pre>'#13#10;

  if fIncludeHTMLWrapper then
  Result:= Result + '</body>'#13#10'</html>';
end;

{-------------------------------------------------------------------------------
  Get FormatName
-------------------------------------------------------------------------------}
function TCESynExporterHTML.GetFormatName: string;
begin
  Result:= 'HTML';
end;

{-------------------------------------------------------------------------------
  Get Header
-------------------------------------------------------------------------------}
function TCESynExporterHTML.GetHeader: UnicodeString;
const
  HTMLAsTextHeader =  '<?xml version="1.0" encoding="%s"?>'#13#10 +
                      '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'#13#10 +
                      '<html xmlns="http://www.w3.org/1999/xhtml">'#13#10 +
                      '<head>'#13#10;

  HTMLAsTextHeader2 = '<meta http-equiv="Content-Type" content="text/html; charset=%s" />'#13#10 +
                      '<meta name="generator" content="CubicExplorer HTML exporter" />'#13#10;

  HTMLAsTextHeader3 = '<style type="text/css">'#13#10 +
                      '<!--'#13#10 +
                      'body {color: %s; background-color: %s;}'#13#10 +
                      '%s' +
                      '-->'#13#10 +
                      '</style>'#13#10;

  HTMLAsTextHeader4 = '</head>'#13#10 +
                      '<body>'#13#10;
                     
  EncodingStrs: array[TSynEncoding] of string =
    ('UTF-8', 'UTF-16', 'UTF-16', 'ANSI is Unsupported');

var
  EncodingStr, Styles, wrapperStyle: string;
begin
  Result:= '';
  // HTML wrapper
  if fIncludeHTMLWrapper then
  begin
    EncodingStr:= EncodingStrs[Encoding];
    // html
    Result:= Format(HTMLAsTextHeader, [EncodingStr]);
    // title
    if fHTMLTitle <> '' then
    Result:= Result + '<title>' + fHTMLTitle + '</title>'#13#10;
    // meta
    Result:= Result + Format(HTMLAsTextHeader2, [EncodingStr]);
    // style
    if not fUseInlineCSS then
    begin
      EnumHighlighterAttris(Highlighter, True, AttriToCSSCallback, [@Styles]);
      // add wrapper div style
      if WrapperDivClass <> '' then
      begin
        wrapperStyle:= '.' + WrapperDivClass + ' {';
        if UseBackground then
        wrapperStyle:= wrapperStyle + 'background-color: ' + ColorToHTML(fDefaultBackgroundColor) + ';';
        wrapperStyle:= wrapperStyle + '}';
        Styles:= wrapperStyle  + #13#10 + Styles;
      end;

      Result:= Result + Format(HTMLAsTextHeader3, [ColorToHtml(fDefaultFontColor),
        ColorToHTML(fDefaultBackgroundColor), Styles]);
    end;
    // body
    Result:= Result + HTMLAsTextHeader4;
  end;

  // wrapper div
  if (WrapperDivClass <> '') or UseBackground then
  begin
    Result:= Result + '<div';
    // add class name if available
    if WrapperDivClass <> '' then
    Result:= Result + ' class="' + WrapperDivClass + '"';
    // add background style. inline css will be always used if wrapper class is empty.
    if UseBackground and (fUseInlineCSS or (WrapperDivClass = '')) then
    Result:= Result + ' style="background-color: ' + ColorToHTML(fDefaultBackgroundColor) + '"';
    Result:= Result + '>';
  end;

  // code wrapper
  Result:= Result + '<pre><code>';

  // code wrapper
//  Result:= Result + '<pre><code>';
end;

{-------------------------------------------------------------------------------
  GetStyleName
-------------------------------------------------------------------------------}
function TCESynExporterHTML.GetStyleName(Highlighter: TSynCustomHighlighter;
    Attri: TSynHighlighterAttributes): string;
begin
  EnumHighlighterAttris(Highlighter, False, StyleNameCallback, [Attri, @Result]);
end;

{-------------------------------------------------------------------------------
  MakeValidName
-------------------------------------------------------------------------------}
function TCESynExporterHTML.MakeValidName(Name: string): string;
var
  i: Integer;
begin
  Result:= LowerCase(Name);
  for i:= Length(Result) downto 1 do
  begin
    if CharInSet(Result[i], ['.', '_']) then
    Result[i] := '-'
    else if not CharInSet(Result[i], ['a'..'z', '0'..'9', '-']) then
    Delete(Result, i, 1);
  end;
end;

{-------------------------------------------------------------------------------
  Replace Reserved Char
-------------------------------------------------------------------------------}
function TCESynExporterHTML.ReplaceReservedChar(AChar: WideChar): UnicodeString;
begin
  case AChar of
    '&': Result := '&amp;';
    '<': Result := '&lt;';
    '>': Result := '&gt;';
    '"': Result := '&quot;';
    else Result := '';
  end
end;

{-------------------------------------------------------------------------------
  StyleNameCallback
-------------------------------------------------------------------------------}
function TCESynExporterHTML.StyleNameCallback(Highlighter:
    TSynCustomHighlighter; Attri: TSynHighlighterAttributes; UniqueAttriName:
    string; Params: array of Pointer): Boolean;
var
  AttriToFind: TSynHighlighterAttributes;
  StyleName: ^string;
begin
  AttriToFind:= Params[0];
  StyleName:= Params[1];

  if Attri = AttriToFind then
  begin
    StyleName^:= MakeValidName(UniqueAttriName);
    Result:= False;
  end
  else
  Result:= True;
end;

{-------------------------------------------------------------------------------
  Supported Encodings
-------------------------------------------------------------------------------}
function TCESynExporterHTML.SupportedEncodings: TSynEncodings;
begin
  Result:= [seUTF8, seUTF16LE, seUTF16BE];
end;

{-------------------------------------------------------------------------------
  Use Bom
-------------------------------------------------------------------------------}
function TCESynExporterHTML.UseBom: Boolean;
begin
  // do not include seUTF8 as some browsers have problems with UTF-8-BOM
  Result:= Encoding in [seUTF16LE, seUTF16BE];
end;

end.
