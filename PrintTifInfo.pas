unit PrintTifInfo;

interface

uses
    Classes, Windows, Printers;

type
    TInfoPrinter = class
    public
        procedure printTiffInfo(ScreenDPI: integer; fontsize: integer;
          tagheaders: TStrings; tagWidths: array of integer;
          geotagHeaders: TStrings; geotagWidths: array of integer);

    private
        procedure renderItem(item: string; width: integer);
        procedure renderHeader(headers: TStrings; widths: array of integer;
          horPos: integer; vertPos: integer);
        procedure renderTags(widths: array of integer); // all normal tiff tags

        procedure renderGeotiffTags(widths: array of integer);
    end;

var
    infoPrinter: TInfoPrinter;

implementation

uses
    SysUtils, Graphics,
    ReadTiff,
    CollectTiffInfo,
    CollectGeotiffInfo;

var
    // rectClip: TRect;
    // iXOffset: integer;
    iXDPI: integer;
    iScreenDPI: integer;
    // iLeftMargin: integer;
    iLeftIFD: integer; // left most position on paper
    iCurVert: integer;
    iLineHeight: integer;
    iCurHorz: integer;
    // i: integer;
    // j: integer;

procedure TInfoPrinter.renderItem(item: string; width: integer);
begin
    with Printer.Canvas do
        TextOut(iCurHorz, iCurVert, item);
    if width <> -1 then
        iCurHorz := iCurHorz + MulDiv(width, iXDPI, iScreenDPI);
end;

procedure TInfoPrinter.renderHeader(headers: TStrings; widths: array of integer;
  horPos: integer; vertPos: integer);
var
    i: integer;
begin
    for i := 0 to headers.Count - 1 do
    begin
        Printer.Canvas.TextOut(horPos, vertPos, headers[i]);
        horPos := horPos + MulDiv(widths[i], iXDPI, iScreenDPI);
    end;

end;

procedure TInfoPrinter.renderTags(widths: array of integer);
var
    i, iTag, iType: integer;
    tagName: string;
begin
    for i := 0 to ttInfo.iNumberIFD - 1 do
    begin
        iCurHorz := iLeftIFD;
        tagName := ttInfo.sTag(i, iTag);
        renderItem(IntToStr(iTag), widths[0]);
        renderItem(tagName, widths[1]);
        renderItem(ttInfo.sType(i, iType), widths[2]);
        renderItem(Format('%d', [ttInfo.iCount(i)]), widths[3]);
        renderItem(Format('%d', [ttInfo.iValue(i)]), widths[4]);
        renderItem(Format('$%.8x', [ttInfo.iValue(i)]), widths[5]);
        iCurVert := iCurVert + iLineHeight;
    end;

end;

procedure TInfoPrinter.renderGeotiffTags(widths: array of integer);
var
    i, iTag, iType: integer;
    tagName: string;
begin
    for i := 0 to ttInfo.iNrGeoKeys - 1 do
    begin
        iCurHorz := iLeftIFD;
        tagName := ttInfo.sGeoTag(i, iTag);
        renderItem(IntToStr(iTag), widths[0]);
        renderItem(tagName, widths[1]);
        renderItem(ttInfo.sGeoLoc(i, iType), widths[2]);
        renderItem(Format('%d', [ttInfo.iGeoCount(i)]), widths[3]);
        renderItem(Format('%d', [ttInfo.iGeoValue(i)]), widths[4]);
        renderItem(ttInfo.sDescript(iTag, ttInfo.iGeoValue(i)), widths[5]);
        iCurVert := iCurVert + iLineHeight;
    end;
end;

// Main print function
procedure TInfoPrinter.printTiffInfo(ScreenDPI: integer; fontsize: integer;
  tagheaders: TStrings; tagWidths: array of integer; geotagHeaders: TStrings;
  geotagWidths: array of integer);

var
    rectClip: TRect;
    iXOffset: integer;
    iLeftMargin: integer;
    i: integer;
    j: integer;
    imageDetails: TStrings;
begin
    iScreenDPI := ScreenDPI;
    Printer.BeginDoc;
    with Printer.Canvas do
    begin
        rectClip := ClipRect;
        Font.Style := [];
        Font.Size := 10;
        Font.Name := 'Times New Roman';
        iXOffset := GetDeviceCaps(Handle, PHYSICALOFFSETX);
        iXDPI := Font.PixelsPerInch;
        iLeftMargin := MulDiv(iXDPI, 100, 254) - iXOffset;
        // left margin = 1 cm
        iLeftIFD := MulDiv(iXDPI, 200, 254) - iXOffset;
        iCurVert := 0;

        { print Header }
        TextOut(rectClip.Left, 0, FormatDateTime('ddd, d mmm yyyy, t', Now));
        TextOut(rectClip.Right - TextWidth(ttInfo.sTiffName) - 10, 0,
          ttInfo.sTiffName);

        { print IFD header line }
        Font.Size := 15;
        Font.Style := [fsBold];
        Font.Name := 'Times New Roman';
        iLineHeight := MulDiv(TextHeight('Hj'), 5, 4);
        iCurHorz := iLeftMargin;
        // line spacing = 20%
        iCurVert := iCurVert + iLineHeight;
        renderItem('IFD summary', -1);
        iCurVert := iCurVert + iLineHeight;
        Font.Size := 12;
        Font.Style := [];
        Font.Name := 'Arial';
        iLineHeight := MulDiv(TextHeight('Hj'), 5, 4);
        // line spacing = 20%

        { print IFD overview }
        iCurHorz := iLeftIFD;
        renderHeader(tagheaders, tagWidths, iCurHorz, iCurVert);

        iCurVert := iCurVert + iLineHeight;
        Font.Size := fontsize;
        // line spacing = 33%
        iLineHeight := MulDiv(TextHeight('Hj'), 4, 3);
        renderTags(tagWidths);

        { Print Image Info }
        Font.Size := 15;
        Font.Style := [fsBold];
        Font.Name := 'Times New Roman';
        // line spacing = 20%
        iLineHeight := MulDiv(TextHeight('Hj'), 5, 4);
        iCurVert := iCurVert + iLineHeight;

        TextOut(iLeftMargin, iCurVert, 'Image info');
        iCurVert := iCurVert + iLineHeight;

        Font.Size := fontsize;
        Font.Style := [];
        Font.Name := 'Arial';
        // line spacing = 33%
        iLineHeight := MulDiv(TextHeight('Hj'), 4, 3);

        imageDetails := collectedTiffInfo.infoAsStrings;
        iCurHorz := iLeftIFD;
        for i := 0 to imageDetails.Count - 1 do
        begin
            renderItem(imageDetails[i], -1);
            iCurVert := iCurVert + iLineHeight;
        end;

        if ttInfo.fHasGeoTiff then
        begin
            Font.Size := 15;
            Font.Style := [fsBold];
            Font.Name := 'Times New Roman';
            iLineHeight := MulDiv(TextHeight('Hj'), 5, 4);
            // line spacing = 20%
            iCurVert := iCurVert + iLineHeight;
            TextOut(iLeftMargin, iCurVert, 'GeoTiff Tag summary');
            iCurVert := iCurVert + iLineHeight;
            Font.Size := 12;
            Font.Style := [];
            Font.Name := 'Arial';
            iLineHeight := MulDiv(TextHeight('Hj'), 5, 4);
            // line spacing = 20%

            { Print GeoTiff Tag Info }
            iCurHorz := iLeftIFD;
            renderHeader(geotagHeaders, geotagWidths, iCurHorz, iCurVert);
            iCurVert := iCurVert + iLineHeight;
            Font.Size := fontsize;
            iLineHeight := MulDiv(TextHeight('Hj'), 4, 3);
            // line spacing = 33%

            renderGeotiffTags(geotagWidths);

            Font.Size := 15;
            Font.Style := [fsBold];
            Font.Name := 'Times New Roman';
            iLineHeight := MulDiv(TextHeight('Hj'), 5, 4);
            // line spacing = 20%
            iCurVert := iCurVert + iLineHeight;
            TextOut(iLeftMargin, iCurVert, 'GeoTiff details');
            iCurVert := iCurVert + iLineHeight;

            { Print GeoTiff summary info }
            Font.Size := fontsize;
            Font.Style := [];
            Font.Name := 'Arial';
            iLineHeight := MulDiv(TextHeight('Hj'), 4, 3);
            // line spacing = 33%

            imageDetails := collectedGeotiffInfo.infoAsStrings;
            iCurHorz := iLeftIFD;
            for i := 0 to imageDetails.Count - 1 do
            begin
                renderItem(imageDetails[i], -1);
                iCurVert := iCurVert + iLineHeight;
            end;

        end;
    end;
    Printer.EndDoc;
end;

end.
