unit CollectTiffInfo;

interface

uses
    Classes;

type
    TCollectInfo = class
    public
        function infoAsStrings: TStrings;
    end;

var
    collectedTiffInfo: TCollectInfo;

implementation

uses
    ReadTiff, SysUtils,
    XMLIntf,
    XMLDoc;

function TCollectInfo.infoAsStrings: TStrings;
var
    s: string;
    items: TStringList;
    tip: TImageProperties;
    oxml: TXMLDocument;
begin
    items := TStringList.Create;
    if ttInfo.fLittleEndian then
        items.Add('Tiff file stored in little endian format')
    else
        items.Add('Tiff file stored in high endian format');
    if ttInfo.fMultiImage then
        items.Add('Tiff file has multiple images')
    else
        items.Add('Tiff file contains single image');
    ttInfo.GetImageProperties(tip);
    with tip do
    begin
        if tip.fIsBigTiff then
            items.Add('Big TIFF')
        else
            items.Add('Normal TIFF');
        if tip.fPalette then
            s := 'colors'
        else
            s := 'values';
        if iPhoto = 2 then
            iNrColors := 1 shl 24;
        if iNrColors = 0 then
            if iComp = 2 then
                iNrColors := 2;
        items.Add(Format('Image size: %d x %d, %.0n %s', [iWidth, iHeight,
          1.0 * iNrColors, s]));
        case iPhoto of
            0:
                items.Add('WhiteIsZero: Colors range from white to black');
            1:
                items.Add('BlackIsZero: Colors range from black to white');
            2:
                items.Add('RGB: Full color image (' + IntToStr(iNrBits) +
                  ' bits per pixel)');
            3:
                items.Add('Colors are determined from a palette (LUT)');
            4:
                items.Add('Image is a bit mask');
        end;
        if (sResUnit = 'Inch') or (sResUnit = 'Centimeter') then
        begin
            if iXRes <> 0 then
                items.Add(Format('X Resolution: %d / %s', [iXRes, sResUnit]));
            if iYRes <> 0 then
                items.Add(Format('Y Resolution: %d / %s', [iYRes, sResUnit]));
        end
        else if sResUnit <> '' then
            items.Add(sResUnit);
        case iComp of
            1:
                items.Add('No compression');
            2:
                items.Add(
                  'CCITT Group 3 1-dimensional modified HuffMan runlength encoding');
            3:
                items.Add('CCITT T.4 bi-level encoding');
            4:
                items.Add('CCITT T.6 bi-level encoding');
            5:
                begin
                    s := 'LZW compression';
                    if iPredict = 1 then
                        s := s + ' (No prediction scheme)'
                    else if iPredict = 2 then
                        s := s + ' (Horizontal differencing)';
                    items.Add(s)
                end;
            6:
                items.Add('JPEG compression');
            32773:
                items.Add('Packbits compression');
        else
            items.Add('No TIFF 6.0 compression scheme');
        end;
        if sImageDesc <> '' then
            items.Add('Image Description: ''' + sImageDesc + '''');
        if nodata_val <> '' then
            items.Add('GDAL nodata value: ' + nodata_val);
        if metadata_xml <> '' then
        begin
            oxml := TXMLDocument.Create(nil);
            oxml.LoadFromXML(metadata_xml);
            oxml.XML.Text := XMLDoc.FormatXMLData(oxml.XML.Text);
            oxml.Active := true;

            // Items.Add('GDAL metadata: ');
            items.AddStrings(oxml.XML);
        end;
    end;

    Result := items;
end;

end.
