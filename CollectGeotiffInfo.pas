unit CollectGeotiffInfo;

interface

uses
    Classes;

type
    TCollectGeotiffInfo = class
    public
        function infoAsStrings: TStrings;

    end;

var
    collectedGeotiffInfo : TCollectGeotiffInfo;

implementation

uses
    SysUtils,
    ReadTiff;

{ TCollectGeotiffInfo }

function TCollectGeotiffInfo.infoAsStrings: TStrings;
var
    items: TStringList;
    ghRevis : TGeoHeader;
    tgp : TGeoTiffProperties;
    itemValue : string;
    s :string;
    var i : integer;
begin
    items := TStringList.Create;
    ttinfo.GetGeoTiffVersion(ghRevis);
    items.Add(Format('GeoTiff GeoKey directory version %d', [ghRevis.iVersion]));
    items.Add(Format('GeoTiff keys revision %d.%d', [ghRevis.iRevision, ghRevis.iMinor]));
    ttInfo.GetGeoTiffProperties(tgp);
    if abs(tgp.t3Scale.X) > 0.001 then
    begin
        with tgp.t3Scale do
            s := Format('(%.8f, %.8f, %.8f)', [X, Y, Z]);
        items.Add('Model Pixel Scaling factor (X, Y, Z): ' + s);
    end;
    if tgp.ptieTiePoints <> nil then
    begin
        items.Add('Tiepoint table: ');
        for i := 0 to tgp.inrTiePts - 1 do
            with tgp.ptieTiePoints^[i] do
            begin
                items.Add(Format('%10s(%.2f, %.2f, %.2f) -> (%.8f, %.8f, %.8f)',
                  ['', t3Pixel.X, t3Pixel.Y, t3Pixel.Z, t3World.X, t3World.Y,
                  t3World.Z]));
            end;
    end;
    if tgp.fMatrix then
    begin
        items.Add('Transformation matrix (4 x 4):');
        for i := 0 to 3 do
        begin
            items.Add(Format('%10s %10.2f  %10.2f  %10.2f  %10.2f',
              ['', tgp.matTransform[i * 4], tgp.matTransform[i * 4 + 1],
              tgp.matTransform[i * 4 + 2], tgp.matTransform[i * 4 + 3]]));
        end;
    end;

    result := items;
end;

end.
