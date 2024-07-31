unit ReadTiff;

interface

// This implementation is designed for machines on INTEL architecture only!

uses
  Classes;

type
  PIFDEntry = ^TIFDEntry;
  TSmallIFDEntry = packed record
    iTag      : word;
    iType     : word;
    iCount    : longint;
    iValue    : longint;  // either value or pointer, depending on the type and the count
{    constructor Create(iTg, iTp : word; iCnt, iVal : longint);}
  end;

  TIFDEntry = packed record
    iTag      : word;
    iType     : word;
    iCount    : int64;      // actually unsigned, so can only use 63 bits...
    iValue    : int64;      // either value or pointer, depending on the type and the count
  end;

  TIFDEntryV = packed record
    iTag      : word;
    iType     : word;
    case BigTiff : boolean of
      true : (
        iCount    : int64;      // actually unsigned, so can only use 63 bits...
        iValue    : int64;      // either value or pointer, depending on the type and the count
      );
      false : (
        iCountS    : word;
        iValueS    : word;
      )
  end;

  TGeoHeader = record
    iVersion  : word;     // Geo Key implementation version (should be 1)
    iRevision : word;
    iMinor    : word;     // revision: <iRevision>.<iMinor>
    iNrGeoKeys : word;
  end;

  PGeoKey = ^TGeoKey;
  TGeoKey = record
    iKeyID    : word;     // Geo Key tag
    iLoc      : word;     //
    iCount    : word;
    iValue    : word;
  end;

  TImageProperties = class
  public
    iWidth    : integer;
    iHeight   : integer;
    iPhoto    : integer;
    iNrBits   : integer;  // bits per pixel
    iNrColors : longint;
    fPalette  : boolean;
    palColors : array[0..255] of longint;
    iComp     : integer;
    iPredict  : integer;
    iXRes,
    iYRes     : longint;
    sResUnit  : string;
    fGeoTiff  : boolean;  // if set GeoTiff info is found
    fIsBigTiff : boolean;
    sImageDesc : string;
    // GDAL related
    nodata_val : string;
    metadata_xml : string;
    procedure Clear;
    procedure Reset;
  end;

  T3DCoord = record
               X, Y, Z : double;
             end;
  TTiepoint = record
                t3Pixel : T3DCoord;
                t3World : T3DCoord;
              end;
  PDynTiePointArray = ^TDynTiePointArray;
  TDynTiePointArray = array[0..1000] of TTiePoint;

  TMetaTag = record
               iTag : word;
               rVal : double;
             end;

  PDynMetaTagArray = ^TDynMetaTagArray;
  TDynMetaTagArray = array[0..1000] of TMetaTag;

  TMultiTiffLoc = record
    imgID : integer;   // image / page number
    subIFD : integer;  // subIFD per image / page
    IFDoffset : int64; // IFD offset to subTiff
  end;
  TMultiTiffLocArray = array[0..25] of TMultiTiffLoc;

  TMultiTiff = class
  public
    count,
    current : integer;
    all : TMultiTiffLocArray;
    procedure init;
  end;

  TGeoTiffProperties = class
  public
    sGTCitation     : string;
    sGeoCitation    : string;
    sPCSCitation    : string; // 3073
    iGeoModel       : integer;
    rLinUnitSize    : double;
    t3Scale         : T3DCoord;
    iNrTiePts       : integer;
    iNrDblParams    : integer;
    fMatrix         : boolean;
    matTransForm    : array[0..15] of double;
    ptieTiePoints   : PDynTiePointArray;
    ptmDblParam     : PDynMetaTagArray;
    procedure Clear;
  end;

  TDoubleArray = array of double;

  TConvert = class
  public
    isTiffLittle : boolean;
    isMachineLittle : boolean;
    isBig : boolean;

  public
    constructor Create;
    property isLittleEndian : boolean read isTiffLittle write isTiffLittle;
    property isMachineLittleEndian : boolean read isMachineLittle;
    property isBigTiff : boolean read isBig write isBig;

    function wSwap(w : word) : word;
    function iSwap(i : longword) : longword;
    function ibigSwap(big : uint64) : uint64;
    function rSwap(r : double)  : double;
    function truncTo(val : int64; iType : int64; iCount : int64) : int64;

    function iSizeOfType(iType: integer): integer;
  end;

  TTiffInfo = class
  private
    sFile      : string;
    lifd       : TList;       // list of Tiff keys (IFD)
    gifd       : TList;       // list of GeoTiff keys
    iNrGeoEntries,
    iNrEntries : uint64;
    fLittle    : boolean;
    fMulti     : boolean;
    tip        : TImageProperties;
    tgp        : TGeoTiffProperties;
    ghRevis    : TGeoHeader;
    aiIndex    : array[0..9] of word;  // temp storage for GeoKeys indices

    convert    : TConvert;

  public
    constructor Create;
    destructor Destroy; override;
    function getStructure : TMultiTiff;
    function ReadTiff: boolean;
    function iNumberIFD : integer;
    function iNRGeoKeys : integer;
    function sGeoTag(iGeoIndex : integer; var iGTag : integer) : string;
    function sGeoLoc(iGeoIndex : integer; var iGLoc : integer) : string;
    function iGeoCount(iGeoIndex : integer) : integer;
    function iGeoValue(iGeoIndex : integer) : integer;
    function fLittleEndian : boolean;
    function fHasGeoTiff : boolean;
    procedure IFDEntry(iIFD : integer; var tie : TIFDEntry);  // retrieve IFD entry
    function sTag(iIFD : integer; var iTag : integer) : string;
    function sType(iIFD : integer; var iType : integer) : string;
    function iCount(iIFD : integer) : int64;
    function iValue(iIFD : integer) : int64;
    procedure SetTiffName(sFileName : string);
    function sTiffName : string;
    procedure GetImageProperties(var tipOut : TImageProperties);
    procedure GetGeoTiffProperties(var tgpOut : TGeoTiffProperties);
    procedure GetGeoTiffVersion(var gh : TGeoHeader);
    function sDescript(iTag, iVal : integer) : string;
    property fMultiImage : boolean read fMulti;
    // access array values
    function readDoubles(addr_val : integer; count : integer) : TDoubleArray;
    function readText(addr_val : integer; count : integer) : String;
  private
    function GetIFD(var fileTiff : TFileStream; ifd_offset : uint64) : uint64;
    procedure SetProperty(pifd : PIFDEntry; var fileTiff : TFileStream);
    procedure LoadPalette(pifd : PIFDEntry; var fileTiff : TFileStream);
    procedure FindGeoEntry(iGeoKey : word; var pifd : PIFDEntry);
    procedure FindGeoKey(iGeoKey : word; var pgk : PGeoKey);
    procedure SetGeoProperty(pgk : PGeoKey; var fileTiff : TFileStream);
    procedure ReadGeoTiffDirectory(var fileTiff : TFileStream);
    procedure Clean(l : TList);
  end;

  TTiffHeader = packed record
    iEndian    : word;
    iDot       : word;      // 42
    iPointer   : longword;   // point to first IFD
  end;

  TTiffIFDList = class
  private
    sFile       : string;
    iIFDPointer : int64;
    fileTiff    : file;
    header      : TTiffHeader;
    ifdEntry    : TIFDEntry;
    i,
    offset, zero : integer;
    offset_type,
    ifd_nrsize,
    entry_size  : integer;
    ts          : TMultiTiff;
    convert     : TConvert;

    procedure getSubStructure(offset: int64; img_nr, sub_nr : int64);
    procedure addIFD(img_nr, sub_nr: integer; offset : int64);

  public
    constructor Create(name : string);
    destructor Destroy; override;
    function getStructure : TMultiTiff;
  end;

var
  ttInfo : TTiffInfo;

implementation

Uses
    SysUtils;

type
  TBigTiffHeader = packed record
    iEndian    : word;
    iDot       : word;      // 43
    iOffsetSize : word;     // always 8
    iZero       : word;     // must be zero!
    iPointer   : uint64;     // point to first IFD (actually unsigned so can only use 63 bits)
  end;

  PDynIntArray = ^TDynIntArray;
  TDynIntArray = array[0..30000] of word;

// ------- implementation

constructor TConvert.Create;
var
    ab : word;
    pab : array[0..1] of byte absolute ab;
begin
  ab := $102; // 258 decimal
  isMachineLittle := pab[0] = 1;
end;

function TConvert.iSizeOfType(iType : integer) : integer;
begin
  case iType of
    1, 6  : Result := 1;   // byte
    2     : Result := 1;   // byte characters (ASCII)
    7     : Result := 1;   // byte undefined
    3, 8  : Result := 2;   // short
    4, 9  : Result := 4;   // long
    5, 10 : Result := 8;   // rational
    11    : Result := 4;   // float
    12    : Result := 8;   // double
    // subIFD
    13    : Result := 4;   // subIFD is type long
    // big tiff
    16    : Result := 8;   // tiff_long8 (unsigned)
    17    : Result := 8;   // tiff_slong8 (signed)
    18    : Result := 8;   // tiff_ifd8 (unsigned IFD offset)
  else
    Result := 0;
  end;
end;

function TConvert.truncTo(val: int64; iType: int64; iCount : int64): int64;
var nrBytes : integer;
    max_size : integer;
    i, offset : integer;
    tmp : int64;
    ab : array[0..7] of byte absolute tmp;
    cd : array[0..7] of byte absolute val;
begin
  Result := val;
  if iType = 2 then exit; // chars are OK

  max_size := 4;
  if IsBig then max_size := 8;
  nrBytes := iSizeOfType(iType) * iCount;
  if nrBytes >= max_size then exit; // all bytes in val are used

  tmp := 0;
  offset := 0;
  if not isTiffLittle then offset := max_size - nrBytes;
  for i := 0 to nrBytes - 1 do ab[i] := cd[i + offset];
  result := tmp;
end;

function TConvert.wSwap(w : word) : word;
begin
  if isTiffLittle then
    result := w
  else
    result := ((w shl 8) and $ff00) or ((w shr 8) and $ff);
end;

function TConvert.iSwap(i : longword) : longword;
begin
  if isTiffLittle then
    result := i
  else
    result := ((i shl 24) and $ff000000) or
              ((i shl 8)  and $00ff0000) or
              ((i shr 8)  and $0000ff00) or
              ((i shr 24) and $000000ff)
end;

function TConvert.rSwap(r : double) : double;
var ab : array[0..7] of byte absolute r;
    i : integer;
    b : byte;
begin
  if not isTiffLittle then
    for i := 0 to 3 do begin
      b := ab[i];
      ab[i] := ab[7 - i];
      ab[7 - i] := b;
    end;
  result := r;
end;

function TConvert.ibigSwap(big: uint64): uint64;
var ab : array[0..7] of byte absolute big;
    i : integer;
    b : byte;
begin
  if not isTiffLittle then
    for i := 0 to 3 do begin
      b := ab[i];
      ab[i] := ab[7 - i];
      ab[7 - i] := b;
    end;
  result := big;
end;

// -----------
procedure TImageProperties.Clear;
begin
  iWidth := 0;
  iHeight := 0;
  iNrColors := 0;
  iPhoto := -1;
  iXRes := 0;
  iYRes := 0;
  sResUnit := '';
  sImageDesc := '';
  fPalette := false;
  iComp :=0;
  iPredict := 0;
  fGeoTiff := false;
  fIsBigTiff := false;
  nodata_val := '';
  metadata_xml := '';
end;

procedure TImageProperties.Reset;
var b : boolean;
begin
  b := fIsBigTiff;
  Clear;
  fIsBigTiff := b;
end;

procedure TGeoTiffProperties.Clear;
begin
  sGTCitation := '';
  sGeoCitation := '';
  iGeoModel := -1;
  rLinUnitSize := -1.0;
  t3Scale.X := 0.0;
  t3Scale.Y := 0.0;
  t3Scale.Z := 0.0;
  iNrTiePts := 0;
  iNrDblParams := 0;
  fMatrix := false;
  ptieTiePoints := nil;
  ptmDblParam :=nil;
end;

procedure TMultiTiff.init;
begin
  count := 0;
  current := 0;
end;

constructor TTiffIFDList.Create(name : string);
begin
  sFile := name;

  convert := TConvert.Create;
  ts := TMultiTiff.Create;
end;

destructor TTiffIFDList.Destroy;
begin
  ts.Free;

  inherited Destroy;
end;

procedure TTiffIFDList.addIFD(img_nr, sub_nr : integer; offset : int64);
begin
  with ts.all[ts.current] do begin
    imgID := img_nr;
    subIFD := sub_nr;
    IFDoffset := offset;
  end;
  ts.count := ts.count + 1;
  ts.current := ts.current + 1;
end;

procedure TTiffIFDList.getSubStructure(offset : int64; img_nr, sub_nr : int64);
var
    nextIFD,
    nrEntries   : int64;
    nrEntriesSmall : integer absolute nrEntries;
    i : integer;
begin
  if offset = 0 then exit;

  addIFD(img_nr, sub_nr, offset); // add image IFD

  Seek(fileTiff, offset);
  nrEntries := 0;
  BlockRead(fileTiff, nrEntries, ifd_nrsize);
  if convert.isBigTiff then
    nrEntries := convert.truncTo(convert.ibigSwap(nrEntries), offset_type, 1)
  else
    nrEntries := convert.wSwap(nrEntries);
  offset := 0;
  for i := 0 to nrEntries - 1 do begin
    BlockRead(fileTiff, ifdEntry, entry_size);
    ifdEntry.iTag := convert.wSwap(ifdEntry.iTag);
    ifdEntry.iType := convert.wSwap(ifdEntry.iType);

    if ifdEntry.iType = 13 then begin
      nrEntries := ifdEntry.iValue;
      offset := convert.iSwap(nrEntriesSmall);
    end;
  end;
  BlockRead(fileTiff, nrEntries, convert.iSizeOfType(offset_type));
  if convert.isBigTiff then
    nextIFD := convert.ibigSwap(nrEntries)
  else
    nextIFD := convert.iSwap(nrEntriesSmall);
  getSubStructure(offset, img_nr, sub_nr + 1);  // check sub images

  getSubStructure(nextIFD, img_nr + 1, 0); // next image / page IFD
end;

function TTiffIFDList.getStructure: TMultiTiff;
var iIFDPointer : int64;
    header      : TTiffHeader;
    ifdEntry    : TIFDEntryV;
    i, zero     : integer;
    offset      : integer;
    ab          : array[0..1] of word absolute offset;
    img_nr,
    sub_nr      : integer;
begin
  entry_size := 12;
  ifd_nrsize := 2;
  offset_type := 3;
  AssignFile(fileTiff, sFile);
  FileMode := 0;
  try
    Reset(fileTiff, 1);
    BlockRead(fileTiff, header, 8);
    convert.isLittleEndian := header.iEndian = $4949;
    convert.IsBigTiff := convert.wSwap(header.iDot) = $2b;
    offset := convert.iSwap(header.iPointer);
    if convert.isBigTiff then begin
      entry_size := 20;
      ifd_nrsize := 8;
      offset_type := 18;
      if ab[0] + ab[1] <> 8 then exit;
      BlockRead(fileTiff, iIFDPointer, sizeof(int64));
      offset := convert.ibigSwap(iIFDPointer);
    end;

    img_nr := 0;
    sub_nr := 0;
    ts.init;

    getSubStructure(offset, img_nr, sub_nr);

  finally
    CloseFile(fileTiff);
  end;

  result := ts;
end;


constructor TTiffInfo.Create;
begin
  sFile := '';
  lifd := TList.Create;
  gifd := TList.Create;
  tip := TImageProperties.Create;
  tip.Clear;
  tgp := TGeoTiffProperties.Create;
  tgp.Clear;

  convert := TConvert.Create;
end;

destructor TTiffInfo.Destroy;
begin
  if tgp.ptieTiePoints <> nil then begin
    tgp.iNrTiePts := 0;
    FreeMem(tgp.ptieTiePoints);
  end;
  if tgp.ptmDblParam <> nil then begin
    tgp.iNrDblParams := 0;
    FreeMem(tgp.ptmDblParam);
  end;
  Clean(lifd);
  Clean(gifd);
  tgp.Free;
  tip.Free;
  gifd.Free;
  lifd.Free;

  convert.Free;

  inherited Destroy;
end;

procedure TTiffInfo.Clean(l : TList);
var i : integer;
begin
    if l <> nil then
        for i := 0 to l.Count - 1 do
            if l[i] <> nil then dispose(l.items[i]);
end;

procedure TTiffInfo.SetTiffName(sFileName : string);
begin
  sFile := sFileName;
  Clean(lifd);
  Clean(gifd);
  lifd.Free;
  gifd.Free;
  lifd := TList.Create;
  gifd := TLIst.Create;
  if not ReadTiff then begin
    sFile := '';
  end;
end;

function TTiffInfo.sTiffName : string;
begin
  result := sFile;
end;

procedure TTiffInfo.GetGeoTiffVersion(var gh : TGeoHeader);
begin
  gh := ghRevis;
end;

procedure TTiffInfo.FindGeoEntry(iGeoKey : word; var pifd : PIFDEntry);
var i : integer;
begin
  i := 0;
  if (lifd = nil) or (lifd.Count = 0) then
    pifd := nil
  else begin
    repeat
      pifd := lifd.Items[i];
      i := i + 1;
    until (i = lifd.Count) or (pifd^.iTag = iGeoKey);
    if (i > lifd.Count) or (pifd^.iTag <> iGeoKey) then
      pifd := nil;
  end;
end;

procedure TTiffInfo.FindGeoKey(iGeoKey : word; var pgk : PGeoKey);
var i : integer;
begin
  i := 0;
  repeat
    pgk := gifd.Items[i];
    i := i + 1;
  until (i = gifd.Count) or (pgk^.iKeyID = iGeoKey);
  if (i > gifd.Count) or (pgk^.iKeyID <> iGeoKey) then
    pgk := nil;
end;

procedure TTiffInfo.ReadGeoTiffDirectory(var fileTiff : TFileStream);
var i, iGeoSize : integer;
    pifd : PIFDEntry;
    iOldPos : longint;
    pgk     : PGeoKey;
begin
  FindGeoEntry(34735, pifd);     // GeoKeyDirectoryTag
  if pifd = nil then exit;
  iGeoSize := pifd^.iCount;
  fileTiff.Seek(pifd^.iValue, soFromBeginning);
  fileTiff.ReadBuffer(ghRevis, SizeOf(TGeoHeader));
  with ghRevis do begin
    iVersion := convert.wSwap(iVersion);
    iRevision := convert.wSwap(iRevision);
    iMinor := convert.wSwap(iMinor);
    iNrGeoKeys := convert.wSwap(iNrGeoKeys);
  end;
  iNrGeoEntries := ghRevis.iNrGeoKeys;
  for i := 0 to iNrGeoEntries - 1 do begin
    new(pgk);
    fileTiff.ReadBuffer(pgk^, SizeOf(TGeoKey));
    with pgk^ do begin
      iKeyID := convert.wSwap(iKeyID);
      iLoc   := convert.wSwap(iLoc);
      iCount := convert.wSwap(iCount);
      iValue := convert.wSwap(iValue);
    end;
    gifd.Add(pgk);
    SetGeoProperty(pgk, fileTiff);
  end;
end;

procedure TTiffInfo.SetGeoProperty(pgk : PGeoKey; var fileTiff : TFileStream);
var iOldPos : uint64;
    i       : integer;
    pifd    : PIFDEntry;
    pc      : PAnsiChar;
    tostr   : int64;
    max_size : integer;
begin
  max_size := 4;
  if tip.fIsBigTiff then max_size := 8;
  iOldPos := fileTiff.Position;
  with pgk^ do begin
    case iKeyID of
      1024 : if iLoc = 0 then
               tgp.iGeoModel := iValue;
      1026 : if iLoc <> 0 then begin
               FindGeoEntry(iLoc, pifd);
               fileTiff.Seek(pifd^.iValue + iValue, soFromBeginning);
               GetMem(pc, iCount);
               fileTiff.ReadBuffer(pc^, iCount);
               pc[iCount - 1] := #0;
               tgp.sGTCitation := StrPas(pc);
               FreeMem(pc);
             end;
      2049 : if iLoc <> 0 then begin
               FindGeoEntry(iLoc, pifd);
               if pifd <> nil then begin
                 if pifd^.iCount > max_size then begin
                   fileTiff.Seek(pifd^.iValue + iValue, soFromBeginning);
                   GetMem(pc, iCount);
                   fileTiff.ReadBuffer(pc^, iCount);
                   pc[iCount - 1] := #0;
                   tgp.sGeoCitation := StrPas(pc);
                   FreeMem(pc);
                 end
                 else begin
                   tostr := pifd^.iValue;
                   pc := addr(tostr);
                   pc[iCount - 1] := #0;
                   tgp.sGeoCitation := StrPas(pc);
                 end;
               end;
             end;
      2051 : if iLoc <> 0 then begin
               FindGeoEntry(iLoc, pifd);
               fileTiff.Seek(pifd^.iValue + iValue, soFromBeginning);
               fileTiff.ReadBuffer(tgp.rLinUnitSize, SizeOf(double));
               tgp.rLinUnitSize := convert.rSwap(tgp.rLinUnitSize);
             end;
      3073 : if iLoc <> 0 then begin
               FindGeoEntry(iLoc, pifd);
               if pifd <> nil then begin
                 fileTiff.Seek(pifd^.iValue + iValue, soFromBeginning);
                 GetMem(pc, iCount);
                 fileTiff.ReadBuffer(pc^, iCount);
                 pc[iCount - 1] := #0;
                 tgp.sPCSCitation := StrPas(pc);
                 FreeMem(pc);
               end;
             end;
    end;
  end;
  fileTiff.Seek(iOldPos, soFromBeginning);
end;

function TTiffInfo.iNrGeoKeys : integer;
begin
  Result := iNrGeoEntries;
end;

function TTiffInfo.sGeoTag(iGeoIndex : integer; var iGTag : integer) : string;
var pgk : PGeoKey;
begin
  pgk := gifd.Items[iGeoIndex];
  with pgk^ do begin
    iGTag := iKeyID;
    case iKeyID of
      1024 : Result := 'GTModelTypeGeoKey';
      1025 : Result := 'GTRasterTypeGeoKey';
      1026 : Result := 'GTCitationGeoKey';
      2048 : Result := 'GeographicTypeGeoKey';
      2049 : Result := 'GeogCitationGeoKey';
      2050 : Result := 'GeogGeodeticDatumGeoKey';
      2051 : Result := 'GeogPrimeMeridianGeoKey';
      2052 : Result := 'GeogLinearUnitsGeoKey';
      2053 : Result := 'GeogLinearUnitSizeGeoKey';
      2054 : Result := 'GeogAngularUnitsGeoKey';
      2055 : Result := 'GeogAngularUnitSizeGeoKey';
      2056 : Result := 'GeogEllipsoidGeoKey';
      2057 : Result := 'GeogSemiMajorAxisGeoKey';
      2058 : Result := 'GeogSemiMinorAxisGeoKey';
      2059 : Result := 'GeogInvFlatteningGeoKey';
      2060 : Result := 'GeogAzimuthUnitsGeoKey';
      2061 : Result := 'GeogPrimeMeridianLongGeoKey';
      3072 : Result := 'ProjectedCSTypeGeoKey';
      3073 : Result := 'PCSCitationGeoKey';
      3074 : Result := 'ProjectionGeoKey';
      3075 : Result := 'ProjCoordTransGeoKey';
      3076 : Result := 'ProjLinearUnitsGeoKey';
      3077 : Result := 'ProjLinearUnitSizeGeoKey';
      3078 : Result := 'ProjStdParallel1GeoKey';
      3079 : Result := 'ProjStdParallel2GeoKey';
      3080 : Result := 'ProjNatOriginLongGeoKey';
      3081 : Result := 'ProjNatOriginLatGeoKey';
      3082 : Result := 'ProjFalseEastingGeoKey';
      3083 : Result := 'ProjFalseNorthingGeoKey';
      3084 : Result := 'ProjFalseOriginLongGeoKey';
      3085 : Result := 'ProjFalseOriginLatGeoKey';
      3086 : Result := 'ProjFalseOriginEastingGeoKey';
      3087 : Result := 'ProjFalseOriginNorthingGeoKey';
      3088 : Result := 'ProjCenterLongGeoKey';
      3089 : Result := 'ProjCenterLatGeoKey';
      3090 : Result := 'ProjCenterEastingGeoKey';
      3091 : Result := 'ProjFalseOriginNorthingGeoKey';
      3092 : Result := 'ProjScaleAtNatOriginGeoKey';
      3093 : Result := 'ProjScaleAtCenterGeoKey';
      3094 : Result := 'ProjAzimuthAngleGeoKey';
      3095 : Result := 'ProjStraightVertPoleLongGeoKey';
      4096 : Result := 'VerticalCSTypeGeoKey';
      4097 : REsult := 'VerticalCitationGeoKey';
      4098 : Result := 'VerticalDatumGeoKey';
      4099 : Result := 'VerticalUnitsGeoKey';
    else
     Result := '<Unrecognized GeoTiff tag>';
    end;
  end;
end;

{
  sGeoLoc actually returns the type of the parameters; the iGLoc variable
  contains the GeoTiff Params tag
}
function TTiffInfo.sGeoLoc(iGeoIndex : integer; var iGLoc : integer) : string;
var pgk : PGeoKey;
begin
  pgk := gifd.Items[iGeoIndex];
  with pgk^ do begin
    iGLoc := iLoc;
    case iLoc of
      0     : Result := 'Short';
      34736 : Result := 'Double';
      34737 : Result := 'Char';
    else
      Result := '<Unknown>';
    end;
  end;
end;

function TTiffInfo.iGeoCount(iGeoIndex : integer) : integer;
var pgk : PGeoKey;
begin
  pgk := gifd.Items[iGeoIndex];
  with pgk^ do
    Result := iCount;
end;

function TTiffInfo.iGeoValue(iGeoIndex : integer) : integer;
var pgk : PGeoKey;
begin
  pgk := gifd.Items[iGeoIndex];
  with pgk^ do
    Result := iValue;
end;

function TTiffInfo.getStructure : TMultiTiff;
var
  ifds : TTiffIFDList;
begin
  ifds := TTiffIFDList.Create(sFile);
  result := ifds.getStructure;
end;

function TTiffInfo.ReadTiff : boolean;
var iIFDPointer : uint64;
    fileTiff    : TFileStream;
    header      : TTiffHeader;
    bighead     : TBigTiffHeader;
    pifd        : PIFDEntry;
    ifdSmall    : TSmallIFDEntry;
    i           : integer;
begin
  Result := true;
  fileTiff := TFileStream.Create(sFile, fmOpenRead or fmShareDenyWrite);
  fileTiff.ReadBuffer(header, sizeof(header));
  convert.isLittleEndian := header.iEndian = $4949;
  flittle := convert.isLittleEndian;
  tip.Clear;
  tgp.Clear;
  try
    convert.IsBigTiff := convert.wSwap(header.iDot) = $2b;
    tip.fIsBigTiff := convert.IsBigTiff;
    if convert.IsBigTiff then begin
        // bigtiff; reread the header
        fileTiff.Seek(0, soFromBeginning);
        fileTiff.ReadBuffer(bighead, sizeof(bighead));
        if (convert.wSwap(bighead.iOffsetSize) <> 8) or
           (convert.wSwap(bighead.iZero) <> 0) then begin
           Result := false;
           exit;
        end;

        iIFDPointer := convert.ibigSwap(bighead.iPointer);
    end
    else begin
        if convert.wSwap(header.iDot) <> $2a then exit;

        // regular TIFF
        iIFDPointer := convert.iSwap(header.iPointer);
    end;
    iIFDPointer := GetIFD(fileTiff, iIFDPointer);
    fMulti := iIFDPointer <> 0;
  finally
    fileTiff.Free;
  end;
end;

function TTiffInfo.GetIFD(var fileTiff : TFileStream; ifd_offset : uint64) : uint64;
var nextIFD     : uint64;
    pifd        : PIFDEntry;
    ifdSmall    : TSmallIFDEntry;
    i           : integer;
begin
  Result := 0;
  if tip.fIsBigTiff then begin
    fileTiff.Seek(ifd_offset, soFromBeginning);
    fileTiff.ReadBuffer(iNrEntries, sizeof(uint64));
    iNrEntries := convert.ibigSwap(iNrEntries);
    if ifd_offset + iNrEntries * 20 > fileTiff.Size then begin
        result := 0;
        iNrEntries := 0;
        exit;
    end;

    for i := 1 to iNrEntries do begin
      new(pifd);
      fileTiff.ReadBuffer(pifd^, sizeof(TIFDEntry));
      if pifd^.iTag = 0 then begin
        iNrEntries := i - 1;
        exit;
      end;
      with pifd^ do begin
        iTag := convert.wSwap(iTag);
        iType := convert.wSwap(iType);
        iCount := convert.ibigSwap(iCount);
        iValue := convert.iBigSwap(iValue);
        iValue := convert.truncTo(iValue, iType, iCount);
      end;
      lifd.Add(pifd);
      SetProperty(pifd, fileTiff);
    end;
    fileTiff.ReadBuffer(nextIFD, sizeof(int64));
  end
  else begin
    // regular TIFF
    fileTiff.Seek(ifd_offset, soFromBeginning);
    fileTiff.ReadBuffer(iNrEntries, sizeof(word));
    iNrEntries := convert.wSwap(iNrEntries);
    if ifd_offset + iNrEntries * 12 > fileTiff.Size then begin
        iNrEntries := 0;
        result := 0;
        exit;
    end;

    for i := 1 to iNrEntries do begin
      fileTiff.ReadBuffer(ifdSmall, sizeof(TSmallIFDEntry));
      if ifdSmall.iTag = 0 then begin
        iNrEntries := i - 1;
        exit;
      end;
      new(pifd);
      with pifd^ do begin
        iTag := convert.wSwap(ifdSmall.iTag);
        iType := convert.wSwap(ifdSmall.iType);
        iCount := convert.iSwap(ifdSmall.iCount);
        if (convert.iSizeOfType(iType) = 2) and (iCount = 1) then
          iValue := convert.wSwap(ifdSmall.iValue) and $ffff
        else
          iValue := convert.iSwap(ifdSmall.iValue);
      end;
      lifd.Add(pifd);
      SetProperty(pifd, fileTiff);
    end;
    nextIFD := 0; // clear needed; next we are only filling it halfway
    fileTiff.ReadBuffer(nextIFD, sizeof(longint));
  end;
  if tip.fGeoTiff then
    ReadGeoTiffDirectory(fileTiff);
  result := nextIFD;
end;

procedure TTiffInfo.LoadPalette(pifd : PIFDEntry; var fileTiff : TFileStream);
var i, iToRead, iRead : integer;
    buf : PDynIntArray;
    r, g, b : word;
begin
  iToRead := pifd^.iCount * convert.iSizeOfType(pifd^.iType);
  try
    GetMem(buf, iToRead);
    fileTiff.ReadBuffer(buf^, iToRead);
    with tip do begin
      for i := 0 to (iNrColors - 1) do begin
        r := convert.wSwap(Buf^[i]) div 256;
        g := convert.wSwap(Buf^[i + iNrColors]) div 256;
        b := convert.wSwap(Buf^[i + 2 * iNrColors]) div 256;
        palColors[i] := r + (g shl 8) + (b shl 16);
      end;
    end;
  finally
    FreeMem(buf);
  end;
end;

function TTiffInfo.readText(addr_val, count: integer): String;
var
    reader: TFileStream;
    // text : TStringList;
    pc: PAnsiChar;
    file_size, to_read, last_pos, val: integer;
begin
    if count > 4 then
    begin // TODO: handle bigtiff
        reader := TFileStream.Create(ttInfo.sTiffName, fmOpenRead);
        try
            reader.Seek(addr_val, soFromBeginning);
            file_size := reader.Size;
            to_read := count;
            if to_read > 32000 then    // prevent too long texts
                to_read := 32000;
            last_pos := addr_val + to_read;
            if last_pos > file_size then
            begin
                to_read := file_size - addr_val - 1; // # bytes to read
                Result := ''; // probably corrupt tif file anyway
                exit;
            end;

            GetMem(pc, to_read);
            reader.ReadBuffer(pc^, to_read);
            pc[to_read - 1] := #0;
            Result := StrPas(pc);
            FreeMem(pc);
        finally
            reader.Free;
        end;
    end
    else
    begin
        val := convert.iSwap(Cardinal(addr_val));
        pc := addr(val);
        pc[count - 1] := #0;
        Result := StrPas(pc)
    end;
end;

function TTiffInfo.readDoubles(addr_val : integer; count: integer): TDoubleArray;
var
    doubles : TDoubleArray;
    i : integer;
    reader : TFileStream;
    buffer : double;
begin
    if count <= 0 then begin
        result := nil;
        exit;
    end;
    reader := TFileStream.Create(sTiffName, fmOpenRead);
    SetLength(doubles, count);
    try
        reader.Seek(addr_val, soFromBeginning);
        for I := 0 to count - 1 do begin
            reader.Read(buffer, 8);
            doubles[i] := convert.rswap(buffer);
        end;

    finally
        reader.Free;
    end;

    result := doubles;
end;

procedure TTiffInfo.SetProperty(pifd : PIFDEntry; var fileTiff : TFileStream);
var iOldPos : uint64;
    i, iPos, iPlaneColors, iRead : integer;
    sz : integer;
    num : uint64;
    s : string;
    pc, pcLoop, pcLoop2 : PAnsiChar;
    tostr : uint64;
    max_size : integer;

  function iReadRational(iPos : uint64) : uint64;   // return truncated value
  var iCurPos : uint64;
      iNom, iDenom : int32;
  begin
    if not tip.fIsBigTiff then begin
        iCurPos := fileTiff.Position;
        fileTiff.Seek(iPos, soFromBeginning);
        fileTiff.ReadBuffer(iNom, SizeOf(int32));
        fileTiff.ReadBuffer(iDeNom, SizeOf(int32));
        fileTiff.Seek(iCurPos, soFromBeginning);
        iNom := convert.wSwap(iNom);
        iDenom := convert.wSwap(iDenom);
    end
    else begin
        iCurPos := convert.ibigSwap(iPos);
        iNom := iCurPos and $ffff;
        iDenom := iCurPos shr 32;
    end;
    if iDenom <> 0 then
      Result := iNom div iDenom
    else
      Result := 0;
  end;

  function rReadDouble : double;
  var r : double;
  begin
    fileTiff.ReadBuffer(r, sizeof(double));
    Result := convert.rSwap(r);
  end;

  function getString(pifd : PIFDEntry; offset : uint64) : string;
  var
    file_size, last_pos, to_read : integer;
  begin
    if pifd^.iCount > max_size then begin
      to_read := pifd^.iCount;
      file_size := fileTiff.Size;
      last_pos := pifd^.iValue + to_read;
      if last_pos > file_size then begin
        to_read := file_size - pifd^.iValue - 1;  // # bytes to read
        result := '';        // probably corrupt tif file anyway
        exit;
      end;

      iOldPos := fileTiff.Position;
      fileTiff.Seek(pifd^.iValue, soFromBeginning);
      GetMem(pc, to_read);
      fileTiff.ReadBuffer(pc^, to_read);
      pc[to_read - 1] := #0;
      Result := StrPas(pc);
      FreeMem(pc);
      fileTiff.Seek(iOldPos, soFromBeginning);
        end
        else
        begin
            tostr := convert.iSwap(Cardinal(pifd^.iValue));
      pc := addr(tostr);
      pc[pifd^.iCount - 1] := #0;
      Result := StrPas(pc);
    end;
  end;

begin
  max_size := 4;
  if tip.fIsBigTiff then max_size := 8;
  case pifd^.iTag of
    256 : tip.iWidth := pifd^.iValue;
    257 : tip.iHeight := pifd^.iValue;
    258 : if pifd^.iCount > 1 then begin
            tip.iNrColors := 1;
            tip.iNrBits := 0;
            sz := convert.iSizeOfType(pifd^.iType);     // bytes used in value
            if pifd^.iCount * sz > max_size then begin
                iOldPos := fileTiff.Position;
                fileTiff.Seek(pifd^.iValue, soFromBeginning);
                iPlaneColors := 0;
                for i := 1 to pifd^.iCount do begin
                  fileTiff.ReadBuffer(iPlaneColors, convert.iSizeOfType(pifd^.iType));
                  iPlaneColors := convert.wSwap(iPlaneColors);
                  tip.iNrBits := tip.iNrBits + iPlaneColors;
                  tip.iNrColors := tip.iNrColors * (1 shl iPlaneColors);
                end;
                fileTiff.Seek(iOldPos, soFromBeginning);
            end
            else begin
                num := pifd^.iValue;
                for i := 1 to pifd^.iCount do begin
                    iPlaneColors := num and $ff;
                    tip.iNrBits := tip.iNrBits + iPlaneColors;
                    tip.iNrColors := (1 shl iPlaneColors);      // colors per plane
                    num := num shr (sz * 8);
                end;
            end;
          end
          else
            tip.iNrColors := 1 shl pifd^.iValue;
    259 : tip.iComp := pifd^.iValue;
    262 : tip.iPhoto := pifd^.iValue;
    270 : if pifd^.iCount > 0 then begin
            iOldPos := fileTiff.Position;
            fileTiff.Seek(pifd^.iValue, soFromBeginning);
            GetMem(pc, pifd^.iCount);
            fileTiff.ReadBuffer(pc^, pifd^.iCount);
            tip.sImageDesc := pc;
            FreeMem(pc);
            fileTiff.Seek(iOldPos, soFromBeginning);
          end;
    282 : if pifd^.iType = 5 then begin
             tip.iXres := iReadRational(pifd^.iValue);;
          end;
    283 : if pifd^.iType = 5 then begin
             tip.iYres := iReadRational(pifd^.iValue);;
          end;
    296 : case pifd^.iValue of
            1 : tip.sResUnit := 'No meaningful unit of measure';
            2 : tip.sResUnit := 'Inch';
            3 : tip.sResUnit := 'Centimeter';
          else
            tip.sResUnit := '';
          end;
    317 : tip.iPredict := pifd^.iValue;
    320 : if pifd^.iCount > 1 then begin
            tip.fPalette := true;
            iOldPos := fileTiff.Position;
            fileTiff.Seek(pifd^.iValue, soFromBeginning);
            LoadPalette(pifd, fileTiff);
            fileTiff.Seek(iOldPos, soFromBeginning);
          end;
{ Geo-Tiff keys: }
    34735 : tip.fGeoTiff := true;
    33550 : begin        // ModelPixelScaleTag
              iOldPos := fileTiff.Position;
              fileTiff.Seek(pifd^.iValue, soFromBeginning);
              tgp.t3Scale.X := rReadDouble;
              tgp.t3Scale.Y := rReadDouble;
              tgp.t3Scale.Z := rReadDouble;
              fileTiff.Seek(iOldPos, soFromBeginning);
            end;
    33922 : begin        // ModelTiepointTag
              iOldPos := fileTiff.Position;
              fileTiff.Seek(pifd^.iValue, soFromBeginning);
              tgp.iNrTiePts := pifd^.iCount div 6;
              if tgp.iNrTiePts > 0 then begin
                GetMem(tgp.ptieTiePoints, tgp.iNrTiePts * sizeof(TTiepoint));
                for i := 0 to tgp.iNrTiePts - 1 do
                  with tgp.ptieTiePoints^[i] do begin
                    t3Pixel.X := rReadDouble;
                    t3Pixel.Y := rReadDouble;
                    t3Pixel.Z := rReadDouble;
                    t3World.X := rReadDouble;
                    t3World.Y := rReadDouble;
                    t3World.Z := rReadDouble;
                  end;
              end;
              fileTiff.Seek(iOldPos, soFromBeginning);
            end;
    34264 : begin          // ModelTransformationTag
            iOldPos := fileTiff.Position;
              fileTiff.Seek(pifd^.iValue, soFromBeginning);
              tgp.fMatrix := true;
              for i := 0 to 15 do
                tgp.matTransform[i] := rReadDouble;
              fileTiff.Seek(iOldPos, soFromBeginning);
            end;
    34736 : begin
              iOldPos := fileTiff.Position;
              fileTiff.Seek(pifd^.iValue, soFromBeginning);
              tgp.iNrDblParams := pifd^.iCount;
              GetMem(tgp.ptmDblParam, pifd^.iCount * sizeOf(TMetaTag));
              for i := 0 to tgp.inrdblParams - 1 do
                tgp.ptmDblParam^[i].rVal := rReadDouble;
              fileTiff.Seek(iOldPos, soFromBeginning);
            end;
    // GDAL tags
    42112 : begin  // GDAL_Metadata
                tip.metadata_xml := getString(pifd, 0);
            end;
    42113 : begin  // GDAL_Nodata (ASCII)
              tip.nodata_val := getString(pifd, 0);
            end;
  end;
end;

procedure TTiffInfo.GetImageProperties(var tipOut : TImageProperties);
begin
  tipOut := tip;
end;

procedure TTiffInfo.GetGeoTiffProperties(var tgpOut : TGeoTiffProperties);
begin
  tgpOut := tgp;
end;

function TTiffInfo.fLittleEndian : boolean;
begin
  Result := fLittle;
end;

function TTiffInfo.fHasGeoTiff : boolean;
begin
  Result := tip.fGeoTiff;
end;

function TTiffInfo.iNumberIFD : integer;
begin
  Result := iNrEntries;
end;

function TTiffInfo.sTag(iIFD : integer; var iTag : integer) : string;
var pifd : PIFDEntry;
begin
  pifd := lifd.items[iIFD];
  iTag := pifd^.iTag;
  case iTag of
    254 : sTag := 'NewSubFileType';
    255 : stag := 'SubFileType';
    256 : sTag := 'ImageWidth';
    257 : sTag := 'ImageHeight';
    258 : sTag := 'BitsPerSample';
    259 : sTag := 'Compression';
    262 : sTag := 'PhotometricInterpretation';
    263 : sTag := 'Thresholding';
    265 : sTag := 'CellWidth';
    266 : sTag := 'FillOrder';
    269 : sTag := 'DocumentName';
    270 : sTag := 'ImageDescription';
    271 : sTag := 'Make';
    272 : sTag := 'Model';
    273 : sTag := 'StripOffsets';
    274 : sTag := 'Orientation';
    277 : sTag := 'SamplesPerPixel';
    278 : sTag := 'RowsPerStrip';
    279 : sTag := 'StripByteCounts';
    280 : sTag := 'MinSampleValue';
    281 : sTag := 'MaxSampleValue';
    282 : sTag := 'XResolution';
    283 : sTag := 'YResolution';
    284 : sTag := 'PlanarConfiguration';
    285 : sTag := 'PageName';
    286 : sTag := 'XPosition';
    287 : sTag := 'YPosition';
    288 : sTag := 'FreeOffsets';
    289 : sTag := 'FreeBytesCount';
    290 : stag := 'GrayResponseUnit';
    291 : stag := 'GrayResponseCurve';
    292 : stag := 'T4Options';
    293 : stag := 'T6Options';
    296 : sTag := 'ResolutionUnit';
    297 : sTag := 'PageNumber';
    301 : sTag := 'TransferFunction';
    305 : sTag := 'Software';
    306 : sTag := 'DateTime';
    315 : sTag := 'Artist';
    316 : sTag := 'HostComputer';
    317 : sTag := 'Predictor';
    318 : sTag := 'WhitePoint';
    319 : sTag := 'PrimaryChromaticities';
    320 : sTag := 'ColorMap';
    321 : sTag := 'HalfToneHints';
    322 : sTag := 'TileWidth';
    323 : sTag := 'TileLength';
    324 : sTag := 'TileOffsets';
    325 : sTag := 'TileByteCounts';
    330 : sTag := 'SubIFDs';
    332 : sTag := 'InkSet';
    333 : sTag := 'InkNames';
    334 : sTag := 'NumberOfInks';
    336 : sTag := 'DotRange';
    337 : sTag := 'TargetPrinter';
    338 : sTag := 'ExtraSamples';
    339 : sTag := 'SampleFormat';
    340 : sTag := 'SMinSampleValue';
    341 : sTag := 'SMaxSampleValue';
    342 : sTag := 'TransferRange';
    530 : sTag := 'YCbCrSubSampling';
    532 : sTag := 'ReferenceBlackWhite';
    33432 : sTag := 'Copyright';
{ Geo-Tiff tags }
    33550 : sTag := 'ModelPixelScaleTag';
    33920 : sTag := 'IntergraphMatrixTag';
    33922 : sTag := 'ModelTiepointTag';
    34264 : sTag := 'ModelTransformationTag';
    34735 : sTag := 'GeoKeyDirectoryTag';
    34736 : sTag := 'GeoDoubleParamsTag';
    34737 : sTag := 'GeoAsciiParamsTag';
{ Adobe Tags }
    37724 : sTag := 'ImageSourceData';
    34377 : sTag := 'Photoshop';
{ GDAL tags }
    42112 : sTag := 'GDAL_Metadata';
    42113 : sTag := 'GDAL_NoData';
{ JPEG specific tags }
    347 : sTag := 'JPEGTables';
{ Deprecated JPEG tags }
    513 : sTag := 'JPEGInterchangeFormat (deprecated)';
{ Pagemaker tags }
    343 : sTag := 'ClipPath';
    344 : sTag := 'XClipPathUnits';
    345 : sTag := 'YClipPathUnits';
    346 : sTag := 'Indexed';
    // OPI-Related Tags
    351 : sTag := 'OPIProxy';
    32781 : sTag := 'ImageID';
    // ANSI IT8 TIFF/IT specification
    34019 : sTag := 'IT8RasterPadding';
    34022 : sTag := 'IT8ColorTable';
{ XMP }
    700 : sTag := 'XMP';
{ IPTC }
    33723 : sTag := 'IPTC';
{ EXIF }
    34665 : sTag := 'Exif IFD';
    34675 : sTag := 'ICC Profile';
{ GPS }
    34853 : sTag := 'GPS IFD';
{ Diverse }
    34732  : sTag := 'ImageLayer';
  else
    if iTag >= 65000 then
        sTag := 'Custom (private)'
    else
        sTag := '<Unrecognized>';
  end;
end;

function TTiffInfo.sType(iIFD : integer; var iType : integer) : string;
var pifd : PIFDEntry;
begin
  pifd := lifd.items[iIFD];
  iType := pifd^.iType;
  case iType of
    1  : sType := 'Byte (unsigned)';
    2  : sType := 'Char (7-bit)';
    3  : sType := 'Short (unsigned)';
    4  : sType := 'LongInt (unsigned)';
    5  : sType := 'Rational';
    6  : sType := 'Byte (signed)';
    7  : sType := 'Byte undefined';
    8  : sType := 'Short (signed)';
    9  : sType := 'LongInt (signed)';
    10 : sType := 'Rational (signed)';
    11 : sType := 'Float (4 bytes IEEE)';
    12 : sType := 'Double (8 bytes IEEE)';
    // SubIFD
    13 : sType := 'IFD';
    14 : sType := 'Unicode';
    15 : sType := 'Complex';
    // Big tiff
    16 : sType := 'Int 64 (unsigned)';
    17 : sType := 'Int 64 (signed)';
    18 : sType := 'Int 64 (IFD offset)';
  else
    sType := '<Unknown>'
  end;
end;

function TTiffInfo.iCount(iIFD : integer) : int64;
var pifd : PIFDEntry;
begin
  pifd := lifd.items[iIFD];
  result := pifd^.iCount;
end;

function TTiffInfo.iValue(iIFD : integer) : int64;
var pifd : PIFDEntry;
begin
  pifd := lifd.items[iIFD];
  result := pifd^.iValue;
end;

procedure TTiffInfo.IFDEntry(iIFD : integer; var tie : TIFDEntry);
var pifd : PIFDEntry;
begin
  pifd := lifd.items[iIFD];
  tie := pifd^;
end;

function TTiffInfo.sDescript(iTag, iVal : integer) : string;
var fCont : boolean;
    pgk   : PGeoKey;
begin
  Result := '';
  fCont := false;
  FindGeoKey(iTag, pgk);
  if pgk <> nil then begin
    if pgk^.iLoc = 34736 then begin  // for doubles only
      iVal := iVal div sizeof(double);
      with tgp do
        Result := Format('%.6f', [ptmDblParam^[iVal].rVal]);
      exit;
    end;
  end;
{
  The double case statement is to extract user-defined and undefined;
  this has to be done after checking for configuration tags because the iVal
  must be checked. In case of the configuration tags this can lead to wrong
  descriptions if not separated.
}
  case iTag of
    1024 : case iVal of
             1 : Result := 'Projected Coordinate System';
             2 : Result := 'Geographic (LatLon)';
             3 : Result := 'Geocentric (X,Y,Z)';
           end;
    1025 : case iVal of
             1 : Result := 'Pixel is area';
             2 : Result := 'Pixel is point';
           end;
    1026 : Result := '"' + tgp.sGTCitation + '"';
    2049 : Result := '"' + tgp.sGeoCitation + '"';
    3073 : Result := '"' + tgp.sPCSCitation + '"';
  else
    fCont := true;
  end;
  if fCont then begin
    if iVal = 0 then begin
      Result := 'Undefined';
      exit;
    end
    else if iVal = 32767 then begin
      Result := 'User-defined';
      exit;
    end;
    Result :='';
    case iTag of
      2052,
      3076,
      4099 : case iVal of
               9001 : Result := 'Linear_Meter';
               9002 : Result := 'Linear_Foot';
               9003 : Result := 'Linear_Foot_US_Survey';
               9004 : Result := 'Linear_Foot_Modified_American';
               9005 : Result := 'Linear_Foot_Clarke';
               9006 : Result := 'Linear_Foot_Indian';
               9007 : Result := 'Linear_Link';
               9008 : Result := 'Linear_Link_Benoit';
               9009 : Result := 'Linear_Link_Sears';
               9010 : Result := 'Linear_Chain_Benoit';
               9011 : Result := 'Linear_Chain_Sears';
               9012 : Result := 'Linear_Yard_Sears';
               9013 : Result := 'Linear_Yard_Indian';
               9014 : Result := 'Linear_Fathom';
               9015 : Result := 'Linear_Mile_International_Nautical';
             end;
      2054,
      2060 : Case iVal of
               9101 : Result := 'Angular_Radian';
               9102 : Result := 'Angular_Degree';
               9103 : Result := 'Angular_Arc_Minute';
               9104 : Result := 'Angular_Arc_Second';
               9105 : Result := 'Angular_Grad';
               9106 : Result := 'Angular_Gon';
               9107 : Result := 'Angular_DMS';
               9108 : Result := 'Angular_DMS_Hemisphere';
             end;
      2048 : case iVal of    { GCS codes }
{ Ellipsoid only GCS: the numeric code is equal to the code of the correspoding
   EPSG ellipsoid, minus 3000.}
               4001 : Result := 'GCSE_Airy1830';
               4002 : Result := 'GCSE_AiryModified1849';
               4003 : Result := 'GCSE_AustralianNationalSpheroid';
               4004 : Result := 'GCSE_Bessel1841';
               4005 : Result := 'GCSE_BesselModified';
               4006 : Result := 'GCSE_BesselNamibia';
               4007 : Result := 'GCSE_Clarke1858';
               4008 : Result := 'GCSE_Clarke1866';
               4009 : Result := 'GCSE_Clarke1866Michigan';
               4010 : Result := 'GCSE_Clarke1880_Benoit';
               4011 : Result := 'GCSE_Clarke1880_IGN';
               4012 : Result := 'GCSE_Clarke1880_RGS';
               4013 : Result := 'GCSE_Clarke1880_Arc';
               4014 : Result := 'GCSE_Clarke1880_SGA1922';
               4015 : Result := 'GCSE_Everest1830_1937Adjustment';
               4016 : Result := 'GCSE_Everest1830_1967Definition';
               4017 : Result := 'GCSE_Everest1830_1975Definition';
               4018 : Result := 'GCSE_Everest1830Modified';
               4019 : Result := 'GCSE_GRS1980';
               4020 : Result := 'GCSE_Helmert1906';
               4021 : Result := 'GCSE_IndonesianNationalSpheroid';
               4022 : Result := 'GCSE_International1924';
               4023 : Result := 'GCSE_International1967';
               4024 : Result := 'GCSE_Krassowsky1940';
               4025 : Result := 'GCSE_NWL9D';
               4026 : Result := 'GCSE_NWL10D';
               4027 : Result := 'GCSE_Plessis1817';
               4028 : Result := 'GCSE_Struve1860';
               4029 : Result := 'GCSE_WarOffice';
               4030 : Result := 'GCSE_WGS84';
               4031 : Result := 'GCSE_GEM10C';
               4032 : Result := 'GCSE_OSU86F';
               4033 : Result := 'GCSE_OSU91A';
               4034 : Result := 'GCSE_Clarke1880';
               4035 : Result := 'GCSE_Sphere';
{ Geodetic datum using Greenwich PM have codes equal to
  the corresponding Datum code - 2000.}
               4201 : Result := 'GCS_Adindan';
               4202 : Result := 'GCS_AGD66';
               4203 : Result := 'GCS_AGD84';
               4204 : Result := 'GCS_Ain_el_Abd';
               4205 : Result := 'GCS_Afgooye';
               4206 : Result := 'GCS_Agadez';
               4207 : Result := 'GCS_Lisbon';
               4208 : Result := 'GCS_Aratu';
               4209 : Result := 'GCS_Arc_1950';
               4210 : Result := 'GCS_Arc_1960';
               4211 : Result := 'GCS_Batavia';
               4212 : Result := 'GCS_Barbados';
               4213 : Result := 'GCS_Beduaram';
               4214 : Result := 'GCS_Beijing_1954';
               4215 : Result := 'GCS_Belge_1950';
               4216 : Result := 'GCS_Bermuda_1957';
               4217 : Result := 'GCS_Bern_1898';
               4218 : Result := 'GCS_Bogota';
               4219 : Result := 'GCS_Bukit_Rimpah';
               4220 : Result := 'GCS_Camacupa';
               4221 : Result := 'GCS_Campo_Inchauspe';
               4222 : Result := 'GCS_Cape';
               4223 : Result := 'GCS_Carthage';
               4224 : Result := 'GCS_Chua';
               4225 : Result := 'GCS_Corrego_Alegre';
               4226 : Result := 'GCS_Cote_d_Ivoire';
               4227 : Result := 'GCS_Deir_ez_Zor';
               4228 : Result := 'GCS_Douala';
               4229 : Result := 'GCS_Egypt_1907';
               4230 : Result := 'GCS_ED50';
               4231 : Result := 'GCS_ED87';
               4232 : Result := 'GCS_Fahud';
               4233 : Result := 'GCS_Gandajika_1970';
               4234 : Result := 'GCS_Garoua';
               4235 : Result := 'GCS_Guyane_Francaise';
               4236 : Result := 'GCS_Hu_Tzu_Shan';
               4237 : Result := 'GCS_HD72';
               4238 : Result := 'GCS_ID74';
               4239 : Result := 'GCS_Indian_1954';
               4240 : Result := 'GCS_Indian_1975';
               4241 : Result := 'GCS_Jamaica_1875';
               4242 : Result := 'GCS_JAD69';
               4243 : Result := 'GCS_Kalianpur';
               4244 : Result := 'GCS_Kandawala';
               4245 : Result := 'GCS_Kertau';
               4246 : Result := 'GCS_KOC';
               4247 : Result := 'GCS_La_Canoa';
               4248 : Result := 'GCS_PSAD56';
               4249 : Result := 'GCS_Lake';
               4250 : Result := 'GCS_Leigon';
               4251 : Result := 'GCS_Liberia_1964';
               4252 : Result := 'GCS_Lome';
               4253 : Result := 'GCS_Luzon_1911';
               4254 : Result := 'GCS_Hito_XVIII_1963';
               4255 : Result := 'GCS_Herat_North';
               4256 : Result := 'GCS_Mahe_1971';
               4257 : Result := 'GCS_Makassar';
               4258 : Result := 'GCS_EUREF89';
               4259 : Result := 'GCS_Malongo_1987';
               4260 : Result := 'GCS_Manoca';
               4261 : Result := 'GCS_Merchich';
               4262 : Result := 'GCS_Massawa';
               4263 : Result := 'GCS_Minna';
               4264 : Result := 'GCS_Mhast';
               4265 : Result := 'GCS_Monte_Mario';
               4266 : Result := 'GCS_M_poraloko';
               4267 : Result := 'GCS_NAD27';
               4268 : Result := 'GCS_NAD_Michigan';
               4269 : Result := 'GCS_NAD83';
               4270 : Result := 'GCS_Nahrwan_1967';
               4271 : Result := 'GCS_Naparima_1972';
               4272 : Result := 'GCS_GD49';
               4273 : Result := 'GCS_NGO_1948';
               4274 : Result := 'GCS_Datum_73';
               4275 : Result := 'GCS_NTF';
               4276 : Result := 'GCS_NSWC_9Z_2';
               4277 : Result := 'GCS_OSGB_1936';
               4278 : Result := 'GCS_OSGB70';
               4279 : Result := 'GCS_OS_SN80';
               4280 : Result := 'GCS_Padang';
               4281 : Result := 'GCS_Palestine_1923';
               4282 : Result := 'GCS_Pointe_Noire';
               4283 : Result := 'GCS_GDA94';
               4284 : Result := 'GCS_Pulkovo_1942';
               4285 : Result := 'GCS_Qatar';
               4286 : Result := 'GCS_Qatar_1948';
               4287 : Result := 'GCS_Qornoq';
               4288 : Result := 'GCS_Loma_Quintana';
               4289 : Result := 'GCS_Amersfoort';
               4290 : Result := 'GCS_RT38';
               4291 : Result := 'GCS_SAD69';
               4292 : Result := 'GCS_Sapper_Hill_1943';
               4293 : Result := 'GCS_Schwarzeck';
               4294 : Result := 'GCS_Segora';
               4295 : Result := 'GCS_Serindung';
               4296 : Result := 'GCS_Sudan';
               4297 : Result := 'GCS_Tananarive';
               4298 : Result := 'GCS_Timbalai_1948';
               4299 : Result := 'GCS_TM65';
               4300 : Result := 'GCS_TM75';
               4301 : Result := 'GCS_Tokyo';
               4302 : Result := 'GCS_Trinidad_1903';
               4303 : Result := 'GCS_TC_1948';
               4304 : Result := 'GCS_Voirol_1875';
               4305 : Result := 'GCS_Voirol_Unifie';
               4306 : Result := 'GCS_Bern_1938';
               4307 : Result := 'GCS_Nord_Sahara_1959';
               4308 : Result := 'GCS_Stockholm_1938';
               4309 : Result := 'GCS_Yacare';
               4310 : Result := 'GCS_Yoff';
               4311 : Result := 'GCS_Zanderij';
               4312 : Result := 'GCS_MGI';
               4313 : Result := 'GCS_Belge_1972';
               4314 : Result := 'GCS_DHDN';
               4315 : Result := 'GCS_Conakry_1905';
               4322 : Result := 'GCS_WGS_72';
               4324 : Result := 'GCS_WGS_72BE';
               4326 : Result := 'GCS_WGS_84';
               4801 : Result := 'GCS_Bern_1898_Bern';
               4802 : Result := 'GCS_Bogota_Bogota';
               4803 : Result := 'GCS_Lisbon_Lisbon';
               4804 : Result := 'GCS_Makassar_Jakarta';
               4805 : Result := 'GCS_MGI_Ferro';
               4806 : Result := 'GCS_Monte_Mario_Rome';
               4807 : Result := 'GCS_NTF_Paris';
               4808 : Result := 'GCS_Padang_Jakarta';
               4809 : Result := 'GCS_Belge_1950_Brussels';
               4810 : Result := 'GCS_Tananarive_Paris';
               4811 : Result := 'GCS_Voirol_1875_Paris';
               4812 : Result := 'GCS_Voirol_Unifie_Paris';
               4813 : Result := 'GCS_Batavia_Jakarta';
               4901 : Result := 'GCS_ATF_Paris';
               4902 : Result := 'GCS_NDG_Paris';
             end;
      2050 : case iVal of     { Geodetic Datum Codes }
{Ellipsoid-Only Datum:
   the numeric code is equal to the corresponding ellipsoid
   code, minus 1000.}
               6001 : Result := 'DatumE_Airy1830';
               6002 : Result := 'DatumE_AiryModified1849';
               6003 : Result := 'DatumE_AustralianNationalSpheroid';
               6004 : Result := 'DatumE_Bessel1841';
               6005 : Result := 'DatumE_BesselModified';
               6006 : Result := 'DatumE_BesselNamibia';
               6007 : Result := 'DatumE_Clarke1858';
               6008 : Result := 'DatumE_Clarke1866';
               6009 : Result := 'DatumE_Clarke1866Michigan';
               6010 : Result := 'DatumE_Clarke1880_Benoit';
               6011 : Result := 'DatumE_Clarke1880_IGN';
               6012 : Result := 'DatumE_Clarke1880_RGS';
               6013 : Result := 'DatumE_Clarke1880_Arc';
               6014 : Result := 'DatumE_Clarke1880_SGA1922';
               6015 : Result := 'DatumE_Everest1830_1937Adjustment';
               6016 : Result := 'DatumE_Everest1830_1967Definition';
               6017 : Result := 'DatumE_Everest1830_1975Definition';
               6018 : Result := 'DatumE_Everest1830Modified';
               6019 : Result := 'DatumE_GRS1980';
               6020 : Result := 'DatumE_Helmert1906';
               6021 : Result := 'DatumE_IndonesianNationalSpheroid';
               6022 : Result := 'DatumE_International1924';
               6023 : Result := 'DatumE_International1967';
               6024 : Result := 'DatumE_Krassowsky1960';
               6025 : Result := 'DatumE_NWL9D';
               6026 : Result := 'DatumE_NWL10D';
               6027 : Result := 'DatumE_Plessis1817';
               6028 : Result := 'DatumE_Struve1860';
               6029 : Result := 'DatumE_WarOffice';
               6030 : Result := 'DatumE_WGS84';
               6031 : Result := 'DatumE_GEM10C';
               6032 : Result := 'DatumE_OSU86F';
               6033 : Result := 'DatumE_OSU91A';
               6034 : Result := 'DatumE_Clarke1880';
               6035 : Result := 'DatumE_Sphere';
{ EPSG Datum Based on EPSG Datum }
               6201 : Result := 'Datum_Adindan';
               6202 : Result := 'Datum_Australian_Geodetic_Datum_1966';
               6203 : Result := 'Datum_Australian_Geodetic_Datum_1984';
               6204 : Result := 'Datum_Ain_el_Abd_1970';
               6205 : Result := 'Datum_Afgooye';
               6206 : Result := 'Datum_Agadez';
               6207 : Result := 'Datum_Lisbon';
               6208 : Result := 'Datum_Aratu';
               6209 : Result := 'Datum_Arc_1950';
               6210 : Result := 'Datum_Arc_1960';
               6211 : Result := 'Datum_Batavia';
               6212 : Result := 'Datum_Barbados';
               6213 : Result := 'Datum_Beduaram';
               6214 : Result := 'Datum_Beijing_1954';
               6215 : Result := 'Datum_Reseau_National_Belge_1950';
               6216 : Result := 'Datum_Bermuda_1957';
               6217 : Result := 'Datum_Bern_1898';
               6218 : Result := 'Datum_Bogota';
               6219 : Result := 'Datum_Bukit_Rimpah';
               6220 : Result := 'Datum_Camacupa';
               6221 : Result := 'Datum_Campo_Inchauspe';
               6222 : Result := 'Datum_Cape';
               6223 : Result := 'Datum_Carthage';
               6224 : Result := 'Datum_Chua';
               6225 : Result := 'Datum_Corrego_Alegre';
               6226 : Result := 'Datum_Cote_d_Ivoire';
               6227 : Result := 'Datum_Deir_ez_Zor';
               6228 : Result := 'Datum_Douala';
               6229 : Result := 'Datum_Egypt_1907';
               6230 : Result := 'Datum_European_Datum_1950';
               6231 : Result := 'Datum_European_Datum_1987';
               6232 : Result := 'Datum_Fahud';
               6233 : Result := 'Datum_Gandajika_1970';
               6234 : Result := 'Datum_Garoua';
               6235 : Result := 'Datum_Guyane_Francaise';
               6236 : Result := 'Datum_Hu_Tzu_Shan';
               6237 : Result := 'Datum_Hungarian_Datum_1972';
               6238 : Result := 'Datum_Indonesian_Datum_1974';
               6239 : Result := 'Datum_Indian_1954';
               6240 : Result := 'Datum_Indian_1975';
               6241 : Result := 'Datum_Jamaica_1875';
               6242 : Result := 'Datum_Jamaica_1969';
               6243 : Result := 'Datum_Kalianpur';
               6244 : Result := 'Datum_Kandawala';
               6245 : Result := 'Datum_Kertau';
               6246 : Result := 'Datum_Kuwait_Oil_Company';
               6247 : Result := 'Datum_La_Canoa';
               6248 : Result := 'Datum_Provisional_S_American_Datum_1956';
               6249 : Result := 'Datum_Lake';
               6250 : Result := 'Datum_Leigon';
               6251 : Result := 'Datum_Liberia_1964';
               6252 : Result := 'Datum_Lome';
               6253 : Result := 'Datum_Luzon_1911';
               6254 : Result := 'Datum_Hito_XVIII_1963';
               6255 : Result := 'Datum_Herat_North';
               6256 : Result := 'Datum_Mahe_1971';
               6257 : Result := 'Datum_Makassar';
               6258 : Result := 'Datum_European_Reference_System_1989';
               6259 : Result := 'Datum_Malongo_1987';
               6260 : Result := 'Datum_Manoca';
               6261 : Result := 'Datum_Merchich';
               6262 : Result := 'Datum_Massawa';
               6263 : Result := 'Datum_Minna';
               6264 : Result := 'Datum_Mhast';
               6265 : Result := 'Datum_Monte_Mario';
               6266 : Result := 'Datum_M_poraloko';
               6267 : Result := 'Datum_North_American_Datum_1927';
               6268 : Result := 'Datum_NAD_Michigan';
               6269 : Result := 'Datum_North_American_Datum_1983';
               6270 : Result := 'Datum_Nahrwan_1967';
               6271 : Result := 'Datum_Naparima_1972';
               6272 : Result := 'Datum_New_Zealand_Geodetic_Datum_1949';
               6273 : Result := 'Datum_NGO_1948';
               6274 : Result := 'Datum_Datum_73';
               6275 : Result := 'Datum_Nouvelle_Triangulation_Francaise';
               6276 : Result := 'Datum_NSWC_9Z_2';
               6277 : Result := 'Datum_OSGB_1936';
               6278 : Result := 'Datum_OSGB_1970_SN';
               6279 : Result := 'Datum_OS_SN_1980';
               6280 : Result := 'Datum_Padang_1884';
               6281 : Result := 'Datum_Palestine_1923';
               6282 : Result := 'Datum_Pointe_Noire';
               6283 : Result := 'Datum_Geocentric_Datum_of_Australia_1994';
               6284 : Result := 'Datum_Pulkovo_1942';
               6285 : Result := 'Datum_Qatar';
               6286 : Result := 'Datum_Qatar_1948';
               6287 : Result := 'Datum_Qornoq';
               6288 : Result := 'Datum_Loma_Quintana';
               6289 : Result := 'Datum_Amersfoort';
               6290 : Result := 'Datum_RT38';
               6291 : Result := 'Datum_South_American_Datum_1969';
               6292 : Result := 'Datum_Sapper_Hill_1943';
               6293 : Result := 'Datum_Schwarzeck';
               6294 : Result := 'Datum_Segora';
               6295 : Result := 'Datum_Serindung';
               6296 : Result := 'Datum_Sudan';
               6297 : Result := 'Datum_Tananarive_1925';
               6298 : Result := 'Datum_Timbalai_1948';
               6299 : Result := 'Datum_TM65';
               6300 : Result := 'Datum_TM75';
               6301 : Result := 'Datum_Tokyo';
               6302 : Result := 'Datum_Trinidad_1903';
               6303 : Result := 'Datum_Trucial_Coast_1948';
               6304 : Result := 'Datum_Voirol_1875';
               6305 : Result := 'Datum_Voirol_Unifie_1960';
               6306 : Result := 'Datum_Bern_1938';
               6307 : Result := 'Datum_Nord_Sahara_1959';
               6308 : Result := 'Datum_Stockholm_1938';
               6309 : Result := 'Datum_Yacare';
               6310 : Result := 'Datum_Yoff';
               6311 : Result := 'Datum_Zanderij';
               6312 : Result := 'Datum_Militar_Geographische_Institut';
               6313 : Result := 'Datum_Reseau_National_Belge_1972';
               6314 : Result := 'Datum_Deutsche_Hauptdreiecksnetz';
               6315 : Result := 'Datum_Conakry_1905';
               6322 : Result := 'Datum_WGS72';
               6324 : Result := 'Datum_WGS72_Transit_Broadcast_Ephemeris';
               6326 : Result := 'Datum_WGS84';
               6901 : Result := 'Datum_Ancienne_Triangulation_Francaise';
               6902 : Result := 'Datum_Nord_de_Guerre';
             end;
      2056 : case iVal of      { Ellipsoid codes }
               7001 : Result := 'Ellipse_Airy_1830';
               7002 : Result := 'Ellipse_Airy_Modified_1849';
               7003 : Result := 'Ellipse_Australian_National_Spheroid';
               7004 : Result := 'Ellipse_Bessel_1841';
               7005 : Result := 'Ellipse_Bessel_Modified';
               7006 : Result := 'Ellipse_Bessel_Namibia';
               7007 : Result := 'Ellipse_Clarke_1858';
               7008 : Result := 'Ellipse_Clarke_1866';
               7009 : Result := 'Ellipse_Clarke_1866_Michigan';
               7010 : Result := 'Ellipse_Clarke_1880_Benoit';
               7011 : Result := 'Ellipse_Clarke_1880_IGN';
               7012 : Result := 'Ellipse_Clarke_1880_RGS';
               7013 : Result := 'Ellipse_Clarke_1880_Arc';
               7014 : Result := 'Ellipse_Clarke_1880_SGA_1922';
               7015 : Result := 'Ellipse_Everest_1830_1937_Adjustment';
               7016 : Result := 'Ellipse_Everest_1830_1967_Definition';
               7017 : Result := 'Ellipse_Everest_1830_1975_Definition';
               7018 : Result := 'Ellipse_Everest_1830_Modified';
               7019 : Result := 'Ellipse_GRS_1980';
               7020 : Result := 'Ellipse_Helmert_1906';
               7021 : Result := 'Ellipse_Indonesian_National_Spheroid';
               7022 : Result := 'Ellipse_International_1924';
               7023 : Result := 'Ellipse_International_1967';
               7024 : Result := 'Ellipse_Krassowsky_1940';
               7025 : Result := 'Ellipse_NWL_9D';
               7026 : Result := 'Ellipse_NWL_10D';
               7027 : Result := 'Ellipse_Plessis_1817';
               7028 : Result := 'Ellipse_Struve_1860';
               7029 : Result := 'Ellipse_War_Office';
               7030 : Result := 'Ellipse_WGS_84';
               7031 : Result := 'Ellipse_GEM_10C';
               7032 : Result := 'Ellipse_OSU86F';
               7033 : Result := 'Ellipse_OSU91A';
               7034 : Result := 'Ellipse_Clarke_1880';
               7035 : Result := 'Ellipse_Sphere';
             end;
      2051 : case iVal of    { Prime Meridian codes }
               8901 : Result := 'PM_Greenwich';
               8902 : Result := 'PM_Lisbon';
               8903 : Result := 'PM_Paris';
               8904 : Result := 'PM_Bogota';
               8905 : Result := 'PM_Madrid';
               8906 : Result := 'PM_Rome';
               8907 : Result := 'PM_Bern';
               8908 : Result := 'PM_Jakarta';
               8909 : Result := 'PM_Ferro';
               8910 : Result := 'PM_Brussels';
               8911 : Result := 'PM_Stockholm';
             end;
      3072 : case iVal of     { Projected CS Type Codes }
               20137 : Result := 'PCS_Adindan_UTM_zone_37N';
               20138 : Result := 'PCS_Adindan_UTM_zone_38N';
               20248 : Result := 'PCS_AGD66_AMG_zone_48';
               20249 : Result := 'PCS_AGD66_AMG_zone_49';
               20250 : Result := 'PCS_AGD66_AMG_zone_50';
               20251 : Result := 'PCS_AGD66_AMG_zone_51';
               20252 : Result := 'PCS_AGD66_AMG_zone_52';
               20253 : Result := 'PCS_AGD66_AMG_zone_53';
               20254 : Result := 'PCS_AGD66_AMG_zone_54';
               20255 : Result := 'PCS_AGD66_AMG_zone_55';
               20256 : Result := 'PCS_AGD66_AMG_zone_56';
               20257 : Result := 'PCS_AGD66_AMG_zone_57';
               20258 : Result := 'PCS_AGD66_AMG_zone_58';
               20348 : Result := 'PCS_AGD84_AMG_zone_48';
               20349 : Result := 'PCS_AGD84_AMG_zone_49';
               20350 : Result := 'PCS_AGD84_AMG_zone_50';
               20351 : Result := 'PCS_AGD84_AMG_zone_51';
               20352 : Result := 'PCS_AGD84_AMG_zone_52';
               20353 : Result := 'PCS_AGD84_AMG_zone_53';
               20354 : Result := 'PCS_AGD84_AMG_zone_54';
               20355 : Result := 'PCS_AGD84_AMG_zone_55';
               20356 : Result := 'PCS_AGD84_AMG_zone_56';
               20357 : Result := 'PCS_AGD84_AMG_zone_57';
               20358 : Result := 'PCS_AGD84_AMG_zone_58';
               20437 : Result := 'PCS_Ain_el_Abd_UTM_zone_37N';
               20438 : Result := 'PCS_Ain_el_Abd_UTM_zone_38N';
               20439 : Result := 'PCS_Ain_el_Abd_UTM_zone_39N';
               20499 : Result := 'PCS_Ain_el_Abd_Bahrain_Grid';
               20538 : Result := 'PCS_Afgooye_UTM_zone_38N';
               20539 : Result := 'PCS_Afgooye_UTM_zone_39N';
               20700 : Result := 'PCS_Lisbon_Portugese_Grid';
               20822 : Result := 'PCS_Aratu_UTM_zone_22S';
               20823 : Result := 'PCS_Aratu_UTM_zone_23S';
               20824 : Result := 'PCS_Aratu_UTM_zone_24S';
               20973 : Result := 'PCS_Arc_1950_Lo13';
               20975 : Result := 'PCS_Arc_1950_Lo15';
               20977 : Result := 'PCS_Arc_1950_Lo17';
               20979 : Result := 'PCS_Arc_1950_Lo19';
               20981 : Result := 'PCS_Arc_1950_Lo21';
               20983 : Result := 'PCS_Arc_1950_Lo23';
               20985 : Result := 'PCS_Arc_1950_Lo25';
               20987 : Result := 'PCS_Arc_1950_Lo27';
               20989 : Result := 'PCS_Arc_1950_Lo29';
               20991 : Result := 'PCS_Arc_1950_Lo31';
               20993 : Result := 'PCS_Arc_1950_Lo33';
               20995 : Result := 'PCS_Arc_1950_Lo35';
               21100 : Result := 'PCS_Batavia_NEIEZ';
               21148 : Result := 'PCS_Batavia_UTM_zone_48S';
               21149 : Result := 'PCS_Batavia_UTM_zone_49S';
               21150 : Result := 'PCS_Batavia_UTM_zone_50S';
               21413 : Result := 'PCS_Beijing_Gauss_zone_13';
               21414 : Result := 'PCS_Beijing_Gauss_zone_14';
               21415 : Result := 'PCS_Beijing_Gauss_zone_15';
               21416 : Result := 'PCS_Beijing_Gauss_zone_16';
               21417 : Result := 'PCS_Beijing_Gauss_zone_17';
               21418 : Result := 'PCS_Beijing_Gauss_zone_18';
               21419 : Result := 'PCS_Beijing_Gauss_zone_19';
               21420 : Result := 'PCS_Beijing_Gauss_zone_20';
               21421 : Result := 'PCS_Beijing_Gauss_zone_21';
               21422 : Result := 'PCS_Beijing_Gauss_zone_22';
               21423 : Result := 'PCS_Beijing_Gauss_zone_23';
               21473 : Result := 'PCS_Beijing_Gauss_13N';
               21474 : Result := 'PCS_Beijing_Gauss_14N';
               21475 : Result := 'PCS_Beijing_Gauss_15N';
               21476 : Result := 'PCS_Beijing_Gauss_16N';
               21477 : Result := 'PCS_Beijing_Gauss_17N';
               21478 : Result := 'PCS_Beijing_Gauss_18N';
               21479 : Result := 'PCS_Beijing_Gauss_19N';
               21480 : Result := 'PCS_Beijing_Gauss_20N';
               21481 : Result := 'PCS_Beijing_Gauss_21N';
               21482 : Result := 'PCS_Beijing_Gauss_22N';
               21483 : Result := 'PCS_Beijing_Gauss_23N';
               21500 : Result := 'PCS_Belge_Lambert_50';
               21790 : Result := 'PCS_Bern_1898_Swiss_Old';
               21817 : Result := 'PCS_Bogota_UTM_zone_17N';
               21818 : Result := 'PCS_Bogota_UTM_zone_18N';
               21891 : Result := 'PCS_Bogota_Colombia_3W';
               21892 : Result := 'PCS_Bogota_Colombia_Bogota';
               21893 : Result := 'PCS_Bogota_Colombia_3E';
               21894 : Result := 'PCS_Bogota_Colombia_6E';
               22032 : Result := 'PCS_Camacupa_UTM_32S';
               22033 : Result := 'PCS_Camacupa_UTM_33S';
               22191 : Result := 'PCS_C_Inchauspe_Argentina_1';
               22192 : Result := 'PCS_C_Inchauspe_Argentina_2';
               22193 : Result := 'PCS_C_Inchauspe_Argentina_3';
               22194 : Result := 'PCS_C_Inchauspe_Argentina_4';
               22195 : Result := 'PCS_C_Inchauspe_Argentina_5';
               22196 : Result := 'PCS_C_Inchauspe_Argentina_6';
               22197 : Result := 'PCS_C_Inchauspe_Argentina_7';
               22332 : Result := 'PCS_Carthage_UTM_zone_32N';
               22391 : Result := 'PCS_Carthage_Nord_Tunisie';
               22392 : Result := 'PCS_Carthage_Sud_Tunisie';
               22523 : Result := 'PCS_Corrego_Alegre_UTM_23S';
               22524 : Result := 'PCS_Corrego_Alegre_UTM_24S';
               22832 : Result := 'PCS_Douala_UTM_zone_32N';
               22992 : Result := 'PCS_Egypt_1907_Red_Belt';
               22993 : Result := 'PCS_Egypt_1907_Purple_Belt';
               22994 : Result := 'PCS_Egypt_1907_Ext_Purple';
               23028 : Result := 'PCS_ED50_UTM_zone_28N';
               23029 : Result := 'PCS_ED50_UTM_zone_29N';
               23030 : Result := 'PCS_ED50_UTM_zone_30N';
               23031 : Result := 'PCS_ED50_UTM_zone_31N';
               23032 : Result := 'PCS_ED50_UTM_zone_32N';
               23033 : Result := 'PCS_ED50_UTM_zone_33N';
               23034 : Result := 'PCS_ED50_UTM_zone_34N';
               23035 : Result := 'PCS_ED50_UTM_zone_35N';
               23036 : Result := 'PCS_ED50_UTM_zone_36N';
               23037 : Result := 'PCS_ED50_UTM_zone_37N';
               23038 : Result := 'PCS_ED50_UTM_zone_38N';
               23239 : Result := 'PCS_Fahud_UTM_zone_39N';
               23240 : Result := 'PCS_Fahud_UTM_zone_40N';
               23433 : Result := 'PCS_Garoua_UTM_zone_33N';
               23846 : Result := 'PCS_ID74_UTM_zone_46N';
               23847 : Result := 'PCS_ID74_UTM_zone_47N';
               23848 : Result := 'PCS_ID74_UTM_zone_48N';
               23849 : Result := 'PCS_ID74_UTM_zone_49N';
               23850 : Result := 'PCS_ID74_UTM_zone_50N';
               23851 : Result := 'PCS_ID74_UTM_zone_51N';
               23852 : Result := 'PCS_ID74_UTM_zone_52N';
               23853 : Result := 'PCS_ID74_UTM_zone_53N';
               23886 : Result := 'PCS_ID74_UTM_zone_46S';
               23887 : Result := 'PCS_ID74_UTM_zone_47S';
               23888 : Result := 'PCS_ID74_UTM_zone_48S';
               23889 : Result := 'PCS_ID74_UTM_zone_49S';
               23890 : Result := 'PCS_ID74_UTM_zone_50S';
               23891 : Result := 'PCS_ID74_UTM_zone_51S';
               23892 : Result := 'PCS_ID74_UTM_zone_52S';
               23893 : Result := 'PCS_ID74_UTM_zone_53S';
               23894 : Result := 'PCS_ID74_UTM_zone_54S';
               23947 : Result := 'PCS_Indian_1954_UTM_47N';
               23948 : Result := 'PCS_Indian_1954_UTM_48N';
               24047 : Result := 'PCS_Indian_1975_UTM_47N';
               24048 : Result := 'PCS_Indian_1975_UTM_48N';
               24100 : Result := 'PCS_Jamaica_1875_Old_Grid';
               24200 : Result := 'PCS_JAD69_Jamaica_Grid';
               24370 : Result := 'PCS_Kalianpur_India_0';
               24371 : Result := 'PCS_Kalianpur_India_I';
               24372 : Result := 'PCS_Kalianpur_India_IIa';
               24373 : Result := 'PCS_Kalianpur_India_IIIa';
               24374 : Result := 'PCS_Kalianpur_India_IVa';
               24382 : Result := 'PCS_Kalianpur_India_IIb';
               24383 : Result := 'PCS_Kalianpur_India_IIIb';
               24384 : Result := 'PCS_Kalianpur_India_IVb';
               24500 : Result := 'PCS_Kertau_Singapore_Grid';
               24547 : Result := 'PCS_Kertau_UTM_zone_47N';
               24548 : Result := 'PCS_Kertau_UTM_zone_48N';
               24720 : Result := 'PCS_La_Canoa_UTM_zone_20N';
               24721 : Result := 'PCS_La_Canoa_UTM_zone_21N';
               24818 : Result := 'PCS_PSAD56_UTM_zone_18N';
               24819 : Result := 'PCS_PSAD56_UTM_zone_19N';
               24820 : Result := 'PCS_PSAD56_UTM_zone_20N';
               24821 : Result := 'PCS_PSAD56_UTM_zone_21N';
               24877 : Result := 'PCS_PSAD56_UTM_zone_17S';
               24878 : Result := 'PCS_PSAD56_UTM_zone_18S';
               24879 : Result := 'PCS_PSAD56_UTM_zone_19S';
               24880 : Result := 'PCS_PSAD56_UTM_zone_20S';
               24891 : Result := 'PCS_PSAD56_Peru_west_zone';
               24892 : Result := 'PCS_PSAD56_Peru_central';
               24893 : Result := 'PCS_PSAD56_Peru_east_zone';
               25000 : Result := 'PCS_Leigon_Ghana_Grid';
               25231 : Result := 'PCS_Lome_UTM_zone_31N';
               25391 : Result := 'PCS_Luzon_Philippines_I';
               25392 : Result := 'PCS_Luzon_Philippines_II';
               25393 : Result := 'PCS_Luzon_Philippines_III';
               25394 : Result := 'PCS_Luzon_Philippines_IV';
               25395 : Result := 'PCS_Luzon_Philippines_V';
               25700 : Result := 'PCS_Makassar_NEIEZ';
               25932 : Result := 'PCS_Malongo_1987_UTM_32S';
               26191 : Result := 'PCS_Merchich_Nord_Maroc';
               26192 : Result := 'PCS_Merchich_Sud_Maroc';
               26193 : Result := 'PCS_Merchich_Sahara';
               26237 : Result := 'PCS_Massawa_UTM_zone_37N';
               26331 : Result := 'PCS_Minna_UTM_zone_31N';
               26332 : Result := 'PCS_Minna_UTM_zone_32N';
               26391 : Result := 'PCS_Minna_Nigeria_West';
               26392 : Result := 'PCS_Minna_Nigeria_Mid_Belt';
               26393 : Result := 'PCS_Minna_Nigeria_East';
               26432 : Result := 'PCS_Mhast_UTM_zone_32S';
               26591 : Result := 'PCS_Monte_Mario_Italy_1';
               26592 : Result := 'PCS_Monte_Mario_Italy_2';
               26632 : Result := 'PCS_M_poraloko_UTM_32N';
               26692 : Result := 'PCS_M_poraloko_UTM_32S';
               26703 : Result := 'PCS_NAD27_UTM_zone_3N';
               26704 : Result := 'PCS_NAD27_UTM_zone_4N';
               26705 : Result := 'PCS_NAD27_UTM_zone_5N';
               26706 : Result := 'PCS_NAD27_UTM_zone_6N';
               26707 : Result := 'PCS_NAD27_UTM_zone_7N';
               26708 : Result := 'PCS_NAD27_UTM_zone_8N';
               26709 : Result := 'PCS_NAD27_UTM_zone_9N';
               26710 : Result := 'PCS_NAD27_UTM_zone_10N';
               26711 : Result := 'PCS_NAD27_UTM_zone_11N';
               26712 : Result := 'PCS_NAD27_UTM_zone_12N';
               26713 : Result := 'PCS_NAD27_UTM_zone_13N';
               26714 : Result := 'PCS_NAD27_UTM_zone_14N';
               26715 : Result := 'PCS_NAD27_UTM_zone_15N';
               26716 : Result := 'PCS_NAD27_UTM_zone_16N';
               26717 : Result := 'PCS_NAD27_UTM_zone_17N';
               26718 : Result := 'PCS_NAD27_UTM_zone_18N';
               26719 : Result := 'PCS_NAD27_UTM_zone_19N';
               26720 : Result := 'PCS_NAD27_UTM_zone_20N';
               26721 : Result := 'PCS_NAD27_UTM_zone_21N';
               26722 : Result := 'PCS_NAD27_UTM_zone_22N';
               26729 : Result := 'PCS_NAD27_Alabama_East';
               26730 : Result := 'PCS_NAD27_Alabama_West';
               26731 : Result := 'PCS_NAD27_Alaska_zone_1';
               26732 : Result := 'PCS_NAD27_Alaska_zone_2';
               26733 : Result := 'PCS_NAD27_Alaska_zone_3';
               26734 : Result := 'PCS_NAD27_Alaska_zone_4';
               26735 : Result := 'PCS_NAD27_Alaska_zone_5';
               26736 : Result := 'PCS_NAD27_Alaska_zone_6';
               26737 : Result := 'PCS_NAD27_Alaska_zone_7';
               26738 : Result := 'PCS_NAD27_Alaska_zone_8';
               26739 : Result := 'PCS_NAD27_Alaska_zone_9';
               26740 : Result := 'PCS_NAD27_Alaska_zone_10';
               26741 : Result := 'PCS_NAD27_California_I';
               26742 : Result := 'PCS_NAD27_California_II';
               26743 : Result := 'PCS_NAD27_California_III';
               26744 : Result := 'PCS_NAD27_California_IV';
               26745 : Result := 'PCS_NAD27_California_V';
               26746 : Result := 'PCS_NAD27_California_VI';
               26747 : Result := 'PCS_NAD27_California_VII';
               26748 : Result := 'PCS_NAD27_Arizona_East';
               26749 : Result := 'PCS_NAD27_Arizona_Central';
               26750 : Result := 'PCS_NAD27_Arizona_West';
               26751 : Result := 'PCS_NAD27_Arkansas_North';
               26752 : Result := 'PCS_NAD27_Arkansas_South';
               26753 : Result := 'PCS_NAD27_Colorado_North';
               26754 : Result := 'PCS_NAD27_Colorado_Central';
               26755 : Result := 'PCS_NAD27_Colorado_South';
               26756 : Result := 'PCS_NAD27_Connecticut';
               26757 : Result := 'PCS_NAD27_Delaware';
               26758 : Result := 'PCS_NAD27_Florida_East';
               26759 : Result := 'PCS_NAD27_Florida_West';
               26760 : Result := 'PCS_NAD27_Florida_North';
               26761 : Result := 'PCS_NAD27_Hawaii_zone_1';
               26762 : Result := 'PCS_NAD27_Hawaii_zone_2';
               26763 : Result := 'PCS_NAD27_Hawaii_zone_3';
               26764 : Result := 'PCS_NAD27_Hawaii_zone_4';
               26765 : Result := 'PCS_NAD27_Hawaii_zone_5';
               26766 : Result := 'PCS_NAD27_Georgia_East';
               26767 : Result := 'PCS_NAD27_Georgia_West';
               26768 : Result := 'PCS_NAD27_Idaho_East';
               26769 : Result := 'PCS_NAD27_Idaho_Central';
               26770 : Result := 'PCS_NAD27_Idaho_West';
               26771 : Result := 'PCS_NAD27_Illinois_East';
               26772 : Result := 'PCS_NAD27_Illinois_West';
               26773 : Result := 'PCS_NAD27_Indiana_East';
//               26774 : Result := 'PCS_NAD27_BLM_14N_feet';
               26774 : Result := 'PCS_NAD27_Indiana_West';
//               26775 : Result := 'PCS_NAD27_BLM_15N_feet';
               26775 : Result := 'PCS_NAD27_Iowa_North';
//               26776 : Result := 'PCS_NAD27_BLM_16N_feet';
               26776 : Result := 'PCS_NAD27_Iowa_South';
//               26777 : Result := 'PCS_NAD27_BLM_17N_feet';
               26777 : Result := 'PCS_NAD27_Kansas_North';
               26778 : Result := 'PCS_NAD27_Kansas_South';
               26779 : Result := 'PCS_NAD27_Kentucky_North';
               26780 : Result := 'PCS_NAD27_Kentucky_South';
               26781 : Result := 'PCS_NAD27_Louisiana_North';
               26782 : Result := 'PCS_NAD27_Louisiana_South';
               26783 : Result := 'PCS_NAD27_Maine_East';
               26784 : Result := 'PCS_NAD27_Maine_West';
               26785 : Result := 'PCS_NAD27_Maryland';
               26786 : Result := 'PCS_NAD27_Massachusetts';
               26787 : Result := 'PCS_NAD27_Massachusetts_Is';
               26788 : Result := 'PCS_NAD27_Michigan_North';
               26789 : Result := 'PCS_NAD27_Michigan_Central';
               26790 : Result := 'PCS_NAD27_Michigan_South';
               26791 : Result := 'PCS_NAD27_Minnesota_North';
               26792 : Result := 'PCS_NAD27_Minnesota_Cent';
               26793 : Result := 'PCS_NAD27_Minnesota_South';
               26794 : Result := 'PCS_NAD27_Mississippi_East';
               26795 : Result := 'PCS_NAD27_Mississippi_West';
               26796 : Result := 'PCS_NAD27_Missouri_East';
               26797 : Result := 'PCS_NAD27_Missouri_Central';
               26798 : Result := 'PCS_NAD27_Missouri_West';
               26801 : Result := 'PCS_NAD_Michigan_Michigan_East';
               26802 : Result := 'PCS_NAD_Michigan_Michigan_Old_Central';
               26803 : Result := 'PCS_NAD_Michigan_Michigan_West';
               26903 : Result := 'PCS_NAD83_UTM_zone_3N';
               26904 : Result := 'PCS_NAD83_UTM_zone_4N';
               26905 : Result := 'PCS_NAD83_UTM_zone_5N';
               26906 : Result := 'PCS_NAD83_UTM_zone_6N';
               26907 : Result := 'PCS_NAD83_UTM_zone_7N';
               26908 : Result := 'PCS_NAD83_UTM_zone_8N';
               26909 : Result := 'PCS_NAD83_UTM_zone_9N';
               26910 : Result := 'PCS_NAD83_UTM_zone_10N';
               26911 : Result := 'PCS_NAD83_UTM_zone_11N';
               26912 : Result := 'PCS_NAD83_UTM_zone_12N';
               26913 : Result := 'PCS_NAD83_UTM_zone_13N';
               26914 : Result := 'PCS_NAD83_UTM_zone_14N';
               26915 : Result := 'PCS_NAD83_UTM_zone_15N';
               26916 : Result := 'PCS_NAD83_UTM_zone_16N';
               26917 : Result := 'PCS_NAD83_UTM_zone_17N';
               26918 : Result := 'PCS_NAD83_UTM_zone_18N';
               26919 : Result := 'PCS_NAD83_UTM_zone_19N';
               26920 : Result := 'PCS_NAD83_UTM_zone_20N';
               26921 : Result := 'PCS_NAD83_UTM_zone_21N';
               26922 : Result := 'PCS_NAD83_UTM_zone_22N';
               26923 : Result := 'PCS_NAD83_UTM_zone_23N';
               26929 : Result := 'PCS_NAD83_Alabama_East';
               26930 : Result := 'PCS_NAD83_Alabama_West';
               26931 : Result := 'PCS_NAD83_Alaska_zone_1';
               26932 : Result := 'PCS_NAD83_Alaska_zone_2';
               26933 : Result := 'PCS_NAD83_Alaska_zone_3';
               26934 : Result := 'PCS_NAD83_Alaska_zone_4';
               26935 : Result := 'PCS_NAD83_Alaska_zone_5';
               26936 : Result := 'PCS_NAD83_Alaska_zone_6';
               26937 : Result := 'PCS_NAD83_Alaska_zone_7';
               26938 : Result := 'PCS_NAD83_Alaska_zone_8';
               26939 : Result := 'PCS_NAD83_Alaska_zone_9';
               26940 : Result := 'PCS_NAD83_Alaska_zone_10';
               26941 : Result := 'PCS_NAD83_California_1';
               26942 : Result := 'PCS_NAD83_California_2';
               26943 : Result := 'PCS_NAD83_California_3';
               26944 : Result := 'PCS_NAD83_California_4';
               26945 : Result := 'PCS_NAD83_California_5';
               26946 : Result := 'PCS_NAD83_California_6';
               26948 : Result := 'PCS_NAD83_Arizona_East';
               26949 : Result := 'PCS_NAD83_Arizona_Central';
               26950 : Result := 'PCS_NAD83_Arizona_West';
               26951 : Result := 'PCS_NAD83_Arkansas_North';
               26952 : Result := 'PCS_NAD83_Arkansas_South';
               26953 : Result := 'PCS_NAD83_Colorado_North';
               26954 : Result := 'PCS_NAD83_Colorado_Central';
               26955 : Result := 'PCS_NAD83_Colorado_South';
               26956 : Result := 'PCS_NAD83_Connecticut';
               26957 : Result := 'PCS_NAD83_Delaware';
               26958 : Result := 'PCS_NAD83_Florida_East';
               26959 : Result := 'PCS_NAD83_Florida_West';
               26960 : Result := 'PCS_NAD83_Florida_North';
               26961 : Result := 'PCS_NAD83_Hawaii_zone_1';
               26962 : Result := 'PCS_NAD83_Hawaii_zone_2';
               26963 : Result := 'PCS_NAD83_Hawaii_zone_3';
               26964 : Result := 'PCS_NAD83_Hawaii_zone_4';
               26965 : Result := 'PCS_NAD83_Hawaii_zone_5';
               26966 : Result := 'PCS_NAD83_Georgia_East';
               26967 : Result := 'PCS_NAD83_Georgia_West';
               26968 : Result := 'PCS_NAD83_Idaho_East';
               26969 : Result := 'PCS_NAD83_Idaho_Central';
               26970 : Result := 'PCS_NAD83_Idaho_West';
               26971 : Result := 'PCS_NAD83_Illinois_East';
               26972 : Result := 'PCS_NAD83_Illinois_West';
               26973 : Result := 'PCS_NAD83_Indiana_East';
               26974 : Result := 'PCS_NAD83_Indiana_West';
               26975 : Result := 'PCS_NAD83_Iowa_North';
               26976 : Result := 'PCS_NAD83_Iowa_South';
               26977 : Result := 'PCS_NAD83_Kansas_North';
               26978 : Result := 'PCS_NAD83_Kansas_South';
               26979 : Result := 'PCS_NAD83_Kentucky_North';
               26980 : Result := 'PCS_NAD83_Kentucky_South';
               26981 : Result := 'PCS_NAD83_Louisiana_North';
               26982 : Result := 'PCS_NAD83_Louisiana_South';
               26983 : Result := 'PCS_NAD83_Maine_East';
               26984 : Result := 'PCS_NAD83_Maine_West';
               26985 : Result := 'PCS_NAD83_Maryland';
               26986 : Result := 'PCS_NAD83_Massachusetts';
               26987 : Result := 'PCS_NAD83_Massachusetts_Is';
               26988 : Result := 'PCS_NAD83_Michigan_North';
               26989 : Result := 'PCS_NAD83_Michigan_Central';
               26990 : Result := 'PCS_NAD83_Michigan_South';
               26991 : Result := 'PCS_NAD83_Minnesota_North';
               26992 : Result := 'PCS_NAD83_Minnesota_Cent';
               26993 : Result := 'PCS_NAD83_Minnesota_South';
               26994 : Result := 'PCS_NAD83_Mississippi_East';
               26995 : Result := 'PCS_NAD83_Mississippi_West';
               26996 : Result := 'PCS_NAD83_Missouri_East';
               26997 : Result := 'PCS_NAD83_Missouri_Central';
               26998 : Result := 'PCS_NAD83_Missouri_West';
               27038 : Result := 'PCS_Nahrwan_1967_UTM_38N';
               27039 : Result := 'PCS_Nahrwan_1967_UTM_39N';
               27040 : Result := 'PCS_Nahrwan_1967_UTM_40N';
               27120 : Result := 'PCS_Naparima_UTM_20N';
               27200 : Result := 'PCS_GD49_NZ_Map_Grid';
               27291 : Result := 'PCS_GD49_North_Island_Grid';
               27292 : Result := 'PCS_GD49_South_Island_Grid';
               27429 : Result := 'PCS_Datum_73_UTM_zone_29N';
               27500 : Result := 'PCS_ATF_Nord_de_Guerre';
               27581 : Result := 'PCS_NTF_France_I';
               27582 : Result := 'PCS_NTF_France_II';
               27583 : Result := 'PCS_NTF_France_III';
               27591 : Result := 'PCS_NTF_Nord_France';
               27592 : Result := 'PCS_NTF_Centre_France';
               27593 : Result := 'PCS_NTF_Sud_France';
               27700 : Result := 'PCS_British_National_Grid';
               28232 : Result := 'PCS_Point_Noire_UTM_32S';
               28348 : Result := 'PCS_GDA94_MGA_zone_48';
               28349 : Result := 'PCS_GDA94_MGA_zone_49';
               28350 : Result := 'PCS_GDA94_MGA_zone_50';
               28351 : Result := 'PCS_GDA94_MGA_zone_51';
               28352 : Result := 'PCS_GDA94_MGA_zone_52';
               28353 : Result := 'PCS_GDA94_MGA_zone_53';
               28354 : Result := 'PCS_GDA94_MGA_zone_54';
               28355 : Result := 'PCS_GDA94_MGA_zone_55';
               28356 : Result := 'PCS_GDA94_MGA_zone_56';
               28357 : Result := 'PCS_GDA94_MGA_zone_57';
               28358 : Result := 'PCS_GDA94_MGA_zone_58';
               28404 : Result := 'PCS_Pulkovo_Gauss_zone_4';
               28405 : Result := 'PCS_Pulkovo_Gauss_zone_5';
               28406 : Result := 'PCS_Pulkovo_Gauss_zone_6';
               28407 : Result := 'PCS_Pulkovo_Gauss_zone_7';
               28408 : Result := 'PCS_Pulkovo_Gauss_zone_8';
               28409 : Result := 'PCS_Pulkovo_Gauss_zone_9';
               28410 : Result := 'PCS_Pulkovo_Gauss_zone_10';
               28411 : Result := 'PCS_Pulkovo_Gauss_zone_11';
               28412 : Result := 'PCS_Pulkovo_Gauss_zone_12';
               28413 : Result := 'PCS_Pulkovo_Gauss_zone_13';
               28414 : Result := 'PCS_Pulkovo_Gauss_zone_14';
               28415 : Result := 'PCS_Pulkovo_Gauss_zone_15';
               28416 : Result := 'PCS_Pulkovo_Gauss_zone_16';
               28417 : Result := 'PCS_Pulkovo_Gauss_zone_17';
               28418 : Result := 'PCS_Pulkovo_Gauss_zone_18';
               28419 : Result := 'PCS_Pulkovo_Gauss_zone_19';
               28420 : Result := 'PCS_Pulkovo_Gauss_zone_20';
               28421 : Result := 'PCS_Pulkovo_Gauss_zone_21';
               28422 : Result := 'PCS_Pulkovo_Gauss_zone_22';
               28423 : Result := 'PCS_Pulkovo_Gauss_zone_23';
               28424 : Result := 'PCS_Pulkovo_Gauss_zone_24';
               28425 : Result := 'PCS_Pulkovo_Gauss_zone_25';
               28426 : Result := 'PCS_Pulkovo_Gauss_zone_26';
               28427 : Result := 'PCS_Pulkovo_Gauss_zone_27';
               28428 : Result := 'PCS_Pulkovo_Gauss_zone_28';
               28429 : Result := 'PCS_Pulkovo_Gauss_zone_29';
               28430 : Result := 'PCS_Pulkovo_Gauss_zone_30';
               28431 : Result := 'PCS_Pulkovo_Gauss_zone_31';
               28432 : Result := 'PCS_Pulkovo_Gauss_zone_32';
               28464 : Result := 'PCS_Pulkovo_Gauss_4N';
               28465 : Result := 'PCS_Pulkovo_Gauss_5N';
               28466 : Result := 'PCS_Pulkovo_Gauss_6N';
               28467 : Result := 'PCS_Pulkovo_Gauss_7N';
               28468 : Result := 'PCS_Pulkovo_Gauss_8N';
               28469 : Result := 'PCS_Pulkovo_Gauss_9N';
               28470 : Result := 'PCS_Pulkovo_Gauss_10N';
               28471 : Result := 'PCS_Pulkovo_Gauss_11N';
               28472 : Result := 'PCS_Pulkovo_Gauss_12N';
               28473 : Result := 'PCS_Pulkovo_Gauss_13N';
               28474 : Result := 'PCS_Pulkovo_Gauss_14N';
               28475 : Result := 'PCS_Pulkovo_Gauss_15N';
               28476 : Result := 'PCS_Pulkovo_Gauss_16N';
               28477 : Result := 'PCS_Pulkovo_Gauss_17N';
               28478 : Result := 'PCS_Pulkovo_Gauss_18N';
               28479 : Result := 'PCS_Pulkovo_Gauss_19N';
               28480 : Result := 'PCS_Pulkovo_Gauss_20N';
               28481 : Result := 'PCS_Pulkovo_Gauss_21N';
               28482 : Result := 'PCS_Pulkovo_Gauss_22N';
               28483 : Result := 'PCS_Pulkovo_Gauss_23N';
               28484 : Result := 'PCS_Pulkovo_Gauss_24N';
               28485 : Result := 'PCS_Pulkovo_Gauss_25N';
               28486 : Result := 'PCS_Pulkovo_Gauss_26N';
               28487 : Result := 'PCS_Pulkovo_Gauss_27N';
               28488 : Result := 'PCS_Pulkovo_Gauss_28N';
               28489 : Result := 'PCS_Pulkovo_Gauss_29N';
               28490 : Result := 'PCS_Pulkovo_Gauss_30N';
               28491 : Result := 'PCS_Pulkovo_Gauss_31N';
               28492 : Result := 'PCS_Pulkovo_Gauss_32N';
               28600 : Result := 'PCS_Qatar_National_Grid';
               28991 : Result := 'PCS_RD_Netherlands_Old';
               28992 : Result := 'PCS_RD_Netherlands_New';
               29118 : Result := 'PCS_SAD69_UTM_zone_18N';
               29119 : Result := 'PCS_SAD69_UTM_zone_19N';
               29120 : Result := 'PCS_SAD69_UTM_zone_20N';
               29121 : Result := 'PCS_SAD69_UTM_zone_21N';
               29122 : Result := 'PCS_SAD69_UTM_zone_22N';
               29177 : Result := 'PCS_SAD69_UTM_zone_17S';
               29178 : Result := 'PCS_SAD69_UTM_zone_18S';
               29179 : Result := 'PCS_SAD69_UTM_zone_19S';
               29180 : Result := 'PCS_SAD69_UTM_zone_20S';
               29181 : Result := 'PCS_SAD69_UTM_zone_21S';
               29182 : Result := 'PCS_SAD69_UTM_zone_22S';
               29183 : Result := 'PCS_SAD69_UTM_zone_23S';
               29184 : Result := 'PCS_SAD69_UTM_zone_24S';
               29185 : Result := 'PCS_SAD69_UTM_zone_25S';
               29220 : Result := 'PCS_Sapper_Hill_UTM_20S';
               29221 : Result := 'PCS_Sapper_Hill_UTM_21S';
               29333 : Result := 'PCS_Schwarzeck_UTM_33S';
               29635 : Result := 'PCS_Sudan_UTM_zone_35N';
               29636 : Result := 'PCS_Sudan_UTM_zone_36N';
               29700 : Result := 'PCS_Tananarive_Laborde';
               29738 : Result := 'PCS_Tananarive_UTM_38S';
               29739 : Result := 'PCS_Tananarive_UTM_39S';
               29800 : Result := 'PCS_Timbalai_1948_Borneo';
               29849 : Result := 'PCS_Timbalai_1948_UTM_49N';
               29850 : Result := 'PCS_Timbalai_1948_UTM_50N';
               29900 : Result := 'PCS_TM65_Irish_Nat_Grid';
               30200 : Result := 'PCS_Trinidad_1903_Trinidad';
               30339 : Result := 'PCS_TC_1948_UTM_zone_39N';
               30340 : Result := 'PCS_TC_1948_UTM_zone_40N';
               30491 : Result := 'PCS_Voirol_N_Algerie_ancien';
               30492 : Result := 'PCS_Voirol_S_Algerie_ancien';
               30591 : Result := 'PCS_Voirol_Unifie_N_Algerie';
               30592 : Result := 'PCS_Voirol_Unifie_S_Algerie';
               30600 : Result := 'PCS_Bern_1938_Swiss_New';
               30729 : Result := 'PCS_Nord_Sahara_UTM_29N';
               30730 : Result := 'PCS_Nord_Sahara_UTM_30N';
               30731 : Result := 'PCS_Nord_Sahara_UTM_31N';
               30732 : Result := 'PCS_Nord_Sahara_UTM_32N';
               31028 : Result := 'PCS_Yoff_UTM_zone_28N';
               31121 : Result := 'PCS_Zanderij_UTM_zone_21N';
               31291 : Result := 'PCS_MGI_Austria_West';
               31292 : Result := 'PCS_MGI_Austria_Central';
               31293 : Result := 'PCS_MGI_Austria_East';
               31300 : Result := 'PCS_Belge_Lambert_72';
               31491 : Result := 'PCS_DHDN_Germany_zone_1';
               31492 : Result := 'PCS_DHDN_Germany_zone_2';
               31493 : Result := 'PCS_DHDN_Germany_zone_3';
               31494 : Result := 'PCS_DHDN_Germany_zone_4';
               31495 : Result := 'PCS_DHDN_Germany_zone_5';
               32001 : Result := 'PCS_NAD27_Montana_North';
               32002 : Result := 'PCS_NAD27_Montana_Central';
               32003 : Result := 'PCS_NAD27_Montana_South';
               32005 : Result := 'PCS_NAD27_Nebraska_North';
               32006 : Result := 'PCS_NAD27_Nebraska_South';
               32007 : Result := 'PCS_NAD27_Nevada_East';
               32008 : Result := 'PCS_NAD27_Nevada_Central';
               32009 : Result := 'PCS_NAD27_Nevada_West';
               32010 : Result := 'PCS_NAD27_New_Hampshire';
               32011 : Result := 'PCS_NAD27_New_Jersey';
               32012 : Result := 'PCS_NAD27_New_Mexico_East';
               32013 : Result := 'PCS_NAD27_New_Mexico_Cent';
               32014 : Result := 'PCS_NAD27_New_Mexico_West';
               32015 : Result := 'PCS_NAD27_New_York_East';
               32016 : Result := 'PCS_NAD27_New_York_Central';
               32017 : Result := 'PCS_NAD27_New_York_West';
               32018 : Result := 'PCS_NAD27_New_York_Long_Is';
               32019 : Result := 'PCS_NAD27_North_Carolina';
               32020 : Result := 'PCS_NAD27_North_Dakota_N';
               32021 : Result := 'PCS_NAD27_North_Dakota_S';
               32022 : Result := 'PCS_NAD27_Ohio_North';
               32023 : Result := 'PCS_NAD27_Ohio_South';
               32024 : Result := 'PCS_NAD27_Oklahoma_North';
               32025 : Result := 'PCS_NAD27_Oklahoma_South';
               32026 : Result := 'PCS_NAD27_Oregon_North';
               32027 : Result := 'PCS_NAD27_Oregon_South';
               32028 : Result := 'PCS_NAD27_Pennsylvania_N';
               32029 : Result := 'PCS_NAD27_Pennsylvania_S';
               32030 : Result := 'PCS_NAD27_Rhode_Island';
               32031 : Result := 'PCS_NAD27_South_Carolina_N';
               32033 : Result := 'PCS_NAD27_South_Carolina_S';
               32034 : Result := 'PCS_NAD27_South_Dakota_N';
               32035 : Result := 'PCS_NAD27_South_Dakota_S';
               32036 : Result := 'PCS_NAD27_Tennessee';
               32037 : Result := 'PCS_NAD27_Texas_North';
               32038 : Result := 'PCS_NAD27_Texas_North_Cen';
               32039 : Result := 'PCS_NAD27_Texas_Central';
               32040 : Result := 'PCS_NAD27_Texas_South_Cen';
               32041 : Result := 'PCS_NAD27_Texas_South';
               32042 : Result := 'PCS_NAD27_Utah_North';
               32043 : Result := 'PCS_NAD27_Utah_Central';
               32044 : Result := 'PCS_NAD27_Utah_South';
               32045 : Result := 'PCS_NAD27_Vermont';
               32046 : Result := 'PCS_NAD27_Virginia_North';
               32047 : Result := 'PCS_NAD27_Virginia_South';
               32048 : Result := 'PCS_NAD27_Washington_North';
               32049 : Result := 'PCS_NAD27_Washington_South';
               32050 : Result := 'PCS_NAD27_West_Virginia_N';
               32051 : Result := 'PCS_NAD27_West_Virginia_S';
               32052 : Result := 'PCS_NAD27_Wisconsin_North';
               32053 : Result := 'PCS_NAD27_Wisconsin_Cen';
               32054 : Result := 'PCS_NAD27_Wisconsin_South';
               32055 : Result := 'PCS_NAD27_Wyoming_East';
               32056 : Result := 'PCS_NAD27_Wyoming_E_Cen';
               32057 : Result := 'PCS_NAD27_Wyoming_W_Cen';
               32058 : Result := 'PCS_NAD27_Wyoming_West';
               32059 : Result := 'PCS_NAD27_Puerto_Rico';
               32060 : Result := 'PCS_NAD27_St_Croix';
               32100 : Result := 'PCS_NAD83_Montana';
               32104 : Result := 'PCS_NAD83_Nebraska';
               32107 : Result := 'PCS_NAD83_Nevada_East';
               32108 : Result := 'PCS_NAD83_Nevada_Central';
               32109 : Result := 'PCS_NAD83_Nevada_West';
               32110 : Result := 'PCS_NAD83_New_Hampshire';
               32111 : Result := 'PCS_NAD83_New_Jersey';
               32112 : Result := 'PCS_NAD83_New_Mexico_East';
               32113 : Result := 'PCS_NAD83_New_Mexico_Cent';
               32114 : Result := 'PCS_NAD83_New_Mexico_West';
               32115 : Result := 'PCS_NAD83_New_York_East';
               32116 : Result := 'PCS_NAD83_New_York_Central';
               32117 : Result := 'PCS_NAD83_New_York_West';
               32118 : Result := 'PCS_NAD83_New_York_Long_Is';
               32119 : Result := 'PCS_NAD83_North_Carolina';
               32120 : Result := 'PCS_NAD83_North_Dakota_N';
               32121 : Result := 'PCS_NAD83_North_Dakota_S';
               32122 : Result := 'PCS_NAD83_Ohio_North';
               32123 : Result := 'PCS_NAD83_Ohio_South';
               32124 : Result := 'PCS_NAD83_Oklahoma_North';
               32125 : Result := 'PCS_NAD83_Oklahoma_South';
               32126 : Result := 'PCS_NAD83_Oregon_North';
               32127 : Result := 'PCS_NAD83_Oregon_South';
               32128 : Result := 'PCS_NAD83_Pennsylvania_N';
               32129 : Result := 'PCS_NAD83_Pennsylvania_S';
               32130 : Result := 'PCS_NAD83_Rhode_Island';
               32133 : Result := 'PCS_NAD83_South_Carolina';
               32134 : Result := 'PCS_NAD83_South_Dakota_N';
               32135 : Result := 'PCS_NAD83_South_Dakota_S';
               32136 : Result := 'PCS_NAD83_Tennessee';
               32137 : Result := 'PCS_NAD83_Texas_North';
               32138 : Result := 'PCS_NAD83_Texas_North_Cen';
               32139 : Result := 'PCS_NAD83_Texas_Central';
               32140 : Result := 'PCS_NAD83_Texas_South_Cen';
               32141 : Result := 'PCS_NAD83_Texas_South';
               32142 : Result := 'PCS_NAD83_Utah_North';
               32143 : Result := 'PCS_NAD83_Utah_Central';
               32144 : Result := 'PCS_NAD83_Utah_South';
               32145 : Result := 'PCS_NAD83_Vermont';
               32146 : Result := 'PCS_NAD83_Virginia_North';
               32147 : Result := 'PCS_NAD83_Virginia_South';
               32148 : Result := 'PCS_NAD83_Washington_North';
               32149 : Result := 'PCS_NAD83_Washington_South';
               32150 : Result := 'PCS_NAD83_West_Virginia_N';
               32151 : Result := 'PCS_NAD83_West_Virginia_S';
               32152 : Result := 'PCS_NAD83_Wisconsin_North';
               32153 : Result := 'PCS_NAD83_Wisconsin_Cen';
               32154 : Result := 'PCS_NAD83_Wisconsin_South';
               32155 : Result := 'PCS_NAD83_Wyoming_East';
               32156 : Result := 'PCS_NAD83_Wyoming_E_Cen';
               32157 : Result := 'PCS_NAD83_Wyoming_W_Cen';
               32158 : Result := 'PCS_NAD83_Wyoming_West';
               32161 : Result := 'PCS_NAD83_Puerto_Rico_Virgin_Is';
               32201 : Result := 'PCS_WGS72_UTM_zone_1N';
               32202 : Result := 'PCS_WGS72_UTM_zone_2N';
               32203 : Result := 'PCS_WGS72_UTM_zone_3N';
               32204 : Result := 'PCS_WGS72_UTM_zone_4N';
               32205 : Result := 'PCS_WGS72_UTM_zone_5N';
               32206 : Result := 'PCS_WGS72_UTM_zone_6N';
               32207 : Result := 'PCS_WGS72_UTM_zone_7N';
               32208 : Result := 'PCS_WGS72_UTM_zone_8N';
               32209 : Result := 'PCS_WGS72_UTM_zone_9N';
               32210 : Result := 'PCS_WGS72_UTM_zone_10N';
               32211 : Result := 'PCS_WGS72_UTM_zone_11N';
               32212 : Result := 'PCS_WGS72_UTM_zone_12N';
               32213 : Result := 'PCS_WGS72_UTM_zone_13N';
               32214 : Result := 'PCS_WGS72_UTM_zone_14N';
               32215 : Result := 'PCS_WGS72_UTM_zone_15N';
               32216 : Result := 'PCS_WGS72_UTM_zone_16N';
               32217 : Result := 'PCS_WGS72_UTM_zone_17N';
               32218 : Result := 'PCS_WGS72_UTM_zone_18N';
               32219 : Result := 'PCS_WGS72_UTM_zone_19N';
               32220 : Result := 'PCS_WGS72_UTM_zone_20N';
               32221 : Result := 'PCS_WGS72_UTM_zone_21N';
               32222 : Result := 'PCS_WGS72_UTM_zone_22N';
               32223 : Result := 'PCS_WGS72_UTM_zone_23N';
               32224 : Result := 'PCS_WGS72_UTM_zone_24N';
               32225 : Result := 'PCS_WGS72_UTM_zone_25N';
               32226 : Result := 'PCS_WGS72_UTM_zone_26N';
               32227 : Result := 'PCS_WGS72_UTM_zone_27N';
               32228 : Result := 'PCS_WGS72_UTM_zone_28N';
               32229 : Result := 'PCS_WGS72_UTM_zone_29N';
               32230 : Result := 'PCS_WGS72_UTM_zone_30N';
               32231 : Result := 'PCS_WGS72_UTM_zone_31N';
               32232 : Result := 'PCS_WGS72_UTM_zone_32N';
               32233 : Result := 'PCS_WGS72_UTM_zone_33N';
               32234 : Result := 'PCS_WGS72_UTM_zone_34N';
               32235 : Result := 'PCS_WGS72_UTM_zone_35N';
               32236 : Result := 'PCS_WGS72_UTM_zone_36N';
               32237 : Result := 'PCS_WGS72_UTM_zone_37N';
               32238 : Result := 'PCS_WGS72_UTM_zone_38N';
               32239 : Result := 'PCS_WGS72_UTM_zone_39N';
               32240 : Result := 'PCS_WGS72_UTM_zone_40N';
               32241 : Result := 'PCS_WGS72_UTM_zone_41N';
               32242 : Result := 'PCS_WGS72_UTM_zone_42N';
               32243 : Result := 'PCS_WGS72_UTM_zone_43N';
               32244 : Result := 'PCS_WGS72_UTM_zone_44N';
               32245 : Result := 'PCS_WGS72_UTM_zone_45N';
               32246 : Result := 'PCS_WGS72_UTM_zone_46N';
               32247 : Result := 'PCS_WGS72_UTM_zone_47N';
               32248 : Result := 'PCS_WGS72_UTM_zone_48N';
               32249 : Result := 'PCS_WGS72_UTM_zone_49N';
               32250 : Result := 'PCS_WGS72_UTM_zone_50N';
               32251 : Result := 'PCS_WGS72_UTM_zone_51N';
               32252 : Result := 'PCS_WGS72_UTM_zone_52N';
               32253 : Result := 'PCS_WGS72_UTM_zone_53N';
               32254 : Result := 'PCS_WGS72_UTM_zone_54N';
               32255 : Result := 'PCS_WGS72_UTM_zone_55N';
               32256 : Result := 'PCS_WGS72_UTM_zone_56N';
               32257 : Result := 'PCS_WGS72_UTM_zone_57N';
               32258 : Result := 'PCS_WGS72_UTM_zone_58N';
               32259 : Result := 'PCS_WGS72_UTM_zone_59N';
               32260 : Result := 'PCS_WGS72_UTM_zone_60N';
               32301 : Result := 'PCS_WGS72_UTM_zone_1S';
               32302 : Result := 'PCS_WGS72_UTM_zone_2S';
               32303 : Result := 'PCS_WGS72_UTM_zone_3S';
               32304 : Result := 'PCS_WGS72_UTM_zone_4S';
               32305 : Result := 'PCS_WGS72_UTM_zone_5S';
               32306 : Result := 'PCS_WGS72_UTM_zone_6S';
               32307 : Result := 'PCS_WGS72_UTM_zone_7S';
               32308 : Result := 'PCS_WGS72_UTM_zone_8S';
               32309 : Result := 'PCS_WGS72_UTM_zone_9S';
               32310 : Result := 'PCS_WGS72_UTM_zone_10S';
               32311 : Result := 'PCS_WGS72_UTM_zone_11S';
               32312 : Result := 'PCS_WGS72_UTM_zone_12S';
               32313 : Result := 'PCS_WGS72_UTM_zone_13S';
               32314 : Result := 'PCS_WGS72_UTM_zone_14S';
               32315 : Result := 'PCS_WGS72_UTM_zone_15S';
               32316 : Result := 'PCS_WGS72_UTM_zone_16S';
               32317 : Result := 'PCS_WGS72_UTM_zone_17S';
               32318 : Result := 'PCS_WGS72_UTM_zone_18S';
               32319 : Result := 'PCS_WGS72_UTM_zone_19S';
               32320 : Result := 'PCS_WGS72_UTM_zone_20S';
               32321 : Result := 'PCS_WGS72_UTM_zone_21S';
               32322 : Result := 'PCS_WGS72_UTM_zone_22S';
               32323 : Result := 'PCS_WGS72_UTM_zone_23S';
               32324 : Result := 'PCS_WGS72_UTM_zone_24S';
               32325 : Result := 'PCS_WGS72_UTM_zone_25S';
               32326 : Result := 'PCS_WGS72_UTM_zone_26S';
               32327 : Result := 'PCS_WGS72_UTM_zone_27S';
               32328 : Result := 'PCS_WGS72_UTM_zone_28S';
               32329 : Result := 'PCS_WGS72_UTM_zone_29S';
               32330 : Result := 'PCS_WGS72_UTM_zone_30S';
               32331 : Result := 'PCS_WGS72_UTM_zone_31S';
               32332 : Result := 'PCS_WGS72_UTM_zone_32S';
               32333 : Result := 'PCS_WGS72_UTM_zone_33S';
               32334 : Result := 'PCS_WGS72_UTM_zone_34S';
               32335 : Result := 'PCS_WGS72_UTM_zone_35S';
               32336 : Result := 'PCS_WGS72_UTM_zone_36S';
               32337 : Result := 'PCS_WGS72_UTM_zone_37S';
               32338 : Result := 'PCS_WGS72_UTM_zone_38S';
               32339 : Result := 'PCS_WGS72_UTM_zone_39S';
               32340 : Result := 'PCS_WGS72_UTM_zone_40S';
               32341 : Result := 'PCS_WGS72_UTM_zone_41S';
               32342 : Result := 'PCS_WGS72_UTM_zone_42S';
               32343 : Result := 'PCS_WGS72_UTM_zone_43S';
               32344 : Result := 'PCS_WGS72_UTM_zone_44S';
               32345 : Result := 'PCS_WGS72_UTM_zone_45S';
               32346 : Result := 'PCS_WGS72_UTM_zone_46S';
               32347 : Result := 'PCS_WGS72_UTM_zone_47S';
               32348 : Result := 'PCS_WGS72_UTM_zone_48S';
               32349 : Result := 'PCS_WGS72_UTM_zone_49S';
               32350 : Result := 'PCS_WGS72_UTM_zone_50S';
               32351 : Result := 'PCS_WGS72_UTM_zone_51S';
               32352 : Result := 'PCS_WGS72_UTM_zone_52S';
               32353 : Result := 'PCS_WGS72_UTM_zone_53S';
               32354 : Result := 'PCS_WGS72_UTM_zone_54S';
               32355 : Result := 'PCS_WGS72_UTM_zone_55S';
               32356 : Result := 'PCS_WGS72_UTM_zone_56S';
               32357 : Result := 'PCS_WGS72_UTM_zone_57S';
               32358 : Result := 'PCS_WGS72_UTM_zone_58S';
               32359 : Result := 'PCS_WGS72_UTM_zone_59S';
               32360 : Result := 'PCS_WGS72_UTM_zone_60S';
               32401 : Result := 'PCS_WGS72BE_UTM_zone_1N';
               32402 : Result := 'PCS_WGS72BE_UTM_zone_2N';
               32403 : Result := 'PCS_WGS72BE_UTM_zone_3N';
               32404 : Result := 'PCS_WGS72BE_UTM_zone_4N';
               32405 : Result := 'PCS_WGS72BE_UTM_zone_5N';
               32406 : Result := 'PCS_WGS72BE_UTM_zone_6N';
               32407 : Result := 'PCS_WGS72BE_UTM_zone_7N';
               32408 : Result := 'PCS_WGS72BE_UTM_zone_8N';
               32409 : Result := 'PCS_WGS72BE_UTM_zone_9N';
               32410 : Result := 'PCS_WGS72BE_UTM_zone_10N';
               32411 : Result := 'PCS_WGS72BE_UTM_zone_11N';
               32412 : Result := 'PCS_WGS72BE_UTM_zone_12N';
               32413 : Result := 'PCS_WGS72BE_UTM_zone_13N';
               32414 : Result := 'PCS_WGS72BE_UTM_zone_14N';
               32415 : Result := 'PCS_WGS72BE_UTM_zone_15N';
               32416 : Result := 'PCS_WGS72BE_UTM_zone_16N';
               32417 : Result := 'PCS_WGS72BE_UTM_zone_17N';
               32418 : Result := 'PCS_WGS72BE_UTM_zone_18N';
               32419 : Result := 'PCS_WGS72BE_UTM_zone_19N';
               32420 : Result := 'PCS_WGS72BE_UTM_zone_20N';
               32421 : Result := 'PCS_WGS72BE_UTM_zone_21N';
               32422 : Result := 'PCS_WGS72BE_UTM_zone_22N';
               32423 : Result := 'PCS_WGS72BE_UTM_zone_23N';
               32424 : Result := 'PCS_WGS72BE_UTM_zone_24N';
               32425 : Result := 'PCS_WGS72BE_UTM_zone_25N';
               32426 : Result := 'PCS_WGS72BE_UTM_zone_26N';
               32427 : Result := 'PCS_WGS72BE_UTM_zone_27N';
               32428 : Result := 'PCS_WGS72BE_UTM_zone_28N';
               32429 : Result := 'PCS_WGS72BE_UTM_zone_29N';
               32430 : Result := 'PCS_WGS72BE_UTM_zone_30N';
               32431 : Result := 'PCS_WGS72BE_UTM_zone_31N';
               32432 : Result := 'PCS_WGS72BE_UTM_zone_32N';
               32433 : Result := 'PCS_WGS72BE_UTM_zone_33N';
               32434 : Result := 'PCS_WGS72BE_UTM_zone_34N';
               32435 : Result := 'PCS_WGS72BE_UTM_zone_35N';
               32436 : Result := 'PCS_WGS72BE_UTM_zone_36N';
               32437 : Result := 'PCS_WGS72BE_UTM_zone_37N';
               32438 : Result := 'PCS_WGS72BE_UTM_zone_38N';
               32439 : Result := 'PCS_WGS72BE_UTM_zone_39N';
               32440 : Result := 'PCS_WGS72BE_UTM_zone_40N';
               32441 : Result := 'PCS_WGS72BE_UTM_zone_41N';
               32442 : Result := 'PCS_WGS72BE_UTM_zone_42N';
               32443 : Result := 'PCS_WGS72BE_UTM_zone_43N';
               32444 : Result := 'PCS_WGS72BE_UTM_zone_44N';
               32445 : Result := 'PCS_WGS72BE_UTM_zone_45N';
               32446 : Result := 'PCS_WGS72BE_UTM_zone_46N';
               32447 : Result := 'PCS_WGS72BE_UTM_zone_47N';
               32448 : Result := 'PCS_WGS72BE_UTM_zone_48N';
               32449 : Result := 'PCS_WGS72BE_UTM_zone_49N';
               32450 : Result := 'PCS_WGS72BE_UTM_zone_50N';
               32451 : Result := 'PCS_WGS72BE_UTM_zone_51N';
               32452 : Result := 'PCS_WGS72BE_UTM_zone_52N';
               32453 : Result := 'PCS_WGS72BE_UTM_zone_53N';
               32454 : Result := 'PCS_WGS72BE_UTM_zone_54N';
               32455 : Result := 'PCS_WGS72BE_UTM_zone_55N';
               32456 : Result := 'PCS_WGS72BE_UTM_zone_56N';
               32457 : Result := 'PCS_WGS72BE_UTM_zone_57N';
               32458 : Result := 'PCS_WGS72BE_UTM_zone_58N';
               32459 : Result := 'PCS_WGS72BE_UTM_zone_59N';
               32460 : Result := 'PCS_WGS72BE_UTM_zone_60N';
               32501 : Result := 'PCS_WGS72BE_UTM_zone_1S';
               32502 : Result := 'PCS_WGS72BE_UTM_zone_2S';
               32503 : Result := 'PCS_WGS72BE_UTM_zone_3S';
               32504 : Result := 'PCS_WGS72BE_UTM_zone_4S';
               32505 : Result := 'PCS_WGS72BE_UTM_zone_5S';
               32506 : Result := 'PCS_WGS72BE_UTM_zone_6S';
               32507 : Result := 'PCS_WGS72BE_UTM_zone_7S';
               32508 : Result := 'PCS_WGS72BE_UTM_zone_8S';
               32509 : Result := 'PCS_WGS72BE_UTM_zone_9S';
               32510 : Result := 'PCS_WGS72BE_UTM_zone_10S';
               32511 : Result := 'PCS_WGS72BE_UTM_zone_11S';
               32512 : Result := 'PCS_WGS72BE_UTM_zone_12S';
               32513 : Result := 'PCS_WGS72BE_UTM_zone_13S';
               32514 : Result := 'PCS_WGS72BE_UTM_zone_14S';
               32515 : Result := 'PCS_WGS72BE_UTM_zone_15S';
               32516 : Result := 'PCS_WGS72BE_UTM_zone_16S';
               32517 : Result := 'PCS_WGS72BE_UTM_zone_17S';
               32518 : Result := 'PCS_WGS72BE_UTM_zone_18S';
               32519 : Result := 'PCS_WGS72BE_UTM_zone_19S';
               32520 : Result := 'PCS_WGS72BE_UTM_zone_20S';
               32521 : Result := 'PCS_WGS72BE_UTM_zone_21S';
               32522 : Result := 'PCS_WGS72BE_UTM_zone_22S';
               32523 : Result := 'PCS_WGS72BE_UTM_zone_23S';
               32524 : Result := 'PCS_WGS72BE_UTM_zone_24S';
               32525 : Result := 'PCS_WGS72BE_UTM_zone_25S';
               32526 : Result := 'PCS_WGS72BE_UTM_zone_26S';
               32527 : Result := 'PCS_WGS72BE_UTM_zone_27S';
               32528 : Result := 'PCS_WGS72BE_UTM_zone_28S';
               32529 : Result := 'PCS_WGS72BE_UTM_zone_29S';
               32530 : Result := 'PCS_WGS72BE_UTM_zone_30S';
               32531 : Result := 'PCS_WGS72BE_UTM_zone_31S';
               32532 : Result := 'PCS_WGS72BE_UTM_zone_32S';
               32533 : Result := 'PCS_WGS72BE_UTM_zone_33S';
               32534 : Result := 'PCS_WGS72BE_UTM_zone_34S';
               32535 : Result := 'PCS_WGS72BE_UTM_zone_35S';
               32536 : Result := 'PCS_WGS72BE_UTM_zone_36S';
               32537 : Result := 'PCS_WGS72BE_UTM_zone_37S';
               32538 : Result := 'PCS_WGS72BE_UTM_zone_38S';
               32539 : Result := 'PCS_WGS72BE_UTM_zone_39S';
               32540 : Result := 'PCS_WGS72BE_UTM_zone_40S';
               32541 : Result := 'PCS_WGS72BE_UTM_zone_41S';
               32542 : Result := 'PCS_WGS72BE_UTM_zone_42S';
               32543 : Result := 'PCS_WGS72BE_UTM_zone_43S';
               32544 : Result := 'PCS_WGS72BE_UTM_zone_44S';
               32545 : Result := 'PCS_WGS72BE_UTM_zone_45S';
               32546 : Result := 'PCS_WGS72BE_UTM_zone_46S';
               32547 : Result := 'PCS_WGS72BE_UTM_zone_47S';
               32548 : Result := 'PCS_WGS72BE_UTM_zone_48S';
               32549 : Result := 'PCS_WGS72BE_UTM_zone_49S';
               32550 : Result := 'PCS_WGS72BE_UTM_zone_50S';
               32551 : Result := 'PCS_WGS72BE_UTM_zone_51S';
               32552 : Result := 'PCS_WGS72BE_UTM_zone_52S';
               32553 : Result := 'PCS_WGS72BE_UTM_zone_53S';
               32554 : Result := 'PCS_WGS72BE_UTM_zone_54S';
               32555 : Result := 'PCS_WGS72BE_UTM_zone_55S';
               32556 : Result := 'PCS_WGS72BE_UTM_zone_56S';
               32557 : Result := 'PCS_WGS72BE_UTM_zone_57S';
               32558 : Result := 'PCS_WGS72BE_UTM_zone_58S';
               32559 : Result := 'PCS_WGS72BE_UTM_zone_59S';
               32560 : Result := 'PCS_WGS72BE_UTM_zone_60S';
               32601 : Result := 'PCS_WGS84_UTM_zone_1N';
               32602 : Result := 'PCS_WGS84_UTM_zone_2N';
               32603 : Result := 'PCS_WGS84_UTM_zone_3N';
               32604 : Result := 'PCS_WGS84_UTM_zone_4N';
               32605 : Result := 'PCS_WGS84_UTM_zone_5N';
               32606 : Result := 'PCS_WGS84_UTM_zone_6N';
               32607 : Result := 'PCS_WGS84_UTM_zone_7N';
               32608 : Result := 'PCS_WGS84_UTM_zone_8N';
               32609 : Result := 'PCS_WGS84_UTM_zone_9N';
               32610 : Result := 'PCS_WGS84_UTM_zone_10N';
               32611 : Result := 'PCS_WGS84_UTM_zone_11N';
               32612 : Result := 'PCS_WGS84_UTM_zone_12N';
               32613 : Result := 'PCS_WGS84_UTM_zone_13N';
               32614 : Result := 'PCS_WGS84_UTM_zone_14N';
               32615 : Result := 'PCS_WGS84_UTM_zone_15N';
               32616 : Result := 'PCS_WGS84_UTM_zone_16N';
               32617 : Result := 'PCS_WGS84_UTM_zone_17N';
               32618 : Result := 'PCS_WGS84_UTM_zone_18N';
               32619 : Result := 'PCS_WGS84_UTM_zone_19N';
               32620 : Result := 'PCS_WGS84_UTM_zone_20N';
               32621 : Result := 'PCS_WGS84_UTM_zone_21N';
               32622 : Result := 'PCS_WGS84_UTM_zone_22N';
               32623 : Result := 'PCS_WGS84_UTM_zone_23N';
               32624 : Result := 'PCS_WGS84_UTM_zone_24N';
               32625 : Result := 'PCS_WGS84_UTM_zone_25N';
               32626 : Result := 'PCS_WGS84_UTM_zone_26N';
               32627 : Result := 'PCS_WGS84_UTM_zone_27N';
               32628 : Result := 'PCS_WGS84_UTM_zone_28N';
               32629 : Result := 'PCS_WGS84_UTM_zone_29N';
               32630 : Result := 'PCS_WGS84_UTM_zone_30N';
               32631 : Result := 'PCS_WGS84_UTM_zone_31N';
               32632 : Result := 'PCS_WGS84_UTM_zone_32N';
               32633 : Result := 'PCS_WGS84_UTM_zone_33N';
               32634 : Result := 'PCS_WGS84_UTM_zone_34N';
               32635 : Result := 'PCS_WGS84_UTM_zone_35N';
               32636 : Result := 'PCS_WGS84_UTM_zone_36N';
               32637 : Result := 'PCS_WGS84_UTM_zone_37N';
               32638 : Result := 'PCS_WGS84_UTM_zone_38N';
               32639 : Result := 'PCS_WGS84_UTM_zone_39N';
               32640 : Result := 'PCS_WGS84_UTM_zone_40N';
               32641 : Result := 'PCS_WGS84_UTM_zone_41N';
               32642 : Result := 'PCS_WGS84_UTM_zone_42N';
               32643 : Result := 'PCS_WGS84_UTM_zone_43N';
               32644 : Result := 'PCS_WGS84_UTM_zone_44N';
               32645 : Result := 'PCS_WGS84_UTM_zone_45N';
               32646 : Result := 'PCS_WGS84_UTM_zone_46N';
               32647 : Result := 'PCS_WGS84_UTM_zone_47N';
               32648 : Result := 'PCS_WGS84_UTM_zone_48N';
               32649 : Result := 'PCS_WGS84_UTM_zone_49N';
               32650 : Result := 'PCS_WGS84_UTM_zone_50N';
               32651 : Result := 'PCS_WGS84_UTM_zone_51N';
               32652 : Result := 'PCS_WGS84_UTM_zone_52N';
               32653 : Result := 'PCS_WGS84_UTM_zone_53N';
               32654 : Result := 'PCS_WGS84_UTM_zone_54N';
               32655 : Result := 'PCS_WGS84_UTM_zone_55N';
               32656 : Result := 'PCS_WGS84_UTM_zone_56N';
               32657 : Result := 'PCS_WGS84_UTM_zone_57N';
               32658 : Result := 'PCS_WGS84_UTM_zone_58N';
               32659 : Result := 'PCS_WGS84_UTM_zone_59N';
               32660 : Result := 'PCS_WGS84_UTM_zone_60N';
               32701 : Result := 'PCS_WGS84_UTM_zone_1S';
               32702 : Result := 'PCS_WGS84_UTM_zone_2S';
               32703 : Result := 'PCS_WGS84_UTM_zone_3S';
               32704 : Result := 'PCS_WGS84_UTM_zone_4S';
               32705 : Result := 'PCS_WGS84_UTM_zone_5S';
               32706 : Result := 'PCS_WGS84_UTM_zone_6S';
               32707 : Result := 'PCS_WGS84_UTM_zone_7S';
               32708 : Result := 'PCS_WGS84_UTM_zone_8S';
               32709 : Result := 'PCS_WGS84_UTM_zone_9S';
               32710 : Result := 'PCS_WGS84_UTM_zone_10S';
               32711 : Result := 'PCS_WGS84_UTM_zone_11S';
               32712 : Result := 'PCS_WGS84_UTM_zone_12S';
               32713 : Result := 'PCS_WGS84_UTM_zone_13S';
               32714 : Result := 'PCS_WGS84_UTM_zone_14S';
               32715 : Result := 'PCS_WGS84_UTM_zone_15S';
               32716 : Result := 'PCS_WGS84_UTM_zone_16S';
               32717 : Result := 'PCS_WGS84_UTM_zone_17S';
               32718 : Result := 'PCS_WGS84_UTM_zone_18S';
               32719 : Result := 'PCS_WGS84_UTM_zone_19S';
               32720 : Result := 'PCS_WGS84_UTM_zone_20S';
               32721 : Result := 'PCS_WGS84_UTM_zone_21S';
               32722 : Result := 'PCS_WGS84_UTM_zone_22S';
               32723 : Result := 'PCS_WGS84_UTM_zone_23S';
               32724 : Result := 'PCS_WGS84_UTM_zone_24S';
               32725 : Result := 'PCS_WGS84_UTM_zone_25S';
               32726 : Result := 'PCS_WGS84_UTM_zone_26S';
               32727 : Result := 'PCS_WGS84_UTM_zone_27S';
               32728 : Result := 'PCS_WGS84_UTM_zone_28S';
               32729 : Result := 'PCS_WGS84_UTM_zone_29S';
               32730 : Result := 'PCS_WGS84_UTM_zone_30S';
               32731 : Result := 'PCS_WGS84_UTM_zone_31S';
               32732 : Result := 'PCS_WGS84_UTM_zone_32S';
               32733 : Result := 'PCS_WGS84_UTM_zone_33S';
               32734 : Result := 'PCS_WGS84_UTM_zone_34S';
               32735 : Result := 'PCS_WGS84_UTM_zone_35S';
               32736 : Result := 'PCS_WGS84_UTM_zone_36S';
               32737 : Result := 'PCS_WGS84_UTM_zone_37S';
               32738 : Result := 'PCS_WGS84_UTM_zone_38S';
               32739 : Result := 'PCS_WGS84_UTM_zone_39S';
               32740 : Result := 'PCS_WGS84_UTM_zone_40S';
               32741 : Result := 'PCS_WGS84_UTM_zone_41S';
               32742 : Result := 'PCS_WGS84_UTM_zone_42S';
               32743 : Result := 'PCS_WGS84_UTM_zone_43S';
               32744 : Result := 'PCS_WGS84_UTM_zone_44S';
               32745 : Result := 'PCS_WGS84_UTM_zone_45S';
               32746 : Result := 'PCS_WGS84_UTM_zone_46S';
               32747 : Result := 'PCS_WGS84_UTM_zone_47S';
               32748 : Result := 'PCS_WGS84_UTM_zone_48S';
               32749 : Result := 'PCS_WGS84_UTM_zone_49S';
               32750 : Result := 'PCS_WGS84_UTM_zone_50S';
               32751 : Result := 'PCS_WGS84_UTM_zone_51S';
               32752 : Result := 'PCS_WGS84_UTM_zone_52S';
               32753 : Result := 'PCS_WGS84_UTM_zone_53S';
               32754 : Result := 'PCS_WGS84_UTM_zone_54S';
               32755 : Result := 'PCS_WGS84_UTM_zone_55S';
               32756 : Result := 'PCS_WGS84_UTM_zone_56S';
               32757 : Result := 'PCS_WGS84_UTM_zone_57S';
               32758 : Result := 'PCS_WGS84_UTM_zone_58S';
               32759 : Result := 'PCS_WGS84_UTM_zone_59S';
               32760 : Result := 'PCS_WGS84_UTM_zone_60S';
             end;
      3074 : case iVal of      { Projection Codes }
               10101 : Result := 'Proj_Alabama_CS27_East';
               10102 : Result := 'Proj_Alabama_CS27_West';
               10131 : Result := 'Proj_Alabama_CS83_East';
               10132 : Result := 'Proj_Alabama_CS83_West';
               10201 : Result := 'Proj_Arizona_Coordinate_System_east';
               10202 : Result := 'Proj_Arizona_Coordinate_System_Central';
               10203 : Result := 'Proj_Arizona_Coordinate_System_west';
               10231 : Result := 'Proj_Arizona_CS83_east';
               10232 : Result := 'Proj_Arizona_CS83_Central';
               10233 : Result := 'Proj_Arizona_CS83_west';
               10301 : Result := 'Proj_Arkansas_CS27_North';
               10302 : Result := 'Proj_Arkansas_CS27_South';
               10331 : Result := 'Proj_Arkansas_CS83_North';
               10332 : Result := 'Proj_Arkansas_CS83_South';
               10401 : Result := 'Proj_California_CS27_I';
               10402 : Result := 'Proj_California_CS27_II';
               10403 : Result := 'Proj_California_CS27_III';
               10404 : Result := 'Proj_California_CS27_IV';
               10405 : Result := 'Proj_California_CS27_V';
               10406 : Result := 'Proj_California_CS27_VI';
               10407 : Result := 'Proj_California_CS27_VII';
               10431 : Result := 'Proj_California_CS83_1';
               10432 : Result := 'Proj_California_CS83_2';
               10433 : Result := 'Proj_California_CS83_3';
               10434 : Result := 'Proj_California_CS83_4';
               10435 : Result := 'Proj_California_CS83_5';
               10436 : Result := 'Proj_California_CS83_6';
               10501 : Result := 'Proj_Colorado_CS27_North';
               10502 : Result := 'Proj_Colorado_CS27_Central';
               10503 : Result := 'Proj_Colorado_CS27_South';
               10531 : Result := 'Proj_Colorado_CS83_North';
               10532 : Result := 'Proj_Colorado_CS83_Central';
               10533 : Result := 'Proj_Colorado_CS83_South';
               10600 : Result := 'Proj_Connecticut_CS27';
               10630 : Result := 'Proj_Connecticut_CS83';
               10700 : Result := 'Proj_Delaware_CS27';
               10730 : Result := 'Proj_Delaware_CS83';
               10901 : Result := 'Proj_Florida_CS27_East';
               10902 : Result := 'Proj_Florida_CS27_West';
               10903 : Result := 'Proj_Florida_CS27_North';
               10931 : Result := 'Proj_Florida_CS83_East';
               10932 : Result := 'Proj_Florida_CS83_West';
               10933 : Result := 'Proj_Florida_CS83_North';
               11001 : Result := 'Proj_Georgia_CS27_East';
               11002 : Result := 'Proj_Georgia_CS27_West';
               11031 : Result := 'Proj_Georgia_CS83_East';
               11032 : Result := 'Proj_Georgia_CS83_West';
               11101 : Result := 'Proj_Idaho_CS27_East';
               11102 : Result := 'Proj_Idaho_CS27_Central';
               11103 : Result := 'Proj_Idaho_CS27_West';
               11131 : Result := 'Proj_Idaho_CS83_East';
               11132 : Result := 'Proj_Idaho_CS83_Central';
               11133 : Result := 'Proj_Idaho_CS83_West';
               11201 : Result := 'Proj_Illinois_CS27_East';
               11202 : Result := 'Proj_Illinois_CS27_West';
               11231 : Result := 'Proj_Illinois_CS83_East';
               11232 : Result := 'Proj_Illinois_CS83_West';
               11301 : Result := 'Proj_Indiana_CS27_East';
               11302 : Result := 'Proj_Indiana_CS27_West';
               11331 : Result := 'Proj_Indiana_CS83_East';
               11332 : Result := 'Proj_Indiana_CS83_West';
               11401 : Result := 'Proj_Iowa_CS27_North';
               11402 : Result := 'Proj_Iowa_CS27_South';
               11431 : Result := 'Proj_Iowa_CS83_North';
               11432 : Result := 'Proj_Iowa_CS83_South';
               11501 : Result := 'Proj_Kansas_CS27_North';
               11502 : Result := 'Proj_Kansas_CS27_South';
               11531 : Result := 'Proj_Kansas_CS83_North';
               11532 : Result := 'Proj_Kansas_CS83_South';
               11601 : Result := 'Proj_Kentucky_CS27_North';
               11602 : Result := 'Proj_Kentucky_CS27_South';
               11631 : Result := 'Proj_Kentucky_CS83_North';
               11632 : Result := 'Proj_Kentucky_CS83_South';
               11701 : Result := 'Proj_Louisiana_CS27_North';
               11702 : Result := 'Proj_Louisiana_CS27_South';
               11731 : Result := 'Proj_Louisiana_CS83_North';
               11732 : Result := 'Proj_Louisiana_CS83_South';
               11801 : Result := 'Proj_Maine_CS27_East';
               11802 : Result := 'Proj_Maine_CS27_West';
               11831 : Result := 'Proj_Maine_CS83_East';
               11832 : Result := 'Proj_Maine_CS83_West';
               11900 : Result := 'Proj_Maryland_CS27';
               11930 : Result := 'Proj_Maryland_CS83';
               12001 : Result := 'Proj_Massachusetts_CS27_Mainland';
               12002 : Result := 'Proj_Massachusetts_CS27_Island';
               12031 : Result := 'Proj_Massachusetts_CS83_Mainland';
               12032 : Result := 'Proj_Massachusetts_CS83_Island';
               12101 : Result := 'Proj_Michigan_State_Plane_East';
               12102 : Result := 'Proj_Michigan_State_Plane_Old_Central';
               12103 : Result := 'Proj_Michigan_State_Plane_West';
               12111 : Result := 'Proj_Michigan_CS27_North';
               12112 : Result := 'Proj_Michigan_CS27_Central';
               12113 : Result := 'Proj_Michigan_CS27_South';
               12141 : Result := 'Proj_Michigan_CS83_North';
               12142 : Result := 'Proj_Michigan_CS83_Central';
               12143 : Result := 'Proj_Michigan_CS83_South';
               12201 : Result := 'Proj_Minnesota_CS27_North';
               12202 : Result := 'Proj_Minnesota_CS27_Central';
               12203 : Result := 'Proj_Minnesota_CS27_South';
               12231 : Result := 'Proj_Minnesota_CS83_North';
               12232 : Result := 'Proj_Minnesota_CS83_Central';
               12233 : Result := 'Proj_Minnesota_CS83_South';
               12301 : Result := 'Proj_Mississippi_CS27_East';
               12302 : Result := 'Proj_Mississippi_CS27_West';
               12331 : Result := 'Proj_Mississippi_CS83_East';
               12332 : Result := 'Proj_Mississippi_CS83_West';
               12401 : Result := 'Proj_Missouri_CS27_East';
               12402 : Result := 'Proj_Missouri_CS27_Central';
               12403 : Result := 'Proj_Missouri_CS27_West';
               12431 : Result := 'Proj_Missouri_CS83_East';
               12432 : Result := 'Proj_Missouri_CS83_Central';
               12433 : Result := 'Proj_Missouri_CS83_West';
               12501 : Result := 'Proj_Montana_CS27_North';
               12502 : Result := 'Proj_Montana_CS27_Central';
               12503 : Result := 'Proj_Montana_CS27_South';
               12530 : Result := 'Proj_Montana_CS83';
               12601 : Result := 'Proj_Nebraska_CS27_North';
               12602 : Result := 'Proj_Nebraska_CS27_South';
               12630 : Result := 'Proj_Nebraska_CS83';
               12701 : Result := 'Proj_Nevada_CS27_East';
               12702 : Result := 'Proj_Nevada_CS27_Central';
               12703 : Result := 'Proj_Nevada_CS27_West';
               12731 : Result := 'Proj_Nevada_CS83_East';
               12732 : Result := 'Proj_Nevada_CS83_Central';
               12733 : Result := 'Proj_Nevada_CS83_West';
               12800 : Result := 'Proj_New_Hampshire_CS27';
               12830 : Result := 'Proj_New_Hampshire_CS83';
               12900 : Result := 'Proj_New_Jersey_CS27';
               12930 : Result := 'Proj_New_Jersey_CS83';
               13001 : Result := 'Proj_New_Mexico_CS27_East';
               13002 : Result := 'Proj_New_Mexico_CS27_Central';
               13003 : Result := 'Proj_New_Mexico_CS27_West';
               13031 : Result := 'Proj_New_Mexico_CS83_East';
               13032 : Result := 'Proj_New_Mexico_CS83_Central';
               13033 : Result := 'Proj_New_Mexico_CS83_West';
               13101 : Result := 'Proj_New_York_CS27_East';
               13102 : Result := 'Proj_New_York_CS27_Central';
               13103 : Result := 'Proj_New_York_CS27_West';
               13104 : Result := 'Proj_New_York_CS27_Long_Island';
               13131 : Result := 'Proj_New_York_CS83_East';
               13132 : Result := 'Proj_New_York_CS83_Central';
               13133 : Result := 'Proj_New_York_CS83_West';
               13134 : Result := 'Proj_New_York_CS83_Long_Island';
               13200 : Result := 'Proj_North_Carolina_CS27';
               13230 : Result := 'Proj_North_Carolina_CS83';
               13301 : Result := 'Proj_North_Dakota_CS27_North';
               13302 : Result := 'Proj_North_Dakota_CS27_South';
               13331 : Result := 'Proj_North_Dakota_CS83_North';
               13332 : Result := 'Proj_North_Dakota_CS83_South';
               13401 : Result := 'Proj_Ohio_CS27_North';
               13402 : Result := 'Proj_Ohio_CS27_South';
               13431 : Result := 'Proj_Ohio_CS83_North';
               13432 : Result := 'Proj_Ohio_CS83_South';
               13501 : Result := 'Proj_Oklahoma_CS27_North';
               13502 : Result := 'Proj_Oklahoma_CS27_South';
               13531 : Result := 'Proj_Oklahoma_CS83_North';
               13532 : Result := 'Proj_Oklahoma_CS83_South';
               13601 : Result := 'Proj_Oregon_CS27_North';
               13602 : Result := 'Proj_Oregon_CS27_South';
               13631 : Result := 'Proj_Oregon_CS83_North';
               13632 : Result := 'Proj_Oregon_CS83_South';
               13701 : Result := 'Proj_Pennsylvania_CS27_North';
               13702 : Result := 'Proj_Pennsylvania_CS27_South';
               13731 : Result := 'Proj_Pennsylvania_CS83_North';
               13732 : Result := 'Proj_Pennsylvania_CS83_South';
               13800 : Result := 'Proj_Rhode_Island_CS27';
               13830 : Result := 'Proj_Rhode_Island_CS83';
               13901 : Result := 'Proj_South_Carolina_CS27_North';
               13902 : Result := 'Proj_South_Carolina_CS27_South';
               13930 : Result := 'Proj_South_Carolina_CS83';
               14001 : Result := 'Proj_South_Dakota_CS27_North';
               14002 : Result := 'Proj_South_Dakota_CS27_South';
               14031 : Result := 'Proj_South_Dakota_CS83_North';
               14032 : Result := 'Proj_South_Dakota_CS83_South';
               14100 : Result := 'Proj_Tennessee_CS27';
               14130 : Result := 'Proj_Tennessee_CS83';
               14201 : Result := 'Proj_Texas_CS27_North';
               14202 : Result := 'Proj_Texas_CS27_North_Central';
               14203 : Result := 'Proj_Texas_CS27_Central';
               14204 : Result := 'Proj_Texas_CS27_South_Central';
               14205 : Result := 'Proj_Texas_CS27_South';
               14231 : Result := 'Proj_Texas_CS83_North';
               14232 : Result := 'Proj_Texas_CS83_North_Central';
               14233 : Result := 'Proj_Texas_CS83_Central';
               14234 : Result := 'Proj_Texas_CS83_South_Central';
               14235 : Result := 'Proj_Texas_CS83_South';
               14301 : Result := 'Proj_Utah_CS27_North';
               14302 : Result := 'Proj_Utah_CS27_Central';
               14303 : Result := 'Proj_Utah_CS27_South';
               14331 : Result := 'Proj_Utah_CS83_North';
               14332 : Result := 'Proj_Utah_CS83_Central';
               14333 : Result := 'Proj_Utah_CS83_South';
               14400 : Result := 'Proj_Vermont_CS27';
               14430 : Result := 'Proj_Vermont_CS83';
               14501 : Result := 'Proj_Virginia_CS27_North';
               14502 : Result := 'Proj_Virginia_CS27_South';
               14531 : Result := 'Proj_Virginia_CS83_North';
               14532 : Result := 'Proj_Virginia_CS83_South';
               14601 : Result := 'Proj_Washington_CS27_North';
               14602 : Result := 'Proj_Washington_CS27_South';
               14631 : Result := 'Proj_Washington_CS83_North';
               14632 : Result := 'Proj_Washington_CS83_South';
               14701 : Result := 'Proj_West_Virginia_CS27_North';
               14702 : Result := 'Proj_West_Virginia_CS27_South';
               14731 : Result := 'Proj_West_Virginia_CS83_North';
               14732 : Result := 'Proj_West_Virginia_CS83_South';
               14801 : Result := 'Proj_Wisconsin_CS27_North';
               14802 : Result := 'Proj_Wisconsin_CS27_Central';
               14803 : Result := 'Proj_Wisconsin_CS27_South';
               14831 : Result := 'Proj_Wisconsin_CS83_North';
               14832 : Result := 'Proj_Wisconsin_CS83_Central';
               14833 : Result := 'Proj_Wisconsin_CS83_South';
               14901 : Result := 'Proj_Wyoming_CS27_East';
               14902 : Result := 'Proj_Wyoming_CS27_East_Central';
               14903 : Result := 'Proj_Wyoming_CS27_West_Central';
               14904 : Result := 'Proj_Wyoming_CS27_West';
               14931 : Result := 'Proj_Wyoming_CS83_East';
               14932 : Result := 'Proj_Wyoming_CS83_East_Central';
               14933 : Result := 'Proj_Wyoming_CS83_West_Central';
               14934 : Result := 'Proj_Wyoming_CS83_West';
               15001 : Result := 'Proj_Alaska_CS27_1';
               15002 : Result := 'Proj_Alaska_CS27_2';
               15003 : Result := 'Proj_Alaska_CS27_3';
               15004 : Result := 'Proj_Alaska_CS27_4';
               15005 : Result := 'Proj_Alaska_CS27_5';
               15006 : Result := 'Proj_Alaska_CS27_6';
               15007 : Result := 'Proj_Alaska_CS27_7';
               15008 : Result := 'Proj_Alaska_CS27_8';
               15009 : Result := 'Proj_Alaska_CS27_9';
               15010 : Result := 'Proj_Alaska_CS27_10';
               15031 : Result := 'Proj_Alaska_CS83_1';
               15032 : Result := 'Proj_Alaska_CS83_2';
               15033 : Result := 'Proj_Alaska_CS83_3';
               15034 : Result := 'Proj_Alaska_CS83_4';
               15035 : Result := 'Proj_Alaska_CS83_5';
               15036 : Result := 'Proj_Alaska_CS83_6';
               15037 : Result := 'Proj_Alaska_CS83_7';
               15038 : Result := 'Proj_Alaska_CS83_8';
               15039 : Result := 'Proj_Alaska_CS83_9';
               15040 : Result := 'Proj_Alaska_CS83_10';
               15101 : Result := 'Proj_Hawaii_CS27_1';
               15102 : Result := 'Proj_Hawaii_CS27_2';
               15103 : Result := 'Proj_Hawaii_CS27_3';
               15104 : Result := 'Proj_Hawaii_CS27_4';
               15105 : Result := 'Proj_Hawaii_CS27_5';
               15131 : Result := 'Proj_Hawaii_CS83_1';
               15132 : Result := 'Proj_Hawaii_CS83_2';
               15133 : Result := 'Proj_Hawaii_CS83_3';
               15134 : Result := 'Proj_Hawaii_CS83_4';
               15135 : Result := 'Proj_Hawaii_CS83_5';
               15201 : Result := 'Proj_Puerto_Rico_CS27';
               15202 : Result := 'Proj_St_Croix';
               15230 : Result := 'Proj_Puerto_Rico_Virgin_Is';
               15914 : Result := 'Proj_BLM_14N_feet';
               15915 : Result := 'Proj_BLM_15N_feet';
               15916 : Result := 'Proj_BLM_16N_feet';
               15917 : Result := 'Proj_BLM_17N_feet';
               17348 : Result := 'Proj_Map_Grid_of_Australia_48';
               17349 : Result := 'Proj_Map_Grid_of_Australia_49';
               17350 : Result := 'Proj_Map_Grid_of_Australia_50';
               17351 : Result := 'Proj_Map_Grid_of_Australia_51';
               17352 : Result := 'Proj_Map_Grid_of_Australia_52';
               17353 : Result := 'Proj_Map_Grid_of_Australia_53';
               17354 : Result := 'Proj_Map_Grid_of_Australia_54';
               17355 : Result := 'Proj_Map_Grid_of_Australia_55';
               17356 : Result := 'Proj_Map_Grid_of_Australia_56';
               17357 : Result := 'Proj_Map_Grid_of_Australia_57';
               17358 : Result := 'Proj_Map_Grid_of_Australia_58';
               17448 : Result := 'Proj_Australian_Map_Grid_48';
               17449 : Result := 'Proj_Australian_Map_Grid_49';
               17450 : Result := 'Proj_Australian_Map_Grid_50';
               17451 : Result := 'Proj_Australian_Map_Grid_51';
               17452 : Result := 'Proj_Australian_Map_Grid_52';
               17453 : Result := 'Proj_Australian_Map_Grid_53';
               17454 : Result := 'Proj_Australian_Map_Grid_54';
               17455 : Result := 'Proj_Australian_Map_Grid_55';
               17456 : Result := 'Proj_Australian_Map_Grid_56';
               17457 : Result := 'Proj_Australian_Map_Grid_57';
               17458 : Result := 'Proj_Australian_Map_Grid_58';
               18031 : Result := 'Proj_Argentina_1';
               18032 : Result := 'Proj_Argentina_2';
               18033 : Result := 'Proj_Argentina_3';
               18034 : Result := 'Proj_Argentina_4';
               18035 : Result := 'Proj_Argentina_5';
               18036 : Result := 'Proj_Argentina_6';
               18037 : Result := 'Proj_Argentina_7';
               18051 : Result := 'Proj_Colombia_3W';
               18052 : Result := 'Proj_Colombia_Bogota';
               18053 : Result := 'Proj_Colombia_3E';
               18054 : Result := 'Proj_Colombia_6E';
               18072 : Result := 'Proj_Egypt_Red_Belt';
               18073 : Result := 'Proj_Egypt_Purple_Belt';
               18074 : Result := 'Proj_Extended_Purple_Belt';
               18141 : Result := 'Proj_New_Zealand_North_Island_Nat_Grid';
               18142 : Result := 'Proj_New_Zealand_South_Island_Nat_Grid';
               19900 : Result := 'Proj_Bahrain_Grid';
               19905 : Result := 'Proj_Netherlands_E_Indies_Equatorial';
               19912 : Result := 'Proj_RSO_Borneo';
             end;
      3075 : case iVal of       { Coordinate Transformation Codes }
               1 : Result := 'CT_TransverseMercator';
               2 : Result := 'CT_TransvMercator_Modified_Alaska';
               3 : Result := 'CT_ObliqueMercator';
               4 : Result := 'CT_ObliqueMercator_Laborde';
               5 : Result := 'CT_ObliqueMercator_Rosenmund';
               6 : Result := 'CT_ObliqueMercator_Spherical';
               7 : Result := 'CT_Mercator';
               8 : Result := 'CT_LambertConfConic_2SP';
               9 : Result := 'CT_LambertConfConic_Helmert';
               10 : Result := 'CT_LambertAzimEqualArea';
               11 : Result := 'CT_AlbersEqualArea';
               12 : Result := 'CT_AzimuthalEquidistant';
               13 : Result := 'CT_EquidistantConic';
               14 : Result := 'CT_Stereographic';
               15 : Result := 'CT_PolarStereographic';
               16 : Result := 'CT_ObliqueStereographic';
               17 : Result := 'CT_Equirectangular';
               18 : Result := 'CT_CassiniSoldner';
               19 : Result := 'CT_Gnomonic';
               20 : Result := 'CT_MillerCylindrical';
               21 : Result := 'CT_Orthographic';
               22 : Result := 'CT_Polyconic';
               23 : Result := 'CT_Robinson';
               24 : Result := 'CT_Sinusoidal';
               25 : Result := 'CT_VanDerGrinten';
               26 : Result := 'CT_NewZealandMapGrid';
               27 : Result := 'CT_TransvMercator_SouthOriented';
             end;
      4096 : case iVal of      { Vertical CS Type Codes }
{ EPSG Ellipsoid Vertical CS Codes }
               5001 : Result := 'VertCS_Airy_1830_ellipsoid';
               5002 : Result := 'VertCS_Airy_Modified_1849_ellipsoid';
               5003 : Result := 'VertCS_ANS_ellipsoid';
               5004 : Result := 'VertCS_Bessel_1841_ellipsoid';
               5005 : Result := 'VertCS_Bessel_Modified_ellipsoid';
               5006 : Result := 'VertCS_Bessel_Namibia_ellipsoid';
               5007 : Result := 'VertCS_Clarke_1858_ellipsoid';
               5008 : Result := 'VertCS_Clarke_1866_ellipsoid';
               5010 : Result := 'VertCS_Clarke_1880_Benoit_ellipsoid';
               5011 : Result := 'VertCS_Clarke_1880_IGN_ellipsoid';
               5012 : Result := 'VertCS_Clarke_1880_RGS_ellipsoid';
               5013 : Result := 'VertCS_Clarke_1880_Arc_ellipsoid';
               5014 : Result := 'VertCS_Clarke_1880_SGA_1922_ellipsoid';
               5015 : Result := 'VertCS_Everest_1830_1937_Adjustment_ellipsoid';
               5016 : Result := 'VertCS_Everest_1830_1967_Definition_ellipsoid';
               5017 : Result := 'VertCS_Everest_1830_1975_Definition_ellipsoid';
               5018 : Result := 'VertCS_Everest_1830_Modified_ellipsoid';
               5019 : Result := 'VertCS_GRS_1980_ellipsoid';
               5020 : Result := 'VertCS_Helmert_1906_ellipsoid';
               5021 : Result := 'VertCS_INS_ellipsoid';
               5022 : Result := 'VertCS_International_1924_ellipsoid';
               5023 : Result := 'VertCS_International_1967_ellipsoid';
               5024 : Result := 'VertCS_Krassowsky_1940_ellipsoid';
               5025 : Result := 'VertCS_NWL_9D_ellipsoid';
               5026 : Result := 'VertCS_NWL_10D_ellipsoid';
               5027 : Result := 'VertCS_Plessis_1817_ellipsoid';
               5028 : Result := 'VertCS_Struve_1860_ellipsoid';
               5029 : Result := 'VertCS_War_Office_ellipsoid';
               5030 : Result := 'VertCS_WGS_84_ellipsoid';
               5031 : Result := 'VertCS_GEM_10C_ellipsoid';
               5032 : Result := 'VertCS_OSU86F_ellipsoid';
               5033 : Result := 'VertCS_OSU91A_ellipsoid';
{ EPSG Orthometric Vertical CS Codes }
               5101 : Result := 'VertCS_Newlyn';
               5102 : Result := 'VertCS_North_American_Vertical_Datum_1929';
               5103 : Result := 'VertCS_North_American_Vertical_Datum_1988';
               5104 : Result := 'VertCS_Yellow_Sea_1956';
               5105 : Result := 'VertCS_Baltic_Sea';
               5106 : Result := 'VertCS_Caspian_Sea';
             end;
      4098 : case iVal of      { Vertical CS Datum Codes }
               0 : Result := 'Undefined';   // No codes have been defined yet
             end;
    end;
  end;
end;

begin
end.
