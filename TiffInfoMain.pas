unit TiffInfoMain;

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Menus, ComCtrls, StdCtrls, ShellApi, Grids, ExtCtrls, ImgList,
    System.ImageList;

type
    TformTiffInfo = class(TForm)
        mmTiffInfo: TMainMenu;
        Help1: TMenuItem;
        About1: TMenuItem;
        File1: TMenuItem;
        Open1: TMenuItem;
        N1: TMenuItem;
        Exit1: TMenuItem;
        LV_TiffInfo: TListView;
        OD_Tiff: TOpenDialog;
        Print1: TMenuItem;
        Printsetup1: TMenuItem;
        N2: TMenuItem;
        L_TiffName: TLabel;
        PSD_Tiff: TPrinterSetupDialog;
        PC_TiffInfo: TPageControl;
        TS_TiffIFD: TTabSheet;
        TS_TiffImage: TTabSheet;
        LB_ImageDetails: TListBox;
        TS_Palette: TTabSheet;
        DG_Palette: TDrawGrid;
        PD_Tiff: TPrintDialog;
        TS_GeoTiff: TTabSheet;
        LB_GeoTiff: TListBox;
        LV_GeoTiff: TListView;
        TS_GeoTiffSummary: TTabSheet;
        M_GeoSummary: TMemo;
        LB_TiffFiles: TListBox;
        Spl_TiffFiles: TSplitter;
        PM_TiffList: TPopupMenu;
        PMI_ClearList: TMenuItem;
        Edit1: TMenuItem;
        Clearlist1: TMenuItem;
        Spl_GeoTiff: TSplitter;
        Save1: TMenuItem;
        Options1: TMenuItem;
        Setreportfilename1: TMenuItem;
        ImageList1: TImageList;
        R1: TMenuItem;
        Structure1: TMenuItem;
        procedure Exit1Click(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure Open1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure Printsetup1Click(Sender: TObject);
        procedure Print1Click(Sender: TObject);
        procedure About1Click(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure DG_PaletteDrawCell(Sender: TObject; Col, Row: integer;
          Rect: TRect; State: TGridDrawState);
        procedure FormResize(Sender: TObject);
        procedure LB_TiffFilesClick(Sender: TObject);
        procedure PMI_ClearListClick(Sender: TObject);
        procedure LV_GeoTiffDblClick(Sender: TObject);
        procedure Save1Click(Sender: TObject);
        procedure Setreportfilename1Click(Sender: TObject);
        procedure Structure1Click(Sender: TObject);
        procedure LV_TiffInfoDblClick(Sender: TObject);
        procedure LB_TiffFilesMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
    private
        { Private declarations }
        procedure ExtractInfo;
        procedure RecordDragDrop(Drop: THandle; Min: Boolean);
        procedure WMDropFiles(var Message: TWMDropFiles); message WM_DropFiles;
        function MakeListItem(pc: PChar): string;
        function MakeFileFromItem(sItem: string): string;
    public
        { Public declarations }
        m_fnReportName: TFileName;
        constructor Create(AOwner: TComponent); override;
        procedure SetNewReportName(fn: TFileName);
    end;

var
    formTiffInfo: TformTiffInfo;

implementation

uses
    XMLIntf,
    ReadTiff, TiffAbout, GeoDetail, Options, Details, Math, hexviewer,
    PrintTifInfo, CollectTiffInfo, CollectGeotiffInfo;

var
    buffer: PAnsiChar;

{$R *.DFM}

constructor TformTiffInfo.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);

    m_fnReportName := ExtractFilePath(Application.ExeName) + 'TiffReport.txt';
end;

procedure TformTiffInfo.Exit1Click(Sender: TObject);
begin
    Close;
end;

procedure TformTiffInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    ttInfo.Free;
    Action := caFree;
end;

procedure TformTiffInfo.Open1Click(Sender: TObject);
var
    pc: array [0 .. 255] of char;
    i, iLast: integer;
begin
    if OD_Tiff.Execute then
    begin
        iLast := LB_TiffFiles.Items.Count;
        i := OD_Tiff.Files.Count;
        for i := i - 1 downto 0 do
            LB_TiffFiles.Items.Add
              (MakeListItem(StrPCopy(pc, OD_Tiff.Files[i])));
        LB_TiffFiles.ItemIndex := iLast;
        LB_TiffFilesClick(LB_TiffFiles);

        // Add the tree view
        // iLast := TV_TiffFiles.Items.Count;
        // i := OD_Tiff.Files.Count;
        // for i := i - 1 downto 0 do begin
        // end;
    end;
end;

procedure TformTiffInfo.ExtractInfo;
var
    i, iTag, iType: integer;
    s: string;
    litem: TListItem;
    tip: TImageProperties;
    tgp: TGeoTiffProperties;
    ghRevis: TGeoHeader;
    oXml: IXMLDocument;

begin
    if ttInfo.sTiffName = '' then
        exit;

    L_TiffName.Caption := ttInfo.sTiffName;
    LV_TiffInfo.Items.Clear;
    if ttInfo.iNumberIFD = 0 then
    begin
        litem := LV_TiffInfo.Items.Add;
        litem.Caption := '<Corrupt IFD>';
        exit;
    end;

    for i := 0 to ttInfo.iNumberIFD - 1 do
        with LV_TiffInfo do
        begin
            litem := Items.Add;
            s := ttInfo.sTag(i, iTag);
            litem.Caption := IntToStr(iTag);
            litem.SubItems.Add(s);
            litem.SubItems.Add(ttInfo.sType(i, iType));
            litem.SubItems.Add(Format('%d', [ttInfo.iCount(i)]));
            litem.SubItems.Add(Format('%d', [ttInfo.iValue(i)]));
            litem.SubItems.Add(Format('$%.8x', [ttInfo.iValue(i)]));
        end;

    LB_ImageDetails.Items := collectedTiffInfo.infoAsStrings;

    ttInfo.GetImageProperties(tip);
    if tip.fPalette then
    begin
        DG_Palette.OnDrawCell := DG_PaletteDrawCell;
        if tip.iNrColors = 16 then
        begin
            DG_Palette.ColCount := 4;
            DG_Palette.RowCount := 4;
        end
        else
        begin
            DG_Palette.ColCount := 16;
            DG_Palette.RowCount := 16;
        end;
        FormResize(DG_Palette);
    end
    else
        DG_Palette.OnDrawCell := nil;
    TS_Palette.TabVisible := tip.fPalette;
    TS_GeoTiff.TabVisible := tip.fGeoTiff;
    // TS_GeoTiffSummary.TabVisible := tip.fGeoTiff;
    if tip.fGeoTiff then
    begin
        with LV_GeoTiff do
        begin
            Items.Clear;
            for i := 0 to ttInfo.iNrGeoKeys - 1 do
            begin
                litem := Items.Add;
                s := ttInfo.sGeoTag(i, iTag);
                litem.Caption := IntToStr(iTag);
                litem.SubItems.Add(s);
                litem.SubItems.Add(ttInfo.sGeoLoc(i, iType));
                litem.SubItems.Add(Format('%d', [ttInfo.iGeoCount(i)]));
                litem.SubItems.Add(Format('%d', [ttInfo.iGeoValue(i)]));
                litem.SubItems.Add(ttInfo.sDescript(iTag, ttInfo.iGeoValue(i)));
            end;
        end;
        LB_GeoTiff.Items := collectedGeotiffInfo.infoAsStrings;
    end;
    if ((PC_TiffInfo.ActivePage = TS_Palette) and not TS_Palette.TabVisible) or
      ((PC_TiffInfo.ActivePage = TS_GeoTiff) and not TS_GeoTiff.TabVisible) then
        PC_TiffInfo.ActivePage := TS_TiffIFD;
end;

procedure TformTiffInfo.FormCreate(Sender: TObject);
begin
    ttInfo := TTiffInfo.Create;
    L_TiffName.Caption := 'No TIFF file open';
    L_TiffName.Font.Color := clWhite;
    DragAcceptFiles(Handle, true);
    PC_TiffInfo.ActivePage := TS_TiffIFD;
    TS_Palette.TabVisible := false;
    TS_GeoTiff.TabVisible := false;
    TS_GeoTiffSummary.TabVisible := false;

    // TV_TiffFiles.Visible := false;
    { Application.UpdateFormatSettings := false;
      DateOrder := doDMY;
      DateFullYear := True; }
end;

procedure TformTiffInfo.Printsetup1Click(Sender: TObject);
begin
    PSD_Tiff.Execute;
end;

procedure TformTiffInfo.Print1Click(Sender: TObject);
var
    screenDPI: integer;
    i, width: integer;
    name: string;
    widths: array of integer;
    names: TStringList;
    geotiffwidths: array of integer;
    geotiffnames: TStringList;
begin
    if ttInfo.sTiffName = '' then
        exit;
    if not PD_Tiff.Execute then
        exit;

    screenDPI := GetDeviceCaps(formTiffInfo.Canvas.Handle, LOGPIXELSY);

    names := TStringList.Create;
    setLength(widths, LV_TiffInfo.Columns.Count);
    for i := 0 to LV_TiffInfo.Columns.Count - 1 do
    begin
        names.Add(LV_TiffInfo.Column[i].Caption);
        widths[i] := LV_TiffInfo.Column[i].width;
    end;
    geotiffnames := TStringList.Create;
    setLength(geotiffwidths, LV_GeoTiff.Columns.Count);
    for i := 0 to LV_GeoTiff.Columns.Count - 1 do
    begin
        geotiffnames.Add(LV_GeoTiff.Column[i].Caption);
        geotiffwidths[i] := LV_GeoTiff.Column[i].width;
    end;

    infoPrinter.printTiffInfo(screenDPI, formTiffInfo.Canvas.Font.Size, names,
      widths, geotiffnames, geotiffwidths);

    // cleanup
    names.Free;
    geotiffnames.Free;
    widths := nil;
    geotiffwidths := nil;
end;

procedure TformTiffInfo.About1Click(Sender: TObject);
begin
    AboutBox.ShowModal;
end;

{
  Check all dropped files for .TIF files, other files are skipped. The
  information in the pages is updated to display info for the first .TIF
  file. All .TIF files will be put into a listbox for easy access.
}
procedure TformTiffInfo.RecordDragDrop(Drop: THandle; Min: Boolean);
var
    sExt: string;
    i, iCount, iLast: integer;
    img, sub, j, k: integer;
    acFileNames: array [0 .. 255] of char;
    fn: string;
    // ts : TMultiTiff;
    // pts : ^TMultiTiffLoc;
    // mtl : TMultiTiffLoc;
    // ifd : TTiffIFDList;
    // fn_node : TTreeNode;
    // img_node,
    // sub_node : TTreeNode;
begin
    iCount := DragQueryFile(Drop, $FFFFFFFF, acFileNames, 250);
    i := 0;
    iLast := LB_TiffFiles.Items.Count - 1;
    repeat
        DragQueryFile(Drop, i, acFileNames, 250);
        sExt := uppercase(ExtractFileExt(strpas(acFileNames)));
        i := i + 1;
        if (sExt = uppercase('.tif')) or (sExt = uppercase('.tiff')) then
        begin
            fn := MakeListItem(acFileNames);
            LB_TiffFiles.Items.Add(fn);
            inc(iLast);

            // fn_node := TV_TiffFiles.Items.Add(nil, fn);
            // ifd := TTiffIFDList.Create(acFileNames);
            // ts := ifd.getStructure;
            // img := -1;
            // sub := -1;
            // j := 0;
            // while j < ts.count do begin
            // mtl := ts.all[j];
            // if mtl.imgID <> img then begin // main images
            // img_node := TV_TiffFiles.items.addChild(fn_node, 'Image ' + IntToStr(mtl.imgID + 1));
            // img := mtl.imgID;
            // k := j;
            // while (k < ts.count) and (ts.all[k].imgID = img) do inc(k);
            // if k <> j then begin
            // while j < k do begin
            // sub_node := TV_TiffFiles.items.addChild(img_node, 'Subimage ' + IntToStr(mtl.subIFD + 1));
            // inc(j);
            // mtl := ts.all[j];
            // end;
            // end;
            // end;
            // end;
            // fn_node.Expand(false);

        end;
    until i = iCount;
    DragFinish(Drop);
    LB_TiffFiles.ItemIndex := iLast; // select the first of the new additions
    if iLast >= 0 then
        LB_TiffFilesClick(LB_TiffFiles);
end;

procedure TformTiffInfo.WMDropFiles(VAR Message: TWMDropFiles);
{ Called only if TApplication is NOT receiving drag/drop }
begin
    RecordDragDrop(Message.Drop, false);
    Message.Result := 0;
end;

procedure TformTiffInfo.FormDestroy(Sender: TObject);
begin
    DragAcceptFiles(Handle, false);
end;

procedure TformTiffInfo.DG_PaletteDrawCell(Sender: TObject; Col, Row: integer;
  Rect: TRect; State: TGridDrawState);
var
    tip: TImageProperties;
begin
    ttInfo.GetImageProperties(tip);
    with DG_Palette.Canvas do
    begin
        Brush.Color := TColor(tip.palColors[Row * DG_Palette.ColCount + Col]);
        FillRect(Rect);
    end;
end;

procedure TformTiffInfo.FormResize(Sender: TObject);
begin
    with DG_Palette do
    begin
        DefaultColWidth := (width - ColCount - 1) div ColCount;
        DefaultRowHeight := (Height - RowCount - 1) div RowCount;
    end;
end;

function TformTiffInfo.MakeListItem(pc: PChar): string;
var
    s, sFile, sPath: string;
begin
    s := strpas(pc);
    sFile := ExtractFileName(s);
    sPath := ExtractFilePath(s);
    Result := AnsiLowerCase(sFile + ' (' + sPath + ')');
end;

function TformTiffInfo.MakeFileFromItem(sItem: string): string;
var
    sFile, sPath: string;
    i: integer;
begin
    i := Pos('(', sItem);
    sFile := copy(sItem, 1, i - 2);
    sPath := copy(sItem, i + 1, length(sItem) - i - 1);
    Result := ExpandFileName(sPath + sFile);
end;

procedure TformTiffInfo.LB_TiffFilesClick(Sender: TObject);
var
    lb: TListBox;
    obj: TObject;
    name: string;
    myint: integer;
begin
    lb := Sender as TListBox;
    if lb.ItemIndex = -1 then
        exit;

    with LB_TiffFiles do
        ttInfo.SetTiffName(MakeFileFromItem(Items[ItemIndex]));
    ExtractInfo;
end;

procedure TformTiffInfo.LB_TiffFilesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    if TListBox(Sender).ItemAtPos(Point(X, Y), true) = -1 then
        // 'whitespace' was clicked
        LB_TiffFiles.ItemIndex := -1;
end;

procedure TformTiffInfo.PMI_ClearListClick(Sender: TObject);
begin
    LB_TiffFiles.Items.Clear;
    L_TiffName.Caption := '';
    LV_TiffInfo.Items.Clear;
    LB_ImageDetails.Items.Clear;
    LV_GeoTiff.Items.Clear;
    LB_GeoTiff.Items.Clear;
    PC_TiffInfo.ActivePage := TS_TiffIFD;
    TS_Palette.TabVisible := false;
    TS_GeoTiff.TabVisible := false;
end;

procedure TformTiffInfo.LV_GeoTiffDblClick(Sender: TObject);
var
    li: TListItem;
begin
    if LV_GeoTiff.SelCount > 0 then
    begin
        li := LV_GeoTiff.Selected;
        with F_GeoTiffKeyDetail do
        begin
            M_GeoKeyValues.Lines.Clear;
            M_GeoKeyDescription.Lines.Clear;
            E_GeoKeyTag.Text := li.Caption;
            E_GeoKeyProperty.Text := li.SubItems[0];
            E_GeoKeyType.Text := li.SubItems[1];
            E_GeoKeyCount.Text := li.SubItems[2];
            M_GeoKeyValues.Lines.Add(li.SubItems[3]);
            M_GeoKeyDescription.Lines.Add(li.SubItems[4]);
        end;
        F_GeoTiffKeyDetail.ShowModal;
    end;
end;

procedure TformTiffInfo.LV_TiffInfoDblClick(Sender: TObject);
var
    li: TListItem;
    addr_val: int64;
    length: int64;
    bytes: TDoubleArray;
begin
    // popup window for text fields
    if LV_TiffInfo.SelCount > 0 then
        with F_Detail do
        begin
            li := LV_TiffInfo.Selected;
            if li.SubItems[1] = 'Char (7-bit)' then
            begin
                M_Detail.Lines.Clear;
                M_Detail.Lines.Add('Reading...');
                Visible := true;
                E_DetailTag.Text := li.Caption;
                addr_val := StrToInt64(li.SubItems[3]);
                length := StrToInt64(li.SubItems[2]);
                M_Detail.Lines.Text := ttInfo.readText(addr_val, length);
            end
            else if li.SubItems[1] = 'Double (8 bytes IEEE)' then
            begin
                addr_val := StrToInt64(li.SubItems[3]);
                length := StrToInt64(li.SubItems[2]);
                bytes := ttInfo.readDoubles(addr_val, length);
                M_Detail.Text := hexconvert.DoublesToHex(bytes);
                Visible := true;
            end;
        end;
end;

procedure TformTiffInfo.Save1Click(Sender: TObject);
var
    slReport: TStringList;
    sItem: string;
    i, j: integer;
begin
    slReport := TStringList.Create;
    if ttInfo.sTiffName = '' then
        exit;

    slReport.Add('Report for ' + ttInfo.sTiffName + ' generated: ' +
      FormatDateTime('ddd, d mmm yyyy, t', Now));
    slReport.Add('');

    slReport.Add('IFD summary');
    slReport.Add('-----------');

    sItem := '';
    with LV_TiffInfo.Columns do
        for i := 0 to Count - 1 do
        begin
            sItem := sItem + Items[i].Caption + #9;
        end;
    slReport.Add(sItem);

    for i := 0 to ttInfo.iNumberIFD - 1 do
    begin
        with LV_TiffInfo.Items[i] do
        begin
            sItem := Caption;
            for j := 0 to SubItems.Count - 1 do
            begin
                sItem := sItem + #9 + SubItems[j];
            end;
        end;
        slReport.Add(sItem);
    end;

    slReport.Add('');
    slReport.Add('Image info');
    slReport.Add('----------');

    { Print Image Info }
    with LB_ImageDetails do
        for i := 0 to Items.Count - 1 do
        begin
            slReport.Add(Items.Strings[i]);
        end;

    if ttInfo.fHasGeoTiff then
    begin
        slReport.Add('');
        slReport.Add('GeoTiff Tag summary');
        slReport.Add('-------------------');

        { Print GeoTiff Tag Info }
        sItem := '';
        with LV_GeoTiff.Columns do
            for i := 0 to Count - 1 do
            begin
                sItem := sItem + Items[i].Caption + #9;
            end;
        slReport.Add(sItem);

        for i := 0 to ttInfo.iNrGeoKeys - 1 do
        begin
            with LV_GeoTiff.Items[i] do
            begin
                sItem := Caption;
                for j := 0 to SubItems.Count - 1 do
                begin
                    sItem := sItem + #9 + SubItems[j];
                end;
            end;
            slReport.Add(sItem);
        end;

        slReport.Add('');
        slReport.Add('GeoTiff details');
        slReport.Add('---------------');

        { Print GeoTiff summary info }
        with LB_GeoTiff do
            for i := 0 to Items.Count - 1 do
            begin
                slReport.Add(Items.Strings[i]);
            end;
    end;
    slReport.SaveToFile(m_fnReportName);
    slReport.Free;
end;

procedure TformTiffInfo.SetNewReportName(fn: TFileName);
begin
    m_fnReportName := fn;
end;

procedure TformTiffInfo.Setreportfilename1Click(Sender: TObject);
var
    form: TF_Options;
begin
    form := TF_Options.Create(self);
    try
        if m_fnReportName <> '' then
            form.FileName := m_fnReportName;
        if form.ShowModal = mrOK then
            m_fnReportName := form.FileName;
    finally
        form.Free;
    end;
end;

procedure TformTiffInfo.Structure1Click(Sender: TObject);
var
    mt: TMultiTiff;
    ts: TStringList;
    i: integer;
begin
    mt := ttInfo.getStructure;

    ts := TStringList.Create;
    for i := 0 to mt.Count - 1 do
    begin
        ts.Add(IntToStr(mt.all[i].imgID) + ' ' + IntToStr(mt.all[i].subIFD) +
          ' ' + IntToStr(mt.all[i].IFDoffset));
    end;
    Application.MessageBox(ts.GetText, 'Structure', IDOK);
end;

end.
