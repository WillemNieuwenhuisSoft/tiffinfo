object formTiffInfo: TformTiffInfo
  Left = 400
  Top = 132
  Caption = 'Tiff Info Viewer'
  ClientHeight = 629
  ClientWidth = 1066
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mmTiffInfo
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  TextHeight = 13
  object Spl_TiffFiles: TSplitter
    Left = 273
    Top = 13
    Height = 616
    ExplicitLeft = 209
    ExplicitTop = 19
    ExplicitHeight = 535
  end
  object L_TiffName: TLabel
    Left = 0
    Top = 0
    Width = 1066
    Height = 13
    Align = alTop
    Caption = 'L_TiffName'
    Color = clRed
    ParentColor = False
    Transparent = False
    ExplicitWidth = 55
  end
  object PC_TiffInfo: TPageControl
    Left = 276
    Top = 13
    Width = 790
    Height = 616
    ActivePage = TS_TiffIFD
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 786
    ExplicitHeight = 615
    object TS_TiffIFD: TTabSheet
      Caption = 'IFD Details'
      object LV_TiffInfo: TListView
        Left = 0
        Top = 0
        Width = 782
        Height = 588
        Align = alClient
        Columns = <
          item
            Caption = 'Tag'
            Width = 60
          end
          item
            Caption = 'Property'
            Width = 160
          end
          item
            Caption = 'Type'
            Width = 120
          end
          item
            Caption = 'Count'
            Width = 60
          end
          item
            Caption = 'Value (dec)'
            Width = 100
          end
          item
            Caption = 'Value (hex)'
            Width = 100
          end>
        DragMode = dmAutomatic
        Items.ItemData = {
          051A0000000100000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
          0000}
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = LV_TiffInfoDblClick
        ExplicitLeft = -1
        ExplicitWidth = 778
        ExplicitHeight = 587
      end
    end
    object TS_TiffImage: TTabSheet
      Caption = 'Image Details'
      object LB_ImageDetails: TListBox
        Left = 0
        Top = 0
        Width = 782
        Height = 588
        Align = alClient
        DragMode = dmAutomatic
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TS_Palette: TTabSheet
      Caption = 'Palette'
      object DG_Palette: TDrawGrid
        Left = 0
        Top = 0
        Width = 782
        Height = 588
        Align = alClient
        ColCount = 16
        FixedCols = 0
        RowCount = 16
        FixedRows = 0
        ScrollBars = ssNone
        TabOrder = 0
        OnDrawCell = DG_PaletteDrawCell
      end
    end
    object TS_GeoTiff: TTabSheet
      Caption = 'GeoTiff Info'
      object Spl_GeoTiff: TSplitter
        Left = 0
        Top = 437
        Width = 782
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 293
        ExplicitWidth = 632
      end
      object LB_GeoTiff: TListBox
        Left = 0
        Top = 440
        Width = 782
        Height = 148
        Align = alBottom
        ItemHeight = 13
        TabOrder = 0
      end
      object LV_GeoTiff: TListView
        Left = 0
        Top = 0
        Width = 782
        Height = 437
        Hint = 'Double click on an item for details'
        Align = alClient
        Columns = <
          item
            Caption = 'Tag'
          end
          item
            Caption = 'Property'
            Width = 150
          end
          item
            Caption = 'Type'
            Width = 70
          end
          item
            Caption = 'Count'
          end
          item
            Caption = 'Value'
            Width = 60
          end
          item
            Caption = 'Description'
            Width = 150
          end>
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
        OnDblClick = LV_GeoTiffDblClick
      end
    end
    object TS_GeoTiffSummary: TTabSheet
      Caption = 'GeoTiff Key Summary'
      object M_GeoSummary: TMemo
        Left = 0
        Top = 0
        Width = 782
        Height = 588
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object LB_TiffFiles: TListBox
    Left = 0
    Top = 13
    Width = 273
    Height = 616
    Align = alLeft
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    PopupMenu = PM_TiffList
    TabOrder = 1
    OnClick = LB_TiffFilesClick
    OnMouseDown = LB_TiffFilesMouseDown
    ExplicitLeft = 1
    ExplicitTop = 8
  end
  object mmTiffInfo: TMainMenu
    Images = ImageList1
    Left = 368
    Top = 200
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open...'
        ImageIndex = 0
        ShortCut = 16463
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Sa&ve report'
        ShortCut = 16467
        OnClick = Save1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Print...'
        Hint = 'Print overview of current TIFF file details'
        ImageIndex = 1
        ShortCut = 16464
        OnClick = Print1Click
      end
      object Printsetup1: TMenuItem
        Caption = 'Printer &setup...'
        OnClick = Printsetup1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 32856
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Clearlist1: TMenuItem
        Caption = 'Clear list'
        ShortCut = 16430
        OnClick = PMI_ClearListClick
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object Setreportfilename1: TMenuItem
        Caption = 'Set Report Filename'
        OnClick = Setreportfilename1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About'
        OnClick = About1Click
      end
      object R1: TMenuItem
        Caption = 'Register'
        Visible = False
      end
    end
  end
  object OD_Tiff: TOpenDialog
    Filter = 'TIFF files|*.TIF'
    Options = [ofAllowMultiSelect, ofFileMustExist, ofShareAware]
    Left = 344
    Top = 120
  end
  object PSD_Tiff: TPrinterSetupDialog
    Left = 192
    Top = 200
  end
  object PD_Tiff: TPrintDialog
    Left = 292
    Top = 213
  end
  object PM_TiffList: TPopupMenu
    Left = 456
    Top = 141
    object PMI_ClearList: TMenuItem
      Caption = 'Clear list'
      ShortCut = 16430
      OnClick = PMI_ClearListClick
    end
    object Structure1: TMenuItem
      Caption = 'Structure'
      Visible = False
      OnClick = Structure1Click
    end
  end
  object ImageList1: TImageList
    Left = 584
    Top = 109
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400008484000084840000848400008484000084840000848400008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF000000
      0000008484000084840000848400008484000084840000848400008484000084
      8400008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFE00000000FFFFC00700000000
      001FBFEB00000000000F00050000000000077E310000000000037E3500000000
      000100060000000000007FEA00000000001F801400000000001FC00A00000000
      001FE001000000008FF1E00700000000FFF9F00700000000FF75F00300000000
      FF8FF80300000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
