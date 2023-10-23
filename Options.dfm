object F_Options: TF_Options
  Left = 615
  Top = 369
  BorderStyle = bsDialog
  Caption = 'TiffInfo Options'
  ClientHeight = 102
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  OnActivate = FormActivate
  TextHeight = 16
  object P_Options: TPanel
    Left = 0
    Top = 0
    Width = 398
    Height = 65
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 394
    object L_ReportFile: TLabel
      Left = 8
      Top = 8
      Width = 63
      Height = 16
      Caption = 'ReportFile'
    end
    object E_Reportfile: TEdit
      Left = 22
      Top = 30
      Width = 305
      Height = 24
      TabOrder = 0
      Text = 'E_Reportfile'
    end
    object BB_Browse: TBitBtn
      Left = 326
      Top = 30
      Width = 25
      Height = 21
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'System'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = BB_BrowseClick
    end
  end
  object BB_Cancel: TBitBtn
    Left = 304
    Top = 72
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 1
  end
  object BB_OK: TBitBtn
    Left = 200
    Top = 72
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
    OnClick = BB_OKClick
  end
  object OD_Browse: TOpenDialog
    Left = 136
    Top = 72
  end
end
