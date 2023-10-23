object F_GeoTiffKeyDetail: TF_GeoTiffKeyDetail
  Left = 336
  Top = 279
  ActiveControl = BB_Close
  BorderStyle = bsDialog
  Caption = 'GeoTiff key detail'
  ClientHeight = 380
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    490
    380)
  TextHeight = 16
  object P_GeoKey: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 343
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 471
    ExplicitHeight = 342
    DesignSize = (
      490
      343)
    object L_GeoKeyTag: TLabel
      Left = 16
      Top = 24
      Width = 98
      Height = 21
      AutoSize = False
      Caption = 'GeoKey tag'
      Layout = tlCenter
    end
    object L_GeoKeyName: TLabel
      Left = 16
      Top = 56
      Width = 98
      Height = 21
      AutoSize = False
      Caption = 'Property'
      Layout = tlCenter
    end
    object L_GeoKeyType: TLabel
      Left = 16
      Top = 88
      Width = 98
      Height = 21
      AutoSize = False
      Caption = 'Type'
      Layout = tlCenter
    end
    object L_GeoKeyCount: TLabel
      Left = 16
      Top = 120
      Width = 98
      Height = 21
      AutoSize = False
      Caption = 'Count'
      Layout = tlCenter
    end
    object L_GeoKeyValue: TLabel
      Left = 16
      Top = 152
      Width = 98
      Height = 21
      AutoSize = False
      Caption = 'Value'
      Layout = tlCenter
    end
    object L_GeoKeyDescription: TLabel
      Left = 16
      Top = 248
      Width = 98
      Height = 21
      AutoSize = False
      Caption = 'Description'
      Layout = tlCenter
    end
    object E_GeoKeyTag: TEdit
      Left = 120
      Top = 23
      Width = 168
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 0
      Text = 'E_GeoKeyTag'
    end
    object E_GeoKeyProperty: TEdit
      Left = 120
      Top = 55
      Width = 240
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 1
      Text = 'E_GeoKeyProperty'
    end
    object E_GeoKeyType: TEdit
      Left = 120
      Top = 87
      Width = 144
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 2
      Text = 'E_GeoKeyType'
    end
    object E_GeoKeyCount: TEdit
      Left = 120
      Top = 119
      Width = 144
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 3
      Text = 'E_GeoKeyCount'
    end
    object M_GeoKeyValues: TMemo
      Left = 120
      Top = 151
      Width = 352
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      Lines.Strings = (
        'M_GeoKeyValues')
      ReadOnly = True
      TabOrder = 4
      WordWrap = False
    end
    object M_GeoKeyDescription: TMemo
      Left = 120
      Top = 247
      Width = 352
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      Lines.Strings = (
        'M_GeoKeyDescription')
      ReadOnly = True
      TabOrder = 5
    end
  end
  object BB_Close: TBitBtn
    Left = 397
    Top = 349
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkClose
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
  end
end
