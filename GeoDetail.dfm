object F_GeoTiffKeyDetail: TF_GeoTiffKeyDetail
  Left = 336
  Top = 279
  ActiveControl = BB_Close
  BorderStyle = bsDialog
  Caption = 'GeoTiff key detail'
  ClientHeight = 380
  ClientWidth = 475
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  DesignSize = (
    475
    380)
  PixelsPerInch = 96
  TextHeight = 13
  object P_GeoKey: TPanel
    Left = 0
    Top = 0
    Width = 475
    Height = 343
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 432
    ExplicitHeight = 345
    DesignSize = (
      475
      343)
    object L_GeoKeyTag: TLabel
      Left = 16
      Top = 24
      Width = 56
      Height = 21
      AutoSize = False
      Caption = 'GeoKey tag'
      Layout = tlCenter
    end
    object L_GeoKeyName: TLabel
      Left = 16
      Top = 56
      Width = 39
      Height = 21
      AutoSize = False
      Caption = 'Property'
      Layout = tlCenter
    end
    object L_GeoKeyType: TLabel
      Left = 16
      Top = 88
      Width = 24
      Height = 21
      AutoSize = False
      Caption = 'Type'
      Layout = tlCenter
    end
    object L_GeoKeyCount: TLabel
      Left = 16
      Top = 120
      Width = 78
      Height = 21
      AutoSize = False
      Caption = 'Count'
      Layout = tlCenter
    end
    object L_GeoKeyValue: TLabel
      Left = 16
      Top = 152
      Width = 27
      Height = 21
      AutoSize = False
      Caption = 'Value'
      Layout = tlCenter
    end
    object L_GeoKeyDescription: TLabel
      Left = 16
      Top = 248
      Width = 53
      Height = 21
      AutoSize = False
      Caption = 'Description'
      Layout = tlCenter
    end
    object E_GeoKeyTag: TEdit
      Left = 120
      Top = 24
      Width = 157
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 0
      Text = 'E_GeoKeyTag'
      ExplicitWidth = 121
    end
    object E_GeoKeyProperty: TEdit
      Left = 120
      Top = 56
      Width = 229
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 1
      Text = 'E_GeoKeyProperty'
      ExplicitWidth = 193
    end
    object E_GeoKeyType: TEdit
      Left = 120
      Top = 88
      Width = 133
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 2
      Text = 'E_GeoKeyType'
      ExplicitWidth = 97
    end
    object E_GeoKeyCount: TEdit
      Left = 120
      Top = 120
      Width = 133
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 3
      Text = 'E_GeoKeyCount'
      ExplicitWidth = 97
    end
    object M_GeoKeyValues: TMemo
      Left = 120
      Top = 152
      Width = 341
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      Lines.Strings = (
        'M_GeoKeyValues')
      ReadOnly = True
      TabOrder = 4
      WordWrap = False
      ExplicitWidth = 305
    end
    object M_GeoKeyDescription: TMemo
      Left = 120
      Top = 248
      Width = 341
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      Lines.Strings = (
        'M_GeoKeyDescription')
      ReadOnly = True
      TabOrder = 5
      ExplicitWidth = 305
    end
  end
  object BB_Close: TBitBtn
    Left = 379
    Top = 349
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkClose
    NumGlyphs = 2
    Spacing = -1
    TabOrder = 1
    ExplicitLeft = 336
    ExplicitTop = 352
  end
end
