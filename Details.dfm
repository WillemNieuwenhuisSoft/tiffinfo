object F_Detail: TF_Detail
  Left = 0
  Top = 0
  Caption = 'Details'
  ClientHeight = 624
  ClientWidth = 889
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 16
  object P_Detail: TPanel
    Left = 0
    Top = 41
    Width = 889
    Height = 542
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 885
    ExplicitHeight = 541
    object M_Detail: TMemo
      Left = 1
      Top = 1
      Width = 887
      Height = 540
      Align = alClient
      Lines.Strings = (
        '')
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitWidth = 883
      ExplicitHeight = 539
    end
  end
  object P_DetailTag: TPanel
    Left = 0
    Top = 0
    Width = 889
    Height = 41
    Align = alTop
    TabOrder = 2
    ExplicitWidth = 885
    DesignSize = (
      889
      41)
    object L_DetailTag: TLabel
      Left = 8
      Top = 8
      Width = 56
      Height = 21
      AutoSize = False
      Caption = 'Tag'
      Layout = tlCenter
    end
    object E_DetailTag: TEdit
      Left = 70
      Top = 8
      Width = 195
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Color = clInfoBk
      ReadOnly = True
      TabOrder = 0
      Text = 'E_DetailTag'
    end
  end
  object P_Button: TPanel
    Left = 0
    Top = 583
    Width = 889
    Height = 41
    Align = alBottom
    Caption = 'P_Button'
    ShowCaption = False
    TabOrder = 1
    ExplicitTop = 582
    ExplicitWidth = 885
    DesignSize = (
      889
      41)
    object BB_DetailClose: TBitBtn
      Left = 781
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Kind = bkClose
      NumGlyphs = 2
      Spacing = -1
      TabOrder = 0
      ExplicitLeft = 777
    end
  end
end
