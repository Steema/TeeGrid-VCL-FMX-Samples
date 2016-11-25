object GridBandsEditor: TGridBandsEditor
  Left = 0
  Top = 0
  Caption = 'Bands Editor'
  ClientHeight = 413
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object SBUp: TSpeedButton
      Left = 8
      Top = 13
      Width = 23
      Height = 22
      Caption = '^'
      Enabled = False
      Flat = True
      OnClick = SBUpClick
    end
    object SBDown: TSpeedButton
      Left = 37
      Top = 13
      Width = 23
      Height = 22
      Caption = 'v'
      Enabled = False
      Flat = True
      OnClick = SBDownClick
    end
    object CBVisible: TCheckBox
      Left = 72
      Top = 13
      Width = 97
      Height = 17
      Caption = 'Visible'
      TabOrder = 0
      OnClick = CBVisibleClick
    end
  end
  object LBBands: TListBox
    Left = 0
    Top = 41
    Width = 121
    Height = 372
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = LBBandsClick
  end
  object PanelMain: TPanel
    Left = 121
    Top = 41
    Width = 326
    Height = 372
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 136
    ExplicitTop = 208
    ExplicitWidth = 185
    ExplicitHeight = 41
    object PanelBandTop: TPanel
      Left = 0
      Top = 0
      Width = 326
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 72
      ExplicitTop = 168
      ExplicitWidth = 185
      object CBBandVisible: TCheckBox
        Left = 16
        Top = 16
        Width = 97
        Height = 17
        Caption = '&Visible'
        TabOrder = 0
        OnClick = CBBandVisibleClick
      end
    end
  end
end
