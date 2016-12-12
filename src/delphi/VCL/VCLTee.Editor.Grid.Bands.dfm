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
      Left = 69
      Top = 13
      Width = 23
      Height = 22
      Caption = '^'
      Enabled = False
      Flat = True
      OnClick = SBUpClick
    end
    object SBDown: TSpeedButton
      Left = 97
      Top = 13
      Width = 23
      Height = 22
      Caption = 'v'
      Enabled = False
      Flat = True
      OnClick = SBDownClick
    end
    object SBAdd: TSpeedButton
      Left = 8
      Top = 13
      Width = 23
      Height = 22
      Caption = '+'
      Flat = True
      OnClick = SBAddClick
    end
    object SBRemove: TSpeedButton
      Left = 35
      Top = 13
      Width = 23
      Height = 22
      Caption = '-'
      Enabled = False
      Flat = True
      OnClick = SBRemoveClick
    end
    object CBVisible: TCheckBox
      Left = 130
      Top = 13
      Width = 97
      Height = 17
      Caption = 'Visible'
      TabOrder = 0
      OnClick = CBVisibleClick
    end
  end
  object LBBands: TCheckListBox
    Left = 0
    Top = 41
    Width = 121
    Height = 372
    OnClickCheck = LBBandsClickCheck
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
  end
  object PopupMenu1: TPopupMenu
    Left = 216
    Top = 216
    object ext1: TMenuItem
      Caption = '&Text'
      OnClick = ext1Click
    end
    object Header1: TMenuItem
      Caption = '&Header'
      OnClick = Header1Click
    end
    object otals1: TMenuItem
      Caption = 'T&otals'
      OnClick = otals1Click
    end
    object otalsHeader1: TMenuItem
      Caption = 'Totals H&eader'
      OnClick = otalsHeader1Click
    end
  end
end
