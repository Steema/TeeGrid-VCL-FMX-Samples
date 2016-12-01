object FormGridThemes: TFormGridThemes
  Left = 0
  Top = 0
  Caption = 'TeeGrid Themes'
  ClientHeight = 684
  ClientWidth = 923
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TeeGrid1: TTeeGrid
    Left = 73
    Top = 41
    Width = 850
    Height = 643
    Columns = <>
    Footer = <>
    ReadOnly = False
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
  end
  object LBTheme: TListBox
    Left = 0
    Top = 41
    Width = 73
    Height = 643
    Align = alLeft
    ItemHeight = 13
    Items.Strings = (
      'Default'
      'iOS'
      'Android'
      'Black')
    TabOrder = 1
    OnClick = LBThemeClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 923
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 8
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Edit...'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
