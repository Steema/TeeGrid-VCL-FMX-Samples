object FormMatrixGrid: TFormMatrixGrid
  Left = 0
  Top = 0
  Caption = 'TeeGrid Matrix Example'
  ClientHeight = 769
  ClientWidth = 1052
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TeeGrid1: TTeeGrid
    Left = 0
    Top = 52
    Width = 1052
    Height = 717
    Columns = <>
    ReadOnly = False
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    _Headers = (
      1
      'TColumnHeaderBand'
      <
        item
        end>)
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1052
    Height = 52
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object RGMode: TRadioGroup
      Left = 16
      Top = 11
      Width = 265
      Height = 36
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Virtual Mode'
        'Rtti')
      TabOrder = 0
      OnClick = RGModeClick
    end
    object Button1: TButton
      Left = 344
      Top = 14
      Width = 75
      Height = 25
      Caption = '&Edit...'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
