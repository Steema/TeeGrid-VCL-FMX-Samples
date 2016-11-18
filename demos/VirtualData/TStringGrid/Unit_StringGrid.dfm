object StringGridForm: TStringGridForm
  Left = 0
  Top = 0
  Caption = 'StringGrid Example'
  ClientHeight = 492
  ClientWidth = 703
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
    Left = 0
    Top = 0
    Width = 703
    Height = 451
    Columns = <>
    Header.OnClick = TeeGrid1ClickedHeader
    Footer = <>
    ReadOnly = False
    OnClickedHeader = TeeGrid1ClickedHeader
    OnSelect = TeeGrid1Select
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 451
    Width = 703
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
end
