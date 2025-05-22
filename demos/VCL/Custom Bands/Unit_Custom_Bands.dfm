object CustomBandForm: TCustomBandForm
  Left = 0
  Top = 0
  Caption = 'TeeGrid - Custom Footer Band Example'
  ClientHeight = 469
  ClientWidth = 788
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 13
  object TeeGrid1: TTeeGrid
    Left = 425
    Top = 0
    Width = 363
    Height = 469
    Columns = <>
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
    Width = 425
    Height = 469
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
end
