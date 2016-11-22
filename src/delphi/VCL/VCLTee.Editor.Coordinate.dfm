object CoordinateEditor: TCoordinateEditor
  Left = 0
  Top = 0
  Caption = 'Coordinate Editor'
  ClientHeight = 201
  ClientWidth = 155
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 16
    Top = 47
    Width = 30
    Height = 13
    Caption = '&Value:'
    FocusControl = EValue
  end
  object CBAuto: TCheckBox
    Left = 16
    Top = 16
    Width = 97
    Height = 17
    Caption = '&Automatic'
    TabOrder = 0
    OnClick = CBAutoClick
  end
  object UDValue: TUpDown
    Left = 84
    Top = 66
    Width = 16
    Height = 21
    Associate = EValue
    Max = 10000
    TabOrder = 1
  end
  object EValue: TEdit
    Left = 16
    Top = 66
    Width = 68
    Height = 21
    TabOrder = 2
    Text = '0'
    OnChange = EValueChange
  end
  object RGUnits: TRadioGroup
    Left = 16
    Top = 104
    Width = 105
    Height = 63
    Caption = '&Units:'
    ItemIndex = 0
    Items.Strings = (
      '&Pixels'
      'P&ercent')
    TabOrder = 3
    OnClick = RGUnitsClick
  end
end
