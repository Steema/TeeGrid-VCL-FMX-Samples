object BrushEditor: TBrushEditor
  Left = 0
  Top = 0
  Caption = 'Brush Editor'
  ClientHeight = 267
  ClientWidth = 285
  Color = clWhite
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PageBrush: TPageControl
    Left = 0
    Top = 41
    Width = 285
    Height = 226
    ActivePage = TabSolid
    Align = alClient
    TabOrder = 0
    OnChange = PageBrushChange
    ExplicitLeft = 8
    ExplicitTop = 88
    ExplicitWidth = 289
    ExplicitHeight = 193
    object TabSolid: TTabSheet
      Caption = 'Color'
      ExplicitWidth = 281
      ExplicitHeight = 165
      object CBColor: TColorBox
        Left = 11
        Top = 16
        Width = 90
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 0
        OnChange = CBColorChange
      end
    end
    object TabGradient: TTabSheet
      Caption = 'Gradient'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
    end
    object TabPicture: TTabSheet
      Caption = 'Picture'
      ImageIndex = 2
      ExplicitWidth = 281
      ExplicitHeight = 165
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 285
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 176
    ExplicitTop = 232
    ExplicitWidth = 185
    object CBVisible: TCheckBox
      Left = 16
      Top = 14
      Width = 97
      Height = 17
      Caption = '&Visible'
      TabOrder = 0
      OnClick = CBVisibleClick
    end
  end
end
