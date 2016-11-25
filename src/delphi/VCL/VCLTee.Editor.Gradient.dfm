object GradientEditor: TGradientEditor
  Left = 0
  Top = 0
  Caption = 'Gradient Editor'
  ClientHeight = 290
  ClientWidth = 303
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
  object PageGradient: TPageControl
    Left = 0
    Top = 0
    Width = 303
    Height = 290
    ActivePage = TabDirection
    Align = alClient
    TabOrder = 0
    object TabColors: TTabSheet
      Caption = 'Colors'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 12
        Top = 67
        Width = 41
        Height = 13
        Caption = '&Opacity:'
      end
      object LOpacity0: TLabel
        Left = 155
        Top = 86
        Width = 29
        Height = 13
        Caption = '100%'
      end
      object Label2: TLabel
        Left = 12
        Top = 157
        Width = 41
        Height = 13
        Caption = 'Op&acity:'
      end
      object LOpacity1: TLabel
        Left = 155
        Top = 176
        Width = 29
        Height = 13
        Caption = '100%'
      end
      object CBVisible: TCheckBox
        Left = 12
        Top = 11
        Width = 97
        Height = 17
        Caption = '&Visible'
        TabOrder = 0
        OnClick = CBVisibleClick
      end
      object CBColor0: TColorBox
        Left = 12
        Top = 40
        Width = 90
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 1
        OnChange = CBColor0Change
      end
      object CBColor1: TColorBox
        Left = 12
        Top = 130
        Width = 90
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 2
        OnChange = CBColor1Change
      end
      object TBOpacity0: TTrackBar
        Left = 3
        Top = 86
        Width = 150
        Height = 30
        Max = 100
        Frequency = 5
        Position = 100
        TabOrder = 3
        ThumbLength = 14
        OnChange = TBOpacity0Change
      end
      object TBOpacity1: TTrackBar
        Left = 3
        Top = 176
        Width = 150
        Height = 30
        Max = 100
        Frequency = 5
        Position = 100
        TabOrder = 4
        ThumbLength = 14
        OnChange = TBOpacity1Change
      end
    end
    object TabDirection: TTabSheet
      Caption = 'Direction'
      ImageIndex = 1
      object RGDirection: TRadioGroup
        Left = 16
        Top = 18
        Width = 113
        Height = 121
        Caption = '&Direction'
        Items.Strings = (
          'Vertical'
          'Horizontal'
          'Diagonal'
          'Back Diagonal'
          'Radial')
        TabOrder = 0
        OnClick = RGDirectionClick
      end
      object CBInverted: TCheckBox
        Left = 16
        Top = 155
        Width = 97
        Height = 17
        Caption = '&Inverted'
        TabOrder = 1
        OnClick = CBInvertedClick
      end
    end
  end
end
