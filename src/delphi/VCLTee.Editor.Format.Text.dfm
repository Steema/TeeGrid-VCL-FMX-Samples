object TextFormatEditor: TTextFormatEditor
  Left = 0
  Top = 0
  ActiveControl = TBFontSize
  Caption = 'Format Editor'
  ClientHeight = 296
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageFormat: TPageControl
    Left = 0
    Top = 0
    Width = 334
    Height = 296
    ActivePage = TabFont
    Align = alClient
    TabOrder = 0
    OnChange = PageFormatChange
    object TabFont: TTabSheet
      Caption = 'Font'
      object Label2: TLabel
        Left = 8
        Top = 7
        Width = 23
        Height = 13
        Caption = '&Size:'
        FocusControl = TBFontSize
      end
      object Label1: TLabel
        Left = 8
        Top = 57
        Width = 31
        Height = 13
        Caption = '&Name:'
      end
      object LFontSize: TLabel
        Left = 159
        Top = 26
        Width = 6
        Height = 13
        Caption = '0'
      end
      object TBFontSize: TTrackBar
        Left = 3
        Top = 26
        Width = 150
        Height = 21
        Max = 50
        Frequency = 5
        Position = 10
        TabOrder = 0
        ThumbLength = 12
        OnChange = TBFontSizeChange
      end
      object LBFontName: TListBox
        Left = 8
        Top = 76
        Width = 137
        Height = 174
        ItemHeight = 13
        TabOrder = 1
        OnClick = LBFontNameClick
      end
      object CBFontColor: TColorBox
        Left = 168
        Top = 216
        Width = 90
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 2
        OnChange = CBFontColorChange
      end
      object GroupBox1: TGroupBox
        Left = 168
        Top = 57
        Width = 121
        Height = 145
        Caption = '&Style:'
        TabOrder = 3
        object CBFontBold: TCheckBox
          Left = 10
          Top = 21
          Width = 104
          Height = 17
          Caption = '&Bold'
          TabOrder = 0
          OnClick = CBFontBoldClick
        end
        object CBFontItalic: TCheckBox
          Left = 10
          Top = 44
          Width = 104
          Height = 17
          Caption = '&Italic'
          TabOrder = 1
          OnClick = CBFontItalicClick
        end
        object CBFontUnderline: TCheckBox
          Left = 10
          Top = 68
          Width = 104
          Height = 17
          Caption = '&Underline'
          TabOrder = 2
          OnClick = CBFontUnderlineClick
        end
        object CBFontStrikeOut: TCheckBox
          Left = 10
          Top = 91
          Width = 104
          Height = 17
          Caption = 'Strike &Out'
          TabOrder = 3
          OnClick = CBFontStrikeOutClick
        end
      end
    end
    object TabStroke: TTabSheet
      Caption = 'Stroke'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabBrush: TTabSheet
      Caption = 'Fill'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
