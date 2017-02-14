object FontEditor: TFontEditor
  Left = 0
  Top = 0
  Caption = 'Font Editor'
  ClientHeight = 298
  ClientWidth = 310
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
  object PageFont: TPageControl
    Left = 0
    Top = 0
    Width = 310
    Height = 298
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 0
    OnChange = PageFontChange
    object TabOptions: TTabSheet
      Caption = 'Options'
      object Label2: TLabel
        Left = 8
        Top = 7
        Width = 23
        Height = 13
        Caption = '&Size:'
        FocusControl = TBSize
      end
      object Label1: TLabel
        Left = 8
        Top = 57
        Width = 31
        Height = 13
        Caption = '&Name:'
      end
      object LSize: TLabel
        Left = 159
        Top = 26
        Width = 6
        Height = 13
        Caption = '0'
      end
      object TBSize: TTrackBar
        Left = 3
        Top = 26
        Width = 150
        Height = 21
        LineSize = 10
        Max = 500
        Frequency = 50
        Position = 100
        TabOrder = 0
        ThumbLength = 12
        OnChange = TBSizeChange
      end
      object LBName: TListBox
        Left = 8
        Top = 76
        Width = 137
        Height = 101
        ItemHeight = 13
        TabOrder = 1
        OnClick = LBNameClick
      end
      object GroupBox1: TGroupBox
        Left = 162
        Top = 57
        Width = 121
        Height = 120
        Caption = '&Style:'
        TabOrder = 2
        object CBBold: TCheckBox
          Left = 10
          Top = 21
          Width = 104
          Height = 17
          Caption = '&Bold'
          TabOrder = 0
          OnClick = CBBoldClick
        end
        object CBItalic: TCheckBox
          Left = 10
          Top = 44
          Width = 104
          Height = 17
          Caption = '&Italic'
          TabOrder = 1
          OnClick = CBItalicClick
        end
        object CBUnderline: TCheckBox
          Left = 10
          Top = 68
          Width = 104
          Height = 17
          Caption = '&Underline'
          TabOrder = 2
          OnClick = CBUnderlineClick
        end
        object CBStrikeOut: TCheckBox
          Left = 10
          Top = 91
          Width = 104
          Height = 17
          Caption = 'Strike &Out'
          TabOrder = 3
          OnClick = CBStrikeOutClick
        end
      end
    end
    object TabFill: TTabSheet
      Caption = 'Fill'
      ImageIndex = 1
    end
  end
end
