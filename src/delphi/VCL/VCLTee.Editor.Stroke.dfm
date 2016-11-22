object StrokeEditor: TStrokeEditor
  Left = 0
  Top = 0
  Caption = 'Stroke Editor'
  ClientHeight = 314
  ClientWidth = 285
  Color = clWhite
  ParentFont = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageStroke: TPageControl
    Left = 0
    Top = 0
    Width = 285
    Height = 314
    ActivePage = TabPen
    Align = alClient
    TabOrder = 0
    OnChange = PageStrokeChange
    object TabPen: TTabSheet
      Caption = 'Options'
      ExplicitLeft = 0
      ExplicitTop = 28
      object Label3: TLabel
        Left = 8
        Top = 29
        Width = 23
        Height = 13
        Caption = '&Size:'
        FocusControl = TBStrokeSize
      end
      object Label4: TLabel
        Left = 8
        Top = 85
        Width = 28
        Height = 13
        Caption = 'S&tyle:'
        FocusControl = LBStrokeStyle
      end
      object LStrokeSize: TLabel
        Left = 160
        Top = 48
        Width = 6
        Height = 13
        Caption = '0'
      end
      object Label1: TLabel
        Left = 144
        Top = 85
        Width = 49
        Height = 13
        Caption = '&End Style:'
        FocusControl = LBEndStyle
      end
      object Label2: TLabel
        Left = 144
        Top = 165
        Width = 50
        Height = 13
        Caption = '&Join Style:'
        FocusControl = LBJoinStyle
      end
      object CBStrokeVisible: TCheckBox
        Left = 8
        Top = 6
        Width = 145
        Height = 17
        Caption = '&Visible'
        TabOrder = 0
        OnClick = CBStrokeVisibleClick
      end
      object TBStrokeSize: TTrackBar
        Left = 3
        Top = 48
        Width = 150
        Height = 21
        Max = 50
        Frequency = 5
        Position = 10
        TabOrder = 1
        ThumbLength = 12
        OnChange = TBStrokeSizeChange
      end
      object CBStrokeColor: TColorBox
        Left = 103
        Top = 4
        Width = 90
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 2
        OnChange = CBStrokeColorChange
      end
      object LBStrokeStyle: TListBox
        Left = 8
        Top = 104
        Width = 121
        Height = 89
        ItemHeight = 13
        Items.Strings = (
          'Solid'
          'Dash'
          'Dot'
          'Dash Dot'
          'Dash Dot Dot'
          'Custom')
        TabOrder = 3
        OnClick = LBStrokeStyleClick
      end
      object LBEndStyle: TListBox
        Left = 144
        Top = 104
        Width = 73
        Height = 49
        ItemHeight = 13
        Items.Strings = (
          'Round'
          'Square'
          'Flat')
        TabOrder = 4
        OnClick = LBEndStyleClick
      end
      object LBJoinStyle: TListBox
        Left = 144
        Top = 184
        Width = 73
        Height = 49
        ItemHeight = 13
        Items.Strings = (
          'Round'
          'Bevel'
          'Mitter')
        TabOrder = 5
        OnClick = LBJoinStyleClick
      end
    end
    object TabBrush: TTabSheet
      Caption = 'Fill'
      ImageIndex = 1
    end
  end
end
