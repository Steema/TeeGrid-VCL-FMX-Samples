object StrokeEditor: TStrokeEditor
  Left = 0
  Top = 0
  ActiveControl = CBVisible
  Caption = 'Stroke Editor'
  ClientHeight = 314
  ClientWidth = 285
  Color = clWhite
  ParentFont = True
  OldCreateOrder = False
  OnCreate = FormCreate
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
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label3: TLabel
        Left = 8
        Top = 77
        Width = 23
        Height = 13
        Caption = '&Size:'
        FocusControl = TBSize
      end
      object LSize: TLabel
        Left = 160
        Top = 96
        Width = 6
        Height = 13
        Caption = '0'
      end
      object CBVisible: TCheckBox
        Left = 8
        Top = 6
        Width = 73
        Height = 17
        Caption = '&Visible'
        TabOrder = 0
        OnClick = CBVisibleClick
      end
      object TBSize: TTrackBar
        Left = 3
        Top = 96
        Width = 150
        Height = 21
        Max = 50
        Frequency = 5
        Position = 10
        TabOrder = 1
        ThumbLength = 12
        OnChange = TBSizeChange
      end
      object CBColor: TColorBox
        Left = 8
        Top = 36
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbIncludeDefault, cbCustomColor, cbPrettyNames, cbCustomColors]
        TabOrder = 2
        OnChange = CBColorChange
      end
    end
    object TabBrush: TTabSheet
      Caption = 'Fill'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabStyle: TTabSheet
      Caption = 'Style'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label4: TLabel
        Left = 9
        Top = 5
        Width = 23
        Height = 13
        Caption = '&Line:'
        FocusControl = LBStyle
      end
      object Label1: TLabel
        Left = 144
        Top = 5
        Width = 22
        Height = 13
        Caption = '&End:'
        FocusControl = LBEndStyle
      end
      object Label2: TLabel
        Left = 144
        Top = 85
        Width = 23
        Height = 13
        Caption = '&Join:'
        FocusControl = LBJoinStyle
      end
      object LBStyle: TListBox
        Left = 9
        Top = 24
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
        TabOrder = 0
        OnClick = LBStyleClick
      end
      object LBEndStyle: TListBox
        Left = 144
        Top = 24
        Width = 73
        Height = 49
        ItemHeight = 13
        Items.Strings = (
          'Round'
          'Square'
          'Flat')
        TabOrder = 1
        OnClick = LBEndStyleClick
      end
      object LBJoinStyle: TListBox
        Left = 144
        Top = 104
        Width = 73
        Height = 49
        ItemHeight = 13
        Items.Strings = (
          'Round'
          'Bevel'
          'Mitter')
        TabOrder = 2
        OnClick = LBJoinStyleClick
      end
    end
  end
end
