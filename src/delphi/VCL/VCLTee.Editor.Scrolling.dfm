object ScrollingEditor: TScrollingEditor
  Left = 0
  Top = 0
  Caption = 'ScrollingEditor'
  ClientHeight = 378
  ClientWidth = 434
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
  object PageScrolling: TPageControl
    Left = 0
    Top = 0
    Width = 434
    Height = 378
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 80
    ExplicitTop = 112
    ExplicitWidth = 289
    ExplicitHeight = 193
    object TabOptions: TTabSheet
      Caption = 'Options'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
      object LHoriz: TLabel
        Left = 16
        Top = 152
        Width = 52
        Height = 13
        Caption = '&Horizontal:'
        FocusControl = CBHoriz
      end
      object Label1: TLabel
        Left = 16
        Top = 200
        Width = 39
        Height = 13
        Caption = '&Vertical:'
        FocusControl = CBVert
      end
      object RGMode: TRadioGroup
        Left = 16
        Top = 16
        Width = 137
        Height = 113
        Caption = '&Mode:'
        Items.Strings = (
          '&Touch'
          'M&ouse'
          '&Both'
          '&None')
        TabOrder = 0
        OnClick = RGModeClick
      end
      object CBHoriz: TComboBox
        Left = 16
        Top = 171
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'Normal'
        OnChange = CBHorizChange
        Items.Strings = (
          'Normal'
          'Inverted'
          'Disabled')
      end
      object CBVert: TComboBox
        Left = 16
        Top = 219
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = 'Normal'
        OnChange = CBVertChange
        Items.Strings = (
          'Normal'
          'Inverted'
          'Disabled')
      end
    end
    object TabScrollBars: TTabSheet
      Caption = 'Scroll Bars'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
      object Label2: TLabel
        Left = 16
        Top = 48
        Width = 52
        Height = 13
        Caption = '&Horizontal:'
        FocusControl = CBHorizScrollBar
      end
      object Label5: TLabel
        Left = 16
        Top = 96
        Width = 39
        Height = 13
        Caption = '&Vertical:'
        FocusControl = CBVertScrollBar
      end
      object CBScrollBars: TCheckBox
        Left = 16
        Top = 17
        Width = 97
        Height = 17
        Caption = '&Visible'
        TabOrder = 0
        OnClick = CBScrollBarsClick
      end
      object CBHorizScrollBar: TComboBox
        Left = 16
        Top = 67
        Width = 113
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        OnChange = CBHorizScrollBarChange
        Items.Strings = (
          'Automatic'
          'Show'
          'Hide')
      end
      object CBVertScrollBar: TComboBox
        Left = 16
        Top = 115
        Width = 113
        Height = 21
        Style = csDropDownList
        TabOrder = 2
        OnChange = CBVertScrollBarChange
        Items.Strings = (
          'Automatic'
          'Show'
          'Hide')
      end
    end
  end
end
