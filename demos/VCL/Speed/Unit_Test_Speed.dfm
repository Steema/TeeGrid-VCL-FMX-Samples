object FormSpeed: TFormSpeed
  Left = 0
  Top = 0
  Caption = 'TeeGrid VCL Benchmark Speed Test'
  ClientHeight = 582
  ClientWidth = 853
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 853
    Height = 41
    Align = alTop
    TabOrder = 0
    object LabelResult: TLabel
      Left = 112
      Top = 13
      Width = 69
      Height = 15
      Caption = 'Time: 0 msec'
    end
    object Button1: TButton
      Left = 16
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Go !'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ComboGraphics: TComboBox
      Left = 236
      Top = 10
      Width = 65
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'GDI+'
      OnChange = ComboGraphicsChange
      Items.Strings = (
        'GDI+'
        'GDI'
        'Skia')
    end
    object CBAntiAlias: TCheckBox
      Left = 505
      Top = 13
      Width = 97
      Height = 17
      Caption = '&Antialias'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CBAntiAliasClick
    end
    object TrackBar1: TTrackBar
      Left = 608
      Top = 9
      Width = 150
      Height = 26
      Max = 30
      Min = -5
      TabOrder = 3
      OnChange = TrackBar1Change
    end
    object CBFormatting: TCheckBox
      Left = 352
      Top = 13
      Width = 129
      Height = 17
      Caption = 'Custom Formats'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CBFormattingClick
    end
  end
  object TeeGrid1: TTeeGrid
    Left = 0
    Top = 41
    Width = 853
    Height = 541
    Columns = <>
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    _Headers = (
      1
      'TColumnHeaderBand'
      <
        item
        end>)
  end
end
