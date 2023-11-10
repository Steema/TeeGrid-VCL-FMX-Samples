object FormGridThemes: TFormGridThemes
  Left = 0
  Top = 0
  Caption = 'TeeGrid Themes and VCL System Themes'
  ClientHeight = 684
  ClientWidth = 923
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
    Left = 153
    Top = 41
    Width = 770
    Height = 643
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
    Width = 923
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 5
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Edit...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 88
      Top = 9
      Width = 75
      Height = 25
      Caption = '+ - font.size'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 153
    Height = 643
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 153
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 5
        Top = 6
        Width = 81
        Height = 13
        Caption = 'TeeGrid Themes:'
      end
    end
    object LBTheme: TListBox
      Left = 0
      Top = 25
      Width = 153
      Height = 73
      Align = alTop
      ItemHeight = 13
      Items.Strings = (
        'Default'
        'iOS'
        'Android'
        'Black')
      TabOrder = 1
      OnClick = LBThemeClick
    end
    object Panel4: TPanel
      Left = 0
      Top = 98
      Width = 153
      Height = 31
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object Label2: TLabel
        Left = 5
        Top = 6
        Width = 100
        Height = 13
        Caption = 'VCL System Themes:'
      end
    end
    object LBVCLThemes: TListBox
      Left = 0
      Top = 129
      Width = 153
      Height = 514
      Align = alClient
      ItemHeight = 13
      TabOrder = 3
      OnClick = LBVCLThemesClick
    end
  end
end
