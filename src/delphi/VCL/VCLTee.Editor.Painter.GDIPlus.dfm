object GDIPlusEditor: TGDIPlusEditor
  Left = 0
  Top = 0
  Caption = 'GDI+ Editor'
  ClientHeight = 226
  ClientWidth = 210
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
  object Label1: TLabel
    Left = 24
    Top = 160
    Width = 40
    Height = 13
    Caption = '&Smooth:'
    FocusControl = CBSmooth
  end
  object CBSmooth: TComboBox
    Left = 24
    Top = 179
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = CBSmoothChange
    Items.Strings = (
      'Default'
      'High Speed'
      'High Quality'
      'None'
      'Anti Alias 8x4'
      'Anti Alias 8x8')
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 185
    Height = 137
    Caption = 'Text:'
    TabOrder = 1
    object Label2: TLabel
      Left = 16
      Top = 24
      Width = 38
      Height = 13
      Caption = '&Quality:'
      FocusControl = CBTextQuality
    end
    object Label3: TLabel
      Left = 16
      Top = 72
      Width = 46
      Height = 13
      Caption = '&Trimming:'
      FocusControl = CBTrimming
    end
    object CBTextQuality: TComboBox
      Left = 16
      Top = 43
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = CBTextQualityChange
      Items.Strings = (
        'AntiAlias'
        'AntiAlias GridFit'
        'ClearType'
        'Single'
        'Single GridFit'
        'System Default')
    end
    object CBTrimming: TComboBox
      Left = 16
      Top = 91
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = CBTrimmingChange
      Items.Strings = (
        'None'
        'Character'
        'Word'
        'Ellipsis Character'
        'Ellipsis Word'
        'Ellipsis Path')
    end
  end
end
