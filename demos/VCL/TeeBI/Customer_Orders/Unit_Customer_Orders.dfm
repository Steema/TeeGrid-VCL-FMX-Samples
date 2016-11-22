object FormDetailRows: TFormDetailRows
  Left = 0
  Top = 0
  Caption = 'TeeGrid - Detail Rows'
  ClientHeight = 583
  ClientWidth = 692
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
  object TeeGrid1: TTeeGrid
    Left = 0
    Top = 41
    Width = 692
    Height = 542
    Columns = <>
    Footer = <>
    ReadOnly = False
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 692
    Height = 41
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 128
      Top = 14
      Width = 55
      Height = 13
      Caption = '&Expanders:'
      FocusControl = CBExpander
    end
    object Button1: TButton
      Left = 24
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Edit...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object CBExpander: TComboBox
      Left = 189
      Top = 11
      Width = 92
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Plus Minus'
      OnChange = CBExpanderChange
      Items.Strings = (
        'Plus Minus'
        'Triangle'
        'Arrow')
    end
  end
end
