object StringGridForm: TStringGridForm
  Left = 0
  Top = 0
  Caption = 'StringGrid Example'
  ClientHeight = 492
  ClientWidth = 703
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
    Width = 703
    Height = 410
    Columns = <>
    Header.OnClick = TeeGrid1ClickedHeader
    Footer = <>
    ReadOnly = False
    OnClickedHeader = TeeGrid1ClickedHeader
    OnSelect = TeeGrid1Select
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 451
    Width = 703
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 703
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 44
      Height = 13
      Caption = 'Columns:'
    end
    object Label2: TLabel
      Left = 136
      Top = 14
      Width = 30
      Height = 13
      Caption = 'Rows:'
    end
    object EColumns: TEdit
      Left = 66
      Top = 11
      Width = 47
      Height = 21
      Alignment = taRightJustify
      TabOrder = 0
      Text = '3'
      OnChange = EColumnsChange
    end
    object ERows: TEdit
      Left = 186
      Top = 11
      Width = 47
      Height = 21
      Alignment = taRightJustify
      TabOrder = 1
      Text = '6'
      OnChange = ERowsChange
    end
    object Button1: TButton
      Left = 312
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Edit...'
      TabOrder = 2
      OnClick = Button1Click
    end
  end
end
