object FormRowHeights: TFormRowHeights
  Left = 0
  Top = 0
  Caption = 'TeeGrid Row Heights'
  ClientHeight = 618
  ClientWidth = 903
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
  object TeeGrid1: TTeeGrid
    Left = 0
    Top = 41
    Width = 903
    Height = 577
    Columns = <>
    ReadOnly = False
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
    Width = 903
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object CBMultiLine: TCheckBox
      Left = 16
      Top = 13
      Width = 97
      Height = 17
      Caption = '&Multi-line Text'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBMultiLineClick
    end
    object Button1: TButton
      Left = 144
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Edit...'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 264
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Benchmark'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
end
