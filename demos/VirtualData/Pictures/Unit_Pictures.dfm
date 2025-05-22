object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'TeeGrid automatic TPicture rendering'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object TeeGrid1: TTeeGrid
    Left = 8
    Top = 8
    Width = 608
    Height = 417
    Columns = <>
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
end
