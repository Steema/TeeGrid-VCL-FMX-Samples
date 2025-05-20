object Chart_in_Grid: TChart_in_Grid
  Left = 0
  Top = 0
  Caption = 'TeeGrid and TeeChart'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object TeeGrid1: TTeeGrid
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    Columns = <>
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitLeft = 120
    ExplicitTop = 112
    ExplicitWidth = 400
    ExplicitHeight = 250
    _Headers = (
      1
      'TColumnHeaderBand'
      <
        item
        end>)
  end
end
