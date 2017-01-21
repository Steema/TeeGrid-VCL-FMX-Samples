object FormCellEditors: TFormCellEditors
  Left = 0
  Top = 0
  Caption = 'TeeGrid - Cell Editors Example'
  ClientHeight = 362
  ClientWidth = 644
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
    Width = 644
    Height = 321
    Columns = <>
    ReadOnly = False
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitTop = 33
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
    Width = 644
    Height = 41
    Align = alTop
    TabOrder = 1
    ExplicitLeft = 240
    ExplicitTop = 184
    ExplicitWidth = 185
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 242
      Height = 13
      Caption = 'Different controls to edit cells.  Click a cell to edit it'
    end
  end
end
