object FormLocked: TFormLocked
  Left = 0
  Top = 0
  Caption = 'TeeGrid - Locked Columns Example'
  ClientHeight = 528
  ClientWidth = 795
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 122
    Height = 528
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 44
      Height = 13
      Caption = 'Columns:'
    end
    object LBColumns: TListBox
      Left = 8
      Top = 27
      Width = 99
      Height = 262
      ItemHeight = 13
      TabOrder = 0
      OnClick = LBColumnsClick
    end
    object RGLocked: TRadioGroup
      Left = 8
      Top = 295
      Width = 99
      Height = 105
      Caption = '&Locked:'
      Enabled = False
      Items.Strings = (
        'None'
        'Left'
        'Right')
      TabOrder = 1
      OnClick = RGLockedClick
    end
  end
  object TeeGrid1: TTeeGrid
    Left = 122
    Top = 0
    Width = 673
    Height = 528
    Columns = <>
    ReadOnly = False
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
