object Form27: TForm27
  Left = 0
  Top = 0
  Caption = 'TeeGrid Options Column'
  ClientHeight = 290
  ClientWidth = 423
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
    Width = 238
    Height = 290
    Cells.Format.Font.Name = 'Segoe UI'
    Cells.Format.Font.Size = 9.000000000000000000
    Columns = <>
    Header.Format.Font.Name = 'Segoe UI'
    Header.Format.Font.Size = 9.000000000000000000
    Rows.Format.Font.Name = 'Segoe UI'
    Rows.Format.Font.Size = 9.000000000000000000
    Rows.Hover.Format.Font.Name = 'Segoe UI'
    Rows.Hover.Format.Font.Size = 9.000000000000000000
    Selected.Format.Font.Name = 'Segoe UI'
    Selected.Format.Font.Size = 9.000000000000000000
    Selected.UnFocused.Format.Font.Name = 'Segoe UI'
    Selected.UnFocused.Format.Font.Size = 9.000000000000000000
    OnDataChanged = TeeGrid1DataChanged
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
          Format.Font.Name = 'Segoe UI'
          Format.Font.Size = 9.000000000000000000
        end>)
  end
  object Panel1: TPanel
    Left = 238
    Top = 0
    Width = 185
    Height = 290
    Align = alRight
    TabOrder = 1
    object Memo1: TMemo
      Left = 1
      Top = 1
      Width = 183
      Height = 89
      Align = alTop
      Lines.Strings = (
        '0=Best'
        '1=Good'
        '2=Normal'
        '3=Bad'
        '4=Worst')
      TabOrder = 0
    end
    object Memo2: TMemo
      Left = 1
      Top = 90
      Width = 183
      Height = 199
      Align = alClient
      Lines.Strings = (
        'Toyota,0'
        'Ford,1'
        'Honda,0'
        'Volkswagen,2'
        'Audi,1'
        'Mercedes,1'
        'Renault,3'
        'Peugeot,3'
        'Fiat,4')
      TabOrder = 1
    end
  end
end
