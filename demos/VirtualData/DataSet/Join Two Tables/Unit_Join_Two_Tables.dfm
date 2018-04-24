object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TeeGrid using Two Tables Joined'
  ClientHeight = 617
  ClientWidth = 780
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
    Top = 0
    Width = 780
    Height = 617
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
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=E:\Users\Public\Documents\Embarcadero\Studio\19.0\Sampl' +
        'es\Data'
      'DataSource=DBDemosParadox'
      'DriverID=ODBC')
    Left = 392
    Top = 16
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      '')
    Left = 480
    Top = 16
  end
end
