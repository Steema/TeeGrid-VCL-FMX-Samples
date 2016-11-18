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
    Top = 0
    Width = 703
    Height = 492
    Columns = <>
    Footer = <>
    ReadOnly = False
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitLeft = 160
    ExplicitTop = 144
    ExplicitWidth = 400
    ExplicitHeight = 250
  end
end
