object HeaderEditor: THeaderEditor
  Left = 0
  Top = 0
  Caption = 'Header Editor'
  ClientHeight = 405
  ClientWidth = 348
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
  object PageHeader: TPageControl
    Left = 0
    Top = 0
    Width = 348
    Height = 405
    ActivePage = TabFormat
    Align = alClient
    TabOrder = 0
    OnChange = PageHeaderChange
    object TabFormat: TTabSheet
      Caption = 'Options'
      ExplicitLeft = 8
      ExplicitTop = 28
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabRowLines: TTabSheet
      Caption = 'Row Lines'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
