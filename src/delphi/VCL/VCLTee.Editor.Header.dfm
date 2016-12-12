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
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 340
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 80
        ExplicitTop = 168
        ExplicitWidth = 185
        object CBAllowResize: TCheckBox
          Left = 16
          Top = 8
          Width = 97
          Height = 17
          Caption = 'Allow Resize'
          TabOrder = 0
          OnClick = CBAllowResizeClick
        end
      end
    end
    object TabRowLines: TTabSheet
      Caption = 'Row Lines'
      ImageIndex = 3
    end
    object TabMargins: TTabSheet
      Caption = 'Margins'
      ImageIndex = 2
    end
  end
end
