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
      Caption = 'Format'
    end
    object TabHover: TTabSheet
      Caption = 'Hover'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 340
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object CBHoverVisible: TCheckBox
          Left = 16
          Top = 16
          Width = 97
          Height = 17
          Caption = '&Visible'
          TabOrder = 0
          OnClick = CBHoverVisibleClick
        end
        object CBHoverParentFont: TCheckBox
          Left = 152
          Top = 16
          Width = 97
          Height = 17
          Caption = '&Parent Font'
          TabOrder = 1
        end
      end
    end
    object TabHeight: TTabSheet
      Caption = 'Height'
      ImageIndex = 2
      ExplicitLeft = 8
      ExplicitTop = 28
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
