object TextRenderEditor: TTextRenderEditor
  Left = 0
  Top = 0
  Caption = 'Text Render Editor'
  ClientHeight = 484
  ClientWidth = 431
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
  object PageSelected: TPageControl
    Left = 0
    Top = 0
    Width = 431
    Height = 484
    ActivePage = TabFormat
    Align = alClient
    TabOrder = 0
    OnChange = PageSelectedChange
    object TabFormat: TTabSheet
      Caption = 'Format'
    end
    object TabBorders: TTabSheet
      Caption = 'Borders'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabMargins: TTabSheet
      Caption = 'Margins'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabAlign: TTabSheet
      Caption = 'Align'
      ImageIndex = 3
      object RGVerticalAlign: TRadioGroup
        Left = 138
        Top = 16
        Width = 119
        Height = 113
        Caption = '&Vertical'
        ItemIndex = 0
        Items.Strings = (
          '&Top'
          '&Center'
          '&Bottom')
        TabOrder = 0
        OnClick = RGVerticalAlignClick
      end
      object RGHorizAlign: TRadioGroup
        Left = 6
        Top = 16
        Width = 118
        Height = 113
        Caption = '&Horizontal:'
        ItemIndex = 0
        Items.Strings = (
          '&Left'
          '&Center'
          '&Right')
        TabOrder = 1
        OnClick = RGHorizAlignClick
      end
    end
  end
end
