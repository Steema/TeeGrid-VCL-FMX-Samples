object ColumnBandEditor: TColumnBandEditor
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
  object PageBand: TPageControl
    Left = 0
    Top = 0
    Width = 348
    Height = 405
    ActivePage = TabMouse
    Align = alClient
    TabOrder = 0
    OnChange = PageBandChange
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
    end
    object TabLines: TTabSheet
      Caption = 'Lines'
      ImageIndex = 3
    end
    object TabMouse: TTabSheet
      Caption = 'Mouse'
      ImageIndex = 4
      object CBAllowResize: TCheckBox
        Left = 16
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Allow &Resize'
        TabOrder = 0
        OnClick = CBAllowResizeClick
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 40
        Width = 129
        Height = 129
        Caption = '&Drag:'
        TabOrder = 1
        object CBAllowDrag: TCheckBox
          Left = 16
          Top = 23
          Width = 97
          Height = 17
          Caption = '&Allow'
          TabOrder = 0
          OnClick = CBAllowDragClick
        end
        object Button1: TButton
          Left = 16
          Top = 56
          Width = 75
          Height = 25
          Caption = '&Source...'
          TabOrder = 1
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 16
          Top = 87
          Width = 75
          Height = 25
          Caption = '&Target...'
          TabOrder = 2
          OnClick = Button2Click
        end
      end
    end
  end
end
