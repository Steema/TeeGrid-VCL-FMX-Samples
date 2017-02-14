object TeeFormatEditor: TTeeFormatEditor
  Left = 0
  Top = 0
  Caption = 'Format Editor'
  ClientHeight = 296
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageFormat: TPageControl
    Left = 0
    Top = 0
    Width = 334
    Height = 296
    ActivePage = TabStroke
    Align = alClient
    TabOrder = 0
    OnChange = PageFormatChange
    object TabStroke: TTabSheet
      Caption = 'Stroke'
      ImageIndex = 1
    end
    object TabBrush: TTabSheet
      Caption = 'Fill'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
