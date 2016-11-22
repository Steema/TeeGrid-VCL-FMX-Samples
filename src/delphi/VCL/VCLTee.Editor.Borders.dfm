object BordersEditor: TBordersEditor
  Left = 0
  Top = 0
  Caption = 'Borders Editor'
  ClientHeight = 338
  ClientWidth = 341
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
  object PageBorders: TPageControl
    Left = 0
    Top = 0
    Width = 341
    Height = 338
    ActivePage = TabLeft
    Align = alClient
    TabOrder = 0
    OnChange = PageBordersChange
    object TabLeft: TTabSheet
      Caption = 'Left'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabTop: TTabSheet
      Caption = 'Top'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabRight: TTabSheet
      Caption = 'Right'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabBottom: TTabSheet
      Caption = 'Bottom'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
