object TeeMarginsEditor: TTeeMarginsEditor
  Left = 0
  Top = 0
  Caption = 'Margins Editor'
  ClientHeight = 347
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 339
    Height = 347
    ActivePage = TabLeft
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabLeft: TTabSheet
      Caption = 'Left'
    end
    object TabTop: TTabSheet
      Caption = 'Top'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 627
      ExplicitHeight = 271
    end
    object TabRight: TTabSheet
      Caption = 'Right'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 627
      ExplicitHeight = 271
    end
    object TabBottom: TTabSheet
      Caption = 'Bottom'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 627
      ExplicitHeight = 271
    end
  end
end
