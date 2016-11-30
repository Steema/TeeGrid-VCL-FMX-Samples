object TextBandEditor: TTextBandEditor
  Left = 0
  Top = 0
  Caption = 'TextBandEditor'
  ClientHeight = 371
  ClientWidth = 402
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
  object PageText: TPageControl
    Left = 0
    Top = 0
    Width = 402
    Height = 371
    ActivePage = TabText
    Align = alClient
    TabOrder = 0
    OnChange = PageTextChange
    object TabText: TTabSheet
      Caption = 'Text'
      object MemoText: TMemo
        Left = 0
        Top = 0
        Width = 394
        Height = 343
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = MemoTextChange
      end
    end
    object TabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
    end
    object TabHeight: TTabSheet
      Caption = 'Height'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
