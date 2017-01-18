object GridTickerEditor: TGridTickerEditor
  Left = 0
  Top = 0
  Caption = 'Grid Ticker Editor'
  ClientHeight = 256
  ClientWidth = 308
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
  object PageTicker: TPageControl
    Left = 0
    Top = 0
    Width = 308
    Height = 256
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 0
    OnChange = PageTickerChange
    object TabOptions: TTabSheet
      Caption = 'Options'
      object Label2: TLabel
        Left = 8
        Top = 67
        Width = 31
        Height = 13
        Caption = '&Delay:'
        FocusControl = TBDelay
      end
      object LDelay: TLabel
        Left = 215
        Top = 67
        Width = 32
        Height = 13
        Caption = 'LDelay'
      end
      object CBFade: TCheckBox
        Left = 8
        Top = 31
        Width = 97
        Height = 17
        Caption = '&Fade Colors'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CBFadeClick
      end
      object CBEnabled: TCheckBox
        Left = 8
        Top = 8
        Width = 74
        Height = 17
        Caption = '&Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = CBEnabledClick
      end
      object TBDelay: TTrackBar
        Left = 56
        Top = 67
        Width = 153
        Height = 20
        LineSize = 25
        Max = 5000
        Min = 100
        Frequency = 150
        Position = 1000
        TabOrder = 2
        ThumbLength = 14
        OnChange = TBDelayChange
      end
    end
    object TabHigher: TTabSheet
      Caption = 'Higher'
      ImageIndex = 1
    end
    object TabLower: TTabSheet
      Caption = 'Lower'
      ImageIndex = 2
    end
  end
end
