object SelectedEditor: TSelectedEditor
  Left = 0
  Top = 0
  Caption = 'Selected Editor'
  ClientHeight = 381
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageSelected: TPageControl
    Left = 0
    Top = 0
    Width = 397
    Height = 381
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 0
    OnChange = PageSelectedChange
    ExplicitTop = -69
    ExplicitWidth = 478
    ExplicitHeight = 368
    object TabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 2
      ExplicitWidth = 470
      ExplicitHeight = 340
      object CBFullRow: TCheckBox
        Left = 19
        Top = 13
        Width = 112
        Height = 17
        Caption = '&Full Row Select'
        TabOrder = 0
        OnClick = CBFullRowClick
      end
      object CBRange: TCheckBox
        Left = 19
        Top = 36
        Width = 97
        Height = 17
        Caption = '&Allow Range'
        TabOrder = 1
        OnClick = CBRangeClick
      end
      object CBScrollToView: TCheckBox
        Left = 19
        Top = 73
        Width = 176
        Height = 17
        Caption = '&Change when scrolling'
        TabOrder = 2
        OnClick = CBScrollToViewClick
      end
    end
    object TabFormat: TTabSheet
      Caption = 'Format'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 275
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 389
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 470
        object CBParentFont: TCheckBox
          Left = 19
          Top = 8
          Width = 112
          Height = 17
          Caption = '&Parent Font'
          TabOrder = 0
          OnClick = CBParentFontClick
        end
      end
      object PageFormat: TPageControl
        Left = 0
        Top = 33
        Width = 389
        Height = 320
        ActivePage = TabFocused
        Align = alClient
        TabOrder = 1
        OnChange = PageFormatChange
        ExplicitWidth = 470
        ExplicitHeight = 307
        object TabFocused: TTabSheet
          Caption = 'Focused'
          ExplicitWidth = 462
          ExplicitHeight = 279
        end
        object TabUnfocused: TTabSheet
          Caption = 'Unfocused'
          ImageIndex = 1
        end
      end
    end
  end
end
