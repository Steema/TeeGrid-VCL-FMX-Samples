object ColumnTotalsEditor: TColumnTotalsEditor
  Left = 0
  Top = 0
  Caption = 'Column Totals'
  ClientHeight = 356
  ClientWidth = 359
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
  object PageTotals: TPageControl
    Left = 0
    Top = 0
    Width = 359
    Height = 356
    ActivePage = TabCalc
    Align = alClient
    TabOrder = 0
    OnChange = PageTotalsChange
    object TabCalc: TTabSheet
      Caption = 'Calculations'
      ExplicitLeft = 8
      ExplicitTop = 28
      object TreeColumns: TTreeView
        Left = 0
        Top = 0
        Width = 121
        Height = 328
        Align = alLeft
        Indent = 19
        TabOrder = 0
        OnChange = TreeColumnsChange
        ExplicitLeft = 112
        ExplicitTop = 120
        ExplicitHeight = 97
      end
      object RGCalc: TRadioGroup
        Left = 143
        Top = 16
        Width = 121
        Height = 169
        Caption = '&Calculation:'
        Enabled = False
        Items.Strings = (
          'Count'
          'Sum'
          'Min'
          'Max'
          'Average')
        TabOrder = 1
        OnClick = RGCalcClick
      end
      object BNone: TButton
        Left = 152
        Top = 208
        Width = 75
        Height = 25
        Caption = '&None'
        Enabled = False
        TabOrder = 2
        OnClick = BNoneClick
      end
    end
    object TabFormat: TTabSheet
      Caption = 'Format'
      ImageIndex = 1
      ExplicitWidth = 281
      ExplicitHeight = 165
    end
    object TabHover: TTabSheet
      Caption = 'Hover'
      ImageIndex = 2
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 351
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = 8
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
          OnClick = CBHoverParentFontClick
        end
      end
    end
    object TabLines: TTabSheet
      Caption = 'Lines'
      ImageIndex = 3
    end
  end
end
