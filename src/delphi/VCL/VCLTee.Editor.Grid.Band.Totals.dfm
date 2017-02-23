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
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 0
    OnChange = PageTotalsChange
    object TabCalc: TTabSheet
      Caption = 'Calculations'
      object TreeColumns: TTreeView
        Left = 0
        Top = 0
        Width = 121
        Height = 328
        Align = alLeft
        Indent = 19
        TabOrder = 0
        OnChange = TreeColumnsChange
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
    object TabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      ExplicitLeft = 8
      ExplicitTop = 28
    end
  end
end
