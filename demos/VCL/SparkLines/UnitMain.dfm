object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Steema TeeGrid - SparkLines demo'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitTop = -6
    object Label1: TLabel
      Left = 16
      Top = 13
      Width = 28
      Height = 15
      Caption = '&Style:'
      FocusControl = CBStyle
    end
    object CBStyle: TComboBox
      Left = 57
      Top = 10
      Width = 63
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Line'
      OnChange = CBStyleChange
      Items.Strings = (
        'Line'
        'Bar'
        'Area')
    end
    object CheckBox1: TCheckBox
      Left = 160
      Top = 13
      Width = 97
      Height = 17
      Caption = 'Custom Sparks'
      TabOrder = 1
      OnClick = CheckBox1Click
    end
  end
  object TeeGrid1: TTeeGrid
    Left = 0
    Top = 41
    Width = 624
    Height = 400
    Columns = <>
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    _Headers = (
      1
      'TColumnHeaderBand'
      <
        item
        end>)
  end
end
