object VCLGridAbout: TVCLGridAbout
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'About TeeGrid'
  ClientHeight = 306
  ClientWidth = 379
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 265
    Width = 379
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object LVersion: TLabel
      Left = 16
      Top = 16
      Width = 3
      Height = 13
    end
    object Panel2: TPanel
      Left = 258
      Top = 0
      Width = 121
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Button1: TButton
        Left = 24
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Close'
        Default = True
        ModalResult = 8
        TabOrder = 0
      end
    end
  end
  object TeeGrid1: TTeeGrid
    Left = 16
    Top = 16
    Width = 344
    Height = 233
    Back.Brush.Gradient.Visible = True
    Back.Brush.Gradient.Inverted = True
    Columns = <>
    ReadOnly = False
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    OnDblClick = TeeGrid1DblClick
    _Headers = (
      1
      'TColumnHeaderBand'
      <
        item
        end>)
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 150
    OnTimer = Timer1Timer
    Left = 160
    Top = 152
  end
end
