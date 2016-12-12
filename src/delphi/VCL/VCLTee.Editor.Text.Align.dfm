object TextAlignEditor: TTextAlignEditor
  Left = 0
  Top = 0
  Caption = 'Text Alignment'
  ClientHeight = 146
  ClientWidth = 294
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object RGHorizontal: TRadioGroup
    Left = 11
    Top = 16
    Width = 118
    Height = 113
    Caption = '&Horizontal:'
    ItemIndex = 0
    Items.Strings = (
      '&Left'
      '&Center'
      '&Right')
    TabOrder = 0
    OnClick = RGHorizontalClick
  end
  object RGVertical: TRadioGroup
    Left = 146
    Top = 16
    Width = 119
    Height = 113
    Caption = '&Vertical'
    ItemIndex = 0
    Items.Strings = (
      '&Top'
      '&Center'
      '&Bottom')
    TabOrder = 1
    OnClick = RGVerticalClick
  end
end
