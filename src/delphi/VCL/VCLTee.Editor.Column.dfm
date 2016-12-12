object ColumnEditor: TColumnEditor
  Left = 0
  Top = 0
  Caption = 'Column Editor'
  ClientHeight = 304
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageFormat: TPageControl
    Left = 0
    Top = 0
    Width = 360
    Height = 304
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 0
    OnChange = PageFormatChange
    object TabGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 3
      object Label5: TLabel
        Left = 13
        Top = 4
        Width = 39
        Height = 13
        Caption = '&Header:'
        FocusControl = MemoHeader
      end
      object Label1: TLabel
        Left = 13
        Top = 168
        Width = 37
        Height = 13
        Caption = '&Locked:'
        FocusControl = CBLocked
      end
      object CBVisible: TCheckBox
        Left = 13
        Top = 85
        Width = 140
        Height = 17
        Caption = '&Visible'
        TabOrder = 0
        OnClick = CBVisibleClick
      end
      object CBExpanded: TCheckBox
        Left = 13
        Top = 108
        Width = 74
        Height = 17
        Caption = '&Expanded'
        Enabled = False
        TabOrder = 1
        OnClick = CBExpandedClick
      end
      object CBReadOnly: TCheckBox
        Left = 13
        Top = 131
        Width = 140
        Height = 17
        Caption = '&Read-only'
        TabOrder = 2
        OnClick = CBReadOnlyClick
      end
      object MemoHeader: TMemo
        Left = 13
        Top = 23
        Width = 201
        Height = 49
        TabOrder = 3
        OnChange = MemoHeaderChange
      end
      object CBLocked: TComboBox
        Left = 13
        Top = 187
        Width = 108
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'None'
        OnChange = CBLockedChange
        Items.Strings = (
          'None'
          'Left'
          'Right')
      end
    end
    object TabWidth: TTabSheet
      Caption = 'Width'
      ImageIndex = 4
    end
    object TabData: TTabSheet
      Caption = 'Data'
      ImageIndex = 5
      object Label3: TLabel
        Left = 10
        Top = 8
        Width = 72
        Height = 13
        Caption = 'Fl&oat numbers:'
      end
      object Label4: TLabel
        Left = 10
        Top = 56
        Width = 52
        Height = 13
        Caption = '&Date Time:'
      end
      object Label6: TLabel
        Left = 10
        Top = 104
        Width = 27
        Height = 13
        Caption = '&Date:'
      end
      object Label7: TLabel
        Left = 10
        Top = 152
        Width = 26
        Height = 13
        Caption = '&Time:'
      end
      object EFloatFormat: TEdit
        Left = 10
        Top = 27
        Width = 183
        Height = 21
        TabOrder = 0
        OnChange = EFloatFormatChange
      end
      object EDateTimeFormat: TEdit
        Left = 10
        Top = 75
        Width = 183
        Height = 21
        TabOrder = 1
        OnChange = EDateTimeFormatChange
      end
      object EDateFormat: TEdit
        Left = 10
        Top = 123
        Width = 183
        Height = 21
        TabOrder = 2
        OnChange = EDateFormatChange
      end
      object ETimeFormat: TEdit
        Left = 10
        Top = 171
        Width = 183
        Height = 21
        TabOrder = 3
        OnChange = ETimeFormatChange
      end
    end
    object TabFormat: TTabSheet
      Caption = 'Format'
      ImageIndex = 3
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 352
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object CBParentFormat: TCheckBox
          Left = 16
          Top = 11
          Width = 97
          Height = 17
          Caption = '&Parent Format'
          TabOrder = 0
          OnClick = CBParentFormatClick
        end
      end
    end
    object TabAlign: TTabSheet
      Caption = 'Text Align'
      ImageIndex = 4
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 352
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object CBAutoAlign: TCheckBox
          Left = 19
          Top = 8
          Width = 97
          Height = 17
          Caption = '&Automatic'
          TabOrder = 0
          OnClick = CBAutoAlignClick
        end
      end
    end
    object TabMargins: TTabSheet
      Caption = 'Margins'
      ImageIndex = 5
    end
    object TabHeader: TTabSheet
      Caption = 'Header'
      ImageIndex = 6
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 352
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object CBHeaderParent: TCheckBox
          Left = 16
          Top = 11
          Width = 97
          Height = 17
          Caption = '&Parent Format'
          TabOrder = 0
          OnClick = CBHeaderParentClick
        end
        object CBHeaderVisible: TCheckBox
          Left = 136
          Top = 11
          Width = 97
          Height = 17
          Caption = '&Visible'
          TabOrder = 1
          OnClick = CBHeaderVisibleClick
        end
      end
    end
  end
end
