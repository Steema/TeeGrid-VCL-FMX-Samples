inherited ColumnEditor: TColumnEditor
  ActiveControl = nil
  Caption = 'ColumnEditor'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageFormat: TPageControl
    ActivePage = TabGeneral
    object TabGeneral: TTabSheet [0]
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
    end
    inherited TabFont: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 326
      ExplicitHeight = 268
    end
    inherited TabStroke: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 326
      ExplicitHeight = 268
    end
    inherited TabBrush: TTabSheet
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 326
      ExplicitHeight = 268
    end
    object TabWidth: TTabSheet
      Caption = 'Width'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabSheet1: TTabSheet
      Caption = 'Formats'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 28
      ExplicitWidth = 0
      ExplicitHeight = 0
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
  end
end
