object TeeGridEditor: TTeeGridEditor
  Left = 0
  Top = 0
  Caption = 'TeeGrid Editor'
  ClientHeight = 513
  ClientWidth = 339
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
  object PageGrid: TPageControl
    Left = 0
    Top = 0
    Width = 339
    Height = 472
    ActivePage = TabRows
    Align = alClient
    TabOrder = 0
    OnChange = PageGridChange
    object TabColumns: TTabSheet
      Caption = 'Columns'
      object Splitter1: TSplitter
        Left = 0
        Top = 141
        Width = 331
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        Visible = False
        ExplicitTop = 0
        ExplicitWidth = 192
      end
      object TreeColumns: TTreeView
        Left = 0
        Top = 41
        Width = 331
        Height = 100
        Align = alClient
        HideSelection = False
        HotTrack = True
        Indent = 19
        TabOrder = 0
        OnChange = TreeColumnsChange
        OnEdited = TreeColumnsEdited
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 331
        Height = 41
        Align = alTop
        TabOrder = 1
        object SBDeleteColumn: TSpeedButton
          Left = 8
          Top = 6
          Width = 23
          Height = 22
          Caption = '-'
          Enabled = False
          OnClick = SBDeleteColumnClick
        end
        object SBColumnUp: TSpeedButton
          Left = 48
          Top = 6
          Width = 23
          Height = 22
          Caption = '^'
          Enabled = False
          OnClick = SBColumnUpClick
        end
        object SBColumnDown: TSpeedButton
          Left = 77
          Top = 6
          Width = 23
          Height = 22
          Caption = 'v'
          Enabled = False
          OnClick = SBColumnDownClick
        end
        object Label3: TLabel
          Left = 158
          Top = 7
          Width = 41
          Height = 13
          Alignment = taRightJustify
          Caption = '&Spacing:'
          FocusControl = TBHorizSpacing
        end
        object LHorizSpacing: TLabel
          Left = 294
          Top = 7
          Width = 6
          Height = 13
          Caption = '0'
        end
        object TBHorizSpacing: TTrackBar
          Left = 202
          Top = 7
          Width = 87
          Height = 21
          Max = 30
          Frequency = 5
          Position = 10
          TabOrder = 0
          ThumbLength = 12
          OnChange = TBHorizSpacingChange
        end
      end
      object PanelEditor: TPanel
        Left = 0
        Top = 144
        Width = 331
        Height = 300
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        Visible = False
      end
    end
    object TabBands: TTabSheet
      Caption = 'Bands'
      ImageIndex = 2
      object PageBands: TPageControl
        Left = 0
        Top = 0
        Width = 331
        Height = 444
        ActivePage = TabHeaders
        Align = alClient
        TabOrder = 0
        OnChange = PageBandsChange
        object TabHeaders: TTabSheet
          Caption = 'Headers'
        end
        object TabFooter: TTabSheet
          Caption = 'Footer'
          ImageIndex = 1
        end
      end
    end
    object TabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      object PageOptions: TPageControl
        Left = 0
        Top = 0
        Width = 331
        Height = 444
        ActivePage = TabIndicator
        Align = alClient
        TabOrder = 0
        OnChange = PageOptionsChange
        object TabIndicator: TTabSheet
          Caption = 'Indicator'
          object Panel3: TPanel
            Left = 0
            Top = 0
            Width = 323
            Height = 41
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object CBIndicatorVisible: TCheckBox
              Left = 8
              Top = 5
              Width = 65
              Height = 17
              Caption = '&Visible'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = CBIndicatorVisibleClick
            end
          end
          object PageIndicator: TPageControl
            Left = 0
            Top = 41
            Width = 323
            Height = 375
            ActivePage = TabIndicatorFormat
            Align = alClient
            TabOrder = 1
            OnChange = PageIndicatorChange
            object TabIndicatorFormat: TTabSheet
              Caption = 'Format'
            end
            object TabIndicatorWidth: TTabSheet
              Caption = 'Width'
              ImageIndex = 1
            end
          end
        end
        object TabBack: TTabSheet
          Caption = 'Back'
          ImageIndex = 3
        end
        object TabMargins: TTabSheet
          Caption = 'Margins'
          ImageIndex = 4
        end
        object TabSelection: TTabSheet
          Caption = 'Selected'
          ImageIndex = 5
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 323
            Height = 65
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object CBFullRow: TCheckBox
              Left = 9
              Top = 13
              Width = 112
              Height = 17
              Caption = '&Full Row Select'
              TabOrder = 0
              OnClick = CBFullRowClick
            end
            object CBSelectedParentFont: TCheckBox
              Left = 137
              Top = 12
              Width = 112
              Height = 17
              Caption = '&Parent Font'
              TabOrder = 1
              OnClick = CBSelectedParentFontClick
            end
            object CBSelectedRange: TCheckBox
              Left = 9
              Top = 36
              Width = 97
              Height = 17
              Caption = '&Range'
              TabOrder = 2
              OnClick = CBSelectedRangeClick
            end
          end
          object PageSelected: TPageControl
            Left = 0
            Top = 65
            Width = 323
            Height = 351
            ActivePage = TabSelectedFocused
            Align = alClient
            TabOrder = 1
            object TabSelectedFocused: TTabSheet
              Caption = 'Focused'
            end
            object TabSelectedUnfocused: TTabSheet
              Caption = 'Unfocused'
              ImageIndex = 1
            end
          end
        end
        object TabEditing: TTabSheet
          Caption = 'Editing'
          ImageIndex = 5
          object CBDoubleClick: TCheckBox
            Left = 16
            Top = 55
            Width = 97
            Height = 17
            Caption = '&Double Click'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = CBDoubleClickClick
          end
          object CBEditingAlways: TCheckBox
            Left = 16
            Top = 78
            Width = 97
            Height = 17
            Caption = '&Always Visible'
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = CBEditingAlwaysClick
          end
          object CBReadOnly: TCheckBox
            Left = 16
            Top = 16
            Width = 137
            Height = 17
            Caption = '&Read Only'
            TabOrder = 2
            OnClick = CBReadOnlyClick
          end
        end
        object TabScrollBars: TTabSheet
          Caption = 'Scroll Bars'
          ImageIndex = 5
          object CBScrollBars: TCheckBox
            Left = 16
            Top = 16
            Width = 97
            Height = 17
            Caption = '&Visible'
            TabOrder = 0
            OnClick = CBScrollBarsClick
          end
        end
      end
    end
    object TabCells: TTabSheet
      Caption = 'Cells'
      ImageIndex = 3
      object PageCells: TPageControl
        Left = 0
        Top = 0
        Width = 331
        Height = 444
        ActivePage = TabCellsFormat
        Align = alClient
        TabOrder = 0
        OnChange = PageCellsChange
        object TabCellsFormat: TTabSheet
          Caption = 'Format'
        end
        object TabCellsHover: TTabSheet
          Caption = 'Hover'
          ImageIndex = 1
        end
        object TabColumnLines: TTabSheet
          Caption = 'Column Lines'
          ImageIndex = 2
        end
      end
    end
    object TabRows: TTabSheet
      Caption = 'Rows'
      ImageIndex = 4
      object PageRows: TPageControl
        Left = 0
        Top = 0
        Width = 331
        Height = 444
        ActivePage = TabRowsGeneral
        Align = alClient
        TabOrder = 0
        OnChange = PageRowsChange
        object TabRowsGeneral: TTabSheet
          Caption = 'General'
          ImageIndex = 2
          object Label1: TLabel
            Left = 9
            Top = 15
            Width = 35
            Height = 13
            Caption = '&Height:'
            FocusControl = ERowHeight
          end
          object Label4: TLabel
            Left = 8
            Top = 65
            Width = 41
            Height = 13
            Caption = '&Spacing:'
            FocusControl = TBVertSpacing
          end
          object LVertSpacing: TLabel
            Left = 161
            Top = 81
            Width = 6
            Height = 13
            Caption = '0'
          end
          object ERowHeight: TEdit
            Left = 50
            Top = 12
            Width = 68
            Height = 21
            TabOrder = 0
            Text = '0'
            OnChange = ERowHeightChange
          end
          object UDRowHeight: TUpDown
            Left = 118
            Top = 12
            Width = 16
            Height = 21
            Associate = ERowHeight
            TabOrder = 1
          end
          object CBRowHeightAuto: TCheckBox
            Left = 152
            Top = 14
            Width = 97
            Height = 17
            Caption = '&Automatic'
            TabOrder = 2
            OnClick = CBRowHeightAutoClick
          end
          object TBVertSpacing: TTrackBar
            Left = 3
            Top = 81
            Width = 150
            Height = 21
            Max = 50
            Frequency = 5
            Position = 10
            TabOrder = 3
            ThumbLength = 12
            OnChange = TBVertSpacingChange
          end
        end
        object TabRowLines: TTabSheet
          Caption = 'Lines'
        end
        object TabRowAlternate: TTabSheet
          Caption = 'Alternate'
          ImageIndex = 1
          object Panel4: TPanel
            Left = 0
            Top = 0
            Width = 323
            Height = 41
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object CBAlternateVisible: TCheckBox
              Left = 9
              Top = 13
              Width = 65
              Height = 17
              Caption = '&Visible'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = CBAlternateVisibleClick
            end
          end
        end
      end
    end
    object TabTheme: TTabSheet
      Caption = 'Theme'
      ImageIndex = 5
      object LBThemes: TListBox
        Left = 16
        Top = 16
        Width = 145
        Height = 177
        ItemHeight = 13
        TabOrder = 0
        OnClick = LBThemesClick
      end
      object RGPainter: TRadioGroup
        Left = 16
        Top = 208
        Width = 145
        Height = 81
        Caption = '&Painter:'
        ItemIndex = 0
        Items.Strings = (
          'GDI+'
          'GDI')
        TabOrder = 1
        OnClick = RGPainterClick
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 472
    Width = 339
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object PanelOk: TPanel
      Left = 240
      Top = 0
      Width = 99
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOk: TButton
        Left = 8
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Close'
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
end
