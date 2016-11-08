object TeeGridEditor: TTeeGridEditor
  Left = 0
  Top = 0
  Caption = 'TeeGridEditor'
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
    ActivePage = TabColumns
    Align = alClient
    TabOrder = 0
    OnChange = PageGridChange
    object TabColumns: TTabSheet
      Caption = 'Columns'
      object Splitter1: TSplitter
        Left = 0
        Top = 100
        Width = 331
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        Visible = False
        ExplicitTop = 0
        ExplicitWidth = 192
      end
      object TreeView1: TTreeView
        Left = 0
        Top = 41
        Width = 331
        Height = 59
        Align = alClient
        HideSelection = False
        HotTrack = True
        Indent = 19
        TabOrder = 0
        OnChange = TreeView1Change
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
      end
      object PanelEditor: TPanel
        Left = 0
        Top = 103
        Width = 331
        Height = 341
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        Visible = False
      end
    end
    object TabHeader: TTabSheet
      Caption = 'Header'
      ImageIndex = 2
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 331
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitTop = -6
        object CBHeaderVisible: TCheckBox
          Left = 9
          Top = 13
          Width = 65
          Height = 17
          Caption = '&Visible'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CBHeaderVisibleClick
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
        ActivePage = TabGeneralOptions
        Align = alClient
        TabOrder = 0
        object TabGeneralOptions: TTabSheet
          Caption = 'General'
          ImageIndex = 2
          object CBReadOnly: TCheckBox
            Left = 16
            Top = 16
            Width = 137
            Height = 17
            Caption = '&Read Only'
            TabOrder = 0
            OnClick = CBReadOnlyClick
          end
        end
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
            object Label2: TLabel
              Left = 94
              Top = 6
              Width = 32
              Height = 13
              Caption = '&Width:'
              FocusControl = TBIndicatorWidth
            end
            object LIndicatorWidth: TLabel
              Left = 288
              Top = 6
              Width = 6
              Height = 13
              Caption = '0'
            end
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
            object TBIndicatorWidth: TTrackBar
              Left = 131
              Top = 6
              Width = 150
              Height = 21
              Max = 50
              Frequency = 5
              Position = 10
              TabOrder = 1
              ThumbLength = 12
              OnChange = TBIndicatorWidthChange
            end
          end
        end
        object TabColumnLines: TTabSheet
          Caption = 'Column Lines'
          ImageIndex = 1
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
        object TabCellsFormat: TTabSheet
          Caption = 'Format'
        end
        object TabCellsHover: TTabSheet
          Caption = 'Hover'
          ImageIndex = 1
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
          object CBFullRow: TCheckBox
            Left = 9
            Top = 54
            Width = 97
            Height = 17
            Caption = 'Full Row Select'
            TabOrder = 2
            OnClick = CBFullRowClick
          end
          object CBRowHeightAuto: TCheckBox
            Left = 152
            Top = 14
            Width = 97
            Height = 17
            Caption = '&Automatic'
            TabOrder = 3
            OnClick = CBRowHeightAutoClick
          end
        end
        object TabRowLines: TTabSheet
          Caption = 'Lines'
        end
        object TabRowAlternate: TTabSheet
          Caption = 'Alternate'
          ImageIndex = 1
        end
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
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
end
