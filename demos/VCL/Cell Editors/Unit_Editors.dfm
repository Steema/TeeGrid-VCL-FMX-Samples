object FormCellEditors: TFormCellEditors
  Left = 0
  Top = 0
  Caption = 'TeeGrid - Cell Editors Example'
  ClientHeight = 362
  ClientWidth = 644
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
  object TeeGrid1: TTeeGrid
    Left = 0
    Top = 41
    Width = 644
    Height = 191
    Columns = <>
    MouseActivity = [All]
    CellFormat = <>
    OnCellEditing = TeeGrid1CellEditing
    OnCellEdited = TeeGrid1CellEdited
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    _Headers = (
      1
      'TColumnHeaderBand'
      <
        item
        end>)
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 644
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 248
      Top = 14
      Width = 185
      Height = 13
      Caption = 'Double-Click a cell or press F2 to edit it'
    end
    object CBCustomEditors: TCheckBox
      Left = 16
      Top = 13
      Width = 209
      Height = 17
      Caption = 'Use different controls to edit cells'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBCustomEditorsClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 232
    Width = 644
    Height = 130
    Align = alBottom
    TabOrder = 2
    object Label2: TLabel
      Left = 16
      Top = 6
      Width = 74
      Height = 13
      Caption = 'Editing options:'
    end
    object Label3: TLabel
      Left = 127
      Top = 26
      Width = 376
      Height = 13
      Caption = 
        'True = Type any key to start cell editing.   False = Double-clic' +
        'k cell or press F2'
    end
    object Label4: TLabel
      Left = 127
      Top = 45
      Width = 382
      Height = 13
      Caption = 
        'True = Cell editing is always active when changing from one cell' +
        ' to another cells'
    end
    object Label5: TLabel
      Left = 71
      Top = 102
      Width = 50
      Height = 13
      Caption = 'Enter key:'
    end
    object Label6: TLabel
      Left = 127
      Top = 65
      Width = 303
      Height = 13
      Caption = 
        'True = Cell editor using TEdit will select all text when showing' +
        ' it'
    end
    object CBAutoEdit: TCheckBox
      Left = 24
      Top = 25
      Width = 97
      Height = 17
      Caption = 'Auto Edit'
      TabOrder = 0
      OnClick = CBAutoEditClick
    end
    object CBAlwaysVisible: TCheckBox
      Left = 24
      Top = 44
      Width = 97
      Height = 17
      Caption = 'Always Visible'
      TabOrder = 1
      OnClick = CBAlwaysVisibleClick
    end
    object CBEnterKey: TComboBox
      Left = 127
      Top = 99
      Width = 194
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Move to next cell (at right)'
      OnChange = CBEnterKeyChange
      Items.Strings = (
        'Move to next cell (at right)'
        'Move to cell below'
        'Keep on same cell')
    end
    object CBSelectedText: TCheckBox
      Left = 24
      Top = 64
      Width = 97
      Height = 17
      Caption = 'Selected Text'
      TabOrder = 3
      OnClick = CBSelectedTextClick
    end
    object Button1: TButton
      Left = 511
      Top = 64
      Width = 116
      Height = 25
      Caption = 'Mod Cell 1,1 format'
      TabOrder = 4
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 511
      Top = 95
      Width = 116
      Height = 25
      Caption = 'Mod Cell 2,1 format'
      TabOrder = 5
      OnClick = Button2Click
    end
  end
end
