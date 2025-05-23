object FormCellEditors: TFormCellEditors
  Left = 0
  Top = 0
  Caption = 'TeeGrid - Cell Editors Example'
  ClientHeight = 396
  ClientWidth = 726
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 13
  object TeeGrid1: TTeeGrid
    Left = 0
    Top = 41
    Width = 726
    Height = 183
    Columns = <>
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
    Width = 726
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
    Top = 224
    Width = 726
    Height = 172
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
      Left = 36
      Top = 102
      Width = 85
      Height = 13
      Caption = 'Editing Enter key:'
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
    object Label7: TLabel
      Left = 25
      Top = 129
      Width = 96
      Height = 13
      Caption = 'Selecting Enter key:'
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
      Width = 242
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
      Left = 568
      Top = 16
      Width = 139
      Height = 25
      Caption = 'Custom Cell 1,1 format'
      TabOrder = 4
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 568
      Top = 47
      Width = 139
      Height = 25
      Caption = 'Custom Cell 2,1 format'
      TabOrder = 5
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 568
      Top = 78
      Width = 139
      Height = 25
      Caption = 'Custom Row 3 format'
      TabOrder = 6
      OnClick = Button3Click
    end
    object CBSelectingEnter: TComboBox
      Left = 127
      Top = 126
      Width = 242
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 7
      Text = 'Move to next cell at right and bottom'
      OnChange = CBSelectingEnterChange
      Items.Strings = (
        'Move to next cell at right and bottom'
        'Move to cell below'
        'Move to next cell at right')
    end
  end
end
