# Custom Cell Editor Controls Example

The example "Custom Cell Editor Controls" shows how TeeGrid is capable to use different controls for each cell depending on data to display, and also some editing options which will improve the user experience at the time to edit and update the data inside the Grid.

We can customize each grid column by specific control type by using code as can be seen here :

```delphi
procedure TFormCellEditors.SetupCustomEditors;
begin
  // Custom cell editor controls (default is TEdit):

  TeeGrid1.Columns['Height'].EditorClass:=TTrackBar;

  TeeGrid1.Columns['BirthDate'].EditorClass:=TDateEdit;

  TeeGrid1.Columns['Vehicle'].EditorClass:=TComboBox;

  TeeGrid1.Columns['EyeColor'].EditorClass:=TComboColorBox;

  TeeGrid1.Columns['Holidays'].EditorClass:=TCheckBox;

  TeeGrid1.Columns['Happiness'].EditorClass:=TNumberBox;
end;
```

Each specific control will appear automatically once the cell is in editting mode.

By setting the "AutoEdit" property to True, pressing any key will show the cell editor automatically, otherwise, if it's set to False, double-click or F2 key will be required to starts editing.

```delphi
  TeeGrid1.Editing.AutoEdit:=CBAutoEdit.IsChecked;
```

By setting the "AlwaysVisible" property to True, moving from one cell to another will keep the cell editor visible.

```delphi
  TeeGrid1.Editing.AlwaysVisible
```

By setting the "Selected" property to True, editing a cell using a TEdit will select all text.

```delphi
  TeeGrid1.Editing.Text.Selected:=CBSelectedText.IsChecked;
```

We can configure the way the Grid have to act once the user press Enter key while editing a cell. There're the following options :

```delphi
  TEditingEnter=(NextCell,NextRow,SameCell);
```

We can choose a specific one by using the EnterKey property :

```delphi
  TeeGrid1.Editing.EnterKey:=TEditingEnter(CBEnterKey.ItemIndex);
```

