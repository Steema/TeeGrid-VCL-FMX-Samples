This example shows how to use different custom "cell editor" controls, associated to grid columns.

![](https://raw.githubusercontent.com/Steema/TeeGrid/master/demos/VCL/Cell%20Editors/TeeGrid_Cell_Editors.gif)

The grid event OnCellEditing is called before a cell editor is displayed, which is the appropiate place to customize the editor (for example to fill a combobox values).

When the editor is finished (pressing the Enter key or moving to another cell), the OnCellEdited event is called, which is the place to use the editor control value (for example a TrackBar Position) to modify the grid data for that cell.