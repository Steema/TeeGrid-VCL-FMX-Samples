### Custom sort

![](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_Custom_Sorting_VCL.gif)

A small class (TSortableHeader) enables clicking grid column headers to sort rows by that column.

```delphi
  TeeGrid1.Header.SortRender:= TSortableHeader.Create(TeeGrid1.Header.Changed);
```

This class does the following:

- Paints a triangle icon at column header when the column is sorted.

- Enlarges column width so the triangle icon has enough space to be painted.

- Detects header mouse clicks to perform sorting.

- Columns can be dragged (moved) and resized.


TSortableHeader needs 3 events to customize sorting:

- OnCanSort : Determine if a given column can be sorted or not

- OnSortBy : Perform row sorting, in ascending or descending order

- OnSortState : Returns the current order of a given column (Ascending, Descending, or None)


Note:

When a TeeGrid is connected to a TeeBI data source [see example](https://github.com/Steema/TeeGrid/tree/master/demos/VCL/TeeBI/Customer_Orders), sorting is automatically enabled.
