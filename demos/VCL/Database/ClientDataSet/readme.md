## Connecting TeeGrid to a TDataSet

### NEW: Sorting the grid can now be done just clicking any column header

Sorting works with any DataSnap TClientDataSet or FireDAC TFDDataSet like a TFDMemTable.

<img width="879" height="839" alt="image" src="https://github.com/user-attachments/assets/59dc3565-876c-4556-9e5b-e8b7a0db5634" />

Clicking a column header sorts the dataset by that field, and clicking again switches the order (ascending or descending).

Activate sorting is done with a simple code:

```delphi

  uses
    Tee.GridData.DB.Sortable;
 
  // For example, a FireDAC dataset:

  TeeGrid1.DataSource := FDMemTable1;

  TSortableFireDACDataset.CreateSortable(TeeGrid1.Grid, FDMemTable1);

  ...

  // after working with the dataset, its better to call "CleanUp", to remove possibly
  // created indexes when sorting it.

  procedure TForm1.FormDestroy(Sender: TObject);
  begin
    TSortableFireDACDataset.CleanUp(FDMemTable1);
  end;

```

Field types like TBlob (or Bitmap) and TObjectField (ADT, Array) are currently not sorted.


###
