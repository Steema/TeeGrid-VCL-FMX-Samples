# TeeGrid

## Lightweight full-featured Grid / Tabular control

### For Embarcadero Studio, Delphi and C++, VCL and Firemonkey frameworks

Written from scratch (not deriving from TCustomGrid or TGrid), aprox 5k lines of code and 64K compiled size.

### Download

Click this link to download TeeGrid full source code

### Install

- Open TeeGrid project group from Sources\Packages
- Right-click DCLVCLTeeGrid package, and Install
- Right-click DCLFMXTeeGrid package, and Install

### Using TeeGrid

TeeGrid works like any other normal control, with the big difference vs TDBGrid and TGrid being there is no automatic support for "TDataset" or "TDataSource" components. 

Data must be provided to TeeGrid using a data provider class manually created. (This will be solved in the near future, with a DataSource property assignable at design-time)

The reason is TeeGrid is data-agnostic, it does not depends on database units or TDataset, which enables linking it to any kind of data like arrays of objects or records, TList, TCollection, TeeBI TDataItem or any other source.

Several classes to bind data to TeeGrid are provided, like TGridArrayData (for arrays) or TBIGridData (for TeeBI TDataItem objects)

Examples:

```delphi
// From array:
var MyData : Array of TPerson; 
...
TeeGrid1.Data:=TBIGridData.FromArray<TPerson>(TeeGrid1.Grid, MyData);

// From a TeeBI TDataItem:
var MyData : TDataItem;
MyData := TStore.Load('SQLite_Demo')['Products'];
TeeGrid1.Data := TBIGridData.Create(TeeGrid1.Grid, MyData );
```



### Current Features

- Virtual data (TVirtualData - derived class to provide cell values)

- Sub-columns (any column can have children columns)

- Per-column formatting (font, back fill, stroke, text alignment)

- Individual row heights (per-row custom height)

- Custom cell rendering (checkbox for booleans, or any TCellRender class)

- Cell text format (float, date-time formatting strings)

- Column ReadOnly, Visible and Expanded (for sub-columns)

- Automatic column width (or fixed, in pixels or % percent of grid width)

- Column mouse drag resizing

- Automatic scroll bars visibility

- Column ordering

- Grid Header formatting (font, back fill, stroke)

- Grid Header mouse-hover 

- Grid "indicator" column (left-most column with symbol for current row)

- Row highlighting (mouse-hover and selected row formatting)

- Full selected row highlight

- Grid ReadOnly

- Custom Grid editors (note: Edit box for texts in current version)

- Rows and Columns lines separators (stroke settings)

- Cell mouse-hover (highlights cell under mouse cursor)

- All coordinates as floats (for sub-pixel finetuning, Firemonkey only)

- Alternate row background filling (back brush, stroke settings)

- Onxxx events

- Abstract Grid "Painter" (canvas) deriving TPainter class

- Design-time editor dialog to modify all settings and properties

