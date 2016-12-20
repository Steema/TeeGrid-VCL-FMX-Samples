# TeeGrid
<www.steebi.com>

### Full **source code** and **automatic installer**: [Download latest version](http://www.steebi.com/files/code/beta/vcl_fmx/teegrid/index.htm) **Dec-12th, v0.4 Beta**

## Lightweight full-featured Grid / Tabular control

### For Embarcadero RAD Studio, Delphi and C++, VCL and Firemonkey frameworks (all platforms), and Lazarus FreePascal (Windows, Linux, etc)

Written from scratch (not derived from TCustomGrid or TGrid), aprox 10K lines of code and 100K compiled size.


[![](https://raw.github.com/Steema/TeeGrid/master/docs/img/small/TeeGrid_FMX.png)](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_FMX.png)
[![](https://raw.github.com/Steema/TeeGrid/master/docs/img/small/TeeGrid_VCL.png)](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_VCL.png)
[![](https://raw.github.com/Steema/TeeGrid/master/docs/img/small/TeeGrid_Lazarus.png)](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_Lazarus.png)
[![](https://raw.github.com/Steema/TeeGrid/master/docs/img/small/TeeGrid_Hierarchical.png)](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_Hierarchical.png)

### Links

[Release Notes](https://github.com/Steema/TeeGrid/blob/master/docs/releasenotes.md) (What's new?)

[Google+ Community](https://plus.google.com/u/0/communities/117324086536146457211)

Website: <http://www.steebi.com>

TeeBI data-mining: <https://github.com/Steema/BI>


### Install

Automatically:

- Run TeeGridRecompile.exe

Manually:

- Open TeeGrid project group from "Sources\Packages" folder
- Right-click DCLVCLTeeGrid package, and do "Install"
- Right-click DCLFMXTeeGrid package, and do "Install"

### Using TeeGrid

TeeGrid is **"data-agnostic"**

The "DataSource" property can be used not only with TDataSource or TDataSet but also with other component classes.

Custom Data can also be set to TeeGrid using a data provider class, manually created by code.

TeeGrid has no dependencies on database units (DB.pas) or any TDataset component.

This enables linking a TeeGrid to any kind of data, like a TDataset, TDataSource, arrays of objects or records or a generic TList using Rtti, a TCollection, TeeBI TDataItem structures, pure "Virtual Mode" or any other source, including your own custom "virtual data" class.

Several classes are provided to bind data to TeeGrid, like:

- TVirtualModeData in [Tee.Grid.Data.Strings](https://github.com/Steema/TeeGrid/blob/master/src/delphi/Tee.Grid.Data.Strings.pas) unit, to use OnGet and OnSet events

- TVirtualData<T> in [Tee.Grid.Data.Rtti](https://github.com/Steema/TeeGrid/blob/master/src/delphi/Tee.Grid.Data.Rtti.pas) unit (for records, arrays, generic TList, collections etc)

- TVirtualDBData in [Tee.Grid.Data.DB](https://github.com/Steema/TeeGrid/blob/master/src/delphi/Tee.Grid.Data.DB.pas) unit (for TDataSource and TDataSet)

- TBIGridData in [BI.Grid.Data](https://github.com/Steema/TeeGrid/blob/master/src/delphi/BI.Grid.Data.pas) unit (for [TeeBI](https://github.com/Steema/BI) TDataItem objects)

- TStringData in [Tee.Grid.Data.Strings](https://github.com/Steema/TeeGrid/blob/master/src/delphi/Tee.Grid.Data.Strings.pas) unit (to emulate a TStringGrid with "Cells[col,row]" property)

Examples:

```delphi
// From a TDataSource or TDataSet:
TeeGrid1.DataSource:= DataSource1;  // <-- FDQuery1 etc

// From a TDataSource creating a class:
TeeGrid1.Data:= TVirtualDBData.From(DataSource1);

// From an array:
var MyData : Array of TPerson; 
... fill array...
TeeGrid1.Data:=TVirtualArrayData<TPerson>.Create(MyData);

// From a TeeBI TDataItem:
var MyData : TDataItem;
MyData := TStore.Load('SQLite_Demo')['Products'];
TeeGrid1.Data := TBIGridData.Create(TeeGrid1.Grid, MyData );

// Emulating a TStringGrid:
var MyData : TStringsData;
MyData:= TStringsData.Create(10, 1000); // 10 columns, 1000 rows
TeeGrid1.Data:= MyData;
...
MyData.Cells[4,50]:= 'Hello';
```



### Current Features

- Huge data

TeeGrid is capable of handling a very big number of cells.
For example **1 billion** cells ( 1000 columns by 1 million rows ).

The only limit is the memory used by your own data, (compile for the 64bit platform for more than 2GB/3GB).

- Virtual data 

TVirtualModeData class to automatically create columns and provide cell values using OnGet and OnSet events.

- TStringGrid emulation

TeeGrid can be used like a TStringGrid with a TStringsData object:

```delphi
var Data : TStringsData;
Data:= TStringsData.Create;

// Initialize size
Data.Columns:= 2;
Data.Rows:= 6;

// Set header texts
Data.Headers[0]:= 'A';
Data.Headers[1]:= 'B';
 
// Fill rows and cells
Data[0,0]:= 'A0';
Data[1,0]:= 'B0';

// Set data to grid
TeeGrid1.Data:=Data;
```

- Sub-columns (any column can have children columns)

```delphi
TeeGrid1.Columns.AddColumn('My Column 1').Items.AddColumn('Sub-Column 1')...
```

- Per-column formatting (font, back fill, stroke, text alignment)

```delphi
TeeGrid1.Columns[3].Format.Font.Size:= 14;
```


- Per-cell custom paint using OnPaint event

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_Custom_Cell_Paint.png)

- Locked columns to left or right grid edges

```delphi
TeeGrid1.Columns[4].Locked:= TColumnLocked.Left;
```

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_Locked_Columns.gif)

- Individual row heights (per-row custom height)

```delphi
TeeGrid1.Rows.Heights[3]:= 50; 
```

- Automatic multi-line text in cells

```delphi
TeeGrid1.Rows.Height.Automatic:= True;  // <-- every row can be of different height
```

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_multiline_text.png)

- Row groups

Any row can be expanded to show its detail sub-grid rows (recursive grids-in-grids)
The grid Data class must support master-detail relationships.

For example, the TBIGridData class is provided to link TeeBI TDataItem data objects supporting master-detail.

See ["TeeBI_Customer_Orders"](https://github.com/Steema/TeeGrid/tree/master/demos/VCL/TeeBI/Customer_Orders) example.

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_DetailRows.png)

- Totals and SubTotals

Automatic summary "grid bands" can be added to any grid header or footer, and also to "detail" subgrids.

```delphi
var Totals : TColumnTotals;
Totals:= TColumnTotals.Create(TeeGrid1.Footer);

Totals.Calculation.Add( 'Quantity', TColumnCalculation.Sum);

// Add also a band with total names
TTotalsHeader.CreateTotals( TeeGrid1.Footer, Totals ) );
```

- Row "Sub-Bands"

Any row might display a grid band above the row.
The "band" can be anything, from a simple TTextBand to a complex group of bands or rows.

```delphi
var Title: TTextBand;
Title:= TTextBand.Create(TeeGrid1.Rows.SubBands);
Title.Text:='My Rows';

TeeGrid1.Rows.SubBands[23]:= Title;
```

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_SubTitle_Rows.png)

- Custom cell rendering

Default class for cell rendering is TCellRender. Other classes can be used or created to override the default behaviour, like for example to show check-boxes in columns with boolean (True/False) values:

```delphi
TeeGrid1.Columns[7].Render:= TBooleanRender.Create; 
```

- Cell text format (float, date-time formatting strings)

```delphi
TeeGrid1.Columns[0].FloatFormat:= '0.###'; 
```

- Column Visible and Expanded (for sub-columns)

```delphi
TeeGrid1.Columns[0].Visible:= False; 
TeeGrid1.Columns[0].Items[3].Expanded:= False; // visible, but collapsed
```

- Automatic column width (or fixed, in pixels or % percent of grid width)

```delphi
TeeGrid1.Columns[0].Width.Automatic:= False; 
TeeGrid1.Columns[0].Width.Value:= 40; 
TeeGrid1.Columns[0].Width.Units:= TSizeUnits.Percent;
```

- Column mouse drag resizing

Dragging the left mouse button in a column header edge resizes it

- Automatic scroll bars visibility

Scrollbars are automatically displayed when necessary, or they can be forced visible or hidden.

```delphi
TeeGrid1.ScrollBars.Vertical.Width:=50;
TeeGrid1.ScrollBars.Horizontal.Visible:= TScrollBarVisible.Hide; // <-- Automatic, Show or Hide
```

- Column ordering

Columns and sub-columns can be re-positioned:

```delphi
TeeGrid1.Columns[2].Index:= 0;  // move 2nd column to first (left-most) position
```

- Grid Header formatting (font, back fill, stroke)

```delphi
TeeGrid1.Columns[0].Header.Text:= 'My Column';
TeeGrid1.Columns[0].Header.Format.Font.Color:= TAlphaColors.Red;
```

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_Formatting.png)

- Grid Header mouse-hover 

```delphi
TeeGrid1.Header.Hover.Visible:= True;
TeeGrid1.Header.Hover.Format.Brush.Color:= TAlphaColors.Green;
```

- Grid "indicator" column (left-most column with symbol for current row)

```delphi
TeeGrid1.Indicator.Visible:= True; // <-- False to hide indicator
TeeGrid1.Indicator.Width:= 20;
```

- Row highlighting (mouse-hover and selected row formatting)

```delphi
// selection
TeeGrid1.Selected.Column:= TeeGrid1.Columns[3];
TeeGrid1.Selected.Row:= 5;
  
// formatting
TeeGrid1.Selected.ParentFont:= False;
TeeGrid1.Selected.Format.Font.Style:= [TFontStyle.fsBold];
```

- Multi-cell range selection (by mouse and arrow keys)

```delphi
// range selection
TeeGrid1.Selected.Range.FromColumn:= TeeGrid1.Columns[3];
TeeGrid1.Selected.Range.ToColumn:= TeeGrid1.Columns[6];

TeeGrid1.Selected.Range.FromRow:= 10;
TeeGrid1.Selected.Range.ToRow:= 15;
```

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_SubTotals_Range_Select_CSV_Footer.png)

- Copy selected cells to clipboard in CSV format, pressing Ctrl+C or Ctrl+Insert key and also by code:

```delphi
Clipboard.AsText:= TCSVData.From(TeeGrid1.Grid, TeeGrid1.Selected);
```

- Full selected row highlight

```delphi
TeeGrid1.Selected.FullRow:=True;
```

- Grid and Columns ReadOnly

```delphi
TeeGrid1.ReadOnly:= False;
TeeGrid1.Columns[0].ReadOnly:= True;
```

- Custom Grid editors 

```delphi
TeeGrid1.Columns[1].EditorClass:= TCalendarEditor;
```

- Rows and Columns lines separators (stroke settings)

```delphi
TeeGrid1.Rows.RowLines.Visible:= True;
TeeGrid1.Rows.RowLines.Size:= 3;
TeeGrid1.Rows.RowLines.Color:= TAlphaColors.Skyblue;
```

- Cell mouse-hover (highlights cell under mouse cursor)

Cell (or all cells in row) under mouse cursor can be highlighted:

```delphi
TeeGrid1.Cells.Hover.Visible:= True;
TeeGrid1.Cells.Hover.FullRow:= True;
TeeGrid1.Cells.Hover.Format.Stroke.Size:= 2;
```

- All coordinates as floats

For sub-pixel finetuning, Firemonkey only. VCL always rounds to integer pixels.

```delphi
TeeGrid1.Header.Height.Automatic:=False;
TeeGrid1.Header.Height.Value:=124.3;   // sub-pixels, decimals
```

- Alternate row background filling (back brush, stroke settings)

```delphi
TeeGrid1.Rows.Alternate.Brush.Visible:= True;
TeeGrid1.Rows.Alternate.Brush.Color:= TAlphaColors.Lightcoral;
TeeGrid1.Rows.Alternate.Stroke.Visible:= True;
```

- Events

The usual Onxxx events:

  * OnAfterDraw
  * OnClickedHeader
  * OnColumnResized
  * OnEditing
  * OnEdited

**TeeGrid-specific events:**

  * OnNewDetail (called when a row is expanded to show a sub-grid)
  * OnPaint (at TColumn class, to paint individual cells)
  * OnShowEditor (called when a cell editor is about to be displayed)


- Abstract Grid "Painter" (canvas) 

TeeGrid Painter property is of TPainter class.
This is an abstract class that can be overriden, for example to use GDI+ in VCL:

```delphi
TeeGrid1.Painter:= TGDIPlusPainter.Create;
```

- Design-time editor dialog to modify all settings and properties

Several editor dialogs, for both VCL and Firemonkey, usable at design-time and runtime to modify all TeeGrid properties like columns, formatting, etc

```delphi
uses VCLTee.Editor.Grid;
TTeeGridEditor.Edit(Self,TeeGrid1);
```

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_Bands_Editor.png)



- "Ticker" class

Update grid cell changes using a background thread with optional fade-out colors

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_Ticker.gif)



### Wish-List, Pending Features

- Using TStringGrid with TeeBI expressions to build a Spreadsheet (Excel like) control

- TreeColumn class (to display a tree inside a column, to expand / collapse rows)

- Improve TVirtualDBData class to support DB buffering and events

- Easy embeddable controls in cells or rows.

  A new TControlRender class to for example display sub-grids or TeeCharts below a row or inside a cell.

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_embedded_TeeChart.png)

- Compositions (several texts, images, etc inside the same cell)

- Column mouse-drag to reorder columns

- Buttons at header to expand / collapse sub-columns

- Automatic row sorting, filtering and searching

  Note: Row sorting, filtering and searching is already possible with TeeBI TBIGrid control.


### Lazarus / FreePascal and Ubuntu Linux

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_Ubuntu_Lazarus_FreePascal.png)
