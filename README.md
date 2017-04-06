# TeeGrid
<www.steebi.com>

### V1 **binary installer**: [Download latest version](http://www.teechart.net/files/teegrid/install/1.02/TeeGrid-Binary-1.02.exe) **6th April 2017, version, v1.02**

## Lightweight full-featured Grid / Tabular control

### For Embarcadero RAD Studio 2009 and up to Tokyo 10.2, Delphi and C++, VCL and Firemonkey frameworks (all platforms: Windows 32 and 64 bit, Mac OSX, Android and iOS), and Lazarus FreePascal (Windows, Linux, etc)

Written from scratch (not derived from TCustomGrid or TGrid), aprox 10K lines of code and 100K compiled size.

### Free for non-commercial use (in binary format)

See full [License](https://github.com/Steema/TeeGrid/blob/master/docs/license.txt) document


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

This enables linking a TeeGrid to any kind of data, like a TDataset, TDataSource, arrays of objects or records or a generic TList using Rtti, a TCollection, TStrings, TeeBI TDataItem structures, pure "Virtual Mode" or any other source, including your own custom "virtual data" class.

Several classes are already provided to bind data to TeeGrid, like:

- TVirtualModeData in [Tee.Grid.Data.Strings](https://github.com/Steema/TeeGrid/blob/master/src/delphi/Tee.Grid.Data.Strings.pas) unit, to use OnGet and OnSet events (pure virtual mode)

- TVirtualData<T> in [Tee.Grid.Data.Rtti](https://github.com/Steema/TeeGrid/blob/master/src/delphi/Tee.Grid.Data.Rtti.pas) unit (for records, arrays, generic TList, collections etc)

- TVirtualDBData in [Tee.Grid.Data.DB](https://github.com/Steema/TeeGrid/blob/master/src/delphi/Tee.Grid.Data.DB.pas) unit (for TDataSource and TDataSet)

- TBIGridData in [BI.Grid.Data](https://github.com/Steema/TeeGrid/blob/master/src/delphi/BI.Grid.Data.pas) unit (for [TeeBI](https://github.com/Steema/BI) TDataItem objects, ultra-fast big-data)

- TStringData in [Tee.Grid.Data.Strings](https://github.com/Steema/TeeGrid/blob/master/src/delphi/Tee.Grid.Data.Strings.pas) unit (to emulate a TStringGrid with "Cells[col,row]" property)

Examples:

```delphi
// From a TDataSource or TDataSet:
TeeGrid1.DataSource:= DataSource1;  // <-- or any dataset, FDQuery1, ClientDataSet1 etc

// From a TDataSource creating a class:
TeeGrid1.Data:= TVirtualDBData.From(DataSource1);

// From an array of records or classes:
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

- Virtual data mode

TVirtualModeData class to automatically create columns and provide cell values using OnGet and OnSet events.

- TStringGrid emulation

TeeGrid can be used like a TStringGrid using a TStringsData object:

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
TeeGrid1.Data:= Data;
```

- Sub-columns (any column can have children columns)

```delphi
TeeGrid1.Columns.AddColumn('My Column 1').Items.AddColumn('Sub-Column 1')...
```

- Per-column formatting (font, back fill, stroke, text alignment, margins)

```delphi
TeeGrid1.Columns[3].ParentFormat:= False;
TeeGrid1.Columns[3].Format.Font.Size:= 14;

TeeGrid1.Columns[3].TextAlignment:= TColumnTextAlign.Custom;
TeeGrid1.Columns[3].TextAlign:= TTextAlign.Center; // or Left or Right
```

- Per-cell custom paint using the column OnPaint event

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_Custom_Cell_Paint.png)

- Lock columns to left or right grid edges

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


- Custom rows height (same height for all rows)

```delphi
TeeGrid1.Rows.Height.Automatic:= False;
TeeGrid1.Rows.Height.Value:= 50;
```

- Row groups

Any row can be expanded to show a detail sub-grid (recursive grids-in-grids)
The grid Data class must support master-detail relationships.

For example, the TBIGridData class can link TeeBI TDataItem data objects supporting master-detail.

See ["TeeBI_Customer_Orders"](https://github.com/Steema/TeeGrid/tree/master/demos/VCL/TeeBI/Customer_Orders) example.

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_DetailRows.png)

Master-detail sub-grids using standard TDataSet components is also supported:

See ["Master-Detail FireDAC"](https://github.com/Steema/TeeGrid/tree/master/demos/FireMonkey/Database/Master_Detail_FireDAC) example.

- Totals and SubTotals

Automatic summary "grid bands" can be added to any grid header or footer, and also to "detail" subgrids.

```delphi
var Totals : TColumnTotals;
Totals:= TColumnTotals.Create(TeeGrid1.Footer);

Totals.Calculation.Add( 'Quantity', TColumnCalculation.Sum);

// Add also another band with header names for the totals band:
TTotalsHeader.CreateTotals( TeeGrid1.Footer, Totals ) );
```

- Row "Sub-Bands"

Any row might display a grid band above that row.
The "band" can be anything, from a simple TTextBand to a complex group of bands or rows.

```delphi
var Title: TTextBand;
Title:= TTextBand.Create(TeeGrid1.Rows.SubBands);
Title.Text:='My Rows';

TeeGrid1.Rows.SubBands[23]:= Title;  // <-- only for row 23
```

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_SubTitle_Rows.png)

- Custom cell rendering

The default class for cell rendering is TCellRender. Other classes can be used or created to override the default behaviour, like for example the TBooleanRender class to show check-boxes in columns that have boolean (True/False) values:

```delphi
TeeGrid1.Columns[7].Render:= TBooleanRender.Create; 
```

- Cell text custom formatting (float, date-time formatting strings)

```delphi
TeeGrid1.Columns[0].DataFormat.Float:= '0.###';  // also Date, Time, DateTime formats
```

- Automatic display of Bitmap and Picture data

Cells are filled with pictures when columns data is for example a TDataSet TField Blob type

- Column Visible and Expanded (for sub-columns)

```delphi
TeeGrid1.Columns[0].Visible:= False; 
TeeGrid1.Columns[0].Items[3].Expanded:= False; // visible, but collapsed

TeeGrid1.Columns[3].Hide; // <-- same as Visible:= False
```

- Column Selectable (enable or disable focusing cells of that column)

```delphi
TeeGrid1.Columns[1].Selectable:= False; 
```

- Automatic column width

Column width is automatically adjusted by default. It can also be set to a custom fixed value, in pixels or as % percent of total grid width.

```delphi
TeeGrid1.Columns[0].Width.Automatic:= False; 
TeeGrid1.Columns[0].Width.Value:= 40; 
TeeGrid1.Columns[0].Width.Units:= TSizeUnits.Percent;  // or Pixels
```

- Column mouse drag resizing

Dragging the left mouse button in a column header edge resizes it

- Automatic scroll bars visibility

Scrollbars are automatically displayed when necessary, or they can be forced visible or hidden.

```delphi
TeeGrid1.ScrollBars.Vertical.Width:= 50;
TeeGrid1.ScrollBars.Horizontal.Visible:= TScrollBarVisible.Hide; // <-- Automatic, Show or Hide
```

- Column ordering

Columns and sub-columns can be re-positioned:

```delphi
TeeGrid1.Columns[2].Index:= 0;  // move 2nd column to first (left-most) position
```

- Column dragging

Column headers can be mouse-dragged to move or re-order them to new positions

- Grid Header formatting (font, back fill, stroke, margins, text alignment)

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
TeeGrid1.Indicator.Visible:= True; // <-- set to False to hide indicator
TeeGrid1.Indicator.Width:= 20;
```

- Row highlighting (mouse-hover and selected row formatting)

Cells can be selected by mouse clicking them or using the arrow, enter and tab keys to navigate among them.

Selecting by mouse happens at mouse down or mouse up depending on grid scrolling and range selection, see below.

```delphi
// selecting a single cell
TeeGrid1.Selected.Column:= TeeGrid1.Columns[3];
TeeGrid1.Selected.Row:= 5;
  
// formatting of selected cells
TeeGrid1.Selected.ParentFont:= False;
TeeGrid1.Selected.Format.Font.Style:= [TFontStyle.fsBold];
```

- Unfocused selected format

Selected cells can be displayed using different formatting settings when the grid is not the focused control

```delphi
TeeGrid1.Selected.UnFocused.Format.Brush.Color:= TColors.Red;
```

- Multi-cell range selection (by mouse and/or arrow keys, or by code)

```delphi
// range selection
TeeGrid1.Selected.Range.Enabled:= True; // <-- It is disabled by default

TeeGrid1.Selected.Range.FromColumn:= TeeGrid1.Columns[3];
TeeGrid1.Selected.Range.ToColumn:= TeeGrid1.Columns[6];

TeeGrid1.Selected.Range.FromRow:= 10;
TeeGrid1.Selected.Range.ToRow:= 15;
```

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_SubTotals_Range_Select_CSV_Footer.png)

- Automatic grid scrolling when selecting cells

Grid can be automatically scrolled when selecting cells so the selected cell is always visible

```delphi
TeeGrid1.Selected.ScrollToView:= True; // default is False
```

- Grid scrolling

Horizontal and / or vertical scrolling can be performed using the grid scrollbars or arrow keys, and also optionally dragging the grid  using the mouse left button or panning via finger touch for touch devices.

Note: Cell range selection is disabled when mouse or touch scrolling is enabled

```delphi
TeeGrid1.Scrolling.Mode:= TScrollingMode.Touch; // or Mouse, Both, None
TeeGrid1.Scrolling.Horizontal:= TScrollDirection.Normal; // or Inverted, Disabled
```

- Copy selected cells to clipboard in CSV format, pressing Ctrl+C or Ctrl+Insert key and also by code:

```delphi
// The second parameter can be omitted to use all cells in the grid
Clipboard.AsText:= TCSVData.From(TeeGrid1.Grid, TeeGrid1.Selected);
```

- Full selected row highlight

```delphi
TeeGrid1.Selected.FullRow:= True;
```

- Grid and Columns ReadOnly

Note: If grid data is readonly (for example a TDataSet), it cannot be overriden

```delphi
TeeGrid1.ReadOnly:= False;
TeeGrid1.Columns[0].ReadOnly:= True;
```

- Custom Grid editors 

The EditorClass property controls which editor control to use to edit cells data.

See ["Cell Editors"](https://github.com/Steema/TeeGrid/tree/master/demos/FireMonkey/Cell%20Editors) VCL and FMX examples.

```delphi
TeeGrid1.Columns[1].EditorClass:= TCalendarEditor;
```

- Grid Editing properties

Several properties control grid cell editing features.

Selected cells can be edited by pressing the F2 key or by other means.

```delphi
TeeGrid1.Editing.AutoEdit:= True; // start editing cells when typing any letter or number

TeeGrid1.Editing.AlwaysVisible:= True; // keep cell editor active when moving to other cells

TeeGrid1.Editing.DoubleClick:= True; // start editing cells when double-clicking them

TeeGrid1.Editing.EnterKey:= NextCell; // move to other cells when finishing editing them pressing the Enter key

TeeGrid1.Editing.Text.Selected:= True; // select all cell text when starting editing it
```

- Rows and Columns lines separators (stroke settings)

```delphi

// row line dividers
TeeGrid1.Rows.RowLines.Visible:= True;
TeeGrid1.Rows.RowLines.Size:= 3;
TeeGrid1.Rows.RowLines.Color:= TAlphaColors.Skyblue;

// column line dividers
TeeGrid1.Rows.Lines.Visible:= False;
TeeGrid1.Rows.Lines.Size:= 2;
```

- Cell mouse-hover (highlights cell under mouse cursor)

The Cell (or all cells in a row) under the mouse cursor can be highlighted:

```delphi
TeeGrid1.Cells.Hover.Visible:= True;
TeeGrid1.Cells.Hover.FullRow:= True;
TeeGrid1.Cells.Hover.Format.Stroke.Size:= 2;
```

- All coordinates as floats

For sub-pixel finetuning. 

Note: For VCL, using the old "GDI" canvas always rounds to integer pixels

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

  OnAfterDraw
  
  OnCellEditing
  
  OnCellEdited
  
  OnClickedHeader
  
  OnColumnResized

**TeeGrid-specific events:**

  OnMoved (at TeeGrid.Columns, called when a column is dragged)
  
  OnNewDetail (called when a row is expanded to show a sub-grid)
  
  OnPaint (at TColumn class, to custom paint individual cells)
  
  OnSelect (called when the grid selected cell is changed)


- Abstract Grid "Painter" (canvas) 

TeeGrid Painter property is of TPainter class.

This is an abstract class that can be overriden, for example to use the old GDI in VCL:

```delphi
TeeGrid1.Painter:= TGDIPainter.Create;
```

Note: The default painter class in VCL is TGdiPlusPainter (Windows GDI+)

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

- TSheet component ("alpha" experimental version)

An Excel-like spreadsheet component with per-cell custom formatting and formula (using TeeBI TExpression class), and automatic recursive recalculation of cell values that depend on other cell values


### Wish-List, Pending Features

- TreeColumn class (to display a tree inside a column, to expand / collapse rows)

- Improve TVirtualDBData class to support DB buffering and insert/append events

- Easy embeddable controls in cells or rows.

  A new TControlRender class to for example display sub-grids or TeeCharts below a row or inside a cell.

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_embedded_TeeChart.png)

- Compositions (several texts, images, etc inside the same cell)

- Buttons at header to expand / collapse sub-columns

- Automatic row sorting, filtering and searching

  Note: Row sorting, filtering and searching is already possible with TeeBI TBIGrid control.


### Lazarus / FreePascal and Ubuntu Linux

![](https://github.com/Steema/TeeGrid/blob/master/docs/img/TeeGrid_Ubuntu_Lazarus_FreePascal.png)
