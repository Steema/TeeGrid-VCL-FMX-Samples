# TeeGrid
<www.steebi.com>

## Lightweight full-featured Grid / Tabular control

### For Embarcadero Studio, Delphi and C++, VCL and Firemonkey frameworks and Lazarus FreePascal

Written from scratch (not derived from TCustomGrid or TGrid), aprox 5K lines of code and 64K compiled size.

[![](https://raw.github.com/Steema/TeeGrid/master/docs/img/small/TeeGrid_FMX.png)](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_FMX.png)

[![](https://raw.github.com/Steema/TeeGrid/master/docs/img/small/TeeGrid_VCL.png)](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_VCL.png)

[![](https://raw.github.com/Steema/TeeGrid/master/docs/img/small/TeeGrid_Lazarus.png)](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_Lazarus.png)

### Links

Full source code and automatic installer:

[Download latest version](http://www.steebi.com/files/code/beta/vcl_fmx/teegrid/index.htm)

[Release Notes](https://github.com/Steema/TeeGrid/blob/master/docs/releasenotes.md) (What's new?)

[Google+ Community](https://plus.google.com/u/0/communities/117324086536146457211)

Website: <http://www.steebi.com>


### Install

Automatically:

- Run TeeGridRecompile.exe

Manually:

- Open TeeGrid project group from Sources\Packages
- Right-click DCLVCLTeeGrid package, and do "Install"
- Right-click DCLFMXTeeGrid package, and do "Install"

### Using TeeGrid

TeeGrid is "data-agnostic"

Data must be provided to TeeGrid using a data provider class, manually created.

The reason is TeeGrid has no dependencies on database units (DB.pas) or any TDataset component.

This enables linking a TeeGrid to any kind of data, like a TDataset, TDataSource, arrays of objects or records or a generic TList using Rtti, a TCollection, TeeBI TDataItem structures or any other source, including your own custom "virtual data" class.

Note: Support for "TDataset" or "TDataSource" components at design-time is not yet implemented.

Several classes are provided to bind data to TeeGrid, like:

- TVirtualData<T> in Tee.Grid.Data unit (for arrays, generic TList etc)

- TVirtualDBData in Tee.Grid.Data.DB unit (for TDataSource and TDataSet)

- TBIGridData in BI.Grid.Data unit (for TeeBI TDataItem objects)

Examples:

```delphi
// From a TDataSource:
TeeGrid1.Data:= TVirtualDBData.From(DataSource1);

// From an array:
var MyData : Array of TPerson; 
... fill array...
TeeGrid1.Data:=TVirtualData<TPerson>.Create(MyData);

// From a TeeBI TDataItem:
var MyData : TDataItem;
MyData := TStore.Load('SQLite_Demo')['Products'];
TeeGrid1.Data := TBIGridData.Create(TeeGrid1.Grid, MyData );
```



### Current Features

- Virtual data 

TVirtualData or derived class to automatically create columns and provide cell values

- Sub-columns (any column can have children columns)

```delphi
TeeGrid1.Columns.AddColumn('My Column 1').Items.AddColumn('Sub-Column 1')...
```

- Per-column formatting (font, back fill, stroke, text alignment)

```delphi
TeeGrid1.Columns[3].Format.Font.Size:= 14;
```

- Individual row heights (per-row custom height)

```delphi
TeeGrid1.Rows.Heights[3]:= 50; 
```

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

Scrollbars are automatically displayed when necessary.
In Firemonkey they can be customized:

```delphi
TeeGrid1.ScrollBars.Vertical.Width:=50;
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

Note: Edit box for texts in current beta version.

```delphi
TeeGrid1.Columns[1].EditorClass:= TCalendarEditor;
```

- Rows and Columns lines separators (stroke settings)

```delphi
TeeGrid1.Lines.Rows.Visible:= True;
TeeGrid1.Lines.Rows.Size:= 3;
TeeGrid1.Lines.Rows.Color:= TAlphaColors.Skyblue;
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

OnAfterDraw
OnClickHeader
OnColumnResized
OnEditing
OnEdited

- Abstract Grid "Painter" (canvas) 

TeeGrid Painter property is of TPainter class.
This is an abstract class that can be overriden, for example to use GDI+ in VCL:

```delphi
TeeGrid1.Painter:= TGDIPlusPainter.Create; // <-- Note: not yet in beta version
```

- Design-time editor dialog to modify all settings and properties

Several editor dialogs, for both VCL and Firemonkey, usable at design-time and runtime to modify all TeeGrid properties like columns, formatting, etc

```delphi
uses VCLTee.Editor.Grid;
TTeeGridEditor.Edit(Self,TeeGrid1);
```

### Wish-List, Pending Features

- TDataSource / TDataSet support

Improve TVirtualDBData class to support DB buffering and events

- Easy embeddable controls in cells or rows.

To for example display sub-grids or TeeCharts below a row or inside a cell.

- Image / Picture display in cells and header

- Compositions (several texts, images, etc inside the same cell)

- Column mouse-drag to reorder columns

- Buttons at header to expand / collapse sub-columns

- Row groups (with expand / collapse)

- Automatic row sorting, filtering and searching

Note: Row sorting, filtering and searching is already possible with TeeBI TBIGrid control.

