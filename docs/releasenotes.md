# TeeGrid Release Notes

##Version: 0.3 Beta, Nov-30th 2016

###TGridTicker

A new small component class to refresh grid cells using an internal thread.
See the ["Ticker"](https://github.com/Steema/TeeGrid/tree/master/demos/VCL/Ticker) demos for VCL and Firemonkey, and the design-time "About..." dialog for a live example.

![](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_Ticker.gif)

### Multi-line text support

Grid Headers automatically resize according to multi-line text.
Rows cells are multi-line disabled by default (as it can slow for "millions" of rows grids)

```delphi
TeeGrid1.Columns[3].Header.Text:= 'This is a'#13#10'multi-line text';

// Enable automatic per-row independent heights based on cell contents:
TeeGrid1.Rows.Height.Automatic:= True;
```

![](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_multiline_text.png)


### TColumn

New OnPaint event, called when a cell of a column is going to be painted.
This event has a boolean parameter "DefaultPaint", setting it to True will make the column to paint its text content after the event finishes.

```delphi
TeeGrid1.Columns[3].OnPaint:= PaintPicture;

procedure TForm1.PaintPicture(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
var tmp : TRectF;
begin
  DefaultPaint:=False; // True = paint cell after this method finishes

  if not DefaultPaint then
  begin
    tmp:=AData.Rect;
    tmp.Inflate(-8,-6);

    AData.Painter.Draw(MyPicture,tmp);
  end;
end;
```

New ParentFormat property (boolean, default True).
Setting it to False uses the Column.Format attributes to paint cells.
The default is to use the TeeGrid1.Cells.Format, available to all columns.

```delphi
TeeGrid1.Columns['Person'].ParentFormat:= False;
TeeGrid1.Columns['Person'].Format.Font.Color:= TColors.Green;
```

### Formatting

Gradient property in TBrush, to fill shapes and lines using multiple colors.
Note: GDI+ or Firemonkey painters required.

```delphi
uses Tee.Format;
TeeGrid1.Header.Format.Brush.Show;
TeeGrid1.Header.Format.Brush.Gradient.Show;
TeeGrid1.Header.Format.Brush.Gradient.Direction:= TGradientDirection.Horizontal;
```

Picture support.

```delphi
uses Tee.Format, VCLTee.Picture; // FMXTee.Picture in Firemonkey
TeeGrid1.Header.Format.Brush.Show;
TeeGrid1.Header.Format.Brush.Picture:= TVCLPicture.From('myimage.png');
```

Pictures are also automatically displayed when connecting a TeeGrid with a Dataset with graphic fields.

The [StringGrid](https://github.com/Steema/TeeGrid/tree/master/demos/VirtualData/TStringGrid) demo shows how to paint custom pictures at specific grid cells.


### DataSource property

Links a TeeGrid with a component at both design and runtime.

Components supported in this version are:

  TDataSet and TDataSource (using Tee.Grid.Data.Db.pas unit)
  TDataProvider (TeeBI only, using BI.Grid.Data.pas unit)

```delphi
uses Tee.Grid.Data.DB;

TeeGrid1.DataSource:= DataSource1;

// also allowed:
TeeGrid1.DataSource:= ClientDataset1;
```

### Tee.Grid.Data.DB

Improved support for DataSet linking, such as initializing columns horizontal text alignment, detecting changes to TField properties (Visible, DisplayWidth, etc), painting Blob graphic fields, and design-time support.

### Tee.Grid.Data.Rtti

Improved support to handle generic records and objects from generic TArray, TList, TDictionary etc

### Grid Bands

TeeGrid Headers, Footers, Row "Sub-Bands" and Row "Children" properties are now collection components (note: not yet usable at design-time). 
They are accessible at runtime at the TeeGrid editor dialog.

![](https://raw.github.com/Steema/TeeGrid/master/docs/img/TeeGrid_Bands_Editor.png)


### New Demos included

For [VCL](https://github.com/Steema/TeeGrid/tree/master/demos/VCL):

  * Header_Footer
  * Row_Heights
  * Customer_Orders (Master-detail hierarchical grid. Note: needs TeeBI)
  * Themes
  * Ticker
  
For [Firemonkey](https://github.com/Steema/TeeGrid/tree/master/demos/Firemonkey):  

  * Row_Subtitles
  * Ticker
  * TStringGrid
  
For [Lazarus / FreePascal](https://github.com/Steema/TeeGrid/tree/master/demos/Lazarus):

  * DataSet
  * StringGrid
  
[VirtualData demos](https://github.com/Steema/TeeGrid/tree/master/demos/VirtualData):

  * Arrays
  * DataSet
  * TList
  * TStringGrid
  
### TeeBI specific

Any TeeBI "Provider" component can be assigned to TeeGrid DataSource property at design and runtime:

```delphi
TeeGrid1.DataSource:= DataDefinition1;
```

### Miscellaneous

  * New TUIColor.Interpolate function at Tee.Format unit
  * TTitleBand class renamed to TTextBand  
  * Fixed C++ Builder header files generation
  * Overall speed improvements (see "Benchmark" buttons at StringGrid demos)
  * TeeGrid1.Rows.Back property (TFormat) to optionally fill the space behind all rows
  * ScrollBar visibility (Automatic or hidden) (VCL only)
  
  
##Version: 0.2 Beta, Nov-22nd 2016

- GDI+ Plus support 

```delphi
uses Tee.Grid.Painter.GdiPlus;
TeeGrid1.Painter:= TGDIPlusPainter.Create;
```

- Grid Sub-Bands

Custom "bands" (extra row space) above any row

```delphi
uses Tee.Grid.Bands;
var Title: TTitleBand;
Title:= TTitleBand.Create(TeeGrid1.Changed);
Title.Text:= 'North';
TeeGrid1.Rows.SubBands[10]:= Title;
```

- Master-Detail Row groups

Implemented for TeeBI data in BI.Grid.Data.pas unit and available for any custom-derived virtual data class (GetDetail and CanExpand, methods)

See [this example](https://github.com/Steema/TeeGrid/tree/master/demos/VCL/TeeBI/Customer_Orders)

- Much improved TDataSet / TDataSource support

Still not working perfectly. See issues below.

```delphi
uses Tee.Grid.Data.DB;
TeeGrid1.Data:= TVirtualDBData.From( DataSource1 );
```

- TeeGrid for Firemonkey source code

Under [Sources\FMX](https://github.com/Steema/TeeGrid/tree/master/src/delphi/FMX) folder

- TVirtualData sorting support

Implemented for TeeBI in BI.Grid.Data.pas unit and available for any custom-derived virtual data class (IsSorted, CanSortBy and SortBy methods)

See [this example](https://github.com/Steema/TeeGrid/tree/master/demos/VCL/TeeBI/Customer_Orders)

- Fine-tuned speed (billions of cells perfectly capable)

The only limit might be your data memory requeriments.
Note: Rows with custom (different) heights, or with sub-bands, take more time to repaint.

- Copy to clipboard

Selected cells ctrl+c or ctrl+insert copy data in CSV format

```delphi
if not TeeGrid1.Selected.IsEmpty then
   TeeGrid1.CopySelected;
```

- Range cell selection

By mouse-drag, shift+ arrow keys or by code:

```delphi
TeeGrid1.Selected.Range.FromRow:= 10;
TeeGrid1.Selected.Range.ToRow:= 20;
TeeGrid1.Selected.Range.FromColumn:= TeeGrid1.Columns['A'];
TeeGrid1.Selected.Range.ToColumn:= TeeGrid1.Columns['F'];
```

- Totals

New TColumnTotals and TTotalsHeader grid-band classes enable adding automatic subtotals to any row group.
See [this demo](https://github.com/Steema/TeeGrid/tree/master/demos/VCL/TeeBI/Customer_Orders) or [this demo](https://github.com/Steema/TeeGrid/tree/master/demos/VirtualData/Array) for an example of use.

- TStringGrid emulation

TStringData class enables using TeeGrid as a TStringGrid (property: Cells[AColumn,ARow]:= 'abc')

```delphi
uses Tee.Grid.Data.Strings;
var Data: TStringsData;
Data:= TStringsData.Create;
Data.Columns:= 100;
Data.Rows:= 10000;
TeeGrid1.Data:= Data;

Data.Cells[10,10]= 'Hello';
```

- Grid Footer

A collection of "grid bands" to display at the bottom of rows.
Also available for detail sub-grid rows.

```delphi
TeeGrid1.Footer.Add( TTitleBand.Create... );
```

- Several new "Themes"

iOS, Android, "Black" new themes in Tee.Grid.Themes.pas unit

- Fixed bugs related to editing cells

## Pending Issues:

- Cosmetic (missing margins, spacing with big stroke sizes)

- Scrolling (some inaccurate scrollbar positioning)

- DB TDataSource (improper buffering of records)


##Version: 0.5 Alpha, Nov-7th 2016

- Initial release

- Sources compile and support Embarcadero RAD Studio (from XE4 up to 10.1 Berlin Update 1) and Lazarus

- VCL, Firemonkey FMX and LCL supported

- Editor dialogs work at design-time and runtime (VCL and LCL)

- Cell Borders property

To display individual cell edge lines

- Cell Margins property

To control the amount of empty space around a cell contents

- TExpanderRender

New render class to display a "+" / "-" box at any cell, to for example enable expanding / collapsing a row group.
See demos for an example of use.

- TPainter

Abstract TPainter class new methods to draw / fill ellipses, polylines, etc

