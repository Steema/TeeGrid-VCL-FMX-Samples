# TeeGrid Release Notes

Version: 0.2 Beta, Nov-22nd 2016

- GDI+ Plus support 

```delphi
uses Tee.Grid.Painter.GdiPlus;
TeeGrid1.Painter:= TGDIPlusPainter.Create;
```

- Grid Sub-Bands

Custom "bands" (extra row space) above any row

- Master-Detail Row groups

Implemented for TeeBI data in BI.Grid.Data.pas unit and available for any custom-derived virtual data class (GetDetail and CanExpand, methods)

- Much improved TDataSet / TDataSource support

Still not working perfectly. See issues below.

```delphi
uses Tee.Grid.Data.DB;
TeeGrid1.Data:= TVirtualDBData.From( DataSource1 );
```

- TeeGrid for Firemonkey source code

Under Sources\FMX folder

- TVirtualData sorting support

Implemented for TeeBI in BI.Grid.Data.pas unit and available for any custom-derived virtual data class (IsSorted, CanSortBy and SortBy methods)

- Fine-tuned speed (billions of cells perfectly capable)

The only limit might be your data memory requeriments.
Note: Rows with custom (different) heights increase the speed to repaint.

- Copy to clipboard

Selected cells ctrl+c or ctrl+insert copy data in CSV format

- Range cell selection

By mouse-drag or shift+ arrow keys

- Totals

New TColumnTotals and TTotalsHeader grid-band classes enable adding automatic subtotals to any row group.
See demos for an example of use.

- TStringGrid emulation

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
TeeGrid1.Footer.Add( TTitleBand.Create );
```

- Fixed bugs related to editing cells

- Several new "Themes"

iOS, Android, "Black" new themes in Tee.Grid.Themes.pas unit

- 
Version: 0.5 Alpha, Nov-7th 2016

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


## Issues:

- Cosmetic (missing margins, spacing with big stroke sizes)

- Scrolling (some inaccurate scrollbar positioning)

- DB TDataSource (improper buffering of records)

- Column resizing (no refreshing while dragging)




