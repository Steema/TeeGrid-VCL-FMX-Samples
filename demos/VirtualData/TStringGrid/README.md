## Example showing how to use TeeGrid like a "TStringGrid"

### Usage:

```pascal
uses Tee.GridData.Strings;

var Data : TStringsData;

// Rows x Columns
Data:=TStringsData.Create(1000,100000 {,60}); // speed tip: pass a default pixel column width (for example: 60)

// Alternative ways to resize a grid at any time:

// Data.Columns:=1000;
// Data.Rows:=100000;
// Data.Resize(1000,100000);  // Rows x Columns

TeeGrid1.Data:=Data;

// Example, fill rows and cells
for var row:=0 to Data.Rows-1 do
begin
  Data[ 0, row]:='0 '+IntToStr(row); // Column 0
  Data[ 1, row]:='1 '+IntToStr(row); // Column 1
  ...
end;

```

![](https://raw.githubusercontent.com/Steema/TeeGrid/master/demos/VirtualData/TStringGrid/TeeGrid_as_TStringGrid.png)

This example setups the grid to a relatively "big" size: 1000 columns x 100000 rows (hundred million cells)

For bigger sizes, this project should be compiled as 64bit platform because 32bit has a limit of 2GB (or 3GB using a custom PEFlag)
