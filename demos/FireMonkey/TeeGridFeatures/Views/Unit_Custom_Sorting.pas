unit Unit_Custom_Sorting;

interface

{
  This example shows how to enable sorting TeeGrid columns by custom code,
  clicking at column headers.

  Note:
    For automatic sorting (no code), see TeeGrid TeeBI demos.
    The "BI.GridData.pas" unit already provides automatic sorting.
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  Tee.Grid.Header, Tee.Grid.Columns, FMXTee.Control, FMXTee.Grid,
  Tee.GridData.Rtti, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.Objects;

type
  // Remember the last sorted column and order
  TLastSorted=record
  public
    Column : TColumn;
    Ascending : Boolean;
  end;

  TFormCustomSorting = class(TForm)
    TeeGrid1: TTeeGrid;
    Layout1: TLayout;
    Label1: TLabel;
    Rectangle1: TRectangle;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    LastSorted : TLastSorted;

    // Methods to enable custom grid Sort by mouse click at Header

    procedure CanSortBy(const AColumn:TColumn; var CanSort:Boolean);
    function CreateSortable:TSortableHeader;
    procedure SortBy(Sender:TObject; const AColumn:TColumn);
    procedure SortData(const AColumn:TColumn; const Ascending:Boolean);
    procedure SortState(const AColumn:TColumn; var State:TSortState);
  public
    { Public declarations }
  end;

var
  FormCustomSorting: TFormCustomSorting;

implementation

{$R *.fmx}

// Generic units are used just to perform array sorting
uses
  System.Generics.Collections, System.Generics.Defaults;

type
  TLocation=record
  public
    Name : String;
    Country : String;
  end;

var
  Locations : TArray<TLocation>;

procedure AddRandomData;
begin
  SetLength(Locations,10);

  Locations[0].Name:='New York';
  Locations[1].Name:='Barcelona';
  Locations[2].Name:='Tokyo';
  Locations[3].Name:='Sao Paulo';
  Locations[4].Name:='Santa Cruz';
  Locations[5].Name:='Oslo';
  Locations[6].Name:='Camberra';
  Locations[7].Name:='Delhi';
  Locations[8].Name:='Montreal';
  Locations[9].Name:='Beijing';

  Locations[0].Country:='USA';
  Locations[1].Country:='Catalonia';
  Locations[2].Country:='Japan';
  Locations[3].Country:='Brazil';
  Locations[4].Country:='USA';
  Locations[5].Country:='Norway';
  Locations[6].Country:='Australia';
  Locations[7].Country:='India';
  Locations[8].Country:='Canada';
  Locations[9].Country:='China';
end;

procedure TFormCustomSorting.FormCreate(Sender: TObject);
begin
  AddRandomData;

  // Set Header sortable
  TeeGrid1.Header.Sortable:=True;
  TeeGrid1.Header.SortRender:=CreateSortable;

  // Set grid data
  TeeGrid1.Data:=TVirtualArrayData<TLocation>.Create(Locations);

  // Cosmetics
  TeeGrid1.Columns.Items[0].Width.Value:= 150;
  TeeGrid1.Columns.Items[1].Width.Value:= 150;
end;

// Associate a special "render" to grid header (to paint the triangle icons)
function TFormCustomSorting.CreateSortable:TSortableHeader;
begin
  result:=TSortableHeader.Create(TeeGrid1.Header.Changed);

  // cosmetic example
  result.Format.Brush.Color:=TColors.Red;
  result.Format.Stroke.Show;

  // Set custom events
  result.OnCanSort:=CanSortBy;
  result.OnSortBy:=SortBy;
  result.OnSortState:=SortState;
end;

// Allow sorting only for specific columns (in this example, the "Name" column)
procedure TFormCustomSorting.CanSortBy(const AColumn: TColumn; var CanSort: Boolean);
begin
  CanSort:=(AColumn<>nil) and
           (
             (AColumn.Header.Text='Name')
             or
             (AColumn.Header.Text='Country')
           );
end;

// Return the sort state for AColumn (Ascending, Descending, or None)
procedure TFormCustomSorting.SortState(const AColumn:TColumn; var State:TSortState);
begin
  if AColumn=LastSorted.Column then
     if LastSorted.Ascending then
        State:=TSortState.Ascending
     else
        State:=TSortState.Descending
  else
     State:=TSortState.None;
end;

// When clicking a column header, sort it
procedure TFormCustomSorting.SortBy(Sender: TObject; const AColumn: TColumn);
var tmp : TSortState;
begin
  // Get current sort order for AColumn
  SortState(AColumn,tmp);

  // Invert order ( Ascending <--> Descending )
  LastSorted.Ascending:=tmp<>TSortState.Ascending;

  // Sort array
  SortData(AColumn,LastSorted.Ascending);

  // Remember last sorted column
  LastSorted.Column:=AColumn;
end;


// This comparer type is used to sort the array data
type
  TLocationField=(Name,Country);

  TLocationComparer=class(TComparer<TLocation>)
  public
    Ascending : Boolean;
    Field : TLocationField;

    function Compare(const Left, Right: TLocation): Integer; override;
  end;

procedure TFormCustomSorting.SortData(const AColumn:TColumn; const Ascending:Boolean);

  // Returns the current selected Location in the grid, if any
  function CurrentLocation:TLocation;
  var row : Integer;
  begin
    row:=TeeGrid1.Selected.Row;

    if row=-1 then
    begin
      result.Name:='';
      result.Country:='';
    end
    else
      result:=Locations[row];
  end;

  // Finds the index of ALocation in our array, or -1
  function FindLocation(const ALocation:TLocation):Integer;
  var t : Integer;
  begin
    result:=-1;

    if ALocation.Name<>'' then
       for t:=0 to High(Locations) do
           if (Locations[t].Name=ALocation.Name) and
              (Locations[t].Country=ALocation.Country) then
           begin
             result:=t;
             break;
           end;
  end;

var Comparer : TLocationComparer;
    Current : TLocation;
begin
  // Remember the current selection Location record in the grid, if any
  Current:=CurrentLocation;

  Comparer:=TLocationComparer.Create;
  try
    Comparer.Ascending:=Ascending;

    // Choose a column
    if AColumn.Header.Text='Name' then
       Comparer.Field:=TLocationField.Name
    else
       Comparer.Field:=TLocationField.Country;

    // Do the Sort !
    TArray.Sort<TLocation>(Locations,Comparer);

    // Set back again the current selected Location
    if Current.Name<>'' then
       TeeGrid1.Selected.Row:=FindLocation(Current);

  finally
    Comparer.Free;
  end;
end;

{ TLocationComparer }

function TLocationComparer.Compare(const Left, Right: TLocation): Integer;
var A,B : String;
begin
  if Field=TLocationField.Name then
  begin
    A:=Left.Name;
    B:=Right.Name;
  end
  else
  begin
    A:=Left.Country;
    B:=Right.Country;
  end;

  if Ascending then
     result:=CompareText(A,B)
  else
     result:=CompareText(B,A);
end;

end.
