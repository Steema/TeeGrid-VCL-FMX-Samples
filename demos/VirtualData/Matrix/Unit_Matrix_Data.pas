unit Unit_Matrix_Data;

interface

{
  This example shows how to "bind" a TeeGrid with a two-dimensional array.

  Huge arrays are supported (compile in 64bit mode for memory limits).
  (eg: 1 million rows by 1 thousand columns = 1 billion cells)

  For example, an array of array of Double:

    type
      TMatrixColumn = TArray<Double>;
      TMatrix = TArray<TMatrixColumn>;

    var
      Matrix : TMatrix;


  There are two ways to do it:

  1) Using "virtual mode" OnGet and OnSet events to supply cells data:

     Data:= TVirtualModeData.Create(NumColumns,NumRows,50);
     Data.OnGetValue:= GetValue;
     Data.OnSetValue:= SetValue;


  2) Using Rtti (totally automatic):

     TeeGrid1.Data:= TVirtualArrayData<TMatrixColumn>.Create(Matrix);

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  VCLTee.Control, VCLTee.Grid,

  Tee.Grid.Columns;

type
  TFormMatrixGrid = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    RGMode: TRadioGroup;
    Button1: TButton;
    procedure RGModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    procedure GetValue(Sender:TObject;
                       const AColumn:TColumn;
                       const ARow:Integer;
                       var AValue:String);

    procedure SetValue(Sender:TObject;
                       const AColumn:TColumn;
                       const ARow:Integer;
                       var AValue:String);


    procedure AllColumnsFixedWidth(const AWidth:Single);
    procedure AllColumnsRightAligned;

    function NumColumns:Integer;
    function NumRows:Integer;

    procedure RttiMode;
    procedure VirtualMode;
  public
    { Public declarations }
  end;

var
  FormMatrixGrid: TFormMatrixGrid;

implementation

{$R *.dfm}

uses
  Tee.Grid.Data.Strings, Tee.Grid.Data.Rtti, Tee.Painter,
  VCLTee.Editor.Grid;

type
  TMatrixColumn = TArray<Double>;
  TMatrix = TArray<TMatrixColumn>;

var
  Matrix : TMatrix;

// Show the grid editor dialog
procedure TFormMatrixGrid.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

procedure TFormMatrixGrid.FormCreate(Sender: TObject);
var t : Integer;
begin
  // Rows,Columns
  SetLength(Matrix,1000,1000);

  // Note: for huge arrays compile in 64bit mode
  // SetLength(Matrix,1000000,1000)

  // Sample data at column 0
  for t:=0 to NumRows-1 do
      Matrix[t,0]:=t;

  // Bind data to grid
  RGModeClick(Self);
end;

// Returns a Matrix cell to paint at grid
procedure TFormMatrixGrid.GetValue(Sender: TObject; const AColumn: TColumn;
  const ARow: Integer; var AValue: String);
begin
  AValue:= Matrix[ARow, AColumn.Tag].ToString;
end;

// Changes a Matrix cell when the grid is edited
procedure TFormMatrixGrid.SetValue(Sender: TObject; const AColumn: TColumn;
  const ARow: Integer; var AValue: String);
begin
  Matrix[ARow, AColumn.Tag]:= StrToFloat(AValue);
end;

// Number of columns (second array dimension)
function TFormMatrixGrid.NumColumns: Integer;
begin
  if NumRows=0 then
     result:=0
  else
     result:=Length(Matrix[0]);
end;

// Number of rows (first array dimension)
function TFormMatrixGrid.NumRows: Integer;
begin
  result:=Length(Matrix);
end;

// Example: two ways to bind data
procedure TFormMatrixGrid.RGModeClick(Sender: TObject);
begin
  if RGMode.ItemIndex=0 then
     VirtualMode
  else
     RttiMode;
end;

// Use Rtti to automatically bind Matrix
procedure TFormMatrixGrid.RttiMode;
begin
  TeeGrid1.Data:= TVirtualArrayData<TMatrixColumn>.Create(Matrix);

// Alternative way:
//  TeeGrid1.Data:= TVirtualArray2DData<Double>.Create(Matrix);

  // cosmetic, optional
  AllColumnsFixedWidth(50);
end;

// Use virtual-mode events
procedure TFormMatrixGrid.VirtualMode;
var Data : TVirtualModeData;
    t : Integer;
begin
  Data:= TVirtualModeData.Create(NumColumns,NumRows,50);  // 50 = column width

  // Set initial text for column headers
  for t:=0 to NumColumns-1 do
      Data.Headers[t]:=IntToStr(t);

  // Setup events
  Data.OnGetValue:= GetValue;
  Data.OnSetValue:= SetValue;

  // Bind
  TeeGrid1.Data:= Data;

  // Cosmetic, optional
  AllColumnsRightAligned;
end;

// Cosmetic, set all columns width to a fixed value (not automatic)
procedure TFormMatrixGrid.AllColumnsFixedWidth(const AWidth:Single);
var tmp : TColumn;
    t : Integer;
begin
  for t:=0 to TeeGrid1.Columns.Count-1 do
  begin
    tmp:= TeeGrid1.Columns[t];

    tmp.Width.Automatic:= False;
    tmp.Width.Value:= AWidth;
  end;
end;

// Sets all columns horizontal alignment to Right
procedure TFormMatrixGrid.AllColumnsRightAligned;
var tmp : TColumn;
    t : Integer;
begin
  for t:=0 to TeeGrid1.Columns.Count-1 do
  begin
    tmp:=TeeGrid1.Columns[t];

    // Disable automatic
    tmp.TextAlignment:=TColumnTextAlign.Custom;

    // Set to custom
    tmp.TextAlign.Horizontal:=THorizontalAlign.Right;
  end;
end;

end.
