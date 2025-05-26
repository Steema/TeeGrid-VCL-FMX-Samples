unit ArrayData;

{
  Example showing how to display and edit different kinds of Arrays using
  TeeGrid.

  1) Array of records:

     TeeGrid1.Data:= TVirtualData<TArray<TPerson>>.Create(MyPersons);

  2) Array of simple types:

      TeeGrid1.Data:= TVirtualData<TArray<Integer>>.Create(MyIntegers);

      TeeGrid1.Data:= TVirtualData<TArray<Single>>.Create(MyFloats);

      TeeGrid1.Data:= TVirtualData<TArray<String>>.Create(MyStrings);

  3) Array of object instances:

     TeeGrid1.Data:= TVirtualData<TArray<TCar>>.Create(MyCars);

}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMXTee.Control, FMXTee.Grid,
  FMX.Objects, FMX.Layouts,
  Tee.Renders, Tee.Grid.Columns, Tee.Grid.Rows,
  Tee.Grid.Bands, FMX.ListBox, FMX.DateTimeCtrls;

type
  TArrayAsDataForm = class(TForm)
    Layout1: TLayout;
    Rectangle1: TRectangle;
    TeeGrid1: TTeeGrid;
    BArrayRecords: TButton;
    BArrayIntegers: TButton;
    BArrayFloat: TButton;
    BArrayString: TButton;
    BArrayObject: TButton;
    procedure BEditClick(Sender: TObject);
    procedure BArrayRecordsClick(Sender: TObject);
    procedure BArrayIntegersClick(Sender: TObject);
    procedure BArrayFloatClick(Sender: TObject);
    procedure BArrayStringClick(Sender: TObject);
    procedure BArrayObjectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    SampleHeader : TTextBand;
    procedure SetExpander(const AColumn: String; const ARows: TRows);
    procedure AddSampleFooter;
    procedure SetupCellEditors;
    procedure GridCellEditing(const Sender: TObject; const AEditor: TControl;
      const AColumn: TColumn; const ARow: Integer);
  public
  end;

var
  ArrayAsDataForm: TArrayAsDataForm;

implementation

{$R *.fmx}

uses
  Unit_MyData, Tee.GridData, Tee.GridData.Rtti,
  FMXTee.Painter, Tee.Format,
  Tee.Grid.Totals;

var
  MyData : TArray<TPerson>;


// Change a column render to enable expanding / collapsing it
procedure TArrayAsDataForm.SetExpander(const AColumn:String; const ARows:TRows);
var tmp : TColumn;
begin
  tmp:=TeeGrid1.Columns[AColumn];

  tmp.Render:=TExpanderRender.Create(tmp.Changed);

  TExpanderRender(tmp.Render).OnGetExpanded:=ARows.IsChildrenVisible;
end;

// Return a new Totals grid-band
function Totals(const ACollection:TCollection):TColumnTotals;
begin
  result:=TColumnTotals.Create(ACollection);

  result.Calculation.Add('Name',TColumnCalculation.Count);
  result.Calculation.Add('Children',TColumnCalculation.Sum);
  result.Calculation.Add('Height',TColumnCalculation.Average);

  result.Calculation.Add(result.Columns['Address'].Items['Number'],TColumnCalculation.Max);

  result.Format.Font.Style:=[TFontStyle.fsBold];
end;

// Create a new Title grid-band
function NewTitle(const ACollection:TCollection; const AText:String):TTextBand;
begin
  result:=TTextBand.Create(ACollection);

  result.Text:=AText;

  // Cosmetic
  result.Format.Font.Size:=12;
  result.Format.Stroke.Visible:=True;
  result.Format.Stroke.Color:=TColors.Red;
end;

procedure TArrayAsDataForm.AddSampleFooter;
var tmp : TTextBand;
begin
  tmp:=NewTitle(TeeGrid1.Footer,'Footer Sample'#13'Text');

  tmp.Format.Brush.Show;
  tmp.Format.Brush.Gradient.Show;
  tmp.Format.Brush.Gradient.Direction:=TGradientDirection.Horizontal;
end;


var
  MyFloats : TArray<Single>;

// Simple data: Array of Single
procedure TArrayAsDataForm.BArrayFloatClick(Sender: TObject);
var t : Integer;
begin
  SetLength(MyFloats,200);

  for t:=0 to High(MyFloats) do
      MyFloats[t]:=Random(1000)*0.01;

  TeeGrid1.Data:=TVirtualData<TArray<Single>>.Create(MyFloats);

  TeeGrid1.Footer.Clear;
end;

var
  MyIntegers : TArray<Integer>;

// Simple data: Array of Integer
procedure TArrayAsDataForm.BArrayIntegersClick(Sender: TObject);
var t : Integer;
begin
  SetLength(MyIntegers,100);

  for t:=0 to High(MyIntegers) do
      MyIntegers[t]:=Random(1000);

  TeeGrid1.Data:=TVirtualData<TArray<Integer>>.Create(MyIntegers);

  TeeGrid1.Footer.Clear;
end;

// Destroy all objects in Array
procedure Clean(const ACars:TArray<TCar>);
var t : Integer;
begin
  for t:=Low(ACars) to High(ACars) do
      ACars[t].Free;
end;

var
  MyCars : TArray<TCar>;

// Simple data: Array of TObject
procedure TArrayAsDataForm.BArrayObjectClick(Sender: TObject);
begin
  Clean(MyCars);

  SetLength(MyCars,10);
  FillMyData(MyCars);

  TeeGrid1.Data:=TVirtualData<TArray<TCar>>.Create(MyCars);

  TeeGrid1.Footer.Clear;

  SetupCellEditors;
end;

procedure TArrayAsDataForm.BArrayRecordsClick(Sender: TObject);
var tmp : TColumnTotals;
begin
  SetLength(MyData,10);
  FillMyData(MyData);

  TeeGrid1.Data:=TVirtualData<TArray<TPerson>>.Create(MyData);

  // Set "Name" column as expandable
  SetExpander('Name',TeeGrid1.Rows);

  // Setup grid Footer bands
  TeeGrid1.Footer.Clear;

  tmp:=Totals(TeeGrid1.Footer);
  TTotalsHeader.CreateTotals(TeeGrid1.Footer,tmp);

  // Add a simple Title band to footer
  AddSampleFooter;

  // Destroy the previously created SampleHeader, if any
  SampleHeader.Free;

  // Add a simple Title band to headers
  SampleHeader:=NewTitle(TeeGrid1.Headers,'Header Sample'#13#10'Text');

  // Move it to top
  SampleHeader.Index:=0;

  // Just to show how to customize cell editing using combobox, datetime picker etc
  SetupCellEditors;

  // Event to initialize the combobox when it is about being to show
  TeeGrid1.OnCellEditing:=GridCellEditing;
end;

// Return a random string
function RandomString:String;
const
  Samples:Array[0..3] of String=('Red','Blue','Yellow','Green');
begin
  result:=Samples[Random(Length(Samples))];
end;

var
  MyStrings : TArray<String>;

// Simple data: Array of String
procedure TArrayAsDataForm.BArrayStringClick(Sender: TObject);
var t : Integer;
begin
  SetLength(MyStrings,10);

  for t:=0 to High(MyStrings) do
      MyStrings[t]:=RandomString;

  TeeGrid1.Data:=TVirtualData<TArray<String>>.Create(MyStrings);

  TeeGrid1.Footer.Clear;
end;

// Example, just fill the combobox with numbers
procedure SetupCombo(const AComboBox:TComboBox; const AText:String);
var t : Integer;
begin
  AComboBox.Clear;

  for t:=0 to 15 do
      AComboBox.Items.Add(t.ToString);

  // Select the current combobox index, as position of AText
  AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
end;

// Event called when the grid is going to show an editor control.
// For "Children" column, prepare the editor control as a combobox.
procedure TArrayAsDataForm.GridCellEditing(const Sender:TObject; const AEditor:TControl;
                                 const AColumn:TColumn; const ARow:Integer);
begin
  if AColumn=TeeGrid1.Columns['Children'] then
     SetupCombo(AEditor as TComboBox, TeeGrid1.Data.AsString(AColumn,ARow) );
end;


procedure TArrayAsDataForm.SetupCellEditors;
begin
  // Use a TDateTimePicker as editor control for "BirthDate" column
  TeeGrid1.Columns['BirthDate'].EditorClass:=TDateEdit;

  // Use a TComboBox as editor control for "Children" column
  TeeGrid1.Columns['Children'].EditorClass:=TComboBox;
end;

procedure TArrayAsDataForm.BEditClick(Sender: TObject);
begin
//  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

procedure TArrayAsDataForm.FormDestroy(Sender: TObject);
begin
  Clean(MyCars); // Free memory
end;

end.
