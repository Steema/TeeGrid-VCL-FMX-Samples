unit Unit_Array;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  VCLTee.Editor.Grid, Vcl.StdCtrls, Vcl.ExtCtrls, System.UITypes,

  Tee.Renders, Tee.Grid.Columns, VCLTee.Editor.Render.Text, Tee.Grid.Rows;

type
  TFormArray = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    Button1: TButton;
    BInteger: TButton;
    BFloat: TButton;
    BString: TButton;
    BTObject: TButton;
    BRecord: TButton;
    Panel2: TPanel;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BIntegerClick(Sender: TObject);
    procedure BFloatClick(Sender: TObject);
    procedure BStringClick(Sender: TObject);
    procedure BRecordClick(Sender: TObject);
    procedure BTObjectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }

    procedure GridShowEditor(const Sender:TObject; const AEditor:TControl;
                             const AColumn:TColumn; const ARow:Integer);

    procedure SetExpander(const AColumn:String; const ARows:TRows);
  public
    { Public declarations }
  end;

var
  FormArray: TFormArray;

implementation

{$R *.dfm}

uses
  Unit_MyData, Tee.Grid.Data, Tee.Grid.Data.Rtti, VCLTee.Painter.GdiPlus,

  Tee.Grid.Totals, Tee.Grid.Bands;

// Show the TeeGrid editor dialog
procedure TFormArray.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

var
  MyIntegers : TArray<Integer>;

// Simple data: Array of Integer
procedure TFormArray.BIntegerClick(Sender: TObject);
var t : Integer;
begin
  SetLength(MyIntegers,10);

  for t:=0 to High(MyIntegers) do
      MyIntegers[t]:=Random(1000);

  TeeGrid1.Data:=TVirtualArrayData<Integer>.Create(MyIntegers);

  TeeGrid1.Footer.Clear;
end;

var
  MyFloats : TArray<Single>;

// Simple data: Array of Single
procedure TFormArray.BFloatClick(Sender: TObject);
var t : Integer;
begin
  SetLength(MyFloats,10);

  for t:=0 to High(MyFloats) do
      MyFloats[t]:=Random(1000)*0.01;

  TeeGrid1.Data:=TVirtualArrayData<Single>.Create(MyFloats);

  TeeGrid1.Footer.Clear;
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
procedure TFormArray.BStringClick(Sender: TObject);
var t : Integer;
begin
  SetLength(MyStrings,10);

  for t:=0 to High(MyStrings) do
      MyStrings[t]:=RandomString;

  TeeGrid1.Data:=TVirtualArrayData<String>.Create(MyStrings);

  TeeGrid1.Footer.Clear;
end;

// Show the TextRender editor dialog, to edit Footer band
procedure TFormArray.Button5Click(Sender: TObject);
begin
  if TeeGrid1.Footer.Count>1 then
     TTextRenderEditor.Edit(Self,TeeGrid1.Footer[1].Band)
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
procedure TFormArray.BTObjectClick(Sender: TObject);
begin
  Clean(MyCars);

  SetLength(MyCars,10);
  FillMyData(MyCars);

  TeeGrid1.Data:=TVirtualArrayData<TCar>.Create(MyCars);

  TeeGrid1.Footer.Clear;
end;

// Return a new Totals grid-band
function Totals(const AGrid:TTeeGrid):TColumnTotals;
begin
  result:=TColumnTotals.Create(AGrid.Changed,AGrid.Columns,AGrid.Data);

  result.Calculation.Add(AGrid.Columns['Name'],TColumnCalculation.Count);
  result.Calculation.Add(AGrid.Columns['Children'],TColumnCalculation.Sum);
  result.Calculation.Add(AGrid.Columns['Height'],TColumnCalculation.Average);

  result.Calculation.Add(AGrid.Columns['Address'].Items['Number'],TColumnCalculation.Max);

  result.Format.Font.Style:=[fsBold];
end;

// Change a column render to enable expanding / collapsing it
procedure TFormArray.SetExpander(const AColumn:String; const ARows:TRows);
var tmp : TColumn;
begin
  tmp:=TeeGrid1.Columns[AColumn];
  tmp.ChangeRender(TExpanderRender);
  TExpanderRender(tmp.Render).OnGetExpanded:=ARows.IsChildrenVisible;
end;

// Create a new Title grid-band
function Hello(const AGrid:TTeeGrid):TTitleBand;
begin
  result:=TTitleBand.Create(AGrid.Changed);

  result.Text:='Hello';

  // Cosmetic
  result.Format.Font.Size:=12;
  result.Format.Stroke.Visible:=True;
  result.Format.Stroke.Color:=TColors.Red;
end;

var
  MyData : TArray<TPerson>;

// Simple data: Array of Record
procedure TFormArray.BRecordClick(Sender: TObject);
var tmp : TColumnTotals;
begin
  SetLength(MyData,10);
  FillMyData(MyData);

  TeeGrid1.Data:=TVirtualArrayData<TPerson>.Create(MyData);

  // Set "Name" column as expandable
  SetExpander('Name',TeeGrid1.Rows);

  // Setup grid Footer bands
  TeeGrid1.Footer.Clear;

  tmp:=Totals(TeeGrid1);

  TeeGrid1.Footer.Add(TTotalsHeader.CreateTotals(tmp));
  TeeGrid1.Footer.Add(tmp);

  // Add a simple Title band
  TeeGrid1.Footer.Add(Hello(TeeGrid1));

  // Use a TDateTimePicker as editor control for "BirthDate" column
  TeeGrid1.Columns['BirthDate'].EditorClass:=TDateTimePicker;

  // Use a TComboBox as editor control for "Children" column
  TeeGrid1.Columns['Children'].EditorClass:=TComboBox;

  // Event to initialize the combobox when it is about being to show
  TeeGrid1.OnShowEditor:=GridShowEditor;
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
procedure TFormArray.GridShowEditor(const Sender:TObject; const AEditor:TControl;
                                 const AColumn:TColumn; const ARow:Integer);
begin
  if AColumn=TeeGrid1.Columns['Children'] then
     SetupCombo(AEditor as TComboBox, TeeGrid1.Data.AsString(AColumn,ARow) );
end;

procedure TFormArray.FormCreate(Sender: TObject);
begin
  // Change painter to use GDI+ plus
  TeeGrid1.Painter:=TGdiPlusPainter.Create;

  // Start with the Array of Record example
  BRecordClick(Self);
end;

procedure TFormArray.FormDestroy(Sender: TObject);
begin
  // Release memory, just in case
  Clean(MyCars);
end;

end.
