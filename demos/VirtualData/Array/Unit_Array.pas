unit Unit_Array;

interface

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

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  VCLTee.Editor.Grid, Vcl.StdCtrls, Vcl.ExtCtrls, System.UITypes,

  Tee.Renders, Tee.Grid.Columns, VCLTee.Editor.Render.Text, Tee.Grid.Rows,
  Tee.Grid.Bands;

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
    Button2: TButton;
    CBGdiPlus: TCheckBox;
    BGDIPlusEdit: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BIntegerClick(Sender: TObject);
    procedure BFloatClick(Sender: TObject);
    procedure BStringClick(Sender: TObject);
    procedure BRecordClick(Sender: TObject);
    procedure BTObjectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBGdiPlusClick(Sender: TObject);
    procedure BGDIPlusEditClick(Sender: TObject);
  private
    { Private declarations }

    SampleHeader : TTextBand;

    procedure AddSampleFooter;
    procedure GDIPlusChanged(Sender: TObject);
    procedure GridCellEditing(const Sender:TObject; const AEditor:TControl;
                              const AColumn:TColumn; const ARow:Integer);

    procedure SetExpander(const AColumn:String; const ARows:TRows);
    procedure SetupCellEditors;
  public
    { Public declarations }
  end;

var
  FormArray: TFormArray;

implementation

{$R *.dfm}

uses
  Unit_MyData, Tee.GridData, Tee.GridData.Rtti,

  VCLTee.Painter.GdiPlus, VCLTee.Painter, Tee.Format,

  Tee.Grid.Totals, VCLTee.Editor.Grid.Bands, VCLTee.Editor.Painter.GDIPlus;

// Show the TeeGrid editor dialog
procedure TFormArray.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

procedure TFormArray.Button2Click(Sender: TObject);
begin
  TGridBandsEditor.Edit(Self,TeeGrid1.Headers);
end;

var
  MyIntegers : TArray<Integer>;

// Simple data: Array of Integer
procedure TFormArray.BIntegerClick(Sender: TObject);
var t : Integer;
begin
  SetLength(MyIntegers,100);

  for t:=0 to High(MyIntegers) do
      MyIntegers[t]:=Random(1000);

  //  TeeGrid1.Data:=TVirtualData<TArray<Integer>>.Create(MyIntegers);  // <-- same as below

  TeeGrid1.Data:=TVirtualArrayData<Integer>.Create(MyIntegers);

  TeeGrid1.Footer.Clear;
end;

var
  MyFloats : TArray<Single>;

// Simple data: Array of Single
procedure TFormArray.BFloatClick(Sender: TObject);
var t : Integer;
begin
  SetLength(MyFloats,200);

  for t:=0 to High(MyFloats) do
      MyFloats[t]:=Random(1000)*0.01;

  TeeGrid1.Data:=TVirtualData<TArray<Single>>.Create(MyFloats);

  TeeGrid1.Footer.Clear;
end;

procedure TFormArray.GDIPlusChanged(Sender: TObject);
begin
  TeeGrid1.Invalidate;
end;

procedure TFormArray.BGDIPlusEditClick(Sender: TObject);
begin
  TGDIPlusEditor.Edit(Self,TeeGrid1.Painter as TGdiPlusPainter,GDIPlusChanged);
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

  TeeGrid1.Data:=TVirtualData<TArray<String>>.Create(MyStrings);

  TeeGrid1.Footer.Clear;
end;

// Show the TextRender editor dialog, to edit Footer band
procedure TFormArray.Button5Click(Sender: TObject);
begin
  TGridBandsEditor.Edit(Self,TeeGrid1.Footer);
end;

procedure TFormArray.CBGdiPlusClick(Sender: TObject);
begin
  if CBGdiPlus.Checked then
     TeeGrid1.Painter:=TGdiPlusPainter.Create
  else
     TeeGrid1.Painter:=TGDIPainter.Create(TeeGrid1.Canvas);

  BGDIPlusEdit.Enabled:=CBGdiPlus.Checked;
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

  TeeGrid1.Data:=TVirtualData<TArray<TCar>>.Create(MyCars);

  TeeGrid1.Footer.Clear;

  SetupCellEditors;
end;

// Return a new Totals grid-band
function Totals(const ACollection:TCollection):TColumnTotals;
begin
  result:=TColumnTotals.Create(ACollection);

  result.Calculation.Add('Name',TColumnCalculation.Count);
  result.Calculation.Add('Children',TColumnCalculation.Sum);
  result.Calculation.Add('Height',TColumnCalculation.Average);

  result.Calculation.Add(result.Columns['Address'].Items['Number'],TColumnCalculation.Max);

  result.Format.Font.Style:=[fsBold];
end;

// Change a column render to enable expanding / collapsing it
procedure TFormArray.SetExpander(const AColumn:String; const ARows:TRows);
var tmp : TColumn;
begin
  tmp:=TeeGrid1.Columns[AColumn];

  if tmp<>nil then
  begin
    tmp.Render:=TExpanderRender.Create(tmp.Changed);

    TExpanderRender(tmp.Render).OnGetExpanded:=ARows.IsChildrenVisible;
  end;
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

procedure TFormArray.AddSampleFooter;
var tmp : TTextBand;
begin
  tmp:=NewTitle(TeeGrid1.Footer,'Footer Sample'#13'Text');

  tmp.Format.Brush.Show;
  tmp.Format.Brush.Gradient.Show;
  tmp.Format.Brush.Gradient.Direction:=TGradientDirection.Horizontal;
end;

var
  MyData : TArray<TPerson>;

// Simple data: Array of Record
procedure TFormArray.BRecordClick(Sender: TObject);
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
procedure TFormArray.GridCellEditing(const Sender:TObject; const AEditor:TControl;
                                 const AColumn:TColumn; const ARow:Integer);
begin
  if AColumn=TeeGrid1.Columns['Children'] then
     SetupCombo(AEditor as TComboBox, TeeGrid1.Data.AsString(AColumn,ARow) );
end;

procedure TFormArray.FormCreate(Sender: TObject);
begin
  // Change painter to use GDI+ plus
  CBGdiPlusClick(Self);

  // Start with the Array of Record example
  BRecordClick(Self);

  // Enable automatic multi-line text in cells
  TeeGrid1.Rows.Height.Automatic:=True;
end;

procedure TFormArray.FormDestroy(Sender: TObject);
begin
  // Release memory, just in case
  Clean(MyCars);
end;

procedure TFormArray.SetupCellEditors;
begin
  // Use a TDateTimePicker as editor control for "BirthDate" column
  TeeGrid1.Columns['BirthDate'].EditorClass:=TDateTimePicker;

  // Use a TComboBox as editor control for "Children" column
  TeeGrid1.Columns['Children'].EditorClass:=TComboBox;
end;

end.
