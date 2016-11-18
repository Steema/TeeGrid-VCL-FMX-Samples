unit Unit_Array;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.ComCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  VCLTee.Editor.Grid, Vcl.StdCtrls, Vcl.ExtCtrls, System.UITypes,

  Tee.Renders, Tee.Grid.Columns, VCLTee.Editor.Render.Text;

type
  TForm43 = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    BTObject: TButton;
    BRecord: TButton;
    Panel2: TPanel;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure BRecordClick(Sender: TObject);
    procedure BTObjectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }

    procedure GridShowEditor(const Sender:TObject; const AEditor:TControl;
                             const AColumn:TColumn; const ARow:Integer);

    procedure SetExpander(const AColumn:String);
  public
    { Public declarations }
  end;

var
  Form43: TForm43;

implementation

{$R *.dfm}

uses
  Unit_MyData, Tee.Grid.Data, Tee.Grid.Data.Rtti,

  Tee.Grid.Totals, Tee.Grid.Bands;

procedure TForm43.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

var
  MyIntegers : TArray<Integer>;

procedure TForm43.Button2Click(Sender: TObject);
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

procedure TForm43.Button3Click(Sender: TObject);
var t : Integer;
begin
  SetLength(MyFloats,10);

  for t:=0 to High(MyFloats) do
      MyFloats[t]:=Random(1000)*0.01;

  TeeGrid1.Data:=TVirtualArrayData<Single>.Create(MyFloats);

  TeeGrid1.Footer.Clear;
end;

function RandomString:String;
const
  Samples:Array[0..3] of String=('Red','Blue','Yellow','Green');
begin
  result:=Samples[Random(Length(Samples))];
end;

var
  MyStrings : TArray<String>;

procedure TForm43.Button4Click(Sender: TObject);
var t : Integer;
begin
  SetLength(MyStrings,10);

  for t:=0 to High(MyStrings) do
      MyStrings[t]:=RandomString;

  TeeGrid1.Data:=TVirtualArrayData<String>.Create(MyStrings);

  TeeGrid1.Footer.Clear;
end;

procedure TForm43.Button5Click(Sender: TObject);
begin
  TTextRenderEditor.Edit(Self,TeeGrid1.Footer[1].Band)
end;

procedure Clean(const ACars:TArray<TCar>);
var t : Integer;
begin
  for t:=Low(ACars) to High(ACars) do
      ACars[t].Free;
end;

var
  MyCars : TArray<TCar>;

procedure TForm43.BTObjectClick(Sender: TObject);
begin
  Clean(MyCars);

  SetLength(MyCars,10);
  FillMyData(MyCars);

  TeeGrid1.Data:=TVirtualArrayData<TCar>.Create(MyCars);

  TeeGrid1.Footer.Clear;
end;

function Totals(const AColumns:TColumns; const AData:TVirtualData):TColumnTotals;
begin
  result:=TColumnTotals.Create(nil,AColumns,AData);

  result.Calculation.Add(AColumns['Name'],TColumnCalculation.Count);
  result.Calculation.Add(AColumns['Children'],TColumnCalculation.Sum);
  result.Calculation.Add(AColumns['Height'],TColumnCalculation.Average);

  result.Calculation.Add(AColumns['Address'].Items['Number'],TColumnCalculation.Max);

  result.Format.Font.Style:=[fsBold];
end;

procedure TForm43.SetExpander(const AColumn:String);
var tmp : TColumn;
begin
  tmp:=TeeGrid1.Columns[AColumn];
  tmp.ChangeRender(TExpanderRender);

  TExpanderRender(tmp.Render).OnGetExpanded:=TeeGrid1.Grid.GetExpanded;
end;

function Hello(const AGrid:TTeeGrid):TTitleBand;
begin
  result:=TTitleBand.Create(AGrid.Changed);

  result.Text:='Hello';

  result.Format.Font.Size:=12;
  result.Format.Stroke.Visible:=True;
  result.Format.Stroke.Color:=TColors.Red;
end;

var
  MyData : TArray<TPerson>;

procedure TForm43.BRecordClick(Sender: TObject);
var tmp : TColumnTotals;
begin
  SetLength(MyData,10);
  FillMyData(MyData);

  TeeGrid1.Data:=TVirtualArrayData<TPerson>.Create(MyData);

//  TeeGrid1.Header.Hide;

  SetExpander('Name');

  TeeGrid1.Footer.Clear;

  tmp:=Totals(TeeGrid1.Columns,TeeGrid1.Data);

  TeeGrid1.Footer.Add(TTotalsHeader.CreateTotals(tmp));
  TeeGrid1.Footer.Add(tmp);

  TeeGrid1.Footer.Add(Hello(TeeGrid1));

  TeeGrid1.Columns['BirthDate'].EditorClass:=TDateTimePicker;

  TeeGrid1.Columns['Children'].EditorClass:=TComboBox;

  TeeGrid1.OnShowEditor:=GridShowEditor;
end;

procedure SetupCombo(const AComboBox:TComboBox; const AText:String);
var t : Integer;
begin
  AComboBox.Clear;

  for t:=0 to 15 do
      AComboBox.Items.Add(t.ToString);

  AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
end;

procedure TForm43.GridShowEditor(const Sender:TObject; const AEditor:TControl;
                                 const AColumn:TColumn; const ARow:Integer);
begin
  if AColumn=TeeGrid1.Columns['Children'] then
     SetupCombo(AEditor as TComboBox, TeeGrid1.Data.AsString(AColumn,ARow) );
end;

procedure TForm43.FormCreate(Sender: TObject);
begin
  BRecordClick(Self);
end;

procedure TForm43.FormDestroy(Sender: TObject);
begin
  Clean(MyCars);
end;

end.
