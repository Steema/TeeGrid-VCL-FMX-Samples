unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  Vcl.ExtCtrls, Tee.Renders.SparkLines, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    TeeGrid1: TTeeGrid;
    Label1: TLabel;
    CBStyle: TComboBox;
    CheckBox1: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure CBStyleChange(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }

    Sparks : TSparkLines;

    procedure AddRandomData;
    procedure PaintSparks(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Tee.GridData.Strings, Tee.Painter, UITypes;

// For simplicity only, lets use string cells as data.
procedure TMainForm.AddRandomData;
var Data : TStringsData;
    Row, Col : Integer;
begin
  // Create data
  Data:=TStringsData.Create(11,20);

  // Set headers
  for Col:=1 to Data.Columns-1 do
      Data.Headers[Col]:=IntToStr(Col);

  // Set cell values
  for Row:=0 to Data.Rows-1 do
  begin
    Data[0,Row]:='Row '+IntToStr(Row);

    Data[1,Row]:=IntToStr(Random(1000));

    for Col:=2 to Data.Columns-1 do
        Data[Col,Row]:=IntToStr(StrToInt(Data[Col-1,Row])+Random(100)-49);
  end;

  // Set data to Grid
  TeeGrid1.Data:=Data;

  // Set numeric columns
  for Col:=1 to TeeGrid1.Columns.Count-1 do
      TeeGrid1.Columns[Col].InitAlign(THorizontalAlign.Right);
end;

procedure TMainForm.CBStyleChange(Sender: TObject);
begin
  Sparks.Style:=TSparkStyle(CBStyle.ItemIndex);
end;

procedure TMainForm.CheckBox1Click(Sender: TObject);
begin
  TeeGrid1.Invalidate;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  TeeGrid1.Rows.Hover.FullRow:=True;

  TeeGrid1.Rows.Hover.Format.Stroke.Hide;
  TeeGrid1.Rows.Hover.Format.Brush.Show;
  TeeGrid1.Rows.Hover.Format.Brush.Color:=TColors.Wheat;

  TeeGrid1.Rows.Hover.PaintText:=True;

//  TeeGrid1.Selected.FullRow:=True;

  AddRandomData;

  Sparks:=TSparkLines.AddTo(TeeGrid1.Grid, 1,10, 'Sparks');

  Sparks.Stroke.Size:=2;
  Sparks.Stroke.Color:=TColors.Blue;

  Sparks.Format.Brush.Color:=TColors.Lightsteelblue;

  Sparks.OnPaint:=PaintSparks;
end;

// Optional, just to demonstrate how to customize per-row
procedure TMainForm.PaintSparks(Sender: TObject);
begin
  if not CheckBox1.Checked then
     Exit;

  if Sparks.Row=9 then
     Sparks.Style:=TSparkStyle.Area
  else
  if Sparks.Row in [7,11,14] then
     Sparks.Style:=TSparkStyle.Bar
  else
     Sparks.Style:=TSparkStyle(CBStyle.ItemIndex);

  if Sparks.Row in [3,11,16] then
     Sparks.Stroke.Color:=TColors.Red
  else
     Sparks.Stroke.Color:=TColors.Blue;
end;

end.
