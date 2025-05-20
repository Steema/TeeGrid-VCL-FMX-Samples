unit Unit_Chart_and_Grid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid;

type
  TChart_in_Grid = class(TForm)
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    procedure RandomData;
  public
    { Public declarations }
  end;

var
  Chart_in_Grid: TChart_in_Grid;

implementation

{$R *.dfm}

uses
  Tee.GridData.Rtti, Chart, Series;

type
  TMyData=record
    Values : Array of Integer;
    Chart : TChart;
  end;

var
  MyData : TArray<TMyData>;

procedure TChart_in_Grid.RandomData;

  function NewChart(const AIndex:Integer):TChart;
  begin
    result:=TChart.Create(Self);
    result.AddSeries(TBarSeries).AddArray(MyData[AIndex].Values);
  end;

begin
  SetLength(MyData,3);

  MyData[0].Values:=[20,40,15];
  MyData[0].Chart:=NewChart(0);

  MyData[1].Values:=[120,30,150,60];
  MyData[1].Chart:=NewChart(1);

  MyData[2].Values:=[5,9];
  MyData[2].Chart:=NewChart(2);
end;

procedure TChart_in_Grid.FormCreate(Sender: TObject);
begin
  RandomData;

  TeeGrid1.Data:=TVirtualArrayData<TMyData>.Create(MyData);

  // Cosmetics

  TeeGrid1.Rows.Height.Automatic:=False;
  TeeGrid1.Rows.Height.Value:=250;

  TeeGrid1.Columns['Chart'].ReadOnly:=True;
  TeeGrid1.Columns['Chart'].Width.Value:=400;
end;

end.
