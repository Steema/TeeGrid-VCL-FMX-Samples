unit Unit_TeeGrid_TChart_Dataset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VCLTee.Grid, TeeGDIPlus, TeEngine, Series, ExtCtrls, TeeProcs, Chart,
  DB, TeeData;

type
  TForm224 = class(TForm)
    DataSource1: TDataSource;
    ChartDataSet1: TChartDataSet;
    Chart1: TChart;
    Series1: TLineSeries;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    Grid : TTeeGrid;
  public
    { Public declarations }
  end;

var
  Form224: TForm224;

implementation

{$R *.dfm}

uses
  Tee.Grid.Data.DB;

procedure TForm224.FormCreate(Sender: TObject);
begin
  Grid:=TTeeGrid.Create(Self);
  Grid.Align:=alClient;
  Grid.Parent:=Self;

  Grid.Data:=TVirtualDBData.From(DataSource1);
end;

end.
