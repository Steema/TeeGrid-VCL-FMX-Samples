unit Unit_TeeGrid_TChart_Dataset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VCLTee.Grid, TeeGDIPlus, TeEngine, Series, ExtCtrls, TeeProcs, Chart,

  // Tee.GridData.DB,

  DB, TeeData, VCLTee.Control;

type
  TForm224 = class(TForm)
    DataSource1: TDataSource;
    ChartDataSet1: TChartDataSet;
    Chart1: TChart;
    Series1: TLineSeries;
    Splitter1: TSplitter;
    TeeGrid1: TTeeGrid;
    procedure Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form224: TForm224;

implementation

{$R *.dfm}

procedure TForm224.Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TeeGrid1.Selected.Row:=ValueIndex;
end;

end.
