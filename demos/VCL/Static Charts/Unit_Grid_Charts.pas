unit Unit_Grid_Charts;

{
  Example showing how to paint static TeeChart controls inside TeeGrid cells.
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  VclTee.TeeGDIPlus, VCLTee.TeEngine,
  VCLTee.Series, VCLTee.Control, VCLTee.Grid, Vcl.ExtCtrls, VCLTee.TeeProcs,
  VCLTee.Chart, Tee.Format,

  Tee.GridData.Rtti, Tee.Grid.Columns, Tee.Renders, VCLTee.Picture,
  VCLTee.EditChar, VCLTee.Editor.Grid, Tee.Control;

type
  TFormGridCharts = class(TForm)
    Chart1: TChart;
    TeeGrid1: TTeeGrid;
    Series1: TBarSeries;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    PanelBottom: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TeeGrid1ClickedHeader(Sender: TObject);
  private
    { Private declarations }

    function CreateChart(const ARow,AWidth,AHeight:Integer):TBitmap;

    procedure PaintChart(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
    procedure PrepareChartColumn;
  public
    { Public declarations }
  end;

var
  FormGridCharts: TFormGridCharts;

implementation

{$R *.dfm}

// Sample data type
type
  TTemperatures = Array[0..3] of Integer;

  TLocation=record
  public
    Name : String;
    Temperatures : TTemperatures;

    procedure SetRandom;
  end;

{ TLocation }

procedure TLocation.SetRandom; // initialize "Temperatures" array
var t: Integer;
begin
  for t:=0 to High(Temperatures) do
      Temperatures[t]:=10+Random(20);
end;

var
  Locations : TArray<TLocation>;

procedure AddRandomData;
var t : Integer;
begin
  SetLength(Locations,4);

  Locations[0].Name:='New York';
  Locations[1].Name:='Barcelona';
  Locations[2].Name:='Tokyo';
  Locations[3].Name:='Sao Paulo';

  for t:=0 to High(Locations) do
      Locations[t].SetRandom;
end;

procedure TFormGridCharts.FormCreate(Sender: TObject);
begin
  AddRandomData;

  // Assign data to grid
  TeeGrid1.Data:=TVirtualArrayData<TLocation>.Create(Locations);

  // Set custom fixed 200 pixel row height
  TeeGrid1.Rows.Height.Automatic:=False;
  TeeGrid1.Rows.Height.Value:=200;

  PrepareChartColumn;

  TeeGrid1.ScrollBars.Vertical.Visible:=TScrollBarVisible.Show;

  Chart1.Hide;
end;

procedure TFormGridCharts.PrepareChartColumn;
var Temp : TColumn;
begin
  Temp:=TeeGrid1.Columns['Temperatures'];

  Temp.OnPaint:=PaintChart;

  // Custom width:
  Temp.Width.Automatic:=False;
  Temp.Width.Value:=300;

  Temp.ReadOnly:=True;
end;

// Just to show using the TeeGrid "OnClickedHeader" event
procedure TFormGridCharts.TeeGrid1ClickedHeader(Sender: TObject);
var S : String;
begin
  S:='Clicked header: '+Sender.ClassName;

  if Sender is TColumn then
     S:=S+' '+TColumn(Sender).Header.Text+' '+IntToStr(TColumn(Sender).Index);

  PanelBottom.Caption:=S;
end;

procedure TFormGridCharts.Button1Click(Sender: TObject);
begin
  EditChart(Self,Chart1);

  TeeGrid1.Invalidate;
end;

procedure TFormGridCharts.Button2Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

procedure FillSeries(const ASeries:TChartSeries; const AValues:TTemperatures);
begin
  ASeries.Clear;

  ASeries.Add(AValues[0]);
  ASeries.Add(AValues[1]);
  ASeries.Add(AValues[2]);
  ASeries.Add(AValues[3]);
end;

// CHART

function TFormGridCharts.CreateChart(const ARow,AWidth,AHeight:Integer):TBitmap;
var R : TRect;
begin
  FillSeries(Series1, Locations[ARow].Temperatures);

  Chart1.Title.Caption:=Locations[ARow].Name;

  Chart1.Gradient.Visible := False;
  Chart1.Walls.Visible := False;
  Chart1.Color := clGray;

  R:=TRect.Create(0,0,AWidth-1,AHeight-1);

  result:=Chart1.TeeCreateBitmap(Chart1.Color,R);
end;

procedure TFormGridCharts.PaintChart(const Sender: TColumn;
  var AData: TRenderData; var DefaultPaint: Boolean);
var Picture : Tee.Format.TPicture;
    Bitmap : TBitmap;
begin
  DefaultPaint:=False;

  Bitmap:=CreateChart(AData.Row,Round(AData.Bounds.Width),Round(AData.Bounds.Height));
  Picture:=TVCLPicture.From(Bitmap);
  Picture.Transparent := True;   //example transparency
  Picture.TransparentColor := clGray;
  Picture.TransparentMode := TTeeTransparentMode.ttmFixed;
  try
    TeeGrid1.Painter.Draw(Picture,AData.Bounds);
  finally
    Bitmap.Free;
    Picture.Free;
  end;
end;

end.
