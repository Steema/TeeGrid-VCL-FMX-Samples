unit Unit_Ticker;

interface

{
  Example using TeeGrid with a TGridTicker component to automatically
  refresh grid cells with colors depending on cell value changes.
}

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ExtCtrls,
  {Vcl.}ComCtrls, {Vcl.}StdCtrls,

  VCLTee.Control, VCLTee.Grid, VCLTee.Editor.Grid.Ticker,

  Tee.GridData.Strings, Tee.Grid.Ticker, Tee.Painter,
  Tee.Grid.Columns, Tee.Format, Tee.Renders;

type
  TTickerForm = class(TForm)
    TeeGrid1: TTeeGrid;
    Timer1: TTimer;
    PanelEditor: TPanel;
    Panel1: TPanel;
    Label1: TLabel;
    TBSpeed: TTrackBar;
    LSpeed: TLabel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TBSpeedChange(Sender: TObject);
  private
    { Private declarations }

    Data : TStringsData;
    Ticker : TGridTicker;

    TickerEditor : TGridTickerEditor;

    const
      Product='Product';

    procedure ColumnTextAlign;
    procedure CustomFormat;
    procedure FillNames;
    procedure FillRandomValues;
    procedure RandomCell(out ACol,ARow:Integer);
    procedure RefreshSpeed;
  public
    { Public declarations }
  end;

var
  TickerForm: TTickerForm;

implementation

{$R *.dfm}

uses
  System.UITypes, Tee.Grid, Tee.Grid.RowGroup;

procedure TTickerForm.FormCreate(Sender: TObject);
begin
  // Create grid data, for example "strings"
  Data:=TStringsData.Create(5,8);

  // Set data to TeeGrid
  TeeGrid1.Data:=Data;

  // Fill data with random values
  FillNames;
  FillRandomValues;

  // Adjust column alignment and cosmetic formatting
  ColumnTextAlign;
  CustomFormat;

  RefreshSpeed;

  // Create Ticker
  Ticker:=TGridTicker.Create(TeeGrid1.Grid.Current);

  TickerEditor:=TGridTickerEditor.Embedd(Self,PanelEditor,Ticker);
end;

procedure TTickerForm.RefreshSpeed;
begin
  LSpeed.Caption:=IntToStr(Timer1.Interval)+' msec';
end;

procedure TTickerForm.FormDestroy(Sender: TObject);
begin
  Ticker.Free; // avoid memory leak
end;

procedure TTickerForm.FormShow(Sender: TObject);
begin
  // Start updating values
  Timer1.Enabled:=True;
end;

// Return a random cell coordinate (Column and Row)
procedure TTickerForm.RandomCell(out ACol,ARow:Integer);
begin
  // Choose a column different than the "Product" column
  repeat
    ACol:=Random(TeeGrid1.Columns.Count);
  until TeeGrid1.Columns[ACol].Header.Text<>Product;

  // Random row
  ARow:=Random(TeeGrid1.Data.Count);
end;

procedure TTickerForm.TBSpeedChange(Sender: TObject);
begin
  Timer1.Interval:=TBSpeed.Position;
  RefreshSpeed;
end;

// Just for this example, update grid cell values using a Timer
procedure TTickerForm.Timer1Timer(Sender: TObject);
var Col, Row : Integer;

    tmp : TColumn;

    OldValue : Integer;
begin
  // Choose a random cell
  RandomCell(Col,Row);

  tmp:=TeeGrid1.Columns[Col];

  // Get current cell value
  OldValue:=StrToInt(Data.AsString(tmp,Row));

  // Add some random and set new value to grid data
  Data.SetValue(tmp,Row,IntToStr(OldValue+Random(100)-50));

  // Update Ticker
  Ticker.Change(Col,Row,OldValue);
end;

// Just fill grid cells with random values
procedure TTickerForm.FillRandomValues;
var Col,Row : Integer;
begin
  for Col:=1 to TeeGrid1.Columns.Count-1 do
      for Row:=0 to Data.Count-1 do
          Data[Col,Row]:=IntToStr(Random(Col*2000));
end;

// Change first column font style
procedure TTickerForm.CustomFormat;
var Col : TColumn;
begin
  Col:=TeeGrid1.Columns[0];

  Col.ParentFormat:=False;
  Col.Format.Font.Style:=[fsBold];
  Col.Format.Font.Color:=clNavy;
end;

// For all numeric columns, set right text alignment
procedure TTickerForm.ColumnTextAlign;
var Col : Integer;
    tmp : TColumn;
begin
  for Col:=1 to TeeGrid1.Columns.Count-1 do
  begin
    tmp:=TeeGrid1.Columns[Col];

    tmp.ParentFormat:=False;
    tmp.TextAlignment:=TColumnTextAlign.Custom;
    tmp.TextAlign.Horizontal:=THorizontalAlign.Right;
  end;
end;

// Initialize data headers
procedure TTickerForm.FillNames;
begin
  Data.Headers[0]:=Product;
  Data.Headers[1]:='Sales';
  Data.Headers[2]:='Stock';
  Data.Headers[3]:='Orders';
  Data.Headers[4]:='Returns';

  // First column cells
  Data[0,0]:='Cars';
  Data[0,1]:='Chairs';
  Data[0,2]:='Keyboards';
  Data[0,3]:='Lamps';
  Data[0,4]:='Monitors';
  Data[0,5]:='Tables';
  Data[0,6]:='Umbrellas';
  Data[0,7]:='Windows';
end;

end.
