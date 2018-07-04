unit Unit_FMX_Ticker;

interface

{
  Example using TeeGrid with a TGridTicker component to automatically
  refresh grid cells with colors depending on cell value changes.
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMXTee.Control,
  FMXTee.Grid, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,

  Tee.GridData.Strings, Tee.Grid.Ticker, FMX.Objects;

type
  TTickerForm = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    TBSpeed: TTrackBar;
    LSpeed: TLabel;
    TeeGrid1: TTeeGrid;
    LayoutEditor: TLayout;
    Timer1: TTimer;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TBSpeedChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

    Data : TStringsData;
    Ticker : TGridTicker;

    //TickerEditor : TGridTickerEditor;

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

{$R *.fmx}

uses
  Tee.Grid.Columns, FMXTee.Editor.Grid.Ticker,

  // Just for inline hints:
  Tee.Painter, Tee.Grid, Tee.Format, Tee.Grid.RowGroup;

procedure TTickerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := False;
end;

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

  TGridTickerEditor.Embedd(Self,LayoutEditor,Ticker);

  // Start updating values
  Timer1.Enabled:=True;
end;

procedure TTickerForm.FormDestroy(Sender: TObject);
begin
  Ticker.Free; // avoid memory leak
end;

procedure TTickerForm.FormResize(Sender: TObject);
begin
  if Width>Height then
  begin
    LayoutEditor.Align:=TAlignLayout.Right;
    LayoutEditor.Width:=300;
  end
  else
  begin
    LayoutEditor.Align:=TAlignLayout.Bottom;
    LayoutEditor.Height:=234;
  end;
end;

procedure TTickerForm.TBSpeedChange(Sender: TObject);
begin
  Timer1.Interval:=Round(TBSpeed.Value);
  RefreshSpeed;
end;

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
  Col.Format.Font.Style:=[TFontStyle.fsBold];
  Col.Format.Font.Color:=TAlphaColors.Navy;
end;

// For all numeric columns, set right text alignment
procedure TTickerForm.ColumnTextAlign;
var Col : Integer;
    tmp : TColumn;
begin
  for Col:=1 to TeeGrid1.Columns.Count-1 do
  begin
    tmp:=TeeGrid1.Columns[Col];

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

procedure TTickerForm.RefreshSpeed;
begin
  LSpeed.Text:=IntToStr(Timer1.Interval)+' msec';
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

end.
