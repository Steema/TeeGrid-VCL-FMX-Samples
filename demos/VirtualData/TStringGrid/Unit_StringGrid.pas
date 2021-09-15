unit Unit_StringGrid;

interface

{
  This example shows how to use TeeGrid like a "TStringGrid"

  It also setups the grid to a relatively "big" size: 1000 x 100000 (hundred million cells)

  For bigger sizes, this project should be compiled as 64bit platform because 32bit
  has a limit of 2GB (or 3GB using a custom PEFlag)
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  System.UITypes, System.Types,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,

  Vcl.ExtCtrls, Tee.Grid.RowGroup, Vcl.StdCtrls,
  Tee.GridData.Strings, Tee.Grid.Columns, Tee.Renders, Vcl.Imaging.pngimage,
  Tee.Format;

type
  TStringGridForm = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    EColumns: TEdit;
    Label2: TLabel;
    ERows: TEdit;
    Button1: TButton;
    Label3: TLabel;
    LCells: TLabel;
    CBGDIPlus: TCheckBox;
    OkImage: TImage;
    Benchmark: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TeeGrid1ClickedHeader(Sender: TObject);
    procedure TeeGrid1Select(Sender: TObject);
    procedure EColumnsChange(Sender: TObject);
    procedure ERowsChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBGDIPlusClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BenchmarkClick(Sender: TObject);
  private
    { Private declarations }

    Data : TStringsData;

    CustomFormat : TTextFormat;
    OkPicture : TPicture;

    procedure OptimizePaintSpeed;
    procedure PaintPicture(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
    procedure RefreshTotalCells;
    procedure TestPaintBackground(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
  public
    { Public declarations }
  end;

var
  StringGridForm1: TStringGridForm;
  aChanged: TNotifyEvent;

implementation

{$R *.dfm}

uses
  VCLTee.Editor.Grid, Tee.Grid.Bands, Tee.Grid, Tee.Painter,
  VCLTee.Painter.GDIPlus, VCLTee.Painter,
  Tee.Grid.Selection, System.Diagnostics, VCLTee.Picture, Tee.GridData,
  Tee.Grid.Rows;

// Shows the TeeGrid editor dialog
procedure TStringGridForm.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

// Repaints the TeeGrid 1000 times to benchmark painting speed
procedure TStringGridForm.BenchmarkClick(Sender: TObject);
var t1 : TStopWatch;
    t : Integer;
    t2 : Int64;
begin
  t1:=TStopwatch.StartNew;

  for t:=0 to 999 do
      TeeGrid1.Grid.Paint;

  t2:=t1.ElapsedMilliseconds;

  Caption:=t2.ToString+' msec to repaint: 1000 times';
end;

// Use GDI+ or normal GDI canvas
procedure TStringGridForm.CBGDIPlusClick(Sender: TObject);
begin
  if CBGDIPlus.Checked then
     TeeGrid1.Painter:=TGdiPlusPainter.Create
  else
     TeeGrid1.Painter:=TGdiPainter.Create(TeeGrid1.Canvas);
end;

// Change the number of grid columns
procedure TStringGridForm.EColumnsChange(Sender: TObject);
var tmp : Integer;
begin
  if TryStrToInt(EColumns.Text,tmp) then
  begin
    TStringsData(TeeGrid1.Data).Columns:=tmp;

    RefreshTotalCells;
  end;
end;

// Change the number of grid rows
procedure TStringGridForm.ERowsChange(Sender: TObject);
var tmp : Integer;
begin
  if TryStrToInt(ERows.Text,tmp) then
  begin
    TStringsData(TeeGrid1.Data).Rows:=tmp;

    RefreshTotalCells;
  end;
end;

procedure TStringGridForm.FormCreate(Sender: TObject);

  // Simple test, returns a new "Text Band" object
  function NewTitle:TTextBand;
  begin
    result:=TTextBand.Create(TeeGrid1.Rows.SubBands);
    result.Text:='Sub-Title'#13#10'Double';

    result.Format.Font.Style:=[fsBold];
    result.Format.Brush.Show;
    result.Format.Brush.Color:=TColors.Indianred;
    result.Format.Stroke.Show;
  end;

var t : Integer;
begin
  // Speed tip: For huge number of rows, hidden ScrollBars accelerate painting
  // TeeGrid1.ScrollBars.Visible:=False;

  // Create data
  Data:=TStringsData.Create(1000,100000 {,60}); // speed tip: pass a default pixel width (for example: 60)

  // Other ways to initialize size:
  // Data.Columns:=1000;
  // Data.Rows:=100000;
  // Data.Resize(1000,100000);

  RefreshTotalCells;

  // Set column header texts
  Data.Headers[0]:='A'#13#10'Text';
  Data.Headers[1]:='B';
  Data.Headers[2]:='C';
  Data.Headers[3]:='OK';

  // Fill rows and cells
  for t:=0 to Data.Rows-1 do
  begin
    Data[0,t]:='0 '+IntToStr(t);
    Data[1,t]:='1 '+IntToStr(t);
    Data[2,t]:='2 '+IntToStr(t);

    if Random(100)<30 then
       Data[3,t]:='OK';
  end;

  // Multi-line test:
  Data[2,4]:=Data[2,4]+#13#10+'this is a long line';

  // Set data to grid
  TeeGrid1.Data:=Data;

  // Refresh edit boxes
  EColumns.Text:=IntToStr(Data.Columns);
  ERows.Text:=IntToStr(Data.Rows);

  // Insert a "sub-band" at position 20
  TeeGrid1.Rows.SubBands.Row[20]:=NewTitle;

  // Just a test, hide 2nd column header text
  TeeGrid1.Columns[1].Header.Hide;

  // Set event to paint a picture at some of 4th column cells
  TeeGrid1.Columns[3].OnPaint:=PaintPicture;
  TeeGrid1.Columns[3].Width.InitValue(40);

  // Just some text alignment tests
  TeeGrid1.Cells.TextAlign.Vertical:=TVerticalAlign.Center;
  TeeGrid1.Selected.TextAlign.Vertical:=TVerticalAlign.Center;

  // Create a picture from a TImage on this form
  OkPicture:=TVCLPicture.From(OkImage);
  // OkPicture.Stretch:=False;

  //TeeGrid1.Rows.Heights[4]:=32;

  // Default row height, for all rows
  TeeGrid1.Rows.Height.Value:=32;

  OptimizePaintSpeed;

  // Test Column OnPaint
  CustomFormat:=TTextFormat.Create(nil);

  TeeGrid1.Columns[6].OnPaint:=TestPaintBackground;

  TeeGrid1.Columns[0].OnPaint:=TestPaintBackground;  //texts here
end;

// Simple test, customize per-cell background format
procedure TStringGridForm.TestPaintBackground(const Sender:TColumn;
                      var AData:TRenderData; var DefaultPaint:Boolean);
var aFont, oldFont : TFont;
begin
  DefaultPaint:=True;

  CustomFormat.Stroke.Visible:=False;
  CustomFormat.Brush.Visible:=True;
  CustomFormat.Brush.Gradient.Hide;

  if AData.Row=4 then
     CustomFormat.Brush.Color:=TColors.Red
  else
  if AData.Row=6 then
  begin
    CustomFormat.Brush.Color:=TColors.Lime;

    CustomFormat.Stroke.Visible:=True;
    CustomFormat.Stroke.Size:=3;
  end
  else
  if AData.Row=2 then
  begin
    CustomFormat.Brush.Gradient.Show;
    CustomFormat.Brush.Gradient.Colors[0].Color:=TColors.Navy;
    CustomFormat.Brush.Gradient.Colors[1].Color:=TColors.Yellow;
  end
  else
    Exit;

  DefaultPaint:=False;

  //Modify cell Font
  oldFont := CustomFormat.Font;

  aFont := TFont.Create(aChanged);
  aFont.Size := 20;
  aFont.Color := TColors.Orange;

  AData.Painter.SetFont(aFont);

  //background
  AData.Painter.Paint(CustomFormat,AData.Bounds);
  //text
  (Sender.Render as TTextRender).Paint(AData);

  AData.Painter.SetFont(oldFont);

  aFont.Free;
end;

// Speed performance, disable cosmetic effects:
procedure TStringGridForm.OptimizePaintSpeed;
begin
  TeeGrid1.Rows.Alternate.Hide;
  TeeGrid1.Header.Format.Brush.Gradient.Hide;
end;

procedure TStringGridForm.FormDestroy(Sender: TObject);
begin
  // Destroy the picture, just to avoid a memory leak
  OkPicture.Free;

  // Destroy the custom brush, just to avoid a memory leak
  CustomFormat.Free;
end;

// This event is called for all cells of 4th column, when they are going to be painted
procedure TStringGridForm.PaintPicture(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
var tmp : TRectF;
begin
  // We'll replace "OK" text with a picture
  DefaultPaint:=not SameText(AData.Data,'OK');

  if not DefaultPaint then
  begin
    tmp:=AData.Bounds;
    tmp.Inflate(-8,-6);

    AData.Painter.Draw(OkPicture,tmp);
  end;
end;

// Show the total number of grid cells
procedure TStringGridForm.RefreshTotalCells;
begin
  LCells.Caption:=FormatFloat('#,###',Data.Columns*Data.Rows);
end;

// Just a test, when clicking a column header
procedure TStringGridForm.TeeGrid1ClickedHeader(Sender: TObject);
begin
  Panel1.Caption:='Clicked column header: '+(Sender as TColumn).Header.Text;
end;

// Just a test, when selecting a grid cell with mouse click or arrow keys
procedure TStringGridForm.TeeGrid1Select(Sender: TObject);

  function ColumnHeader(const AColumn:TColumn):String;
  begin
    result:=AColumn.Header.Text;

    if result='' then
       result:=IntToStr(AColumn.Index);
  end;

var tmp : TGridSelection;
begin
  tmp:=TeeGrid1.Grid.Current.Selected;

  if tmp.IsEmpty then
     Panel1.Caption:=''
  else
     Panel1.Caption:='Selected cell: '+ColumnHeader(tmp.Column)+
                     ' Row: '+IntToStr(tmp.Row)+
                     ' Value: '+TeeGrid1.Grid.Current.Data.AsString(tmp.Column,tmp.Row);
end;

end.
