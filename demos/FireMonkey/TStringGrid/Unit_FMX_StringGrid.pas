unit Unit_FMX_StringGrid;

interface

{
  This example shows using a TeeGrid like a TStringGrid
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMXTee.Control, FMXTee.Grid,
  FMX.Layouts, FMX.Edit, FMX.EditBox, FMX.NumberBox,

  Tee.GridData.Strings, Tee.Grid.Columns, Tee.Renders, Tee.Format, Tee.Painter,
  FMX.Objects, FMXTee.Picture;

type
  TStringGridForm = class(TForm)
    Layout1: TLayout;
    TeeGrid1: TTeeGrid;
    Button1: TButton;
    EColumns: TNumberBox;
    ERows: TNumberBox;
    OkImage: TImage;
    Layout2: TLayout;
    Label1: TLabel;
    LCells: TLabel;
    Text1: TText;
    Text2: TText;
    procedure Button1Click(Sender: TObject);
    procedure EColumnsChangeTracking(Sender: TObject);
    procedure ERowsChangeTracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TeeGrid1ClickedHeader(Sender: TObject);
    procedure TeeGrid1Select(Sender: TObject);
  private
    { Private declarations }

    Data : TStringsData;

    OkPicture : TPicture;

    procedure OptimizePaintSpeed;
    procedure PaintPicture(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
    procedure RefreshTotalCells;
  public
    { Public declarations }
  end;

var
  StringGridForm: TStringGridForm;

implementation

{$R *.fmx}

uses
  System.Diagnostics, Tee.Grid.Bands, Tee.Grid.Selection, Tee.Grid,
  Tee.Grid.RowGroup, FMXTee.Painter, Tee.Grid.Rows, Tee.GridData;

// Repaint the grid 1000 times to benchmark painting speed
procedure TStringGridForm.Button1Click(Sender: TObject);
var t1 : TStopWatch;
    t : Integer;
    t2 : Int64;
begin
  t1:=TStopwatch.StartNew;

  if TeeGrid1.Canvas.BeginScene then
  begin
    for t:=0 to 999 do
        TeeGrid1.Grid.Paint;

    TeeGrid1.Canvas.EndScene;
  end;

  t2:=t1.ElapsedMilliseconds;

  Caption:=t2.ToString+' msec to repaint: 1000 times';

  Invalidate;
end;

// Change the number of grid columns
procedure TStringGridForm.EColumnsChangeTracking(Sender: TObject);
begin
  TStringsData(TeeGrid1.Data).Columns:=Round(EColumns.Value);

  RefreshTotalCells;
end;

// Change the number of grid rows
procedure TStringGridForm.ERowsChangeTracking(Sender: TObject);
begin
  TStringsData(TeeGrid1.Data).Columns:=Round(EColumns.Value);

  RefreshTotalCells;
end;

procedure TStringGridForm.FormCreate(Sender: TObject);

  // Create an example TextBand
  function NewTitle:TTextBand;
  begin
    result:=TeeGrid1.Rows.SubBands.AddText('Sub-Title'#13+'Double');

    result.Format.Font.Style:=[TFontStyle.fsBold];
    result.Format.Brush.Show;
    result.Format.Brush.Color:=TColors.Indianred;
    result.Format.Stroke.Show;
  end;

var t : Integer;
begin
  // TeeGrid1.Selected.Range.Enabled:=True;  // <-- for non-touch screen devices

  // Create data
  Data:=TStringsData.Create(1000,100000,60);

  // Other ways to initialize size:

  // Data.Columns:=1000;
  // Data.Rows:=100000;

  // Data.Resize(1000,100000);

  // Set header texts
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

  // Set data to grid
  TeeGrid1.Data:=Data;

  // Refresh edit boxes
  EColumns.Text:=IntToStr(Data.Columns);
  ERows.Text:=IntToStr(Data.Rows);

  RefreshTotalCells;

  // Insert a text band positioned above row 20th
  TeeGrid1.Rows.SubBands.Row[20]:=NewTitle;

  // Just an example, hide 2nd column header text
  TeeGrid1.Columns[1].Header.Hide;

  // Set 4th column paint to display a picture
  TeeGrid1.Columns[3].OnPaint:=PaintPicture;

  // Create a picture from an image
  OkPicture:=TFMXPicture.From(OkImage);

  // Set the default row height (same height for all rows)
  TeeGrid1.Rows.Height.Value:=32;

  // Set picture column width to match bitmap picture aspect ratio (width to height)
  TeeGrid1.Columns[3].Width.Value:=40;

  // Example: Set cell editing to auto-start when typing a character key
  TeeGrid1.Editing.AutoEdit:=True;

  OptimizePaintSpeed;
end;

// Several options to increase painting speed
procedure TStringGridForm.OptimizePaintSpeed;
begin
  // Speed optimization, disable scrollbars
//  TeeGrid1.ScrollBars.Visible:=False;

  TeeGrid1.Painter.BitmapQuality:=TBitmapQuality.Speed;

  TeeGrid1.Rows.Alternate.Hide;
  TeeGrid1.Header.Format.Brush.Gradient.Hide;

  // Vertical text alignment, default for all columns:
  // TeeGrid1.Cells.TextAlign.Vertical:=TVerticalAlign.Center;
end;

procedure TStringGridForm.FormDestroy(Sender: TObject);
begin
  // Destroy the picture to avoid memory leak
  OkPicture.Free;
end;

// Return the header text of AColumn
function HeaderText(const AColumn:TColumn):String;
begin
  result:=AColumn.Header.Text.Replace(#13#10,' ');

  if result='' then
     result:=IntToStr(AColumn.Index);
end;

// Called when clicking a column header
procedure TStringGridForm.TeeGrid1ClickedHeader(Sender: TObject);
begin
  Label1.Text:='Clicked column header: '+HeaderText(Sender as TColumn);
end;

// Called when selecting a cell, by mouse click, touch, or arrow keys
procedure TStringGridForm.TeeGrid1Select(Sender: TObject);
var tmp : TGridSelection;
begin
  tmp:=TeeGrid1.Grid.Current.Selected;

  if tmp.IsEmpty then
     Label1.Text:=''
  else
     Label1.Text:='Selected cell: '+HeaderText(tmp.Column)+
                     ' Row: '+IntToStr(tmp.Row)+
                     ' Value: '+TeeGrid1.Grid.Current.Data.AsString(tmp.Column,tmp.Row);
end;

// Display a picture inside a cell
procedure TStringGridForm.PaintPicture(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
var tmp : TRectF;
begin
  DefaultPaint:=not SameText(AData.Data,'OK');

  if not DefaultPaint then
  begin
    tmp:=AData.Bounds;
    tmp.Inflate(-8,-6);

    AData.Painter.Draw(OkPicture,tmp);
  end;
end;

procedure TStringGridForm.RefreshTotalCells;
begin
  LCells.Text:='Total cells: '+FormatFloat('#,###',Data.Columns*Data.Rows);
end;

end.
