unit Unit_Test_Speed;

{
  This units performs a speed rendering test of an VCL TeeGrid control
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.UITypes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  VCLTee.Control, VCLTee.Grid, Vcl.ComCtrls;

type
  TFormSpeed = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    LabelResult: TLabel;
    TeeGrid1: TTeeGrid;
    ComboGraphics: TComboBox;
    CBAntiAlias: TCheckBox;
    TrackBar1: TTrackBar;
    CBFormatting: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboGraphicsChange(Sender: TObject);
    procedure CBAntiAliasClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CBFormattingClick(Sender: TObject);
  private
    { Private declarations }

    procedure ClearCosmetics;
    procedure RunBenchmark;
    procedure SetCosmetics;
  public
    { Public declarations }
  end;

var
  FormSpeed: TFormSpeed;

implementation

{$R *.dfm}

{$IF CompilerVersion>=36}  // RAD 12.0 Athens
{$DEFINE HAS_SKIA}
{$IFEND}

uses
  Tee.GridData.Strings,

  Tee.Grid.Columns, Tee.Painter, Tee.Format,

  VCLTee.Painter,
  VCLTee.Painter.GdiPlus,

  {$IFDEF HAS_SKIA}
  VCLTee.Painter.Skia,
  {$IFEND}

  System.Diagnostics;

// Returns data to use in the grid
function Sample_Data:TStringsData;  // Just a sample data grid
var Row,
    Column :Integer;
begin
  result:=TStringsData.Create(20,100);

  // Data

  for Row:=0 to result.Rows-1 do
  begin
    result[0,Row]:=IntToStr(Row);

    for Column:=1 to result.Columns-1 do
        result[Column,Row]:=IntToStr(Column);
  end;

  // Headers

  result.Headers[0]:='Row';

  for Column:=1 to result.Columns-1 do
      result.Headers[Column]:='Col'+IntToStr(Column);
end;

// Switch the graphics render engine (GDI+, GDI, Skia)
procedure TFormSpeed.CBAntiAliasClick(Sender: TObject);
begin
  if TeeGrid1.Painter is TGdiPlusPainter then
     TGdiPlusPainter(TeeGrid1.Painter).AntiAlias:=CBAntiAlias.Checked
  {$IFDEF HAS_SKIA}
  else
  if TeeGrid1.Painter is TSkiaPainter then
     TSkiaPainter(TeeGrid1.Painter).AntiAlias:=CBAntiAlias.Checked;
  {$ENDIF}
end;

procedure TFormSpeed.CBFormattingClick(Sender: TObject);
begin
  if CBFormatting.Checked then
     SetCosmetics
  else
     ClearCosmetics;
end;

procedure TFormSpeed.ComboGraphicsChange(Sender: TObject);
begin
  case ComboGraphics.ItemIndex of
    0: TeeGrid1.Painter:=TGdiPlusPainter.Create;
    1: TeeGrid1.Painter:=TGdiPainter.Create(TeeGrid1.Canvas);

    {$IFDEF HAS_SKIA}
    2: TeeGrid1.Painter:=TSkiaPainter.Create;
    {$ENDIF}
  end;

  CBAntiAlias.Enabled:=not (TeeGrid1.Painter is TGdiPainter);
end;

// Initialize grid with sample data
procedure TFormSpeed.FormCreate(Sender: TObject);
begin
  TeeGrid1.Data:=Sample_Data;

  {$IFNDEF HAS_SKIA} // Remove Skia option
  ComboGraphics.Items.Delete(2);
  {$IFEND}

  SetCosmetics;
end;

// Remove custom column formatting settings
procedure TFormSpeed.ClearCosmetics;
var Column : TColumn;
begin
  for Column in TeeGrid1.Columns do
  begin
    Column.ParentFormat:=True;
    Column.TextAlignment:=TColumnTextAlign.Automatic;
  end;

  TeeGrid1.Rows.ResetHeights;
end;

// Set some grid settings, just to test
procedure TFormSpeed.SetCosmetics;

  procedure SetTextAlign(const AColumn:Integer; const AAlign:THorizontalAlign); overload;
  var Column : TColumn;
  begin
    Column:=TeeGrid1.Columns[AColumn];

    Column.TextAlignment:=TColumnTextAlign.Custom;
    Column.TextAlign.Horizontal:=AAlign;
  end;

  procedure SetTextAlign(const AColumn:Integer; const AAlign:TVerticalAlign); overload;
  var Column : TColumn;
  begin
    Column:=TeeGrid1.Columns[AColumn];

    Column.TextAlignment:=TColumnTextAlign.Custom;
    Column.TextAlign.Vertical:=AAlign;
  end;

  procedure SetFontStyle(const AColumn:Integer; const AStyle:TFontStyle);
  var Column : TColumn;
  begin
    Column:=TeeGrid1.Columns[AColumn];

    Column.ParentFormat:=False;
    Column.Format.Font.Style:=[AStyle];
  end;

begin
  SetTextAlign(2,THorizontalAlign.Center);
  SetTextAlign(3,THorizontalAlign.Right);

  SetFontStyle(4,TFontStyle.fsBold);
  SetFontStyle(5,TFontStyle.fsItalic);
  SetFontStyle(6,TFontStyle.fsUnderline);
  SetFontStyle(7,TFontStyle.fsStrikeOut);

  TeeGrid1.Columns[8].ParentFormat:=False;
  TeeGrid1.Columns[8].Format.Brush.Show;
  TeeGrid1.Columns[8].Format.Brush.Color:=clWebBisque;

  TeeGrid1.Columns[9].ParentFormat:=False;
  TeeGrid1.Columns[9].Format.Font.Color:=clBlue;

  TeeGrid1.Rows.Heights[20]:=40;

//  SetTextAlign(10,TVerticalAlign.Center);

  TeeGrid1.Columns[11].ParentFormat:=False;
  TeeGrid1.Columns[11].Format.Font.Size:=8;

  TeeGrid1.Columns[12].ParentFormat:=False;
  TeeGrid1.Columns[12].Format.Font.Name:='Courier New';

  TeeGrid1.Columns[13].ParentFormat:=False;
  TeeGrid1.Columns[13].Format.Stroke.Show;
  TeeGrid1.Columns[13].Format.Stroke.Color:=clGreen;
end;

// Scroll through all grid cells, all rows and all columns
procedure TFormSpeed.RunBenchmark;
var Row : Integer;
    Column : TColumn;
begin
  for Row:=0 to TeeGrid1.Data.Count-1 do
      for Column in TeeGrid1.Columns do
      begin
        // Select one cell
        TeeGrid1.Selected.Column:=Column;
        TeeGrid1.Selected.Row:=Row;
      end;
end;

procedure TFormSpeed.TrackBar1Change(Sender: TObject);
begin
  TeeGrid1.Rows.Spacing.Value:=TrackBar1.Position
end;

// Do the speed test and measure it
procedure TFormSpeed.Button1Click(Sender: TObject);
var t1 : TStopwatch;
begin
  ComboGraphics.Enabled:=False;
  try
    TeeGrid1.SetFocus;

    t1:=TStopwatch.StartNew;

    RunBenchmark;

    LabelResult.Caption:='Time: '+IntToStr(t1.ElapsedMilliseconds)+' msec';
  finally
    ComboGraphics.Enabled:=True;
  end;
end;

end.
