unit Unit_Row_Heights;

interface

{
  Several test with multi-line text in Grid Header and Cells
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  Vcl.StdCtrls, Vcl.ExtCtrls,

  VCLTee.Editor.Grid;

type
  TFormRowHeights = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    CBMultiLine: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CBMultiLineClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRowHeights: TFormRowHeights;

implementation

{$R *.dfm}

uses
  System.Diagnostics,
  Tee.Grid.Data.Strings, Tee.Grid.Columns, Tee.Painter;

procedure TFormRowHeights.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1)
end;

procedure TFormRowHeights.Button2Click(Sender: TObject);
var t1 : TStopwatch;
    t : Integer;
begin
  t1:=TStopwatch.StartNew;

  for t:=1 to 1000 do
      TeeGrid1.Grid.Paint;

  Caption:=IntToStr(t1.ElapsedMilliseconds);
end;

procedure TFormRowHeights.CBMultiLineClick(Sender: TObject);
begin
  // Different row heights, depending on all row cell contents
  TeeGrid1.Rows.Height.Automatic:=CBMultiLine.Checked;

  // Tell data to use multi-line or not, to recalculate cell widths
//  (TeeGrid1.Data as TStringsData).MultiLine:=CBMultiLine.Checked;
end;

procedure TFormRowHeights.FormCreate(Sender: TObject);
var tmp : TStringsData;
begin
  tmp:=TStringsData.Create(2,11);
  TeeGrid1.Data:=tmp;

  TeeGrid1.Columns[0].Header.Text:='ABC'#13#10'Extra Content';
  TeeGrid1.Columns[1].Header.Text:='DEF'#13#10'SubText';

  tmp[0,3]:='Sample';
  tmp[0,5]:='Hello'#13#10'World';
  tmp[0,10]:='This is a long line'#13#10'of text with multiple'#13#10'lines';

  tmp[1,1]:='Several lines'#13#10'of text';
  tmp[1,6]:='More sample'#13#10'text';
  tmp[1,10]:='Another long line'#10#13'of text with multiple'#10#13'lines';

  TeeGrid1.Columns[1].ParentFormat:=False;
  TeeGrid1.Columns[1].Format.Font.Size:=13;

  TeeGrid1.Columns[1].TextAlignment:=TColumnTextAlign.Custom;
  TeeGrid1.Columns[1].TextAlign.Horizontal:=THorizontalAlign.Right;

  CBMultiLineClick(Self);

  // Just a test, always show the vertical scrollbar
//  TeeGrid1.ScrollBars.Vertical.Visible:=TScrollBarVisible.Yes;

//  TeeGrid1.Columns[1].Format.Stroke.Show;
//  TeeGrid1.Columns[1].Format.Stroke.Size:=4;
end;

end.
