unit Unit_Custom_Bands;

{
  This example shows how to add a custom "band" (row) to a TeeGrid,
  in this case to the grid footer.

  The custom band is a TColumnsBand object that allows setting custom string
  text for each visible column.

}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid, Vcl.ExtCtrls,

  // Unit with TColumnsBand:
  Tee.Grid.Bands.Columns,

  // Unit to use strings as data, just as an example:
  Tee.GridData.Strings, Vcl.StdCtrls;


type
  TCustomBandForm = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    Data : TStringsData;
    Footer : TColumnsBand;

    procedure AddSampleData;
    procedure AddCustomBand;
    procedure CreateGridEditor;
  public
    { Public declarations }
  end;

var
  CustomBandForm: TCustomBandForm;

implementation

{$R *.dfm}

uses
  VCLTee.Editor.Grid, Tee.Grid.Columns, Tee.Painter;

procedure TCustomBandForm.FormCreate(Sender: TObject);
begin
  AddSampleData;
  AddCustomBand;

  CreateGridEditor;

  // Initial grid footer setting to "float"
  // (just at bottom of last grid row, when vertical scrollbar is not present)
  TeeGrid1.Footer.Floating:=True;
end;

procedure TCustomBandForm.AddCustomBand;
begin
  // Create the band, associate it with the grid footer:
  Footer:=TColumnsBand.Create(TeeGrid1.Footer);   // Also: TeeGrid1.Header, etc

  // Set custom text for the columns we wish:

  Footer.Text[TeeGrid1.Columns[1]]:='Kg';

  Footer.Text[TeeGrid1.Columns[3]]:='Lb';
end;

procedure TCustomBandForm.AddSampleData;
var col, row : Integer;
    Column : TColumn;
begin
  Data:=TStringsData.Create(5,4);

  Data.Headers[0]:='C0';
  Data.Headers[1]:='C1';
  Data.Headers[2]:='C2';
  Data.Headers[3]:='C3';
  Data.Headers[4]:='C4';

  // Random cells

  for col:=0 to Data.Columns-1 do
      for row:=0 to Data.Rows-1 do
          Data[col,row]:=FloatToStr(Random(100));

  TeeGrid1.Data:=Data;

  // Now we can force right-alignment
  for Column in Data.ColumnList do
  begin
    Column.TextAlignment:=TColumnTextAlign.Custom;
    Column.TextAlign.Horizontal:=THorizontalAlign.Right;
  end;
end;

procedure TCustomBandForm.CreateGridEditor;
var Editor : TTeeGridEditor;
begin
  Editor:=TTeeGridEditor.Embedd(Self,Panel1,TeeGrid1);

  // Set the initial tab of the grid editor (the "Bands" tab)
  Editor.PageGrid.ActivePage:=Editor.TabBands;
  Editor.PageGridChange(Editor);
end;

end.
