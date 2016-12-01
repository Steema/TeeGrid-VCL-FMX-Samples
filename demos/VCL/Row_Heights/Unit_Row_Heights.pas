unit Unit_Row_Heights;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid;

type
  TFormRowHeights = class(TForm)
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
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
  Tee.Grid.Data.Strings, Tee.Grid.Columns;

procedure TFormRowHeights.FormCreate(Sender: TObject);
var tmp : TStringsData;
begin
  tmp:=TStringsData.Create(2,11);
  TeeGrid1.Data:=tmp;

  TeeGrid1.Columns[0].Header.Text:='ABC';
  TeeGrid1.Columns[1].Header.Text:='DEF'#13#10'SubText';

  tmp[0,4]:='Hello';
  tmp[0,5]:='Hello'#13#10'World';
  tmp[0,10]:='This is a long line'#13#10'of text with multiple'#13#10'lines';

  // Different row heights, depending on all row cell contents
  TeeGrid1.Rows.Height.Automatic:=True;

  // Just a test, always show the vertical scrollbar
//  TeeGrid1.ScrollBars.Vertical.Visible:=TScrollBarVisible.Yes;

  TeeGrid1.Columns[1].Format.Stroke.Show;
  TeeGrid1.Columns[1].Format.Stroke.Size:=4;
end;

end.
