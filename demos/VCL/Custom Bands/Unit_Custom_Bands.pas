unit Unit_Custom_Bands;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  Tee.Grid.Bands.Columns, Tee.Grid.Header;

type
  TForm1 = class(TForm)
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Tee.GridData.Strings;

var
  Data : TStringsData;

  Footer : TColumnsBand;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Data:=TStringsData.Create(5,4);

  Data.Headers[0]:='C0';
  Data.Headers[1]:='C1';
  Data.Headers[2]:='C2';
  Data.Headers[3]:='C3';
  Data.Headers[4]:='C4';

  TeeGrid1.Data:=Data;

  Footer:=TColumnsBand.Create(TeeGrid1.Footer);

  Footer.Text[TeeGrid1.Columns[1]]:='F1';
end;

end.
