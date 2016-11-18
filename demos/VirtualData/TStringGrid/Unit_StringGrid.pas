unit Unit_StringGrid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid;

type
  TStringGridForm = class(TForm)
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StringGridForm1: TStringGridForm;

implementation

{$R *.dfm}

uses
  Tee.Grid.Data.Strings;

procedure TStringGridForm.FormCreate(Sender: TObject);
var Data : TCustomVirtualData;
    t : Integer;
begin
  Data:=TCustomVirtualData.Create;

  Data.Columns:=3;

  Data.Headers[0]:='A';
  Data.Headers[1]:='B';
  Data.Headers[2]:='C';

  Data.Rows:=6;

  for t:=0 to Data.Rows-1 do
  begin
    Data[0,t]:='0 '+IntToStr(t);
    Data[1,t]:='1 '+IntToStr(t);
    Data[2,t]:='2 '+IntToStr(t);
  end;

  TeeGrid1.Data:=Data;
end;

end.
