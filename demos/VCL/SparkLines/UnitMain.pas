unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  Vcl.ExtCtrls, Tee.Renders.SparkLines;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    TeeGrid1: TTeeGrid;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    Sparks : TSparkLines;

    procedure AddRandomData;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Tee.GridData.Strings, UITypes;

// For simplicity only, lets use string cells as data.
procedure TMainForm.AddRandomData;
var Data : TStringsData;
    Row, Col : Integer;
begin
  Data:=TStringsData.Create(11,20);

  for Row:=0 to Data.Rows-1 do
  begin
    Data[0,Row]:='Row '+IntToStr(Row);

    for Col:=1 to Data.Columns-1 do
        Data.Headers[Col]:=IntToStr(Col);

    Data[1,Row]:=IntToStr(Random(1000));

    for Col:=2 to Data.Columns-1 do
        Data[Col,Row]:=IntToStr(StrToInt(Data[Col-1,Row])+Random(100)-49);
  end;

  TeeGrid1.Data:=Data;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  AddRandomData;

  Sparks:=TSparkLines.AddTo(TeeGrid1.Grid, 1,TeeGrid1.Columns.Count-2, 'Sparks');

  Sparks.Stroke.Size:=2;
  Sparks.Stroke.Color:=TColors.Blue;
end;

end.
