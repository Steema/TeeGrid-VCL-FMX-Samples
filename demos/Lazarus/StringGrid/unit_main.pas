unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  VCLTee.Grid;

type

  { TForm1 }

  TForm1 = class(TForm)
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses
  Tee.GridData.Strings;

procedure TForm1.FormCreate(Sender: TObject);
var tmp : TStringsData;
    t : Integer;
begin
  tmp:=TStringsData.Create;
  tmp.Rows:=10000;
  tmp.Columns:=12;

  for t:=1 to tmp.Columns do
      tmp.Headers[t-1]:=DefaultFormatSettings.LongMonthNames[t];

  for t:=0 to tmp.Rows-1 do
      tmp.Cells[0,t]:=IntToStr(t);

  TeeGrid1.Data:=tmp;
end;

end.

