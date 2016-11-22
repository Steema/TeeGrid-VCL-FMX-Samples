unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  VCLTee.Grid;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Grid1 : TTeeGrid;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

uses
  Tee.Grid.Data.Strings;

procedure TForm1.FormCreate(Sender: TObject);
var tmp : TStringsData;
    t : Integer;
begin
  Grid1:=TTeeGrid.Create(Self);
  Grid1.Align:=alClient;
  Grid1.Parent:=Self;

  tmp:=TStringsData.Create;
  tmp.Rows:=10000;
  tmp.Columns:=12;

  for t:=1 to tmp.Columns do
      tmp.Headers[t-1]:=DefaultFormatSettings.LongMonthNames[t];

  for t:=0 to tmp.Rows-1 do
      tmp.Cells[0,t]:=IntToStr(t);

  Grid1.Data:=tmp;


end;

end.

