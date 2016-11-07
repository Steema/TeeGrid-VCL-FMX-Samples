unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, VCLTee_Grid, db, dbf;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DataSource1: TDataSource;
    Dbf1: TDbf;
    Panel1: TPanel;
    TeeGrid1: TTeeGrid;
    procedure Button1Click(Sender: TObject);
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

uses
  VCLTee_Editor_Grid,
  Tee_Grid_Data_DB;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TeeGrid1.Data:=TVirtualDBData.From(DataSource1);

  Dbf1.TableName:='disco.dbf';
  Dbf1.Open;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

end.

