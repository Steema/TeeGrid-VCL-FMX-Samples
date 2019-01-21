unit Unit_MasterDetail_TwoGrids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, odbcconn, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, VCLTee.Grid;

type

  { TForm1 }

  TForm1 = class(TForm)
    ODBCConnection1: TODBCConnection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    TeeGrid1: TTeeGrid;
    TeeGrid2: TTeeGrid;
    procedure FormCreate(Sender: TObject);
  private
    procedure TeeGrid1Selected(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ODBCConnection1.FileDSN := ExtractFilePath(Application.ExeName) + 'fddemo.dsn';
  ODBCConnection1.Connected := True;
  ODBCConnection1.KeepConnection := True;

  //transaction
  SQLTransaction1.DataBase := ODBCConnection1;
  SQLTransaction1.Action := caCommit;
  SQLTransaction1.Active := True;

  SQLQuery1.DataBase := ODBCConnection1;
  SQLQuery1.UsePrimaryKeyAsKey := False;
  SQLQuery1.SQL.Text := 'select * from Customers';
  SQLQuery1.Open;

  SQLQuery2.DataBase := ODBCConnection1;
  SQLQuery2.SQL.Text := 'select * from Orders';
  SQLQuery2.UsePrimaryKeyAsKey := False;
  SQLQuery2.Open;

  TeeGrid1.OnSelect:=@TeeGrid1Selected;
end;

procedure TForm1.TeeGrid1Selected(Sender: TObject);
var CustomerID: String;
begin
  CustomerID:=SQLQuery1.FieldByName('CustomerID').AsString;
  SQLQuery2.Close;
  SQLQuery2.SQL.Text:='select * from Orders WHERE CustomerID=''' + CustomerID + '''';
  SQLQuery2.Open;
end;

end.

