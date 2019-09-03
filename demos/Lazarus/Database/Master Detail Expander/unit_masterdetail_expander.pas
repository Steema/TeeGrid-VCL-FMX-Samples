unit Unit_MasterDetail_Expander;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, odbcconn, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, VCLTee.Grid, Tee.Renders, Tee.Grid.RowGroup, db;

type

  { TForm1 }

  TForm1 = class(TForm)
    ODBCConnection1: TODBCConnection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
  private
    Expander : TExpanderRender;

    procedure GetOrders(const Sender: TExpanderRender; const ARow:Integer; out AData:TObject);
    procedure DetailNewGroup(const Sender,NewGroup:TRowGroup);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Tee.GridData.DB, Tee.Grid.Totals, Tee.GridData;

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
  SQLQuery2.UsePrimaryKeyAsKey := False;

  TeeGrid1.Selected.FullRow:=True;

  // Create "Expander"
  Expander:=TeeGrid1.Grid.Current.NewExpander;

  // Setup event
  Expander.OnGetData:=@GetOrders;

  // We don't know in advance if a row can be expanded or not, so set Always
  Expander.AlwaysExpand:=True;

  // Set to first Column
  TeeGrid1.Columns[0].Render:=Expander;

  // Optional:
  TeeGrid1.Grid.Current.OnNewDetail:=@DetailNewGroup;
end;

procedure TForm1.GetOrders(const Sender: TExpanderRender; const ARow:Integer; out AData:TObject);
var CustomerID: String;
begin
  SQLQuery1.RecNo:=ARow+1;
  CustomerID:=SQLQuery1.FieldByName('CustomerID').AsString;

  SQLQuery2.Close;
  SQLQuery2.SQL.Text:='select * from Orders WHERE CustomerID=''' + CustomerID + '''';
  AData:=TVirtualDBData.From(SQLQuery2);
  SQLQuery2.Open;
end;

procedure TForm1.DetailNewGroup(const Sender,NewGroup:TRowGroup);
var tmpTot : TColumnTotals;
begin
  // Create a Totals band
  tmpTot:=TColumnTotals.Create(NewGroup.Footer); // <--- set to Footer

  // Add some calculations
  tmpTot.Calculation.Add(NewGroup.Columns[0],TColumnCalculation.Count);
  tmpTot.Calculation.Add('Freight',TColumnCalculation.Sum);

  // Add a Totals header:
  TTotalsHeader.CreateTotals(NewGroup.Footer,tmpTot);
end;

end.

