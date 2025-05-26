unit Customer_Orders;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Stan.StorageBin, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Phys.SQLiteWrapper.Stat;

type
  TSampleData = class(TDataModule)
    Sqlite_demoConnection: TFDConnection;
    CustomersTable: TFDQuery;
    OrdersTable: TFDQuery;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }

    function CloneData(const ADataSet:TFDDataSet):TDataSet;
  public
    { Public declarations }

    function OrdersOfCustomer(const ARecNo:Integer):TDataSet;
  end;

var
  SampleData: TSampleData;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TSampleData.DataModuleCreate(Sender: TObject);
begin
  Sqlite_demoConnection.Open;
end;

function TSampleData.OrdersOfCustomer(const ARecNo:Integer):TDataSet;

  // Return the CustomerID for row: ARow
  function CustomerID:String;
  begin
    CustomersTable.RecNo:=ARecNo;
    result:=CustomersTable.FieldByName('CustomerID').AsString;
  end;

  // Execute OrdersTable query for a given CustomerID
  procedure FilterOrders(const ACustomerID:String);
  begin
    OrdersTable.Close;
    OrdersTable.ParamByName('Cust').AsString:=ACustomerID;
    OrdersTable.Open;
  end;

begin
  FilterOrders(CustomerID);
  result:=CloneData(OrdersTable);
end;

// Return a new clone copy of ADataSet data, all rows
function TSampleData.CloneData(const ADataSet:TFDDataSet):TDataSet;
var tmp : TMemoryStream;
begin
  tmp:=TMemoryStream.Create;
  try
    ADataSet.SaveToStream(tmp);
    tmp.Position:=0;

    result:=TFDMemTable.Create(nil);
    TFDMemTable(result).LoadFromStream(tmp);
  finally
    tmp.Free;
  end;
end;

end.
