unit Unit_Join_Two_Tables;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.ODBC,
  FireDAC.Phys.ODBCDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, VCLTee.Control, VCLTee.Grid;

type
  TOrder=record
  public
    OrderNo: Integer;
    SaleDate: TDateTime;
    AmountPaid: Integer;
  end;

  TCustomer=record
  public
    CustNo: Integer;
    Company: string;
    City: string;
  end;

  TOrderByCustomer=record
  public
    Order: TOrder;
    Customer: TCustomer;
  end;

  TForm1 = class(TForm)
    TeeGrid1: TTeeGrid;
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure FillMyData(var AData: Array of TOrderByCustomer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Tee.GridData.Rtti, Tee.Grid.Columns, Tee.Renders, Tee.Painter;

var MyData: TArray<TOrderByCustomer>;

procedure TForm1.FormCreate(Sender: TObject);
var i: Integer;
begin
  FillMyData(MyData);
  TeeGrid1.Data:=TVirtualData<TArray<TOrderByCustomer>>.Create(MyData);

  for i:=0 to TeeGrid1.Columns.Count-1 do
    with TeeGrid1.Columns[i].Header do
    begin
      Format.Brush.Visible:=True;
      Format.Brush.Color:=clLtGray;
      TextAlignment:=TColumnTextAlign.Custom;
      TextAlign.Horizontal:=THorizontalAlign.Center;
    end;
end;

procedure TForm1.FillMyData(var AData: Array of TOrderByCustomer);

  procedure AppendData(AItem: TOrderByCustomer);
  var i: Integer;
      tmp: TArray<TOrderByCustomer>;
  begin
    SetLength(tmp, Length(MyData)+1);
    for i:=0 to High(MyData) do
      tmp[i]:=MyData[i];

    tmp[Length(tmp)-1]:=AItem;
    MyData:=tmp;
  end;

var tmpItem: TOrderByCustomer;
begin
  FDConnection1.Params.DriverID:='ODBC';
  FDConnection1.Params.Database:=GetEnvironmentVariable('DEMOSDIR')+'\Data';
  FDConnection1.Params.Values['DataSource']:='DBDemosParadox';

  FDQuery1.Connection:=FDConnection1;

  FDQuery1.SQL.Clear;
  FDQuery1.SQL.Add('SELECT orders.OrderNo, orders.SaleDate, orders.AmountPaid, customer.CustNo, customer.Company, customer.City ' +
                   'FROM customer ' +
                   'INNER JOIN orders ON customer.CustNo = orders.CustNo;');

  FDQuery1.Active:=True;

  FDQuery1.DisableControls;
  try
    FDQuery1.First;
    while not FDQuery1.Eof do
    begin
      tmpItem.Order.OrderNo:=FDQuery1.FieldByName('OrderNo').AsInteger;
      tmpItem.Order.SaleDate:=FDQuery1.FieldByName('SaleDate').AsDateTime;
      tmpItem.Order.AmountPaid:=FDQuery1.FieldByName('AmountPaid').AsInteger;
      tmpItem.Customer.CustNo:=FDQuery1.FieldByName('CustNo').AsInteger;
      tmpItem.Customer.Company:=FDQuery1.FieldByName('Company').AsString;
      tmpItem.Customer.City:=FDQuery1.FieldByName('City').AsString;

      AppendData(tmpItem);
      FDQuery1.Next;
    end;
  finally
    FDQuery1.EnableControls;
  end;
end;

end.
