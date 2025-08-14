## Master-Detail example using FireDAC datasets

This demo shows how to create 3 levels of nested sub-grid rows.

![](https://raw.github.com/Steema/TeeGrid-VCL-FMX-Samples/master/docs/img/TeeGrid_Three_Level_SubGrids_FireDAC.png)

Adding a sub-grid to another sub-grid is done in the following steps:

- Create a new "Expander" render

  This is the class that draws the "+" and "-" symbols you can click to expand or collapse a row.

  For the main grid:

  ```delphi
  uses Tee.Renders;
  var Expander : TExpanderRender;
  Expander:=TeeGrid1.Grid.Current.NewExpander;
  ```

  For any sub-grid, you should do the same as above but using the "NewGroup" variable that is passed when a sub-grid is expanded:

  ```delphi
  procedure TMasterDetail.DetailNewGroup(const Sender,NewGroup:TRowGroup);
  begin
    Expander:=NewGroup.NewExpander;
  ...
  ```
  
  
- The Expander should be connected to an event that will give the detail data rows we want for a given master row:

  ```delphi
  procedure TMasterDetail.GetOrders(const Sender: TExpanderRender; const ARow:Integer; out AData:TObject);
  begin
    // Return a new Data using a clone of Orders rows for a given Customer
    AData:=TVirtualDBData.From(SampleData.OrdersOfCustomer(ARow+1));

    // Data should be destroyed automatically
    TVirtualDBData(AData).OwnsData:=True;
  end;

  Expander.OnGetData:=GetOrders;
  ```

- In this example, the "OrdersOfCustomer" function is done manually using FireDAC TFDQuery components:

  ```delphi
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
  ```

  You can obtain the sub-details rows the way you like.

  If you use [TeeBI Datamining Library](https://github.com/Steema/TeeBI) these steps are much more simplified because TeeBI knows the relationship master-detail between datasets and prepares data automatically.
  There is a TeeBI and TeeGrid example showing these levels of sub-grid details:

  [TeeBI and TeeGrid Example](https://github.com/Steema/TeeGrid-VCL-FMX-Samples/tree/master/demos/VCL/TeeBI)
  

