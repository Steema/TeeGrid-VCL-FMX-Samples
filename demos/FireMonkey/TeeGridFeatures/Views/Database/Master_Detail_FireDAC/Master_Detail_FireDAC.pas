unit Master_Detail_FireDAC;

{
   Example using TeeGrid to display a master-detail relationship.

   This example uses FireDAC, two queries:

   Master:
     CustomersTable (select * from Customers)

   Detail:
     OrdersTable   (select * from Orders where CustomerID= .... )
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation,

  FMXTee.Control, FMXTee.Grid,

  Tee.Renders, Tee.Grid.RowGroup, Customer_Orders, Data.Bind.Controls,
  Data.Bind.Components, Data.Bind.DBScope, Data.DB, Fmx.Bind.Navigator;

type
  TMasterDetail = class(TForm)
    Layout1: TLayout;
    CBEnabled: TCheckBox;
    BindNavigator1: TBindNavigator;
    DataSource1: TDataSource;
    BindSourceDB1: TBindSourceDB;
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
    procedure CBEnabledChange(Sender: TObject);
  private
    { Private declarations }

    Expander : TExpanderRender;

    procedure DetailNewGroup(const Sender,NewGroup:TRowGroup);
    procedure GetOrders(const Sender: TExpanderRender; const ARow:Integer; out AData:TObject);

  public
    { Public declarations }
  end;

var
  MasterDetail: TMasterDetail;

implementation

{$R *.fmx}

uses
  Tee.Grid.Totals, Tee.GridData.DB, Tee.Grid.Columns, Tee.Grid, Tee.GridData;

procedure TMasterDetail.FormCreate(Sender: TObject);
begin
  SampleData.OpenTables;

  TeeGrid1.DataSource:=SampleData.CustomersTable;

  // Initialize "Expander"
  CBEnabledChange(Self);

  // Optional:
  TeeGrid1.Grid.Current.OnNewDetail:=DetailNewGroup;
end;

// Optional. Called when a new detail sub-grid has been created
procedure TMasterDetail.CBEnabledChange(Sender: TObject);
begin
  if CBEnabled.IsChecked then
  begin
    // Create "Expander"
    Expander:=TeeGrid1.Grid.Current.NewExpander;

    // Setup event
    Expander.OnGetData:=GetOrders;

    // We don't know in advance if a row can be expanded or not, so set Always
    Expander.AlwaysExpand:=True;

    // Set to first Column

    if TeeGrid1.Columns.Count>0 then
       TeeGrid1.Columns[0].Render:=Expander;
  end
  else
  begin
    // Remove all detail grids
    TeeGrid1.Rows.Children.Clear;

    // Set first column render to default (no expander)
    TeeGrid1.Columns[0].Render:=nil;
  end;
end;

procedure TMasterDetail.DetailNewGroup(const Sender,NewGroup:TRowGroup);
var tmpTot : TColumnTotals;
begin
  // Create a Totals band
  tmpTot:=TColumnTotals.Create(NewGroup.Footer); // <--- set to Footer

  // Add some calculations
  tmpTot.Calculation.Add(NewGroup.Columns[0],TColumnCalculation.Count);
  tmpTot.Calculation.Add('EmployeeID',TColumnCalculation.Sum);

  // Add a Totals header:
  TTotalsHeader.CreateTotals(NewGroup.Footer,tmpTot);
end;

// Called when a new sub-grid has been created, to obtain the sub-grid Data
// The "Orders" for the current "Customer"

procedure TMasterDetail.GetOrders(const Sender: TExpanderRender; const ARow:Integer; out AData:TObject);
begin
  // Return a new Data using a clone of Orders rows for a given Customer
  AData:=TVirtualDBData.From(SampleData.OrdersOfCustomer(ARow+1));

  // Data should be destroyed automatically
  TVirtualDBData(AData).OwnsData:=True;
end;

end.
