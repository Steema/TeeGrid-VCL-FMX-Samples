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

  Tee.Format, Tee.Renders, Tee.Grid.RowGroup, Customer_Orders, Data.Bind.Controls,
  Data.Bind.Components, Data.Bind.DBScope, Data.DB, Fmx.Bind.Navigator;

type
  TMasterDetail = class(TForm)
    TeeGrid1: TTeeGrid;
    Layout1: TLayout;
    CBEnabled: TCheckBox;
    BindNavigator1: TBindNavigator;
    DataSource1: TDataSource;
    BindSourceDB1: TBindSourceDB;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CBEnabledChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    procedure DetailNewGroup(const Sender,NewGroup:TRowGroup);
    procedure SubDetailNewGroup(const Sender,NewGroup:TRowGroup);

    procedure EnableDisableSubGrid(const AGroup:TRowGroup; const AEvent:TExpanderGetDataEvent);

    // Sub-Grid level 1
    procedure GetOrders(const Sender: TExpanderRender; const ARow:Integer; out AData:TObject);

    // Sub-Grid level 2
    procedure GetOrderItems(const Sender: TExpanderRender; const ARow:Integer; out AData:TObject);

  public
    { Public declarations }
  end;

var
  MasterDetail: TMasterDetail;
  Open : Boolean;

implementation

{$R *.fmx}

uses
  Tee.Grid.Totals, Tee.GridData.DB, Tee.Grid.Columns, Tee.Grid, Tee.GridData;

procedure TMasterDetail.FormCreate(Sender: TObject);
begin
  SampleData.CustomersTable.Open;

  TeeGrid1.DataSource:=SampleData.CustomersTable;

  Open := False;

  // Initialize "Expander"
  CBEnabledChange(Self);

  // Optional:
  TeeGrid1.Grid.Current.OnNewDetail:=DetailNewGroup;
end;

// Optional. Called when a new detail sub-grid has been created
procedure TMasterDetail.Button1Click(Sender: TObject);
begin
  if Open then
  Begin
    TeeGrid1.Grid.Current.ShowHideAllDetail(0,False);
    Open:= False;
  End
  else
  Begin
    TeeGrid1.Grid.Current.ShowHideAllDetail(0,True);
    Open:= True;
  End;
end;

// This method is used for any sub-grid level, in this example it is used
// twice, one for Customer->Orders, and another for Orders->Order Details
procedure TMasterDetail.EnableDisableSubGrid(const AGroup:TRowGroup; const AEvent:TExpanderGetDataEvent);
var Expander : TExpanderRender;
begin
  if CBEnabled.IsChecked then
  begin
    // Create "Expander"
    Expander:=AGroup.NewExpander;

    // Setup data event
    Expander.OnGetData:=AEvent;

    // We don't know in advance if a row can be expanded or not, so set Always
    Expander.AlwaysExpand:=True;

    // Set to first Column
    if AGroup.Columns.Count>0 then
       AGroup.Columns[0].Render:=Expander;
  end
  else
  begin
    // Remove all detail grids
    AGroup.Rows.Children.Clear;

    // Set first column render to default (no expander)
    if AGroup.Columns.Count>0 then
       AGroup.Columns[0].Render:=nil;
  end;
end;

procedure TMasterDetail.CBEnabledChange(Sender: TObject);
begin
  EnableDisableSubGrid(TeeGrid1.Grid.Current,GetOrders);
end;

procedure TMasterDetail.DetailNewGroup(const Sender,NewGroup:TRowGroup);
var tmpTot : TColumnTotals;
begin
  EnableDisableSubGrid(NewGroup,GetOrderItems);

    // Optional:
  NewGroup.OnNewDetail:=SubDetailNewGroup;

  // Create a Totals band
  tmpTot:=TColumnTotals.Create(NewGroup.Footer); // <--- set to Footer

  // Add some calculations
  tmpTot.Calculation.Add(NewGroup.Columns[0],TColumnCalculation.Count);
  tmpTot.Calculation.Add('EmployeeID',TColumnCalculation.Sum);

  // Add a Totals header:
  TTotalsHeader.CreateTotals(NewGroup.Footer,tmpTot);

  // Cosmetics on the sub-grid
  NewGroup.Rows.Back.Brush.Color:=TAlphaColors.Bisque;
  NewGroup.Rows.Back.Brush.Visible:=True;

  NewGroup.Cells.Format.Font.Color:=TAlphaColors.Darkblue;
end;

procedure TMasterDetail.SubDetailNewGroup(const Sender,NewGroup:TRowGroup);
begin
  // Cosmetics on the sub-sub-grid
  NewGroup.Rows.Back.Brush.Color:=TAlphaColors.Lavender;
  NewGroup.Rows.Back.Brush.Visible:=True;

  NewGroup.Cells.Format.Font.Color:=TAlphaColors.Blueviolet;
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

// Called when a new sub-sub-grid has been created, to obtain the sub-sub-grid Data
// The "Order Items" for the current "Order" for the current "Customer"
procedure TMasterDetail.GetOrderItems(const Sender: TExpanderRender; const ARow:Integer; out AData:TObject);
begin
  // Return a new Data using a clone of Order Items rows for a given Order of a given Customer
  AData:=TVirtualDBData.From(SampleData.OrderDetailsOfOrder(ARow+1));

  // Data should be destroyed automatically
  TVirtualDBData(AData).OwnsData:=True;
end;

end.
