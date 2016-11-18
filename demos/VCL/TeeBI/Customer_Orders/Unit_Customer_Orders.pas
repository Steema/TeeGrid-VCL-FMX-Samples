{*********************************************}
{  TeeGrid Software Library                   }
{  Master -> Detail sub-grid rows             }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Unit_Customer_Orders;

interface

{
   Example to show multi-level row groups (Master -> Detail)

   Using TeeBI data samples, TeeGrid rows can be expanded / collapsed to show
   their detail sub-rows.

   Customer -> Orders -> Order Items
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  Tee.Renders, BI.DataSet, VCLTee.Editor.Grid, Vcl.StdCtrls, Vcl.ExtCtrls,

  BI.Data, Tee.Grid.Rows, Tee.Grid.Columns, Tee.Grid.Data;

type
  TFormDetailRows = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    // Sample data items
    Customers,
    Orders,
    OrderItems : TDataItem;

    procedure AddDetail(const AData:TVirtualData; const ADetail:TDataItem; const AColumn:TColumn);
    procedure AddMainTotals(const AData:TVirtualData; const AColumns:TColumns);
    procedure AddOrderTotals(const AGroup:TRowGroup);
    procedure NewDetail(const Sender,NewGroup:TRowGroup);
    procedure LoadSampleData;
  public
    { Public declarations }
  end;

var
  FormDetailRows: TFormDetailRows;

implementation

{$R *.dfm}

uses
  BI.DataSource, BI.Persist, BI.Grid.Data,
  Tee.Grid.Header, Tee.Grid.Totals;

procedure TFormDetailRows.Button1Click(Sender: TObject);
begin
  // Show TeeGrid editor dialog
  TTeeGridEditor.Edit(Self,TeeGrid1)
end;

procedure TFormDetailRows.AddOrderTotals(const AGroup:TRowGroup);
var tmp : TColumnTotals;
begin
  tmp:=TColumnTotals.Create(nil,AGroup.Columns,AGroup.Data);

  tmp.Calculation.Add(AGroup.Columns['Freight'],TColumnCalculation.Sum);
  tmp.Format.Font.Style:=[fsBold];

  AGroup.Footer.Add(tmp);
  AGroup.Footer.Add(TTotalsHeader.CreateTotals(tmp));
end;

procedure TFormDetailRows.AddDetail(const AData:TVirtualData; const ADetail:TDataItem; const AColumn:TColumn);
begin
  // Set the detail item
  (AData as TBIGridData).Detail:=ADetail;

  // Set the "+/-" icon to a column to enable expand / collapse
  AColumn.Render:=TeeGrid1.NewExpander(AData);
end;

// Optional event.
// It is used here to add a new 3rd level sub-detail (Customer->Orders->OrderItems)
procedure TFormDetailRows.NewDetail(const Sender,NewGroup:TRowGroup);
begin
  AddDetail(NewGroup.Data,OrderItems,NewGroup.Columns['OrderID']);
  AddOrderTotals(NewGroup);
end;

// Load sample data from TeeBI default "BISamples" disk store
procedure TFormDetailRows.LoadSampleData;
var Demo : TDataItem;
begin
  Demo:=TStore.Load('SQLite_Demo');

  Customers:=Demo['Customers'];
  Orders:=Demo['Orders'];
  OrderItems:=Demo['"Order Details"'];
end;

procedure TFormDetailRows.FormCreate(Sender: TObject);
begin
  LoadSampleData;

  // Main data: Customers
  TeeGrid1.Data:=TBIGridData.From(Customers);

  // Detail data: Orders
  AddDetail(TeeGrid1.Data,Orders,TeeGrid1.Columns['CustomerID']);

  TeeGrid1.OnNewDetail:=NewDetail;

  AddMainTotals(TeeGrid1.Data,TeeGrid1.Columns);
end;

// Add a grid footer band with "Totals" for main Customer table
procedure TFormDetailRows.AddMainTotals(const AData:TVirtualData; const AColumns:TColumns);
var Totals : TColumnTotals;
begin
  // Create grid band
  Totals:=TColumnTotals.From(AData,AColumns);

  // There is no numeric field in Customer table, so the only thing we can
  // add is a counter:

  Totals.Calculation.Add(AColumns['CustomerID'],TColumnCalculation.Count);

  // Add band to grid footer
  TeeGrid1.Footer.Add(Totals);
end;

end.
