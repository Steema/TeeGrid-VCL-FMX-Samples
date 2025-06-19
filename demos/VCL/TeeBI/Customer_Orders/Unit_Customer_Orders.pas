{*********************************************}
{  TeeGrid Software Library                   }
{  Master -> Detail sub-grid rows             }
{  Copyright (c) 2015-2018 by Steema Software }
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

  BI.DataItem, Tee.Grid.Rows, Tee.Grid.Columns, Tee.GridData, Tee.Grid.RowGroup;

type
  TFormDetailRows = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    Button1: TButton;
    CBExpander: TComboBox;
    Label1: TLabel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CBExpanderChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

    // Sample data items
    Customers,
    Orders,
    OrderItems : TDataItem;

    procedure AddDetail(const AGroup:TRowGroup; const ADetail:TDataItem; const AColumn:TColumn);
    procedure AddMainTotals;
    procedure AddOrderTotals(const AGroup:TRowGroup);
    procedure NewDetail(const Sender,NewGroup:TRowGroup);
    procedure NewOrderItemsDetail(const Sender,NewGroup:TRowGroup);
    procedure LoadSampleData;
  public
    { Public declarations }
  end;

var
  FormDetailRows: TFormDetailRows;

implementation

{$R *.dfm}

uses
  {System.}UITypes,

  BI.DataSource, BI.Persist, BI.GridData,
  Tee.Grid.Header, Tee.Grid.Totals, Tee.Grid.Themes, Tee.Grid.Bands, Tee.Grid,
  Tee.Format, VCLTee.Editor.Font;

procedure TFormDetailRows.Button1Click(Sender: TObject);
begin
  // Show TeeGrid editor dialog
  TTeeGridEditor.Edit(Self,TeeGrid1)
end;

procedure TFormDetailRows.Button2Click(Sender: TObject);
begin
  TFontEditor.Edit(Self,TeeGrid1.Grid.Current.Cells.Format.Font);
end;

// Change the aspect of the "expanded" box
procedure TFormDetailRows.CBExpanderChange(Sender: TObject);
var tmp : TRender;
begin
  tmp:=TeeGrid1.Columns[0].Render;

  if tmp is TExpanderRender then
     TExpanderRender(tmp).Style:=TExpanderStyle(CBExpander.ItemIndex);
end;

procedure TFormDetailRows.AddOrderTotals(const AGroup:TRowGroup);
var tmp : TColumnTotals;
begin
  tmp:=TColumnTotals.Create(AGroup.Footer);

  tmp.Calculation.Add(AGroup.Columns[0],TColumnCalculation.Count);

  tmp.Calculation.Add('Freight',TColumnCalculation.Sum);
  tmp.Format.Font.Style:=[fsBold];

  TTotalsHeader.CreateTotals(AGroup.Footer,tmp);
end;

procedure TFormDetailRows.AddDetail(const AGroup:TRowGroup; const ADetail:TDataItem; const AColumn:TColumn);
var Expander : TExpanderRender;
begin
  // Set the detail data item
  (AGroup.Data as TBIGridData).Detail:=ADetail;

  // Create a new expander ("+/-" icon) render
  Expander:=AGroup.NewExpander;

  // Customize the expander style ("+"/"-", arrows, etc)
  Expander.Style:=TExpanderStyle(CBExpander.ItemIndex);

  // Set the column Render to be the expander, to enable expand / collapse
  AColumn.Render:=Expander;
end;

// Optional event.
// It is used here to add subtotals for the 3rd level sub-detail (Customer->Orders->OrderItems)
procedure TFormDetailRows.NewOrderItemsDetail(const Sender,NewGroup:TRowGroup);
var tmp : TColumnTotals;
begin
  tmp:=TColumnTotals.Create(NewGroup.Footer);

  tmp.Calculation.Add(NewGroup.Columns[0],TColumnCalculation.Count);

  tmp.Calculation.Add(NewGroup.Columns['UnitPrice'],TColumnCalculation.Average);

  tmp.Calculation.Add(NewGroup.Columns['Quantity'],TColumnCalculation.Sum);

  tmp.Format.Font.Style:=[fsBold];
end;

// Optional event.
// It is used here to add a new 3rd level sub-detail (Customer->Orders->OrderItems)
procedure TFormDetailRows.NewDetail(const Sender,NewGroup:TRowGroup);
begin
  AddDetail(NewGroup,OrderItems,NewGroup.Columns['OrderID']);
  AddOrderTotals(NewGroup);

  NewGroup.OnNewDetail:=NewOrderItemsDetail;
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

// Return a new simple Title band
function TitleSample(const ACollection:TCollection):TTextBand;
begin
  result:=TTextBand.Create(ACollection);
  result.Text:='Some sub-header';

  result.Format.Font.Style:=[fsBold];
  result.Format.Font.Color:=clNavy;
end;

procedure TFormDetailRows.FormCreate(Sender: TObject);
begin
  LoadSampleData;

  // Main data: Customers
  TeeGrid1.Data:=TBIGridData.From(Customers);

  // Detail data: Orders
  AddDetail(TeeGrid1.Grid.Current,Orders,TeeGrid1.Columns['CustomerID']);

  TeeGrid1.OnNewDetail:=NewDetail;

  AddMainTotals;

  // Example, insert a subband at row position 10
  TeeGrid1.Rows.SubBands.Row[10]:=TitleSample(TeeGrid1.Rows.SubBands);
end;

// Add a grid footer band with "Totals" for main Customer table
procedure TFormDetailRows.AddMainTotals;
var Totals : TColumnTotals;
begin
  // Create grid band
  Totals:=TColumnTotals.Create(TeeGrid1.Footer);

  // There is no numeric field in Customer table, so the only thing we can
  // add is a counter:

  Totals.Calculation.Add('CustomerID',TColumnCalculation.Count);
end;

end.
