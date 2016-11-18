unit Unit_Customer_Orders;

interface

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

    Customers,
    Orders,
    OrderItems : TDataItem;

    procedure AddMainTotals(const AData:TVirtualData; const AColumns:TColumns);
    procedure AddOrderTotals(const AGroup:TRowGroup);
    procedure NewDetail(const Sender,NewGroup:TRowGroup);
    procedure SetExpander(const AData:TVirtualData; const AColumn:TColumn);
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
  TTeeGridEditor.Edit(Self,TeeGrid1)
end;

procedure TFormDetailRows.SetExpander(const AData:TVirtualData; const AColumn:TColumn);
var Expander : TExpanderRender;
begin
  Expander:=TExpanderRender.Create(AColumn.Changed);

  Expander.OnCanExpand:=AData.CanExpand;
  Expander.OnGetExpanded:=TeeGrid1.Grid.GetExpanded;

  AColumn.Render:=Expander;
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

procedure TFormDetailRows.NewDetail(const Sender,NewGroup:TRowGroup);
var Data : TBIGridData;
begin
  Data:=(NewGroup.Data as TBIGridData);

  Data.Detail:=OrderItems;

  SetExpander(NewGroup.Data,NewGroup.Columns['OrderID']);

  AddOrderTotals(NewGroup);
end;

procedure TFormDetailRows.FormCreate(Sender: TObject);
var Data : TBIGridData;
begin
  Customers:=TStore.Load('SQLite_Demo')['Customers'];
  Orders:=TStore.Load('SQLite_Demo')['Orders'];
  OrderItems:=TStore.Load('SQLite_Demo')['"Order Details"'];

  Data:=TBIGridData.From(Customers);
  TeeGrid1.Data:=Data;

  Data.Detail:=Orders;

  TeeGrid1.OnNewDetail:=NewDetail;

  AddMainTotals(Data,TeeGrid1.Columns);

  SetExpander(Data,TeeGrid1.Columns['CustomerID']);
end;

procedure TFormDetailRows.AddMainTotals(const AData:TVirtualData; const AColumns:TColumns);
var Totals : TColumnTotals;
begin
  Totals:=TColumnTotals.From(AData,AColumns);
  Totals.Calculation.Add(AColumns['CustomerID'],TColumnCalculation.Count);

  TeeGrid1.Footer.Add(Totals);
end;

end.
