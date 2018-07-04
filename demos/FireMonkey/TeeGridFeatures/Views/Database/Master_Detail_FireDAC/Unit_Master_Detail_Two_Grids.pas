unit Unit_Master_Detail_Two_Grids;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FMXTee.Control, FMXTee.Grid, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Layouts, FMX.Objects;

type
  TMasterDetail2GridsForm = class(TForm)
    Sqlite_demoConnection: TFDConnection;
    CustomersTable: TFDQuery;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    TeeGrid1: TTeeGrid;
    TeeGrid2: TTeeGrid;
    OrdersTable: TFDTable;
    Rectangle1: TRectangle;
    Layout1: TLayout;
    cbConnect: TCheckBox;
    procedure cbConnectChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MasterDetail2GridsForm: TMasterDetail2GridsForm;

implementation

{$R *.fmx}

procedure TMasterDetail2GridsForm.cbConnectChange(Sender: TObject);
begin
  CustomersTable.Active:=cbConnect.IsChecked;
  OrdersTable.Active:=cbConnect.IsChecked;
end;

end.
