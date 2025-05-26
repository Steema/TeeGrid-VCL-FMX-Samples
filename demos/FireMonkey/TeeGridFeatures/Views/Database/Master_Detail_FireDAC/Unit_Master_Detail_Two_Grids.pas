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
  FMX.StdCtrls, FMX.Layouts, FMX.Objects, FireDAC.Phys.SQLiteWrapper.Stat;

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
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MasterDetail2GridsForm: TMasterDetail2GridsForm;

implementation

{$R *.fmx}

uses
  IOUtils;

function FolderWithSampleData:String;
begin
  result:='fddemo.sdb';

  repeat
    if TFile.Exists(result) then
       exit
    else
       result:=TPath.Combine('..',result);

  until Length(result)>100;
end;

procedure TMasterDetail2GridsForm.cbConnectChange(Sender: TObject);
begin
  CustomersTable.Active:=cbConnect.IsChecked;
  OrdersTable.Active:=cbConnect.IsChecked;
end;

procedure TMasterDetail2GridsForm.FormCreate(Sender: TObject);
var Path : String;
begin
  Path:=FolderWithSampleData;

  if Path='' then
     raise Exception.Create('Error: Missing SQL sample database file FDDEMO.SDB required by this demo');

  Sqlite_demoConnection.Params.Values['Database']:=Path;
end;

end.
