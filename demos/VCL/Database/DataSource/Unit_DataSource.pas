unit Unit_DataSource;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Datasnap.DBClient,
  VCLTee.Control, VCLTee.Grid, Tee.GridData.DB, Vcl.StdCtrls,
  Vcl.Grids, Vcl.DBGrids, VCLTee.Painter, Vcl.DBCtrls, Tee.Painter,
  System.Generics.Collections, Vcl.ExtCtrls;

type
  TForm43 = class(TForm)
    TeeGrid1: TTeeGrid;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    CheckBox1: TCheckBox;
    ClientDataSet1NAME: TStringField;
    ClientDataSet1SIZE: TSmallintField;
    ClientDataSet1WEIGHT: TSmallintField;
    ClientDataSet1AREA: TStringField;
    ClientDataSet1BMP: TBlobField;
    DBImage1: TDBImage;
    Button2: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    cbRowSelect: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbRowSelectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form43: TForm43;

implementation

{$R *.dfm}

uses
VCLTee.Editor.Grid;

procedure TForm43.Button2Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1)
end;

procedure TForm43.cbRowSelectClick(Sender: TObject);
begin
  TeeGrid1.Selected.FullRow := cbRowSelect.Checked;
end;

procedure TForm43.CheckBox1Click(Sender: TObject);
begin
  ClientDataSet1.Active:=CheckBox1.Checked
end;

procedure TForm43.FormCreate(Sender: TObject);
begin
  CheckBox1.Checked:=ClientDataSet1.Active;
  TeeGrid1.Rows.Height.Value:=64;
  TeeGrid1.Rows.Spacing.Value:=8;
  TeeGrid1.Rows.RowLines.Hide;
  TeeGrid1.Cells.TextAlign.Vertical:=TVerticalAlign.Center;
  TeeGrid1.Selected.TextAlign.Vertical:=TVerticalAlign.Center;
end;

end.
