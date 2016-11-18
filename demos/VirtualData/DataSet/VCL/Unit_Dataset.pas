unit Unit_Dataset;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid, Data.DB,
  Datasnap.DBClient, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.DBCtrls;

type
  TFormGridDataset = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    DBNavigator1: TDBNavigator;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGridDataset: TFormGridDataset;

implementation

{$R *.dfm}

uses
  Tee.Grid.Data.DB, VCLTee.Editor.Grid;

procedure TFormGridDataset.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

procedure TFormGridDataset.CheckBox1Click(Sender: TObject);
begin
  ClientDataSet1.Active:=CheckBox1.Checked;
end;

procedure TFormGridDataset.FormCreate(Sender: TObject);
begin
//  TeeGrid1.Data:=TVirtualDBData.From(ClientDataSet1);

  TeeGrid1.Data:=TVirtualDBData.From(DataSource1);
end;

end.
