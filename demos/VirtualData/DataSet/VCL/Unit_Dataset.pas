unit Unit_Dataset;

interface

{
  Linking a TeeGrid with a TDataSource or TDataSet:

  TeeGrid1.DataSource:= DataSource1;
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid, Data.DB,
  Datasnap.DBClient, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Grids,
  Vcl.DBGrids;

type
  TFormGridDataset = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    DBNavigator1: TDBNavigator;
    Panel2: TPanel;
    DBGrid1: TDBGrid;
    Splitter1: TSplitter;
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

// Show the TeeGrid editor dialog
procedure TFormGridDataset.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

// Open or close the dataset
procedure TFormGridDataset.CheckBox1Click(Sender: TObject);
begin
  ClientDataSet1.Active:=CheckBox1.Checked;
end;

end.
