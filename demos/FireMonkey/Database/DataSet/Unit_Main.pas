unit Unit_Main;

{
  Linking a TeeGrid with a TDataSource or TDataSet:

  uses Tee.Grid.Data.DB;

  TeeGrid1.DataSource:= DataSource1;

  TeeGrid1.DataSource:= ClientDataset1;
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Data.DB,
  Datasnap.DBClient, FMXTee.Control, FMXTee.Grid;

type
  TFormGridDataSet = class(TForm)
    TeeGrid1: TTeeGrid;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Name: TStringField;
    ClientDataSet1Height: TSingleField;
    ClientDataSet1Address: TStringField;
    ClientDataSet1Children: TIntegerField;
    DataSource1: TDataSource;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    procedure CheckBigDataset;
  public
    { Public declarations }
  end;

var
  FormGridDataSet: TFormGridDataSet;

implementation

{$R *.fmx}

uses
  Tee.Grid.RowGroup;

procedure TFormGridDataSet.CheckBigDataset;

  procedure AddSampleRecords;
  var t : Integer;
  begin
    for t:=1 to 50000 do
        ClientDataSet1.AppendRecord(['Abc',3.45,'Some St',t]);
  end;

begin
  if ClientDataSet1.IsEmpty then
  begin
    ClientDataSet1.DisableControls;
    try
      ClientDataSet1.CreateDataSet;

      ClientDataSet1.Open;

      AddSampleRecords;
    finally
      ClientDataSet1.EnableControls;
    end;
  end;
end;

procedure TFormGridDataSet.FormCreate(Sender: TObject);
begin
  CheckBigDataSet;

  TeeGrid1.Scrolling.Mode:=TScrollingMode.Both;

  TeeGrid1.DataSource:=DataSource1;
end;

end.
