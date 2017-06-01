unit Unit_Main;

{
  Linking a TeeGrid with a TDataSource or TDataSet:

  uses Tee.GridData.DB;  // <-- not necessary, it is automatically used

    TeeGrid1.DataSource:= DataSource1;

    TeeGrid1.DataSource:= ClientDataset1;
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Data.DB,
  Datasnap.DBClient, FMXTee.Control, FMXTee.Grid, Tee.Renders;

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
  Tee.Grid.RowGroup, Tee.Painter;

procedure TFormGridDataSet.CheckBigDataset;

  procedure AddSampleRecords;
  var t : Integer;
  begin
    for t:=1 to 150 do
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

procedure Cosmetic(const ARender:TTextRender);
begin
  ARender.Format.Font.Size:=10;

  ARender.Margins.Bottom.Value:=0;
  ARender.Margins.Top.Value:=0;
end;

procedure TFormGridDataSet.FormCreate(Sender: TObject);
begin
  CheckBigDataSet;

  // Enable using the mouse to drag and scroll grid contents
  TeeGrid1.Scrolling.Mode:=TScrollingMode.Both;

  // Set data
  TeeGrid1.DataSource:=DataSource1;

  //Cosmetic(TeeGrid1.Cells);
  //Cosmetic(TeeGrid1.Selected);

  // TeeGrid1.Selected.ScrollToView:=True;

  TeeGrid1.Cells.Trimming.Mode:=TTrimmingMode.Character;
end;

end.
