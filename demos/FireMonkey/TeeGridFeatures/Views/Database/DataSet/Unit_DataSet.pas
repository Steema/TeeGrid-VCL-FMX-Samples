unit Unit_DataSet;

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
  Datasnap.DBClient, FMXTee.Control, FMXTee.Grid, Tee.Renders, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.Objects;

type
  TFormGridDataSet = class(TForm)
    ClientDataSet1: TClientDataSet;
    ClientDataSet1Name: TStringField;
    ClientDataSet1Height: TSingleField;
    ClientDataSet1Address: TStringField;
    ClientDataSet1Children: TIntegerField;
    DataSource1: TDataSource;
    Layout1: TLayout;
    Edit1: TEdit;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Rectangle1: TRectangle;
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
    procedure Edit1ChangeTracking(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
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

  function RandomName:String;
  const Names:Array[0..4] of String=('Abc','Jim','Melissa','Spirit','Zune');
  begin
   result:=Names[Random(Length(Names))];
  end;

  procedure AddSampleRecords;
  var t : Integer;
  begin
    for t:=1 to 150 do

        ClientDataSet1.AppendRecord([RandomName,3.45,'Some St',t]);
  end;

begin
  if ClientDataSet1.IsEmpty then
  begin
    ClientDataSet1.DisableControls;
    try
      ClientDataSet1.CreateDataSet;

      ClientDataSet1.Open;

      AddSampleRecords;

      ClientDataSet1.First;
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

procedure TFormGridDataSet.CheckBox1Change(Sender: TObject);
begin
  Edit1ChangeTracking(Self);
end;

procedure TFormGridDataSet.CheckBox2Change(Sender: TObject);
begin
  Edit1ChangeTracking(Self);
end;

procedure TFormGridDataSet.Edit1ChangeTracking(Sender: TObject);

  function CalcFilterOptions:TFilterOptions;
  begin
    result:=[];

    if CheckBox1.IsChecked then
       Include(result,TFilterOption.foCaseInsensitive);

    if not CheckBox2.IsChecked then
       Include(result,TFilterOption.foNoPartialCompare);
  end;

  function GetWildCard : String;
  Begin
     if CheckBox2.IsChecked then
        result := '*'
     else
        result:='';
  end;

var tmpS : String;
begin
  ClientDataSet1.Filtered:=False;

  ClientDataSet1.FilterOptions:=CalcFilterOptions;

  tmpS:=Trim(Edit1.Text);

  if tmpS='' then
     ClientDataSet1.Filter:=''
  else
    // Unfortunately, Delphi ClientDataset Filtering do not support *
    // wildcards at the beginning of the filter string, only at the end
     ClientDataSet1.Filter:='Name = '+QuotedStr(tmpS+GetWildCard);

  ClientDataSet1.Filtered:=ClientDataSet1.Filter<>'';
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
