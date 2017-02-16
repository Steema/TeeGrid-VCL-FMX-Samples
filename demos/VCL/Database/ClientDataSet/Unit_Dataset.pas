unit Unit_Dataset;

interface

{
  Linking a TeeGrid with a TDataSource or TDataSet:

  uses Tee.Grid.Data.DB;

  TeeGrid1.DataSource:= DataSource1;

  TeeGrid1.DataSource:= ClientDataset1;
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid, Data.DB,
  Datasnap.DBClient, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.ExtCtrls;

type
  TFormGridDataset = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    DBNavigator1: TDBNavigator;
    Splitter1: TSplitter;
    ComboSource: TComboBox;
    Label1: TLabel;
    ClientDataSet2: TClientDataSet;
    DataSource2: TDataSource;
    Button2: TButton;
    ClientDataSet3: TClientDataSet;
    DataSource3: TDataSource;
    ClientDataSet3Name: TStringField;
    ClientDataSet3Height: TSingleField;
    ClientDataSet3Address: TStringField;
    ClientDataSet3Children: TIntegerField;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboSourceChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    procedure CheckBigDataset;
  public
    { Public declarations }
  end;

var
  FormGridDataset: TFormGridDataset;

implementation

{$R *.dfm}

uses
  VCLTee.Editor.Grid, Tee.Grid.RowGroup, Tee.Grid;

// Show the TeeGrid editor dialog
procedure TFormGridDataset.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

// Open or close the dataset
procedure TFormGridDataset.Button2Click(Sender: TObject);
begin
  TeeGrid1.Grid.Copy;
end;

procedure TFormGridDataset.CheckBox1Click(Sender: TObject);
begin
  ClientDataSet1.Active:=CheckBox1.Checked;
end;

procedure TFormGridDataset.CheckBigDataset;

  procedure AddSampleRecords;
  var t : Integer;
  begin
    for t:=1 to 10000 do
        ClientDataSet3.AppendRecord(['Abc',3.45,'Some St',t]);
  end;

begin
  if ClientDataSet3.IsEmpty then
  begin
    Screen.Cursor:=crHourGlass;
    try
      ClientDataSet3.DisableControls;
      try
        ClientDataSet3.Close;
        ClientDataSet3.CreateDataSet;

        ClientDataSet3.Open;

        AddSampleRecords;

        ClientDataSet3.First;
      finally
        ClientDataSet3.EnableControls;
      end;
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TFormGridDataset.ComboSourceChange(Sender: TObject);
begin
  case ComboSource.ItemIndex of
    0: begin
         TeeGrid1.DataSource:=nil;
         DBNavigator1.DataSource:=nil;
       end;

    1: begin
         TeeGrid1.DataSource:=ClientDataSet1;

         // Just a test, set a custom row Height
         TeeGrid1.Rows.Height.Automatic:=False;
         TeeGrid1.Rows.Height.Value:= 18;

         DBNavigator1.DataSource:=DataSource1;
       end;

    2: begin
         TeeGrid1.DataSource:=ClientDataSet2;

         // Just a test, set a custom row Height
         TeeGrid1.Rows.Height.Automatic:=False;
         TeeGrid1.Rows.Height.Value:=100;

         DBNavigator1.DataSource:=DataSource2;
       end;

    3: begin
         // "Automatic=True" means row height will be recalculated for
         // every row, so considering possible multi-line text in cells
         TeeGrid1.Rows.Height.Automatic:=True;

         TeeGrid1.DataSource:=DataSource1;
         DBNavigator1.DataSource:=DataSource1;
       end;
  else
    begin
      CheckBigDataSet;

      TeeGrid1.Rows.Height.Automatic:=False;
      TeeGrid1.Rows.Height.Value:=0;

      TeeGrid1.DataSource:=DataSource3;
      DBNavigator1.DataSource:=DataSource3;
    end;
  end;

  TeeGrid1.SetFocus;
end;

procedure TFormGridDataset.FormCreate(Sender: TObject);
begin
  // Enable drag / pan grid scrolling by finger touch and / or left-button mouse
  TeeGrid1.Scrolling.Mode:=TScrollingMode.Both;
//  TeeGrid1.Scrolling.Horizontal:=TScrollDirection.Disabled;

  // Change the selected row when scrolling the grid
  TeeGrid1.Selected.ScrollToView:=True;
end;

end.
