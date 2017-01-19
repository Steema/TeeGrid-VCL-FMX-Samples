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
    Splitter1: TSplitter;
    ComboSource: TComboBox;
    Label1: TLabel;
    ClientDataSet2: TClientDataSet;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboSourceChange(Sender: TObject);
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

procedure TFormGridDataset.ComboSourceChange(Sender: TObject);
var i : integer;
begin
  case ComboSource.ItemIndex of
    0: TeeGrid1.DataSource:=nil;
    1: begin
        TeeGrid1.DataSource:=ClientDataSet1;
        TeeGrid1.Rows.DefaultHeight := 18;
       end;
    2: begin
        TeeGrid1.DataSource:=ClientDataSet2;
        for i := 0 to TeeGrid1.Rows.Count-1 do
          TeeGrid1.Rows.Heights[i] := 100;
        // Follwing line does not work, workaround above.
        //TeeGrid1.Rows.DefaultHeight := 100;
       end
  else
    TeeGrid1.DataSource:=DataSource1;
  end;
end;

end.
