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
begin
  case ComboSource.ItemIndex of
    0: TeeGrid1.DataSource:=nil;

    1: begin
         TeeGrid1.DataSource:=ClientDataSet1;

         // Just a test, set a custom row Height
         TeeGrid1.Rows.Height.Automatic:=False;
         TeeGrid1.Rows.Height.Pixels:= 18;
       end;

    2: begin
         TeeGrid1.DataSource:=ClientDataSet2;

         // Just a test, set a custom row Height
         TeeGrid1.Rows.Height.Automatic:=False;
         TeeGrid1.Rows.Height.Pixels:= 100;
       end;
  else
    begin
      // "Automatic=True" means row height will be recalculated for
      // every row, so considering possible multi-line text in cells
      TeeGrid1.Rows.Height.Automatic:=True;

      TeeGrid1.DataSource:=DataSource1;
    end;
  end;
end;

end.
