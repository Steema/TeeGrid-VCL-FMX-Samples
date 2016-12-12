unit Unit_TList;

interface

{
  Using TeeGrid with data from a generic TList<T>
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMXTee.Control,
  FMXTee.Grid, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TFormGridTList = class(TForm)
    Layout1: TLayout;
    TeeGrid1: TTeeGrid;
    CBTheme: TComboBox;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CBThemeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGridTList: TFormGridTList;

implementation

{$R *.fmx}

uses
  Tee.Grid.Data, Tee.Grid.Data.Rtti, Tee.Grid.Columns, Tee.Control,

  Unit_MyData, System.Generics.Collections, Tee.Grid.Themes;

// Example of applying "Themes" to a grid
procedure TFormGridTList.CBThemeChange(Sender: TObject);
begin
  case CBTheme.ItemIndex of
    1: TGridThemes.Default.ApplyTo(TeeGrid1.Grid);
    2: TGridThemes.Black.ApplyTo(TeeGrid1.Grid);
    3: TGridThemes.iOS.ApplyTo(TeeGrid1.Grid);
    4: TGridThemes.Android.ApplyTo(TeeGrid1.Grid);
  end;
end;

var
  MyData : TList<TPerson>;  // <-- sample data variable

procedure TFormGridTList.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.IsChecked then
     TeeGrid1.ScrollBars.Horizontal.Visible:=TScrollBarVisible.Hide
  else
     TeeGrid1.ScrollBars.Horizontal.Visible:=TScrollBarVisible.Automatic;
end;

procedure TFormGridTList.FormCreate(Sender: TObject);
begin
  MyData:=TList<TPerson>.Create;

  FillMyData(MyData,20);

  // Set Data from TList<TPerson>
  TeeGrid1.Data:=TVirtualData<TList<TPerson>>.Create(MyData);

// Alternative way, using TeeBI
//  TeeGrid1.Data:=TBIGridData.FromList<TPerson>(MyData);

  TeeGrid1.Rows.Height.Automatic:=True;
end;

procedure TFormGridTList.FormDestroy(Sender: TObject);
begin
  MyData.Free;
end;

end.
