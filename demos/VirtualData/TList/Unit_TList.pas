unit Unit_TList;

interface

{
  Using TeeGrid with data from a generic TList<T>
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMXTee.Control,
  FMXTee.Grid, FMX.Layouts, FMX.ListBox;

type
  TFormGridTList = class(TForm)
    Layout1: TLayout;
    TeeGrid1: TTeeGrid;
    CBTheme: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CBThemeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
  Tee.Grid.Data, Tee.Grid.Data.Rtti,

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

procedure TFormGridTList.FormCreate(Sender: TObject);
begin
  MyData:=TList<TPerson>.Create;

  FillMyData(MyData,20);

  // Set Data from TList<TPerson>
  TeeGrid1.Data:=TVirtualData<TList<TPerson>>.Create(MyData);

// Alternative way, using TeeBI
//  TeeGrid1.Data:=TBIGridData.FromList<TPerson>(MyData);
end;

procedure TFormGridTList.FormDestroy(Sender: TObject);
begin
  MyData.Free;
end;

end.
