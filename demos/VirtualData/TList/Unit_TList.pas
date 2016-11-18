unit Unit_TList;

interface

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
  // BI.Grid.Data,
  Tee.Grid.Data, Tee.Grid.Data.Rtti,

  Unit_MyData, System.Generics.Collections, Tee.Grid.Themes;

var
  MyData : TList<TPerson>;

procedure TFormGridTList.CBThemeChange(Sender: TObject);
begin
  case CBTheme.ItemIndex of
    1: TGridThemes.Default.ApplyTo(TeeGrid1.Grid);
    2: TGridThemes.Black.ApplyTo(TeeGrid1.Grid);
    3: TGridThemes.iOS.ApplyTo(TeeGrid1.Grid);
    4: TGridThemes.Android.ApplyTo(TeeGrid1.Grid);
  end;
end;

procedure TFormGridTList.FormCreate(Sender: TObject);
begin
  MyData:=TList<TPerson>.Create;

  FillMyData(MyData,100);

//  TeeGrid1.Data:=TBIGridData.FromList<TPerson>(MyData);
  TeeGrid1.Data:=TVirtualListData<TPerson>.Create(MyData);
end;

end.
