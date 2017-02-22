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
    CBScrollBar: TCheckBox;
    CBAncestor: TCheckBox;
    CBExample: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CBThemeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CBScrollBarChange(Sender: TObject);
    procedure CBAncestorChange(Sender: TObject);
    procedure CBExampleChange(Sender: TObject);
  private
    { Private declarations }

    procedure CarsExample;
    procedure CreateExample;
    procedure PersonsExample;
  public
    { Public declarations }
  end;

var
  FormGridTList: TFormGridTList;

implementation

{$R *.fmx}

uses
  {System.}TypInfo,

  Tee.GridData, Tee.GridData.Rtti, Tee.Grid.Columns, Tee.Control,

  Unit_MyData, System.Generics.Collections, Tee.Grid.Themes;

// Sample data variables
var
  MyPersons : TList<TPerson>;
  MyCars    : TObjectList<TCar>;  // or: TList<TCar>

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

procedure TFormGridTList.CBAncestorChange(Sender: TObject);
begin
  CreateExample;
end;

procedure TFormGridTList.CBExampleChange(Sender: TObject);
begin
  CreateExample;

  CBAncestor.Enabled:=CBExample.ItemIndex=1;
end;

procedure TFormGridTList.CBScrollBarChange(Sender: TObject);
begin
  if CBScrollBar.IsChecked then
     TeeGrid1.ScrollBars.Horizontal.Visible:=TScrollBarVisible.Automatic
  else
     TeeGrid1.ScrollBars.Horizontal.Visible:=TScrollBarVisible.Hide;
end;

const
  PublicAndPublished=[TMemberVisibility.mvPublic,TMemberVisibility.mvPublished];

procedure TFormGridTList.PersonsExample;
begin
  // Set Data from: TList<TPerson>

  TeeGrid1.Data:=TVirtualListData<TPerson>.Create(MyPersons,
                            PublicAndPublished,
                            TRttiMembers.Both,
                            CBAncestor.IsChecked);

// Alternative way:
//  TeeGrid1.Data:=TVirtualData<TList<TPerson>>.Create(MyPersons....);

// Alternative way, using TeeBI:
//  TeeGrid1.Data:=TBIGridData.FromList<TPerson>(MyPersons);
end;

procedure TFormGridTList.CarsExample;
begin
  // Set Data from: TObjectList<TCar>

  TeeGrid1.Data:=TVirtualObjectListData<TCar>.Create(MyCars,

                            PublicAndPublished,

                            TRttiMembers.Both,

                            CBAncestor.IsChecked);
end;

// Use combobox
procedure TFormGridTList.CreateExample;
begin
  if CBExample.ItemIndex=0 then
     PersonsExample
  else
     CarsExample;
end;

procedure TFormGridTList.FormCreate(Sender: TObject);
begin
  // Create sample data

  MyPersons:=TList<TPerson>.Create;
  FillMyData(MyPersons,20);

  MyCars:=TObjectList<TCar>.Create;
  FillMyData(MyCars,20);

  // Fill grid
  CreateExample;

  // Set independent best-height for each row
  TeeGrid1.Rows.Height.Automatic:=True;
end;

procedure TFormGridTList.FormDestroy(Sender: TObject);
begin
  // Release memory
  MyPersons.Free;
  MyCars.Free;
end;

end.
