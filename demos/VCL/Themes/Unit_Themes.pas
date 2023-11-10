unit Unit_Themes;

interface

{
  Example of TeeGrid cosmetic customization using "Themes".

  uses Tee.Grid.Themes;
    TGridThemes.Android.ApplyTo(TeeGrid1.Grid);
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid, Tee.Grid, Tee.Grid.Bands,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormGridThemes = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    Button1: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    LBTheme: TListBox;
    Panel4: TPanel;
    Label2: TLabel;
    LBVCLThemes: TListBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LBThemeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LBVCLThemesClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

    procedure CreateSampleData;
  public
    { Public declarations }
  end;

var
  FormGridThemes: TFormGridThemes;

implementation

{$R *.dfm}

uses
  Tee.Painter, Tee.Grid.Themes, Unit_MyData, Tee.Grid.Columns,
  Tee.GridData.Rtti, VCLTee.Editor.Grid,

  VCLTee.Grid.Themes, Vcl.Themes;

var
  Persons : TArray<TPerson>;

procedure TFormGridThemes.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1)
end;

procedure TFormGridThemes.Button2Click(Sender: TObject);
begin
  if Font.Size <= 10 then
     Font.Size := 16
  else
     Font.Size := 10;
end;

procedure TFormGridThemes.CreateSampleData;
begin
  SetLength(Persons,100);
  FillMyData(Persons);  // <-- fill with random values

  // Assign the array to TeeGrid
  TeeGrid1.Data:=TVirtualArrayData<TPerson>.Create(Persons);
end;

procedure TFormGridThemes.FormCreate(Sender: TObject);
begin
  CreateSampleData;

  LBTheme.ItemIndex:=0;

  // Get all VCL themes that are linked to this executable
  // (Project->Options->Application->Appearance)
  TVCLGridThemes.Available(LBVCLThemes.Items);

  LBVCLThemes.Sorted:=True;

  //Grid font size follows parent
  TeeGrid1.ParentFont := True;

  //Do not pick mousemove over Grid (reduces CPU activity)
  //TeeGrid1.Grid.MouseActivity := [TGridMouseSense.Down, TGridMouseSense.Up];

  // Just a test:
  TeeGrid1.Columns['Children'].InitAlign(THorizontalAlign.Center);
end;

procedure TFormGridThemes.LBThemeClick(Sender: TObject);
begin
  if LBTheme.ItemIndex<>-1 then
  begin
    LBVCLThemes.ItemIndex:=-1;

    case LBTheme.ItemIndex of
      0: TGridThemes.Default.ApplyTo(TeeGrid1.Grid);
      1: TGridThemes.iOS.ApplyTo(TeeGrid1.Grid);
      2: TGridThemes.Android.ApplyTo(TeeGrid1.Grid);
      3: TGridThemes.Black.ApplyTo(TeeGrid1.Grid);
    end;
  end;
end;

procedure TFormGridThemes.LBVCLThemesClick(Sender: TObject);
var tmp : String;
begin
  if LBVCLThemes.ItemIndex<>-1 then
  begin
    LBTheme.ItemIndex:=-1;

    tmp:=LBVCLThemes.Items[LBVCLThemes.ItemIndex];

    TStyleManager.SetStyle(tmp);

    TVCLGridThemes.ApplyTo(TeeGrid1);
  end;
end;

end.
