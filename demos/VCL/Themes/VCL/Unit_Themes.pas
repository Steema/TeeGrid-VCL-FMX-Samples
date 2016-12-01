unit Unit_Themes;

interface

{
  Example of TeeGrid cosmetic customization using "Themes".

  uses Tee.Grid.Themes;
    TGridThemes.Android.ApplyTo(TeeGrid1.Grid);
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormGridThemes = class(TForm)
    TeeGrid1: TTeeGrid;
    LBTheme: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LBThemeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGridThemes: TFormGridThemes;

implementation

{$R *.dfm}

uses
  Tee.Grid.Themes, Unit_MyData, Tee.Grid.Data.Rtti, VCLTee.Editor.Grid;

var
  Persons : TArray<TPerson>;

procedure TFormGridThemes.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1)
end;

procedure TFormGridThemes.FormCreate(Sender: TObject);
begin
  SetLength(Persons,10);
  FillMyData(Persons);

  TeeGrid1.Data:=TVirtualArrayData<TPerson>.Create(Persons);
end;

procedure TFormGridThemes.LBThemeClick(Sender: TObject);
begin
  case LBTheme.ItemIndex of
    0: TGridThemes.Default.ApplyTo(TeeGrid1.Grid);
    1: TGridThemes.iOS.ApplyTo(TeeGrid1.Grid);
    2: TGridThemes.Android.ApplyTo(TeeGrid1.Grid);
    3: TGridThemes.Black.ApplyTo(TeeGrid1.Grid);
  end;
end;

end.
