unit Unit_FMX_Themes;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Objects, FMXTee.Control, FMXTee.Grid;

type
  TFormGridThemes = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Text1: TText;
    LBTheme: TListBox;
    Layout4: TLayout;
    Text2: TText;
    LBFMXThemes: TListBox;
    TeeGrid1: TTeeGrid;
    procedure LBThemeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LBFMXThemesChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    Styles : TStringList;

    procedure AddStylesToList;
    procedure ChangeFiremonkeyStyle;

    function SelectedStyleFile:String;
    function SelectedStyleIsResource:Boolean;
    function SelectedStyleResourceName:String;
  public
    { Public declarations }
  end;

var
  FormGridThemes: TFormGridThemes;

implementation

{$R *.fmx}

uses
  Tee.Grid.Themes, Unit_MyData, Tee.Grid.Data.Rtti,

  FMXTee.Grid.Themes, Fmx.Styles, System.IOUtils;

function GetAvailableStyles:TStringList;
begin
  result:=TStringList.Create;
  TFMXGridThemes.Available(result);
  result.Sort;
end;

procedure TFormGridThemes.AddStylesToList;
var t : Integer;
begin
  for t:=0 to Styles.Count-1 do
      LBFMXThemes.Items.Add(Styles.ValueFromIndex[t]);
end;

var
  Persons : TArray<TPerson>;

procedure TFormGridThemes.FormCreate(Sender: TObject);
begin
  SetLength(Persons,100);
  FillMyData(Persons);

  TeeGrid1.Data:=TVirtualArrayData<TPerson>.Create(Persons);

  LBTheme.ItemIndex:=0;

  Styles:=GetAvailableStyles;

  AddStylesToList;
end;

procedure TFormGridThemes.FormDestroy(Sender: TObject);
begin
  Styles.Free;
end;

function TFormGridThemes.SelectedStyleIsResource:Boolean;
begin
  result:=Styles.Names[LBFMXThemes.ItemIndex]='Resource';
end;

function TFormGridThemes.SelectedStyleResourceName:String;
begin
  result:=Styles.ValueFromIndex[LBFMXThemes.ItemIndex];
end;

function TFormGridThemes.SelectedStyleFile:String;
var tmp : Integer;
begin
  tmp:=LBFMXThemes.ItemIndex;

  result:=Styles.Names[tmp];
  result:=TPath.Combine(result,Styles.ValueFromIndex[tmp]);
end;

procedure TFormGridThemes.ChangeFiremonkeyStyle;
var tmp,
    tmpExt : String;
begin
  if SelectedStyleIsResource then
     TStyleManager.TrySetStyleFromResource(SelectedStyleResourceName)
  else
  begin
    tmp:=SelectedStyleFile;

    tmpExt:=TPath.GetExtension(tmp);

    if SameText(tmpExt,'.fsf') or SameText(tmpExt,'.vsf') then
       TStyleManager.SetStyle(TStyleStreaming.LoadFromFile(tmp))
    else
       TStyleManager.SetStyleFromFile(SelectedStyleFile);
  end;
end;

procedure TFormGridThemes.LBFMXThemesChange(Sender: TObject);
begin
  if LBFMXThemes.ItemIndex<>-1 then
  begin
    LBTheme.ItemIndex:=-1;

    ChangeFiremonkeyStyle;

    TFMXGridThemes.ApplyTo(TeeGrid1);
  end;
end;

procedure TFormGridThemes.LBThemeChange(Sender: TObject);
begin
  if LBTheme.ItemIndex<>-1 then
  begin
    LBFMXThemes.ItemIndex:=-1;

    case LBTheme.ItemIndex of
      0: TGridThemes.Default.ApplyTo(TeeGrid1.Grid);
      1: TGridThemes.iOS.ApplyTo(TeeGrid1.Grid);
      2: TGridThemes.Android.ApplyTo(TeeGrid1.Grid);
      3: TGridThemes.Black.ApplyTo(TeeGrid1.Grid);
    end;
  end;
end;

end.
