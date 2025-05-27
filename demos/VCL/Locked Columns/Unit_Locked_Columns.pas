unit Unit_Locked_Columns;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  Vcl.StdCtrls, Vcl.ExtCtrls,

  Tee.GridData.Strings, Tee.Grid.Columns, VCLTee.Painter;

type
  TFormLocked = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    LBColumns: TListBox;
    RGLocked: TRadioGroup;
    TeeGrid1: TTeeGrid;
    procedure LBColumnsClick(Sender: TObject);
    procedure RGLockedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    Data : TStringsData;

    procedure FillBox;
    procedure FillCells;
    procedure SetLocked(const AColumn:TColumn; const ALocked:TColumnLocked);
  public
    { Public declarations }
  end;

var
  FormLocked: TFormLocked;

implementation

{$R *.dfm}

uses
  System.UITypes;

procedure TFormLocked.FormCreate(Sender: TObject);
begin
  Data:=TStringsData.Create(30,100);

  FillCells;

  TeeGrid1.Data:=Data;

  //enabling multi-field copy-paste
  TeeGrid1.Grid.QuoteStringsOnCopy := false;
  TeeGrid1.Grid.CopyFieldSeparator := ';';
  TeeGrid1.Selected.Range.Enabled:= True;

  FillBox;

  // Example, lock some columns to left and right edges
  SetLocked(TeeGrid1.Columns['A'],TColumnLocked.Left);
  SetLocked(TeeGrid1.Columns['B'],TColumnLocked.Right);

  // Select first column in listbox
  LBColumns.ItemIndex:=0;
  LBColumnsClick(Self);

//  TeeGrid1.Painter:=TGDIPainter.Create(TeeGrid1.Canvas);
end;

// Initialize grid cells with sample contents
procedure TFormLocked.FillCells;
var t,
    row : Integer;
begin
  for t:=0 to Data.Columns-1 do
      Data.Headers[t]:=Chr(Ord('A')+t);

  for t:=0 to Data.Columns-1 do
      for row:=0 to Data.Rows-1 do
          Data[t,row]:=IntToStr(t)+' x '+IntToStr(row);
end;

// Add all columns to listbox
procedure TFormLocked.FillBox;
var t : Integer;
begin
  LBColumns.Clear;

  for t:=0 to Data.Columns-1 do
      LBColumns.Items.AddObject(Data.Headers[t],Data.ColumnList[t]);
end;

// Refresh radiogroup with selected column Locked value
procedure TFormLocked.LBColumnsClick(Sender: TObject);
var tmp : Integer;
begin
  tmp:=LBColumns.ItemIndex;

  RGLocked.Enabled:=tmp<>-1;

  if tmp<>-1 then
     RGLocked.ItemIndex:=Ord(TeeGrid1.Columns[tmp].Locked);
end;

// Change selected column Locked property with radiogroup value
procedure TFormLocked.RGLockedClick(Sender: TObject);
var Column : TColumn;
    Locked : TColumnLocked;
begin
  Column:=TeeGrid1.Columns[LBColumns.ItemIndex];

  Locked:=TColumnLocked(RGLocked.ItemIndex);

  SetLocked(Column,Locked);
end;

// Set column Locked property, change cosmetic color
procedure TFormLocked.SetLocked(const AColumn:TColumn; const ALocked:TColumnLocked);
begin
  AColumn.Locked:=ALocked;

  if ALocked=TColumnLocked.None then
     AColumn.ParentFormat:=True  // <-- default TeeGrid1.Cells format
  else
  begin
    // Custom brush color
    AColumn.ParentFormat:=False;
    AColumn.Format.Brush.Show;

    if ALocked=TColumnLocked.Left then
       AColumn.Format.Brush.Color:=TColors.Navajowhite
    else
       AColumn.Format.Brush.Color:=TColors.Salmon;
  end;
end;

end.
