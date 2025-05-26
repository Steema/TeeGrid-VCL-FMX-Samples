unit Unit_Locked_Columns;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMXTee.Control,
  FMXTee.Grid,
  Tee.GridData.Strings, FMXTee.Painter, Tee.Grid.Columns, FMX.Objects;

type
  TLockedColumnsForm = class(TForm)
    TeeGrid1: TTeeGrid;
    Layout1: TLayout;
    Label1: TLabel;
    LBColumns: TListBox;
    BNone: TSpeedButton;
    Label2: TLabel;
    BLeft: TSpeedButton;
    BRight: TSpeedButton;
    Rectangle1: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure LBColumnsClick(Sender: TObject);
    procedure BNoneClick(Sender: TObject);
    procedure TeeGrid1Select(Sender: TObject);
  private
    Data : TStringsData;
    procedure FillCells;
    procedure FillBox;
    procedure SetLocked(const AColumn: TColumn; const ALocked: TColumnLocked);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LockedColumnsForm: TLockedColumnsForm;

implementation

{$R *.fmx}

procedure TLockedColumnsForm.FormCreate(Sender: TObject);
begin
  Data:=TStringsData.Create(30,100);

  FillCells;

  TeeGrid1.Data:=Data;

  FillBox;

  // Example, lock some columns to left and right edges
  SetLocked(TeeGrid1.Columns['A'],TColumnLocked.Left);
  SetLocked(TeeGrid1.Columns['B'],TColumnLocked.Right);

  // Select first column in listbox
  LBColumns.ItemIndex:=0;
  LBColumnsClick(Self);

//  TeeGrid1.Painter:=TGDIPainter.Create(TeeGrid1.Canvas);
end;

procedure TLockedColumnsForm.LBColumnsClick(Sender: TObject);
var tmp, i : Integer;
begin
  tmp:=LBColumns.ItemIndex;

  BNone.Enabled:=tmp<>-1;
  BRight.Enabled:=tmp<>-1;
  BLeft.Enabled:=tmp<>-1;

  i:= Ord(TeeGrid1.Columns[tmp].Locked);
  if tmp<>-1 then
  begin
     case i of
       0 : BNone.IsPressed:=True;
       1 : BLeft.IsPressed:=True;
       2 : BRight.IsPressed:=True;
     end;
  end;
end;

// Initialize grid cells with sample contents
procedure TLockedColumnsForm.FillCells;
var t,
    row : Integer;
begin
  for t:=0 to Data.Columns-1 do
      Data.Headers[t]:=Chr(Ord('A')+t);

  for t:=0 to Data.Columns-1 do
      for row:=0 to Data.Rows-1 do
          Data[t,row]:=IntToStr(t)+' x '+IntToStr(row);
end;

// Change the Locked status of the selected Column
procedure TLockedColumnsForm.BNoneClick(Sender: TObject);
var Column : TColumn;
    Locked : TColumnLocked;
begin
  Column:=TeeGrid1.Columns[LBColumns.ItemIndex];

  if BNone.IsPressed then
     Locked:=TColumnLocked.None
  else
  if BLeft.IsPressed then
     Locked:=TColumnLocked.Left
  else
  if BRight.IsPressed then
     Locked:=TColumnLocked.Right
  else
     Locked:=TColumnLocked.None;

  SetLocked(Column,Locked);
end;

// Add all columns to listbox
procedure TLockedColumnsForm.FillBox;
var t : Integer;
begin
  LBColumns.Clear;

  for t:=0 to Data.Columns-1 do
      LBColumns.Items.AddObject(Data.Headers[t],Data.ColumnList[t]);
end;

// Set column Locked property, change cosmetic color
procedure TLockedColumnsForm.SetLocked(const AColumn:TColumn; const ALocked:TColumnLocked);
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

// Change the current listbox Column when selecting it in the grid
procedure TLockedColumnsForm.TeeGrid1Select(Sender: TObject);
begin
  if TeeGrid1.Selected.Column<>nil then
  begin
    LBColumns.ItemIndex:=TeeGrid1.Selected.Column.Index;
    LBColumnsClick(Self);
  end;
end;

end.
