unit Unit_Custom_Sorting;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,

  Tee.Grid.Header, Tee.Grid.Columns, System.Generics.Defaults, System.Generics.Collections,
  Math, Tee.GridData.Rtti, Unit_MyData;

type
  TForm1 = class(TForm)
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function CreateSortable:TSortableHeader;
    procedure HeaderCanSortBy(const AColumn: TColumn; var CanSort: Boolean);
    procedure HeaderSortBy(Sender: TObject; const AColumn: TColumn);
    procedure HeaderSortState(const AColumn:TColumn; var State:TSortState);
    procedure SortData(const AColumn:Integer; const Ascending:Boolean); overload;
    procedure SortData(const AColumn:TColumn; const Ascending:Boolean); overload;
  public
    { Public declarations }

    Persons : TArray<TPerson>;

    SortColumn,
    SortSubColumn: Integer;
    SortAsc: Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ Data }
type
  TPersonField=(Name,Street,Number,BirthDate,Children,Height);

  TPersonComparer=class(TComparer<TPerson>)
  public
    Ascending : Boolean;
    PersonField : TPersonField;

    function Compare(const Left, Right: TPerson): Integer; override;
    class function FieldOf(const AName:String):TPersonField; static;
  end;

{ Form1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLength(Persons,10);
  FillMyData(Persons);

  SortColumn:=-1;
  SortSubColumn:=-1;

  TeeGrid1.Data:=TVirtualData<TArray<TPerson>>.Create(Persons);

  TeeGrid1.Header.SortRender:=CreateSortable;
end;

function TForm1.CreateSortable:TSortableHeader;
begin
  result:=TSortableHeader.Create(TeeGrid1.Header.Changed);

  // Set custom events
  result.OnCanSort:=HeaderCanSortBy;
  result.OnSortBy:=HeaderSortBy;
  result.OnSortState:=HeaderSortState;
end;

procedure TForm1.HeaderCanSortBy(const AColumn: TColumn; var CanSort: Boolean);
begin
  CanSort:=(AColumn<>nil) and (AColumn.Index<>4);
end;

function ParentColumn(AGrid: TTeeGrid; AColumn: TColumn): TColumn;
var i: Integer;
begin
  if AColumn.Parent<>nil then
     for i:=0 to AGrid.Columns.Count-1 do
         if AColumn.Parent = AGrid.Columns[i] then
            Exit(AGrid.Columns[i]);

  Result:=nil;
end;

procedure TForm1.HeaderSortBy(Sender:TObject; const AColumn:TColumn);
var ParentCol: TColumn;
begin
  ParentCol:=ParentColumn(TeeGrid1, AColumn);

  if ParentCol<>nil then
  begin
    if (SortColumn=ParentCol.Index) and (SortSubColumn=AColumn.Index) then
        SortAsc:=not SortAsc;

    SortColumn:=ParentCol.Index;
    SortSubColumn:=AColumn.Index;

    {
    if (SortColumn=1) and (SortSubColumn=0) then
       SortAsc:=True
    else
       SortAsc:=False;
    }
  end
  else
  begin
    if SortColumn=AColumn.Index then
       SortAsc:=not SortAsc
    else
    begin
      SortColumn:=AColumn.Index;

      if SortColumn=0 then
         SortAsc:=True
      else
         SortAsc:=False;
    end;
  end;

  SortData(AColumn,SortAsc);
end;

procedure TForm1.SortData(const AColumn:Integer; const Ascending:Boolean);
begin
  SortData(TeeGrid1.Columns[AColumn], Ascending);
end;

procedure TForm1.SortData(const AColumn:TColumn; const Ascending:Boolean);
var Comparer : TPersonComparer;
begin
  Comparer:=TPersonComparer.Create;
  try
    Comparer.Ascending:=Ascending;
    Comparer.PersonField:=TPersonComparer.FieldOf(AColumn.Header.Text);

    TArray.Sort<TPerson>(Persons,Comparer);
  finally
    Comparer.Free;
  end;
end;

procedure TForm1.HeaderSortState(const AColumn:TColumn; var State:TSortState);

  function StateOf(const IsSorted:Boolean):TSortState;
  begin
    if IsSorted then
       if SortAsc then
          result:=Descending
       else
          result:=Ascending
    else
      result:=TSortState.None;
  end;

var ParentCol: TColumn;
begin
  if AColumn.HasItems then
     State:=TSortState.None
  else
  begin
    ParentCol:=ParentColumn(TeeGrid1,AColumn);

    if ParentCol=nil then
       State:=StateOf(SortColumn=AColumn.Index)
    else
       State:=StateOf((ParentCol.Index=SortColumn) and (AColumn.Index=SortSubColumn));
  end;
end;

function TPersonComparer.Compare(const Left, Right: TPerson): Integer;
var APerson, BPerson: TPerson;
begin
  if Ascending then
  begin
    APerson:=Left;
    BPerson:=Right;
  end
  else
  begin
    APerson:=Right;
    BPerson:=Left;
  end;

  case PersonField of
    Name  : result:=CompareText(APerson.Name,BPerson.Name);
    Street: result:=CompareText(APerson.Address.Street, BPerson.Address.Street);
    Number: result:=CompareValue(APerson.Address.Number, BPerson.Address.Number);
    BirthDate: result:=CompareValue(APerson.BirthDate, BPerson.BirthDate);
    Children : result:=CompareValue(APerson.Children, BPerson.Children);
    Height: result:=CompareValue(APerson.Height, BPerson.Height);
  else
    result:=0;
  end;
end;

class function TPersonComparer.FieldOf(const AName: String): TPersonField;
begin
  if AName='Name' then
     result:=TPersonField.Name
  else
  if AName='Street' then
     result:=TPersonField.Street
  else
  if AName='Number' then
     result:=TPersonField.Number
  else
  if AName='BirthDate' then
     result:=TPersonField.BirthDate
  else
  if AName='Children' then
     result:=TPersonField.Children
  else
     result:=TPersonField.Height;
end;

end.
