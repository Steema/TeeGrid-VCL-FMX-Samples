{*********************************************}
{  TeeGrid Software Library                   }
{  Grid Sortable Header class for TDataSet    }
{  Copyright (c) 2025 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.DB.SortableDataSet;

interface

{
  This unit enables sorting a TeeGrid by a column when the grid is
  connected to a TClientDataSet (DataSnap), or to a FireDAC TFDDataSet
  (TFDMemTable etc).

  USAGE EXAMPLE:

  Set the TeeGrid1.DataSource to your dataset, and then:

  uses
    Tee.Grid.DB.SortableDataSet;

  // For TClientDataSet:
  TSortableClientDataset.CreateSortable(TeeGrid1.Grid,ClientDataSet1);

  // For FireDAC TFDDataSet:
  TSortableFireDACDataset.CreateSortable(TeeGrid1.Grid,FDMemTable1);

}

uses
  Tee.Grid, Tee.Grid.Header, Tee.Grid.Columns,

  Datasnap.DBClient, FireDAC.Comp.DataSet;

type
  TSortableDataset=class(TSortableHeader)
  private
    FGrid : TCustomTeeGrid;

    SortColumn,
    SortSubColumn: Integer;
    SortAsc: Boolean;
  end;

  TSortableClientDataset=class(TSortableDataSet)
  private
    FDataSet : TClientDataSet;

    procedure CanSortBy(const AColumn: TColumn; var CanSort: Boolean);
    procedure SortBy(Sender: TObject; const AColumn: TColumn);
    procedure SortState(const AColumn: TColumn; var State: TSortState);
  public
    class function CreateSortable(const AGrid:TCustomTeeGrid; const ADataSet:TClientDataSet):TSortableClientDataset; static;
    class procedure CleanUp(const ADataSet:TClientDataSet); static;
  end;

  TSortableFireDACDataset=class(TSortableDataSet)
  private
    FDataSet : TFDDataSet;

    procedure CanSortBy(const AColumn: TColumn; var CanSort: Boolean);
    procedure SortBy(Sender: TObject; const AColumn: TColumn);
    procedure SortState(const AColumn: TColumn; var State: TSortState);
  public
    class function CreateSortable(const AGrid:TCustomTeeGrid; const ADataSet:TFDDataSet):TSortableFireDACDataset; static;
  end;

implementation

uses
  Data.DB, System.Classes, System.SysUtils;

procedure TSortableClientDataset.CanSortBy(const AColumn: TColumn; var CanSort: Boolean);
begin
  CanSort:=(AColumn<>nil);
end;

function ParentColumn(AGrid: TCustomTeeGrid; AColumn: TColumn): TColumn;
var i: Integer;
begin
  if AColumn.Parent<>nil then
     for i:=0 to AGrid.Columns.Count-1 do
         if AColumn.Parent = AGrid.Columns[i] then
            Exit(AGrid.Columns[i]);

  Result:=nil;
end;

const
  IndexPrefix='_teegrid_';

procedure TSortableClientDataset.SortBy(Sender: TObject; const AColumn: TColumn);

  procedure PrepareColumnOrder;
  var ParentCol: TColumn;
  begin
    ParentCol:=ParentColumn(FGrid, AColumn);

    if ParentCol<>nil then
    begin
      if (SortColumn=ParentCol.Index) and (SortSubColumn=AColumn.Index) then
          SortAsc:=not SortAsc;

      SortColumn:=ParentCol.Index;
      SortSubColumn:=AColumn.Index;
    end
    else
    begin
      if SortColumn=AColumn.Index then
         SortAsc:=not SortAsc
      else
      begin
        SortColumn:=AColumn.Index;
        SortAsc:=SortColumn=0;
      end;
    end;
  end;

  function SameOrder(const AIndex:TIndexDef):Boolean;
  begin
    result:=not (ixDescending in AIndex.Options);

    if SortAsc then
       result:=not result;
  end;

  function FindIndexName(const AIndexName:String):TIndexDef;
  var t : Integer;
  begin
    for t:=0 to FDataSet.IndexDefs.Count-1 do
        if FDataSet.IndexDefs[t].Name=AIndexName then
        begin
          result:=FDataSet.IndexDefs[t];
          Exit;
        end;

    result:=nil;
  end;

  function FindIndex(const AFieldName:String):TIndexDef;
  var t : Integer;
  begin
    result:=FDataSet.IndexDefs.GetIndexForFields(AFieldName,False);

    if (result=nil) or (not SameOrder(result)) then
    begin
      for t:=0 to FDataSet.IndexDefs.Count-1 do
      begin
        result:=FDataSet.IndexDefs[t];

        if SameText(result.Fields,AFieldName) and SameOrder(result) then
           Exit;
      end;

      result:=nil;
    end;
  end;

var tmp : TIndexDef;
    tmpIndexName,
    tmpFieldName : String;
    tmpOptions : TIndexOptions;
begin
  PrepareColumnOrder;

  if AColumn.TagObject is TField then
  begin
    tmpFieldName:=TField(AColumn.TagObject).FieldName;

    tmp:=FindIndex(tmpFieldName);

    if tmp=nil then
    begin
      if not SortAsc then
         tmpOptions:=[]
      else
         tmpOptions:=[ixDescending];

      tmpIndexName:=IndexPrefix+tmpFieldName;

      if FindIndexName(tmpIndexName)<>nil then
         FDataSet.DeleteIndex(tmpIndexName);

      FDataSet.AddIndex(tmpIndexName,tmpFieldName,tmpOptions);

      tmp:=FindIndex(tmpFieldName);
    end;

    if tmp<>nil then
    begin
      FDataSet.IndexName:=tmp.Name;
    end;
  end;
end;

procedure TSortableClientDataset.SortState(const AColumn: TColumn; var State: TSortState);

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
    ParentCol:=ParentColumn(FGrid,AColumn);

    if ParentCol=nil then
       State:=StateOf(SortColumn=AColumn.Index)
    else
       State:=StateOf((ParentCol.Index=SortColumn) and (AColumn.Index=SortSubColumn));
  end;
end;

class procedure TSortableClientDataset.CleanUp(const ADataSet: TClientDataSet);
var t : Integer;
begin
  t:=0;

  while t<ADataSet.IndexDefs.Count do
      if ADataSet.IndexDefs[t].Name.StartsWith(IndexPrefix) then
         ADataSet.IndexDefs.Delete(t)
      else
         Inc(t);
end;

class function TSortableClientDataset.CreateSortable(const AGrid:TCustomTeeGrid; const ADataSet:TClientDataSet):TSortableClientDataset;
begin
  result:=TSortableClientDataset.Create(AGrid.Header.Changed);

  result.FDataSet:=ADataSet;
  result.FGrid:=AGrid;

  result.SortColumn:=-1;  // Find DEFAULT_ORDER field name, if any !

  // Set custom events
  result.OnCanSort:=result.CanSortBy;
  result.OnSortBy:=result.SortBy;
  result.OnSortState:=result.SortState;

  AGrid.Header.SortRender:=result;
end;

{ TSortableFireDACDataset }

class function TSortableFireDACDataset.CreateSortable(
  const AGrid: TCustomTeeGrid;
  const ADataSet: TFDDataSet): TSortableFireDACDataset;
begin
  result:=TSortableFireDACDataset.Create(AGrid.Header.Changed);

  result.SortColumn:=-1;

  result.FDataSet:=ADataSet;
  result.FGrid:=AGrid;

  // Set custom events
  result.OnCanSort:=result.CanSortBy;
  result.OnSortBy:=result.SortBy;
  result.OnSortState:=result.SortState;

  AGrid.Header.SortRender:=result;
end;

procedure TSortableFireDACDataset.SortBy(Sender: TObject;
  const AColumn: TColumn);
begin

end;

procedure TSortableFireDACDataset.SortState(const AColumn: TColumn;
  var State: TSortState);
begin

end;

procedure TSortableFireDACDataset.CanSortBy(const AColumn: TColumn;
  var CanSort: Boolean);
begin

end;

end.
