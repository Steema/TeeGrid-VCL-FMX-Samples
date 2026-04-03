{*********************************************}
{  TeeGrid Software Library                   }
{  Grid Sortable Header class for TDataSet    }
{  Copyright (c) 2025 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.GridData.DB.Sortable;
{$I Tee.inc}

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

  // Optional, when removing the dataset or datasource, do a cleanup of
  // possibly created indexes:
}

uses
  Tee.Grid, Tee.Grid.Header, Tee.Grid.Columns,

  // Dependencies:
  Data.DB,

  {$IFDEF FPC}
  BufDataset
  {$ELSE}
  Datasnap.DBClient, FireDAC.Comp.DataSet, FireDAC.Stan.Intf
  {$ENDIF}
  ;

type
  // Base class, shared by TClientDataSet and TFDDataSet
  TSortableDataset=class(TSortableHeader)
  private
    FGrid : TCustomTeeGrid;

    // Remember when changing the order
    SelectedColumn : TColumn;

    SortColumn,
    SortSubColumn: Integer;
    SortAsc: Boolean;

    procedure CanSortBy(const AColumn: TColumn; var CanSort: Boolean);
    function FindIndex(const AFieldName:String; const AIndexDefs:TIndexDefs):TIndexDef;
    function FindIndexName(const AIndexName:String; const AIndexDefs:TIndexDefs):TIndexDef;
    procedure PrepareColumnOrder(const AColumn:TColumn);
    function SameOrder(const AIndex:TIndexDef):Boolean;
    procedure SortState(const AColumn: TColumn; var State: TSortState);
  end;

  {$IFDEF FPC}
  TClientDataSet=TBufDataSet;
  {$ENDIF}
 
  // TClientDataset specific
  TSortableClientDataset=class(TSortableDataSet)
  private
    FDataSet : TClientDataSet;

    procedure SortBy(Sender: TObject; const AColumn: TColumn);
  public
    class function CreateSortable(const AGrid:TCustomTeeGrid; const ADataSet:TClientDataSet):TSortableClientDataset; static;
    class procedure CleanUp(const ADataSet:TClientDataSet); static;
  end;

  {$IFNDEF FPC}

  // FireDAC TFDDataSet specific
  TSortableFireDACDataset=class(TSortableDataSet)
  private
    FDataSet : TFDDataSet;

    procedure SortBy(Sender: TObject; const AColumn: TColumn);
  public
    class function CreateSortable(const AGrid:TCustomTeeGrid; const ADataSet:TFDDataSet):TSortableFireDACDataset; static;
    class procedure CleanUp(const ADataSet:TFDDataSet); static;
  end;

  {$ENDIF}

implementation

uses
  System.Classes, System.SysUtils,
  Tee.Grid.RowGroup;

{ TSortableDataset }

procedure TSortableDataset.CanSortBy(const AColumn: TColumn; var CanSort: Boolean);

  function ValidField:Boolean;
  begin
    result:=AColumn.TagObject is TField;

    if result then
    begin
      result:=not TField(AColumn.TagObject).IsBlob;

      {$IFNDEF FPC}
      if result then
         if TField(AColumn.TagObject) is TObjectField then // ADT, DataSet, Array...
            result:=False;
      {$ENDIF}
    end;
  end;

begin
  CanSort:=(AColumn<>nil) and ValidField;
end;

function ParentColumn(AGrid: TCustomTeeGrid; AColumn: TColumn): TColumn;
var t: Integer;
begin
  if AColumn.Parent<>nil then
     for t:=0 to AGrid.Columns.Count-1 do
         if AColumn.Parent=AGrid.Columns[t] then
         begin
           result:=AGrid.Columns[t];
           Exit;
         end;

  result:=nil;
end;

procedure TSortableDataset.SortState(const AColumn: TColumn; var State: TSortState);

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

procedure TSortableDataset.PrepareColumnOrder(const AColumn:TColumn);
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

function TSortableDataset.SameOrder(const AIndex:TIndexDef):Boolean;
begin
  result:=not (ixDescending in AIndex.Options);

  if SortAsc then
     result:=not result;
end;

function TSortableDataset.FindIndexName(const AIndexName:String; const AIndexDefs:TIndexDefs):TIndexDef;
var t : Integer;
begin
  for t:=0 to AIndexDefs.Count-1 do
      if AIndexDefs[t].Name=AIndexName then
      begin
        result:=AIndexDefs[t];
        Exit;
      end;

  result:=nil;
end;

function TSortableDataset.FindIndex(const AFieldName:String; const AIndexDefs:TIndexDefs):TIndexDef;
var t : Integer;
begin
  result:=AIndexDefs.GetIndexForFields(AFieldName,False);

  if (result=nil) or (not SameOrder(result)) then
  begin
    for t:=0 to AIndexDefs.Count-1 do
    begin
      result:=AIndexDefs[t];

      if SameText(result.Fields,AFieldName) and SameOrder(result) then
         Exit;
    end;

    result:=nil;
  end;
end;

const
  IndexPrefix='_teegrid_';

{ TSortableClientDataset }

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

procedure TSortableClientDataset.SortBy(Sender: TObject; const AColumn: TColumn);

  procedure SortDataset(const AIndex:String);
  begin
    FDataSet.IndexName:=AIndex;
  end;

var tmp : TIndexDef;
    tmpIndexName,
    tmpFieldName : String;
    tmpOptions : TIndexOptions;
begin
  PrepareColumnOrder(AColumn);

  if AColumn.TagObject is TField then
  begin
    SelectedColumn:=FGrid.Current.Selected.Column; // remember currently selected column

    tmpFieldName:=TField(AColumn.TagObject).FieldName;

    tmp:=FindIndex(tmpFieldName,FDataSet.IndexDefs);

    if tmp=nil then
    begin
      if not SortAsc then
         tmpOptions:=[]
      else
         tmpOptions:=[ixDescending];

      tmpIndexName:=IndexPrefix+tmpFieldName;

      {$IFNDEF FPC}  // ZeosLib and UniDac for Lazarus support deleting/adding indexes

      if FindIndexName(tmpIndexName,FDataSet.IndexDefs)<>nil then
         FDataSet.DeleteIndex(tmpIndexName);

      FDataSet.AddIndex(tmpIndexName,tmpFieldName,tmpOptions);
      
      {$ENDIF}

      tmp:=FindIndex(tmpFieldName,FDataSet.IndexDefs);
    end;

    // Activate Sort !
    if tmp<>nil then
    begin
      SortDataset(tmp.Name);

      // Restore selected column
      FGrid.Current.Selected.Column:=SelectedColumn;
    end;
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

{$IFNDEF FPC}

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

procedure TSortableFireDACDataset.SortBy(Sender: TObject; const AColumn: TColumn);

  procedure SortDataset(const AIndex:String);
  begin
    FDataSet.IndexName:=AIndex;
  end;

var tmp : TIndexDef;
    tmpIndexName,
    tmpFieldName : String;
    tmpOptions : TFDSortOptions;
begin
  PrepareColumnOrder(AColumn);

  if AColumn.TagObject is TField then
  begin
    SelectedColumn:=FGrid.Current.Selected.Column; // remember currently selected column

    tmpFieldName:=TField(AColumn.TagObject).FieldName;

    tmp:=FindIndex(tmpFieldName,FDataSet.IndexDefs);

    if tmp=nil then
    begin
      if not SortAsc then
         tmpOptions:=[]
      else
         tmpOptions:=[soDescending];

      tmpIndexName:=IndexPrefix+tmpFieldName;

      {$IFNDEF FPC} // No support for on-the-fly deleting and adding indexes on default TBufDataSet
      
      // ZeosLib and UniDac for Lazarus support deleting/adding indexes

      if FindIndexName(tmpIndexName,FDataSet.IndexDefs)<>nil then
         FDataSet.DeleteIndex(tmpIndexName);

      FDataSet.AddIndex(tmpIndexName,tmpFieldName,'',tmpOptions);

      {$ENDIF}

      tmp:=FindIndex(tmpFieldName,FDataSet.IndexDefs);
    end;

    // Activate Sort !
    if tmp<>nil then
    begin
      SortDataset(tmp.Name);

      // Restore selected column
      FGrid.Current.Selected.Column:=SelectedColumn;
    end;
  end;
end;

class procedure TSortableFireDACDataset.CleanUp(const ADataSet: TFDDataSet);
var t : Integer;
begin
  t:=0;

  while t<ADataSet.IndexDefs.Count do
      if ADataSet.IndexDefs[t].Name.StartsWith(IndexPrefix) then
         ADataSet.IndexDefs.Delete(t)
      else
         Inc(t);
end;

{$ENDIF}

end.
