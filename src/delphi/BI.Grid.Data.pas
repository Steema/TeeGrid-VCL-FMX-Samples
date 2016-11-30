{*********************************************}
{  TeeGrid Software Library                   }
{  TeeBI Virtual Data class                   }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Grid.Data;

interface

uses
  System.Classes,
  Tee.Grid, Tee.Grid.Columns, Tee.Painter, Tee.Renders, Tee.Grid.Data,
  BI.Data, BI.DataSource, BI.Data.RTTI, System.Generics.Collections;

type
  TBIGridData=class(TVirtualData)
  private
    FCursor : TDataCursor;

    class function DataOf(const AColumn:TColumn):TDataItem; inline; static;

    function GetData: TDataItem;
    function RowOf(const ARow:Integer):Integer;
    procedure SetData(const Value: TDataItem);
  protected
    Master : TDataItem;
  public
    Detail : TDataItem;

    Constructor Create(const AProvider:TDataProvider=nil);
    Destructor Destroy; override;

    procedure AddColumns(const AColumns:TColumns); override;
    function AsFloat(const AColumn:TColumn; const ARow:Integer):TFloat; override;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;

    function CanExpand(const Sender:TRender; const ARow:Integer):Boolean; override;
    function CanSortBy(const AColumn:TColumn):Boolean; override;
    class function ColumnOf(const AGrid:TCustomTeeGrid; const AItem:TDataItem):TColumn; static;

    function Count:Integer; override;

    class function From(const ASource:TComponent):TVirtualData; override;
    class function From(const AData:TDataItem):TBIGridData; overload;

    class function From<T>(const Value:Array of T):TBIGridData; overload; static;
    class function From<T>(const Value:TList<T>):TBIGridData; overload; static;

    function GetDetail(const ARow:Integer; const AColumns:TColumns; out AParent:TColumn): TVirtualData; override;

    class function IsNumeric(const AColumn:TColumn):Boolean; override;
    function IsSorted(const AColumn:TColumn; out Ascending:Boolean):Boolean; override;

    procedure Load; override;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;
    procedure SortBy(const AColumn:TColumn); override;

    property Data:TDataItem read GetData write SetData;
  end;

implementation
