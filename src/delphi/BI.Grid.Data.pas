{*********************************************}
{  TeeGrid Software Library                   }
{  TeeBI Virtual Data class                   }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Grid.Data;

interface

uses
  Tee.Grid, Tee.Grid.Columns, Tee.Painter, Tee.Grid.Data,
  BI.Data, BI.Data.RTTI, System.Generics.Collections;

type
  TBIGridData=class(TVirtualData)
  private
    class function DataOf(const AColumn:TColumn):TDataItem; inline; static;
  public
    Data : TDataItem;

    Constructor Create(const AProvider:TDataProvider=nil);

    procedure AddColumns(const AColumns:TColumns); override;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;

    class function ColumnOf(const AGrid:TCustomTeeGrid; const AItem:TDataItem):TColumn; static;

    function Count:Integer; override;

    class function FromArray<T>(const Value:Array of T):TBIGridData; static;
    class function FromList<T>(const Value:TList<T>):TBIGridData; static;

    procedure Load; override;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;
  end;

implementation
