{*********************************************}
{  TeeGrid Software Library                   }
{  DB TDataSet Virtual Data                   }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Data.DB;
{$I Tee.inc}

interface

uses
  {Data.}DB, Tee.Grid.Columns, Tee.Grid.Data, Tee.Painter;

type
  TVirtualDBData=class;

  TVirtualDataLink=class(TDataLink)
  private
    IData : TVirtualDBData;

    IChanging : Boolean;

    procedure ChangeRow(const ARow:Integer);
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  end;

  TVirtualDBData=class(TVirtualData)
  protected
    IDataSet : TDataSet;
    IDataSource : TDataSource;

    ILink : TVirtualDataLink;

    OwnsDataSource : Boolean;

    class function Add(const AColumns:TColumns; const AField:TField):TColumn;
    procedure CreateLink;
    function FieldOf(const AColumn:TColumn):TField; inline;
    procedure RowChanged(const ARow:Integer); override;
  public
    Destructor Destroy; override;

    procedure AddColumns(const AColumns:TColumns); override;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;
    function Count:Integer; override;

    class function From(const ADataSet:TDataSet):TVirtualDBData; overload; static;
    class function From(const ADataSource:TDataSource):TVirtualDBData; overload; static;

    procedure Load; override;
    function ReadOnly(const AColumn:TColumn):Boolean; override;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;
  end;

implementation
