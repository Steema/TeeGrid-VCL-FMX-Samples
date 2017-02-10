{*********************************************}
{  TeeGrid Software Library                   }
{  DB TDataSet Virtual Data                   }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Data.DB;
{$I Tee.inc}

interface

{
  Virtual Data class to link TDataSet or TDataSource fields to a TeeGrid

  Note:

  Setting TeeGrid DataSource property to a TDataSet or TDataSource component
  will automatically create a TVirtualDBData object.

    TeeGrid1.DataSource := FDMemTable1;

    TeeGrid1.DataSource := DataSource2;


  Manual Usage examples:

    uses Tee.Grid.Data.DB;

      TeeGrid1.Data:= TVirtualDBData.From(DataSource1);

      TeeGrid1.Data:= TVirtualDBData.From(MyDataSet1);

}

uses
  {System.}Classes, {System.}TypInfo, {Data.}DB,

  Tee.Grid.Columns, Tee.Grid.Data, Tee.Painter;

type
  TVirtualDBData=class;

  TVirtualDataLink=class(TDataLink)
  private
    IData : TVirtualDBData;

    IChanging : Boolean;

    procedure ChangeRow(const ARow:Integer);
    procedure TrySetBufferCount;
  protected
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  end;

  TVirtualDBData=class(TVirtualData)
  private
    function BeginRow(const ARow:Integer):Integer;
    procedure EndRow(const ARow:Integer);
  protected
    IDataSet : TDataSet;
    IDataSource : TDataSource;

    ILink : TVirtualDataLink;

    OwnsDataSource : Boolean;

    class function Add(const AColumns:TColumns; const AField:TField):TColumn;
    procedure CreateLink;
    function FieldOf(const AColumn:TColumn):TField; inline;
    class function HorizAlignOf(const AField:TField):THorizontalAlign; static;
    procedure RowChanged(const ARow:Integer); override;
  public
    OwnsData : Boolean;

    Destructor Destroy; override;

    procedure AddColumns(const AColumns:TColumns); override;
    function AsFloat(const AColumn:TColumn; const ARow:Integer):TFloat; override;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;
    procedure EditMode(const AMode:TEditMode); override;
    function Count:Integer; override;

    class function From(const ASource:TComponent):TVirtualData; override;

    procedure Load(const AColumns:TColumns); override;
    function ReadOnly(const AColumn:TColumn):Boolean; override;

    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;

    function DataType(const AColumn:TColumn):PTypeInfo; override;
  end;

implementation
