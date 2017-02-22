{*********************************************}
{  TeeGrid Software Library                   }
{  DB TDataSet Virtual Data                   }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.GridData.DB;
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

    uses Tee.GridData.DB;

      TeeGrid1.Data:= TVirtualDBData.From(DataSource1);

      TeeGrid1.Data:= TVirtualDBData.From(MyDataSet1);

}

uses
  {System.}Classes, {System.}TypInfo, {Data.}DB,

  Tee.Grid.Columns, Tee.GridData, Tee.Painter;

type
  TVirtualDBData=class;

  // Internal custom TDataLink class, to link a TVirtualDBData with a TDataSet
  TVirtualDataLink=class(TDataLink)
  private
    IData : TVirtualDBData;

    IChanging : Boolean;
    IFirstRow : Integer;

    procedure ChangeRow(const ARow:Integer);
    procedure TrySetBufferCount;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  end;

  TVirtualFetchMode=(Automatic,All,Partial);

  // TeeGrid data class to link with a TDataSet or TDataSource component
  TVirtualDBData=class(TVirtualData)
  private
    FFetchMode : TVirtualFetchMode;

    IBuffered : Boolean;

    function ActiveDataSet:Boolean; inline;
    class procedure AddFields(const AColumns: TColumns; const AFields: TFields); static;
    function BeginRow(const ARow:Integer):Integer;
    function DataSetRecNo:Integer;
    procedure EndRow(const ARow:Integer);
    procedure FetchAllRecords;
    function HasFields:Boolean;
    procedure SetFetchMode(const Value:TVirtualFetchMode);
  protected
    IDataSet : TDataSet;
    IDataSource : TDataSource;

    ILink : TVirtualDataLink;

    OwnsDataSource : Boolean;

    class function Add(const AColumns:TColumns; const AField:TField):TColumn;
    procedure CreateLink;
    function Empty:Boolean; override;
    function EOF(const ARow:Integer):Boolean; override;
    function FieldOf(const AColumn:TColumn):TField; inline;
    class function HorizAlignOf(const AField:TField):THorizontalAlign; static;
    function KnownCount: Boolean; override;
    procedure Refresh; override;
    procedure RowChanged(const ARow:Integer); override;
    procedure SetField(const AColumn:TColumn; const ASource:TObject); override;
    procedure SetFirstRow(const ARow:Integer); override;
  public
    OwnsData : Boolean;

    Destructor Destroy; override;

    procedure AddColumns(const AColumns:TColumns); override;
    function AsFloat(const AColumn:TColumn; const ARow:Integer):TFloat; override;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;
    procedure EditMode(const AMode:TEditMode); override;
    function Count:Integer; override;
    function DataType(const AColumn:TColumn):PTypeInfo; override;

    class function From(const ASource:TComponent):TVirtualData; override;
    class procedure LinkTo(const AColumn:TColumn; const AField:TField); static;

    procedure Load(const AColumns:TColumns); override;
    function ReadOnly(const AColumn:TColumn):Boolean; override;

    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;

    property DataSet:TDataSet read IDataSet;

  // published
    property FetchMode:TVirtualFetchMode read FFetchMode write SetFetchMode default TVirtualFetchMode.Automatic;
  end;

implementation
