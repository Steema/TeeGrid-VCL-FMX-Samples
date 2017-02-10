{*********************************************}
{  TeeGrid Software Library                   }
{  TStringGrid emulation data class           }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Data.Strings;
{$I Tee.inc}

// Force range-checking in Debug mode
{$IFOPT D+}
{$R+}
{$ENDIF}

interface

{
 Two classes in this unit:

 1) "Virtual Mode" data

  A virtual data class for custom "Column x Row" grid using events.

  Usage:

    var Data : TVirtualModeData;

    // Columns, Rows and optional default column width (much faster)
    Data:= TVirtualModeData.Create(10,1000,60);

    // Events:
    Data.OnGetValue:=GetCell;
    Data.OnSetValue:=SetCell;

    TeeGrid1.Data:= Data;

    procedure TMyForm.GetCell(Sender:TObject; const AColumn:TColumn; const ARow:Integer; var AValue:String);
    procedure TMyForm.SetCell(Sender:TObject; const AColumn:TColumn; const ARow:Integer; var AValue:String);


 2) "TStringGrid" emulator with TStringsData class

  Derived from TVirtualModeData, maintains an internal array of strings, one for
  each cell in the grid.

  Usage:

    var Data : TStringsData;
    Data:= TStringsData.Create; // optional params: Create(5,4)

    // Set size
    Data.Columns:= 5;
    Data.Rows:= 4;

    // Set data to grid
    TeeGrid1.Data:= Data;

    // Set header texts
    Data.Headers[1]:='Company';

    // Set cell values
    Data[1,1]:= 'Steema';
}

uses
  {System.}Classes,

  Tee.Grid.Columns, Tee.Grid.Data, Tee.Painter;

type
  TOnVirtualData=procedure(Sender:TObject;
                           const AColumn:TColumn;
                           const ARow:Integer;
                           var AValue:String {Variant}) of object;

  TStringArray=Array of String;

  { TVirtualModeData }

  TVirtualModeData=class(TVirtualData)
  private
    FColumns: Integer;
    FRows: Integer;

    IDefaultWidth : Single;
    IColumns : TColumns;
    IHeaders : TStringArray;

    FOnSetValue,
    FOnGetValue: TOnVirtualData;

    function GetHeader(const Column: Integer): String;
    procedure SetColumns(const Value: Integer);
    procedure SetHeader(const Column: Integer; const Value: String);
    procedure SetRows(const Value: Integer);

    {$IFOPT R+}
    procedure RangeCheck(const Column,Row: Integer);
    {$ENDIF}
  protected
    procedure InternalResize; virtual;
  public
    Constructor Create(const AColumns:Integer=0; const ARows:Integer=0; const DefaultWidth:Single=0); virtual;

    procedure AddColumns(const AColumns:TColumns); override;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;
    function Count:Integer; override;
    function IndexOf(const AColumn: TColumn):Integer; inline;
    procedure Load(const AColumns:TColumns); override;
    procedure Resize(const AColumns,ARows:Integer);
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;

    // properties
    property ColumnList:TColumns read IColumns;
    property Headers[const Column:Integer]:String read GetHeader write SetHeader;

    // published
    property Columns:Integer read FColumns write SetColumns default 0;
    property Rows:Integer read FRows write SetRows default 0;

    property OnGetValue:TOnVirtualData read FOnGetValue write FOnGetValue;
    property OnSetValue:TOnVirtualData read FOnSetValue write FOnSetValue;
  end;

  { TStringsData }

  TStringArrays=Array of TStringArray;

  TStringsData=class(TVirtualModeData)
  protected
    IData : TStringArrays;

    function GetCell(const AColumn,ARow: Integer): String; virtual;
    procedure InternalResize; override;
    procedure SetCell(const AColumn,ARow: Integer; const Value: String); virtual;
  public
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;

    property Cells[const Column,Row:Integer]:String read GetCell write SetCell; default;
  end;

implementation
