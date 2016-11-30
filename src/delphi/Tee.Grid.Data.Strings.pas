{*********************************************}
{  TeeGrid Software Library                   }
{  TStringGrid emulation data class           }
{  Copyright (c) 2016 by Steema Software      }
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
  "TStringGrid" emulator.

  A virtual data class for custom "Column x Row" grid of Cells[col,row].

  Usage:

    var Data : TStringsData;
    Data:= TStringsData.Create;

    // Initial size

    Data.Columns:= 5;
    Data.Rows:= 4;

    // Set data to grid

    TeeGrid1.Data:= Data;

    // Set header texts

    Data.Headers[1]:='Company';

    // Set cell values

    Data[1,1]:= 'Steema';

    // Optional events
    Data.OnGetValue:=...
    Data.OnSetValue:=...
}

uses
  {System.}Classes,
  Tee.Grid.Columns, Tee.Grid.Data, Tee.Painter;

type
  TOnGetValue=procedure(Sender:TObject; const AColumn,ARow:Integer; var AValue:String) of object;
  TOnSetValue=procedure(Sender:TObject; const AColumn,ARow:Integer; const AValue:String) of object;

  { TStringsData }

  TStringArray=Array of String;

  TStringsData=class(TVirtualData)
  private
    FColumns: Integer;
    FRows: Integer;

    IColumns : TColumns;
    IHeaders : TStringArray;
    IData : Array of TStringArray;

    FOnSetValue: TOnSetValue;
    FOnGetValue: TOnGetValue;

    {$IFOPT R+}
    procedure RangeCheck(const Column,Row: Integer);
    {$ENDIF}

    function GetCell(const Column,Row: Integer): String;
    function GetHeader(const Column: Integer): String;
    function IndexOf(const AColumn: TColumn):Integer; inline;
    procedure SetCell(const Column,Row: Integer; const Value: String);
    procedure SetColumns(const Value: Integer);
    procedure SetHeader(const Column: Integer; const Value: String);
    procedure SetRows(const Value: Integer);
  public
    Constructor Create(const AColumns:Integer=0; const ARows:Integer=0);

    procedure AddColumns(const AColumns:TColumns); override;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;
    function Count:Integer; override;
    procedure Load; override;
    procedure Resize(const AColumns,ARows:Integer);
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;

    property Cells[const Column,Row:Integer]:String read GetCell write SetCell; default;
    property ColumnList:TColumns read IColumns;
    property Headers[const Column:Integer]:String read GetHeader write SetHeader;

  //published
    property Columns:Integer read FColumns write SetColumns default 0;
    property Rows:Integer read FRows write SetRows default 0;

    property OnGetValue:TOnGetValue read FOnGetValue write FOnGetValue;
    property OnSetValue:TOnSetValue read FOnSetValue write FOnSetValue;
  end;

implementation
