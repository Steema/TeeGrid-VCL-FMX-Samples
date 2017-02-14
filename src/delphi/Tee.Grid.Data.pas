{*********************************************}
{  TeeGrid Software Library                   }
{  Abstract TVirtualData class                }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Data;
{$I Tee.inc}

interface

{
  Base abstract TVirtualData class.

  Provides data to a TeeGrid (main rows or sub-rows)

  See concrete implementations at the following units:

  Tee.Grid.Data.Rtti     (for generic TArray<T> or TList<T> data)
  Tee.Grid.Data.DB       (for TDataSet and TDataSource)
  Tee.Grid.Data.Strings  (to emulate a TStringGrid with Cells[Col,Row] property)

  BI.Grid.Data           (for any TeeBI TDataItem data structure)

}

uses
  {System.}Classes, {System.}TypInfo,
  Tee.Grid.Columns, Tee.Painter, Tee.Renders;

type
  TFloat=Double;

  TDataEditingEvent=procedure(const Sender:TObject; const IsEditing:Boolean) of object;

  TRowChangedEvent=procedure(const Sender:TObject; const ARow:Integer) of object;

  TEditMode=(Start,Cancel,Finish);

  TColumnCalculation=(Count,Sum,Min,Max,Average);

  TVirtualData=class abstract
  protected
    FOnChangeRow : TRowChangedEvent;
    FOnEditing   : TDataEditingEvent;
    FOnRepaint,
    FOnRefresh   : TNotifyEvent;

    PictureClass : TPersistentClass;

    class function Add(const AColumns:TColumns; const AName:String; const ATag:TObject):TColumn; overload; static;

    procedure ChangeSelectedRow(const ARow:Integer);

    class procedure DoError(const AText:String); static;
    procedure EditingChanged(const IsEditing:Boolean); virtual;

    procedure Refresh;
    procedure RowChanged(const ARow:Integer); virtual;
    procedure Repaint;

    function SampleDate(const AColumn:TColumn): String;
    function SampleDateTime(const AColumn:TColumn):String;
    function SampleTime(const AColumn:TColumn):String;

    procedure SetField(const AColumn:TColumn; const ASource:TObject); virtual;
  public
    procedure AddColumns(const AColumns:TColumns); virtual; abstract;
    function AsFloat(const AColumn:TColumn; const ARow:Integer):TFloat; virtual;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; virtual; abstract;
    function AutoHeight(const APainter:TPainter; const AColumn:TColumn; const ARow:Integer; out AHeight:Single):Boolean; virtual;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; virtual; abstract;
    function Calculate(const AColumn:TColumn; const ACalculation:TColumnCalculation):TFloat;
    procedure EditMode(const AMode:TEditMode); virtual;
    function CanExpand(const Sender:TRender; const ARow:Integer):Boolean; virtual;
    function CanSortBy(const AColumn:TColumn):Boolean; virtual;
    function Count:Integer; virtual; abstract;
    function DataType(const AColumn:TColumn):PTypeInfo; virtual;
    class function From(const ASource:TComponent):TVirtualData; overload; virtual;
    function GetDetail(const ARow:Integer; const AColumns:TColumns; out AParent:TColumn):TVirtualData; virtual;
    function HasDetail(const ARow:Integer):Boolean; virtual;
    class function IsNumeric(const AColumn:TColumn):Boolean; overload; virtual;
    function IsSorted(const AColumn:TColumn; out Ascending:Boolean):Boolean; virtual;
    procedure Load(const AColumns:TColumns); virtual; abstract;
    function LongestString(const APainter:TPainter; const AColumn:TColumn):Single;
    function ReadOnly(const AColumn:TColumn):Boolean; virtual;
    procedure SetFirstRow(const ARow:Integer); virtual;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); virtual; abstract;
    procedure SortBy(const AColumn:TColumn); virtual;
    procedure ToggleBoolean(const AColumn:TColumn; const ARow:Integer);
  end;

  TVirtualDataClass=class of TVirtualData;

  // List of current registered VirtualData classes
  TVirtualDataClasses=record
  private
    class var
      Items : Array of TVirtualDataClass;

    class function IndexOf(const AClass:TVirtualDataClass):Integer; static;
  public
    class function Guess(const ASource:TComponent):TVirtualData; static;
    class procedure Register(const AClass:TVirtualDataClass); static;
    class procedure UnRegister(const AClass:TVirtualDataClass); static;
  end;

implementation
