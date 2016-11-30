{*********************************************}
{  TeeGrid Software Library                   }
{  Ticker Component                           }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Ticker;
{$I Tee.inc}

interface

{
  Helper class to implement a "ticker", automatic repaint of grid cells
  with changing background cell color when a cell value is changed.

  Usage:

  1) Create a Ticker

  var Ticker : TGridTicker;

    Ticker:=TGridTicker.Create(TeeGrid1.Grid.Current);

  2) After a cell value is changed, update ticker:

    Ticker.Change(Col,Row,OldValue);


  Options:

    Enabled or paused:

      Ticker.Enabled:= True;

    Colors:

      Ticker.Higher.Color:= TColors.Green;
      Ticker.Lower.Color:= TColors.Red;

    Fade effect:

      Ticker.FadeColors:= True;

    Delay:

      Ticker.Delay:= 2000; // milliseconds

    Speed for internal thread:

      Ticker.RefreshSpeed:= 30;  // milliseconds

}

uses
  {System.}Classes,

  {$IFNDEF NOUITYPES}
  System.UITypes,
  {$ENDIF}

  Tee.Grid.Data, Tee.Grid.Columns, Tee.Grid.RowGroup, Tee.Renders, Tee.Format;

type
  TTickerCell=record
  private
    function Color(const OldColor,NewColor:TColor):TColor;
    procedure Init(const AOld,ANew,ASpeed:Double);
    function Update(const NewValue:Double):Boolean;
  public
    Enabled : Boolean;
    Delta,
    Range,
    Old,
    Value : Double;
  end;

  TGridTicker=class;

  TTickerThread=class(TThread)
  private
    Ticker : TGridTicker;
  protected
    procedure Execute; override;
  end;

  TGridTicker=class(TPersistentChange)
  private
    FDelay : Integer;
    FEnabled : Boolean;
    FFadeColors : Boolean;
    FGroup : TRowGroup;
    FHigherBrush : TBrush;
    FLowerBrush : TBrush;
    FRefreshSpeed : Integer;

    FValues : Array of Array of TTickerCell;

    ICount : Integer;
    IThread : TTickerThread;

    procedure CheckSize(const ACol, ARow: Integer);

    procedure ColumnPaint(const Sender:TColumn; var AData:TRenderData;
                          var DefaultPaint:Boolean);

    procedure ColumnPaintEvent;
    procedure DoChanged(Sender:TObject);
    procedure ProcessThread;
    procedure SetHigherBrush(const Value: TBrush);
    procedure SetLowerBrush(const Value: TBrush);
    function ValueAt(const AColumn:TColumn; const ARow:Integer):Double;
  public
    Constructor Create(const AGroup:TRowGroup); reintroduce;
    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Change(const ACol,ARow:Integer; const AOldValue:Double);
    procedure Paint(const AColumn:TColumn; var AData:TRenderData);
    procedure Resize;
  published
    property Delay:Integer read FDelay write FDelay default 1000;
    property Enabled:Boolean read FEnabled write FEnabled default True;
    property FadeColors:Boolean read FFadeColors write FFadeColors default True;
    property Higher:TBrush read FHigherBrush write SetHigherBrush;
    property Lower:TBrush read FLowerBrush write SetLowerBrush;
    property RefreshSpeed:Integer read FRefreshSpeed write FRefreshSpeed default 10;
  end;

implementation
