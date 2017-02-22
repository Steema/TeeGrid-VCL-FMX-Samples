{*********************************************}
{  TeeGrid Software Library                   }
{  TRows class                                }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Rows;
{$I Tee.inc}

interface

{
   This unit implements a TRows class, derived from TGridBand.

   TRows class paints multiple rows of column cells.

   It also includes a "Children[]" property, with optional sub-rows for
   each row.

   This enables implementing hierarchical "rows-inside-rows".
}

uses
  {System.}Classes,

  {$IFNDEF FPC}
  {System.}Types,
  {$ENDIF}

  {System.}SysUtils,

  Tee.Format, Tee.Painter, Tee.Renders,
  Tee.GridData, Tee.Grid.Columns, Tee.Grid.Bands,
  Tee.Grid.Selection;

type
  // Formatting properties to paint odd row cells
  TAlternateFormat=class(TVisibleFormat)
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    function IsVisible:Boolean;
  published
    property Visible default False;
  end;

  TCellHover=class(TGridSelection)
  private
    procedure InitFormat;
  end;

  TRowsBands=class(TGridBands)
  private
    procedure DeleteRow(const AItem: TCollectionItem);
    function GetRow(const Index:Integer):TGridBand;
    procedure SetRow(const Index:Integer; const ABand:TGridBand);
  protected
    Rows : Array of TGridBand;

    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    function Height(const ARow: Integer): Single;
    function Hit(const X,Y:Single):TGridBand;
    function RowOf(const ABand:TGridBand):Integer;
    procedure Swap(const A,B:Integer);

    property Row[const Index:Integer]:TGridBand read GetRow write SetRow;
  end;

  // Grid Band to paint multiple rows of cells
  TRows=class(TGridBandLines)
  private
    FAlternate: TAlternateFormat;
    FBack : TFormat;
    FHeight: TCoordinate;
    FHover: TCellHover;
    FRowLines: TStroke;
    FSpacing: TCoordinate;
    FVisibleColumns : TVisibleColumns;

    IData : TVirtualData;

    IHeights : Array of Single;
    ISpacing : Single;

    FChildren : TRowsBands;
    FSubBands : TRowsBands;

    function CalcColumnHeight(const AColumn:TColumn; const AText:String): Single;
    procedure ChangedHover(Sender: TObject);
    function GetHeights(const Index: Integer): Single;
    procedure SetAlternate(const Value: TAlternateFormat);
    procedure SetBack(const Value: TFormat);
    procedure SetHeight(const Value: TCoordinate);
    procedure SetHeights(const Index: Integer; const Value: Single);
    procedure SetHover(const Value: TCellHover);
    procedure SetSpacing(const Value: TCoordinate);
    procedure SetRowLines(const Value: TStroke);
  protected
    FOnHorizScroll,
    FOnVertScroll : TNotifyEvent;

    function ChangeHorizScroll(const Value:Single):Boolean;
    function ChangeVertScroll(const Value:Single):Boolean;
    function MaxRight:Single;
    procedure PaintRow(var AData:TRenderData; const ARender: TRender);
  public
    Painter : TPainter;

    // Temporary:
    XOffset : Single;
    Scroll : TPointF;

    Constructor Create(ACollection:TCollection); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    function AllHeightsEqual:Boolean;
    function AutoHeight(const ARow: Integer): Single; overload;
    function BottomOf(const ARow:Integer):Single;
    procedure CalcDefaultHeight;
    procedure CalcYSpacing(const AHeight:Single);
    procedure Clear;
    function Empty:Boolean;
    function FirstVisible(const AOffset:Single=0):Integer;
    function FontOf(const AColumn:TColumn):TFont;
    function HeightOf(const ARow:Integer):Single;
    function IsChildrenVisible(const Sender:TRender; const ARow: Integer): Boolean;
    function MaxBottom: Single;
    procedure Paint(var AData:TRenderData; const ARender:TRender); override;
    procedure PaintLines(const AData:TRenderData; const FirstRow:Integer; Y:Single);
    function RowAt(const Y, AvailableHeight: Single): Integer;
    function SetColumnsLeft(const ALeft,ARight,ASpacing:Single):TRectF;
    procedure Swap(const A,B:Integer);
    function TopOf(const ARow:Integer):Single;
    function TopOfRow(const ARow:Integer):Single;
    function TotalHeight(const ARow:Integer):Single;
    function UpToRowHeight(const ARow:Integer):Single;

    property Children:TRowsBands read FChildren;
    property Data:TVirtualData read IData write IData;
    property Heights[const Index:Integer]:Single read GetHeights write SetHeights;
    property SubBands:TRowsBands read FSubBands;
    property VisibleColumns:TVisibleColumns read FVisibleColumns;
    property YSpacing:Single read ISpacing;
  published
    property Alternate:TAlternateFormat read FAlternate write SetAlternate;
    property Back:TFormat read FBack write SetBack;
    property Height:TCoordinate read FHeight write SetHeight;
    property Hover:TCellHover read FHover write SetHover;
    property RowLines:TStroke read FRowLines write SetRowLines;
    property Spacing:TCoordinate read FSpacing write SetSpacing;
  end;

implementation
