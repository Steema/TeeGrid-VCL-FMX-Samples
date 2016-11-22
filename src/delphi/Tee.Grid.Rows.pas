{*********************************************}
{  TeeGrid Software Library                   }
{  TRows class                                }
{  Copyright (c) 2016 by Steema Software      }
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
  Tee.Grid.Data, Tee.Grid.Columns, Tee.Grid.Bands,
  Tee.Grid.Selection;

type
  // Formatting properties to paint odd row cells
  TAlternateFormat=class(TVisibleFormat)
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    function ShouldPaint:Boolean;
  published
    property Visible default False;
  end;

  TCellHover=class(TGridSelection)
  private
    procedure InitFormat;
  end;

  TRowsBands=record
  private
    FItems : Array of TGridBand;

    procedure FreeAll;
    function Get(const AIndex:Integer):TGridBand;
    procedure Put(const AIndex:Integer; const AValue:TGridBand);
  public
    function Count:Integer; inline;
    function Height(const ARow: Integer): Single;
    function Hit(const X,Y:Single):TGridBand;
    property Items[const Index:Integer]:TGridBand read Get write Put; default;
  end;

  // Grid Band to paint multiple rows of cells
  TRows=class(TGridBandLines)
  private
    FAlternate: TAlternateFormat;
    FHeight: Single;
    FHover: TCellHover;
    FRowLines: TStroke;
    FSpacing: TCoordinate;

    IData : TVirtualData;

    ICustomHeights,
    IHeights : Array of Single;

    ISpacing : Single;

    FChildren,
    FSubBands : TRowsBands;

    function AllHeightsEqual:Boolean;
    function GetHeights(const Index: Integer): Single;
    function IsHeightStored: Boolean;
    procedure SetAlternate(const Value: TAlternateFormat);
    procedure SetHeight(const Value: Single);
    procedure SetHeights(const Index: Integer; const Value: Single);
    procedure SetHover(const Value: TCellHover);
    procedure SetSpacing(const Value: TCoordinate);
    procedure SetRowLines(const Value: TStroke);
    function SubBandHeight(const ARow:Integer):Single;
  protected
    procedure PaintRow(var AData:TRenderData; const ARender: TRender);
  public
    DefaultHeight : Single;
    VisibleColumns : TVisibleColumns;

    Render : TTextRender;

    // Temporary:
    XOffset : Single;
    XSpacing : Single;
    Scroll : TPointF;

    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure CalcYSpacing(const AHeight:Single);
    procedure Clear;
    function Count:Integer;
    function DraggedColumn(const X:Single; const AColumn:TColumn):TColumn;
    function FirstVisible:Integer;
    function HeightOf(const ARow:Integer):Single;
    function IsChildrenVisible(const Sender:TRender; const ARow: Integer): Boolean;
    function MaxBottom: Single;
    procedure Paint(var AData:TRenderData); override;
    procedure PaintLines(const AData:TRenderData; const FirstRow:Integer; Y:Single);
    function RowAt(const Y, AvailableHeight: Single): Integer;
    procedure SetColumnsLeft(const ALeft:Single);
    function TopOf(const ARow:Integer):Single;
    function TopOfRow(const ARow:Integer):Single;
    function TotalHeight(const ARow:Integer):Single;
    function UpToRowHeight(const ARow:Integer):Single;

    property Children:TRowsBands read FChildren;
    property Data:TVirtualData read IData write IData;
    property Heights[const Index:Integer]:Single read GetHeights write SetHeights;
    property SubBands:TRowsBands read FSubBands;
    property YSpacing:Single read ISpacing;
  published
    property Alternate:TAlternateFormat read FAlternate write SetAlternate;
    property Height:Single read FHeight write SetHeight stored IsHeightStored;
    property Hover:TCellHover read FHover write SetHover;
    property RowLines:TStroke read FRowLines write SetRowLines;
    property Spacing:TCoordinate read FSpacing write SetSpacing;
  end;

implementation
