{*********************************************}
{  TeeGrid Software Library                   }
{  TRowGroup class                            }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.RowGroup;
{$I Tee.inc}

interface

{
   This unit implements a TRowGroup class derived from TGridBand.

   TRowGroup class paints Rows, Headers and Footers:

     Rows is a TGridBand with a Data property (TVirtualData).

     Headers and Footers are TGridBands (collection of TGridBand).

     A TGridBand is an abstract class, any kind of band is supported.

}

uses
  {System.}Classes,

  {$IFNDEF FPC}
  {System.}Types,
  {$ENDIF}

  {System.}SysUtils,

  Tee.Format, Tee.Painter, Tee.Renders,
  Tee.Grid.Data, Tee.Grid.Columns, Tee.Grid.Bands, Tee.Grid.Header,
  Tee.Grid.Selection, Tee.Grid.Rows;

type
  TRowGroup=class;

  TNewDetailEvent=procedure(const Sender,NewGroup:TRowGroup) of object;

  // Grid band to paint headers, rows and footers
  TRowGroup=class(TGridBand)
  private
    FCells: TTextRender;
    FFooter: TGridBands;
    FHeader: TColumnHeaderBand;  // Collection
    FIndicator: TIndicator;
    FOnChangedSelected : TNotifyEvent;
    FOnNewDetail : TNewDetailEvent;
    FReadOnly: Boolean;
    FRows : TRows;
    FSelected: TGridSelection;

    OwnsData : Boolean;
    FCurrent: TRowGroup;

    function CalcAutoWidth(const APainter:TPainter; const AColumn:TColumn;
                           const AWidth:Single):Single;
    procedure ChangedCellFormat(Sender: TObject);
    procedure ChangedFooter(Sender:TObject);
    procedure DoCalcWidth(const APainter:TPainter; const AColumn: TColumn;
                          const AWidth:Single);
    procedure DoHeaderFooter(var AState:TMouseState; const AWidth,AHeight:Single);
    procedure DoMove(var AState:TMouseState; const AWidth,AHeight:Single);
    procedure DoSelectedChanged(Sender:TObject);
    function GetColumns: TColumns;
    function GetData: TVirtualData;
    function GetFooter: TGridBands;
    procedure PaintRest(var AData: TRenderData);
    function PositionOf(const AColumn:TColumn; const ARow:Integer):TPointF;
    procedure RemovedColumn(Sender:TObject; const AColumn:TColumn);
    procedure SetCells(const Value: TTextRender);
    procedure SetCurrent(const Value: TRowGroup);
    procedure SetFooter(const Value: TGridBands);
    procedure SetHeader(const Value: TColumnHeaderBand);
    procedure SetIndicator(const Value: TIndicator);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetRows(const Value: TRows);
    procedure SetSelected(const Value: TGridSelection);
    procedure TryMouseChildren(var AState:TMouseState; const AWidth,AHeight:Single);
    function WidthOf(const APainter:TPainter; const AColumns: TColumns;
                     const AWidth:Single): Single;
  public
    ParentColumn : TColumn;
    IsFocused,
    RecalcScrollBars : Boolean;

    Constructor Create(const AChanged:TNotifyEvent; const AData:TVirtualData); reintroduce;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure CalcHeight(const ATotal:Single); override;
    procedure CalcRowsHeight;
    function CanEditRender(const AColumn:TColumn):Boolean;
    function CanStartEditor:Boolean;

    function CellRect(const ATopLeft:TPointF; const AColumn:TColumn; const ARow:Integer):TRectF; overload;
    function CellRect(const AColumn: TColumn; const ARow: Integer): TRectF; overload; inline;

    procedure CheckColumnsWidth(const APainter:TPainter; const Forced:Boolean;
                                const AWidth:Single);
    function FontOf(const AColumn:TColumn):TFont;

    procedure Key(const AState:TKeyState);

    function MaxBottom:Single;

    function Mouse(var AState:TMouseState; const AWidth,AHeight:Single): Boolean; override;

    procedure Paint(var AData:TRenderData); override;
    procedure PrepareColumns(const APainter:TPainter; const ALeft,AWidth:Single);

    procedure RefreshData(const AData:TVirtualData);
    function RenderHit(const AColumn:TColumn; const ARow:Integer; const X,Y:Single):Boolean;
    procedure ToggleDetailRows(const ARow:Integer);

    property Columns:TColumns read GetColumns;
    property Current:TRowGroup read FCurrent write SetCurrent;
    property Data:TVirtualData read GetData;
  published
    property Cells:TTextRender read FCells write SetCells;
    property Footer:TGridBands read GetFooter write SetFooter;
    property Header:TColumnHeaderBand read FHeader write SetHeader;
    property Indicator:TIndicator read FIndicator write SetIndicator;
    property ReadOnly:Boolean read FReadOnly write SetReadOnly default False;
    property Rows:TRows read FRows write SetRows;
    property Selected:TGridSelection read FSelected write SetSelected;

    property OnNewDetail:TNewDetailEvent read FOnNewDetail write FOnNewDetail;
    property OnChangedSelected:TNotifyEvent read FOnChangedSelected write FOnChangedSelected;
  end;

implementation
