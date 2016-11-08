{*********************************************}
{  TeeGrid Software Library                   }
{  Base abstract Grid class                   }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid;

interface

uses
  System.Classes, System.Types,

  {$IFNDEF FPC}
  System.UITypes,
  {$ENDIF}

  Tee.Painter, Tee.Grid.Columns, Tee.Format, Tee.Control, Tee.Grid.Base,
  Tee.Grid.Header, Tee.Renders;

type
  TCustomTeeGrid=class;

  TGridSelection=class(TVisibleFormatRender)
  private
    FRow: Integer;
    FColumn: TColumn;
    FFull: Boolean;
    FParentFont: Boolean;

    IGrid : TCustomTeeGrid;

    procedure SetColumn(const Value: TColumn);
    procedure SetRow(const Value: Integer);
    procedure SetFull(const Value: Boolean);
    procedure SetParentFont(const Value: Boolean);
  protected
    AlwaysPaintText,
    CheckScroll : Boolean;
  public
    Constructor CreateGrid(const AGrid:TCustomTeeGridBase); virtual;
    function Paint(const APainter:TPainter):Boolean;
  published
    property Column:TColumn read FColumn write SetColumn;
    property FullRow:Boolean read FFull write SetFull default False;
    property ParentFont:Boolean read FParentFont write SetParentFont default True;
    property Row:Integer read FRow write SetRow default -1;
  end;

  TCellHover=class(TGridSelection)
  public
    Constructor CreateGrid(const AGrid:TCustomTeeGridBase); override;

    procedure TryPaint(const APainter:TPainter);
  end;

  TGridCells=class(TTextRender)
  private
    FHover: TCellHover;

    procedure SetHover(const Value: TCellHover);
  public
    Constructor CreateGrid(const AGrid:TCustomTeeGridBase);
    Destructor Destroy; override;
  published
    property Hover:TCellHover read FHover write SetHover;
  end;

  TCustomTeeGrid=class(TCustomTeeGridBase)
  private
    FCells: TGridCells;
    FHeader: THeader;
    FIndicator: TIndicator;
    FLines: TGridLines;
    FOnAfterDraw: TNotifyEvent;
    FOnClickHeader : TNotifyEvent;
    FOnColumnResized: TNotifyEvent;
    FReadOnly: Boolean;
    FRows : TRows;
    FSelected: TGridSelection;

    procedure CalcRowsHeight;

    procedure CheckHorizScroll(const AColumn:TColumn);
    procedure CheckVertScroll(const ARow:Integer);

    procedure ClickedHeader(Sender:TObject);

    function ColumnsStartX:Single;

    function DraggedColumn(const X:Single; const AColumn:TColumn):TColumn;
    function FontOf(const AColumn:TColumn):TFont;
    function InColumnResize(const X:Single; const AColumn:TColumn):Boolean;
    function IndicatorBounds(const ARow:Integer):TRectF;
    function IndicatorXOffset:Single;
    function InHeader(const Y:Single):Boolean;

    procedure SetCells(const Value: TGridCells);
    procedure SetIndicator(const Value: TIndicator);
    procedure SetHeader(const Value: THeader);
    procedure SetLines(const Value: TGridLines);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetRows(const Value: TRows);
    procedure SetSelected(const Value: TGridSelection);

    function WidthPixels(const AWidth:TColumnWidth):Single;
  protected
    Editing : Boolean;

    procedure ChangeHorizScroll(const Value:Single);
    procedure ChangeVertScroll(const Value:Single);
    procedure CheckColumnsWidth(const Forced:Boolean);

    procedure DataChanged; virtual;

    procedure KeyDown(const AKey:Word; Shift: TShiftState);

    function HorizScrollBarHeight:Single; virtual; abstract;
    procedure HorizScrollChanged; virtual; abstract;

    procedure MouseDown(const LeftButton:Boolean; const Shift:TShiftState; const X,Y:Single);
    procedure MouseMove(const Shift:TShiftState; const X,Y:Single);
    procedure MouseUp(const LeftButton:Boolean; const Shift:TShiftState; const X,Y:Single);
    procedure StartEditor(const AColumn:TColumn; const ARow:Integer); virtual; abstract;
    procedure StopEditor; virtual; abstract;

    function VertScrollBarWidth:Single; virtual; abstract;
    procedure VertScrollChanged; virtual; abstract;
  public
    MouseCursor : TMouseCursor;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    function AutoWidth(const AColumn:TColumn):Single;
    function CalcAutoWidth(const AColumn:TColumn):Single;
    function ColumnAt(const X:Single):TColumn;

    function CellRect(const ATopLeft:TPointF; const AColumn:TColumn; const ARow:Integer):TRectF; overload;
    function CellRect(const AColumn: TColumn; const ARow: Integer): TRectF; overload; inline;

    function ColumnLeft(const AColumn:TColumn):Single;
    function ColumnTop(const ARow: Integer):Single;

    function MaxBottom:Single;
    function MaxRight:Single; override;

    function PositionOf(const AColumn:TColumn; const ARow:Integer):TPointF;
    function RowAt(const Y:Single):Integer;

    procedure Paint; override;
    procedure RefreshData;

    function WidthOf(const AColumns:TColumns):Single;

    property Rows:TRows read FRows write SetRows;
  published
    property Cells:TGridCells read FCells write SetCells;

    property Header:THeader read FHeader write SetHeader;
    property Indicator:TIndicator read FIndicator write SetIndicator;
    property Lines:TGridLines read FLines write SetLines;
    property ReadOnly:Boolean read FReadOnly write SetReadOnly default False;
    property Selected:TGridSelection read FSelected write SetSelected;

    property OnAfterDraw:TNotifyEvent read FOnAfterDraw write FOnAfterDraw;
    property OnClickHeader:TNotifyEvent read FOnClickHeader write FOnClickHeader;
    property OnColumnResized:TNotifyEvent read FOnColumnResized write FOnColumnResized;
  end;

implementation
