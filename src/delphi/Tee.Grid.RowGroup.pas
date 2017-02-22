{*********************************************}
{  TeeGrid Software Library                   }
{  TRowGroup class                            }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.RowGroup;
{$I Tee.inc}

interface

{
   This unit implements a TRowGroup class derived from TGridBand.

   A TRowGroup class paints:

     Rows, and optional Headers and Footers.

     "Rows" is a TGridBand with a Data property (TVirtualData).

     Headers and Footers are TGridBands (collection of TGridBand items).

     A TGridBand is an abstract class, any kind of band is supported.

}

uses
  {System.}Classes,

  {$IFNDEF FPC}
  {System.}Types,
  {$ENDIF}

  {System.}SysUtils,

  Tee.Format, Tee.Painter, Tee.Renders,
  Tee.GridData, Tee.Grid.Columns, Tee.Grid.Bands, Tee.Grid.Header,
  Tee.Grid.Selection, Tee.Grid.Rows;

type
  TRowGroup=class;

  // Type of event used to notify when a sub-grid (master-detail) is created
  TNewDetailEvent=procedure(const Sender,NewGroup:TRowGroup) of object;

  // Enable using finger-touch (pan) or left-button mouse drag to scroll
  TScrollingMode=(Touch,Mouse,Both,None);

  // Control scroll direction
  TScrollDirection=(Normal,Inverted,Disabled);

  // Properties to control scrolling of grid rows and columns,
  // when using the mouse or finger-touch
  TGridScrolling=class(TPersistentChange)
  private
    FHorizontal : TScrollDirection;
    FMode : TScrollingMode;
    FVertical : TScrollDirection;

    function Calculate(const AState:TMouseState; out P:TPointF):Boolean;
    function MouseEnabled(const AButton:TGridMouseButton):Boolean;
    procedure Reset(const AState: TMouseState; const AScroll:TPointF);
    procedure SetHorizontal(const Value:TScrollDirection);
    procedure SetVertical(const Value:TScrollDirection);
    procedure Stop;
    procedure TryStart(const AState:TMouseState; const AScroll:TPointF);
  protected
    Origin : TPointF;
    Scroll : TPointF;
    Scrolled : Boolean;
  public
    Active : Boolean;

    procedure Assign(Source:TPersistent); override;
  published
    property Horizontal:TScrollDirection read FHorizontal write SetHorizontal default TScrollDirection.Normal;
    property Mode:TScrollingMode read FMode write FMode default TScrollingMode.Touch;
    property Vertical:TScrollDirection read FVertical write SetVertical default TScrollDirection.Normal;
  end;

  // Grid band to paint headers, cell rows and footers
  TRowGroup=class(TGridBand)
  private
    FColumns : TColumns;
    FFooter: TGridBands;
    FHeader : TColumnHeaderBand;
    FHeaders: TGridBands;
    FIndicator: TIndicator;
    FOnChangedSelected : TNotifyEvent;
    FOnNewDetail : TNewDetailEvent;
    FReadOnly: Boolean;
    FRows : TRows;
    FScrolling : TGridScrolling;
    FSelected: TGridSelection;

    FCurrent: TRowGroup;

    IBands : TGridBands;
    IParent : TRowGroup;

    procedure AddedBand(Sender:TObject);
    function CalcAutoWidth(const APainter:TPainter; const AColumn:TColumn;
                           const AWidth:Single):Single;

    procedure CalcColumnRow(const AState:TMouseState;
                            const AWidth,AHeight:Single;
                            out AColumn:TColumn;
                            out ARow:Integer);

    procedure ChangedBands(Sender: TObject);
    procedure ChangedCellFormat(Sender: TObject);
    procedure ChangedHeadersFooter(Sender:TObject);
    function CreateHeader:TColumnHeaderBand;
    procedure DoCalcWidth(const APainter:TPainter; const AColumn: TColumn;
                          const AWidth:Single);
    procedure DoHeadersFooter(var AState:TMouseState; const AWidth,AHeight:Single);
    procedure DoMove(var AState:TMouseState; const AWidth,AHeight:Single);
    procedure DoSelectedChanged(Sender:TObject);

    function GetCells: TTextRender;
    function GetData: TVirtualData; inline;
    function GetFields:TColumns;
    function GetFooter: TGridBands;
    function GetHeaders: TGridBands;
    function GetMargins: TMargins;

    class procedure InternalAddColumns(const AColumns:TColumns; const AData:TVirtualData); static;

    procedure PaintRest(var AData: TRenderData);
    function PositionOf(const AColumn:TColumn; const ARow:Integer):TPointF;
    procedure RemovedColumn(Sender:TObject);
    procedure RemoveHighLights(const ABands:TGridBands);
    procedure SetCells(const Value: TTextRender);
    procedure SetCurrent(const Value: TRowGroup);
    procedure SetField(const AColumn:TColumn; const ASource:TObject);
    procedure SetFooter(const Value: TGridBands);
    procedure SetHeader(const Value: TColumnHeaderBand);
    procedure SetHeaders(const Value: TGridBands);
    procedure SetIndicator(const Value: TIndicator);
    procedure SetMargins(const Value: TMargins);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetRows(const Value: TRows);
    procedure SetSelected(const Value: TGridSelection);

    function TryCreateBands(var ABands: TGridBands):TGridBands;
    procedure TryMouseChildren(var AState:TMouseState; const AWidth,AHeight:Single);
    function WidthOf(const APainter:TPainter; const AColumns: TColumns;
                     const AWidth:Single): Single;
    procedure SetScrolling(const Value: TGridScrolling);
  protected
    type
      // Internal event used to limit the minimum and maximum scrolling available
      TLimitScrollEvent=procedure(var X,Y:Single) of object;

    var
    OwnsData : Boolean;

    CheckLimits : TLimitScrollEvent;

    procedure CalculateHeight(var AData:TRenderData);
    procedure Loaded;
    function Scroll(const X,Y:Single):Boolean;
    procedure TryPaste(const AValue:String);
  public
    ParentColumn : TColumn;
    IsFocused : Boolean;
    MinColumnWidth : Single;
    RecalcScrollBars : Boolean;

    Constructor Create(const ACollection:TCollection;
                       const AData:TVirtualData); reintroduce;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure AutoScroll;

    procedure CalcHeight(const APainter:TPainter; const ATotal:Single); override;
    function CanEditRender(const AColumn:TColumn):Boolean;
    function CanStartEditor:Boolean;

    function CellRect(const ATopLeft:TPointF; const AColumn:TColumn; const ARow:Integer):TRectF; overload;
    function CellRect(const AColumn: TColumn; const ARow: Integer): TRectF; overload; inline;

    procedure CheckColumnsWidth(const APainter:TPainter; const Forced:Boolean;
                                const AWidth:Single);

    procedure Key(const AState:TKeyState);

    function MaxBottom:Single;

    function Mouse(var AState:TMouseState; const AWidth,AHeight:Single): Boolean; override;

    function NewExpander:TExpanderRender;

    procedure Paint(var AData:TRenderData; const ARender:TRender); override;
    procedure PrepareColumns(const APainter:TPainter; const ALeft,ARight:Single);

    procedure RefreshData(const AData:TVirtualData);
    function RenderHit(const AColumn:TColumn; const ARow:Integer; const X,Y:Single):Boolean;
    function SelectedContains(const X,Y:Single):Boolean; overload;
    function SelectedContains(const P:TPoint): Boolean; overload; inline;
    function ToggleDetail(const Sender:TRender; const ARow:Integer):Boolean;
    procedure TrySelectColumn;

    property Columns:TColumns read FColumns;
    property Current:TRowGroup read FCurrent write SetCurrent;
    property Data:TVirtualData read GetData;
  published
    property Cells:TTextRender read GetCells write SetCells;
    property Footer:TGridBands read GetFooter write SetFooter stored False;
    property Header:TColumnHeaderBand read FHeader write SetHeader;
    property Headers:TGridBands read GetHeaders write SetHeaders stored False;
    property Indicator:TIndicator read FIndicator write SetIndicator;
    property Margins:TMargins read GetMargins write SetMargins;
    property ReadOnly:Boolean read FReadOnly write SetReadOnly default False;
    property Rows:TRows read FRows write SetRows;
    property Scrolling:TGridScrolling read FScrolling write SetScrolling;
    property Selected:TGridSelection read FSelected write SetSelected;

    property OnNewDetail:TNewDetailEvent read FOnNewDetail write FOnNewDetail;
    property OnChangedSelected:TNotifyEvent read FOnChangedSelected write FOnChangedSelected;
  end;

implementation
