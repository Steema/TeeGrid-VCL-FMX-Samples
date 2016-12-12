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
    FColumns : TColumns;
    FFooter: TGridBands;
    FHeader : TColumnHeaderBand;
    FHeaders: TGridBands;
    FIndicator: TIndicator;
    FOnChangedSelected : TNotifyEvent;
    FOnNewDetail : TNewDetailEvent;
    FReadOnly: Boolean;
    FRows : TRows;
    FSelected: TGridSelection;

    OwnsData : Boolean;
    FCurrent: TRowGroup;

    IBands : TGridBands;
    ISelectedDragging : Integer;

    procedure AddedBand(Sender:TObject);
    function CalcAutoWidth(const APainter:TPainter; const AColumn:TColumn;
                           const AWidth:Single):Single;
    procedure ChangedCellFormat(Sender: TObject);
    procedure ChangedHeadersFooter(Sender:TObject);
    function CreateHeader:TColumnHeaderBand;
    procedure DoCalcWidth(const APainter:TPainter; const AColumn: TColumn;
                          const AWidth:Single);
    procedure DoHeadersFooter(var AState:TMouseState; const AWidth,AHeight:Single);
    procedure DoMove(var AState:TMouseState; const AWidth,AHeight:Single);
    procedure DoSelectedChanged(Sender:TObject);

    function GetCells: TTextRender;
    function GetData: TVirtualData;
    function GetFooter: TGridBands;
    function GetHeaders: TGridBands;
    function GetMargins: TMargins;

    procedure PaintRest(var AData: TRenderData);
    function PositionOf(const AColumn:TColumn; const ARow:Integer):TPointF;
    procedure RemovedColumn(Sender:TObject; const AColumn:TColumn);
    procedure SetCells(const Value: TTextRender);
    procedure SetCurrent(const Value: TRowGroup);
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
  protected
    procedure Loaded;

  public
    ParentColumn : TColumn;
    IsFocused,
    RecalcScrollBars : Boolean;

    const
      MinColumnWidth:Single=24;

    Constructor Create(const ACollection:TCollection;
                       const AData:TVirtualData); reintroduce;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

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

    procedure Paint(var AData:TRenderData; const ARender:TRender); override;
    procedure PrepareColumns(const APainter:TPainter; const ALeft,ARight:Single);

    procedure RefreshData(const AData:TVirtualData);
    function RenderHit(const AColumn:TColumn; const ARow:Integer; const X,Y:Single):Boolean;
    function ToggleDetail(const Sender:TRender; const ARow:Integer):Boolean;

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
    property Selected:TGridSelection read FSelected write SetSelected;

    property OnNewDetail:TNewDetailEvent read FOnNewDetail write FOnNewDetail;
    property OnChangedSelected:TNotifyEvent read FOnChangedSelected write FOnChangedSelected;
  end;

implementation
