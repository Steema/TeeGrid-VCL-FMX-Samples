{*********************************************}
{  TeeGrid Software Library                   }
{  Grid Rows class                            }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Base;

interface

uses
  System.Classes, System.Types,

  Tee.Control, Tee.Format, Tee.Painter, Tee.Grid.Columns, Tee.Grid.Data,
  Tee.Renders;

type
  THover=class(TVisibleTextRender)
  private
    FParentFont: Boolean;

    procedure SetParentFont(const Value: Boolean);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
  published
    property ParentFont:Boolean read FParentFont write SetParentFont default True;
  end;

  TIndicator=class(TVisibleFormatRender)
  private
    FWidth: Single;

    function IsWidthStored: Boolean;
    procedure SetWidth(const Value: Single);
    function Triangle(const R:TRectF):TPointsF;
  public
    const
      DefaultWidth=10;

    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure Paint(const APainter:TPainter; const ABounds:TRectF);
  published
    property Width:Single read FWidth write SetWidth stored IsWidthStored;
  end;

  TVisibleColumns=Array of TColumn;

  TVisibleColumnsHelper=record helper for TVisibleColumns
  public
    function AllSameFormat:Boolean;
    function First:TColumn;
    function IndexOf(const AColumn: TColumn): Integer;
    function Last:TColumn;
    function Next(const AColumn:TColumn):TColumn;
    function Previous(const AColumn:TColumn):TColumn;
  end;

  TScroll=TPointF;

  TCustomTeeGridBase=class(TCustomTeeControl)
  private
    FColumns : TColumns;

    function IsColumnsStored: Boolean;
    procedure SetColumns(const Value: TColumns);
  protected
    Data : TVirtualData;

    FLineSize : Single;

    ValidRowsHeight : Boolean;

    procedure ChangedCellFormat(Sender:TObject);
    function CreateColumns:TColumns; virtual;
    function FirstRowTop:Single;
  public
    Scroll : TScroll;
    VisibleColumns : TVisibleColumns;

    const
      HorizOffset=4;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    function FirstColumnLeft: Single;

    function MaxRight: Single; virtual;

    function NextX(const AWidth:Single):Single; overload;
    function NextX(const AColumn:TColumn):Single; overload; inline;
  published
    property Columns:TColumns read FColumns write SetColumns stored IsColumnsStored;
  end;

  TGridPersistent=class(TPersistentChange)
  protected
    IGrid : TCustomTeeGridBase;
  public
    Constructor CreateGrid(const AGrid:TCustomTeeGridBase); virtual;
  end;

  TAlternateFormat=class(TFormat)
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
  end;

  TRows=class(TGridPersistent)
  private
    FAlternate: TAlternateFormat;
    FHeight: Single;

    ICustomHeights,
    IHeights : Array of Single;

    function GetHeights(const Index: Integer): Single;
    function IsHeightStored: Boolean;
    procedure SetAlternate(const Value: TAlternateFormat);
    procedure SetHeight(const Value: Single);
    procedure SetHeights(const Index: Integer; const Value: Single);
  protected
    procedure Paint(const APainter:TPainter;
                    const ARow:Integer;
                    const AMinX,APos,AStartX:Single); overload;
  public
    DefaultHeight : Single;

    Constructor CreateGrid(const AGrid:TCustomTeeGridBase); override;
    Destructor Destroy; override;

    function CalcTopRowPos(const ARow:Integer; const ATopOffset:Single):Single;
    procedure Clear;
    function Count:Integer;
    function FirstVisible:Integer;
    function HeightOf(const ARow:Integer):Single;

    procedure Paint(const APainter:TPainter;
                    const AFormat:TFormat;
                    const AMinX,ATopOffset:Single); overload;

    procedure PaintColumn(const APainter:TPainter;
                    const AColumn:TColumn;
                    const AMinX:Single;
                    const ARow:Integer;
                    const ARect:TRectF);

    property Heights[const Index:Integer]:Single read GetHeights write SetHeights;
  published
    property Alternate:TAlternateFormat read FAlternate write SetAlternate;
    property Height:Single read FHeight write SetHeight stored IsHeightStored;
  end;

  TGridLines=class(TGridPersistent)
  private
    FRows: TStroke;
    FColumns: TStroke;

    XOffset : Single;

    procedure Changed(Sender:TObject);

    procedure PaintColumns(const APainter: TPainter; const AMaxY:Single; const AIndicator:TIndicator);
    procedure PaintRows(const APainter: TPainter; const FirstY:Single; const FirstRow:Integer);

    procedure SetColumns(const Value: TStroke);
    procedure SetRows(const Value: TStroke);
  public
    IRows : TRows;

    const
      LineColor=$DDDDDD;

    Constructor CreateGrid(const AGrid:TCustomTeeGridBase); override;
    Destructor Destroy; override;

    procedure Paint(const APainter:TPainter;
                    const AMaxY,ATopOffset:Single;
                    const AIndicator:TIndicator;
                    const AFirstY:Single;
                    const AFirstRow:Integer);

    function RowsSize:Single;

  published
    property Columns:TStroke read FColumns write SetColumns;
    property Rows:TStroke read FRows write SetRows;
  end;

implementation
