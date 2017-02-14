{*********************************************}
{  TeeGrid Software Library                   }
{  Grid Header class                          }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Header;
{$I Tee.inc}

interface

{
  Custom Grid Band classes to display content for Columns.

    TColumnBand :

      Base abstract class for grid bands with Columns.

      Provides:

         Column mouse-dragging
         Column mouse-resizing
         Mouse-hover highlighting
         Horizontal scrolling

    TColumnHeaderBand :

      Implements a TeeGrid Header (to show column names) with multiple
      sub-levels if the columns have sub-columns.

      Includes a stroke "RowLines" property to paint lines between sub-levels.


  Needs: Tee.Painter, Tee.Format, Tee.Renders, Tee.Grid.Bands and Tee.Grid.Columns
}


uses
  {System.}Classes,

  {$IFDEF FPC}
  Graphics,
  {$ENDIF}

  {$IFNDEF FPC}
  {System.}Types,
  {$ENDIF}

  Tee.Painter, Tee.Format, Tee.Renders,
  Tee.Grid.Columns, Tee.Grid.Bands, Tee.Grid.Data;

type
  TSelectedRender=class(THover)
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
  end;

  // Grid band with Columns
  TColumnBand=class(TGridBandLines)
  private
    FAllowDrag : Boolean;
    FAllowResize : Boolean;
    FHover: THover;
    FOnColumnResized: TNotifyEvent;
    FSelected : TSelectedRender;

    IDragging,
    IHighLight,
    IResizing,
    ISelected : TColumn;

    MouseColumn : TColumn; // current column under mouse XY

    NewDragX,
    OldWidth,
    OldX : Single;

    IHeights : Array of Single;

    function BoundsOf(const AColumn:TColumn):TRectF;
    procedure ChangeDraggedWidth(const AValue:Single);
    procedure DoChangedRepaint;
    function DragHit(const ARect:TRectF; out HitRect:TRectF):TColumn;
    function DragRectangle:TRectF;
    function GetMargins: TMargins;
    function HeaderRender(const AColumn:TColumn):TRender;
    function MaxColumnHeight(const APainter:TPainter; const ATotal:Single):Single;
    procedure PaintLines(var AData:TRenderData; const DrawFirst:Boolean);
    procedure SetColumns(const Value: TColumns);
    procedure SetHighLight(const Value: TColumn);
    procedure SetHover(const Value: THover);
    procedure SetMargins(const Value: TMargins);
    procedure SetSelected(const Value: TSelectedRender);
    procedure SetSelectedColumn(const Value: TColumn);
  protected
    IColumns : TColumns;
    IData : TVirtualData;
    IJustRepaint : Boolean;
    IVisible : TVisibleColumns;

    ISingleRow : Boolean;

    function AdjustBounds(const AColumn:TColumn; const R:TRectF):TRectF;
    function AsString(const AColumn:TColumn):String; virtual; abstract;
    function LevelTop(const ALevel:Integer):Single;
    procedure DoClick; virtual;
  public
    Width : Single;

    // Temporary
    MinX,
    //OffsetX,
    StartX : Single;

    Constructor Create(ACollection:TCollection); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    function CalcFont(const AColumn:TColumn; const AFont:TFont):TFont;
    procedure CalcHeight(const APainter:TPainter; const ATotal:Single); override;
    procedure InitFormat;
    function Mouse(var AState:TMouseState; const AWidth,AHeight:Single):Boolean; override;
    procedure Paint(var AData:TRenderData; const ARender:TRender); override;
    function RowCount:Integer;

    property Columns:TColumns read IColumns write SetColumns;
    property Data:TVirtualData read IData write IData;

    // Current column being dragged (moved)
    property Dragging:TColumn read IDragging;

    // Current column being resized
    property Resizing:TColumn read IResizing;

    // Current column to highlight (on mouse hover)
    property HighLight:TColumn read IHighLight write SetHighLight;

    // Current column to highlight (on selected cell)
    property SelectedColumn:TColumn read ISelected write SetSelectedColumn;
  published
    property AllowDrag:Boolean read FAllowDrag write FAllowDrag default True;
    property AllowResize:Boolean read FAllowResize write FAllowResize default True;
    property Hover:THover read FHover write SetHover;
    property Margins:TMargins read GetMargins write SetMargins;
    property Selected:TSelectedRender read FSelected write SetSelected;

    property OnColumnResized:TNotifyEvent read FOnColumnResized write FOnColumnResized;
  end;

  TCanSortEvent=procedure(const AColumn:TColumn; var CanSort:Boolean) of object;

  TSortState=(None,Ascending,Descending);

  TSortByEvent=TColumnEvent;

  TSortStateEvent=procedure(const AColumn:TColumn; var State:TSortState) of object;

  TSortableHeader=class(TFormatRender)
  private
    const
      DefaultSize=6;

    var
      FOnCanSort : TCanSortEvent;
      FOnSortBy : TSortByEvent;
      FOnSortState : TSortStateEvent;
      FSize: Single;

      IState : TSortState;

    function IsSizeStored: Boolean;
    procedure SetSize(const Value: Single);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    function Hit(const R:TRectF; const X,Y:Single):Boolean; override;
    procedure Paint(var AData:TRenderData); override;
  published
    property Size:Single read FSize write SetSize stored IsSizeStored;

    property OnCanSort:TCanSortEvent read FOnCanSort write FOnCanSort;
    property OnSortBy:TSortByEvent read FOnSortBy write FOnSortBy;
    property OnSortState:TSortStateEvent read FOnSortState write FOnSortState;
  end;

  // Grid Header main class
  TColumnHeaderBand=class(TColumnBand)
  private
    FSortRender : TRender;
    FRowLines : TStroke;
    FSortable: Boolean;

    procedure PaintRowLines(const APainter:TPainter; const AColumns:TColumns; const ALevel:Integer);
    procedure SetRowLines(const Value: TStroke);
    procedure SetSortable(const Value: Boolean);
    procedure SetSortRender(const Value: TRender);
  protected
    function AsString(const AColumn:TColumn):String; override;
    procedure DoClick; override;
  public
    Constructor Create(ACollection:TCollection); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single;
    function CanSort(const AColumn:TColumn):Boolean;
    class function Description:String; override;
    function Mouse(var AState:TMouseState; const AWidth,AHeight:Single):Boolean; override;
    procedure Paint(var AData:TRenderData; const ARender:TRender); override;

    property SortRender:TRender read FSortRender write SetSortRender;
  published
    property RowLines:TStroke read FRowLines write SetRowLines;
    property Sortable:Boolean read FSortable write SetSortable default True;
  end;

implementation
