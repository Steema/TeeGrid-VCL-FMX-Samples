{*********************************************}
{  TeeGrid Software Library                   }
{  Grid Header class                          }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Header;

interface

uses
  System.Classes,

  Tee.Grid.Columns, Tee.Grid.Base, Tee.Painter, Tee.Format, Tee.Renders;

type
  THeaderHeight=class(TCoordinate)
  private
    IHeader : TPersistentChange;
  end;

  THeader=class(TVisibleFormatRender)
  private
    FHover: THover;

    IDragging  : TColumn;
    IHighLight : TColumn;

    OldWidth,
    OldX : Single;

    IGrid : TCustomTeeGridBase;
    IColumns : TColumns;
    FOnClick: TNotifyEvent;
    FHeight: THeaderHeight;

    procedure ChangeDraggedWidth(const AValue:Single);
    procedure SetHover(const Value: THover);
    procedure SetHighLight(const Value: TColumn);
    procedure SetHeight(const Value: THeaderHeight);
  protected
  public
    const
      TopOffset=4;

    Constructor CreateGrid(const AGrid:TCustomTeeGridBase);
    Destructor Destroy; override;

    function CalcFont(const AColumn:TColumn):TFont;
    procedure MouseDown(const X:Single; const ADragged,AColumn:TColumn);
    function MouseMove(const Shift:TShiftState; const X,Y:Single; const AColumn:TColumn):Boolean;
    procedure MouseUp;

    procedure Paint(const APainter:TPainter; const AIndicator:TIndicator;
                    const AMinX,AStartX:Single);

    function HeightPixels:Single;
    function RowCount:Integer;
    function RowHeight: Single;

    property Dragging:TColumn read IDragging;
    property HighLight:TColumn read IHighLight write SetHighLight;

    function VertOffset:Single;
  published
    property Height:THeaderHeight read FHeight write SetHeight;
    property Hover:THover read FHover write SetHover;
    property OnClick:TNotifyEvent read FOnClick write FOnClick;
  end;

implementation
