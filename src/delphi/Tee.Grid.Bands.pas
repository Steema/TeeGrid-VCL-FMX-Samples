{*********************************************}
{  TeeGrid Software Library                   }
{  Basic Grid "Bands"                         }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Bands;
{$I Tee.inc}

{$SCOPEDENUMS ON}

interface

{
   Base classes for "Grid Bands" collection.

   A "Grid Band" is a rectangle to display at a Grid with support for
   mouse-hover highlighting, custom height and OnClick.

   TTitleBand is an example of a custom TGridBand to display text at for
   example grid header and footer.

   Other units implement different kinds of Grid Bands, like Grid Header,
   Grid Totals and Grid Rows.

   Needs: Tee.Format and Tee.Renders
}

uses
  {System.}Classes,

  Tee.Format, Tee.Renders;

type
  // Mouse and keyboard agnostic types

  TGridMouseButton=(Left,Middle,Right);

  TGridMouseEvent=(Down,Move,Up,DoubleClick);

  TMouseCursor=(Default,HandPoint,HorizResize,VertResize);

  TMouseState=record
  public
    Shift : TShiftState;
    X,Y : Single;
    Button : TGridMouseButton;
    Event : TGridMouseEvent;
    Cursor : TMouseCursor;
  end;

  TGridKeyEvent=(Down,Up);

  TKeyState=record
    Key : Word;
    Shift : TShiftState;
    Event : TGridKeyEvent;
  end;

  // Just an alias
  TBandHeight=class(TCoordinate);

  // Base class with Hover and Height properties
  TGridBand=class(TVisibleTextRender)
  private
    FHeight: TBandHeight;
    FOnClick: TNotifyEvent;

    procedure SetHeight(const Value: TBandHeight);
  protected
    Top : Single;
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure CalcHeight(const ATotal:Single); virtual;
    function Contains(const X,Y:Single):Boolean;
    function Mouse(var AState:TMouseState; const AWidth,AHeight:Single): Boolean; overload; virtual;
    procedure Paint(var AData:TRenderData); override;
    function RowHeight: Single;
  published
    property Height:TBandHeight read FHeight write SetHeight;
    property OnClick:TNotifyEvent read FOnClick write FOnClick;
  end;

  TGridBandLines=class(TGridBand)
  private
    FLines : TStroke;

    procedure SetLines(const Value: TStroke);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}
  published
    property Lines:TStroke read FLines write SetLines;
  end;

  // Collection Item with a Band property
  TGridBandItem=class(TCollectionItem)
  private
    FBand : TGridBand;
  public
    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    property Band : TGridBand read FBand write FBand;
  end;

  // Collection of Grid Bands
  TGridBands=class(TCollectionChange)
  private
    function Get(Index: Integer): TGridBandItem;
    procedure Put(Index: Integer; const Value: TGridBandItem);
  public
    const
      Spacing=0;

    var
      Floating : Boolean;
      Height : Single;

    function Add(const ABand:TGridBand):TGridBandItem;
    procedure CalcHeight(const ATotal:Single);

    procedure Mouse(var AState:TMouseState; const AWidth,AHeight:Single);

    procedure Paint(var AData:TRenderData);

    property Items[Index: Integer]: TGridBandItem read Get write Put; default;
  end;

  // Basic grid band with Text, usable at grid header and footer
  TTitleBand=class(TGridBand)
  private
    FText: String;

    procedure SetText(const Value: String);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure Paint(var AData:TRenderData); override;
  published
    property Text:String read FText write SetText;
  end;

implementation
