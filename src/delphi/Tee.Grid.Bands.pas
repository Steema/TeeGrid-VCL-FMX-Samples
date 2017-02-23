{*********************************************}
{  TeeGrid Software Library                   }
{  Basic Grid "Bands"                         }
{  Copyright (c) 2016-2017 by Steema Software }
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

   TTextBand is an example of a custom TGridBand to display text at for
   example grid header and footer.

   Other units implement different kinds of Grid Bands, like Grid Header,
   Grid Totals and Grid Rows.

   Needs: Tee.Format and Tee.Renders
}

uses
  {System.}Classes,

  Tee.Painter, Tee.Format, Tee.Renders;

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

    function IsDoubleLeft:Boolean;
  end;

  TGridKeyEvent=(Down,Up);

  TKeyState=record
    Key : Word;
    KeyChar : {Wide}Char;
    Shift : TShiftState;
    Event : TGridKeyEvent;
  end;

  // Just an alias
  TBandHeight=class(TCoordinate);

  // Base class with Hover and Height properties
  TGridBand=class(TVisibleRenderItem)
  private
    FHeight: TBandHeight;
    FOnClick: TNotifyEvent;

    procedure Changed(Sender: TObject);
    procedure SetHeight(const Value: TBandHeight);
  protected
    Top,
    Bottom : Single;
  public
    Constructor Create(ACollection:TCollection); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure CalcHeight(const APainter:TPainter; const ATotal:Single); virtual;
    function Contains(const X,Y:Single):Boolean;
    class function Description:String; virtual;
    function Mouse(var AState:TMouseState; const AWidth,AHeight:Single): Boolean; overload; virtual;
    procedure Paint(var AData:TRenderData; const ARender:TRender); override;
  published
    property Height:TBandHeight read FHeight write SetHeight;
    property OnClick:TNotifyEvent read FOnClick write FOnClick;
  end;

  TGridBandClass=class of TGridBand;

  // Basic grid band with a Lines property (Stroke)
  TGridBandLines=class(TGridBand)
  private
    FLines : TStroke;

    procedure SetLines(const Value: TStroke);
  protected
    IJustRepaint : Boolean;
  public
    Constructor Create(ACollection:TCollection); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}
  published
    property Lines:TStroke read FLines write SetLines;
  end;

  // Basic grid band with Text, usable at grid header and footer
  TTextBand=class(TGridBand)
  private
    function GetText:String;
    function GetTextRender: TCellRender;
    procedure SetText(const Value: String);
  protected
    function CreateRender:TRender; override;
  public
    procedure CalcHeight(const APainter:TPainter; const ATotal:Single); override;
    class function Description:String; override;
    procedure Paint(var AData:TRenderData; const ARender:TRender); override;

    property TextRender:TCellRender read GetTextRender;
  published
    property Text:String read GetText write SetText;
  end;

  // Collection of Grid Bands
  TGridBands=class(TCollectionChange)
  private
    FVisible : Boolean;

    function Get(Index: Integer): TGridBand; {$IFNDEF FPC}inline;{$ENDIF}
    procedure Put(Index: Integer; const Value: TGridBand); {$IFNDEF FPC}inline;{$ENDIF}
    procedure SetVisible(const Value: Boolean);
  public
    const
      Spacing=0;

    var
      Floating : Boolean;
      Height : Single;

    Constructor Create(AOwner: TPersistent; const AChanged:TNotifyEvent); reintroduce;

    procedure Assign(Source:TPersistent); override;

    function AddText(const AText:String):TTextBand;

    procedure CalcHeight(const APainter:TPainter; const ATotal:Single);
    function CanDisplay:Boolean; inline;
    procedure Mouse(var AState:TMouseState; const AWidth,AHeight:Single);
    procedure Paint(var AData:TRenderData);

    property Items[Index: Integer]: TGridBand read Get write Put; default;
  published
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

implementation
