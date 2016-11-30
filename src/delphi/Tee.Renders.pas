{*********************************************}
{  TeeGrid Software Library                   }
{  Basic Render shapes                        }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Renders;
{$I Tee.inc}

interface

{
   Basic shape objects:

   Rectangle, rectangle with text, checkbox, progress bar, expander etc.

   Needs: Tee.Format and Tee.Painter
}

uses
  {System.}Classes,

  {$IFNDEF FPC}
  {System.}Types,

  {$IFNDEF NOUITYPES}
  {System.}UITypes,
  {$ENDIF}

  {$ENDIF}

  Tee.Format, Tee.Painter;

type
  // Internal
  TRenderData=record
  public
    Painter : TPainter;
    Rect : TRectF;
    Row : Integer;
    Text : String;
  end;

  // Base class
  TRender=class(TPersistentChange)
  public
    function Hit(const R:TRectF; const X,Y:Single):Boolean; virtual;
    procedure Paint(var AData:TRenderData); virtual;
  end;

  // Just an alias
  TBorder=class(THiddenStroke);

  // Left,Top,Right,Bottom "Border" Strokes
  TBorders=class(TPersistentChange)
  private
    FRight: TBorder;
    FBottom: TBorder;
    FTop: TBorder;
    FLeft: TBorder;

    procedure SetBottom(const Value: TBorder);
    procedure SetLeft(const Value: TBorder);
    procedure SetRight(const Value: TBorder);
    procedure SetTop(const Value: TBorder);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;
    procedure Paint(const APainter: TPainter; const ARect: TRectF);
  published
    property Left:TBorder read FLeft write SetLeft;
    property Top:TBorder read FTop write SetTop;
    property Right:TBorder read FRight write SetRight;
    property Bottom:TBorder read FBottom write SetBottom;
  end;

  // Rectangle shape with Borders
  TFormatRender=class(TRender)
  private
    FBorders : TBorders;
    FFormat : TTextFormat;

    function GetBorders: TBorders;
    function GetFormat: TTextFormat;
    procedure SetBorders(const Value: TBorders);
    procedure SetFormat(const Value: TTextFormat);
  public
    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    function HasFormat:Boolean; inline;
    procedure Paint(var AData:TRenderData); override;
  published
    property Borders:TBorders read GetBorders write SetBorders;
    property Format:TTextFormat read GetFormat write SetFormat;
  end;

  // Left,Top,Right,Bottom edge properties (pixels or %)
  TEdges=class(TPersistentChange)
  private
    FRight: TCoordinate;
    FBottom: TCoordinate;
    FTop: TCoordinate;
    FLeft: TCoordinate;

    procedure SetBottom(const Value: TCoordinate);
    procedure SetLeft(const Value: TCoordinate);
    procedure SetRight(const Value: TCoordinate);
    procedure SetTop(const Value: TCoordinate);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    function Adjust(const R:TRectF):TRectF;
    procedure Assign(Source:TPersistent); override;
    function Horizontal:Single;
    procedure Prepare(const AWidth,AHeight:Single);
    function Vertical:Single;
  published
    property Left:TCoordinate read FLeft write SetLeft;
    property Top:TCoordinate read FTop write SetTop;
    property Right:TCoordinate read FRight write SetRight;
    property Bottom:TCoordinate read FBottom write SetBottom;
  end;

  TMargins=class(TEdges);

  // Horizontal and Vertical alignments
  TAlignments=class(TPersistentChange)
  private
    FHorizontal : THorizontalAlign;
    FVertical : TVerticalAlign;

    DefaultHorizontal : THorizontalAlign;
    DefaultVertical : TVerticalAlign;

    function IsHorizontalStored: Boolean;
    function IsVerticalStored: Boolean;
    procedure SetHorizontal(const Value: THorizontalAlign);
    procedure SetVertical(const Value: TVerticalAlign);
  public
    procedure Assign(Source:TPersistent); override;

    procedure InitHorizontal(const Value:THorizontalAlign);
    procedure InitVertical(const Value:TVerticalAlign);
  published
    property Horizontal:THorizontalAlign read FHorizontal write SetHorizontal stored IsHorizontalStored;
    property Vertical:TVerticalAlign read FVertical write SetVertical stored IsVerticalStored;
  end;

  // Just an alias
  TTextAlign=class(TAlignments);

  // Rectangle shape with Text, Margins and Alignments
  TTextRender=class(TFormatRender)
  private
    FAlign : TTextAlign;
    FMargins : TMargins;

    procedure SetAlign(const Value: TTextAlign);
    procedure SetMargins(const Value: TMargins);
  protected
  public
    PaintText : Boolean;
    TextLines : Integer;

    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    function CalcHeight:Single;
    function CalcTextLines(const AText:String):Integer;
    procedure Paint(var AData:TRenderData); override;
  published
    property Margins:TMargins read FMargins write SetMargins;
    property TextAlign:TTextAlign read FAlign write SetAlign;
  end;

  // Rectangle Text shape with Visible:Boolean property
  TVisibleTextRender=class(TTextRender)
  private
    FVisible : Boolean;

    procedure SetVisible(const Value: Boolean);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure Assign(Source:TPersistent); override;

    procedure Hide;
    procedure Show;
  published
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  TCellRender=class(TVisibleTextRender)
  private
    FText: String;

    procedure SetText(const Value: String);
  public
    procedure Assign(Source:TPersistent); override;
  published
    property Text:String read FText write SetText;
  end;

  // Square shape with format (Brush and Stroke)
  TBox=class(TPersistentChange)
  private
    const
      DefaultSize=5;

    var
    FFormat : TFormat;
    FSize : Single;

    procedure SetFormat(const Value: TFormat);
    function IsSizeStored: Boolean;
    procedure SetSize(const Value: Single);
  protected
    DrawBox,
    Draw : Boolean;
  public
    Constructor Create(const AChanged: TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

  published
    property Format:TFormat read FFormat write SetFormat;
    property Size:Single read FSize write SetSize stored IsSizeStored;
  end;

  // Rectangle Text shape with an extra Box (square)
  TBoxRender=class(TTextRender)
  private
    FBox : TBox;

    procedure SetBox(const Value: TBox);
  protected
    function Calculate(const R:TRectF):TRectF;
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    function Hit(const R:TRectF; const X,Y:Single):Boolean; override;
    procedure Paint(var AData:TRenderData); override;
  published
    property Box:TBox read FBox write SetBox;
  end;

  TBooleanRenderStyle=(Check,Text);

  // Rectangle shape with Box to emulate a "CheckBox"
  TBooleanRender=class(TBoxRender)
  private
    FCheckFormat : TFormat;
    FStyle: TBooleanRenderStyle;

    procedure SetStyle(const Value: TBooleanRenderStyle);
    procedure SetCheckFormat(const Value: TFormat);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure Paint(var AData:TRenderData); override;
  published
    property CheckFormat:TFormat read FCheckFormat write SetCheckFormat;
    property Style:TBooleanRenderStyle read FStyle write SetStyle default TBooleanRenderStyle.Check;
  end;

  TExpanderRender=class;

  TCanExpandEvent=function(const Sender:TRender; const ARow:Integer):Boolean of object;
  TGetExpandedEvent=function(const Sender:TRender; const ARow:Integer):Boolean of object;

  TExpanderStyle=(PlusMinus,Triangle,Arrow);

  // Rectangle shape with Box to emulate a "+" "-" expander (also triangles or arrows)
  TExpanderRender=class(TBoxRender)
  private
    FExpandFormat : TFormat;
    FExpandLine: TStroke;
    FOnCanExpand : TCanExpandEvent;
    FOnGetExpanded : TGetExpandedEvent;
    FStyle : TExpanderStyle;

    procedure SetExpandFormat(const Value: TFormat);
    procedure SetExpandLine(const Value: TStroke);
    procedure SetStyle(const Value: TExpanderStyle);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure Paint(var AData:TRenderData); override;
    procedure PaintLines(var AData:TRenderData);

    property OnCanExpand:TCanExpandEvent read FOnCanExpand write FOnCanExpand;
    property OnGetExpanded:TGetExpandedEvent read FOnGetExpanded write FOnGetExpanded;
  published
    property ExpandFormat:TFormat read FExpandFormat write SetExpandFormat;
    property ExpandLine:TStroke read FExpandLine write SetExpandLine;
    property Style:TExpanderStyle read FStyle write SetStyle default TExpanderStyle.PlusMinus;
  end;

  TOrientation=(Horizontal,Vertical);

  // Rectangle shape with partial filling to emulate a "ProgressBar"
  TProgressRender=class(TFormatRender)
  private
    FMax : Single;
    FMin: Single;
    FOrientation: TOrientation;

    IRange : Single;

    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetOrientation(const Value: TOrientation);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure Assign(Source:TPersistent); override;

    procedure Paint(var AData:TRenderData); override;
  published
    property Maximum:Single read FMax write SetMax stored IsMaxStored;
    property Minimum:Single read FMin write SetMin stored IsMinStored;
    property Orientation:TOrientation read FOrientation write SetOrientation default TOrientation.Horizontal;
  end;

  // Rectangle with Text and "ParentFont" boolean property
  THover=class(TVisibleTextRender)
  private
    FParentFont: Boolean;

    procedure SetParentFont(const Value: Boolean);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure InitFormat;
  published
    property ParentFont:Boolean read FParentFont write SetParentFont default True;
  end;

  // Base class for TGridBand and TColumn
  TVisibleRenderItem=class(TCollectionItem)
  private
    FTagObject : TObject;
    FVisible: Boolean;

    function GetRender: TRender;
    procedure SetFormat(const Value: TTextFormat);
    procedure SetRender(const Value: TRender);
    procedure SetVisible(const Value: Boolean);
    function IsFormatStored: Boolean;
  protected
    FRender: TRender;

    function CreateRender:TRender; virtual;
    procedure DoChanged; virtual;
    function GetFormat: TTextFormat; virtual;
  public
    Constructor Create(ACollection:TCollection); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure Changed(Sender:TObject);
    function HasFormat:Boolean; inline;
    function HasRender:Boolean; inline;
    procedure Paint(var AData:TRenderData; const ARender:TRender); virtual;

    property Render:TRender read GetRender write SetRender;
    property TagObject:TObject read FTagObject write FTagObject;
  published
    property Format:TTextFormat read GetFormat write SetFormat stored IsFormatStored;
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

implementation
