{*********************************************}
{  TeeGrid Software Library                   }
{  FMX Painter class                          }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Painter;
{$I Tee.inc}

interface

{$IF CompilerVersion>24}
{$DEFINE USELAYOUT}
{$IFEND}

uses
  System.Types, System.UITypes,

  {$IFDEF USELAYOUT}
  FMX.TextLayout,
  {$ENDIF}

  FMX.Types,

  {$IF CompilerVersion<=25}
  {$DEFINE HASFMX20}
  {$IFEND}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  Tee.Painter, Tee.Format;

{$IF CompilerVersion>26}
{$DEFINE D20}
{$IFEND}

type
  {$IF CompilerVersion<=25}
  FMXTFont=FMX.Types.TFont;
  FMXTBrush=FMX.Types.TBrush;
  {$ELSE}
  FMXTFont=FMX.Graphics.TFont;
  FMXTBrush=FMX.Graphics.TBrush;
  {$IFEND}

  TFMXPainter=class(TPainter)
  private
    ICanvas : TCanvas;

    IClipped : Array of TCanvasSaveState;

    {$IFDEF USELAYOUT}
    ILayout : TTextLayout;
    {$ENDIF}

    ISolidBrush : FMXTBrush;

    FFontColor : TColor;
    FHorizAlign : THorizontalAlign;
    FOpacity : Single;
    FVertAlign : TVerticalAlign;

    function GraphicOf(const APicture: TPicture):TBitmap;
    procedure TextSize(const AText:String; out AWidth,AHeight:Single);

    {$IFDEF USELAYOUT}
    procedure PrepareLayout(const R:TRectF; const AText:String);
    {$ENDIF}
  protected
    procedure SetCanvas(const ACanvas:TCanvas); inline;
  public
    Constructor Create(const ACanvas:TCanvas);
    Destructor Destroy; override;

    class procedure ApplyFont(const ASource:Tee.Format.TFont; const ADest:FMXTFont); static;

    procedure Clip(const R:TRectF); override;
    procedure UnClip; override;

    procedure HideBrush; override;

    procedure SetBrush(const ABrush:TBrush); override;
    procedure SetFont(const AFont:TFont); override;
    procedure SetFontColor(const AColor:TColor); override;
    procedure SetHorizontalAlign(const Align:THorizontalAlign); override;
    procedure SetStroke(const AStroke:TStroke); override;
    procedure SetVerticalAlign(const Align:TVerticalAlign); override;

    procedure Draw(const R:TRectF); override;
    procedure Draw(const P:TPointsF); override;
    procedure Draw(const APicture: TPicture; const X, Y: Single); override;
    procedure Draw(const APicture: TPicture; const R: TRectF); override;
    procedure DrawEllipse(const R:TRectF); override;

    procedure Fill(const R:TRectF); override;
    procedure Fill(const R:TRectF; const AColor:TColor); override;
    procedure Fill(const P:TPointsF); override;
    procedure FillEllipse(const R:TRectF); override;

    procedure HorizontalLine(const Y,X0,X1:Single); override;
    procedure Line(const X0,Y0,X1,Y1:Single); override;
    procedure Lines(const P:TPointsF); override;
    procedure Paint(const AFormat:TFormat; const R:TRectF); override;
    procedure Paint(const AFormat:TFormat; const P:TPointsF); override;
    function TextHeight(const AText:String):Single; override;
    procedure TextOut(const ARect:TRectF; const AText:String); override;
    function TextWidth(const AText:String):Single; override;
    procedure VerticalLine(const X,Y0,Y1:Single); override;
  end;

implementation
