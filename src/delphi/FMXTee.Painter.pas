{*********************************************}
{  TeeGrid Software Library                   }
{  FMX Painter class                          }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Painter;

interface

uses
  System.Types, System.UITypes,
  FMX.TextLayout, FMX.Types,

  {$IF CompilerVersion<=25}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  Tee.Painter, Tee.Format;

{$DEFINE USELAYOUT}

type
  {$IF CompilerVersion<=25}
  FMXTFont=FMX.Types.TFont;
  {$ELSE}
  FMXTFont=FMX.Graphics.TFont;
  {$ENDIF}

  TFMXPainter=class(TPainter)
  private
    ICanvas : TCanvas;
    ICanvasState : TCanvasSaveState;

    {$IFDEF USELAYOUT}
    ILayout : TTextLayout;
    {$ENDIF}

    FFontColor : TColor;
    FHorizAlign : THorizAlign;
    FOpacity : Single;

    function GraphicOf(const APicture: TPicture):TBitmap;
    procedure TextSize(const AText:String; out AWidth,AHeight:Single);

    {$IFDEF USELAYOUT}
    procedure PrepareLayout(const R:TRectF; const AText:String);
    {$ENDIF}
  public
    Constructor Create(const ACanvas:TCanvas);
    Destructor Destroy; override;

    class procedure ApplyFont(const ASource:Tee.Format.TFont; const ADest:FMXTFont); static;

    procedure Clip(const R:TRectF); override;
    procedure UnClip; override;

    procedure HideBrush; override;

    procedure SetBrush(const ABrush:TBrush); override;
    procedure SetCanvas(const ACanvas:TCanvas); inline;
    procedure SetFont(const AFont:TFont); override;
    procedure SetHorizAlign(const Align:THorizAlign); override;
    procedure SetStroke(const AStroke:TStroke); override;

    procedure Draw(const R:TRectF); override;
    procedure Draw(const P:TPointsF); override;
    procedure Draw(const APicture: TPicture; const X, Y: Single); override;
    procedure Draw(const APicture: TPicture; const R: TRectF); override;
    procedure Fill(const R:TRectF); override;
    procedure Fill(const P:TPointsF); override;
    procedure HorizontalLine(const Y,X0,X1:Single); override;
    procedure Line(const X0,Y0,X1,Y1:Single); override;
    procedure TextOut(const X,Y:Single; const AText:String); override;
    function TextWidth(const AText:String):Single; override;
    procedure VerticalLine(const X,Y0,Y1:Single); override;
  end;

implementation
