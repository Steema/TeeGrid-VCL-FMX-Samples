{*********************************************}
{  TeeGrid Software Library                   }
{  Windows GDI+ Plus Painter class            }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Painter.GdiPlus;
{$I Tee.inc}

interface

uses
  {System.}Types,
  Windows, {VCL.}Graphics, {Winapi.}GdiPAPI, {Winapi.}GdiPObj,
  Tee.Painter, Tee.Format, VCLTee.Painter;

type
  TGdiPlusPainter=class(TWindowsPainter)
  private
    FSmoothingMode : TSmoothingMode;
    FTextQuality : TTextRenderingHint;

    IPlus : TGPGraphics;
    IStringFormat : TGPStringFormat;

    IBitmap : TBitmap;
    IBrush : TGPSolidBrush;
    IFont : TGPFont;
    IFontBrush,
    ISolidBrush : TGPSolidBrush;
    IPen : TGpPen;

    IHorizAlign : THorizontalAlign;
    IVertAlign : TVerticalAlign;

    IClipped : Array of TGPRectF;

    IFontGradient,
    IStrokeGradient,
    IBrushGradient : TGradient;

    IBrushPicture : TPicture;

    // Maximum 256 colors for Gradient:
    IBlendColors : Array[0..255] of TGPColor;
    IBlendPositions : Array[0..255] of Single;

    function CreateGradient(const AGradient:TGradient; const R:TGPRectF):TGPBrush; overload;
    function CreateGradient(const AGradient:TGradient; const P:TPointsF):TGPBrush; overload;

    procedure DrawImage(const APicture: TPicture; const AImage:TGPImage; const ARect:TGPRectF);
    procedure FinishPen(const APen:TGPPen; const AStroke: TStroke);
    function GetCanvas:TGPGraphics;
    function ImageFrom(const AGraphic: TGraphic):TGPImage; overload;
    function ImageFrom(const APicture: TPicture):TGPImage; overload;
    procedure InitGraphics;
    procedure SetSmoothingMode(const Value: TSmoothingMode);
    procedure SetTextQuality(const Value: TTextRenderingHint);
    function TextSize(const AText:String):TGPRectF;
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Clip(const R:TRectF); override;
    procedure UnClip; override;

    procedure HideBrush; override;

    procedure Init(const DC:HDC); override;

    procedure SetBrush(const ABrush:TBrush); override;
    procedure SetFont(const AFont:TFont); override;
    procedure SetFontColor(const AColor:TColor); override;
    procedure SetHorizontalAlign(const Align:THorizontalAlign); override;
    procedure SetStroke(const AStroke:TStroke); override;
    procedure SetTextTrimming(const ATrimming:TTrimmingMode; const Ellipsi:Boolean); override;
    procedure SetVerticalAlign(const Align:TVerticalAlign); override;

    procedure Draw(const R:TRectF); override;
    procedure Draw(const P:TPointsF); override;
    procedure Draw(const APicture:TPicture; const X,Y:Single); overload; override;
    procedure Draw(const APicture:TPicture; const R:TRectF); overload; override;
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

  // properties

    property Canvas:TGPGraphics read GetCanvas;

    property SmoothMode:TSmoothingMode read FSmoothingMode write SetSmoothingMode
                                       default SmoothingModeHighQuality;

    property StringFormat:TGpStringFormat read IStringFormat write IStringFormat;
    property TextQuality:TTextRenderingHint read FTextQuality write SetTextQuality
                                  default TextRenderingHintClearTypeGridFit;
  end;

implementation
