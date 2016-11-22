{*********************************************}
{  TeeGrid Software Library                   }
{  Abstract Painter class                     }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Painter.GdiPlus;
{$I Tee.inc}

interface

uses
  System.Types,
  Windows, VCL.Graphics, GdiPAPI, GdiPObj,
  Tee.Painter, Tee.Format, VCLTee.Painter;

type
  TGdiPlusPainter=class(TWindowsPainter)
  private
    IPlus : TGPGraphics;

    IBrush : TGPSolidBrush;
    IFont : TGPFont;
    IFontBrush,
    ISolidBrush : TGPSolidBrush;
    IPen : TGpPen;

    IHorizAlign : THorizontalAlign;
    IVertAlign : TVerticalAlign;

    IClipped : Array of TGPRectF;

    function GetCanvas:TGPGraphics;
    function ImageFrom(const AGraphic: TGraphic):TGPImage;
    function TextSize(const AText:String):TGPRectF;
  public
    Constructor Create;
    Destructor Destroy; override;

    property Canvas:TGPGraphics read GetCanvas;

    procedure Clip(const R:TRectF); override;
    procedure UnClip; override;

    procedure HideBrush; override;

    procedure Init(const DC:HDC); override;

    procedure SetBrush(const ABrush:TBrush); override;
    procedure SetFont(const AFont:TFont); override;
    procedure SetHorizontalAlign(const Align:THorizontalAlign); override;
    procedure SetStroke(const AStroke:TStroke); override;
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
    procedure TextOut(const X,Y:Single; const AText:String); override;
    function TextWidth(const AText:String):Single; override;
    procedure VerticalLine(const X,Y0,Y1:Single); override;
  end;

implementation
