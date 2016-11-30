{*********************************************}
{  TeeGrid Software Library                   }
{  Abstract Painter class                     }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Painter;
{$I Tee.inc}

interface

uses
  {$IFNDEF FPC}
  {System.}Types,
  {$ENDIF}

  {WinAPI.}Windows, {VCL.}Graphics,
  Tee.Painter, Tee.Format;

type
  TTeeFont=Tee.Format.TFont;

  TWindowsPainter=class(TPainter)
  protected
    ICanvas : TCanvas;
    IDC : HDC;
  public
    Constructor Create(const ACanvas:TCanvas);

    property Canvas:TCanvas read ICanvas;

    procedure Init(const DC:HDC); virtual;
  end;

  TGDIPainter=class(TWindowsPainter)
  private
    type
      TAlign=record
      public
        Horizontal : THorizontalAlign;
        Vertical : TVerticalAlign;

        function Flags:Cardinal;
      end;

    var
      IAlign : TAlign;
      IClipHistory : Array of HRGN;

      //IFontGradient,
      //IStrokeGradient,
      IBrushGradient : TGradient;

      IBrushPicture : TPicture;

    function GraphicOf(const APicture: TPicture):TGraphic;
    function ImageFrom(const APicture: TPicture): TGraphic;
    procedure SetTextAlignments;
  public
    class procedure ApplyFont(const ASource:TTeeFont; const ADest:Graphics.TFont); static;

    procedure Clip(const R:TRectF); override;
    procedure Clip(const R:TRect); overload;
    procedure UnClip; override;

    procedure HideBrush; override;

    procedure SetBrush(const ABrush:TBrush); override;
    procedure SetFont(const AFont:TFont); override;
    procedure SetHorizontalAlign(const Value:THorizontalAlign); override;
    procedure SetStroke(const AStroke:TStroke); override;
    procedure SetVerticalAlign(const Value:TVerticalAlign); override;

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
    procedure Paint(const AFormat: TFormat; const R: TRectF); override;
    procedure Paint(const AFormat: TFormat; const P: TPointsF); override;
    function TextHeight(const AText:String):Single; override;
    procedure TextOut(const X,Y:Single; const AText:String); override;
    function TextWidth(const AText:String):Single; override;
    procedure VerticalLine(const X,Y0,Y1:Single); override;
  end;

implementation
