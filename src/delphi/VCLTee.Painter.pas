{*********************************************}
{  TeeGrid Software Library                   }
{  Abstract Painter class                     }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Painter;

interface

uses
  System.Types,
  WinAPI.Windows, VCL.Graphics,
  Tee.Painter, Tee.Format;

type
  TGDIPainter=class(TPainter)
  private
    ICanvas : TCanvas;

    function GraphicOf(const APicture: TPicture):TGraphic;
  public
    Constructor Create(const ACanvas:TCanvas);
    Destructor Destroy; override;

    property Canvas:TCanvas read ICanvas;

    procedure Clip(const R:TRectF); override;
    procedure UnClip; override;

    procedure HideBrush; override;

    procedure Init(const DC:HDC);

    procedure SetBrush(const ABrush:TBrush); override;
    procedure SetFont(const AFont:TFont); override;
    procedure SetHorizAlign(const Align:THorizAlign); override;
    procedure SetStroke(const AStroke:TStroke); override;

    procedure Draw(const R:TRectF); override;
    procedure Draw(const P:TPointsF); override;
    procedure Draw(const APicture:TPicture; const X,Y:Single); overload; override;
    procedure Draw(const APicture:TPicture; const R:TRectF); overload; override;
    procedure HorizontalLine(const Y,X0,X1:Single); override;
    procedure Fill(const R:TRectF); override;
    procedure Fill(const P:TPointsF); override;
    procedure Line(const X0,Y0,X1,Y1:Single); override;
    procedure TextOut(const X,Y:Single; const AText:String); override;
    function TextWidth(const AText:String):Single; override;
    procedure VerticalLine(const X,Y0,Y1:Single); override;
  end;

implementation
