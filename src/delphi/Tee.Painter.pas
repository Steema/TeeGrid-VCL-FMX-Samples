{*********************************************}
{  TeeGrid Software Library                   }
{  Base abstract Painter class                }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Painter;
{$I Tee.inc}
{$SCOPEDENUMS ON}

{
   Abstract TPainter class.

   TPainter provides basic methods to draw and fill shapes and text.

   VCLTee.Painter and FMXTee.Painter units inherit from TPainter class to
   implement the actual drawing for VCL and Firemonkey frameworks.

   Needs: Tee.Format
}

interface

uses
  {System.}Classes, {System.}Types,

  {$IFNDEF FPC}
  {$IFNDEF NOUITYPES}
  System.UITypes,
  {$ENDIF}
  {$ENDIF}

  Tee.Format;

type
  {$IFDEF NOUITYPES}
  TPointF=record
  public
    X,Y : Single;

    Constructor Create(const AX,AY:Single);
  end;

  TRectF=record
  public
    Left,
    Top,
    Right,
    Bottom : Single;

    Constructor Create(const ALeft,ATop,ARight,ABottom:Single); overload;
    Constructor Create(const ATopLeft:TPointF; const AWidth,AHeight:Single); overload;
    Constructor Create(const ARect:TRect); overload;

    function CenterPoint:TPointF;
    function Contains(const P:TPointF):Boolean;
    procedure Inflate(const X,Y:Single);
    function Height:Single; inline;
    function Round:TRect;
    function Width:Single; inline;
  end;
  {$ENDIF}

  THorizontalAlign=(Left,Center,Right);
  TVerticalAlign=(Top,Center,Bottom);

  TPointsF=Array of TPointF;

  TPainter=class
  protected
  public
    procedure Clip(const R:TRectF); overload; virtual; abstract;
    procedure UnClip; virtual; abstract;

    procedure HideBrush; virtual; abstract;

    procedure SetBrush(const ABrush:TBrush); virtual; abstract;
    procedure SetFont(const AFont:TFont); virtual; abstract;
    procedure SetHorizontalAlign(const Align:THorizontalAlign); virtual; abstract;
    procedure SetStroke(const AStroke:TStroke); virtual; abstract;
    procedure SetVerticalAlign(const Align:TVerticalAlign); virtual; abstract;

    procedure Draw(const R:TRectF); overload; virtual; abstract;
    procedure Draw(const P:TPointsF); overload; virtual; abstract;
    procedure Draw(const APicture:TPicture; const X,Y:Single); overload; virtual; abstract;
    procedure Draw(const APicture:TPicture; const R:TRectF); overload; virtual; abstract;
    procedure DrawEllipse(const R:TRectF); virtual; abstract;

    procedure Fill(const R:TRectF); overload; virtual; abstract;
    procedure Fill(const R:TRectF; const AColor:TColor); overload; virtual; abstract;
    procedure Fill(const P:TPointsF); overload; virtual; abstract;
    procedure FillEllipse(const R:TRectF); virtual; abstract;

    procedure HorizontalLine(const Y,X0,X1:Single); virtual; abstract;
    procedure Line(const X0,Y0,X1,Y1:Single); virtual; abstract;
    procedure Lines(const P:TPointsF); virtual; abstract;
    procedure Paint(const AFormat:TFormat; const R:TRectF); overload; virtual; abstract;
    procedure Paint(const AFormat:TFormat; const P:TPointsF); overload; virtual; abstract;
    function TextHeight(const AText:String):Single; virtual; abstract;
    procedure TextOut(const X,Y:Single; const AText:String); virtual; abstract;
    function TextWidth(const AText:String):Single; virtual; abstract;
    procedure VerticalLine(const X,Y0,Y1:Single); virtual; abstract;

    function TryClip(const AText:String;
                     const X,Y,AWidth,AHeight,AMinX:Single):Boolean; overload;

    function TryClip(const AText:String;
                     const ARect:TRectF;
                     const AMinX:Single):Boolean; overload;
  end;

implementation
