{*********************************************}
{  TeeGrid Software Library                   }
{  Base abstract Painter class                }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Painter;

interface

uses
  System.Classes, System.Types,
  {$IFNDEF FPC}
  System.UITypes,
  {$ENDIF}
  Tee.Format;

type
  {$IFDEF FPC} // 3.0.0 includes TPointF etc at Types.pp ??
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

    function CenterPoint:TPointF;
    function Height:Single; inline;
    function Round:TRect;
    function Width:Single; inline;
  end;
  {$ENDIF}

  THorizAlign=(Left,Center,Right);

  TPointsF=Array of TPointF;

  TPainter=class
  protected
  public
    procedure Clip(const R:TRectF); virtual; abstract;
    procedure UnClip; virtual; abstract;

    procedure HideBrush; virtual; abstract;
    procedure SetBrush(const ABrush:TBrush); virtual; abstract;
    procedure SetFont(const AFont:TFont); virtual; abstract;
    procedure SetHorizAlign(const Align:THorizAlign); virtual; abstract;
    procedure SetStroke(const AStroke:TStroke); virtual; abstract;
    procedure Draw(const R:TRectF); overload; virtual; abstract;
    procedure Draw(const P:TPointsF); overload; virtual; abstract;
    procedure Draw(const APicture:TPicture; const X,Y:Single); overload; virtual; abstract;
    procedure Draw(const APicture:TPicture; const R:TRectF); overload; virtual; abstract;
    procedure HorizontalLine(const Y,X0,X1:Single); virtual; abstract;
    procedure Fill(const R:TRectF); overload; virtual; abstract;
    procedure Fill(const P:TPointsF); overload; virtual; abstract;
    procedure Line(const X0,Y0,X1,Y1:Single); virtual; abstract;
    procedure TextOut(const X,Y:Single; const AText:String); virtual; abstract;
    function TextWidth(const AText:String):Single; virtual; abstract;
    procedure VerticalLine(const X,Y0,Y1:Single); virtual; abstract;

    function TryClip(const AText:String;
                     const X,Y,AWidth,AHeight,AMinX:Single):Boolean; overload;

    function TryClip(const AText:String;
                     const ARect:TRectF;
                     const AMinX:Single):Boolean; overload;

    procedure TryPaint(const AFormat:TFormat; const R:TRectF); overload;
    procedure TryPaint(const AFormat:TFormat; const P:TPointsF); overload;
  end;

implementation
