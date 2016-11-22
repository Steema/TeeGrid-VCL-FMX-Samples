{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TScrollableControl                     }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Control;

interface

uses
  Messages,
  {VCL.}Controls;

type
  TScrollableControl=class(TCustomControl)
  private
    ScrollBarVisible : Array[0..1] of Boolean;
    ScrollBarThumbs : Array[0..1] of Integer;

    function CalcScroll(const Bar:Word; const Code,Pos:Integer):Single;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;

    function RemainSize(const Bar:Word):Single;
  protected
    function GetMaxBottom:Single; virtual; abstract;
    function GetMaxRight:Single; virtual; abstract;

    function GetScrollX:Single; virtual; abstract;
    function GetScrollY:Single; virtual; abstract;

    function HorizScrollHeight:Integer;
    function VertScrollWidth:Integer;

    procedure ResetScrollBars; virtual;

    procedure SetScrollX(const Value:Single); virtual; abstract;
    procedure SetScrollY(const Value:Single); virtual; abstract;

    procedure UpdateScroll(const Bar:Integer; const Value:Single);
  end;

implementation
