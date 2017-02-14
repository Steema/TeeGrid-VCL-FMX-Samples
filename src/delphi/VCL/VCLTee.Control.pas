{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TScrollableControl                     }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Control;
{$I Tee.inc}

interface

uses
  Messages, {System.}Classes,
  {VCL.}Controls, {VCL.}Forms, Tee.Control;

type
  TScrollableControl=class(TCustomControl)
  private
    FScrollBars: TScrollBars;

    function AvailableWidth:Single;
    function AvailableHeight:Single;

    function CalcScroll(const Bar:Word; const Code,Pos:Integer):Single;
    procedure DoResetScrollBars(Sender:TObject);
    procedure DoSetScrollInfo(const ABar:Word; const APos:Integer);
    procedure DoUpdateScrollPosition(const Bar:Integer; const Value:Single);
    function GetBar(const ABar: Word): TScrollBar;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;

    procedure SetScrollBars(const Value: TScrollBars);
  protected
    function GetMaxBottom:Single; virtual; abstract;
    function GetMaxRight:Single; virtual; abstract;

    function GetScrollX:Single; virtual; abstract;
    function GetScrollY:Single; virtual; abstract;

    function HorizScrollHeight:Integer;
    function VertScrollWidth:Integer;

    function RemainSize(const Horizontal:Boolean):Single;
    procedure ResetScrollBars; virtual;

    procedure SetScrollX(const Value:Single); virtual; abstract;
    procedure SetScrollY(const Value:Single); virtual; abstract;

    procedure UpdateScroll(const Bar:Integer; const Value:Single);
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    property ScrollBars:TScrollBars read FScrollBars write SetScrollBars;
  end;

implementation
