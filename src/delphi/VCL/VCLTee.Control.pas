{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TScrollableControl                     }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Control;

interface

uses
  Messages, {System.}Classes,
  {VCL.}Controls;

type
  TScrollBarVisible=(Automatic,Yes,No);

  TScrollBars=class;

  TScrollBar=class(TPersistent)
  private
    FVisible : TScrollBarVisible;

    IsVisible : Boolean;
    ThumbPosition : Integer;

    IBars : TScrollBars;

    procedure PrepareVisible(const Needed:Boolean);
    procedure SetVisible(const Value: TScrollBarVisible);
  public
    property Visible : TScrollBarVisible read FVisible write SetVisible default TScrollBarVisible.Automatic;
  end;

  TScrollableControl=class;

  TScrollBars=class(TPersistent)
  private
    FHorizontal,
    FVertical : TScrollBar;
    FVisible : Boolean;

    IControl : TScrollableControl;

    procedure SetVisible(const Value: Boolean);
  public
    Constructor Create(const AControl:TScrollableControl);
    Destructor Destroy; override;

    property Horizontal:TScrollBar read FHorizontal;
    property Vertical:TScrollBar read FVertical;
  published
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  TScrollableControl=class(TCustomControl)
  private
    FScrollBars: TScrollBars;

    function CalcScroll(const Bar:Word; const Code,Pos:Integer):Single;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;

    function RemainSize(const Bar:Word):Single;
    procedure SetScrollBars(const Value: TScrollBars);
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
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    property ScrollBars:TScrollBars read FScrollBars write SetScrollBars;
  end;

implementation
