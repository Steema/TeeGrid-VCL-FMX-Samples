{*********************************************}
{  TeeGrid Software Library                   }
{  FMX TScrollable control                    }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Control;
{$I Tee.inc}

interface

uses
  System.Classes,

  {$IF CompilerVersion>24}
  FMX.StdCtrls,
  {$IFEND}

  FMX.Types, FMX.Controls, Tee.Format, Tee.Control;

type
  TFMXScrollBar=FMX.{$IF CompilerVersion>24}StdCtrls{$ELSE}Controls{$IFEND}.TScrollBar;

  TFMXScrollBars=class
  private
    function Calc(const Horiz:Boolean; const AValue:Single):Single;
    procedure Reset(const W,H,AWidth,AHeight:Single; const AScrollBars:TScrollBars);
    function ThumbSize(const ABar:TFMXScrollBar):Single;
  public
    Horizontal,
    Vertical : TFMXScrollBar;

    Constructor CreateParent(const AParent:TControl);

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}
  end;

  TScrollableControl=class(TStyledControl)
  private
    FScrollBars : TScrollBars;

    procedure DoHorizScroll(Sender:TObject);
    procedure DoResetScrollBars(Sender:TObject);
    procedure DoUpdateScrollPosition(const Horiz:Boolean; const Value:Single);
    procedure DoVertScroll(Sender:TObject);
    function HorizScrollHeight:Single;
    procedure SetScrollBars(const Value: TScrollBars);
    function VertScrollWidth:Single;
  protected
    IFMXScrollBars : TFMXScrollBars;

    function GetMaxBottom:Single; virtual; abstract;
    function GetMaxRight:Single; virtual; abstract;
    function RemainSize(const Horizontal:Boolean):Single;
    procedure ResetScrollBars; virtual;
    procedure SetScrollX(const Value:Single); virtual; abstract;
    procedure SetScrollY(const Value:Single); virtual; abstract;
    procedure UpdateScroll(const Horiz:Boolean; const AValue: Single);
  public
    Constructor Create(AOwner: TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    // Assign

    property ScrollBars:TScrollBars read FScrollBars write SetScrollBars;
  end;

implementation
