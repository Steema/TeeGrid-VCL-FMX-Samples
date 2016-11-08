{*********************************************}
{  TeeGrid Software Library                   }
{  FMX TScrollable control                    }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Control;

interface

uses
  System.Classes,
  FMX.Types, FMX.Controls, FMX.StdCtrls;

type
  TScrollBars=class(TPersistent)
  private
    FHorizontal: TScrollBar;
    FVertical: TScrollBar;

    function Calc(const Horiz:Boolean; const AValue:Single):Single;
  public
    Constructor Create(const AParent:TControl);
    Destructor Destroy; override;

    procedure Reset(const W,H,AWidth,AHeight:Single);
  published
    property Horizontal:TScrollBar read FHorizontal;
    property Vertical:TScrollBar read FVertical;
  end;

  TScrollableControl=class(TStyledControl)
  private
    FScrollBars : TScrollBars;

    procedure DoHorizScroll(Sender:TObject);
    procedure DoVertScroll(Sender:TObject);
  protected
    function GetMaxBottom:Single; virtual; abstract;
    function GetMaxRight:Single; virtual; abstract;
    function RemainSize(const Horizontal:Boolean):Single;
    procedure SetScrollX(const Value:Single); virtual; abstract;
    procedure SetScrollY(const Value:Single); virtual; abstract;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    property ScrollBars:TScrollBars read FScrollBars;
  end;

implementation
