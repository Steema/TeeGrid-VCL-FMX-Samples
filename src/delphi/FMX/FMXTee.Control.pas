{*********************************************}
{  TeeGrid Software Library                   }
{  FMX TScrollable control                    }
{  Copyright (c) 2016 by Steema Software      }
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

  FMX.Types, FMX.Controls, Tee.Format;

type
  // Horizontal and Vertical scrollbars
  TScrollBars=class(TVisiblePersistentChange)
  private
    FHorizontal: TScrollBar;
    FVertical: TScrollBar;

    IParent : TControl;

    function Calc(const Horiz:Boolean; const AValue:Single):Single;
    procedure DoChanged(Sender:TObject);
  public
    Constructor CreateParent(const AParent:TControl);

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

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
    procedure ResetScrollBars; virtual;
    procedure SetScrollX(const Value:Single); virtual; abstract;
    procedure SetScrollY(const Value:Single); virtual; abstract;
  public
    Constructor Create(AOwner: TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    property ScrollBars:TScrollBars read FScrollBars;
  end;

implementation
