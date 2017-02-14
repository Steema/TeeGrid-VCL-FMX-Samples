{*********************************************}
{  TeeGrid Software Library                   }
{  Base abstract Control class                }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Control;
{$I Tee.inc}

interface

uses
  {System.}Classes,

  {$IFNDEF FPC}
  {System.}Types,
  {$ENDIF}

  Tee.Format, Tee.Painter;

type
  TScrollBarVisible=(Automatic,Show,Hide);

  TScrollBars=class;

  // Scroll bar with a Visible property
  TScrollBar=class(TPersistent)
  private
    FThumbSize : Integer;
    FIsVisible : Boolean;
    FVisible : TScrollBarVisible;

    // [Weak]
    IBars : TScrollBars;

    procedure SetVisible(const Value: TScrollBarVisible);
  public
    procedure Assign(Source:TPersistent); override;

    procedure PrepareVisible(const Needed:Boolean);

    property IsVisible:Boolean read FIsVisible write FIsVisible;
    property ThumbSize:Integer read FThumbSize write FThumbSize;
  published
    property Visible : TScrollBarVisible read FVisible write SetVisible
                                         default TScrollBarVisible.Automatic;
  end;

  // Horizontal and Vertical scroll bars
  TScrollBars=class(TPersistent)
  private
    FHorizontal,
    FVertical : TScrollBar;
    FVisible : Boolean;

    IReset : TNotifyEvent;

    procedure SetHorizontal(const Value: TScrollBar);
    procedure SetVertical(const Value: TScrollBar);
    procedure SetVisible(const Value: Boolean);
  public
    const
      VirtualRange=2000; // Range min to max

    Constructor Create(const AOnReset:TNotifyEvent);

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;
  published
    property Horizontal:TScrollBar read FHorizontal write SetHorizontal;
    property Vertical:TScrollBar read FVertical write SetVertical;
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  // Base class of an agnostic Control
  TCustomTeeControl=class(TComponent)
  private
    FBack: TFormat;

    IChanged : TNotifyEvent;

    procedure SetBack(const Value: TFormat);
  protected
    IUpdating : Integer;

    procedure BeginUpdate; inline;
    procedure EndUpdate;

    procedure DoChanged(Sender:TObject);
    procedure ResetScrollBars; virtual;

    property OnChange:TNotifyEvent read IChanged write IChanged;
  public
    Constructor Create(AOwner:TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure Paint; virtual;

    function ClientHeight:Single; virtual;
    function ClientWidth:Single; virtual;
    function Height:Single; virtual; abstract;
    function Painter:TPainter; virtual; abstract;
    function Width:Single; virtual; abstract;
  published
    property Back:TFormat read FBack write SetBack;
  end;

implementation
