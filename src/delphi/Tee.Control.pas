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

  TScrollBar=class(TPersistent)
  private
    FVisible : TScrollBarVisible;

    IBars : TScrollBars;

    procedure SetVisible(const Value: TScrollBarVisible);
  public
    IsVisible : Boolean;
    ThumbSize : Integer;

    procedure PrepareVisible(const Needed:Boolean);

    property Visible : TScrollBarVisible read FVisible write SetVisible
             default TScrollBarVisible.Automatic;
  end;

  TScrollBars=class(TPersistent)
  private
    FHorizontal,
    FVertical : TScrollBar;
    FVisible : Boolean;

    IReset : TNotifyEvent;

    procedure SetVisible(const Value: Boolean);
  public
    const
      VirtualRange=2000;

    Constructor Create(const AOnReset:TNotifyEvent);
    Destructor Destroy; override;

  published
    property Horizontal:TScrollBar read FHorizontal;
    property Vertical:TScrollBar read FVertical;
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

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
    Destructor Destroy; override;

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
