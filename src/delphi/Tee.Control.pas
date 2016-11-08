{*********************************************}
{  TeeGrid Software Library                   }
{  Base abstract Control class                }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Control;

interface

uses
  System.Classes,

  {$IFNDEF FPC}
  System.Types,
  {$ENDIF}

  Tee.Format, Tee.Painter;

type
  TCustomTeeControl=class(TComponent)
  private
    FBack: TBrush;

    IChanged : TNotifyEvent;

    procedure SetBack(const Value: TBrush);
  protected
    IUpdating : Integer;

    procedure BeginUpdate; inline;
    procedure EndUpdate;

    function Bounds:TRectF;

    procedure Changed(Sender:TObject);
    property OnChange:TNotifyEvent read IChanged write IChanged;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Paint; virtual;

    function Height:Single; virtual; abstract;
    function Painter:TPainter; virtual; abstract;
    function Width:Single; virtual; abstract;
  published
    property Back:TBrush read FBack write SetBack;
  end;

  TMouseCursor=(Default,HandPoint,HorizResize,VertResize);

implementation
