{*********************************************}
{  TeeGrid Software Library                   }
{  Base abstract Control class                }
{  Copyright (c) 2016 by Steema Software      }
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
  TCustomTeeControl=class(TComponent)
  private
    FBack: TFormat;

    IChanged : TNotifyEvent;

    procedure SetBack(const Value: TFormat);
  protected
    IUpdating : Integer;

    procedure BeginUpdate; inline;
    procedure EndUpdate;

    procedure Changed(Sender:TObject);
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
