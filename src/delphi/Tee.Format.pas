{*********************************************}
{  TeeGrid Software Library                   }
{  Abstract Formatting classes                }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Format;
{$I Tee.inc}
{$SCOPEDENUMS ON}

{
   Basic agnostic classes for typical formatting properties:

   TFont
   TStroke (pen)
   TBrush
   TPicture
   TGradient

   and other small classes derived from TPersistent to reuse them in
   higher-level containers:

   TFormat (Brush and Stroke)

   TVisibleFormat (TFormat and Visible:Boolean)

   TTextFormat  (TFormat and TFont)

   TCoordinate  (Value:Single and Units: pixels or % )


   Needs: (none)

}

interface

uses
  {$IFDEF FPC}
  Graphics,
  {$ELSE}
  {$IFNDEF NOUITYPES}
  System.UITypes,
  {$ENDIF}
  {$ENDIF}
  {System.}Classes;

type
  TPersistentChange=class(TPersistent)
  protected
    IChanged : TNotifyEvent;

    procedure ChangeBoolean(var Variable:Boolean; const Value: Boolean);
    procedure ChangeInteger(var Variable: Integer; const Value: Integer);
    procedure ChangeSingle(var Variable:Single; const Value: Single);
    procedure ChangeString(var Variable:String; const Value: String);
  public
    Constructor Create(const AChanged:TNotifyEvent); virtual;

    procedure Changed;
  end;

  {$IFNDEF FPC}
  {$IF CompilerVersion>22}
  TColor=TAlphaColor;
  {$IFEND}
  {$ENDIF}

  TGradient=class(TPersistentChange)
  public
    procedure Assign(Source:TPersistent); override;
  published
  end;

  TVisiblePersistentChange=class(TPersistentChange)
  private
    FVisible: Boolean;

    IDefaultVisible : Boolean;

    procedure SetVisible(const Value: Boolean);
    function IsVisibleStored: Boolean;
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure Assign(Source:TPersistent); override;
    procedure Hide;
    procedure InitVisible(const Value:Boolean);
    procedure Show;
  published
    property Visible:Boolean read FVisible write SetVisible stored IsVisibleStored;
  end;

  TPicture=class(TPersistentChange)
  public
    TagObject : TObject;
  end;

  {$IFDEF FPC}
  TColor=Cardinal;
  {$ELSE}

  {$IF CompilerVersion<=22}
  TColor=Cardinal;
  TFontStyle=(fsBold, fsItalic, fsUnderline, fsStrikeOut);
  TFontStyles=set of TFontStyle;
  {$IFEND}

  {$ENDIF}

  TBrush=class(TVisiblePersistentChange)
  private
    FGradient: TGradient;
    FColor: TColor;

    IDefaultColor : TColor;

    function IsColorStored:Boolean;
    procedure SetColor(const Value: TColor);
    procedure SetGradient(const Value: TGradient);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure InitColor(const AColor:TColor);
  published
    property Color:TColor read FColor write SetColor stored IsColorStored;
    property Gradient:TGradient read FGradient write SetGradient;
  end;

  TFont=class(TPersistentChange)
  private
    FBrush : TBrush;
    FName: String;
    FSize: Single;
    FStyle : TFontStyles;

    function GetColor: TColor; inline;
    function IsNameStored: Boolean;
    function IsSizeStored: Boolean;
    function IsStyleStored: Boolean;
    procedure SetBrush(const Value: TBrush);
    procedure SetColor(const Value: TColor); inline;
    procedure SetName(const Value: String);
    procedure SetSize(const Value: Single);
    procedure SetStyle(const Value: TFontStyles);
  public
    class var
      DefaultSize : Single;
      DefaultName : String;

    Constructor Create(const AChanged:TNotifyEvent); override;
    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure InitColor(const AColor:TColor);
  published
    property Brush:TBrush read FBrush write SetBrush;
    property Color:TColor read GetColor write SetColor stored False;
    property Name:String read FName write SetName stored IsNameStored;
    property Size:Single read FSize write SetSize stored IsSizeStored;
    property Style:TFontStyles read FStyle write SetStyle stored IsStyleStored;
  end;

  TStrokeStyle=(Solid,Dash,Dot,DashDot,DashDotDot,Custom);

  TStrokeEnd=(Round,Square,Flat);
  TStrokeJoin=(Round,Bevel,Mitter);

  TStroke=class(TVisiblePersistentChange)
  private
    FBrush: TBrush;
    FSize: Single;
    FStyle: TStrokeStyle;

    IDefaultStyle : TStrokeStyle;
    FJoin: TStrokeJoin;
    FEnd: TStrokeEnd;

    function GetColor: TColor; inline;
    function IsSizeStored: Boolean;
    function IsStyleStored: Boolean;
    procedure SetBrush(const Value: TBrush);
    procedure SetColor(const Value: TColor);
    procedure SetSize(const Value: Single);
    procedure SetStyle(const Value: TStrokeStyle);
    procedure SetEnd(const Value: TStrokeEnd);
    procedure SetJoin(const Value: TStrokeJoin);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
    Constructor CreateColor(const AChanged:TNotifyEvent; const AColor:TColor);

    Destructor Destroy; override;

    procedure Assign(Source:TPersistent); override;

    procedure InitStyle(const Value:TStrokeStyle);
  published
    property Brush:TBrush read FBrush write SetBrush;
    property Color:TColor read GetColor write SetColor stored False;
    property EndStyle:TStrokeEnd read FEnd write SetEnd default TStrokeEnd.Flat;
    property JoinStyle:TStrokeJoin read FJoin write SetJoin default TStrokeJoin.Round;
    property Size:Single read FSize write SetSize stored IsSizeStored;
    property Style:TStrokeStyle read FStyle write SetStyle stored IsStyleStored;
  end;

  // TStroke with default Visible=False
  THiddenStroke=class(TStroke)
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
  published
    property Visible default False;
  end;

  TFormat=class(TPersistentChange)
  private
    FBrush: TBrush;
    FStroke: TStroke;

    procedure SetBrush(const Value: TBrush);
    procedure SetStroke(const Value: TStroke);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
    Destructor Destroy; override;
  published
    property Brush:TBrush read FBrush write SetBrush;
    property Stroke:TStroke read FStroke write SetStroke;
  end;

  TSizeUnits=(Pixels,Percent);

  TCoordinate=class(TPersistentChange)
  private
    FAutomatic : Boolean;
    FUnits : TSizeUnits;
    FValue : Single;

    DefaultValue : Single;

    procedure SetAutomatic(const Value: Boolean);
    procedure SetUnits(const Value: TSizeUnits);
    procedure SetValue(const AValue: Single);
    function IsValueStored: Boolean;
  public
    Pixels : Single; // <-- calculated

    Constructor Create(const AChanged:TNotifyEvent); override;

    function Calculate(const ATotal:Single):Single;
    procedure InitValue(const AValue:Single);
    procedure Prepare(const ATotal:Single);
  published
    property Automatic:Boolean read FAutomatic write SetAutomatic default True;
    property Units:TSizeUnits read FUnits write SetUnits default TSizeUnits.Pixels;
    property Value:Single read FValue write SetValue stored IsValueStored;
  end;

  TVisibleFormat=class(TFormat)
  private
    FVisible : Boolean;
    DefaultVisible : Boolean;
    function IsVisibleStored: Boolean;
    procedure SetVisible(const Value: Boolean);
  public
    procedure InitVisible(const Value:Boolean);
  published
    property Visible:Boolean read FVisible write SetVisible stored IsVisibleStored;
  end;

  TTextFormat=class(TFormat)
  private
    FFont: TFont;

    procedure SetFont(const Value: TFont);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
    Destructor Destroy; override;
  published
    property Font:TFont read FFont write SetFont;
  end;

  {$IFDEF NOUITYPES}
  TColors=record
  public
    const
      Aqua=$FFFF00;
      Black=0;
      Cream=$F0FBFF;
      DkGray=$808080;
      LightGray=$D3D3D3;
      SkyBlue=$87CEEB;
      White=$FFFFFF;
  end;
  {$ENDIF}

implementation
