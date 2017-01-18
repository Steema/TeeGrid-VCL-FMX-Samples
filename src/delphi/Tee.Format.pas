{*********************************************}
{  TeeGrid Software Library                   }
{  Abstract Formatting classes                }
{  Copyright (c) 2016-2017 by Steema Software }
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

    property OnChange:TNotifyEvent read IChanged write IChanged;
  end;

  // Helper TCollection with OnChanged event
  TCollectionChange=class(TOwnedCollection)
  private
    FOnAdded,
    FOnChanged,
    FOnRemoved : TNotifyEvent;
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    procedure DoChanged(Sender:TObject);
  published
    property OnAdded:TNotifyEvent read FOnAdded write FOnAdded;
    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
    property OnRemoved:TNotifyEvent read FOnRemoved write FOnRemoved;
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

  {$IFDEF FPC}
  TColor=Graphics.TColor;  // Graphics.TColor=TGraphicsColor (for design-time Inspector support)
  {$ELSE}
  {$IF CompilerVersion>22}
  TColor=TAlphaColor;
  {$ELSE}
  TColor=Cardinal;
  {$IFEND}
  {$ENDIF}

  TColorOffset=Single;

  TGradientColor=class(TCollectionItem)
  private
    FColor: TColor;
    FOffset: TColorOffset;

    procedure DoChanged;
    procedure SetColor(const Value: TColor);
    procedure SetOffset(const Value: TColorOffset);
  public
    procedure Assign(Source:TPersistent); override;
  published
    property Color:TColor read FColor write SetColor;
    property Offset:TColorOffset read FOffset write SetOffset;
  end;

  TGradientColors=class(TCollectionChange)
  private
    function Get(const Index: Integer): TGradientColor; {$IFNDEF FPC}inline;{$ENDIF}
    procedure Put(const Index: Integer; const Value: TGradientColor); {$IFNDEF FPC}inline;{$ENDIF}
  public
    function Add:TGradientColor; overload; {$IFNDEF FPC}inline;{$ENDIF}
    function Add(const AColor:TColor; const AOffset:TColorOffset):TGradientColor; overload;

    property Items[const Index:Integer]:TGradientColor read Get write Put; default;
  end;

  TAngle=Single; // 0..360

  TGradientDirection=(Vertical,Horizontal,Diagonal,BackDiagonal,Radial);

  TGradient=class(TVisiblePersistentChange)
  private
    FAngle: TAngle;
    FColors: TGradientColors;
    FDirection: TGradientDirection;
    FInverted: Boolean;

    function IsAngleStored:Boolean;
    function IsColorsStored: Boolean;
    procedure SetAngle(const Value: TAngle);
    procedure SetColors(const Value: TGradientColors);
    procedure SetDirection(const Value: TGradientDirection);
    procedure SetInverted(const Value: Boolean);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure InitColors(const AColor0,AColor1:TColor);
  published
    property Angle:TAngle read FAngle write SetAngle stored IsAngleStored;
    property Colors:TGradientColors read FColors write SetColors stored IsColorsStored;

    property Direction:TGradientDirection read FDirection write SetDirection
                              default TGradientDirection.Vertical;

    property Inverted:Boolean read FInverted write SetInverted default False;
  end;

  TPicture=class(TPersistentChange)
  protected
    Internal,
    Original  : TObject;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    TagObject : TObject;

    procedure FreeInternal;
  public
    Stretch : Boolean;

    Constructor Create(const AChanged:TNotifyEvent); override;

    Destructor Destroy; override;

    procedure Clear;
    procedure SetGraphic(const AObject:TObject);
  end;

  {$IFNDEF FPC}

  {$IF CompilerVersion<=22}
  TFontStyle=(fsBold, fsItalic, fsUnderline, fsStrikeOut);
  TFontStyles=set of TFontStyle;
  {$IFEND}

  {$ENDIF}

  TColorHelper=record
  public
    class function From(const AColor:TColor; const AOpacity:Single):TColor; static;
    class function Opacity(const AColor: TColor):Byte; inline; static;
    class function RemoveOpacity(const AColor: TColor):TColor; inline; static;
    class function Split(const AColor:TColor; out AOpacity:Single):TColor; static;
    class function Swap(const AColor:TColor):TColor; static;
    class function SwapCheck(const AColor:TColor):TColor; static;
  end;

  TBrush=class(TVisiblePersistentChange)
  private
    FColor: TColor;
    FGradient: TGradient;
    FPicture: TPicture;

    IDefaultColor : TColor;

    function GetGradient:TGradient;
    function GetPicture: TPicture;

    function IsColorStored:Boolean;
    function IsGradientStored: Boolean; inline;
    function IsPictureStored: Boolean;

    procedure SetColor(const Value: TColor);
    procedure SetGradient(const Value: TGradient);
    procedure SetPicture(const Value: TPicture);
  protected
    function HasGradient:Boolean; inline;
    function HasPicture: Boolean; inline;
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure InitColor(const AColor:TColor);
  published
    property Color:TColor read FColor write SetColor stored IsColorStored;
    property Gradient:TGradient read GetGradient write SetGradient stored IsGradientStored;
    property Picture:TPicture read GetPicture write SetPicture stored IsPictureStored;
  end;

  TFont=class(TPersistentChange)
  private
    FBrush : TBrush;
    FName: String;
    FSize: Single;
    FStyle : TFontStyles;

    DefaultStyle : TFontStyles;

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

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure InitColor(const AColor:TColor);
    procedure InitStyle(const AStyle:TFontStyles);
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
    FEnd: TStrokeEnd;
    FJoin: TStrokeJoin;
    FSize: Single;
    FStyle: TStrokeStyle;

    IDefaultStyle : TStrokeStyle;

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

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure InitStyle(const Value:TStrokeStyle);
  published
    property Brush:TBrush read FBrush write SetBrush;
    property Color:TColor read GetColor write SetColor stored False;
    property EndStyle:TStrokeEnd read FEnd write SetEnd default TStrokeEnd.Flat;
    property JoinStyle:TStrokeJoin read FJoin write SetJoin default TStrokeJoin.Mitter;
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

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    function ShouldPaint:Boolean;
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

    DefaultAutomatic : Boolean;
    DefaultValue : Single;

    function IsAutomaticStored: Boolean;
    function IsValueStored: Boolean;
    procedure SetAutomatic(const Value: Boolean);
    procedure SetUnits(const Value: TSizeUnits);
    procedure SetValue(const AValue: Single);
  public
    Pixels : Single; // <-- calculated

    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure Assign(Source:TPersistent); override;

    function Calculate(const ATotal:Single):Single;
    procedure InitValue(const AValue:Single);
    procedure Prepare(const ATotal:Single);
  published
    property Automatic:Boolean read FAutomatic write SetAutomatic stored IsAutomaticStored;
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
    procedure Hide;
    procedure InitVisible(const Value:Boolean);
    procedure Show;
  published
    property Visible:Boolean read FVisible write SetVisible stored IsVisibleStored;
  end;

  TTextFormat=class(TFormat)
  private
    FFont: TFont;

    procedure SetFont(const Value: TFont);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;
  published
    property Font:TFont read FFont write SetFont;
  end;

  {$IFDEF NOUITYPES}
  TColors=record
  public
  const
    {$IFDEF FPC}
    Aqua=clAqua;
    Black=clBlack;
    Cream=clCream;
    DkGray=clDkGray;
    DarkGray=DkGray;
    Green=clGreen;
    LightGray=clLtGray;
    Lime=$00FF00;
    Navy=clNavy;
    Pink=$CBC0FF;
    Red=clRed;
    SkyBlue=clSkyBlue;
    White=clWhite;
    {$ELSE}
    Aqua=$FFFF00;
    Black=0;
    Cream=$F0FBFF;
    DkGray=$808080;
    DarkGray=DkGray;
    Green=$008000;
    LightGray=$D3D3D3;
    Lime=$00FF00;
    Navy=$800000;
    Pink=$CBC0FF;
    Red=$800000;
    SkyBlue=$87CEEB;
    White=$FFFFFF;
    {$ENDIF}
  end;
  {$ENDIF}

  // From TeeBI BI.UI.Colors.pas
  TUIColor=record
  public
    class function Interpolate(const AOld,ANew:TColor; const APercent:Single):TColor; static;
  end;

implementation
