{*********************************************}
{  TeeGrid Software Library                   }
{  Abstract Formatting classes                }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Format;

interface

uses
  {$IFDEF FPC}
  Graphics,
  {$ELSE}
  System.UITypes,
  {$ENDIF}
  System.Classes;

type
  TPersistentChange=class(TPersistent)
  protected
    IChanged : TNotifyEvent;

    procedure ChangeBoolean(var Variable:Boolean; const Value: Boolean);
    procedure ChangeSingle(var Variable:Single; const Value: Single);
    procedure ChangeString(var Variable:String; const Value: String);
  public
    Constructor Create(const AChanged:TNotifyEvent); virtual;
    procedure Changed;
  end;

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
    procedure InitVisible(const Value:Boolean);
  published
    property Visible:Boolean read FVisible write SetVisible stored IsVisibleStored;
  end;

  TPicture=class(TPersistentChange)
  public
    TagObject : TObject;
  end;

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

    function GetColor: TColor;
    function IsSizeStored: Boolean;
    procedure SetBrush(const Value: TBrush);
    procedure SetColor(const Value: TColor);
    procedure SetName(const Value: String);
    procedure SetSize(const Value: Single);
    procedure SetStyle(const Value: TFontStyles);
    function IsNameStored: Boolean;
    function IsStyleStored: Boolean;
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

  TStroke=class(TVisiblePersistentChange)
  private
    FBrush: TBrush;
    FSize: Single;
    FStyle: TStrokeStyle;

    IDefaultStyle : TStrokeStyle;

    function GetColor: TColor;
    function IsSizeStored: Boolean;
    function IsStyleStored: Boolean;
    procedure SetBrush(const Value: TBrush);
    procedure SetColor(const Value: TColor);
    procedure SetSize(const Value: Single);
    procedure SetStyle(const Value: TStrokeStyle);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
    Constructor CreateColor(const AChanged:TNotifyEvent; const AColor:TColor);

    Destructor Destroy; override;

    procedure InitStyle(const Value:TStrokeStyle);
  published
    property Brush:TBrush read FBrush write SetBrush;
    property Color:TColor read GetColor write SetColor stored False;
    property Size:Single read FSize write SetSize stored IsSizeStored;
    property Style:TStrokeStyle read FStyle write SetStyle stored IsStyleStored;
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

    procedure SetAutomatic(const Value: Boolean);
    procedure SetUnits(const Value: TSizeUnits);
    procedure SetValue(const AValue: Single);
  public
    Pixels : Single; // <-- calculated

    Constructor Create(const AChanged:TNotifyEvent); override;

    function Calculate(const ATotal:Single):Single;
    procedure SetPixels(const AValue:Single);
  published
    property Automatic:Boolean read FAutomatic write SetAutomatic default True;
    property Units:TSizeUnits read FUnits write SetUnits default TSizeUnits.Pixels;
    property Value:Single read FValue write SetValue;
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

  {$IFDEF FPC}
  TColors=record
  public
    const
      Aqua=$FFFF00;
      Black=0;
      DkGray=$808080;
      LightGray=$D3D3D3;
      SkyBlue=$87CEEB;
      White=$FFFFFF;
  end;
  {$ENDIF}

implementation
