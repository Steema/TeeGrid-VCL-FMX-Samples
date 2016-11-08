unit Tee.Renders;

interface

uses
  System.Classes,

  {$IFNDEF FPC}
  System.Types,
  {$ENDIF}

  Tee.Format, Tee.Painter;

type
  TFormatRender=class(TPersistentChange)
  private
    FFormat : TTextFormat;

    function GetFormat: TTextFormat;
    procedure SetFormat(const Value: TTextFormat);
  public
    Destructor Destroy; override;

    function HasFormat:Boolean; inline;
  published
    property Format:TTextFormat read GetFormat write SetFormat;
  end;

  TTextRender=class(TFormatRender)
  private
    function IsHorizAlignStored: Boolean;
    procedure SetHorizAlign(const Value: THorizAlign);
    procedure SetText(const Value: String);
  protected
    DefaultHorizAlign : THorizAlign;

    FHorizAlign: THorizAlign;
    FText : String;
  public
    const
      Separation=3; // <-- change to TCoordinate

    procedure Paint(const APainter:TPainter; const ARect:TRectF); virtual;
  published
    property HorizAlign:THorizAlign read FHorizAlign write SetHorizAlign stored IsHorizAlignStored;
    property Text:String read FText write SetText;
  end;

  TVisibleFormatRender=class(TFormatRender)
  private
    FVisible : Boolean;
    procedure SetVisible(const Value: Boolean);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
  published
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  TVisibleTextRender=class(TTextRender)
  private
    FVisible : Boolean;
    procedure SetVisible(const Value: Boolean);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
  published
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  TBooleanRenderStyle=(Check,Text);

  TBooleanRender=class(TTextRender)
  private
    FCheckFormat : TFormat;
    FStyle: TBooleanRenderStyle;

    procedure SetStyle(const Value: TBooleanRenderStyle);
    procedure SetCheckFormat(const Value: TFormat);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
    Destructor Destroy; override;

    procedure Paint(const APainter:TPainter; const ARect:TRectF); override;
  published
    property CheckFormat:TFormat read FCheckFormat write SetCheckFormat;
    property Style:TBooleanRenderStyle read FStyle write SetStyle default TBooleanRenderStyle.Check;
  end;

implementation
