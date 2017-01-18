unit LinuxTee.Painter;

interface

uses
  XLib,

  System.Types, System.UITypes,

  Tee.Painter, Tee.Format;

type
  TFont=class
  end;

  TBrushKind=(Solid);

  TBrush=class
  private
    FColor : TAlphaColor;
    FKind : TBrushKind;
  public
    Constructor Create(const AKind:TBrushKind; const AColor:TAlphaColor);
  end;

  TStroke=class
  end;

  TCanvasSaveState=record
  end;

  TCanvas=class
  end;

  TCanvasManager=record
  public
    class var
      MeasureCanvas : TCanvas;
  end;

  TCursor=Integer;

  TLinuxPainter=class(TPainter)
  private
    ICanvas : TCanvas;

    FDisplay : PDisplay;
    FWindow : XID;
    GC: TGC;

    IFont : XFontStruct;

    IClipped : Array of TCanvasSaveState;

    {$IFDEF USELAYOUT}
    ILayout : TTextLayout;
    {$ENDIF}

    ISolidBrush : TBrush;

    FFontColor : TColor;
    FHorizAlign : THorizontalAlign;
    FOpacity : Single;
    FVertAlign : TVerticalAlign;

  protected
    procedure SetCanvas(const ACanvas:TCanvas);
  public
    Constructor Create(const ACanvas:TCanvas);
    Destructor Destroy; override;

    class procedure ApplyFont(const ASource:Tee.Format.TFont; const ADest:TFont); static;

    procedure Clear;

    procedure Clip(const R:TRectF); override;
    procedure UnClip; override;

    procedure HideBrush; override;

    procedure SetBrush(const ABrush:Tee.Format.TBrush); override;
    procedure SetFont(const AFont:Tee.Format.TFont); override;
    procedure SetFontColor(const AColor:TColor); override;
    procedure SetHorizontalAlign(const Align:THorizontalAlign); override;
    procedure SetStroke(const AStroke:Tee.Format.TStroke); override;
    procedure SetVerticalAlign(const Align:TVerticalAlign); override;

    procedure Draw(const R:TRectF); override;
    procedure Draw(const P:TPointsF); override;
    procedure Draw(const APicture: TPicture; const X, Y: Single); override;
    procedure Draw(const APicture: TPicture; const R: TRectF); override;
    procedure DrawEllipse(const R:TRectF); override;

    procedure Fill(const R:TRectF); override;
    procedure Fill(const R:TRectF; const AColor:TColor); override;
    procedure Fill(const P:TPointsF); override;
    procedure FillEllipse(const R:TRectF); override;

    procedure HorizontalLine(const Y,X0,X1:Single); override;
    procedure Line(const X0,Y0,X1,Y1:Single); override;
    procedure Lines(const P:TPointsF); override;
    procedure Paint(const AFormat:TFormat; const R:TRectF); override;
    procedure Paint(const AFormat:TFormat; const P:TPointsF); override;
    function TextHeight(const AText:String):Single; override;
    procedure TextOut(const ARect:TRectF; const AText:String); override;
    function TextWidth(const AText:String):Single; override;
    procedure VerticalLine(const X,Y0,Y1:Single); override;

    property Canvas:TCanvas read ICanvas write SetCanvas;
    property Display:PDisplay read FDisplay;
    property Window:XID read FWindow;
  end;

implementation
