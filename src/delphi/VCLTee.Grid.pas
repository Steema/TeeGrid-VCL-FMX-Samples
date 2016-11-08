{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TeeGrid                                }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Grid;

interface

uses
  System.Classes, System.Types,

  Windows, Messages,
  VCL.Controls, VCL.Graphics,

  {$IFDEF FPC}
  LCLType,
  {$ENDIF}
  
  Tee.Control, Tee.Grid, Tee.Grid.Base, Tee.Grid.Columns, Tee.Painter,
  Tee.Format, Tee.Grid.Header, Tee.Grid.Data, VCLTee.Painter, VCLTee.Control;

type
  TTeeGrid=class;

  TVCLTeeGrid=class(TCustomTeeGrid)
  private
    IEditor : TControl;
    IGrid : TTeeGrid;

    procedure DoStopEditor(const ChangeValue:Boolean);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure DataChanged; override;

    function HorizScrollBarHeight:Single; override;
    procedure HorizScrollChanged; override;

    function VertScrollBarWidth:Single; override;
    procedure VertScrollChanged; override;

    procedure StartEditor(const AColumn:TColumn; const ARow:Integer); override;
    procedure StopEditor; override;
  public
    function Height:Single; override;
    function Painter:TPainter; override;
    function Width:Single; override;
  end;

  {$IFNDEF FPC}
  {$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  {$ENDIF}
  {$ENDIF}
  TTeeGrid=class(TScrollableControl)
  private
    FGrid : TVCLTeeGrid;
    FPainter : TPainter;
    FOnColumnResized: TColumnEvent;

    {$IFDEF FPC}
    FParentBack : Boolean;
    {$ENDIF}
    
    procedure ColumnResized(Sender:TObject);
    procedure DataRefresh(Sender:TObject);
    procedure DoChanged(Sender:TObject);
    procedure SetData(const Value: TVirtualData);

    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    function GetColumns: TColumns;
    function GetSelected: TGridSelection;
    procedure SetColumns(const Value: TColumns);
    procedure SetSelected(const Value: TGridSelection);
    function GetHeader: THeader;
    procedure SetHeader(const Value: THeader);
    function GetLines: TGridLines;
    procedure SetLines(const Value: TGridLines);
    function GetClickedHeader: TNotifyEvent;
    procedure SetClickedHeader(const Value: TNotifyEvent);
    function GetIndicator: TIndicator;
    procedure SetIndicator(const Value: TIndicator);
    function GetCells: TGridCells;
    procedure SetCells(const Value: TGridCells);
    function GetData: TVirtualData;
    function GetRows: TRows;
    procedure SetRows(const Value: TRows);
    function GetAfterDraw: TNotifyEvent;
    procedure SetAfterDraw(const Value: TNotifyEvent);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CreateParams(var Params: TCreateParams); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;

    function GetMaxBottom:Single; override;
    function GetMaxRight:Single; override;

    function GetScrollX:Single; override;
    function GetScrollY:Single; override;

    procedure SetScrollX(const Value:Single); override;
    procedure SetScrollY(const Value:Single); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;

    property Painter:TPainter read FPainter;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    property Data:TVirtualData read GetData write SetData;
    property Grid:TVCLTeeGrid read FGrid;
  published
    property Cells:TGridCells read GetCells write SetCells;
    property Color default clWhite;
    property Columns:TColumns read GetColumns write SetColumns;
    property DoubleBuffered default True;
    property Header:THeader read GetHeader write SetHeader;
    property Indicator:TIndicator read GetIndicator write SetIndicator;
    property Lines:TGridLines read GetLines write SetLines;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly default True;
    property Rows:TRows read GetRows write SetRows;
    property Selected:TGridSelection read GetSelected write SetSelected;

    {$IFDEF FPC}
    property TabStop default True;
    {$ELSE}
    {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion>27}
    property TabStop default True;
    {$IFEND}
    {$ENDIF}
    {$ENDIF}

    property OnAfterDraw:TNotifyEvent read GetAfterDraw write SetAfterDraw;
    property OnClickedHeader:TNotifyEvent read GetClickedHeader write SetClickedHeader;
    property OnColumnResized:TColumnEvent read FOnColumnResized write FOnColumnResized;

    // inherited

    property Align;
    property Anchors;

    // NO: property Caption;

    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;

    property DragMode;
    property Enabled;
    property Height;

    {$IFNDEF FPC}
    property Margins;
    property Padding;
    {$ENDIF}

    property ParentBiDiMode;
    property ParentBackground {$IFDEF FPC}:Boolean read FParentBack write FParentBack{$ENDIF};
    property ParentColor;
    {$IFNDEF FPC}
    property ParentDoubleBuffered default False;
    {$ENDIF}
    property ParentShowHint;

    property PopupMenu;
    property ShowHint;
    property TabOrder;

    {$IFNDEF FPC}
    property Touch;
    {$ENDIF}

    property Visible;
    property Width;

    {$IFNDEF FPC}
    {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion>23}
    property StyleElements;
    {$IFEND}
    {$ENDIF}
    {$ENDIF}

    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$IFNDEF FPC}
    property OnCanResize;
    {$ENDIF}

    property OnClick;

    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;

    property OnDblClick;
    property OnDragDrop;

    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;

    property OnEnter;
    property OnExit;
    {$IFNDEF FPC}
    property OnGesture;
    {$ENDIF}

    property OnGetSiteInfo;
    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}

    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation
