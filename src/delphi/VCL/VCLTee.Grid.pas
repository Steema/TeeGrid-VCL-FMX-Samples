{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TeeGrid                                }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Grid;
{$I Tee.inc}

interface

uses
  {System.}Classes,

  {$IFNDEF FPC}
  {System.}Types,
  {$ENDIF}

  Tee.Grid.Data.DB, // <--- Forced always (so this unit is not needed at every project "uses")

  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF}
  {VCL.}Controls, {VCL.}Graphics, {VCL.}ExtCtrls, {VCL.}Forms,

  {$IFDEF FPC}
  LCLType, LCLIntf, LMessages,
  {$ENDIF}

  Tee.Control, Tee.Grid, Tee.Grid.Columns, Tee.Painter, Tee.Renders,
  Tee.Format, Tee.Grid.Header, Tee.Grid.Data, Tee.Grid.Bands,
  Tee.Grid.Selection, Tee.Grid.Rows, Tee.Grid.RowGroup,

  VCLTee.Painter, VCLTee.Control;

type
  TTeeGrid=class;

  TVCLTeeGrid=class(TCustomTeeGrid)
  private
    IEditor : TControl;
    IGrid : TTeeGrid;

    IEditorColumn : TColumn;
    IEditorRow : Integer;

    function CanHideEditor:Boolean;
    procedure CreateEditor(const AColumn:TColumn);
    procedure DoHideEditor(const CallEvent:Boolean);
    procedure EditorKeyUp(Sender: TObject; var AKey: Word; Shift: TShiftState);
    function EditorShowing:Boolean;
    procedure SetEditorBounds(const PositionOnly:Boolean);
    procedure TryChangeEditorData;
    procedure TryShowEditor(const AColumn:TColumn; const ARow:Integer; const AutoEdit:String);
  protected
    procedure CancelEditor; override;
    procedure DataChanged; override;

    procedure CheckScrollLimits(var X,Y:Single); override;

    function HorizScrollBarHeight:Single; override;
    procedure HorizScrollChanged(Sender:TObject); override;

    procedure StartEditor(const AColumn:TColumn; const ARow:Integer;
                          const AutoEdit:String=''); override;
    procedure StopEditor; override;

    function VertScrollBarWidth:Single; override;
    procedure VertScrollChanged(Sender:TObject); override;
  public
    procedure Copy(const ASelection:TGridSelection=nil); override;
    function Height:Single; override;
    function Painter:TPainter; override;
    procedure PasteSelected; override;
    function Width:Single; override;
  end;

  TCellEditorEvent=procedure(const Sender:TObject; const AEditor:TControl;
                             const AColumn:TColumn; const ARow:Integer) of object;

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

    FOnCellEditing,
    FOnCellEdited: TCellEditorEvent;

    {$IFDEF FPC}
    FParentBack : Boolean;
    {$ENDIF}

    ICanvas : TControlCanvas;
    IDataSource : TComponent;

    procedure ChangePainter(const Value: TPainter);
    procedure ColumnResized(Sender:TObject);
    procedure DoPaint(const DC:HDC);
    function MouseStateFrom(const Button: TMouseButton; const Shift: TShiftState;
                            const X,Y: Integer; const AEvent:TGridMouseEvent):TMouseState;
    procedure ProcessWheel(const AKey:Word);
    procedure SetData(const Value: TVirtualData);

    {$IFDEF MSWINDOWS}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    {$ENDIF}

    {$IFDEF FPC}
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    {$ELSE}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$ENDIF}

    function GetColumns: TColumns;
    function GetSelected: TGridSelection;
    procedure SetColumns(const Value: TColumns);
    procedure SetSelected(const Value: TGridSelection);
    function GetHeader: TColumnHeaderBand;
    procedure SetHeader(const Value: TColumnHeaderBand);
    function GetClickedHeader: TNotifyEvent;
    procedure SetClickedHeader(const Value: TNotifyEvent);
    function GetIndicator: TIndicator;
    procedure SetIndicator(const Value: TIndicator);
    function CurrentRows: TRows; inline;
    function GetData: TVirtualData;
    function GetRows: TRows;
    procedure SetRows(const Value: TRows);
    function GetAfterDraw: TNotifyEvent;
    procedure SetAfterDraw(const Value: TNotifyEvent);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetBack: TFormat;
    procedure SetBack(const Value: TFormat);
    procedure SetPainter(const Value: TPainter);
    function GetFooter: TGridBands;
    procedure SetFooter(const Value: TGridBands);
    function GetCells: TTextRender;
    procedure SetCells(const Value: TTextRender);
    function GetEditing: TGridEditing;
    procedure SetEditing(const Value: TGridEditing);
    function GetOnNewDetail: TNewDetailEvent;
    procedure SetOnNewDetail(const Value: TNewDetailEvent);
    function GetOnSelect: TNotifyEvent;
    procedure SetOnSelect(const Value: TNotifyEvent);
    function GetHeaders: TGridBands;
    procedure SetHeaders(const Value: TGridBands);
    function GetDataSource: TComponent;
    procedure SetDataSource(const Value: TComponent);

    procedure TryClearColumns;

    procedure ReadPainter(Reader: TReader);
    procedure WritePainter(Writer: TWriter);
    function GetScrolling: TGridScrolling;
    procedure SetScrolling(const Value: TGridScrolling);
  protected
    {$IFDEF FPC}
    procedure MouseLeave; override;
    {$ELSE}
    {$IFDEF MSWINDOWS}
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    {$ENDIF}
    {$ENDIF}

    procedure CreateParams(var Params: TCreateParams); override;

    procedure DblClick; override;

    procedure DefineProperties(Filer: TFiler); override;

    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;

    function GetMaxBottom:Single; override;
    function GetMaxRight:Single; override;

    function GetScrollX:Single; override;
    function GetScrollY:Single; override;

    procedure Loaded; override;

    procedure SetScrollX(const Value:Single); override;
    procedure SetScrollY(const Value:Single); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure PaintWindow(DC: HDC); override;

    procedure ResetScrollBars; override;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure DoChanged(Sender:TObject);

    property Canvas:TControlCanvas read ICanvas;
    property Data:TVirtualData read GetData write SetData;
    property Grid:TVCLTeeGrid read FGrid;
    property Painter:TPainter read FPainter write SetPainter;
  published
    property Back:TFormat read GetBack write SetBack;
    property Cells:TTextRender read GetCells write SetCells;
    property Color default clWhite;
    property Columns:TColumns read GetColumns write SetColumns;
    property DataSource:TComponent read GetDataSource write SetDataSource;
    property DoubleBuffered default True;
    property Editing:TGridEditing read GetEditing write SetEditing;
    property Header:TColumnHeaderBand read GetHeader write SetHeader;
    property Headers:TGridBands read GetHeaders write SetHeaders stored False;
    property Footer:TGridBands read GetFooter write SetFooter stored False;
    property Indicator:TIndicator read GetIndicator write SetIndicator;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly default True;
    property Rows:TRows read GetRows write SetRows;
    property ScrollBars;
    property Scrolling:TGridScrolling read GetScrolling write SetScrolling;
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
    property OnCellEditing:TCellEditorEvent read FOnCellEditing write FOnCellEditing;
    property OnCellEdited:TCellEditorEvent read FOnCellEdited write FOnCellEdited;
    property OnClickedHeader:TNotifyEvent read GetClickedHeader write SetClickedHeader;
    property OnColumnResized:TColumnEvent read FOnColumnResized write FOnColumnResized;
    property OnNewDetail:TNewDetailEvent read GetOnNewDetail write SetOnNewDetail;
    property OnSelect:TNotifyEvent read GetOnSelect write SetOnSelect;

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

    {$IFDEF HASTOUCHPROPERTY}
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
    {$IFDEF HASONGESTURE}
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
