{*********************************************}
{  TeeGrid Software Library                   }
{  FMX TeeGrid                                }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Grid;
{$I Tee.inc}

interface

{
   TTeeGrid control for Firemonkey
}

{$IF CompilerVersion>24}
{$DEFINE USELAYOUT}
{$IFEND}

uses
  System.Classes, System.Types, System.UITypes,

  Tee.GridData.DB, // <--- Forced always (so this unit is not needed at every project "uses")

  FMX.Types, FMX.Controls,

  {$IF CompilerVersion<=25}
  {$DEFINE HASFMX20}
  {$IFEND}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  {$IF CompilerVersion>24}
  FMX.StdCtrls,
  {$IFEND}

  Tee.Painter, Tee.Format, Tee.Renders,

  Tee.Control, Tee.Grid.Columns, Tee.GridData, Tee.Grid.Rows,
  Tee.Grid.RowGroup, Tee.Grid.Header, Tee.Grid.Selection, Tee.Grid.Bands,
  Tee.Grid,

  FMXTee.Painter, FMXTee.Control;

type
  TTeeGrid=class;

  TFMXTeeGrid=class(TCustomTeeGrid)
  private
    FOnTyping : TNotifyEvent;

    IEditor : TControl;
    IGrid : TTeeGrid;

    IEditorColumn : TColumn;
    IEditorRow : Integer;

    function CanHideEditor:Boolean;
    procedure CreateEditor(const AColumn:TColumn);
    procedure DoHideEditor(const CallEvent:Boolean);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    function EditorShowing:Boolean;
    procedure EditorTracking(Sender: TObject);
    procedure SetEditorBounds(const PositionOnly:Boolean);
    procedure TryChangeEditorData;
    procedure TryShowEditor(const AColumn: TColumn; const ARow: Integer; const AutoEdit:String);
  protected
    procedure CancelEditor; override;
    procedure CheckScrollLimits(var X,Y:Single); override;
    procedure DataChanged; override;

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

    property OnTyping:TNotifyEvent read FOnTyping write FOnTyping;
  end;

  TCellEditorEvent=procedure(const Sender:TObject; const AEditor:TControl;
                             const AColumn:TColumn; const ARow:Integer) of object;

  {$IFNDEF FPC}
  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$IFEND}
              {$IF CompilerVersion>=26}or pidAndroid{$IFEND}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$IFEND}
              )]
  {$IFEND}
  {$ENDIF}
  TTeeGrid=class(TScrollableControl)
  private
    FGrid : TFMXTeeGrid;
    FPainter : TFMXPainter;

    FOnColumnResized: TColumnEvent;

    FOnCellEditing,
    FOnCellEdited: TCellEditorEvent;

    IDataSource : TComponent;

    procedure ChangePainter(const APainter:TFMXPainter);
    procedure ColumnResized(Sender:TObject);
    function CurrentRows: TRows;
    procedure SetDataItem(const Value: TVirtualData);

    procedure InitializeCanvas;

    function GetColumns: TColumns;
    function GetScrollX: Single;
    function GetScrollY: Single;
    function GetSelected: TGridSelection;
    procedure SetColumns(const Value: TColumns);
    procedure SetSelected(const Value: TGridSelection);
    function GetHeader: TColumnHeaderBand;
    procedure SetHeader(const Value: TColumnHeaderBand);
    function GetClickedHeader: TNotifyEvent;
    procedure SetClickedHeader(const Value: TNotifyEvent);
    function GetDataItem: TVirtualData;
    function GetIndicator: TIndicator;
    function GetRows: TRows;
    procedure SetIndicator(const Value: TIndicator);
    procedure SetRows(const Value: TRows);
    function GetAfterDraw: TNotifyEvent;
    procedure SetAfterDraw(const Value: TNotifyEvent);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);

    function GetBack: TFormat;
    function GetCells: TTextRender;
    function GetFooter: TGridBands;
    procedure SetBack(const Value: TFormat);
    procedure SetCells(const Value: TTextRender);
    procedure SetFooter(const Value: TGridBands);
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
    function GetScrolling: TGridScrolling;
    procedure SetScrolling(const Value: TGridScrolling);

    function MouseStateFrom(const Button: TMouseButton; const Shift: TShiftState;
                            const X,Y: Single; const AEvent:TGridMouseEvent):TMouseState;

    procedure SetupTouch;
    procedure TryClearColumns;
    procedure TryStartEditing(const P:TPointF);
  protected
    procedure DblClick; override;
    procedure DefineProperties(Filer: TFiler); override;

    procedure DialogKey(var Key: Word; Shift: TShiftState); override;

    procedure DoEnter; override;
    procedure DoExit; override;

    procedure DoMouseLeave; override;

    function GetCursorPos:TPointF;

    function GetMaxBottom:Single; override;
    function GetMaxRight:Single; override;

    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Paint; override;

    procedure ResetScrollBars; override;
    procedure Resize; override;

    procedure SetScrollX(const Value:Single); override;
    procedure SetScrollY(const Value:Single); override;
  public
    Constructor Create(AOwner:TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure ApplyStyle; override;
    procedure Changed(Sender:TObject);

    {$IF CompilerVersion>24}
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    {$IFEND}

    procedure Loaded; override;

    property Data:TVirtualData read GetDataItem write SetDataItem;
    property Grid:TFMXTeeGrid read FGrid;
    property Painter:TFMXPainter read FPainter;
  published
    property Back:TFormat read GetBack write SetBack;
    property Cells:TTextRender read GetCells write SetCells;
    property Columns:TColumns read GetColumns write SetColumns;
    property DataSource:TComponent read GetDataSource write SetDataSource;
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

    property ClipChildren default True;
    property ClipParent;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    property HitTest;
    property Locked;
    property Opacity;
    property Margins;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;

    {$IF CompilerVersion>27}
    property Size;
    {$IFEND}

    {$IF CompilerVersion>23}
    property TouchTargetExpansion;
    {$IFEND}
    
    property ShowHint;
    property TabOrder;

    {$IF CompilerVersion>26}
    property TabStop default True;
    {$IFEND}

    {$IF CompilerVersion>23}
    property Touch;
    {$IFEND}
    
    property Visible;
    property Width;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragEnd;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnEnter;
    property OnExit;

    {$IF CompilerVersion>23}
    property OnGesture;
    {$IFEND}

    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPainting;
    property OnPaint;
    property OnResize;

    {$IFDEF D25}
    property OnResized;
    {$ENDIF}
  end;

implementation
