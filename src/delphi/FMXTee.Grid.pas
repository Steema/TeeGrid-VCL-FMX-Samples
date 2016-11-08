{*********************************************}
{  TeeGrid Software Library                   }
{  FMX TeeGrid                                }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Grid;

interface

uses
  System.Classes, System.Types, System.UITypes,

  FMX.Controls,

  {$IF CompilerVersion<=25}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  FMX.StdCtrls,

  Tee.Grid, Tee.Grid.Base, Tee.Grid.Columns, Tee.Grid.Data,
  Tee.Painter,
  Tee.Format, Tee.Control,
  Tee.Grid.Header, FMXTee.Painter, FMXTee.Control;

{$DEFINE USELAYOUT}

type
  TTeeGrid=class;

  TFMXTeeGrid=class(TCustomTeeGrid)
  private
    IEditor : TControl;
    IGrid : TTeeGrid;

    procedure DoStopEditor(const ChangeValue:Boolean);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
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

    procedure ColumnResized(Sender:TObject);
    procedure DoChanged(Sender:TObject);
    procedure SetDataItem(const Value: TVirtualData);

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
    function GetDataItem: TVirtualData;
    function GetCells: TGridCells;
    function GetIndicator: TIndicator;
    function GetRows: TRows;
    procedure SetCells(const Value: TGridCells);
    procedure SetIndicator(const Value: TIndicator);
    procedure SetRows(const Value: TRows);
    function GetAfterDraw: TNotifyEvent;
    procedure SetAfterDraw(const Value: TNotifyEvent);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);

    procedure ResetScrollBars;
  protected
    procedure DoMouseLeave; override;

    function GetMaxBottom:Single; override;
    function GetMaxRight:Single; override;

    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure Paint; override;

    procedure Resize; override;

    procedure SetScrollX(const Value:Single); override;
    procedure SetScrollY(const Value:Single); override;

    property Painter:TFMXPainter read FPainter;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure ApplyStyle; override;

    property Data:TVirtualData read GetDataItem write SetDataItem;
    property Grid:TFMXTeeGrid read FGrid;
  published
    property Cells:TGridCells read GetCells write SetCells;
    property Columns:TColumns read GetColumns write SetColumns;
    property Header:THeader read GetHeader write SetHeader;
    property Indicator:TIndicator read GetIndicator write SetIndicator;
    property Lines:TGridLines read GetLines write SetLines;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly default True;
    property Rows:TRows read GetRows write SetRows;
    property ScrollBars;
    property Selected:TGridSelection read GetSelected write SetSelected;

    property OnAfterDraw:TNotifyEvent read GetAfterDraw write SetAfterDraw;
    property OnClickedHeader:TNotifyEvent read GetClickedHeader write SetClickedHeader;
    property OnColumnResized:TColumnEvent read FOnColumnResized write FOnColumnResized;

    // inherited
    property Align;
    property Anchors;

    // NO: property Caption;

    property ClipChildren;
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
    property TouchTargetExpansion;
    property ShowHint;
    property TabOrder;

    {$IF CompilerVersion>26}
    property TabStop default True;
    {$IFEND}

    property Touch;
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
    property OnGesture;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

implementation
