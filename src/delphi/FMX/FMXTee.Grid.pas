{*********************************************}
{  TeeGrid Software Library                   }
{  FMX TeeGrid                                }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Grid;
{$I Tee.inc}

//{$IF CompilerVersion>23}
//{$LEGACYIFEND ON}
//{$IFEND}

interface

{
   TTeeGrid control for Firemonkey
}

{$IF CompilerVersion>24}
{$DEFINE USELAYOUT}
{$IFEND}

uses
  System.Classes, System.Types, System.UITypes,

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

  Tee.Control, Tee.Grid.Columns, Tee.Grid.Data, Tee.Grid.Rows,
  Tee.Grid.RowGroup, Tee.Grid.Header, Tee.Grid.Selection, Tee.Grid.Bands,
  Tee.Grid,

  FMXTee.Painter, FMXTee.Control;

type
  TTeeGrid=class;

  TFMXTeeGrid=class(TCustomTeeGrid)
  private
    IEditor : TControl;
    IGrid : TTeeGrid;

    IEditorColumn : TColumn;
    IEditorRow : Integer;

    procedure CreateEditor(const AColumn:TColumn);
    procedure EditorKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    function EditorShowing:Boolean;
    procedure TryChangeEditorData;
    procedure TryShowEditor(const AColumn: TColumn; const ARow: Integer);
  protected
    procedure DataChanged; override;

    function HorizScrollBarHeight:Single; override;
    procedure HorizScrollChanged; override;

    procedure StartEditor(const AColumn:TColumn; const ARow:Integer); override;
    procedure StopEditor; override;

    function VertScrollBarWidth:Single; override;
    procedure VertScrollChanged; override;
  public
    procedure CopySelected; override;
    function Height:Single; override;
    function Painter:TPainter; override;
    function Width:Single; override;
  end;

  TShowEditorEvent=procedure(const Sender:TObject; const AEditor:TControl;
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
    FOnShowEditor: TShowEditorEvent;

    procedure ChangePainter(const APainter:TFMXPainter);
    procedure ColumnResized(Sender:TObject);
    procedure SetDataItem(const Value: TVirtualData);

    function GetColumns: TColumns;
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
  protected
    procedure DblClick; override;

    procedure DoMouseLeave; override;

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
    property Footer:TGridBands read GetFooter write SetFooter;
    property Indicator:TIndicator read GetIndicator write SetIndicator;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly default True;
    property Rows:TRows read GetRows write SetRows;
    property ScrollBars;
    property Selected:TGridSelection read GetSelected write SetSelected;

    property OnAfterDraw:TNotifyEvent read GetAfterDraw write SetAfterDraw;
    property OnClickedHeader:TNotifyEvent read GetClickedHeader write SetClickedHeader;
    property OnColumnResized:TColumnEvent read FOnColumnResized write FOnColumnResized;
    property OnNewDetail:TNewDetailEvent read GetOnNewDetail write SetOnNewDetail;
    property OnSelect:TNotifyEvent read GetOnSelect write SetOnSelect;
    property OnShowEditor:TShowEditorEvent read FOnShowEditor write FOnShowEditor;

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
  end;

implementation
