{*********************************************}
{  TeeGrid Software Library                   }
{  Base abstract Grid class                   }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid;
{$I Tee.inc}

interface

{
  Top high-level agnostic grid class:

  TCustomTeeGrid

  Its an abstract class. Cannot be created directly.

  Other units implement a derived TeeGrid class:

     VCLTee.Grid ->  TTeeGrid for VCL
     FMXTee.Grid ->  TTeeGrid for Firemonkey
}

uses
  {System.}Classes,

  {$IFNDEF FPC}
  {System.}Types,
  {$ENDIF}

  {System.}SysUtils,

  {$IFNDEF NOUITYPES}
  System.UITypes,
  {$ENDIF}

  Tee.Painter, Tee.Format, Tee.Renders, Tee.Control,
  Tee.Grid.Columns, Tee.Grid.Header, Tee.Grid.Bands, Tee.Grid.Rows,
  Tee.Grid.Selection, Tee.GridData, Tee.Grid.RowGroup;

type
  TTextEditing=class(TPersistentChange)
  private
    FSelected : Boolean;

    procedure SetSelected(const Value:Boolean);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Selected:Boolean read FSelected write SetSelected default True;
  end;

  TEditingEnter=(NextCell,NextRow,SameCell);

  // Properties to control Grid Cell editing
  TGridEditing=class(TPersistentChange)
  private
    FActive: Boolean;
    FAlwaysVisible : Boolean;
    FAutoEdit: Boolean;
    FClass : TClass;
    FDoubleClick : Boolean;
    FEnterKey: TEditingEnter;
    FText : TTextEditing;

    procedure SetText(const Value:TTextEditing);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    function ClassOf(const AColumn:TColumn):TClass;

    // True when there is a visible editor control to edit cell contents
    property Active:Boolean read FActive write FActive default False;

    property EditorClass:TClass read FClass write FClass;  // default = TEdit
  published
    property AlwaysVisible:Boolean read FAlwaysVisible write FAlwaysVisible default False;
    property AutoEdit:Boolean read FAutoEdit write FAutoEdit default False;
    property DoubleClick:Boolean read FDoubleClick write FDoubleClick default True;
    property EnterKey:TEditingEnter read FEnterKey write FEnterKey default TEditingEnter.NextCell;
    property Text:TTextEditing read FText write SetText;
  end;

  TScroll=TPointF;

  // Base-agnostic grid class
  TCustomTeeGrid=class(TCustomTeeControl)
  private
    FData : TVirtualData;
    FDataSource: TComponent;
    FEditing : TGridEditing;
    FOnAfterDraw: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FRoot: TRowGroup;

    IBands : TGridBands;

    function AvailableHeight:Single;
    procedure ChangedRow(const Sender:TObject; const ARow:Integer);
    procedure CheckHorizScroll(const AColumn:TColumn);
    procedure CheckScroll(const ASelected:TGridSelection);
    procedure CheckVertScroll(const ARow:Integer);
    procedure DataRefresh(Sender:TObject);

    function GetCells: TTextRender;
    function GetColumns: TColumns;
    function GetCurrent: TRowGroup; inline;
    function GetFooter: TGridBands;
    function GetHeader:TColumnHeaderBand;
    function GetHeaders: TGridBands;
    function GetIndicator: TIndicator;
    function GetMargins: TMargins;
    function GetReadOnly: Boolean;
    function GetRows:TRows;
    function GetSelected: TGridSelection;

    function IsColumnsStored: Boolean;

    procedure SelectedChanged(Sender:TObject);

    procedure SetCells(const Value: TTextRender);
    procedure SetColumns(const Value: TColumns);
    procedure SetCurrent(const Value: TRowGroup); inline;
    procedure SetData(const Value:TVirtualData);
    procedure SetDataSource(const Value: TComponent);
    procedure SetEditing(const Value: TGridEditing);
    procedure SetFooter(const Value: TGridBands);
    procedure SetHeader(const Value: TColumnHeaderBand);
    procedure SetHeaders(const Value: TGridBands);
    procedure SetIndicator(const Value: TIndicator);
    procedure SetMargins(const Value: TMargins);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetRows(const Value: TRows);
    procedure SetSelected(const Value: TGridSelection);
  protected
    ILoading : Boolean;

    procedure CancelEditor; virtual;
    procedure ChangeEditing(const AState:TIndicatorState);
    procedure ChangeHorizScroll(const Value:Single);
    procedure ChangeVertScroll(const Value:Single);
    procedure DataChanged; virtual;
    procedure DataSourceChanged;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoEditing(const Sender:TObject; const IsEditing:Boolean);
    procedure CheckScrollLimits(var X,Y:Single); virtual; abstract;
    function HasBandOfClass(const AClass:TGridBandClass):Boolean;
    procedure Key(const AState:TKeyState);
    function HorizScrollBarHeight:Single; virtual; abstract;
    procedure HorizScrollChanged(Sender:TObject); virtual; abstract;
    procedure Mouse(var AState:TMouseState);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure FinishPaint(const IsDesign:Boolean);
    function PerfectScrollBar(const Vertical:Boolean):Boolean;
    procedure ReadFooter(Reader: TReader);
    procedure ReadHeaders(Reader: TReader);
    procedure StartEditor(const AColumn:TColumn; const ARow:Integer;
                          const AutoEdit:String=''); overload; virtual; abstract;
    procedure StartEditor(const AutoEdit:String=''); overload;
    procedure StopEditor; virtual;
    procedure TryStartEditor(const AutoEdit:String='');
    function VertScrollBarWidth:Single; virtual; abstract;
    procedure VertScrollChanged(Sender:TObject); virtual; abstract;
    function WidthForColumns(const AGroup:TRowGroup; const AWidth:Single):Single;
    procedure WriteFooter(Writer: TWriter);
    procedure WriteHeaders(Writer: TWriter);
  public
    Constructor Create(AOwner:TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    function CanExpand(const Sender:TRender; const ARow:Integer):Boolean;
    function ClientHeight:Single; override;
    function ClientWidth:Single; override;
    procedure Copy(const ASelection:TGridSelection=nil); virtual; abstract;
    procedure Loaded; override;
    procedure MouseLeave;
    procedure Paint; override;
    procedure PasteSelected; virtual; abstract;
    procedure RefreshData;

    // Focused rows group
    property Current:TRowGroup read GetCurrent write SetCurrent;

    // Data for main rows
    property Data:TVirtualData read FData write SetData;

    // Main Group
    property Root:TRowGroup read FRoot;

    // Main rows
    property Rows:TRows read GetRows write SetRows;
  published
    property Cells:TTextRender read GetCells write SetCells;
    property Columns:TColumns read GetColumns write SetColumns stored IsColumnsStored;
    property DataSource:TComponent read FDataSource write SetDataSource;
    property Editing:TGridEditing read FEditing write SetEditing;
    property Footer:TGridBands read GetFooter write SetFooter;
    property Header:TColumnHeaderBand read GetHeader write SetHeader;
    property Headers:TGridBands read GetHeaders write SetHeaders stored False;
    property Indicator:TIndicator read GetIndicator write SetIndicator;
    property Margins:TMargins read GetMargins write SetMargins;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly default False;
    property Selected:TGridSelection read GetSelected write SetSelected;

    // Events
    property OnAfterDraw:TNotifyEvent read FOnAfterDraw write FOnAfterDraw;
    property OnSelect:TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TCustomTeeGridEditor=class
  public
  type
    TEditGrid=procedure(const Sender:TCustomTeeGrid);

  class var
    OnEdit : TEditGrid;
  end;

implementation
