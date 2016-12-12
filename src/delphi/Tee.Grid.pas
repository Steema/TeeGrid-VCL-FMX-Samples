{*********************************************}
{  TeeGrid Software Library                   }
{  Base abstract Grid class                   }
{  Copyright (c) 2016 by Steema Software      }
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
  Tee.Grid.Selection, Tee.Grid.Data, Tee.Grid.RowGroup;

type
  TGridEditing=class(TPersistentChange)
  private
    FActive: Boolean;
    FAlwaysVisible : Boolean;
    FClass : TClass;
    FDoubleClick : Boolean;
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure Assign(Source:TPersistent); override;

    function ClassOf(const AColumn:TColumn):TClass;

    property Active:Boolean read FActive write FActive default False;
    property EditorClass:TClass read FClass write FClass;
  published
    property AlwaysVisible:Boolean read FAlwaysVisible write FAlwaysVisible default False;
    property DoubleClick:Boolean read FDoubleClick write FDoubleClick default True;
  end;

  TScroll=TPointF;

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
    function GetReadOnly: Boolean;
    function GetSelected: TGridSelection;
    function GetRows:TRows;

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
    function GetMargins: TMargins;
  protected
    procedure ChangeHorizScroll(const Value:Single);
    procedure ChangeVertScroll(const Value:Single);
    procedure DataChanged; virtual;
    procedure DataSourceChanged;
    procedure Key(const AState:TKeyState);
    function HorizScrollBarHeight:Single; virtual; abstract;
    procedure HorizScrollChanged; virtual; abstract;
    procedure Mouse(var AState:TMouseState);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure FinishPaint(const IsDesign:Boolean);
    procedure ReadFooter(Reader: TReader);
    procedure ReadHeaders(Reader: TReader);
    property Root:TRowGroup read FRoot;
    procedure StartEditor(const AColumn:TColumn; const ARow:Integer); overload; virtual; abstract;
    procedure StartEditor; overload;
    procedure StopEditor; virtual;
    procedure TryStartEditor;
    function VertScrollBarWidth:Single; virtual; abstract;
    procedure VertScrollChanged; virtual; abstract;
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
    procedure CopySelected; virtual; abstract;
    procedure Loaded; override;
    procedure MouseLeave;
    procedure Paint; override;
    procedure RefreshData;

    // Focused rows group
    property Current:TRowGroup read GetCurrent write SetCurrent;

    // Data for main rows
    property Data:TVirtualData read FData write SetData;

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

implementation
