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
    property AlwaysVisible:Boolean read FAlwaysVisible write FAlwaysVisible default True;
    property DoubleClick:Boolean read FDoubleClick write FDoubleClick default True;
  end;

  TSelectEvent=procedure(const Sender:TRowGroup) of object;

  TScroll=TPointF;

  TCustomTeeGrid=class(TCustomTeeControl)
  private
    FData : TVirtualData;
    FEditing : TGridEditing;
    FMargins : TMargins;
    FOnAfterDraw: TNotifyEvent;
    FOnSelect: TSelectEvent;
    FRoot: TRowGroup;

    function AvailableHeight:Single;
    procedure ChangedRow(const Sender:TObject; const ARow:Integer);
    procedure CheckHorizScroll(const AColumn:TColumn);
    procedure CheckVertScroll(const ARow:Integer);
    procedure DataRefresh(Sender:TObject);

    function GetCells: TTextRender;
    function GetFooter: TGridBands;
    function GetHeader:TColumnHeaderBand;
    function GetRows:TRows;
    procedure CheckScroll(const ASelected:TGridSelection);

    function IsColumnsStored: Boolean;

    procedure SelectedChanged(Sender:TObject);

    procedure SetCells(const Value: TTextRender);
    procedure SetColumns(const Value: TColumns);
    procedure SetData(const Value:TVirtualData);
    procedure SetFooter(const Value: TGridBands);
    procedure SetHeader(const Value: TColumnHeaderBand);
    procedure SetIndicator(const Value: TIndicator);
    procedure SetMargins(const Value: TMargins);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetRows(const Value: TRows);
    procedure SetSelected(const Value: TGridSelection);
    procedure SetEditing(const Value: TGridEditing);
    function GetReadOnly: Boolean;
    function GetColumns: TColumns;
    function GetIndicator: TIndicator;
    function GetSelected: TGridSelection;
    function GetCurrent: TRowGroup; inline;
    procedure SetCurrent(const Value: TRowGroup); inline;
  protected
    procedure ChangeHorizScroll(const Value:Single);
    procedure ChangeVertScroll(const Value:Single);

    procedure CopySelected; virtual; abstract;

    procedure DataChanged; virtual;

    procedure Key(const AState:TKeyState);

    function HorizScrollBarHeight:Single; virtual; abstract;
    procedure HorizScrollChanged; virtual; abstract;

    procedure Mouse(var AState:TMouseState);

    property Root:TRowGroup read FRoot;

    procedure StartEditor(const AColumn:TColumn; const ARow:Integer); overload; virtual; abstract;
    procedure StartEditor; overload;

    procedure StopEditor; virtual;

    procedure TryStartEditor;

    function VertScrollBarWidth:Single; virtual; abstract;
    procedure VertScrollChanged; virtual; abstract;
  public
    Constructor Create(AOwner:TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    function ClientHeight:Single; override;
    function ClientWidth:Single; override;


    function CanExpand(const Sender:TRender; const ARow:Integer):Boolean;

    procedure Paint; override;
    procedure RefreshData;

    property Current:TRowGroup read GetCurrent write SetCurrent;
    property Data:TVirtualData read FData write SetData;
    property Rows:TRows read GetRows write SetRows;
  published
    property Cells:TTextRender read GetCells write SetCells;
    property Columns:TColumns read GetColumns write SetColumns stored IsColumnsStored;
    property Editing:TGridEditing read FEditing write SetEditing;
    property Footer:TGridBands read GetFooter write SetFooter;
    property Header:TColumnHeaderBand read GetHeader write SetHeader;
    property Indicator:TIndicator read GetIndicator write SetIndicator;
    property Margins:TMargins read FMargins write SetMargins;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly default False;
    property Selected:TGridSelection read GetSelected write SetSelected;

    property OnAfterDraw:TNotifyEvent read FOnAfterDraw write FOnAfterDraw;
    property OnSelect:TSelectEvent read FOnSelect write FOnSelect;
  end;

implementation
