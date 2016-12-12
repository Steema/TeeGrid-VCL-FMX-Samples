{*********************************************}
{  TeeGrid Software Library                   }
{  Grid Cells selection class                 }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Selection;
{$I Tee.inc}

interface

{
   TGridSelection class is responsible to paint and mouse-hover highlight
   grid cells.

   Supports single and multi-cell range selection.

   Examples:

     TeeGrid1.Selected.Column:= TeeGrid1.Columns[3];
     TeeGrid1.Selected.Row:= 42;

     // Multi-cell range:

     TeeGrid1.Selected.Range.FromRow:= 10;
     TeeGrid1.Selected.Range.ToRow:= 10;

     TeeGrid1.Selected.Range.FromColumn:= TeeGrid1.Columns[3];
     TeeGrid1.Selected.Range.ToColumn:= TeeGrid1.Columns[6];

   Needs: Tee.Painter, Tee.Format, Tee.Grid.Columns, Tee.Renders
}


uses
  {System.}Classes,

  Tee.Painter, Tee.Format, Tee.Grid.Columns, Tee.Renders;

type
  TSelectionRange=class(TPersistentChange)
  private
    FFromRow: Integer;
    FFromColumn: TColumn;
    FToRow: Integer;
    FToColumn: TColumn;
    FEnabled: Boolean;

    procedure SetFromColumn(const Value: TColumn);
    procedure SetFromRow(const Value: Integer);
    procedure SetToColumn(const Value: TColumn);
    procedure SetToRow(const Value: Integer);
    procedure SetEnabled(const Value: Boolean);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    procedure Assign(Source:TPersistent); override;
  published
    property Enabled:Boolean read FEnabled write SetEnabled default True;
    property FromColumn:TColumn read FFromColumn write SetFromColumn;
    property FromRow:Integer read FFromRow write SetFromRow default -1;
    property ToColumn:TColumn read FToColumn write SetToColumn;
    property ToRow:Integer read FToRow write SetToRow default -1;
  end;

  TGridSelection=class(TVisibleTextRender)
  private
    FColumn : TColumn;
    FRange : TSelectionRange;
    FRow : Integer;
    FFull: Boolean;
    FOnChange: TNotifyEvent;
    FParentFont: Boolean;
    FUnFocused : TVisibleTextRender;

    procedure ChangeColumn(const Value:TColumn);
    procedure DoChanged;
    procedure ResetRange;
    procedure SetColumn(const Value: TColumn);
    procedure SetRange(const Value: TSelectionRange);
    procedure SetRow(const Value: Integer);
    procedure SetFull(const Value: Boolean);
    procedure SetParentFont(const Value: Boolean);
    procedure SetUnfocused(const Value: TVisibleTextRender);
  protected
    CheckScroll : Boolean;
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source:TPersistent); override;

    procedure Change(const AColumn:TColumn; const ARow:Integer);
    procedure Clear;
    procedure GetColumns(const Visible:TVisibleColumns; out AFrom,ATo:Integer);
    procedure InitFormat;
    function IsEmpty:Boolean;

    procedure PaintColumn(var AData:TRenderData; const AColumn:TColumn; const AFont:TFont; const IsFocused:Boolean);

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  published
    property Column:TColumn read FColumn write SetColumn stored False;
    property FullRow:Boolean read FFull write SetFull default False;
    property ParentFont:Boolean read FParentFont write SetParentFont default True;
    property Range:TSelectionRange read FRange write SetRange;
    property Row:Integer read FRow write SetRow stored False;
    property UnFocused:TVisibleTextRender read FUnfocused write SetUnfocused;
  end;

implementation
