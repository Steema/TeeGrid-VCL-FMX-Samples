{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TTeeGrid Editor                        }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Grid;

interface

{
   Main VCL TeeGrid editor dialog
}

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls, {Vcl.}StdCtrls, {Vcl.}ExtCtrls,
  {Vcl.}Buttons,

  Tee.Renders, Tee.Grid.Columns, Tee.Grid.Selection,

  VCLTee.Grid, VCLTee.Control,

  VCLTee.Editor.Format, VCLTee.Editor.Stroke, VCLTee.Editor.Column,
  VCLTee.Editor.Coordinate, VCLTee.Editor.Margins,
  VCLTee.Editor.Render.Text, VCLTee.Editor.Grid.Bands;

type
  TTeeGridEditor = class(TForm)
    PageGrid: TPageControl;
    TabColumns: TTabSheet;
    TreeColumns: TTreeView;
    Panel5: TPanel;
    TabOptions: TTabSheet;
    SBDeleteColumn: TSpeedButton;
    TabBands: TTabSheet;
    PanelEditor: TPanel;
    TabCells: TTabSheet;
    PageCells: TPageControl;
    TabCellsFormat: TTabSheet;
    TabCellsHover: TTabSheet;
    Splitter1: TSplitter;
    TabRows: TTabSheet;
    PageOptions: TPageControl;
    TabIndicator: TTabSheet;
    PageRows: TPageControl;
    TabRowLines: TTabSheet;
    TabRowAlternate: TTabSheet;
    Panel3: TPanel;
    CBIndicatorVisible: TCheckBox;
    SBColumnUp: TSpeedButton;
    SBColumnDown: TSpeedButton;
    TabRowsGeneral: TTabSheet;
    Label1: TLabel;
    ERowHeight: TEdit;
    UDRowHeight: TUpDown;
    CBRowHeightAuto: TCheckBox;
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    TabBack: TTabSheet;
    TabMargins: TTabSheet;
    TabTheme: TTabSheet;
    LBThemes: TListBox;
    TabSelection: TTabSheet;
    Panel2: TPanel;
    CBFullRow: TCheckBox;
    CBSelectedParentFont: TCheckBox;
    Panel4: TPanel;
    CBAlternateVisible: TCheckBox;
    PageIndicator: TPageControl;
    TabIndicatorFormat: TTabSheet;
    TabIndicatorWidth: TTabSheet;
    TabColumnLines: TTabSheet;
    TabEditing: TTabSheet;
    CBDoubleClick: TCheckBox;
    CBEditingAlways: TCheckBox;
    CBReadOnly: TCheckBox;
    Label4: TLabel;
    TBVertSpacing: TTrackBar;
    LVertSpacing: TLabel;
    CBSelectedRange: TCheckBox;
    PageSelected: TPageControl;
    TabSelectedFocused: TTabSheet;
    TabSelectedUnfocused: TTabSheet;
    Label3: TLabel;
    TBHorizSpacing: TTrackBar;
    LHorizSpacing: TLabel;
    RGPainter: TRadioGroup;
    TabScrollBars: TTabSheet;
    CBScrollBars: TCheckBox;
    PageBands: TPageControl;
    TabHeaders: TTabSheet;
    TabFooter: TTabSheet;
    TabRowsBack: TTabSheet;
    SBAdd: TSpeedButton;
    TabSubBands: TTabSheet;
    Label2: TLabel;
    CBHorizScrollBar: TComboBox;
    Label5: TLabel;
    CBVertScrollBar: TComboBox;
    procedure TreeColumnsChange(Sender: TObject; Node: TTreeNode);
    procedure CBFullRowClick(Sender: TObject);
    procedure SBDeleteColumnClick(Sender: TObject);
    procedure PageGridChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBIndicatorVisibleClick(Sender: TObject);
    procedure SBColumnUpClick(Sender: TObject);
    procedure SBColumnDownClick(Sender: TObject);
    procedure ERowHeightChange(Sender: TObject);
    procedure CBRowHeightAutoClick(Sender: TObject);
    procedure CBReadOnlyClick(Sender: TObject);
    procedure PageOptionsChange(Sender: TObject);
    procedure TreeColumnsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure PageRowsChange(Sender: TObject);
    procedure PageCellsChange(Sender: TObject);
    procedure LBThemesClick(Sender: TObject);
    procedure CBSelectedParentFontClick(Sender: TObject);
    procedure CBAlternateVisibleClick(Sender: TObject);
    procedure TBHorizSpacingChange(Sender: TObject);
    procedure TBVertSpacingChange(Sender: TObject);
    procedure PageIndicatorChange(Sender: TObject);
    procedure CBDoubleClickClick(Sender: TObject);
    procedure CBEditingAlwaysClick(Sender: TObject);
    procedure CBSelectedRangeClick(Sender: TObject);
    procedure RGPainterClick(Sender: TObject);
    procedure CBScrollBarsClick(Sender: TObject);
    procedure PageBandsChange(Sender: TObject);
    procedure SBAddClick(Sender: TObject);
    procedure CBHorizScrollBarChange(Sender: TObject);
    procedure CBVertScrollBarChange(Sender: TObject);
  private
    { Private declarations }

    Grid : TTeeGrid;

    ICells,
    ICellsHover,
    ISelectedFocused,
    ISelectedUnfocused : TTextRenderEditor;

    ISubBands,
    IFooters,
    IHeaders : TGridBandsEditor;

    IRowAlternate,
    IRowsBack,
    IIndicatorFormat : TFormatEditor;

    IIndicatorWidth : TCoordinateEditor;

    IColumnEditor : TColumnEditor;

    IBack : TFormatEditor;

    IMargins : TMarginsEditor;

    IRowLines,
    IColumnLines : TStrokeEditor;

    UpdatingTree : Boolean;

    procedure ChangedHeader(Sender:TObject);
    function Column:TColumn;
    procedure EnableUpDown;
    procedure FillThemes;
    procedure MoveColumn(const Delta:Integer);
    procedure RefreshScrollSettings;
    procedure RefreshMargins(const AMargins:TMargins);
    procedure RefreshSelected(const ASelected:TGridSelection);
    procedure SetSpacingSettings;
    procedure ShowColumns(const ATree:TTreeView; const AColumns:TColumns);
  public
    { Public declarations }

    procedure RefreshGrid(const AGrid:TTeeGrid);

    class function Edit(const AOwner:TComponent; const AGrid:TTeeGrid):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AGrid:TTeeGrid):TTeeGridEditor; static;
  end;

implementation
