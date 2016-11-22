{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TTeeGrid Editor                        }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Grid;

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls, {Vcl.}StdCtrls, {Vcl.}ExtCtrls,
  {Vcl.}Buttons,

  Tee.Renders, Tee.Grid.Columns, Tee.Grid.Selection,

  VCLTee.Grid,

  VCLTee.Editor.Format, VCLTee.Editor.Stroke, VCLTee.Editor.Column,
  VCLTee.Editor.Header, VCLTee.Editor.Coordinate, VCLTee.Editor.Margins,
  VCLTee.Editor.Render.Text;

type
  TTeeGridEditor = class(TForm)
    PageGrid: TPageControl;
    TabColumns: TTabSheet;
    TreeView1: TTreeView;
    Panel5: TPanel;
    TabOptions: TTabSheet;
    SBDeleteColumn: TSpeedButton;
    TabHeader: TTabSheet;
    PanelEditor: TPanel;
    Panel1: TPanel;
    CBHeaderVisible: TCheckBox;
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
    TabGeneralOptions: TTabSheet;
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
    GroupBox1: TGroupBox;
    Label3: TLabel;
    TBHorizSpacing: TTrackBar;
    LHorizSpacing: TLabel;
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
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure CBFullRowClick(Sender: TObject);
    procedure CBHeaderVisibleClick(Sender: TObject);
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
    procedure TreeView1Edited(Sender: TObject; Node: TTreeNode; var S: string);
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
  private
    { Private declarations }

    Grid : TTeeGrid;

    ICells,
    ICellsHover,
    ISelectedFocused,
    ISelectedUnfocused : TTextRenderEditor;

    IHeader : THeaderEditor;

    IRowAlternate,
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
