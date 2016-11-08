unit VCLTee.Editor.Grid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  VCLTee.Grid, Tee.Grid.Columns, Vcl.Buttons,
  VCLTee.Editor.Format, VCLTee.Editor.Format.Text, VCLTee.Editor.Stroke,
  VCLTee.Editor.Column, VCLTee.Editor.Header;

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
    TabColumnLines: TTabSheet;
    PageRows: TPageControl;
    TabRowLines: TTabSheet;
    TabRowAlternate: TTabSheet;
    Panel3: TPanel;
    CBIndicatorVisible: TCheckBox;
    Label2: TLabel;
    TBIndicatorWidth: TTrackBar;
    LIndicatorWidth: TLabel;
    SBColumnUp: TSpeedButton;
    SBColumnDown: TSpeedButton;
    TabRowsGeneral: TTabSheet;
    Label1: TLabel;
    ERowHeight: TEdit;
    UDRowHeight: TUpDown;
    CBFullRow: TCheckBox;
    CBRowHeightAuto: TCheckBox;
    PanelButtons: TPanel;
    PanelOk: TPanel;
    BOk: TButton;
    TabGeneralOptions: TTabSheet;
    CBReadOnly: TCheckBox;
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure CBFullRowClick(Sender: TObject);
    procedure CBHeaderVisibleClick(Sender: TObject);
    procedure SBDeleteColumnClick(Sender: TObject);
    procedure PageGridChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBIndicatorVisibleClick(Sender: TObject);
    procedure TBIndicatorWidthChange(Sender: TObject);
    procedure SBColumnUpClick(Sender: TObject);
    procedure SBColumnDownClick(Sender: TObject);
    procedure ERowHeightChange(Sender: TObject);
    procedure CBRowHeightAutoClick(Sender: TObject);
    procedure CBReadOnlyClick(Sender: TObject);
  private
    { Private declarations }

    Grid : TTeeGrid;

    ICellsFormat,
    ICellsHover : TTextFormatEditor;

    IHeader : THeaderEditor;

    IRowAlternate,
    IIndicator : TFormatEditor;

    IColumnEditor : TColumnEditor;

    IRowLines,
    IColumnLines : TStrokeEditor;

    UpdatingTree : Boolean;

    procedure ChangedHeader(Sender:TObject);
    function Column:TColumn;
    procedure EnableUpDown;
    procedure MoveColumn(const Delta:Integer);
    procedure SetIndicatorSettings;
    procedure ShowColumns(const ATree:TTreeView; const AColumns:TColumns);
  public
    { Public declarations }

    procedure RefreshGrid(const AGrid:TTeeGrid);

    class function Edit(const AOwner:TComponent; const AGrid:TTeeGrid):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AGrid:TTeeGrid):TTeeGridEditor; static;
  end;

implementation
