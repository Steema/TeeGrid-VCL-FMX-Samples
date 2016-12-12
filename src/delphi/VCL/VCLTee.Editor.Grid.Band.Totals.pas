unit VCLTee.Editor.Grid.Band.Totals;

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs,
  {Vcl.}ComCtrls, {Vcl.}StdCtrls, {Vcl.}ExtCtrls,

  Tee.Grid.Columns, Tee.Grid.Totals,
  VCLTee.Editor.Format.Text, VCLTee.Editor.Stroke;

type
  TColumnTotalsEditor = class(TForm)
    PageTotals: TPageControl;
    TabCalc: TTabSheet;
    TabFormat: TTabSheet;
    TreeColumns: TTreeView;
    TabHover: TTabSheet;
    TabLines: TTabSheet;
    Panel1: TPanel;
    CBHoverVisible: TCheckBox;
    CBHoverParentFont: TCheckBox;
    RGCalc: TRadioGroup;
    BNone: TButton;
    procedure PageTotalsChange(Sender: TObject);
    procedure CBHoverVisibleClick(Sender: TObject);
    procedure CBHoverParentFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGCalcClick(Sender: TObject);
    procedure TreeColumnsChange(Sender: TObject; Node: TTreeNode);
    procedure BNoneClick(Sender: TObject);
  private
    { Private declarations }

    ITotals : TColumnTotals;

    IFormat : TTextFormatEditor;
    IHover : TTextformatEditor;
    ILines : TStrokeEditor;

    procedure AddColumns;
    function Current:TColumn;
  public
    { Public declarations }

    procedure RefreshTotals(const ATotals:TColumnTotals);

    class function Edit(const AOwner:TComponent; const ATotals:TColumnTotals):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ATotals:TColumnTotals):TColumnTotalsEditor; static;
  end;

implementation
