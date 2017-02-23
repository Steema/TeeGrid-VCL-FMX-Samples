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
  VCLTee.Editor.ColumnBand, VCLTee.Editor.Stroke;

type
  TColumnTotalsEditor = class(TForm)
    PageTotals: TPageControl;
    TabCalc: TTabSheet;
    TabOptions: TTabSheet;
    TreeColumns: TTreeView;
    RGCalc: TRadioGroup;
    BNone: TButton;
    procedure PageTotalsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGCalcClick(Sender: TObject);
    procedure TreeColumnsChange(Sender: TObject; Node: TTreeNode);
    procedure BNoneClick(Sender: TObject);
  private
    { Private declarations }

    ITotals : TColumnTotals;

    IOptions : TColumnBandEditor;

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
