unit VCLTee.Editor.Column;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Editor.Format.Text, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, Tee.Grid.Columns, VCLTee.Editor.Coordinate;

type
  TColumnEditor = class(TTextFormatEditor)
    TabGeneral: TTabSheet;
    CBVisible: TCheckBox;
    CBExpanded: TCheckBox;
    CBReadOnly: TCheckBox;
    TabWidth: TTabSheet;
    TabSheet1: TTabSheet;
    Label5: TLabel;
    MemoHeader: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    EFloatFormat: TEdit;
    EDateTimeFormat: TEdit;
    Label6: TLabel;
    EDateFormat: TEdit;
    Label7: TLabel;
    ETimeFormat: TEdit;
    procedure CBVisibleClick(Sender: TObject);
    procedure CBExpandedClick(Sender: TObject);
    procedure EFloatFormatChange(Sender: TObject);
    procedure EDateTimeFormatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBReadOnlyClick(Sender: TObject);
    procedure PageFormatChange(Sender: TObject);
    procedure MemoHeaderChange(Sender: TObject);
    procedure EDateFormatChange(Sender: TObject);
    procedure ETimeFormatChange(Sender: TObject);
  private
    { Private declarations }

    Column : TColumn;

    IWidth : TCoordinateEditor;

    FOnChangedHeader : TNotifyEvent;

    procedure RefreshColumnFormat(const AFormat: TColumnFormat);
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const AColumn:TColumn):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AColumn:TColumn):TColumnEditor; overload; static;

    procedure RefreshColumn(const AColumn:TColumn);

    property OnChangedHeader:TNotifyEvent read FOnChangedHeader
                                          write FOnChangedHeader;
  end;

implementation
