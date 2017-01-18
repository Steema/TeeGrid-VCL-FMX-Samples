unit VCLTee.Editor.Grid.Ticker;

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls,
  {Vcl.}StdCtrls, {Vcl.}ExtCtrls,

  Tee.Grid.Ticker, VCLTee.Editor.Brush;

type
  TGridTickerEditor = class(TForm)
    PageTicker: TPageControl;
    TabOptions: TTabSheet;
    TabHigher: TTabSheet;
    TabLower: TTabSheet;
    Label2: TLabel;
    LDelay: TLabel;
    CBFade: TCheckBox;
    CBEnabled: TCheckBox;
    TBDelay: TTrackBar;
    procedure TBDelayChange(Sender: TObject);
    procedure CBEnabledClick(Sender: TObject);
    procedure CBFadeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageTickerChange(Sender: TObject);
  private
    { Private declarations }

    ITicker : TGridTicker;

    IHigher,
    ILower : TBrushEditor;

    procedure RefreshDelay;
  public
    { Public declarations }

    procedure RefreshTicker(const ATicker:TGridTicker);

    class function Edit(const AOwner:TComponent; const ATicker:TGridTicker):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ATicker:TGridTicker):TGridTickerEditor; static;
  end;

implementation
