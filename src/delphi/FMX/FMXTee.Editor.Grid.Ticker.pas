unit FMXTee.Editor.Grid.Ticker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Tee.Grid.Ticker,
  FMXTee.Editor.Painter.Brush, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TGridTickerEditor = class(TForm)
    PageTicker: TTabControl;
    TabOptions: TTabItem;
    TabHigher: TTabItem;
    TabLower: TTabItem;
    CBEnabled: TCheckBox;
    CBFade: TCheckBox;
    Label1: TLabel;
    TBDelay: TTrackBar;
    LDelay: TLabel;
    procedure PageTickerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TBDelayChange(Sender: TObject);
    procedure CBFadeChange(Sender: TObject);
    procedure CBEnabledChange(Sender: TObject);
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

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const ATicker:TGridTicker):TGridTickerEditor; static;
  end;

implementation
