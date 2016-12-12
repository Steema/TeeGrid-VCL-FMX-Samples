unit VCLTee.Editor.Painter.GDIPlus;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  VCLTee.Painter.GDIPlus, Vcl.StdCtrls;

type
  TGDIPlusEditor = class(TForm)
    Label1: TLabel;
    CBSmooth: TComboBox;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    CBTextQuality: TComboBox;
    Label3: TLabel;
    CBTrimming: TComboBox;
    procedure CBSmoothChange(Sender: TObject);
    procedure CBTextQualityChange(Sender: TObject);
    procedure CBTrimmingChange(Sender: TObject);
  private
    { Private declarations }

    IPainter : TGdiPlusPainter;
    FOnChange: TNotifyEvent;

    procedure DoChanged;
  public
    { Public declarations }

    procedure RefreshPainter(const APainter:TGdiPlusPainter);

    class function Edit(const AOwner:TComponent; const APainter:TGdiPlusPainter;
                        const AOnChange:TNotifyEvent=nil):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const APainter:TGdiPlusPainter):TGDIPlusEditor; static;

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
