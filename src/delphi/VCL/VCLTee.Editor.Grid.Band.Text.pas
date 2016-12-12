unit VCLTee.Editor.Grid.Band.Text;

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls,
  {Vcl.}StdCtrls,

  VCLTee.Editor.Render.Text, Tee.Grid.Bands, VCLTee.Editor.Coordinate;

type
  TTextBandEditor = class(TForm)
    PageText: TPageControl;
    TabText: TTabSheet;
    TabOptions: TTabSheet;
    MemoText: TMemo;
    TabHeight: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure PageTextChange(Sender: TObject);
    procedure MemoTextChange(Sender: TObject);
  private
    { Private declarations }

    IText : TTextBand;

    IHeight : TCoordinateEditor;
    IOptions : TTextRenderEditor;
  public
    { Public declarations }

    procedure RefreshText(const AText:TTextBand);

    class function Edit(const AOwner:TComponent; const AText:TTextBand):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AText:TTextBand):TTextBandEditor; static;
  end;

implementation
