unit VCLTee.Editor.Gradient;

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls,
  {Vcl.}StdCtrls, {Vcl.}ExtCtrls,

  {$IFDEF FPC}
  ColorBox,
  {$ENDIF}

  Tee.Format;

type
  TGradientEditor = class(TForm)
    PageGradient: TPageControl;
    TabColors: TTabSheet;
    TabDirection: TTabSheet;
    CBVisible: TCheckBox;
    CBColor0: TColorBox;
    CBColor1: TColorBox;
    Label1: TLabel;
    TBOpacity0: TTrackBar;
    LOpacity0: TLabel;
    Label2: TLabel;
    TBOpacity1: TTrackBar;
    LOpacity1: TLabel;
    RGDirection: TRadioGroup;
    CBInverted: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CBColor0Change(Sender: TObject);
    procedure CBColor1Change(Sender: TObject);
    procedure CBInvertedClick(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure TBOpacity0Change(Sender: TObject);
    procedure TBOpacity1Change(Sender: TObject);
    procedure RGDirectionClick(Sender: TObject);
  private
    { Private declarations }

    IGradient : TGradient;

    procedure ChangeColor0;
    procedure SetOpacityLabel0;

    procedure ChangeColor1;
    procedure SetOpacityLabel1;
  public
    { Public declarations }

    procedure RefreshGradient(const AGradient:TGradient);

    class function Edit(const AOwner:TComponent; const AGradient:TGradient):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AGradient:TGradient):TGradientEditor; static;
  end;

implementation
