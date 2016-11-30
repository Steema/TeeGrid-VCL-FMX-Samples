{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TBrush Editor                          }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Brush;

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {$IFDEF FPC}
  ColorBox,
  {$ENDIF}

  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}StdCtrls, {Vcl.}ExtCtrls,
  {Vcl.}ComCtrls, {Vcl.}ExtDlgs,

  VCLTee.Editor.Gradient,

  // Must be last unit in uses:
  Tee.Format;

type
  TBrushEditor = class(TForm)
    PageBrush: TPageControl;
    PanelTop: TPanel;
    CBVisible: TCheckBox;
    TabSolid: TTabSheet;
    TabGradient: TTabSheet;
    TabPicture: TTabSheet;
    CBColor: TColorBox;
    Label1: TLabel;
    TBOpacity: TTrackBar;
    LOpacity: TLabel;
    Image1: TImage;
    BClearPicture: TButton;
    Shape1: TShape;
    Button1: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    LPictureSize: TLabel;
    LPictureType: TLabel;
    procedure CBVisibleClick(Sender: TObject);
    procedure CBColorChange(Sender: TObject);
    procedure PageBrushChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TBOpacityChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BClearPictureClick(Sender: TObject);
  private
    { Private declarations }

    IBrush : TBrush;
    IGradient : TGradientEditor;

    FOnColorChange : TNotifyEvent;

    procedure ChangeColor;
    procedure RefreshPicture;
    procedure SetOpacityLabel;
  public
    { Public declarations }

    procedure RefreshBrush(const ABrush:TBrush);
    procedure RefreshColor;

    class function Edit(const AOwner:TComponent; const ABrush:TBrush):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ABrush:TBrush):TBrushEditor; static;

    property OnColorChange:TNotifyEvent read FOnColorChange write FOnColorChange;
  end;

implementation
