{*********************************************}
{  TeeGrid Software Library                   }
{  VCL THeader Editor                         }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.ColumnBand;

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls, {Vcl.}StdCtrls,
  {Vcl.}ExtCtrls,

  VCLTee.Editor.Format.Text, VCLTee.Editor.Coordinate, VCLTee.Editor.Stroke,

  // Must be last used unit due to clash with THeader in VCL
  Tee.Grid.Header;

type
  TColumnBandEditor = class(TForm)
    PageBand: TPageControl;
    TabFormat: TTabSheet;
    TabHover: TTabSheet;
    TabHeight: TTabSheet;
    Panel1: TPanel;
    CBHoverVisible: TCheckBox;
    CBHoverParentFont: TCheckBox;
    TabLines: TTabSheet;
    TabMouse: TTabSheet;
    CBAllowResize: TCheckBox;
    GroupBox1: TGroupBox;
    CBAllowDrag: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure PageBandChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBHoverVisibleClick(Sender: TObject);
    procedure CBAllowResizeClick(Sender: TObject);
    procedure CBAllowDragClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

    IBand : TColumnBand;

    IHover,
    IFormat : TTextFormatEditor;

    ILines : TStrokeEditor;

    IHeight : TCoordinateEditor;
  public
    { Public declarations }

    procedure RefreshBand(const ABand:TColumnBand);

    class function Edit(const AOwner:TComponent; const ABand:TColumnBand):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ABand:TColumnBand):TColumnBandEditor; overload; static;
  end;

implementation
