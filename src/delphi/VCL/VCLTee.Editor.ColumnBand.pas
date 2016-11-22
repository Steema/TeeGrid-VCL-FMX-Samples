{*********************************************}
{  TeeGrid Software Library                   }
{  VCL THeader Editor                         }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.ColumnBand;

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
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
    procedure PageBandChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBHoverVisibleClick(Sender: TObject);
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
