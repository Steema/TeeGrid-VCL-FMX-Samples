{*********************************************}
{  TeeGrid Software Library                   }
{  VCL THeader Editor                         }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Header;

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls, {Vcl.}StdCtrls,
  {Vcl.}ExtCtrls,

  VCLTee.Editor.Stroke, VCLTee.Editor.ColumnBand, VCLTee.Editor.Margins,

  // Must be last used unit due to clash with THeader in VCL
  Tee.Grid.Header;

type
  THeaderEditor = class(TForm)
    PageHeader: TPageControl;
    TabFormat: TTabSheet;
    TabRowLines: TTabSheet;
    TabMargins: TTabSheet;
    procedure PageHeaderChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    IHeader : TColumnHeaderBand;
    IFormat : TColumnBandEditor;
    IMargins : TTeeMarginsEditor;
    IRowLines : TStrokeEditor;
  public
    { Public declarations }

    procedure RefreshHeader(const AHeader:TColumnHeaderBand);

    class function Edit(const AOwner:TComponent; const AHeader:TColumnHeaderBand):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AHeader:TColumnHeaderBand):THeaderEditor; overload; static;
  end;

implementation
