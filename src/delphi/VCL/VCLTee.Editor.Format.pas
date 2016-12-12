{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TFormat Editor                         }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Format;
{$I Tee.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls, {Vcl.}StdCtrls,
  {Vcl.}ExtCtrls,

  Tee.Format,
  VCLTee.Editor.Stroke, VCLTee.Editor.Brush;

type
  TFormatEditor = class(TForm)
    PageFormat: TPageControl;
    TabStroke: TTabSheet;
    TabBrush: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure PageFormatChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    IFormat : TFormat;

    IBrush : TBrushEditor;
    IStroke : TStrokeEditor;
  public
    { Public declarations }

    procedure RefreshFormat(const AFormat:TFormat);

    class function Edit(const AOwner:TComponent; const AFormat:TFormat):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AFormat:TFormat):TFormatEditor; static;
  end;

implementation
