{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TTextFormat Editor                     }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Format.Text;
{$I Tee.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls, {Vcl.}StdCtrls, Tee.Format,
  {Vcl.}ExtCtrls,

  VCLTee.Editor.Stroke, VCLTee.Editor.Brush, VCLTee.Editor.Font;

type
  TTextFormatEditor = class(TForm)
    PageFormat: TPageControl;
    TabFont: TTabSheet;
    TabStroke: TTabSheet;
    TabBrush: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure PageFormatChange(Sender: TObject);
  private
    { Private declarations }

    IFormat : TTextFormat;

    IFont : TFontEditor;
    IBrush : TBrushEditor;
    IStroke : TStrokeEditor;
  public
    { Public declarations }

    procedure RefreshFormat(const AFormat:TTextFormat);

    class function Edit(const AOwner:TComponent; const AFormat:TTextFormat):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AFormat:TTextFormat):TTextFormatEditor; static;
  end;

implementation
