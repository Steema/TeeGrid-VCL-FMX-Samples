{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TTextFormat Editor                     }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Format.Text;
{$I Tee.inc}

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls, {Vcl.}StdCtrls, Tee.Format,
  {Vcl.}ExtCtrls,
  {$IFDEF FPC}
  ColorBox,
  {$ENDIF}

  VCLTee.Editor.Stroke, VCLTee.Editor.Brush;

type
  TTextFormatEditor = class(TForm)
    PageFormat: TPageControl;
    TabFont: TTabSheet;
    TabStroke: TTabSheet;
    TabBrush: TTabSheet;
    Label2: TLabel;
    TBFontSize: TTrackBar;
    Label1: TLabel;
    LBFontName: TListBox;
    CBFontColor: TColorBox;
    GroupBox1: TGroupBox;
    CBFontBold: TCheckBox;
    CBFontItalic: TCheckBox;
    CBFontUnderline: TCheckBox;
    CBFontStrikeOut: TCheckBox;
    LFontSize: TLabel;
    procedure TBFontSizeChange(Sender: TObject);
    procedure CBFontColorChange(Sender: TObject);
    procedure LBFontNameClick(Sender: TObject);
    procedure CBFontBoldClick(Sender: TObject);
    procedure CBFontItalicClick(Sender: TObject);
    procedure CBFontUnderlineClick(Sender: TObject);
    procedure CBFontStrikeOutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageFormatChange(Sender: TObject);
  private
    { Private declarations }

    IFormat : TTextFormat;

    IBrush : TBrushEditor;
    IStroke : TStrokeEditor;

    procedure ChangeFontStyle(const Enable:Boolean; const AStyle:TFontStyle);
    procedure SetFontSettings(const AFont:TFont);
  public
    { Public declarations }

    procedure RefreshFormat(const AFormat:TTextFormat);

    class function Edit(const AOwner:TComponent; const AFormat:TTextFormat):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AFormat:TTextFormat):TTextFormatEditor; static;
  end;

implementation
