{*********************************************}
{  TeeGrid Software Library                   }
{  TGradient Editor for Firemonkey            }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Editor.Painter.Gradient;
{$I Tee.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Colors,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$IFEND}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  {$IF CompilerVersion<25}
  {$DEFINE HASFMX21}
  {$IFEND}

  {$IFNDEF HASFMX21}
  FMX.StdCtrls,
  {$ENDIF}

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX22}
  {$IFEND}

  {$IFNDEF HASFMX22}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMX.ListBox,

  Tee.Painter, Tee.Format;

type
  TGradientEditor = class(TForm)
    Panel1: TPanel;
    CBVisible: TCheckBox;
    Label1: TLabel;
    CBDirection: TComboBox;
    ColorButton1: TComboColorBox;
    ColorButton2: TComboColorBox;
    CBInverted: TCheckBox;
    procedure CBInvertedChange(Sender: TObject);
    procedure CBDirectionChange(Sender: TObject);
    procedure CBVisibleChange(Sender: TObject);
    procedure ColorButton1Change(Sender: TObject);
    procedure ColorButton2Change(Sender: TObject);
  private
    { Private declarations }

    IGradient : TGradient;
  public
    { Public declarations }

    procedure RefreshGradient(const AGradient:TGradient);

    class function Edit(const AOwner:TComponent; const AGradient:TGradient):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const AGradient:TGradient):TGradientEditor; static;

  end;

implementation
