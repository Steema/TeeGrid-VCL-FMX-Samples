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

  // Must be last unit in uses:
  Tee.Format, {Vcl.}ComCtrls;

type
  TBrushEditor = class(TForm)
    PageBrush: TPageControl;
    Panel1: TPanel;
    CBVisible: TCheckBox;
    TabSolid: TTabSheet;
    TabGradient: TTabSheet;
    TabPicture: TTabSheet;
    CBColor: TColorBox;
    procedure CBVisibleClick(Sender: TObject);
    procedure CBColorChange(Sender: TObject);
    procedure PageBrushChange(Sender: TObject);
  private
    { Private declarations }

    IBrush : TBrush;
  public
    { Public declarations }

    procedure RefreshBrush(const ABrush:TBrush);

    class function Edit(const AOwner:TComponent; const ABrush:TBrush):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ABrush:TBrush):TBrushEditor; static;
  end;

implementation
