{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TStroke Editor                         }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Stroke;
{$I Tee.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}StdCtrls, {Vcl.}ExtCtrls, {Vcl.}ComCtrls,
  {$IFDEF FPC}
  ColorBox,
  {$ENDIF}

  VCLTee.Editor.Brush,

  Tee.Format;

type
  TStrokeEditor = class(TForm)
    PageStroke: TPageControl;
    TabPen: TTabSheet;
    TabBrush: TTabSheet;
    Label3: TLabel;
    CBVisible: TCheckBox;
    TBSize: TTrackBar;
    CBColor: TColorBox;
    LSize: TLabel;
    TabStyle: TTabSheet;
    Label4: TLabel;
    LBStyle: TListBox;
    Label1: TLabel;
    LBEndStyle: TListBox;
    Label2: TLabel;
    LBJoinStyle: TListBox;
    procedure CBColorChange(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure LBStyleClick(Sender: TObject);
    procedure TBSizeChange(Sender: TObject);
    procedure PageStrokeChange(Sender: TObject);
    procedure LBEndStyleClick(Sender: TObject);
    procedure LBJoinStyleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    IBrush : TBrushEditor;
    IStroke : TStroke;

    procedure ChangedColor(Sender: TObject);
    procedure SetLabelSize;
  public
    { Public declarations }

    class procedure AddForm(const AForm: TCustomForm; const AParent: TWinControl); static;

    procedure RefreshStroke(const AStroke:TStroke);

    class function Edit(const AOwner:TComponent; const AStroke:TStroke):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AStroke:TStroke):TStrokeEditor; static;
  end;

implementation
