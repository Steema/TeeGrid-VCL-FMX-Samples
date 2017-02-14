{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TGridSelection Editor                  }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Selected;
{$I Tee.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ExtCtrls, {Vcl.}StdCtrls,
  {Vcl.}ComCtrls,

  VCLTee.Editor.Render.Text, Tee.Grid.Selection;

type
  TSelectedEditor = class(TForm)
    PageSelected: TPageControl;
    TabOptions: TTabSheet;
    CBFullRow: TCheckBox;
    CBRange: TCheckBox;
    CBScrollToView: TCheckBox;
    TabFormat: TTabSheet;
    Panel2: TPanel;
    CBParentFont: TCheckBox;
    PageFormat: TPageControl;
    TabFocused: TTabSheet;
    TabUnfocused: TTabSheet;
    procedure CBFullRowClick(Sender: TObject);
    procedure PageFormatChange(Sender: TObject);
    procedure PageSelectedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBScrollToViewClick(Sender: TObject);
    procedure CBParentFontClick(Sender: TObject);
    procedure CBRangeClick(Sender: TObject);
  private
    { Private declarations }

    IFocused,
    IUnfocused : TTextRenderEditor;

    ISelected : TGridSelection;
  public
    { Public declarations }

    procedure RefreshSelected(const ASelected:TGridSelection);

    class function Edit(const AOwner:TComponent; const ASelected:TGridSelection):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ASelected:TGridSelection):TSelectedEditor; static;
  end;

implementation
