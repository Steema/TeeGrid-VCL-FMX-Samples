{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TGridScrolling Editor                  }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Scrolling;
{$I Tee.inc}

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls,
  {Vcl.}StdCtrls, {Vcl.}ExtCtrls,

  Tee.Grid.RowGroup, Tee.Control, Tee.Grid;

type
  TScrollingEditor = class(TForm)
    PageScrolling: TPageControl;
    TabOptions: TTabSheet;
    TabScrollBars: TTabSheet;
    CBScrollBars: TCheckBox;
    Label2: TLabel;
    CBHorizScrollBar: TComboBox;
    Label5: TLabel;
    CBVertScrollBar: TComboBox;
    RGMode: TRadioGroup;
    LHoriz: TLabel;
    CBHoriz: TComboBox;
    Label1: TLabel;
    CBVert: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CBHorizScrollBarChange(Sender: TObject);
    procedure CBScrollBarsClick(Sender: TObject);
    procedure CBVertScrollBarChange(Sender: TObject);
    procedure RGModeClick(Sender: TObject);
    procedure CBHorizChange(Sender: TObject);
    procedure CBVertChange(Sender: TObject);
  private
    { Private declarations }

    IGrid : TCustomTeeGrid;
    IScrollBars : TScrollBars;

    procedure RefreshOptions(const AScroll:TGridScrolling);
  public
    { Public declarations }

    procedure RefreshScrolling(const AGrid:TCustomTeeGrid;
                               const AScrollBars:TScrollBars);

    class function Edit(const AOwner:TComponent;
                        const AGrid:TCustomTeeGrid;
                        const AScrollBars:TScrollBars):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AGrid:TCustomTeeGrid;
                          const AScrollBars:TScrollBars):TScrollingEditor; static;
  end;

implementation
