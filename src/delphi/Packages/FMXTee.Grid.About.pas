{*********************************************}
{  TeeGrid Software Library                   }
{  FMX About TeeGrid...                       }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Grid.About;
{$I Tee.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

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

  FMX.Dialogs, FMX.Layouts,

  Tee.Grid.Data.Strings, Tee.Grid.Ticker, FMXTee.Control, FMXTee.Grid;

type
  TFMXGridAbout = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    TeeGrid1: TTeeGrid;
    Timer1: TTimer;
    LVersion: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }

    Data : TStringsData;
    Ticker : TGridTicker;

    procedure ColumnTextAlign;
    procedure CustomFormat;
    procedure FillNames;
    procedure FillRandomValues;
    procedure RandomCell(out ACol,ARow:Integer);
  public
    { Public declarations }

    class procedure Show(const AOwner:TComponent); static;
  end;

implementation
