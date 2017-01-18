{*********************************************}
{  TeeGrid Software Library                   }
{  TStroke Editor for Firemonkey              }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Editor.Painter.Stroke;
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

  FMX.Dialogs, Tee.Format,
  FMX.TabControl, FMXTee.Editor.Painter.Brush,
  FMX.ListBox, FMX.Colors, FMX.Layouts;

type
  TStrokeEditor = class(TForm)
    TabStroke: TTabControl;
    TabOptions: TTabItem;
    TabFill: TTabItem;
    CBVisible: TCheckBox;
    CBColor: TColorComboBox;
    TBSize: TTrackBar;
    Label1: TLabel;
    LSize: TLabel;
    TabStyle: TTabItem;
    Label2: TLabel;
    LBStyle: TListBox;
    Label3: TLabel;
    LBEndStyle: TListBox;
    Label4: TLabel;
    LBJoinStyle: TListBox;
    procedure TabStrokeChange(Sender: TObject);
    procedure CBVisibleChange(Sender: TObject);
    procedure CBColorChange(Sender: TObject);
    procedure TBSizeChange(Sender: TObject);
    procedure LBStyleChange(Sender: TObject);
    procedure LBEndStyleChange(Sender: TObject);
    procedure LBJoinStyleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    IStroke : TStroke;
    IBrush : TBrushEditor;

    procedure ChangedColor(Sender: TObject);
    procedure SetLabelSize;
  public
    { Public declarations }

    class procedure AddForm(const AForm: TCustomForm; const AParent: TControl); static;

    procedure RefreshStroke(const AStroke:TStroke);

    class function Edit(const AOwner:TComponent; const AStroke:TStroke):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const AStroke:TStroke):TStrokeEditor; static;
  end;

implementation
