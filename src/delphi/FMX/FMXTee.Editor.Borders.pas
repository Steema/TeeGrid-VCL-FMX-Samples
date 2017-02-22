{*********************************************}
{  TeeGrid Software Library                   }
{  FMX TBorders Editor                        }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Editor.Borders;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  FMXTee.Editor.Painter.Stroke, Tee.Format, Tee.Renders;

type
  TBordersEditor = class(TForm)
    PageBorders: TTabControl;
    TabLeft: TTabItem;
    TabTop: TTabItem;
    TabRight: TTabItem;
    TabBottom: TTabItem;
    procedure FormCreate(Sender: TObject);
    procedure PageBordersChange(Sender: TObject);
  private
    { Private declarations }

    IBorders : TBorders;

    ILeft,
    ITop,
    IRight,
    IBottom : TStrokeEditor;
  public
    { Public declarations }

    procedure RefreshBorders(const ABorders:TBorders);

    class function Edit(const AOwner:TComponent; const ABorders:TBorders):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const ABorders:TBorders):TBordersEditor; static;
  end;

implementation
