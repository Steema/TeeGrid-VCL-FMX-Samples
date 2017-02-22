{*********************************************}
{  TeeGrid Software Library                   }
{  FMX TCoordinate Editor                     }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Editor.Coordinate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Tee.Format, FMX.StdCtrls, FMX.Edit, FMX.EditBox, FMX.NumberBox,
  FMX.Controls.Presentation;

type
  TCoordinateEditor = class(TForm)
    CBAutomatic: TCheckBox;
    Label1: TLabel;
    EValue: TNumberBox;
    GBUnits: TGroupBox;
    RBPixels: TRadioButton;
    RBPercent: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RBPixelsChange(Sender: TObject);
    procedure EValueChangeTracking(Sender: TObject);
    procedure CBAutomaticChange(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;
    ICoord : TCoordinate;

  public
    { Public declarations }

    procedure RefreshCoordinate(const ACoordinate:TCoordinate);

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const ACoordinate:TCoordinate):TCoordinateEditor; overload; static;
  end;

implementation
