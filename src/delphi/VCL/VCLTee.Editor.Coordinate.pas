{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TCoordinate Editor                     }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Coordinate;

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}StdCtrls,
  {Vcl.}ExtCtrls, {Vcl.}ComCtrls,
  Tee.Format;

type
  TCoordinateEditor = class(TForm)
    Label5: TLabel;
    CBAuto: TCheckBox;
    EValue: TEdit;
    RGUnits: TRadioGroup;
    procedure CBAutoClick(Sender: TObject);
    procedure EValueChange(Sender: TObject);
    procedure RGUnitsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;
    ICoord : TCoordinate;

  public
    { Public declarations }

    procedure RefreshCoordinate(const ACoordinate:TCoordinate);

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ACoordinate:TCoordinate):TCoordinateEditor; overload; static;
  end;

implementation
