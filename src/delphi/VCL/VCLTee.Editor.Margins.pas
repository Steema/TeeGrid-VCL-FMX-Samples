{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TMargins Editor                        }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Margins;

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls,
  VCLTee.Editor.Coordinate,
  Tee.Format, Tee.Renders;

type
  TMarginsEditor = class(TForm)
    PageControl1: TPageControl;
    TabLeft: TTabSheet;
    TabTop: TTabSheet;
    TabRight: TTabSheet;
    TabBottom: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }

    IMargins : TMargins;

    ILeft,
    ITop,
    IRight,
    IBottom : TCoordinateEditor;
  public
    { Public declarations }

    procedure RefreshMargins(const AMargins:TMargins);

    class function Edit(const AOwner:TComponent; const AMargins:TMargins):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AMargins:TMargins):TMarginsEditor; static;
  end;

implementation
