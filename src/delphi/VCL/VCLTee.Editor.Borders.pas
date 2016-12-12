{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TBorders Editor                        }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Borders;

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}ComCtrls,
  VCLTee.Editor.Stroke, Tee.Format, Tee.Renders;

type
  TBordersEditor = class(TForm)
    PageBorders: TPageControl;
    TabLeft: TTabSheet;
    TabTop: TTabSheet;
    TabRight: TTabSheet;
    TabBottom: TTabSheet;
    procedure PageBordersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ABorders:TBorders):TBordersEditor; static;
  end;

implementation
