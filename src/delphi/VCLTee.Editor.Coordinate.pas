unit VCLTee.Editor.Coordinate;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Tee.Format;

type
  TCoordinateEditor = class(TForm)
    Label5: TLabel;
    CBAuto: TCheckBox;
    UDValue: TUpDown;
    EValue: TEdit;
    RGUnits: TRadioGroup;
    procedure CBAutoClick(Sender: TObject);
    procedure EValueChange(Sender: TObject);
    procedure RGUnitsClick(Sender: TObject);
  private
    { Private declarations }

    ICoord : TCoordinate;
  public
    { Public declarations }

    procedure RefreshCoordinate(const ACoordinate:TCoordinate);

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ACoordinate:TCoordinate):TCoordinateEditor; overload; static;
  end;

implementation
