unit VCLTee.Editor.Grid.Bands;

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}StdCtrls,
  {Vcl.}ExtCtrls, {Vcl.}ComCtrls, {Vcl.}Buttons,
  Tee.Grid.Bands;

type
  TGridBandsEditor = class(TForm)
    PanelTop: TPanel;
    CBVisible: TCheckBox;
    LBBands: TListBox;
    PanelMain: TPanel;
    PanelBandTop: TPanel;
    CBBandVisible: TCheckBox;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    procedure LBBandsClick(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure CBBandVisibleClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
  private
    { Private declarations }

    IBands : TGridBands;
    IEditor : TCustomForm;

    function Current:TGridBand;
    procedure MoveBand(const Delta:Integer);
    procedure RefreshUpDown;
  public
    { Public declarations }

    procedure RefreshBands(const ABands:TGridBands);

    class function Edit(const AOwner:TComponent; const ABands:TGridBands):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ABands:TGridBands):TGridBandsEditor; static;
  end;

implementation
