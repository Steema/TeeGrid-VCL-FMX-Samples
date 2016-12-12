unit VCLTee.Editor.Grid.Bands;

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}StdCtrls,
  {Vcl.}ExtCtrls, {Vcl.}ComCtrls, {Vcl.}Buttons, {Vcl.}CheckLst, {Vcl.}Menus,

  Tee.Grid.Bands;

type
  TGridBandsEditor = class(TForm)
    PanelTop: TPanel;
    CBVisible: TCheckBox;
    LBBands: TCheckListBox;
    PanelMain: TPanel;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    SBAdd: TSpeedButton;
    SBRemove: TSpeedButton;
    PopupMenu1: TPopupMenu;
    ext1: TMenuItem;
    Header1: TMenuItem;
    otals1: TMenuItem;
    otalsHeader1: TMenuItem;
    procedure LBBandsClick(Sender: TObject);
    procedure CBVisibleClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure LBBandsClickCheck(Sender: TObject);
    procedure SBAddClick(Sender: TObject);
    procedure SBRemoveClick(Sender: TObject);
    procedure ext1Click(Sender: TObject);
    procedure Header1Click(Sender: TObject);
    procedure otals1Click(Sender: TObject);
    procedure otalsHeader1Click(Sender: TObject);
  private
    { Private declarations }

    IBands : TGridBands;
    IEditor : TCustomForm;

    function AddBand(const AClass:TGridBandClass):TGridBand;
    procedure AddBandToList(const ABand: TGridBand);
    function Current:TGridBand;
    procedure MoveBand(const Delta:Integer);
    procedure RefreshUpDown;

  protected
    Header : TGridBand;
  public
    { Public declarations }

    procedure RefreshBands(const ABands:TGridBands);

    class function Edit(const AOwner:TComponent; const ABands:TGridBands):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ABands:TGridBands):TGridBandsEditor; static;
  end;

implementation
