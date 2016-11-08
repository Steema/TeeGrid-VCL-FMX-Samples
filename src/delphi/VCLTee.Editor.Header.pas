unit VCLTee.Editor.Header;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  VCLTee.Editor.Format.Text, VCLTee.Editor.Coordinate, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  // Must be last unit in uses:
  Tee.Grid.Header;

type
  THeaderEditor = class(TForm)
    PageHeader: TPageControl;
    TabFormat: TTabSheet;
    TabHover: TTabSheet;
    TabHeight: TTabSheet;
    Panel1: TPanel;
    CBHoverVisible: TCheckBox;
    CBHoverParentFont: TCheckBox;
    procedure PageHeaderChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBHoverVisibleClick(Sender: TObject);
  private
    { Private declarations }

    IHeader : THeader;

    IHover,
    IFormat : TTextFormatEditor;

    IHeight : TCoordinateEditor;
  public
    { Public declarations }

    procedure RefreshHeader(const AHeader:THeader);

    class function Edit(const AOwner:TComponent; const AHeader:THeader):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AHeader:THeader):THeaderEditor; overload; static;
  end;

implementation
