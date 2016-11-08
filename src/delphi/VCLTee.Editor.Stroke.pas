unit VCLTee.Editor.Stroke;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  {$IFDEF FPC}
  ColorBox,
  {$ENDIF}

  VCLTee.Editor.Brush,

  Tee.Format;

type
  TStrokeEditor = class(TForm)
    PageStroke: TPageControl;
    TabPen: TTabSheet;
    TabBrush: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    CBStrokeVisible: TCheckBox;
    TBStrokeSize: TTrackBar;
    CBStrokeColor: TColorBox;
    LBStrokeStyle: TListBox;
    LStrokeSize: TLabel;
    procedure CBStrokeColorChange(Sender: TObject);
    procedure CBStrokeVisibleClick(Sender: TObject);
    procedure LBStrokeStyleClick(Sender: TObject);
    procedure TBStrokeSizeChange(Sender: TObject);
    procedure PageStrokeChange(Sender: TObject);
  private
    { Private declarations }

    IBrush : TBrushEditor;

    IStroke : TStroke;
  public
    { Public declarations }

    class procedure AddForm(const AForm: TCustomForm; const AParent: TWinControl); static;

    procedure RefreshStroke(const AStroke:TStroke);

    class function Edit(const AOwner:TComponent; const AStroke:TStroke):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AStroke:TStroke):TStrokeEditor; static;
  end;

implementation
