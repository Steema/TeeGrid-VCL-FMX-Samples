unit VCLTee.Editor.Format;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Tee.Format, Vcl.StdCtrls,
  Vcl.ExtCtrls, VCLTee.Editor.Stroke, VCLTee.Editor.Brush;

type
  TFormatEditor = class(TForm)
    PageFormat: TPageControl;
    TabStroke: TTabSheet;
    TabBrush: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure PageFormatChange(Sender: TObject);
  private
    { Private declarations }

    IFormat : TFormat;

    IBrush : TBrushEditor;
    IStroke : TStrokeEditor;
  public
    { Public declarations }

    procedure RefreshFormat(const AFormat:TFormat);

    class function Edit(const AOwner:TComponent; const AFormat:TFormat):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AFormat:TFormat):TFormatEditor; static;
  end;

implementation
