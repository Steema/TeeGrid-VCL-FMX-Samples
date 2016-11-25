unit VCLTee.Editor.Font;

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}StdCtrls,
  {Vcl.}ComCtrls,

  VCLTee.Editor.Brush,

  // Must be last unit
  Tee.Format;

type
  TFontEditor = class(TForm)
    PageFont: TPageControl;
    TabOptions: TTabSheet;
    Label2: TLabel;
    Label1: TLabel;
    LSize: TLabel;
    TBSize: TTrackBar;
    LBName: TListBox;
    GroupBox1: TGroupBox;
    CBBold: TCheckBox;
    CBItalic: TCheckBox;
    CBUnderline: TCheckBox;
    CBStrikeOut: TCheckBox;
    TabFill: TTabSheet;
    procedure CBBoldClick(Sender: TObject);
    procedure CBItalicClick(Sender: TObject);
    procedure CBStrikeOutClick(Sender: TObject);
    procedure CBUnderlineClick(Sender: TObject);
    procedure LBNameClick(Sender: TObject);
    procedure PageFontChange(Sender: TObject);
    procedure TBSizeChange(Sender: TObject);
  private
    { Private declarations }

    IBrush : TBrushEditor;

    IFont : TFont;

    procedure ChangeFontStyle(const Enable:Boolean; const AStyle:TFontStyle);
  public
    { Public declarations }

    procedure RefreshFont(const AFont:TFont);

    class function Edit(const AOwner:TComponent; const AFont:TFont):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AFont:TFont):TFontEditor; static;
  end;

implementation
