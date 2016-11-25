unit FMXTee.Editor.Painter.Brush;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms,

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$IFEND}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  {$IF CompilerVersion<25}
  {$DEFINE HASFMX21}
  {$IFEND}

  {$IFNDEF HASFMX21}
  FMX.StdCtrls,
  {$ENDIF}

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX22}
  {$IFEND}

  {$IFNDEF HASFMX22}
  FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, Tee.Format,
  FMX.Colors, FMX.TabControl,
  FMX.Layouts;

type
  TBrushEditor = class(TForm)
    LayoutTop: TLayout;
    CBVisible: TCheckBox;
    TabBrush: TTabControl;
    TabColor: TTabItem;
    TabGradient: TTabItem;
    TabPicture: TTabItem;
    CBColor: TColorPanel;
    procedure CBVisibleChange(Sender: TObject);
    procedure CBColorChange(Sender: TObject);
  private
    { Private declarations }

    IBrush : TBrush;

    FOnColorChange : TNotifyEvent;

    procedure RefreshGradient(const AGradient:TGradient);
  public
    { Public declarations }

    procedure RefreshBrush(const ABrush:TBrush);
    procedure RefreshColor;

    class function Edit(const AOwner:TComponent; const ABrush:TBrush):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const ABrush:TBrush):TBrushEditor; static;

    property OnColorChange:TNotifyEvent read FOnColorChange write FOnColorChange;
  end;

implementation
