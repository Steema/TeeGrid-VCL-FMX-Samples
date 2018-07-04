unit Appearance;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMXTee.Control,
  FMXTee.Grid, FMX.Layouts, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  Tee.Grid.Bands;

type
  TAppearanceForm = class(TForm)
    Layout1: TLayout;
    TeeGrid1: TTeeGrid;
    LayoutBase: TLayout;
    Rectangle1: TRectangle;
    CBHeader: TCheckBox;
    CBFooter: TCheckBox;
    procedure CBHeaderChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBFooterChange(Sender: TObject);
  private
    SampleHeader : TTextBand;
    SampleFooter : TTextBand;
    procedure AddSampleFooter;
  public
    { Public declarations }
  end;

var
  AppearanceForm: TAppearanceForm;

implementation

{$R *.fmx}

uses
  Tee.Format, Tee.Grid, Tee.GridData;



procedure TAppearanceForm.CBFooterChange(Sender: TObject);
begin
  SampleFooter.Visible := CBFooter.IsChecked;
end;

procedure TAppearanceForm.CBHeaderChange(Sender: TObject);
begin
  SampleHeader.Visible := CBHeader.IsChecked;
end;

// Create a new Title grid-band
function NewTitle(const ACollection:TCollection; const AText:String):TTextBand;
begin
  result:=TTextBand.Create(ACollection);

  result.Text:=AText;

  // Cosmetic
  result.Format.Font.Size:=12;
  result.Format.Stroke.Visible:=True;
  result.Format.Stroke.Color:=TColors.Red;
end;

procedure TAppearanceForm.AddSampleFooter;
begin
  SampleFooter:=NewTitle(TeeGrid1.Footer,'Footer Sample'#13'Text');

  SampleFooter.Format.Brush.Show;
  SampleFooter.Format.Brush.Gradient.Show;
  SampleFooter.Format.Brush.Gradient.Direction:=TGradientDirection.Horizontal;
end;

procedure TAppearanceForm.FormCreate(Sender: TObject);
begin
  // Destroy the previously created SampleHeader, if any
  SampleHeader.Free;

  // Add a simple Title band to headers
  SampleHeader:=NewTitle(TeeGrid1.Headers,'Header Sample'#13#10'Text');

  // Move it to top
  SampleHeader.Index:=0;

  // Add a simple Title band to footer
  AddSampleFooter;
end;

end.
