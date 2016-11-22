unit Unit_Row_SubTitles;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMXTee.Control,
  FMXTee.Grid, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TForm43 = class(TForm)
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form43: TForm43;

implementation

{$R *.fmx}

uses
  Tee.Grid.Data.Strings, Tee.Grid.Bands, Tee.Format, Unit_Sample;

function NewTitle(const AText:String):TTitleBand;
begin
  result:=TTitleBand.Create(nil);
  result.Text:=AText;

  result.Format.Brush.Visible:=True;
  result.Format.Brush.Color:=TAlphaColors.Brown;
  result.Format.Font.Color:=TAlphaColors.White;
end;

procedure TForm43.FormCreate(Sender: TObject);
var Data : TStringsData;
begin
  // Use a TStringsData for this example, just for simplicity

  Data:=TStringsData.Create;
  Data.Resize(5,20);

  // Fill sample values
  FillSamples(Data);

  TeeGrid1.Data:=Data;

  // Add custom sub-title bands

  TeeGrid1.Rows.SubTitles[0]:=NewTitle('North');
  TeeGrid1.Rows.SubTitles[6]:=NewTitle('East');
  TeeGrid1.Rows.SubTitles[11]:=NewTitle('South');
  TeeGrid1.Rows.SubTitles[18]:=NewTitle('West');
end;

end.
