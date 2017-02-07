unit Unit_Row_SubTitles;

interface

{
  This example shows how to insert "middle-bands" in between rows

}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMXTee.Control,

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

  FMXTee.Grid, FMX.Layouts,
  Tee.Grid.Bands, Tee.Grid.Rows, FMX.ListBox;

type
  TForm43 = class(TForm)
    TeeGrid1: TTeeGrid;
    Layout1: TLayout;
    BCopy: TButton;
    CBBands: TComboBox;
    CBVisible: TCheckBox;
    CBAllBands: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BCopyClick(Sender: TObject);
    procedure TeeGrid1Select(Sender: TObject);
    procedure CBBandsChange(Sender: TObject);
    procedure CBVisibleChange(Sender: TObject);
    procedure CBAllBandsChange(Sender: TObject);
  private
    { Private declarations }

    procedure AddBandsToCombo(const ABands:TRowsBands);
    function NewTitle(const AText:String):TTextBand;
  public
    { Public declarations }
  end;

var
  Form43: TForm43;

implementation

{$R *.fmx}

uses
  Tee.Grid.Data.Strings, Tee.Format, Unit_Sample, Tee.Grid.CSV;

// Create a new Title grid-band
function TForm43.NewTitle(const AText:String):TTextBand;
begin
  result:=TTextBand.Create(TeeGrid1.Rows.SubBands);
  result.Text:=AText;

  // Cosmetic
  result.Format.Brush.Show;
  result.Format.Brush.Color:=TAlphaColors.Brown;

  result.Format.Brush.Gradient.Colors[0].Color:=TAlphaColors.Brown;
  result.Format.Brush.Gradient.Show;

  result.Format.Font.Color:=TAlphaColors.White;
end;

// Called when a cell or range of cells are selected
procedure TForm43.TeeGrid1Select(Sender: TObject);
begin
  BCopy.Enabled:=not TeeGrid1.Selected.IsEmpty;
end;

// Copy the selected cells contents to clipboard (in CSV format)
procedure TForm43.BCopyClick(Sender: TObject);
begin
  TeeGrid1.Grid.Copy;
end;

// Show or hide all "Sub-Bands"
procedure TForm43.CBAllBandsChange(Sender: TObject);
begin
  TeeGrid1.Rows.SubBands.Visible:=CBAllBands.IsChecked;
end;

// Called when the combobox selected item is changed
procedure TForm43.CBBandsChange(Sender: TObject);
begin
  CBVisible.Enabled:=CBBands.ItemIndex<>-1;

  if CBVisible.Enabled then
     CBVisible.IsChecked:=TeeGrid1.Rows.SubBands[CBBands.ItemIndex].Visible;
end;

// Show or hide a specific Sub-Band
procedure TForm43.CBVisibleChange(Sender: TObject);
begin
  TeeGrid1.Rows.SubBands[CBBands.ItemIndex].Visible:=CBVisible.IsChecked;
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

  TeeGrid1.Rows.SubBands.Row[0]:=NewTitle('North');
  TeeGrid1.Rows.SubBands.Row[6]:=NewTitle('East');
  TeeGrid1.Rows.SubBands.Row[11]:=NewTitle('South');
  TeeGrid1.Rows.SubBands.Row[18]:=NewTitle('West');

  AddBandsToCombo(TeeGrid1.Rows.SubBands);

  // Do not select cell editor text when editing a cell
  TeeGrid1.Editing.Text.Selected:=False;

  // Keep cell editor active when changing the selected cell
  TeeGrid1.Editing.AutoEdit:=True;
end;

// Fill a ComboBox with all Bands
procedure TForm43.AddBandsToCombo(const ABands:TRowsBands);
var t : Integer;
    tmp : TGridBand;
begin
  for t:=0 to ABands.Count-1 do
  begin
    tmp:=ABands[t];

    if tmp is TTextBand then
       CBBands.Items.Add(TTextBand(tmp).Text)
    else
       CBBands.Items.Add(tmp.ClassName);
  end;

  if ABands.Count=0 then
     CBBands.ItemIndex:=-1
  else
     CBBands.ItemIndex:=0;
end;

end.
