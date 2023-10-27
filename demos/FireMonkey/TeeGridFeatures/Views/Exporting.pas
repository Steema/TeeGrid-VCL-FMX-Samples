unit Exporting;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Colors, FMXTee.Control,
  FMXTee.Grid,
  Tee.Renders, Tee.Grid.Columns, Tee.Grid.Rows,
  Tee.Grid.Bands, FMX.DateTimeCtrls, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types;

type
  TExportingForm = class(TForm)
    TeeGrid1: TTeeGrid;
    ColorBox1: TColorBox;
    Label1: TLabel;
    CBExport: TComboBox;
    BExport: TColorBox;
    Label2: TLabel;
    Memo1: TMemo;
    procedure BExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure FillData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExportingForm: TExportingForm;


implementation

{$R *.fmx}

uses
  Unit_MyData, Tee.GridData, Tee.GridData.Rtti,
  FMXTee.Painter, Tee.Format, System.StrUtils,
  Tee.Grid.Totals, Tee.Grid.CSV, Tee.Grid.JSON,
  System.IoUtils;

var
  MyData : TArray<TPerson>;


procedure TExportingForm.BExportClick(Sender: TObject);

  procedure StrToFile(const FileName, SourceString : string);
  var
  Stream : TFileStream;
  begin
  Stream:= TFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteBuffer(Pointer(SourceString)^, Length(SourceString));
  finally
    Stream.Free;
  end;
  end;

var
  GridDataAsStr : String;
begin
  CreateDir(ExtractFilePath(ParamStr(0))+'\export');
  // Export
  case CBExport.ItemIndex of
    0:
    begin
      GridDataAsStr:= TJSONData.From( TeeGrid1.Grid );  // <-- whole grid
      Memo1.Text :=  GridDataAsStr;
      Memo1.Lines.SaveToFile(format('%s/TeeGrid.JSON', [ExtractFilePath(ParamStr(0)) + '\export']));
//      StrToFile(ExtractFilePath(ParamStr(0)) + '\export\TeeGrid.JSON', GridDataAsStr);
    end;
    1:
    begin
      GridDataAsStr:= TCSVData.From( TeeGrid1.Grid );  // <-- whole grid
      Memo1.Text :=  GridDataAsStr;
      Memo1.Lines.SaveToFile(format('%s/TeeGrid.txt', [ExtractFilePath(ParamStr(0)) + '\export']));
//      StrToFile(ExtractFilePath(ParamStr(0)) + '\export\TeeGrid.txt', GridDataAsStr);
    end;
  end;
end;

procedure TExportingForm.FillData;
//var tmp : TColumnTotals;
begin
  SetLength(MyData,10);
  FillMyData(MyData);

  TeeGrid1.Data:=TVirtualData<TArray<TPerson>>.Create(MyData);

  // Setup grid Footer bands
//  TeeGrid1.Footer.Clear;

//  tmp:=Totals(TeeGrid1.Footer);
//  TTotalsHeader.CreateTotals(TeeGrid1.Footer,tmp);

  // Add a simple Title band to footer
//  AddSampleFooter;

  // Destroy the previously created SampleHeader, if any
//  SampleHeader.Free;

  // Add a simple Title band to headers
//  SampleHeader:=NewTitle(TeeGrid1.Headers,'Header Sample'#13#10'Text');

  // Move it to top
//  SampleHeader.Index:=0;
end;

procedure TExportingForm.FormCreate(Sender: TObject);
begin
  FillData;
end;

end.
