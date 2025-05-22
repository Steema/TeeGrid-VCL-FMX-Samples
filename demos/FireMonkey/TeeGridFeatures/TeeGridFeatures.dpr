program TeeGridFeatures;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {MainForm},
  Appearance in 'Views\Appearance.pas' {AppearanceForm},
  Exporting in 'Views\Exporting.pas' {ExportingForm},
  Unit_Locked_Columns in 'Views\Unit_Locked_Columns.pas' {LockedColumnsForm},
  Unit_FMX_Themes in 'Views\Unit_FMX_Themes.pas' {FormGridThemes},
  Unit_MyData in 'Data\Unit_MyData.pas',
  Unit_Custom_Sorting in 'Views\Unit_Custom_Sorting.pas' {FormCustomSorting},
  Unit_FMX_Ticker in 'Views\Unit_FMX_Ticker.pas' {TickerForm},
  Unit_FMX_REST in 'Views\Unit_FMX_REST.pas' {RESTClientTeeGridForm},
  Unit_Editors in 'Views\Unit_Editors.pas' {FormCellEditors},
  Unit_Example_Data in 'Views\Unit_Example_Data.pas',
  Unit_Utils in 'Views\Unit_Utils.pas',
  Master_Detail_FireDAC in 'Views\Database\Master_Detail_FireDAC\Master_Detail_FireDAC.pas' {MasterDetail},
  Customer_Orders in 'Views\Database\Master_Detail_FireDAC\Customer_Orders.pas' {SampleData: TDataModule},
  Unit_DataSet in 'Views\Database\DataSet\Unit_DataSet.pas' {FormGridDataSet},
  Unit_Master_Detail_Two_Grids in 'Views\Database\Master_Detail_FireDAC\Unit_Master_Detail_Two_Grids.pas' {MasterDetail2GridsForm},
  ArrayData in 'Views\ArrayData.pas' {ArrayAsDataForm},
  Tee.Grid.JSON in '..\..\..\Tee.Grid.JSON.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSampleData, SampleData);
  Application.Run;
end.
