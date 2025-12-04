program VCL_TeeGrid_DataSet;

uses
  Vcl.Forms,
  Unit_Dataset in 'Unit_Dataset.pas' {FormGridDataset},
  Tee.Grid.DB.SortableDataSet in 'Tee.Grid.DB.SortableDataSet.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormGridDataset, FormGridDataset);
  Application.Run;
end.
