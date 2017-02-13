program TeeGrid_Static_Charts;

uses
  Vcl.Forms,
  Unit_Grid_Charts in 'Unit_Grid_Charts.pas' {FormGridCharts};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormGridCharts, FormGridCharts);
  Application.Run;
end.
