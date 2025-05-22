program TeeChart_in_TeeGrid;

uses
  Vcl.Forms,
  Unit_Chart_and_Grid in 'Unit_Chart_and_Grid.pas' {Chart_in_Grid},
  TeeChart_Grid_Render in 'TeeChart_Grid_Render.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TChart_in_Grid, Chart_in_Grid);
  Application.Run;
end.
