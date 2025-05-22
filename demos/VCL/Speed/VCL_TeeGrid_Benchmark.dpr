program VCL_TeeGrid_Benchmark;

uses
  Vcl.Forms,
  Unit_Test_Speed in 'Unit_Test_Speed.pas' {FormSpeed};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSpeed, FormSpeed);
  Application.Run;
end.
