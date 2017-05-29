program TeeGrid_VCL_DataSource;

uses
  Vcl.Forms,
  Unit_DataSource in 'Unit_DataSource.pas' {Form43};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm43, Form43);
  Application.Run;
end.
