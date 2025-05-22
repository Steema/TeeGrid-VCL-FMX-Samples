program TeeGrid_VCL_Row_Heights;

uses
  Vcl.Forms,
  Unit_Row_Heights in 'Unit_Row_Heights.pas' {FormRowHeights};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormRowHeights, FormRowHeights);
  Application.Run;
end.
