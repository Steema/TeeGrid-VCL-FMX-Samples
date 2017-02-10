program TeeGrid_Matrix_Data;

uses
  Vcl.Forms,
  Unit_Matrix_Data in 'Unit_Matrix_Data.pas' {FormMatrixGrid};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMatrixGrid, FormMatrixGrid);
  Application.Run;
end.
