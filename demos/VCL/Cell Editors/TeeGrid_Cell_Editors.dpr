program TeeGrid_Cell_Editors;

uses
  Vcl.Forms,
  Unit_Editors in 'Unit_Editors.pas' {FormCellEditors},
  Unit_Utils in 'Unit_Utils.pas',
  Unit_Example_Data in 'Unit_Example_Data.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormCellEditors, FormCellEditors);
  Application.Run;
end.
