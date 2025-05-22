program TeeGrid_FMX_Cell_Editors;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Editors in 'Unit_Editors.pas' {FormCellEditors},
  Unit_Example_Data in 'Unit_Example_Data.pas',
  Unit_Utils in 'Unit_Utils.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormCellEditors, FormCellEditors);
  Application.Run;
end.
