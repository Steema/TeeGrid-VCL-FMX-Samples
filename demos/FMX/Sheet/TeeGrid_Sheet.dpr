program TeeGrid_Sheet;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Sheet in 'Unit_Sheet.pas' {FormSheet},
  FMXTee.Sheet.Tools in 'FMXTee.Sheet.Tools.pas' {SheetTools},
  FMXTee.Sheet.Grid in 'FMXTee.Sheet.Grid.pas',
  Tee.Sheet in 'Tee.Sheet.pas',
  FMXTee.Sheet.Expression in 'FMXTee.Sheet.Expression.pas' {SheetExpression},
  FMXTee.Sheet.Editor.Font in 'FMXTee.Sheet.Editor.Font.pas' {SheetFontEditor},
  FMXTee.Font.Family in 'FMXTee.Font.Family.pas',
  Tee.Cell.Expression in 'Tee.Cell.Expression.pas',
  Tee.Sheet.Data in 'Tee.Sheet.Data.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormSheet, FormSheet);
  Application.Run;
end.
