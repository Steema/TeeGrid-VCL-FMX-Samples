program TeeGrid_Row_SubTitles;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Row_SubTitles in 'Unit_Row_SubTitles.pas' {Form43},
  Unit_Sample in 'Unit_Sample.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm43, Form43);
  Application.Run;
end.
