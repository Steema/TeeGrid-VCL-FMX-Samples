program TeeGrid_Custom_Sorting;

uses
  Vcl.Forms,
  Unit_Custom_Sorting in 'Unit_Custom_Sorting.pas' {Form1},
  Unit_MyData in '..\Unit_MyData.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
