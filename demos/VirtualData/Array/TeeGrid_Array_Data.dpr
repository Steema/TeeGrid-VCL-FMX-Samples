program TeeGrid_Array_Data;

uses
  FastMM4,
  Vcl.Forms,
  Unit_Array in 'Unit_Array.pas' {Form43},
  Unit_MyData in '..\Unit_MyData.pas';

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
