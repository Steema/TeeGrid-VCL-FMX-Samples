program TeeGrid_Pictures;

uses
  Vcl.Forms,
  Unit_Pictures in 'Unit_Pictures.pas' {Form4};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
