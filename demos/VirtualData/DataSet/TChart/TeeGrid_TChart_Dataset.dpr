program TeeGrid_TChart_Dataset;

uses
  Forms,
  Unit_TeeGrid_TChart_Dataset in 'Unit_TeeGrid_TChart_Dataset.pas' {Form224};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm224, Form224);
  Application.Run;
end.
