program TeeGrid_VCL_Ticker;

uses
  Vcl.Forms,
  Unit_Ticker in 'Unit_Ticker.pas' {TickerForm};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTickerForm, TickerForm);
  Application.Run;
end.
