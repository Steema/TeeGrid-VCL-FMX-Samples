program TeeGrid_FMX_Ticker;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_FMX_Ticker in 'Unit_FMX_Ticker.pas' {TickerForm};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TTickerForm, TickerForm);
  Application.Run;
end.
