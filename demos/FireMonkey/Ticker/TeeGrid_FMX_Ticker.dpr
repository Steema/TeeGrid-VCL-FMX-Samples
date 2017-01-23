program TeeGrid_FMX_Ticker;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  Unit_FMX_Ticker in 'Unit_FMX_Ticker.pas' {TickerForm};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
//  GlobalUseGPUCanvas:=True;
  Application.Initialize;
  Application.CreateForm(TTickerForm, TickerForm);
  Application.Run;
end.
