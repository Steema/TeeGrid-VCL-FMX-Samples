program TeeBI_Customer_Orders;

uses
  Vcl.Forms,
  Unit_Customer_Orders in 'Unit_Customer_Orders.pas' {FormDetailRows};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormDetailRows, FormDetailRows);
  Application.Run;
end.
