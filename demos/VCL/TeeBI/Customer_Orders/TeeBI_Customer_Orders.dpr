program TeeBI_Customer_Orders;

uses
  Vcl.Forms,
  Unit_Customer_Orders in 'Unit_Customer_Orders.pas' {FormDetailRows},
  Tee.Grid.Data.Rtti in '..\..\..\..\..\Tee.Grid.Data.Rtti.pas',
  Tee.Grid.Rows in '..\..\..\..\..\Tee.Grid.Rows.pas',
  Tee.Grid.Selection in '..\..\..\..\..\Tee.Grid.Selection.pas';

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
