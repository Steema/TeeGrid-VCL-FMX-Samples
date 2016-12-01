program TeeBI_Customer_Orders;

uses
  Vcl.Forms,
  Unit_Customer_Orders in 'Unit_Customer_Orders.pas' {FormDetailRows},
  VCLTee.Editor.Text.Align in '..\..\..\..\..\VCL\VCLTee.Editor.Text.Align.pas' {TextAlignEditor};

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
