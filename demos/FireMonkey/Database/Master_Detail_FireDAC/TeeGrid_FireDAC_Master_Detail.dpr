program TeeGrid_FireDAC_Master_Detail;

uses
  System.StartUpCopy,
  FMX.Forms,
  Master_Detail_FireDAC in 'Master_Detail_FireDAC.pas' {MasterDetail},
  Customer_Orders in 'Customer_Orders.pas' {SampleData: TDataModule};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TSampleData, SampleData);
  Application.CreateForm(TMasterDetail, MasterDetail);
  Application.Run;
end.
