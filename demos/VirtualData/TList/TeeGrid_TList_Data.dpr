program TeeGrid_TList_Data;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_TList in 'Unit_TList.pas' {FormGridTList},
  Unit_MyData in '..\Unit_MyData.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormGridTList, FormGridTList);
  Application.Run;
end.
