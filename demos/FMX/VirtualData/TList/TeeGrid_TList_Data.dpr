program TeeGrid_TList_Data;

uses
  System.StartUpCopy,
  FMX.Types,
  FMX.Forms,
  Unit_TList in 'Unit_TList.pas' {FormGridTList},
  Unit_MyData in '..\Unit_MyData.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

//  GlobalUseGPUCanvas:=True;
  Application.Initialize;
  Application.CreateForm(TFormGridTList, FormGridTList);
  Application.Run;
end.
