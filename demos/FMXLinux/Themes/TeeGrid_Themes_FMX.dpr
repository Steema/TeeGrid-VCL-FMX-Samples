program TeeGrid_Themes_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_FMX_Themes in 'Unit_FMX_Themes.pas' {FormGridThemes},
  Unit_MyData in '..\..\VirtualData\Unit_MyData.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TFormGridThemes, FormGridThemes);
  Application.Run;
end.
