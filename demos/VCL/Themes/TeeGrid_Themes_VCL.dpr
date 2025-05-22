program TeeGrid_Themes_VCL;

uses
  Vcl.Forms,
  Unit_Themes in 'Unit_Themes.pas' {FormGridThemes},
  Unit_MyData in '..\..\VirtualData\Unit_MyData.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'TeeGrid - VCL Themes Example';
  TStyleManager.TrySetStyle('Aqua Graphite');
  Application.CreateForm(TFormGridThemes, FormGridThemes);
  Application.Run;
end.
