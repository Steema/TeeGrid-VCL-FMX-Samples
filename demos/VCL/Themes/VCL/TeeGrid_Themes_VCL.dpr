program TeeGrid_Themes_VCL;

uses
  Vcl.Forms,
  Unit_Themes in 'Unit_Themes.pas' {FormGridThemes},
  Unit_MyData in '..\..\..\VirtualData\Unit_MyData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormGridThemes, FormGridThemes);
  Application.Run;
end.
