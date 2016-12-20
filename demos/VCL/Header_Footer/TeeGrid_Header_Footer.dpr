program TeeGrid_Header_Footer;

uses
 // FastMM4,
  Vcl.Forms,
  Unit_Header_Footer in 'Unit_Header_Footer.pas' {FormHeaderFooter};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormHeaderFooter, FormHeaderFooter);
  Application.Run;
end.
