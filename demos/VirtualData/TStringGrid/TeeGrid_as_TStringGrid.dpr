program TeeGrid_as_TStringGrid;

uses
  Vcl.Forms,
  Unit_StringGrid in 'Unit_StringGrid.pas' {StringGridForm1};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TStringGridForm, StringGridForm1);
  Application.Run;
end.
