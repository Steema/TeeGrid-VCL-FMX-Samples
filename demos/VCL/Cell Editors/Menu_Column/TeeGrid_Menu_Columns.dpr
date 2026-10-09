program TeeGrid_Menu_Columns;

uses
  Vcl.Forms,
  Unit_Menu_Column in 'Unit_Menu_Column.pas' {Form27},
  Tee.Grid.OptionRender in 'Tee.Grid.OptionRender.pas';

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm27, Form27);
  Application.Run;
end.
