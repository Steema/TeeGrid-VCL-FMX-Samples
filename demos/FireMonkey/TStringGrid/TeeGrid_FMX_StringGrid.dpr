program TeeGrid_FMX_StringGrid;

uses
  System.StartUpCopy,
  FMX.Forms,
  {$IF CompilerVersion>35}
//  FMX.Skia,
  {$IFEND}
  Unit_FMX_StringGrid in 'Unit_FMX_StringGrid.pas' {StringGridForm};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}

  {$IF CompilerVersion>35}
//  GlobalUseSkia := True;
  {$IFEND}

  Application.Initialize;
  Application.CreateForm(TStringGridForm, StringGridForm);
  Application.Run;
end.
