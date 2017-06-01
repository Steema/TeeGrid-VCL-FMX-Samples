program TeeGrid_FMX_StringGrid;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_FMX_StringGrid in 'Unit_FMX_StringGrid.pas' {StringGridForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TStringGridForm, StringGridForm);
  Application.Run;
end.
