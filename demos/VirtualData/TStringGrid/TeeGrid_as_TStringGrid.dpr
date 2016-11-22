program TeeGrid_as_TStringGrid;

uses
  Vcl.Forms,
  Unit_StringGrid in 'Unit_StringGrid.pas' {StringGridForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TStringGridForm, StringGridForm1);
  Application.Run;
end.
