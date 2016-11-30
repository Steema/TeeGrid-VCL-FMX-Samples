program TeeGrid_VCL_Row_Heights;

uses
  Vcl.Forms,
  Unit_Row_Heights in 'Unit_Row_Heights.pas' {Form43};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm43, Form43);
  Application.Run;
end.
