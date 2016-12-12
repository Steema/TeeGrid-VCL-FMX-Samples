program TeeGrid_Locked_Columns;

uses
  Vcl.Forms,
  Unit_Locked_Columns in 'Unit_Locked_Columns.pas' {FormLocked};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormLocked, FormLocked);
  Application.Run;
end.
