program TeeGrid_SparkLines;

uses
  Vcl.Forms,
  UnitMain in 'UnitMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
