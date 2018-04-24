program TeeGrid_Join_Two_Tables;

uses
  Vcl.Forms,
  Unit_Join_Two_Tables in 'Unit_Join_Two_Tables.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
