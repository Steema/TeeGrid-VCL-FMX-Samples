program TeeGrid_CustomBands;

uses
  Vcl.Forms,
  Unit_Custom_Bands in 'Unit_Custom_Bands.pas' {Form1},
  Tee.Grid.Bands.Columns in '..\..\..\..\..\TeeBee\Sources\Grid\Tee.Grid.Bands.Columns.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
