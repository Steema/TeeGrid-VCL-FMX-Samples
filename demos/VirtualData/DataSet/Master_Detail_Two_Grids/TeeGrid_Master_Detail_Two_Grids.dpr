program TeeGrid_Master_Detail_Two_Grids;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Master_Detail_Two_Grids in 'Unit_Master_Detail_Two_Grids.pas' {Form54};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm54, Form54);
  Application.Run;
end.
