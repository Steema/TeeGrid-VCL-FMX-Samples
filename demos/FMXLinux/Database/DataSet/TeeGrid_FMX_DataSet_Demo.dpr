program TeeGrid_FMX_DataSet_Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Main in 'Unit_Main.pas' {FormGridDataSet};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGridDataSet, FormGridDataSet);
  Application.Run;
end.
