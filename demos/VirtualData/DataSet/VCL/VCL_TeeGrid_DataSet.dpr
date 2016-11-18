program VCL_TeeGrid_DataSet;

uses
  Vcl.Forms,
  Unit_Dataset in 'Unit_Dataset.pas' {FormGridDataset};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormGridDataset, FormGridDataset);
  Application.Run;
end.
