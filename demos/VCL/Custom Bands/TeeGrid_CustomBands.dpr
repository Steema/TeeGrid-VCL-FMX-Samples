program TeeGrid_CustomBands;

uses
  Vcl.Forms,
  Unit_Custom_Bands in 'Unit_Custom_Bands.pas' {CustomBandForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCustomBandForm, CustomBandForm);
  Application.Run;
end.
