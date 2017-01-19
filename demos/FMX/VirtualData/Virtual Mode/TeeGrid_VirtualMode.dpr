program TeeGrid_VirtualMode;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_VirtualMode in 'Unit_VirtualMode.pas' {FormVirtualMode};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormVirtualMode, FormVirtualMode);
  Application.Run;
end.
