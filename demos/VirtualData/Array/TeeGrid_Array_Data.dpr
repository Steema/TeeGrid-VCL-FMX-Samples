program TeeGrid_Array_Data;

uses
  Vcl.Forms,
  Unit_Array in 'Unit_Array.pas' {FormArray},
  Unit_MyData in '..\Unit_MyData.pas',
  VCLTee.Grid.About in '..\..\..\..\Packages\VCLTee.Grid.About.pas' {VCLGridAbout};

{$R *.res}

begin
  {$IFOPT D+}
  ReportMemoryLeaksOnShutdown:=True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormArray, FormArray);
  Application.Run;
end.
