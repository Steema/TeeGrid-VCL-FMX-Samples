program TeeGrid_Array_Data;

uses
  //FastMM4,
  Vcl.Forms,
  Unit_Array in 'Unit_Array.pas' {FormArray},
  Unit_MyData in '..\Unit_MyData.pas';

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
