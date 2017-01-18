unit Unit_VirtualMode;

{
  Example using TeeGrid in pure "Virtual Mode".

  Data is supplied using OnGetValue and OnSetValue events

}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  Tee.Grid.Data.Strings, FMXTee.Control, FMXTee.Grid, Tee.Grid.Columns;

type
  TFormVirtualMode = class(TForm)
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    Data : TVirtualModeData;

    procedure GetCell(Sender:TObject; const AColumn:TColumn; const ARow:Integer; var AValue:String);
    procedure SetCell(Sender:TObject; const AColumn:TColumn; const ARow:Integer; var AValue:String);
  public
    { Public declarations }
  end;

var
  FormVirtualMode: TFormVirtualMode;

implementation

{$R *.fmx}

procedure TFormVirtualMode.FormCreate(Sender: TObject);
var t : Integer;
begin
  // Important:
  // Passing an optional default column width (60) means the grid will not need
  // to calculate it, which is much faster.

  // Create data, with 10 columns, lots of rows, (optionally: a default column width = 60)
  Data:=TVirtualModeData.Create(10,20000,60);

  // Column Headers
  for t:=0 to Data.Columns-1 do
      Data.Headers[t]:=IntToStr(t);

  // Virtual Mode events
  Data.OnGetValue:=GetCell;
  Data.OnSetValue:=SetCell;

  // Set Data to Grid
  TeeGrid1.Data:=Data;
end;

procedure TFormVirtualMode.GetCell(Sender: TObject; const AColumn:TColumn; const ARow: Integer;
  var AValue: String);
begin
  // Return AValue from your data

  // In this example, its just a calculated string:

  AValue:=IntToStr(Data.IndexOf(AColumn))+' x '+IntToStr(ARow);
end;

procedure TFormVirtualMode.SetCell(Sender: TObject; const AColumn:TColumn; const ARow: Integer;
  var AValue: String);
begin
  // Called when the cell has been manually edited, or when a Data cell is changed.

  // Store the new AValue in your real data
end;

end.
