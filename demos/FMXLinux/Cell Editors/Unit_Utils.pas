unit Unit_Utils;

// Some helper methods for the TeeGrid Cell Editors demo

interface

uses
  FMXTee.Grid, FMX.ListBox;

// Set all grid columns EditorClass to nil (to default using a TEdit control)
procedure ResetCustomEditors(const AGrid:TTeeGrid);

// Add all Values to ACombo, and try to find ASelected to set ItemIndex
procedure FillCombo(const ACombo:TComboBox; const Values:Array of String; const ASelected:String);

implementation

uses
  {System.}Classes, {System.}SysUtils,

  Tee.Grid.Columns;

// Set all grid columns EditorClass to nil (to default using a TEdit control)
procedure ResetCustomEditors(const AGrid:TTeeGrid);

  procedure ResetColumns(const AColumns:TColumns);
  var t : Integer;
  begin
    for t:=0 to AColumns.Count-1 do
    begin
      AColumns[t].EditorClass:=nil; // <-- reset to default TEdit box

      ResetColumns(AColumns[t].Items);
    end;
  end;

begin
  ResetColumns(AGrid.Columns);
end;

// Add all Values to ACombo, and try to find ASelected to set ItemIndex
procedure FillCombo(const ACombo:TComboBox; const Values:Array of String; const ASelected:String);

  function FindValue(const AValue:String):Integer;
  var t : Integer;
  begin
    for t:=0 to ACombo.Items.Count-1 do
        if SameText(ACombo.Items[t],AValue) then
           Exit(t);

    result:=-1;
  end;

var t : Integer;
begin
  ACombo.Items.BeginUpdate;
  try
    ACombo.Clear;

    for t:=Low(Values) to High(Values) do
        ACombo.Items.Add(Values[t]);
  finally
    ACombo.Items.EndUpdate;
  end;

  ACombo.ItemIndex:=FindValue(ASelected);
end;

end.
