unit Unit_Sample;

interface

uses
  Tee.Grid.Data.Strings;

procedure FillSamples(const AData:TStringsData);

implementation

uses
  System.SysUtils;

function RandomName:String;
const Names:Array[0..9] of String=(
  'Anne','Paul','Chris','Mike','Julia','Sean','Robert','Pamela','Peter','Sara'
);
begin
  result:=Names[Random(Length(Names))];
end;

function RandomAddress:String;
const Addresses:Array[0..9] of String=(
  '15th St.','Spring Ave.','23 Boulevard','2388th Sky Highway',
  '14th Dim Road','1st Rock Lane','87th Lincoln St.','472nd Creek Way',
  '95th Green Lake','7411 Palm Road'
);
begin
  result:=Addresses[Random(Length(Addresses))];
end;

function RandomCountry:String;
const Countries:Array[0..9] of String=(
  'Brazil','Poland','Australia','Kenya',
  'Indonesia','Chile','France','Catalonia',
  'Canada','Norway'
);
begin
  result:=Countries[Random(Length(Countries))];
end;

procedure FillSamples(const AData:TStringsData);
var row : Integer;
begin
  // Column headers
  AData.Headers[0]:='ID';
  AData.Headers[1]:='Name';
  AData.Headers[2]:='Address';
  AData.Headers[3]:='Country';
  AData.Headers[4]:='Postal Code';

  // Cells
  for row:=0 to AData.Rows-1 do
  begin
    AData[0,row]:=IntToStr(row);
    AData[1,row]:=RandomName;
    AData[2,row]:=RandomAddress;
    AData[3,row]:=RandomCountry;
    AData[4,row]:=IntToStr(Random(99999));
  end;
end;

end.
