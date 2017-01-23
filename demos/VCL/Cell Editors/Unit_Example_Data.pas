unit Unit_Example_Data;

interface

uses
  {System.}UITypes, Tee.Grid.Data;

type
  TVehicle=(None,Bicycle,MotorBike,Car,Caravan,Truck,Boat,Plane);

  TPerson=record
  public
    ID : Integer;
    Name : String;
    Height : Single;
    BirthDate : TDateTime;
    Vehicle : TVehicle;
    EyeColor : TColor;
    Happiness : Double;
    Holidays : Boolean;
  end;

function SampleData:TVirtualData;

function ColorOf(const AColor:TColor):String;

implementation

uses
  Tee.Grid.Data.Rtti, {System.}SysUtils;

var
  Persons : TArray<TPerson>;

procedure RandomPerson(var APerson:TPerson);
const
  RandomNames:Array[0..4] of String=('John','Anne','Paul','Mary','Mike');
  RandomColors:Array[0..3] of TColor=(TColors.Black,TColors.Brown,TColors.Green,TColors.Blue);

begin
  APerson.Name:=RandomNames[Random(High(RandomNames))];
  APerson.Height:=5+(Random(100)*0.01);
  APerson.BirthDate:=EncodeDate(1930+Random(80),1+Random(12),1+Random(28));
  APerson.Vehicle:=TVehicle(Random(1+Ord(High(TVehicle))));
  APerson.EyeColor:=RandomColors[Random(Length(RandomColors))];
  APerson.Happiness:=Random(100)*0.01;
  APerson.Holidays:=Random(100)<50;
end;

procedure CreatePersons;
var t : Integer;
begin
  SetLength(Persons,10);

  for t:=0 to High(Persons) do
  begin
    Persons[t].ID:=100+t;

    RandomPerson(Persons[t]);
  end;
end;

function SampleData:TVirtualData;
begin
  CreatePersons;

  result:=TVirtualArrayData<TPerson>.Create(Persons);
end;

function ColorOf(const AColor:TColor):String;
begin
  case AColor of
    TColors.Black : result:='Black';
    TColors.Brown : result:='Brown';
    TColors.Green : result:='Green';
  else
    result:='Blue';
  end;
end;

end.
