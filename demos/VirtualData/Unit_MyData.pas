unit Unit_MyData;

interface

{
   Example of different data types used as sample data in TeeGrid demos.
}

uses
  {System.}Classes, {System.}Generics.Collections;

type
  TAddress=record
  public
    Street : String;
    Number : Integer;
  end;

  TPerson=record
  public
    Name : String;
    Address : TAddress;   // <-- sub-record
    BirthDate : TDateTime;
    Children : Integer;
    IsDeveloper : Boolean;
    Height : Single;
  end;

  TCar=class(TPersistent)
  private
    FSpeed : Double;
  public
    Name : String;
    Wheels : Word;
    Driver : TPerson;

    property Speed:Double read FSpeed write FSpeed;
  end;

// Sets random values to all fields in AData

procedure FillMyData(var AData:Array of TPerson); overload;
procedure FillMyData(const AData:TList<TPerson>; const ACount:Integer); overload;

procedure FillMyData(var AData:Array of TCar); overload;
procedure FillMyData(const AData:TList<TCar>; const ACount:Integer); overload;

implementation

uses
  {System.}SysUtils;

function RandomName:String;
begin
  case Random(5) of
    0: result:='Anne';
    1: result:='Mike';
    2: result:='Paula';
    3: result:='Peter';
    4: result:='Linda';
  else
    result:='Bob';
  end;
end;

function RandomAddress:String;
begin
  case Random(5) of
    0: result:='Lamps Ave';
    1: result:='Caps St';
    2: result:='Plain Rd';
    3: result:='56th St';
    4: result:='Sand Rock St';
  else
    result:='Moon Ave';
  end;
end;

function RandomDate:TDateTime;
begin
  result:=EncodeDate(1980+Random(20),1+Random(12),1+Random(28));
end;

procedure RandomPerson(var APerson:TPerson);
begin
  APerson.Name:=RandomName;
  APerson.Address.Street:=RandomAddress;
  APerson.Address.Number:=1+Random(1000);
  APerson.BirthDate:=RandomDate;
  APerson.Children:=Random(5);
  APerson.IsDeveloper:=Random(10)<5;
  APerson.Height:=0.1*Random(60);
end;

procedure FillMyData(var AData:Array of TPerson);
var t : Integer;
begin
  for t:=0 to High(AData) do
      RandomPerson(AData[t]);
end;

procedure FillMyData(const AData:TList<TPerson>; const ACount:Integer);
var t : Integer;
    tmp : TPerson;
begin
  for t:=0 to ACount-1 do
  begin
    RandomPerson(tmp);
    AData.Add(tmp);
  end;
end;

function RandomCarName:String;
begin
  case Random(5) of
    0: result:='Ford';
    1: result:='Volvo';
    2: result:='BMW';
    3: result:='Chevrolet';
    4: result:='Tata';
  else
    result:='Hyundai';
  end;
end;

function RandomCar:TCar;
begin
  result:=TCar.Create;

  result.Name:=RandomCarName;

  result.Wheels:=2+Random(3)*2;
  result.Speed:=50+Random(200);

  RandomPerson(result.Driver);
end;

procedure FillMyData(var AData:Array of TCar);
var t : Integer;
begin
  for t:=0 to High(AData) do
      AData[t]:=RandomCar;
end;

procedure FillMyData(const AData:TList<TCar>; const ACount:Integer);
var t : Integer;
begin
  for t:=0 to ACount-1 do
      AData.Add(RandomCar);
end;

end.
