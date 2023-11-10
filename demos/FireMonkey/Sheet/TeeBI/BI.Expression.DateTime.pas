unit BI.Expression.DateTime;

// Optimized date-time functions for speed

// See repository:
// https://github.com/davidberneda/FastDateTime

interface

// Enable / Disable using optimized date-time functions
{$DEFINE FASTDATE}

{$IFDEF FASTDATE}

 {$IFDEF CPUX64}

  // 64bit

  {$IFDEF FPC}
   {$UNDEF FASTDATE} // FPC 64bit RTL is faster !
  {$ELSE}
   {$DEFINE FASTDAYOF}
   {$DEFINE FASTMONTHOF}
   {$DEFINE FASTDAYOFYEAR}
   {$DEFINE FASTYEAROF}
  {$ENDIF}
 {$ELSE}

  // 32bit

  {$IFNDEF FPC}
   {$DEFINE FASTDAYOF}
   {$DEFINE FASTMONTHOF}
  {$ENDIF}

  {$DEFINE FASTDAYOFYEAR}
  {$DEFINE FASTYEAROF}
{$ENDIF}

{$ENDIF}

type
  TBIDateTime=record
  private
    {$IFDEF FASTDATE}
    const
      D1 = 365;
      D4 = D1 * 4 + 1;
      D100 = D4 * 25 - 1;
      D400 = D100 * 4 + 1;

    class function CalcDayOfYear(T: Integer; out Y:Word): Word; static;
    class function CalcLeap(T: Integer; out D:Word): Boolean; inline; static;
    class function DateTimeToDateStamp(const DateTime: TDateTime): Integer; static;
    class function DayMonth(const T:Integer; out M:Byte): Word; overload; static;
    {$ENDIF}
  public
    class function DayOf(const DateTime: TDateTime): Word; {$IFNDEF FASTDAYOF}inline;{$ENDIF} static;
    class function DayOfTheYear(const DateTime: TDateTime):Word; {$IFNDEF FASTDAYOFYEAR}inline;{$ENDIF} static;
    class function MonthOf(const DateTime: TDateTime): Byte; {$IFNDEF FASTMONTHOF}inline;{$ENDIF} static;
    class function YearOf(const DateTime: TDateTime): Word; {$IFNDEF FASTYEAROF}inline;{$ENDIF} static;
  end;

implementation

uses
  {$IFNDEF FASTDATE}
  {System.}DateUtils,
  {$ELSE}
  {$IFDEF FPC}
  DateUtils,
  {$ENDIF}
  {$ENDIF}
  {System.}Math, {System.}SysUtils;

{ TBIDateTime }

{$IFDEF FASTDATE}
class function TBIDateTime.CalcDayOfYear(T: Integer; out Y:Word): Word;
var I : Word;
begin
  Dec(T);

  //Y:=400*(T div D400);
  //T:=T mod D400;

  Y := 1;

  while T >= D400 do
  begin
    Dec(T, D400);
    Inc(Y, 400);
  end;

  DivMod(T, D100, I, result);

  if I>0 then
  begin
    if I = 4 then
    begin
      Dec(I);
      Inc(result, D100);
    end;

    Inc(Y, I * 100);
  end;

  DivMod(result, D4, I, result);

  if I>0 then
     Inc(Y, I * 4);

  DivMod(result, D1, I, result);

  if I>0 then
  begin
    if I = 4 then
    begin
      Dec(I);
      Inc(result, D1);
    end;

    Inc(Y, I);
  end;
end;

class function TBIDateTime.CalcLeap(T: Integer; out D:Word): Boolean;
var Y : Word;
begin
  D:=CalcDayOfYear(T,Y);
  result:=IsLeapYear(Y);
end;

class function TBIDateTime.DateTimeToDateStamp(const DateTime: TDateTime): Integer;
const
  FMSecsPerDay: Single = MSecsPerDay;
  IMSecsPerDay: Integer = MSecsPerDay;

begin
  Result := DateDelta + (Round(DateTime * FMSecsPerDay) div IMSecsPerDay);
end;

{$DEFINE LOOKUP}

{$IFDEF LOOKUP}
type
  TLeapLookup=record
  public
    const Month:Array[0..365] of Byte=
      (
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
        9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
        11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
        12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
      );

    const Day:Array[0..365] of Byte=
      (
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
      );
  end;

  TLookup=record
  public
    const Month:Array[0..364] of Byte=
      (
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
        4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
        9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
        10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
        11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
        12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12
      );

    const Day:Array[0..364] of Byte=
      (
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
      );
  end;

{$ENDIF}


class function TBIDateTime.DayMonth(const T: Integer; out M:Byte): Word;
{$IFNDEF LOOKUP}
type
  TDayTable = array[1..12] of Byte;

const
  MonthDays: TDayTable = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  LeapMonthDays: TDayTable = (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

var I: Byte;
{$ENDIF}
begin
  {$IFNDEF LOOKUP}
  M:=1;
  {$ENDIF}

  if CalcLeap(T,result) then
  begin
    {$IFDEF LOOKUP}
    M:=TLeapLookup.Month[result];
    result:=TLeapLookup.Day[result];
    {$ELSE}
    while True do
    begin
      I := LeapMonthDays[M];
      if result < I then Break;
      Dec(result, I);
      Inc(M);
    end;
    {$ENDIF}
  end
  else
  begin
    {$IFDEF LOOKUP}
    M:=TLookup.Month[result];
    result:=TLookup.Day[result];
    {$ELSE}
    while True do
    begin
      I := MonthDays[M];
      if result < I then Break;
      Dec(result, I);
      Inc(M);
    end;
    {$ENDIF}
  end;

  {$IFNDEF LOOKUP}
  Inc(result);
  {$ENDIF}
end;
{$ENDIF}

class function TBIDateTime.DayOf(const DateTime: TDateTime): Word;
{$IFDEF FASTDAYOF}
var Date : Integer;
    M: Byte;
{$ENDIF}
begin
  {$IFDEF FASTDAYOF}
  Date:=DateTimeToDateStamp(DateTime);

  if Date <= 0 then
     result:= 0
  else
     result:=DayMonth(Date,M);

  {$ELSE}
  result:={$IFNDEF FPC}System.{$ENDIF}DateUtils.DayOf(DateTime);
  {$ENDIF}
end;

class function TBIDateTime.MonthOf(const DateTime: TDateTime): Byte;
{$IFDEF FASTMONTHOF}
var Date : Integer;
{$ENDIF}
begin
  {$IFDEF FASTMONTHOF}
  Date:=DateTimeToDateStamp(DateTime);

  if Date <= 0 then
     result:= 0
  else
     DayMonth(Date,result);

  {$ELSE}
  result:={$IFNDEF FPC}System.{$ENDIF}DateUtils.MonthOf(DateTime);
  {$ENDIF}
end;

class function TBIDateTime.DayOfTheYear(const DateTime: TDateTime):Word;
{$IFDEF FASTDAYOFYEAR}
var Date : Integer;
    Y: Word;
{$ENDIF}
begin
  {$IFDEF FASTDAYOFYEAR}
  Date:=DateTimeToDateStamp(DateTime);

  if Date <= 0 then
     result:= 0
  else
     result:=CalcDayOfYear(Date,Y)+1;

  {$ELSE}
  result:={$IFNDEF FPC}System.{$ENDIF}DateUtils.DayOfTheYear(DateTime);
  {$ENDIF}
end;

class function TBIDateTime.YearOf(const DateTime: TDateTime): Word;
{$IFDEF FASTYEAROF}
var Date : Integer;
    D, I: Word;
{$ENDIF}
begin
  {$IFDEF FASTYEAROF}
  Date:=DateTimeToDateStamp(DateTime);

  if Date <= 0 then
     result:= 0
  else
  begin
    Dec(Date);
    result := 1;

    while Date >= D400 do
    begin
      Dec(Date, D400);
      Inc(result, 400);
    end;

    DivMod(Date, D100, I, D);

    if I>0 then
    begin
      if I = 4 then
      begin
        Dec(I);
        Inc(D, D100);
      end;

      Inc(result, I * 100);
    end;

    DivMod(D, D4, I, D);

    if I>0 then
       Inc(result, I * 4);

    I := D div D1;

    if I>0 then
    begin
      if I = 4 then
         Dec(I);

      Inc(result, I);
    end;
  end;
  {$ELSE}
  result:={$IFNDEF FPC}System.{$ENDIF}DateUtils.YearOf(DateTime);
  {$ENDIF}
end;

end.
