{*********************************************}
{  TeeBI Software Library                     }
{  TStringArray and helper type               }
{  Copyright (c) 2015-2018 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Arrays.Strings;

interface

// TStringArray is a lightweight "array of string",
// similar to BI.Arrays TTextArray type.

// Main purpose of TStringArray is to implement a "Split" function syntax
// compatible with all versions of both FreePascal and Delphi (VCL and FMX)

type
  TStringArray=Array of String;

  TStringArrayHelper=record helper for TStringArray
  public
    procedure Add(const S:String);
    function Count:Integer; inline;

    class function Split(const S,Delimiter:String):TStringArray; overload; static;
    class function Split(const S:String; const Delimiter:Char):TStringArray; overload; static;
    class function Split(const S:String; const Delimiter,Quote:Char):TStringArray; overload; static;
  end;

  TArrayOfStrings=TStringArray;

implementation

uses
  {System.}SysUtils;

{$IFDEF FPC}
//  WordCount / ExtractWord cannot be used, as they skip empty items
function SplitString(const S,Delimiter:String):TStringArray;
var i,N : Integer;
    tmp : String;
begin
  N:=0;

  tmp:=S;

  while tmp<>'' do
  begin
    i:=Pos(Delimiter,tmp);

    if i>0 then
    begin
      SetLength(result,N+1);
      result[N]:=Copy(tmp,1,i-1);
      Inc(N);
      Delete(tmp,1,i+Length(Delimiter)-1);
    end
    else
    begin
      SetLength(result,N+1);
      result[N]:=tmp;
      tmp:='';
    end;
  end;
end;

{$ELSE}

{$IF CompilerVersion<29}
// Twice faster than TStringHelper.Split, but less perfect.
function SplitString(const AString,ADelimiter,AQuote:String):TStringArray;
var
  tmp : String;
  Pos : Integer;

  procedure AddItem;
  begin
    Inc(Pos);

    if Length(result)<Pos+1 then
       SetLength(result,Pos+100);

    result[Pos]:=tmp;
  end;

var L,
    i : Integer;
    InQuote : Boolean;
    c : Char;
begin
  L:=Length(AString);

  if L=0 then
     result:=nil
  else
  begin
    i:=1;
    Pos:=-1;
    InQuote:=False;
    tmp:='';

    while i<=L do
    begin
      c:=AString[i];

      if c=AQuote then
      begin
        InQuote:=not InQuote;

        if InQuote then
           tmp:='';
      end
      else
      if (not InQuote) and (c=ADelimiter) then
      begin
        AddItem;

        if ADelimiter=' ' then
           while i+1<=L do
              if AString[i+1]=ADelimiter then
                 Inc(i)
              else
                 break;

        tmp:='';
      end
      else
        tmp:=tmp+AString[i];

      Inc(i);
    end;

    if tmp<>'' then
       AddItem;

    if Length(result)<>Pos+1 then
       SetLength(result,Pos+1);
  end;
end;
{$ENDIF}
{$ENDIF}

{ TStringArrayHelper }

function TStringArrayHelper.Count:Integer;
begin
  result:=Length(Self);
end;

class function TStringArrayHelper.Split(const S,Delimiter:String):TStringArray;
begin
  {$IFDEF FPC}
  result:=SplitString(S,Delimiter);
  {$ELSE}
  result:=TStringArray(S.Split([Delimiter] {$IF CompilerVersion<29},TStringSplitOptions.None{$ENDIF}));
  {$ENDIF}
end;

class function TStringArrayHelper.Split(const S: String;
  const Delimiter: Char): TStringArray;
begin
  {$IFDEF FPC}
  result:=SplitString(S,Delimiter);
  {$ELSE}
  result:=TStringArray(S.Split([Delimiter]));
  {$ENDIF}
end;

class function TStringArrayHelper.Split(const S: String;
  const Delimiter,Quote: Char): TStringArray;
begin
  {$IFDEF FPC}
  result:=SplitString(S,Delimiter {,Quote}); // Pending !
  {$ELSE}
  {$IF CompilerVersion>28}
  result:=TStringArray(S.Split([Delimiter],Quote));
  {$ELSE}
  result:=SplitString(S,Delimiter,Quote);
  {$ENDIF}
  {$ENDIF}
end;

procedure TStringArrayHelper.Add(const S: String);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);
  Self[L]:=S;
end;

end.

