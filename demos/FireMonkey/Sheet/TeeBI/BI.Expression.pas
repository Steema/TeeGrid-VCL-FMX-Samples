{*********************************************}
{  TeeBI Software Library                     }
{  Expression parser and evaluator            }
{  Copyright (c) 2015-2018 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Expression;

{$SCOPEDENUMS ON}

interface

{.$DEFINE BIVARIANT} // Experimental, replacing Variant with a faster alternative

uses
  {$IFDEF BIVARIANT}
  BI.Variants,
  {$ELSE}
  {System.}Variants,
  {$ENDIF}
  {System.}SysUtils;

type
  EExpressionParse=class(Exception)
  public
    Position : Integer;

    Constructor Create(APos:Integer; const AMessage:String);
  end;

  TData={$IFDEF BIVARIANT}BIVariant{$ELSE}Variant{$ENDIF};

  TExpression=class;

  TResolveProc=
    {$IFDEF FPC}
    function(const S:String; IsFunction:Boolean):TExpression of object;
    {$ELSE}
    reference to function(const S:String; IsFunction:Boolean):TExpression;
    {$ENDIF}

  TErrorProc={$IFNDEF FPC}reference to{$ENDIF}
              function(const APos:Integer; const AMessage:String):Boolean
                {$IFDEF FPC}of object{$ENDIF};

  TExpressionProc=procedure(const Item:TExpression) of object;

  TExpression=class abstract
  private

    {$IFDEF FPC}
    function DoError(const APos:Integer; const AMessage:String):Boolean;
    function NilFunction(const S:String; IsFunction:Boolean):TExpression;
    {$ELSE}
    class function DoError(const APos:Integer; const AMessage:String):Boolean; static;
    {$ENDIF}

  public
    class var
      Null:TData;

    procedure Assign(const Source:TExpression); virtual; abstract;
    function AsString:String; virtual;

    class function Clone(const AExpression:TExpression):TExpression; overload; static;

    class function Evaluate(const S:String):TData; static;
    class function FromString(const S:String):TExpression; overload; static;
    class function FromString(const S:String;
                              const Resolve:TResolveProc):TExpression; overload; static;
    class function FromString(const S:String;
                              const Resolve:TResolveProc;
                              const Error:TErrorProc):TExpression; overload; static;

    procedure Traverse(const AProc:TExpressionProc); virtual;
    function Value:TData; virtual; abstract;
  end;

  TExpressionClass=class of TExpression;

  TIntegerExpression=class(TExpression)
  public
    Number : Int64;

    Constructor Create(const AValue:Int64);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TFloatExpression=class(TExpression)
  public
    Number : Extended;

    Constructor Create(const AValue:Extended);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;

    class function E:TFloatExpression; static;
    class function Pi:TFloatExpression; static;
  end;

  TBooleanExpression=class(TExpression)
  public
    Logical : Boolean;

    Constructor Create(const AValue:Boolean);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TDateTimeExpression=class(TExpression)
  public
    DateTime : TDateTime;

    Constructor Create(const AValue:TDateTime);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;

    class function Date:TDateTimeExpression; static;
    class function DateSpan(const ADelta:TDateTime):TDateTimeExpression; static;
    class function Now:TDateTimeExpression; static;
    class function Time:TDateTimeExpression; static;

    class function FromData(const Value:TData):TDateTime; static;
  end;

  TTextExpression=class(TExpression)
  public
    Text : String;

    Constructor Create(const AValue:String);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TExpressions=Array of TExpression;

  TExpressionsHelper=record helper for TExpressions
  public
    procedure Add(const AExpression:TExpression); overload;
    procedure Add(const AExpressions:Array of TExpression); overload;
    function Count:Integer; inline;
    procedure Resize(const ACount:Integer); inline;
  end;

  TArrayExpression=class(TExpression)
  private
    function Get(const Index: Integer): TExpression; inline;
    procedure Put(const Index: Integer; const Value: TExpression); inline;
  public
    Items : TExpressions;
    IsParams : Boolean;

    Constructor Create(const AValues:Array of TExpression); overload;
    Constructor Create(const AValues:Array of TData); overload;
    Destructor Destroy; override;

    procedure Add(const AValue:TExpression);
    procedure Assign(const Source:TExpression); override;
    procedure Clear;
    function Count:Integer; inline;

    function ToString:String; override;
    procedure Traverse(const AProc:TExpressionProc); override;
    function Value:TData; override;

    property Item[const Index:Integer]:TExpression read Get write Put; default;
  end;

  TOperandExpression=class abstract (TExpression)
  protected
    function Part(const AExpression:TExpression):String;
  public
    Left  : TExpression;
    Right : TExpression;

    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;
    procedure Traverse(const AProc:TExpressionProc); override;
  end;

  TArithmeticOperand=(Add,Subtract,Multiply,Divide,&Mod,Power);

  TArithmeticOperandHelper=record helper for TArithmeticOperand
  private
    const
      Texts : Array[TArithmeticOperand] of String=(
                '+','-','*','/','mod','^'
                );
  public
    class function FromString(const S:String; out Operand:TArithmeticOperand):Boolean; static;
    function ToString:String;
  end;

  TArithmeticExpression=class(TOperandExpression)
  private
     // do not inline
    function Add: TData;
    function Subtract: TData;
    function Multiply: TData;
    function Divide: TData;
    function Modulus: TData;
    function Power: TData;
  public
    Operand : TArithmeticOperand;

    Constructor Create(const ALeft:TExpression; const AOperand:TArithmeticOperand; const ARight:TExpression);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TLogicalOperand=(Equal,NotEqual,Greater,Lower,GreaterOrEqual,LowerOrEqual,&And,&Or,&In);

  TLogicalOperandHelper=record helper for TLogicalOperand
  private
    const
      Texts : Array[TLogicalOperand] of String=(
                '=','<>','>','<','>=','<=','and','or','in'
                );
  public
    class function FromString(const S:String; out Operand:TLogicalOperand):Boolean; static;
    function ToString:String;
  end;

  TLogicalExpression=class(TOperandExpression)
  private
    function CalcAnd:Boolean; inline;
    function CalcOr:Boolean; inline;
    function IsEqual:Boolean; inline;
    function IsGreater:Boolean; inline;
    function IsGreaterOrEqual:Boolean; inline;
    function IsLower:Boolean; inline;
    function IsLowerOrEqual:Boolean; inline;
    function IsNotEqual:Boolean; inline;
    function LeftInRight:Boolean;

    type
      TBooleanFunc=function:Boolean of object;

    var
      Funcs:Array[TLogicalOperand] of TBooleanFunc;

    procedure Init;
  public
    Operand : TLogicalOperand;

    Constructor Create; overload;

    Constructor Create(const ALeft:TExpression;
                       const AOperand:TLogicalOperand;
                       const ARight:TExpression); overload;

    procedure Assign(const Source: TExpression); override;
    class function Join(const AOld,ANew:TLogicalExpression;
                        const AOperand:TLogicalOperand=TLogicalOperand.&And):TLogicalExpression; static;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TUnaryExpression=class abstract(TExpression)
  private
    FExpression: TExpression;
  protected
    procedure SetExpression(const Value:TExpression); virtual;
  public
    Constructor Create(const AExpression:TExpression); overload;
    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;
    procedure Traverse(const AProc:TExpressionProc); override;

    property Expression : TExpression read FExpression write SetExpression;
  end;

  TUnaryNotExpression=class(TUnaryExpression)
  public
    function Value:TData; override;
    function ToString:String; override;
  end;

  TParameterExpression=class;

  TParameterExpressionClass=class of TParameterExpression;

  TParameterExpressions=Array of TParameterExpressionClass;

  TParameterExpressionsHelper=record helper for TParameterExpressions
  public
    procedure Add(const AClass:TParameterExpressionClass);
    function IndexOf(const AClass:TParameterExpressionClass):Integer;
    procedure Remove(const AClass:TParameterExpressionClass);
  end;

  TParameterExpression=class(TUnaryExpression)
  private
    class function Guess(const Token:String):TParameterExpression; static;
  protected
    FParams : TArrayExpression;

    function ResultClass:TExpressionClass; virtual; abstract;
    procedure SetExpression(const Value:TExpression); override;
  public
    class var
      Registered : TParameterExpressions;

    Constructor Create(const AParameters:Array of TData); overload;

    class function Call(const AParameters:TExpressions):TExpression; static;

    class function FromString(const S:String):TParameterExpression; virtual;
    function ToString:String; override;

    property Parameters:TArrayExpression read FParams;
  end;

  TUnaryDateTimeExpression=class(TParameterExpression)
  protected
    function ResultClass:TExpressionClass; override;
  end;

  TDateExpression=class(TUnaryDateTimeExpression)
  public
    class function FromData(const Value:TData):TDateTime; static;
    class function FromString(const S:String):TParameterExpression; override;
    function ToString:String; override;
    function Value:TData; override;

    class function Today:TDateExpression; static;
    class function Tomorrow:TDateExpression; static;
    class function Yesterday:TDateExpression; static;
  end;

  TTimeExpression=class(TUnaryDateTimeExpression)
  public
    class function FromData(const Value:TData):TDateTime; static;
    class function FromString(const S:String):TParameterExpression; override;
    function ToString:String; override;
    function Value:TData; override;
  end;

  TMathOperand=(Abs,Sin,Cos,Tan,Sqr,Sqrt,Log,Ln,Exp,Round,Trunc,Power,Sign);

  TMathOperandHelper=record helper for TMathOperand
  private
    const
      Texts : Array[TMathOperand] of String=(
                'Abs','Sin','Cos','Tan','Sqr','Sqrt','Log','Ln','Exp',
                'Round','Trunc','Power','Sign'
              );
  public
    class function FromString(const S:String; out Operand:TMathOperand):Boolean; static;
    function ToString:String;
  end;

  TMathExpression=class(TParameterExpression)
  protected
    function ResultClass:TExpressionClass; override;
    procedure SetExpression(const Value:TExpression); override;
  public
    Operand : TMathOperand;

    procedure Assign(const Source:TExpression); override;
    class function FromString(const S:String):TParameterExpression; override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TDateTimeSpan=(None,
                 //Nanosecond,
                 //Microsecond,
                 Millisecond,
                 HundredsOfSecond,
                 TenthsOfSecond,
                 Second,
                 Minute,
                 QuarterHour,
                 Hour,
                 Day,
                 Week,
                 // Fortnight, // 14 days
                 Month,
                 Quarter,
                 Year,
                 Decade,
                 Century,
                 Millennium);

  TDateTimePart=(None,
                 //Nanosecond,
                 //Microsecond,
                 Millisecond,
                 HundredsOfSecond,
                 TenthsOfSecond,
                 Second,
                 Minute,
                 QuarterHour,
                 Hour,
                 DayOfMonth,
                 DayOfYear,
                 WeekOfYear,
                 WeekDay,
                 ShortWeekDayName,
                 LongWeekDayName,
                 Month,
                 ShortMonthName,
                 LongMonthName,
                 Quarter,
                 Year,
                 Decade,
                 DecadeOfYear,
                 Century,
                 Millennium);

  TDateTimePartHelper=record helper for TDateTimePart
  private
    const
      Texts : Array[TDateTimePart] of String=(
               'None',
               //Nanosecond,
               //Microsecond,
               'Millisecond',
               'Hundred of second',
               'Tenth of second',
               'Second',
               'Minute',
               'Quarter-Hour',
               'Hour',
               'Day',
               'Day of Year',
               'Week',
               'Weekday',
               'Weekday short',
               'Weekday long',
               'Month',
               'Month short',
               'Month long',
               'Quarter',
               'Year',
               'Decade',
               'Decade of Year',
               'Century',
               'Millennium'
             );
  public
    class
      var QuarterFormat:String;

    class function AllToText:String; static;
    function AsString(const Index:Integer):String;
    class function FromString(const S:String; out APart:TDateTimePart):Boolean; static;
    function High:Integer;
    function Low: Integer;
    class function Max:Integer; static;
    function ToString:String; overload;
    class function ToString(const Index:Integer):String; overload; inline; static;
    class function ToCode(const Index:Integer):String; static;
  end;

  TDateTimePartExpression=class(TParameterExpression)
  private
    function AsInteger(const AValue:TDateTime):Integer;
  protected
    function ResultClass:TExpressionClass; override;
  public
    Part : TDateTimePart;

    procedure Assign(const Source:TExpression); override;
    class function FromString(const S:String):TParameterExpression; override; //out APart:TDateTimePart):Boolean; static;

    function Value:TData; override;
    function ToString:String; override;
  end;

  TTextUnaryOperand=(Lower,Upper,IsEmpty,Length,Trim);

  TTextUnaryOperandHelper=record helper for TTextUnaryOperand
  private
    const
      Texts : Array[TTextUnaryOperand] of String=(
              'Lower',
              'Upper',
              'IsEmpty',
              'Length',
              'Trim'
              );
  public
    class function FromString(const S:String; out Operand:TTextUnaryOperand):Boolean; static;
    function ToString:String;
  end;

  TUnaryTextExpression=class(TParameterExpression)
  protected
    function ResultClass:TExpressionClass; override;
  public
    Operand : TTextUnaryOperand;

    procedure Assign(const Source:TExpression); override;
    class function FromString(const S:String):TParameterExpression; override;
    function Value:TData; override;
    function ToString:String; override;
  end;

  TTextLogicalOperand=(Starts,Ends,Contains);

  TTextLogicalOperandHelper=record helper for TTextLogicalOperand
  private
    const
      Texts : Array[TTextLogicalOperand] of String=(
               'Starts',
               'Ends',
               'Contains'
              );
  public
    class function FromString(const S:String; out Operand:TTextLogicalOperand):Boolean; static;
    function ToString:String;
  end;

  TTextLogicalExpression=class(TOperandExpression)
  private
    FCase : Boolean;

    function DoContains(const A,B:String):Boolean;
    function EndsWith(const A,B:String):Boolean;
    function StartsWith(const A,B:String):Boolean;
  public
    Operand : TTextLogicalOperand;

    Constructor Create(const ALeft:TExpression; const AOperand:TTextLogicalOperand; const ARight:TExpression);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;

    property CaseSensitive:Boolean read FCase write FCase default False;
  end;

  TTextOperand=(IndexOf,Pad,Split,Insert,Remove,Replace,Count,SubString);

  TTextOperandHelper=record helper for TTextOperand
  private
    const
      Texts : Array[TTextOperand] of String=(
               'IndexOf',
               'Pad',
               'Split',
               'Insert',
               'Remove',
               'Replace',
               'Count',
               'SubString'
              );

    function ParameterCount:Integer;
  public
    class function FromString(const S:String; out Operand:TTextOperand):Boolean; static;
    function ToString:String;
  end;

  TTextOperandExpression=class(TParameterExpression)
  private
    class function Split(const S,Delimiter:String):TData;
    function ValueFrom(const Params:TArrayExpression):TData;
  protected
    function ResultClass:TExpressionClass; override;
  public
    Operand : TTextOperand;

    procedure Assign(const Source:TExpression); override;
    function AsString:String; override;

    class function FromString(const S:String):TParameterExpression; override;

    function Value:TData; override;
    function ToString:String; override;
  end;

  // Value := Condition ? Then : Else
  TIfExpression=class(TExpression)
  public
    Condition : TLogicalExpression;

    ThenExpression,
    ElseExpression : TExpression;

    Constructor Create(const ACondition:TLogicalExpression; const AThen,AElse:TExpression);
    Destructor Destroy; override;

    procedure Assign(const Source:TExpression); override;

    function ToString:String; override;
    function Value:TData; override;
  end;

implementation

uses
  {System.}Math,

  {$IFDEF FPC}
  BI.FPC,
  {$ELSE}
  System.Character, System.StrUtils,
  {$ENDIF}

  {$IFDEF OSX}
  Macapi.CoreFoundation,  // Skip inline hint
  {$ENDIF}

  {System.}DateUtils, BI.Expression.DateTime, BI.Arrays.Strings;

const
  CRLF=#13#10;

{ TExpressionsHelper }

procedure TExpressionsHelper.Add(const AExpression: TExpression);
var L : Integer;
begin
  L:=Length(Self);
  SetLength(Self,L+1);
  Self[L]:=AExpression;
end;

procedure TExpressionsHelper.Add(const AExpressions:Array of TExpression);
var t,
    L,
    LExp : Integer;
begin
  L:=Length(Self);
  LExp:=Length(AExpressions);
  SetLength(Self,L+LExp);

  for t:=0 to LExp-1 do
      Self[L+t]:=AExpressions[t];
end;

function TExpressionsHelper.Count: Integer;
begin
  result:=Length(Self);
end;

procedure TExpressionsHelper.Resize(const ACount: Integer);
begin
  SetLength(Self,ACount);
end;

{ TLogicalExpression }

Constructor TLogicalExpression.Create;
begin
  Init;
end;

Constructor TLogicalExpression.Create(const ALeft: TExpression;
                                      const AOperand: TLogicalOperand;
                                      const ARight: TExpression);
begin
  Create;

  Left:=ALeft;
  Right:=ARight;
  Operand:=AOperand;
end;

procedure TLogicalExpression.Init;
begin
  // Fill Funcs array to speed-up "Value" method
  Funcs[TLogicalOperand.Equal]:=IsEqual;
  Funcs[TLogicalOperand.NotEqual]:=IsNotEqual;
  Funcs[TLogicalOperand.Greater]:=IsGreater;
  Funcs[TLogicalOperand.Lower]:=IsLower;
  Funcs[TLogicalOperand.GreaterOrEqual]:=IsGreaterOrEqual;
  Funcs[TLogicalOperand.LowerOrEqual]:=IsLowerOrEqual;
  Funcs[TLogicalOperand.&And]:=CalcAnd;
  Funcs[TLogicalOperand.&Or]:=CalcOr;
  Funcs[TLogicalOperand.&In]:=LeftInRight;
end;

function TLogicalExpression.ToString: String;
begin
  result:=Part(Left)+' '+Operand.ToString+' '+Part(Right);
end;

function TLogicalExpression.CalcAnd: Boolean;
begin
  result:=Left.Value;

  // No short-circuit bool evaluation, so do it manually
  if result then
     result:=Right.Value;
end;

function TLogicalExpression.CalcOr: Boolean;
begin
  result:=Left.Value;

  // No short-circuit bool evaluation, so do it manually
  if not result then
     result:=Right.Value;
end;

procedure TLogicalExpression.Assign(const Source: TExpression);
begin
  if @Funcs[TLogicalOperand.Equal]=nil then
     Init;

  if Source is TLogicalExpression then
     Operand:=TLogicalExpression(Source).Operand;

  inherited;
end;

function TLogicalExpression.IsEqual:Boolean;
begin
  result:=Left.Value=Right.Value;
end;

function TLogicalExpression.IsNotEqual:Boolean;
begin
  result:=Left.Value<>Right.Value;
end;

class function TLogicalExpression.Join(const AOld, ANew: TLogicalExpression;
                                       const AOperand: TLogicalOperand): TLogicalExpression;
begin
  if AOld=nil then
     result:=ANew
  else
  if ANew=nil then
     result:=AOld
  else
     result:=TLogicalExpression.Create(AOld,AOperand,ANew);
end;

function TLogicalExpression.IsGreater: Boolean;
begin
  result:=Left.Value>Right.Value;
end;

function TLogicalExpression.IsGreaterOrEqual: Boolean;
begin
  result:=Left.Value>=Right.Value;
end;

function TLogicalExpression.IsLower: Boolean;
begin
  result:=Left.Value<Right.Value;
end;

function TLogicalExpression.IsLowerOrEqual: Boolean;
begin
  result:=Left.Value<=Right.Value;
end;

function TLogicalExpression.LeftInRight:Boolean;
var t : Integer;
    L,R : TData;
begin
  L:=Left.Value;
  R:=Right.Value;

  if {$IFDEF BIVARIANT}R.IsArray{$ELSE}VarIsArray(R){$ENDIF} then
  begin
    for t:={$IFDEF BIVARIANT}R.ArrayLow(1){$ELSE}VarArrayLowBound(R,1){$ENDIF} to
           {$IFDEF BIVARIANT}R.ArrayHigh(1){$ELSE}VarArrayHighBound(R,1){$ENDIF} do
        if L=R[t] then
           Exit(True);

    result:=False;
  end
  else
    result:=L=R;
end;

function TLogicalExpression.Value: TData;
begin
  result:=Funcs[Operand]; // Funcs is used to avoid FinalizeArray (too many VarClr)
end;

{ TUnaryNotExpression }

function TUnaryNotExpression.ToString: String;
begin
  result:='not ';

  if Expression=nil then
     result:=result+'?'
  else
     result:=result+' ('+Expression.ToString+')';
end;

function TUnaryNotExpression.Value: TData;
begin
  result:=not Expression.Value;
end;

{ TArithmeticExpression }

procedure TArithmeticExpression.Assign(const Source: TExpression);
begin
  if Source is TArithmeticExpression then
     Operand:=TArithmeticExpression(Source).Operand;

  inherited;
end;

constructor TArithmeticExpression.Create(const ALeft: TExpression;
  const AOperand: TArithmeticOperand; const ARight: TExpression);
begin
  //inherited;
  Left:=ALeft;
  Operand:=AOperand;
  Right:=ARight;
end;

function TArithmeticExpression.ToString: String;
begin
  result:=Part(Left)+' '+Operand.ToString+' '+Part(Right);
end;

function TArithmeticExpression.Add: TData;
begin
  result:=Left.Value+Right.Value;
end;

function TArithmeticExpression.Subtract: TData;
begin
  result:=Left.Value-Right.Value;
end;

function TArithmeticExpression.Multiply: TData;
begin
  result:=Left.Value*Right.Value;
end;

function TArithmeticExpression.Divide: TData;
begin
  result:=Left.Value/Right.Value;
end;

function TArithmeticExpression.Modulus: TData;
begin
  result:=Left.Value mod Right.Value;
end;

function TArithmeticExpression.Power: TData;
begin
  result:={$IFNDEF FPC}System.{$ENDIF}Math.Power(Left.Value,Right.Value);
end;

function TArithmeticExpression.Value: TData;
begin
  // Calling private functions Add, Subtract etc is aprox 2x faster
  case Operand of
        TArithmeticOperand.Add : result:=Add;
   TArithmeticOperand.Subtract : result:=Subtract;
   TArithmeticOperand.Multiply : result:=Multiply;
     TArithmeticOperand.Divide : result:=Divide;
       TArithmeticOperand.&Mod : result:=Modulus;
      TArithmeticOperand.Power : result:=Power;
  end;
end;

{ TMathExpression }

procedure TMathExpression.Assign(const Source: TExpression);
begin
  if Source is TMathExpression then
     Operand:=TMathExpression(Source).Operand;

  inherited;
end;

class function TMathExpression.FromString(
  const S: String): TParameterExpression;
var tmp : TMathOperand;
begin
  if TMathOperand.FromString(S,tmp) then
  begin
    result:=TMathExpression.Create;
    TMathExpression(result).Operand:=tmp;
  end
  else
    result:=nil;
end;

function TMathExpression.ResultClass: TExpressionClass;
begin
  if (Operand=TMathOperand.Round) or (Operand=TMathOperand.Trunc) or (Operand=TMathOperand.Sign) then
     result:=TIntegerExpression
  else
     result:=TFloatExpression;
end;

procedure TMathExpression.SetExpression(const Value: TExpression);
begin
  inherited;

  if FParams<>nil then
     if FParams.Count>2 then
        raise Exception.Create('Error: Power function needs one or two parameters');
end;

function TMathExpression.ToString: String;
begin
  result:=Operand.ToString+inherited;
end;

function TMathExpression.Value: TData;
var tmp : Double; // <-- Speed opt. Was "TData" (it'll anyway be converted to Double)
begin
  if Operand=TMathOperand.Power then
  begin
    if FParams=nil then
       result:=Power(10,Expression.Value)
    else
    if FParams.Count=1 then
       result:=Power(10,FParams[0].Value)
    else
       result:=Power(FParams[0].Value,FParams[1].Value);
  end
  else
  begin
    tmp:=Expression.Value;

    case Operand of
       TMathOperand.Abs : result:=System.Abs(tmp);
       TMathOperand.Sin : result:=System.Sin(tmp);
       TMathOperand.Cos : result:=System.Cos(tmp);
       TMathOperand.Tan : result:={$IFDEF FPC}Math.Tan{$ELSE}System.Tangent{$ENDIF}(tmp);
       TMathOperand.Sqr : result:=System.Sqr(tmp);
      TMathOperand.Sqrt : result:=System.Sqrt(tmp);
       TMathOperand.Log : result:=Log10(tmp);
        TMathOperand.Ln : result:=Log2(tmp);
       TMathOperand.Exp : result:=System.Exp(tmp);
     TMathOperand.Round : result:=System.Round(tmp);
     TMathOperand.Trunc : result:=System.Trunc(tmp);
      TMathOperand.Sign : result:=Sign(tmp);
    end;
  end;
end;

{ TIntegerExpression }

Constructor TIntegerExpression.Create(const AValue: Int64);
begin
  //inherited;
  Number:=AValue;
end;

function TIntegerExpression.ToString: String;
begin
  result:=IntToStr(Number);
end;

function TIntegerExpression.Value: TData;
begin
  result:=Number;
end;

procedure TIntegerExpression.Assign(const Source: TExpression);
begin
  if Source is TIntegerExpression then
     Number:=TIntegerExpression(Source).Number;
end;

{ TFloatExpression }

Constructor TFloatExpression.Create(const AValue: Extended);
begin
  //inherited;
  Number:=AValue;
end;

procedure TFloatExpression.Assign(const Source: TExpression);
begin
  if Source is TFloatExpression then
     Number:=TFloatExpression(Source).Number;
end;

class function TFloatExpression.E: TFloatExpression;
begin
  result:=TFloatExpression.Create(System.Exp(1));
end;

class function TFloatExpression.Pi: TFloatExpression;
begin
  result:=TFloatExpression.Create(System.Pi);
end;

function TFloatExpression.ToString: String;
begin
  result:=FloatToStr(Number);
end;

function TFloatExpression.Value: TData;
begin
  result:=Number;
end;

{ TBooleanExpression }

Constructor TBooleanExpression.Create(const AValue: Boolean);
begin
  Logical:=AValue;
end;

procedure TBooleanExpression.Assign(const Source: TExpression);
begin
  if Source is TBooleanExpression then
     Logical:=TBooleanExpression(Source).Logical;
end;

function TBooleanExpression.ToString: String;
begin
  result:=BoolToStr(Logical,True);
end;

function TBooleanExpression.Value: TData;
begin
  result:=Logical;
end;

{ TDateTimeExpression }

Constructor TDateTimeExpression.Create(const AValue: TDateTime);
begin
  DateTime:=AValue;
end;

procedure TDateTimeExpression.Assign(const Source: TExpression);
begin
  if Source is TDateTimeExpression then
     DateTime:=TDateTimeExpression(Source).DateTime;
end;

class function TDateTimeExpression.Date: TDateTimeExpression;
begin
  result:=DateSpan(0);
end;

class function TDateTimeExpression.DateSpan(const ADelta: TDateTime): TDateTimeExpression;
begin
  result:=TDateTimeExpression.Create({$IFNDEF FPC}System.{$ENDIF}SysUtils.Date+ADelta);
end;

class function TDateTimeExpression.Now: TDateTimeExpression;
begin
  result:=TDateTimeExpression.Create({$IFNDEF FPC}System.{$ENDIF}SysUtils.Now);
end;

class function TDateTimeExpression.Time: TDateTimeExpression;
begin
  result:=TDateTimeExpression.Create({$IFNDEF FPC}System.{$ENDIF}SysUtils.Time);
end;

function TDateTimeExpression.ToString: String;
begin
  result:='"'+DateTimeToStr(DateTime)+'"';
end;

function TDateTimeExpression.Value: TData;
begin
  result:=DateTime;
end;

class function TDateTimeExpression.FromData(const Value:TData):TDateTime;
begin
  if {$IFDEF BIVARIANT}Value.IsString{$ELSE}VarIsStr(Value){$ENDIF} then
     result:=StrToDateTime(Value)
  else
     result:=Value;
end;

{ TDateExpression }

class function TDateExpression.FromData(const Value:TData):TDateTime;
begin
  if {$IFDEF BIVARIANT}Value.IsString{$ELSE}VarIsStr(Value){$ENDIF} then
     result:=StrToDate(Value)
  else
     result:=Value;
end;

class function TDateExpression.FromString(const S: String): TParameterExpression;
begin
  if SameText(S,'DATE') then
     result:=TDateExpression.Create
  else
     result:=nil;
end;

class function TDateExpression.Today: TDateExpression;
begin
  result:=TDateExpression.Create(TDateTimeExpression.Date);
end;

class function TDateExpression.Tomorrow: TDateExpression;
begin
  result:=TDateExpression.Create(TDateTimeExpression.DateSpan(1));
end;

function TDateExpression.ToString: String;
begin
  result:='Date'+inherited;
end;

function TDateExpression.Value: TData;
begin
  result:=Trunc {DateOf}(FromData(Expression.Value));
end;

class function TDateExpression.Yesterday: TDateExpression;
begin
  result:=TDateExpression.Create(TDateTimeExpression.DateSpan(-1));
end;

{ TTimeExpression }

class function TTimeExpression.FromData(const Value: TData): TDateTime;
begin
  if {$IFDEF BIVARIANT}Value.IsString{$ELSE}VarIsStr(Value){$ENDIF} then
     result:=StrToTime(Value)
  else
     result:=Value;
end;

class function TTimeExpression.FromString(const S: String): TParameterExpression;
begin
  if SameText(S,'TIME') then
     result:=TTimeExpression.Create
  else
     result:=nil;
end;

function TTimeExpression.ToString: String;
begin
  result:='Time'+inherited;
end;

function TTimeExpression.Value: TData;
begin
  result:=TimeOf(FromData(Expression.Value));
end;

{ TTextExpression }

Constructor TTextExpression.Create(const AValue: String);
begin
  Text:=AValue;
end;

procedure TTextExpression.Assign(const Source: TExpression);
begin
  if Source is TTextExpression then
     Text:=TTextExpression(Source).Text;
end;

function TTextExpression.ToString: String;
begin
  result:='"'+Text+'"';
end;

function TTextExpression.Value: TData;
begin
  result:=Text;
end;

{ TOperandExpression }

Destructor TOperandExpression.Destroy;
begin
  Left.Free;
  Right.Free;
  inherited;
end;

procedure TOperandExpression.Assign(const Source: TExpression);
var tmpL,
    tmpR : TExpression;
begin
  if Source is TOperandExpression then
  begin
    tmpL:=Clone(TOperandExpression(Source).Left);
    tmpR:=Clone(TOperandExpression(Source).Right);

    Left.Free;
    Right.Free;

    Left:=tmpL;
    Right:=tmpR;
  end;
end;

function TOperandExpression.Part(const AExpression: TExpression): String;
begin
  if AExpression=nil then
     result:=''
  else
  begin
    result:=AExpression.ToString;

    if AExpression is TOperandExpression then
       result:='( '+result+' )';
  end;
end;

procedure TOperandExpression.Traverse(const AProc: TExpressionProc);
begin
  inherited;

  if Left<>nil then
     Left.Traverse(AProc);

  if Right<>nil then
     Right.Traverse(AProc);
end;

{ TUnaryExpression }

Constructor TUnaryExpression.Create(const AExpression: TExpression);
begin
  Expression:=AExpression;
end;

Destructor TUnaryExpression.Destroy;
begin
  FExpression.Free;
  inherited;
end;

procedure TUnaryExpression.Assign(const Source: TExpression);
var tmp : TExpression;
begin
  if Source is TUnaryExpression then
  begin
    tmp:=TExpression.Clone(TUnaryExpression(Source).Expression);

    Expression.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};
    Expression:=tmp;
  end;
end;

procedure TUnaryExpression.SetExpression(const Value: TExpression);
begin
  FExpression:=Value;
end;

procedure TUnaryExpression.Traverse(const AProc: TExpressionProc);
begin
  inherited;

  if FExpression<>nil then
     FExpression.Traverse(AProc);
end;

{ TArithmeticOperandHelper }

class function TArithmeticOperandHelper.FromString(const S: String;
  out Operand: TArithmeticOperand): Boolean;
var tmp : TArithmeticOperand;
begin
  for tmp:=Low(TArithmeticOperand) to High(TArithmeticOperand) do
      if SameText(Texts[tmp],S) then
      begin
        Operand:=tmp;
        Exit(True);
      end;

  result:=False;
end;

function TArithmeticOperandHelper.ToString: String;
begin
  result:=Texts[Self];
end;

{ TLogicalOperandHelper }

class function TLogicalOperandHelper.FromString(const S: String;
  out Operand: TLogicalOperand): Boolean;
var tmp : TLogicalOperand;
begin
  for tmp:=Low(TLogicalOperand) to High(TLogicalOperand) do
      if SameText(Texts[tmp],S) then
      begin
        Operand:=tmp;
        Exit(True);
      end;

  result:=False;
end;

function TLogicalOperandHelper.ToString: String;
begin
  result:=Texts[Self];
end;

{ TMathOperandHelper }

class function TMathOperandHelper.FromString(const S: String;
  out Operand: TMathOperand): Boolean;
var tmp : TMathOperand;
begin
  for tmp:=Low(TMathOperand) to High(TMathOperand) do
      if SameText(Texts[tmp],S) then
      begin
        Operand:=tmp;
        Exit(True);
      end;

  result:=False;
end;

function TMathOperandHelper.ToString: String;
begin
  result:=Texts[Self];
end;

{ TArrayExpression }

Constructor TArrayExpression.Create(const AValues: Array of TExpression);
begin
  inherited Create;
  Items.Add(AValues);
end;

Constructor TArrayExpression.Create(const AValues: array of TData);

  function FromItem(const Index:Integer):TExpression;

    procedure DoError;
    begin
      raise Exception.Create('Cannot interpret array item at index: '+IntToStr(Index));
    end;

  var tmp : TData;
  begin
    tmp:=AValues[Index];

    if {$IFDEF BIVARIANT}tmp.IsString{$ELSE}VarIsStr(tmp){$ENDIF} then
       result:=TTextExpression.Create(tmp)
    else
    if {$IFDEF BIVARIANT}tmp.IsFloat{$ELSE}VarIsFloat(tmp){$ENDIF} then
       result:=TFloatExpression.Create(tmp)
    else
    if {$IFDEF BIVARIANT}tmp.IsOrdinal{$ELSE}VarIsOrdinal(tmp){$ENDIF} then
       result:=TIntegerExpression.Create(tmp)
    else
    begin
      DoError;
      result:=nil;
    end;
  end;

var t,
    L : Integer;
begin
  inherited Create;

  L:=Length(AValues);
  Items.Resize(L);

  for t:=0 to L-1 do
      Self[t]:=FromItem(t);
end;

Destructor TArrayExpression.Destroy;
begin
  Clear;
  inherited;
end;

function TArrayExpression.Get(const Index: Integer): TExpression;
begin
  result:=Items[Index];
end;

procedure TArrayExpression.Put(const Index: Integer; const Value: TExpression);
begin
  Items[Index]:=Value;
end;

procedure TArrayExpression.Add(const AValue: TExpression);
begin
  Items.Add(AValue);
end;

procedure TArrayExpression.Assign(const Source: TExpression);
var tmp : TArrayExpression;
    t,
    L : Integer;
begin
  if Source is TArrayExpression then
  begin
    Clear;

    tmp:=TArrayExpression(Source);
    L:=tmp.Count;

    Items.Resize(L);

    for t:=0 to L-1 do
        Self[t]:=TExpression.Clone(tmp[t]);
  end;
end;

procedure TArrayExpression.Clear;
var t : Integer;
begin
  for t:=0 to Count-1 do
      Self[t].{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};

  Items:=nil;
end;

function TArrayExpression.Count: Integer;
begin
  result:=Items.Count;
end;

function TArrayExpression.ToString: String;

  function ItemToString(const AItem:TExpression):String;
  begin
    if AItem=nil then
       result:=''
    else
       result:=AItem.ToString;
  end;

  function AllItems:String;
  var t,H : Integer;
  begin
    result:='';

    H:=High(Items);

    for t:=Low(Items) to H do
    begin
      result:=result+ItemToString(Items[t]);

      if t<H then
         result:=result+',';
    end;
  end;

begin
  if IsParams then
     result:=AllItems
  else
     result:='['+AllItems+']';
end;

procedure TArrayExpression.Traverse(const AProc: TExpressionProc);
var t : Integer;
begin
  inherited;

  for t:=Low(Items) to High(Items) do
      Items[t].Traverse(AProc);
end;

function TArrayExpression.Value: TData;
var t, L : Integer;
    tmp : Array of TData;
begin
  L:=Count;
  SetLength(tmp,L);

  for t:=0 to L-1 do
      tmp[t]:=Items[t].Value;

  result:=tmp;
end;

{ TDateTimePartHelper }

function TDateTimePartHelper.AsString(const Index: Integer): String;
begin
  case Self of
TDateTimePart.ShortWeekDayName:
                      if Index=6 then
                         result:=FormatSettings.ShortDayNames[1]
                      else
                         result:=FormatSettings.ShortDayNames[Index+2];

 TDateTimePart.LongWeekDayName:
                      if Index=6 then
                         result:=FormatSettings.LongDayNames[1]
                      else
                         result:=FormatSettings.LongDayNames[Index+2];

   TDateTimePart.LongMonthName: result:=FormatSettings.LongMonthNames[Index+1];
  TDateTimePart.ShortMonthName: result:=FormatSettings.ShortMonthNames[Index+1];
         TDateTimePart.Quarter: result:=Format(QuarterFormat,[Index+1]);
  else
    result:='';
  end;
end;


const
  DateParts:Array[0..22] of String=(
                     '',
           'Millisecond',
      'HundredsOfSecond',
        'TenthsOfSecond',
                'Second',
                'Minute',
         'QuarterOfHour',
                  'Hour',
                   'Day',
             'DayOfYear',
            'WeekOfYear',
               'WeekDay',
          'ShortWeekDay',
           'LongWeekDay',
                 'Month',
            'ShortMonth',
             'LongMonth',
               'Quarter',
                  'Year',
                'Decade',
          'DecadeOfYear',
               'Century',
            'Millennium');

class function TDateTimePartHelper.FromString(const S:String; out APart:TDateTimePart):Boolean;
var t : Integer;
begin
  for t:=0 to Length(DateParts)-1 do
      if SameText(S,DateParts[t]) then
      begin
        APart:=TDateTimePart(t);
        Exit(True);
      end;

  result:=False;
end;

function TDateTimePartHelper.High: Integer;
const Highs : Array[TDateTimePart] of Integer=(
                0,

                // Not enough resolution:
                //TDateTimePart.Nanosecond: result:=100000;
                //TDateTimePart.Microsecond: result:=10000;

                1000, // Millisecond
                100,  // HundredsOfSecond
                10,   // TenthsOfSecond
                60,   // Second
                60,   // Minute
                4,    // QuarterHour
                24,   // Hour
                31,   // DayOfMonth
                366,  // DayOfYear
                53,   // WeekOfYear

                7,7,7,     // WeekDay
                12,12,12,  // Month

                4,    // Quarter
                0,    // Year
                0,    // Decade
                10,   // DecadeOfYear
                1000, // Century
                100   // Millenium
              );
begin
  result:=Highs[Self];
end;

function TDateTimePartHelper.Low: Integer;
begin
  case Self of
      TDateTimePart.QuarterHour,
       TDateTimePart.DayOfMonth,
        TDateTimePart.DayOfYear,
       TDateTimePart.WeekOfYear,
          TDateTimePart.WeekDay,
 TDateTimePart.ShortWeekDayName,
  TDateTimePart.LongWeekDayName,
            TDateTimePart.Month,
   TDateTimePart.ShortMonthName,
    TDateTimePart.LongMonthName,
         TDateTimePart.Quarter,
    TDateTimePart.DecadeOfYear : result:=1; // <-- Special case: MinYear and tmpMax cannot be used
  else
    // Note: For Year and up, not possible to calculate without a min and a max Year
    result:=0;
  end;
end;

class function TDateTimePartHelper.Max: Integer;
begin
  result:=Ord(TDateTimePart.Millennium);
end;

function TDateTimePartHelper.ToString: String;
begin
  result:=Texts[Self];
end;

class function TDateTimePartHelper.ToCode(const Index: Integer): String;
begin
  result:=DateParts[Index];
end;

class function TDateTimePartHelper.ToString(const Index:Integer): String;
begin
  result:=TDateTimePart(Index).ToString;
end;

class function TDateTimePartHelper.AllToText: String;
begin
  result:=TDateTimePart.None.ToString+CRLF+
          //Nanosecond.ToString+CRLF+
          //Microsecond.ToString+CRLF+
          TDateTimePart.Millisecond.ToString+CRLF+
          TDateTimePart.HundredsOfSecond.ToString+CRLF+
          TDateTimePart.TenthsOfSecond.ToString+CRLF+
          TDateTimePart.Second.ToString+CRLF+
          TDateTimePart.Minute.ToString+CRLF+
          TDateTimePart.QuarterHour.ToString+CRLF+
          TDateTimePart.Hour.ToString+CRLF+
          TDateTimePart.DayOfMonth.ToString+CRLF+
          TDateTimePart.DayOfYear.ToString+CRLF+
          TDateTimePart.WeekOfYear.ToString+CRLF+
          TDateTimePart.WeekDay.ToString+CRLF+
          TDateTimePart.ShortWeekDayName.ToString+CRLF+
          TDateTimePart.LongWeekDayName.ToString+CRLF+
          TDateTimePart.Month.ToString+CRLF+
          TDateTimePart.ShortMonthName.ToString+CRLF+
          TDateTimePart.LongMonthName.ToString+CRLF+
          TDateTimePart.Quarter.ToString+CRLF+
          TDateTimePart.Year.ToString+CRLF+
          TDateTimePart.Decade.ToString+CRLF+
          TDateTimePart.DecadeOfYear.ToString+CRLF+
          TDateTimePart.Century.ToString+CRLF+
          TDateTimePart.Millennium.ToString;
end;

{ TDateTimePartExpression }

procedure TDateTimePartExpression.Assign(const Source: TExpression);
begin
  if Source is TDateTimePartExpression then
     Part:=TDateTimePartExpression(Source).Part;

  inherited;
end;

class function TDateTimePartExpression.FromString(const S: String):TParameterExpression;
//  out APart: TDateTimePart): Boolean;
var tmp : TDateTimePart;
begin
  if TDateTimePart.FromString(S,tmp) then
  begin
    result:=TDateTimePartExpression.Create;
    TDateTimePartExpression(result).Part:=tmp;
  end
  else
    result:=nil;
end;

function TDateTimePartExpression.ResultClass: TExpressionClass;
begin
  case Part of
      TDateTimePart.ShortWeekDayName,
       TDateTimePart.LongWeekDayName,
        TDateTimePart.ShortMonthName,
         TDateTimePart.LongMonthName: result:=TTextExpression;
                  TDateTimePart.None: result:=TDateTimeExpression;
  else
    result:=TIntegerExpression;
  end;
end;

function TDateTimePartExpression.ToString: String;
begin
  result:=DateParts[Ord(Part)]+inherited;
end;

function TDateTimePartExpression.Value: TData;
var tmpDate : TDateTime;
begin
  tmpDate:=TDateTimeExpression.FromData(Expression.Value);

  case Part of
  TDateTimePart.ShortWeekDayName: result:=FormatSettings.ShortDayNames[DayOfTheWeek(tmpDate)];
   TDateTimePart.LongWeekDayName: result:=FormatSettings.LongDayNames[DayOfTheWeek(tmpDate)];
    TDateTimePart.ShortMonthName: result:=FormatSettings.ShortMonthNames[TBIDateTime.MonthOf(tmpDate)];
     TDateTimePart.LongMonthName: result:=FormatSettings.LongMonthNames[TBIDateTime.MonthOf(tmpDate)];

              TDateTimePart.None: result:=tmpDate;
  else
    result:=AsInteger(tmpDate);
  end;
end;

function TDateTimePartExpression.AsInteger(const AValue:TDateTime):Integer;
var tmpYear : Word;
begin
  case Part of
//       Nanosecond: ;
//      Microsecond: ;
     TDateTimePart.Millisecond: result:=MilliSecondOf(AValue);
TDateTimePart.HundredsOfSecond: result:=MilliSecondOf(AValue) div 10;
  TDateTimePart.TenthsOfSecond: result:=MilliSecondOf(AValue) div 100;
          TDateTimePart.Second: result:=SecondOf(AValue);
          TDateTimePart.Minute: result:=MinuteOf(AValue);
     TDateTimePart.QuarterHour: result:=MinuteOf(AValue) div 15;
            TDateTimePart.Hour: result:=HourOf(AValue);

      TDateTimePart.DayOfMonth: result:=TBIDateTime.DayOf(AValue);
       TDateTimePart.DayOfYear: result:=TBIDateTime.DayOfTheYear(AValue);
      TDateTimePart.WeekOfYear: result:=WeekOfTheYear(AValue);

         TDateTimePart.WeekDay: result:=DayOfTheWeek(AValue);
           TDateTimePart.Month: result:=TBIDateTime.MonthOf(AValue);
         TDateTimePart.Quarter: result:=TBIDateTime.MonthOf(AValue) div 4;
  else
    begin
      tmpYear:=TBIDateTime.YearOf(AValue);

      case Part of
         TDateTimePart.Year : result:=tmpYear;
       TDateTimePart.Decade : result:=10*(tmpYear div 10); // 2010s.. 2020s...
 TDateTimePart.DecadeOfYear : result:=1+((tmpYear div 10) mod 10); // 1..10
      TDateTimePart.Century : result:=1+(tmpYear div 100); // ...19, 20, 21, 22...
   TDateTimePart.Millennium : result:=1+(tmpYear div 1000);  //
      else
        result:=0; // <-- just to skip warning
      end;
    end;
  end;
end;

{ TExpression }

function TExpression.AsString: String;
begin
  result:=Value;
end;

class function TExpression.Clone(const AExpression: TExpression): TExpression;
begin
  if AExpression=nil then
     result:=nil
  else
  begin
    result:=TExpressionClass(AExpression.ClassType).Create;
    result.Assign(AExpression);
  end;
end;

{
constructor TExpression.Create;
begin
end;
}

class function TExpression.Evaluate(const S: String): TData;
var tmp : TExpression;
begin
  tmp:=TExpression.FromString(S);
  try
    result:=tmp.Value;
  finally
    tmp.Free;
  end;
end;

{$IFDEF FPC}
function TExpression.DoError(const APos:Integer; const AMessage:String):Boolean;
{$ELSE}
class function TExpression.DoError(const APos:Integer; const AMessage:String):Boolean;
{$ENDIF}
begin
  raise EExpressionParse.Create(APos,AMessage);

  {$IF CompilerVersion<35}
  result:=False; // <-- to skip warning
  {$ENDIF}
end;

class function TExpression.FromString(const S: String;
                                      const Resolve:TResolveProc): TExpression;
{$IFDEF FPC}
var tmp : TExpression;
{$ENDIF}
begin
  {$IFDEF FPC}
  tmp:=TExpression.Create;
  try
    result:=TExpression.FromString(S,Resolve,tmp.DoError);
  finally
    tmp.Free;
  end;

  {$ELSE}
  result:=TExpression.FromString(S,Resolve,DoError);
  {$ENDIF}
end;

function GuessIntrinsic(const S:String):TExpression;
var tmpDate : TDateTime;
    tmpFloat : Extended;
    tmpInt : Int64;
    tmpBool : Boolean;
begin
  if SameText(S,'PI') then
     result:=TFloatExpression.Pi
  else
  if SameText(S,'E') then
     result:=TFloatExpression.E
  else
  if SameText(S,'NOW') then
     result:=TDateTimeExpression.Now
  else
  if TryStrToInt64(S,tmpInt) then
     result:=TIntegerExpression.Create(tmpInt)
  else
  if TryStrToFloat(S,tmpFloat) then
     result:=TFloatExpression.Create(tmpFloat)
  else
  if TryStrToBool(S,tmpBool) then
     result:=TBooleanExpression.Create(tmpBool)
  else
  if TryStrToDateTime(S,tmpDate) then
     result:=TDateTimeExpression.Create(tmpDate)
  else
     result:=nil;
end;

function FromArray(const Value:TExpressions; const IsParams:Boolean):TExpression;
begin
  if Value.Count=1 then
     result:=Value[0]
  else
  begin
    result:=TArrayExpression.Create;
    TArrayExpression(result).IsParams:=IsParams;
    TArrayExpression(result).Items:=Value;
  end;
end;

type
  TParser=record
  private
   var
    Start,
    Finish : Integer; // Character position of parsed string

    Error : TErrorProc;
    Resolve : TResolveProc;
    Text : String;

    function Arithmetic(const Left:TExpression; const S:String):TArithmeticExpression;
    procedure DoError(const APos:Integer; const AMessage:String);
    function GetString(const Terminator:Char; out S:String):Boolean;
    function GetText(const Terminator:Char):TTextExpression;
    function GuessLiteral(const S:String):TExpression;
    function Logical(const Left:TExpression; const Token:String):TLogicalExpression;
    function ParseArray(const IsParams:Boolean):TExpression;
    function ParseComparerOperand(const C:Char):String;
    procedure SkipBlanks;
    function TryResolve(const Token:String):TExpression;
  public
    Constructor Create(const S: String;
                       const Resolve:TResolveProc;
                       const Error:TErrorProc);

    function FromString:TExpression;
  end;

Constructor TParser.Create(const S: String;
                           const Resolve:TResolveProc;
                           const Error:TErrorProc);
begin
  Text:=S;
  Self.Resolve:=Resolve;
  Self.Error:=Error;

  Finish:=Length(Text);

  if Finish>0 then
  begin
    Start:=Low(String);

    if Start=0 then
       Dec(Finish); // Zero-based strings
  end;
end;

procedure TParser.SkipBlanks;
{$IFNDEF FPC}
{$IF CompilerVersion>25}
{$DEFINE ISINARRAY}
{$ELSE}
  {$IFDEF NEXTGEN}
  {$DEFINE ISINARRAY}
  {$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
const
  {$IFDEF ISINARRAY}
  SkipChars:Array[0..3] of Char=(' ',#9,#13,#10); // not supported by FPC
  {$ELSE}
  SkipChars=[' ',#9,#13,#10];
  {$ENDIF}
{$ENDIF}

var tmp : Char;
begin
  while Start<=Finish do
  begin
    tmp:=Text[Start]; // <-- use "tmp" to avoid calls to UniqueString inside this "while"

    if {$IFDEF FPC}
       (tmp in [' ',#9,#13,#10])
       {$ELSE}
       {$IFDEF ISINARRAY}
       tmp.IsInArray(SkipChars)
       {$ELSE}
       CharInSet(tmp,SkipChars)
       {$ENDIF}
       {$ENDIF}
        then
          Inc(Start)
    else
      break;
  end;
end;

function TParser.ParseComparerOperand(const C:Char):String;
var Next : Char;
begin
  result:=C;

  if Start<=Finish then
  begin
    if C='<' then
    begin
      Next:=Text[Start];

      if (Next='>') or (Next='=') then
      begin
        result:=result+Next;
        Inc(Start);
      end;
    end
    else
    if C='>' then
    begin
      Next:=Text[Start];

      if Next='=' then
      begin
        result:=result+Next;
        Inc(Start);
      end;
    end;
  end;
end;

function TParser.GetString(const Terminator:Char; out S:String):Boolean;
var tmp : Integer;
begin
  tmp:=Start;

  // Pending: include escaped \xx terminators
  while Start<=Finish do
  begin
    if Text[Start]=Terminator then
    begin
      S:=Copy(Text,tmp,Start-tmp);
      Inc(Start);
      Exit(True);
    end
    else
      Inc(Start);
  end;

  result:=False;
end;

procedure TParser.DoError(const APos:Integer; const AMessage:String);
begin
  if Assigned(Error) then
     Error(APos,AMessage)
  else
     raise EExpressionParse.Create(APos,AMessage);
end;

function TParser.GetText(const Terminator:Char):TTextExpression;
var tmp : String;
begin
  if GetString(Terminator,tmp) then
     result:=TTextExpression.Create(tmp)
  else
  begin
    result:=nil;
    DoError(Start,'Missing string terminator: '+Terminator);
  end;
end;

function TParser.GuessLiteral(const S:String):TExpression;

  function TryResolver:TExpression;
  begin
    try
      result:=Resolve(S,False);
    except
      on E:Exception do
      begin
        DoError(Start,E.Message);
        result:=nil;
      end;
    end;
  end;

begin
  result:=GuessIntrinsic(S);

  if result=nil then
     if Assigned(Resolve) then
        result:=TryResolver
     else
        DoError(Start,'Unknown: '+S);
end;

function TParser.ParseArray(const IsParams:Boolean):TExpression;
var tmpExp : TExpressions;
    tmp  : TExpression;
begin
  {$IFDEF FPC}
  tmpExp:=nil;
  {$ENDIF}

  repeat
    tmp:=FromString;

    if tmp<>nil then
       tmpExp.Add(tmp);

    if Start>1 then
    begin
      if IsParams and (Text[Start-1]=')') then
         break
      else
      if (not IsParams) and (Text[Start-1]=']') then
         break
    end
    else
      break;

  until tmp=nil;

  result:=FromArray(tmpExp,IsParams);
end;

function TParser.Arithmetic(const Left:TExpression; const S:String):TArithmeticExpression;
var tmpOp : TArithmeticOperand;
    Right : TExpression;
begin
  result:=nil;

  if TArithmeticOperand.FromString(S,tmpOp) then
  begin
    Right:=FromString;

    if Right<>nil then
       result:=TArithmeticExpression.Create(Left,tmpOp,Right);
  end;

  if result=nil then
     Left.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};
end;

function TParser.Logical(const Left:TExpression; const Token:String):TLogicalExpression;
var tmpOp : TLogicalOperand;
    Right : TExpression;
begin
  result:=nil;

  if TLogicalOperand.FromString(Token,tmpOp) then
  begin
    Right:=FromString;

    if Right<>nil then
       result:=TLogicalExpression.Create(Left,tmpOp,Right);
  end;

  if result=nil then
     Left.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF};
end;

function TParser.TryResolve(const Token:String):TExpression;
var tmp : TExpression;
begin
  result:=TParameterExpression.Guess(Token);

  if result<>nil then
  begin
    tmp:=ParseArray(True);

    if tmp=nil then
       DoError(Start,'Missing parameters calling: '+Token)
    else
       TParameterExpression(result).Expression:=tmp;
  end;
end;

function IsAndOr(const Token:String):Boolean;
begin
  result:=SameText(Token,'AND') or SameText(Token,'OR');
end;

function CannotBeFloat(const Token:String):Boolean;

  function EndsExponent:Boolean;
  var tmp : Integer;
      tmpValue : Double;
  begin
    tmp:=Length(Token);

    result:=(tmp>1) and (UpCase(Token[tmp])='E') and
             TryStrToFloat(Copy(Token,1,tmp-1),tmpValue);
  end;

begin
  result:=(Token<>'') and (not EndsExponent);
end;

function TParser.FromString:TExpression;
var
  Token : String;
  tmp : TExpression;

  procedure CheckLeftSide(const C:String);
  begin
    if tmp=nil then
       DoError(Start,'Missing left side before: '+C);
  end;

  function IsTextLogical(var AResult:TExpression):Boolean;
  var tmpOp : TTextLogicalOperand;
      Right : TExpression;
  begin
    result:=TTextLogicalOperand.FromString(Token,tmpOp);

    if result then
    begin
      CheckLeftSide(Token);

      Right:=FromString;

      if Right=nil then
         tmp.{$IFDEF AUTOREFCOUNT}DisposeOf{$ELSE}Free{$ENDIF}
      else
         AResult:=TTextLogicalExpression.Create(tmp,tmpOp,Right);
    end;
  end;

  procedure CheckNotLeftSide(const C:String);
  begin
    if tmp<>nil then
       DoError(Start,'Left side exists before: '+C);
  end;

  procedure GuessToken;
  begin
    if Token<>'' then
    begin
      tmp:=GuessLiteral(Token);
      Token:='';
    end;
  end;

  procedure FindToken;
  var Old : String;
  begin
    Old:=Token;
    GuessToken;

    if tmp=nil then
       DoError(Start,'Unknown: '+Old);
  end;

  function IsKeyword(const AToken:String; var AExpression:TExpression):Boolean;
  begin
    result:=True;

    if (AToken='AND') or (AToken='OR') or (AToken='IN') then
    begin
      CheckLeftSide(Token);
      AExpression:=Logical(tmp,Token);
    end
    else
    if AToken='MOD' then
    begin
      CheckLeftSide(Token);
      AExpression:=Arithmetic(tmp,Token);
    end
    else
    if AToken='NOT' then
    begin
      CheckNotLeftSide(Token);

      tmp:=FromString;

      if tmp=nil then
      begin
        AExpression.Free;
        AExpression:=nil;
      end
      else
        AExpression:=TUnaryNotExpression.Create(tmp);
    end
    else
      result:=IsTextLogical(AExpression);
  end;

var C : Char;
begin
  result:=nil;

  if Text='' then
     Exit;

  Token:='';
  tmp:=nil;

  while Start<=Finish do
  begin
    C:=Text[Start];
    Inc(Start);

    case C of
      ' ',#9,#13,#10 :

            begin
              SkipBlanks;

              if Token<>'' then
                 if IsKeyword(UpperCase(Token),result) then
                    Exit
                 else
                    GuessToken;
            end;

      '(' : begin
              if Token='' then
              begin
                CheckNotLeftSide(C);
                tmp:=FromString;
              end
              else
              begin
                if IsAndOr(Token) then
                begin
                  CheckLeftSide(Token);
                  result:=Logical(tmp,Token);
                  Exit;
                end
                else
                begin
                  tmp:=TryResolve(Token);

                  if tmp=nil then
                  begin
                    if Assigned(Resolve) then
                       tmp:=Resolve(Token,True);

                    if tmp=nil then
                       FindToken
                    else
                    if tmp is TUnaryExpression then
                       TUnaryExpression(tmp).Expression:=FromString;
                  end;

                  Token:='';

                  if tmp=nil then
                     break;
                end;
              end;
            end;

      '[' : begin
              CheckNotLeftSide(C);
              tmp:=ParseArray(False);
              break;
            end;

      ')',
      ']',
      ',' : break;

 '"','''' : begin
              if tmp=nil then
              begin
                tmp:=GetText(C);
                Token:='';
              end
              else
                Exit;
            end;

      '{' : begin
              if Token='' then
              begin
                if not GetString('}',Token) then
                begin
                  result.Free;
                  result:=nil;

                  DoError(Start,'Missing }');
                end;
              end
              else
                 Exit;
            end;

      '+',
      '-' : begin
              if CannotBeFloat(Token) then
                 GuessToken;

              if tmp=nil then
                 Token:=Token+C
              else
              begin
                result:=Arithmetic(tmp,C);
                Exit;
              end;
            end;
      '*',
      '/',
      '^' : begin
              GuessToken;
              CheckLeftSide(C);

              result:=Arithmetic(tmp,C);
              Exit;
            end;

      '=',
      '<',
      '>' : begin
              GuessToken;
              CheckLeftSide(C);

              result:=Logical(tmp,ParseComparerOperand(C));
              Exit;
            end;
    else
      Token:=Token+C;
    end;
  end;

  if Token<>'' then
     GuessToken;
 
  result:=tmp
end;

class function TExpression.FromString(const S: String;
                                      const Resolve:TResolveProc;
                                      const Error:TErrorProc): TExpression;
var tmp : TParser;
begin
  tmp:=TParser.Create(S,Resolve,Error);
  result:=tmp.FromString;
end;

procedure TExpression.Traverse(const AProc: TExpressionProc);
begin
  AProc(Self);
end;

function {$IFDEF FPC}TExpression.{$ENDIF}NilFunction(const S:String; IsFunction:Boolean):TExpression;
begin
  result:=nil; // raise ?
end;

class function TExpression.FromString(const S: String): TExpression;
begin
  result:=FromString(S,NilFunction,DoError);
end;

{ EExpressionParse }

Constructor EExpressionParse.Create(APos: Integer; const AMessage: String);
begin
  inherited Create(AMessage);
  Position:=APos;
end;

{ TUnaryTextExpression }

procedure TUnaryTextExpression.Assign(const Source: TExpression);
begin
  if Source is TUnaryTextExpression then
     Operand:=TUnaryTextExpression(Source).Operand;

  inherited;
end;

class function TUnaryTextExpression.FromString(
  const S: String): TParameterExpression;
var tmp : TTextUnaryOperand;
begin
  if TTextUnaryOperand.FromString(S,tmp) then
  begin
    result:=TUnaryTextExpression.Create;
    TUnaryTextExpression(result).Operand:=tmp;
  end
  else
    result:=nil;
end;

function TUnaryTextExpression.ResultClass: TExpressionClass;
begin
  case Operand of
   TTextUnaryOperand.Lower,
   TTextUnaryOperand.Upper,
    TTextUnaryOperand.Trim: result:=TTextExpression;

 TTextUnaryOperand.IsEmpty: result:=TBooleanExpression;
  else
    //TTextUnaryOperand.Length:
    result:=TIntegerExpression;
  end;
end;

function TUnaryTextExpression.ToString: String;
begin
  result:=Operand.ToString+inherited;
end;

function TUnaryTextExpression.Value: TData;
var tmp : String;
begin
  result:=Expression.Value;

  if result<>Null then
  begin
    tmp:=result;

    case Operand of
      TTextUnaryOperand.Lower: result:=LowerCase(tmp);
      TTextUnaryOperand.Upper: result:=UpperCase(tmp);
    TTextUnaryOperand.IsEmpty: result:=Length(tmp)=0;
     TTextUnaryOperand.Length: result:=Length(tmp);
       TTextUnaryOperand.Trim: result:=Trim(tmp);
    end;
  end;
end;

{ TTextUnaryOperandHelper }

class function TTextUnaryOperandHelper.FromString(const S: String;
  out Operand: TTextUnaryOperand): Boolean;
var tmp : TTextUnaryOperand;
begin
  for tmp:=Low(TTextUnaryOperand) to High(TTextUnaryOperand) do
      if SameText(Texts[tmp],S) then
      begin
        Operand:=tmp;
        Exit(True);
      end;

  result:=False;
end;

function TTextUnaryOperandHelper.ToString: String;
begin
  result:=Texts[Self];
end;

{ TTextLogicalOperandHelper }

class function TTextLogicalOperandHelper.FromString(const S: String;
  out Operand: TTextLogicalOperand): Boolean;
var tmp : TTextLogicalOperand;
begin
  for tmp:=Low(TTextLogicalOperand) to High(TTextLogicalOperand) do
      if SameText(Texts[tmp],S) then
      begin
        Operand:=tmp;
        Exit(True);
      end;

  result:=False;
end;

function TTextLogicalOperandHelper.ToString: String;
begin
  result:=Texts[Self];
end;

{ TTextLogicalExpression }

Constructor TTextLogicalExpression.Create(const ALeft: TExpression;
  const AOperand: TTextLogicalOperand; const ARight: TExpression);
begin
  inherited Create;
  Left:=ALeft;
  Operand:=AOperand;
  Right:=ARight;
end;

procedure TTextLogicalExpression.Assign(const Source: TExpression);
begin
  if Source is TTextLogicalExpression then
  begin
    FCase:=TTextLogicalExpression(Source).FCase;
    Operand:=TTextLogicalExpression(Source).Operand;
  end;

  inherited;
end;

function TTextLogicalExpression.ToString: String;
begin
  // Pending: CaseSensitive
  result:=Part(Left)+' '+Operand.ToString+' '+Part(Right);
end;

function TTextLogicalExpression.StartsWith(const A,B:String):Boolean;
begin
  {$IFDEF FPC}
  if CaseSensitive then
     result:=Copy(A,1,Length(B))=B
  else
     result:=SameText(Copy(A,1,Length(B)),B);

  {$ELSE}
  result:=A.StartsWith(B,not CaseSensitive);
  {$ENDIF}
end;

function TTextLogicalExpression.EndsWith(const A,B:String):Boolean;
begin
  {$IFDEF FPC}
  if CaseSensitive then
     result:=Copy(A,Length(A)-Length(B),Length(B))=B
  else
     result:=SameText(Copy(A,Length(A)-Length(B),Length(B)),B);

  {$ELSE}
  result:=A.EndsWith(B,not CaseSensitive);
  {$ENDIF}
end;

function TTextLogicalExpression.DoContains(const A,B:String):Boolean;
begin
  if CaseSensitive then
     result:=Pos(B,A)>0
  else
     result:={$IFDEF FPC}Pos(UpperCase(B),UpperCase(A)){$ELSE}Pos(B.ToUpper,A.ToUpper){$ENDIF}>0;
end;

function TTextLogicalExpression.Value: TData;

  function Calc(const A,B : String):TData;
  begin
    case Operand of
     TTextLogicalOperand.Starts : result:=StartsWith(A,B);
       TTextLogicalOperand.Ends : result:=EndsWith(A,B);
   TTextLogicalOperand.Contains : result:=DoContains(A,B);
    end;
  end;

begin
  result:=Calc(Left.Value,Right.Value);
end;

{ TIfExpression }

Constructor TIfExpression.Create(const ACondition: TLogicalExpression;
  const AThen, AElse: TExpression);
begin
  inherited Create;
  Condition:=ACondition;
  ThenExpression:=AThen;
  ElseExpression:=AElse;
end;

Destructor TIfExpression.Destroy;
begin
  ElseExpression.Free;
  ThenExpression.Free;
  Condition.Free;

  inherited;
end;

function TIfExpression.ToString: String;
begin
  if Condition=nil then
     result:=''
  else
     result:=Condition.ToString;

  result:=result+' ? ';

  if ThenExpression<>nil then
     result:=result+ThenExpression.ToString;

  if ElseExpression<>nil then
     result:=result+' : '+ElseExpression.ToString;
end;

procedure TIfExpression.Assign(const Source: TExpression);
var tmp : TIfExpression;
begin
  if Source is TIfExpression then
  begin
    tmp:=TIfExpression(Source);

    Condition:=TExpression.Clone(tmp.Condition) as TLogicalExpression;
    ThenExpression:=TExpression.Clone(tmp.ThenExpression);
    ElseExpression:=TExpression.Clone(tmp.ElseExpression);
  end;

//  inherited;
end;

// result:= Condition ? Then : Else
function TIfExpression.Value: TData;

  function GetValue(const AExpression:TExpression):TData;
  begin
    if AExpression=nil then
       result:=TExpression.Null
    else
       result:=AExpression.Value;
  end;

begin
  if Condition=nil then
     result:=TExpression.Null
  else
  if Condition.Value then
     result:=GetValue(ThenExpression)
  else
     result:=GetValue(ElseExpression);
end;

{ TTextOperandHelper }

class function TTextOperandHelper.FromString(const S: String;
                                             out Operand: TTextOperand): Boolean;
var tmp : TTextOperand;
begin
  for tmp:=Low(TTextOperand) to High(TTextOperand) do
      if SameText(Texts[tmp],S) then
      begin
        Operand:=tmp;
        Exit(True);
      end;

  result:=False;
end;

function TTextOperandHelper.ParameterCount: Integer;
const ParamCounts : Array[TTextOperand] of Integer=(
       2, // TTextOperand.IndexOf
       2, // TTextOperand.Pad        // or 3 -> source length [,true (trailing)]
       1, // TTextOperand.Split:     // or 2 -> source [,delimiter (default=space)]
       3, // TTextOperand.Insert:    // dest, source, pos
       2, // TTextOperand.Remove:    // or 3 -> source, pos [, count (total length)]
       3, // TTextOperand.Replace:   // or 4 -> dest find replace [,true (case-sensitive)]
       2, // TTextOperand.Count:     // or 3 -> source sub [,true (case-sensitive)]
       2  // TTextOperand.SubString: // or 3 -> source index [,count (total length)]
      );
begin
  result:=ParamCounts[Self];
end;

function TTextOperandHelper.ToString: String;
begin
  result:=Texts[Self];
end;

{ TTextOperandExpression }

procedure TTextOperandExpression.Assign(const Source: TExpression);
begin
  if Source is TTextOperandExpression then
     Operand:=TTextOperandExpression(Source).Operand;

  inherited;
end;

class function TTextOperandExpression.Split(const S, Delimiter: String): TData;
begin
  result:=TStringArray.Split(S,Delimiter);
end;

function TTextOperandExpression.ToString: String;
begin
  result:=Operand.ToString+inherited;
end;

// Returns the number of ocurrences of "sub" inside "value"
function StringCount(Value:String; const Sub:String):Integer; overload;
var i : Integer;
    LSub : Integer;
begin
  result:=0;

  LSub:=Length(Sub)-1;

  repeat
    i:=Pos(Sub,Value);

    if i>0 then
    begin
      Inc(result);
      Delete(Value,1,i+LSub);
    end
    else
      break;

  until Value='';
end;

// Returns the number of ocurrences
function StringCount(const A:TArrayExpression):Integer; overload;
var tmpCase : Boolean;
    Text,
    Search : String;
begin
  if A.Count>2 then
     tmpCase:=A[2].Value
  else
     tmpCase:=False;

  Text:=A[0].Value;
  Search:=A[1].Value;

  if tmpCase then
     result:=StringCount(Text,Search)
  else
     result:=StringCount(UpperCase(Text),UpperCase(Search))
end;

function DoInsert(const A:TArrayExpression):String;
var tmpIndex : Integer;
    tmpDest : String;
    tmpSource : String;
begin
  tmpDest:=A[0].Value;
  tmpSource:=A[1].Value;
  tmpIndex:=A[2].Value;

  Insert(tmpSource,tmpDest,tmpIndex);
  result:=tmpDest;
end;

function DoRemove(const A:TArrayExpression):String;
var tmpIndex,
    tmpCount : Integer;
    tmpS : String;
begin
  tmpS:=A[0].Value;
  tmpIndex:=A[1].Value;

  if A.Count>2 then
  begin
    tmpCount:=A[2].Value;
    Delete(tmpS,tmpIndex,tmpCount);
  end
  else
    Delete(tmpS,tmpIndex,1);

  result:=tmpS;
end;

function DoReplace(const A:TArrayExpression):String;
var tmp : TReplaceFlags;
begin
  tmp:=[rfIgnoreCase];

  // No bool short-circuit eval here, so duplicate "if" :
  if A.Count>3 then
     if A[3].Value then
        tmp:=[];

  result:=StringReplace(A[0].Value,A[1].Value,A[2].Value,tmp+[rfReplaceAll]);
end;

function DoSubString(const A:TArrayExpression):String;
var tmpIndex,
    tmpCount : Integer;
    tmpS : String;
begin
  tmpS:=A[0].Value;
  tmpIndex:=A[1].Value;

  if A.Count>2 then
  begin
    tmpCount:=A[2].Value;
    result:=Copy(tmpS,tmpIndex,tmpCount);
  end
  else
    result:=Copy(tmpS,tmpIndex,Length(tmpS));
end;

function DoSplit(const A:TArrayExpression):TData;
begin
  if A.Count>1 then
     result:=TTextOperandExpression.Split(A[0].Value,A[1].Value)
  else
     result:=TTextOperandExpression.Split(A[0].Value,' ');
end;

function TTextOperandExpression.AsString:String;

  function ArrayToString(const A:TData):String;
  var t : Integer;
      tmp : Integer;
  begin
    result:='[';

    tmp:=VarArrayLowBound(A,1);

    for t:=tmp to VarArrayHighBound(A,1) do
        if t>tmp then
           result:=result+','+A[t]
        else
           result:=result+A[t];

    result:=result+']';
  end;

var tmp : TData;
begin
  tmp:=Value;

  if VarIsArray(tmp) then
     result:=ArrayToString(tmp)
  else
     result:=tmp;
end;

class function TTextOperandExpression.FromString(
  const S: String): TParameterExpression;
var tmp : TTextOperand;
begin
  if TTextOperand.FromString(S,tmp) then
  begin
    result:=TTextOperandExpression.Create;
    TTextOperandExpression(result).Operand:=tmp;
  end
  else
    result:=nil;
end;

function TTextOperandExpression.ResultClass: TExpressionClass;
begin
  case Operand of
    TTextOperand.IndexOf,
      TTextOperand.Count: result:=TIntegerExpression;

        TTextOperand.Pad,
     TTextOperand.Insert,
     TTextOperand.Remove,
    TTextOperand.Replace,
  TTextOperand.SubString: result:=TTextExpression;
  else
    //TTextOperand.Split:
    result:=TTextExpression; // TArrayExpression?
  end;
end;

function DoPos(const Params:TArrayExpression):Integer;
var A,B : String; // needed by nextgen (no AnsiString support in Variants)
begin
  A:=Params[0].Value;
  B:=Params[1].Value;
  result:=Pos(A,B);
end;

function TTextOperandExpression.ValueFrom(const Params:TArrayExpression): TData;
begin
  case Operand of
    TTextOperand.IndexOf: result:=DoPos(Params);

        TTextOperand.Pad: result:=Params[0].Value; // pending

      TTextOperand.Split: result:=DoSplit(Params);

     TTextOperand.Insert: result:=DoInsert(Params);

     TTextOperand.Remove: result:=DoRemove(Params);

    TTextOperand.Replace: result:=DoReplace(Params);

      TTextOperand.Count: result:=StringCount(Params);

  TTextOperand.SubString: result:=DoSubString(Params);
  end;
end;

function TTextOperandExpression.Value: TData;

  procedure DoErrorParams;
  begin
    raise Exception.Create('Error: '+Operand.ToString+
          ' function wrong number of parameters ('+IntToStr(Operand.ParameterCount)+')');
  end;

var tmp : TArrayExpression;
begin
  if Expression is TArrayExpression then
  begin
    tmp:=TArrayExpression(Expression);

    if tmp.Count>=Operand.ParameterCount then
       result:=ValueFrom(tmp)
    else
       DoErrorParams;
  end
  else
  if Operand=TTextOperand.Split then
     result:=TTextOperandExpression.Split(Expression.Value,' ')
  else
     result:=TExpression.Null;
end;

{ TParameterExpression }

Constructor TParameterExpression.Create(const AParameters: Array of TData);
var L : Integer;

    {$IFDEF FPC}
    tmp : TExpression;
    {$ENDIF}
begin
  L:=Length(AParameters);

  if L=1 then
  begin
    {$IFDEF FPC}
    tmp:=TExpression(NativeInt(AParameters[0]));
    Create(tmp);
    {$ELSE}
    Create(AParameters[0]);
    {$ENDIF}
  end
  else
    Create(TArrayExpression.Create(AParameters));
end;

class function TParameterExpression.FromString(const S: String): TParameterExpression;
begin
  result:=nil;
end;

class function TParameterExpression.Call(const AParameters: TExpressions):TExpression;
begin
  if AParameters.Count=1 then
     result:=AParameters[0]
  else
     result:=TArrayExpression.Create(AParameters);
end;

class function TParameterExpression.Guess(
  const Token: String): TParameterExpression;
var t : Integer;
begin
  for t:=Low(Registered) to High(Registered) do
  begin
    result:=Registered[t].FromString(Token);

    if result<>nil then
       Exit;
  end;

  result:=nil;
end;

procedure TParameterExpression.SetExpression(const Value: TExpression);
begin
  inherited;

  if Expression is TArrayExpression then
     FParams:=TArrayExpression(Expression)
  else
     FParams:=nil;
end;

function TParameterExpression.ToString: String;
begin
  result:='(';

  if Expression<>nil then
     result:=result+Expression.ToString;

  result:=result+')';
end;

{ TParameterExpressionsHelper }

procedure TParameterExpressionsHelper.Add(const AClass: TParameterExpressionClass);
var L : Integer;
begin
  if IndexOf(AClass)=-1 then // <-- prevent duplicates
  begin
    L:=Length(Self);
    SetLength(Self,L+1);
    Self[L]:=AClass;
  end;
end;

function TParameterExpressionsHelper.IndexOf(
  const AClass: TParameterExpressionClass): Integer;
var t : Integer;
begin
  for t:=Low(Self) to High(Self) do
      if Self[t]=AClass then
         Exit(t);

  result:=-1;
end;

procedure TParameterExpressionsHelper.Remove(
  const AClass: TParameterExpressionClass);
var t,
    tmp : Integer;
begin
  tmp:=IndexOf(AClass);

  if tmp<>-1 then
  begin
    for t:=tmp to High(Self)-1 do
        Self[t]:=Self[t+1];

    SetLength(Self,High(Self));
  end;
end;

{ TUnaryDateTimeExpression }

function TUnaryDateTimeExpression.ResultClass: TExpressionClass;
begin
  result:=TDateTimeExpression;
end;

initialization
  {$IFDEF BIVARIANT}
  TExpression.Null:=BIVariant.Null;

  {$ELSE}

  // To avoid using System.Variants unit, set Null here:
  TVarData(TExpression.Null).VType:=varNull;
  {$ENDIF}

  TDateTimePart.QuarterFormat:='Q%d';

  // Intrinsic default functions:
  TParameterExpression.Registered.Add(TMathExpression);
  TParameterExpression.Registered.Add(TUnaryTextExpression);
  TParameterExpression.Registered.Add(TTextOperandExpression);
  TParameterExpression.Registered.Add(TDateExpression);
  TParameterExpression.Registered.Add(TTimeExpression);
  TParameterExpression.Registered.Add(TDateTimePartExpression);
finalization
  TParameterExpression.Registered:=nil;
end.

