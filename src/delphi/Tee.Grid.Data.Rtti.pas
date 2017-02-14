{*********************************************}
{  TeeGrid Software Library                   }
{  VirtualData from Records and Objects       }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Data.Rtti;
{$I Tee.inc}

interface

{$IF CompilerVersion>22}
{$DEFINE NEWRTTI}
{$IFEND}

{
   Several classes to automatically link a TeeGrid with records or classes,
   or arrays of records or classes, or TList of records and classes.

   Example using arrays:

    type
      TPerson=record ... end;  // <-- or class

    var
      MyData : TArray<TPerson>;

      SetLength(MyData,10);
      ... fill MyData ....

      TeeGrid1.Data:= TVirtualData<TArray<TPerson>>.Create(MyData);

    Example using TList:

    var
      MyData : TList<TPerson>;

      TeeGrid1.Data:= TVirtualData<TList<TPerson>>.Create(MyData);

    Example using single records:

    var
      MyData : TPerson;

      TeeGrid1.Data:= TVirtualData<TPerson>.Create(MyData);

    Note:

      These are equivalent classes, for easier use:

      TeeGrid1.Data:= TVirtualArrayData<TPerson>.Create(MyData);

      TeeGrid1.Data:= TVirtualData<TArray<TPerson>>.Create(MyData);

}

uses
  {System.}Classes,

  Tee.Grid.Data, Tee.Grid.Columns, Tee.Painter,
  {System.}Generics.Collections, {System.}Rtti, {System.}TypInfo;

type
  TVirtualDataRtti=class(TVirtualData)
  private
    class function Add(const AColumns:TColumns;
                       const AMember:TRttiMember;
                       const AType:TRttiType):TColumn; overload; static;

    function AutoWidthOf(const AType:TRttiType;
                         const APainter:TPainter;
                         const AColumn:TColumn):Single;

    function FinalPointer(const AColumn: TColumn; P:Pointer):Pointer;
    function FinalValue(const AColumn: TColumn; const AValue:TValue):TValue; overload;

    class function IsBoolean(const AType:TRttiType):Boolean; static;
    class function IsDate(const AType:TRttiType):Boolean; static;
    class function IsTime(const AType:TRttiType):Boolean; static;
    class function IsDateTime(const AType:TRttiType):Boolean; static;
    class function IsNumeric(const AType:TRttiType):Boolean; overload; static;

    class procedure LinkTo(const AColumn:TColumn; const AMember:TRttiMember;
                           const AType:TRttiType); overload; static;

    class procedure LinkTo(const AColumn:TColumn;
                           const AName:String;
                           const AObject:TObject;
                           const AType:TRttiType); overload; static;

    class function MemberOf(const AColumn:TColumn):TRttiMember; static; inline;
    class function PointerOf(const AValue:TValue):Pointer; static;
    class function TypeOf(const AColumn: TColumn): TRttiType; overload; static;
    class function TypeOf(const AMember:TRttiMember):TRttiType; overload; static;
    class function ValueOf(const AMember:TRttiMember; const P:Pointer):TValue; overload; static;
    class function ValueOf(const AColumn:TColumn; const P:Pointer):TValue; overload; static; inline;
  public
    function DataType(const AColumn:TColumn):PTypeInfo; override;
  end;

  TVisibility=set of TMemberVisibility;

  TRttiMembers=(Both, Fields, Properties);

  TBaseVirtualData=class(TVirtualDataRtti)
  private
  class var
    Context : TRttiContext;

  var
    FAncestor : Boolean;
    FMembers : TRttiMembers;
    FTypeInfo : PTypeInfo;
    FVisibility : TVisibility;

    IsDynArray : Boolean;

    {$IFDEF NEWRTTI}
    IItems : TRttiIndexedProperty;
    {$ENDIF}

    ICount : TRttiProperty;
    IFixedCount : Integer;
    IObject : TObject;

    procedure AddFields(const AColumns:TColumns; const AType:TRttiType); overload;
    procedure AddProperties(const AColumns:TColumns; const AType:TRttiType); overload;

    procedure DoAddArrayColumns(const AColumns:TColumns; const ARow:TValue);
    procedure DoAddColumns(const AColumns:TColumns; const AType:TRttiType);
    procedure DoSetValue(const AColumn:TColumn; const ARow:Integer; const Value:TValue);

    function FinalType:TRttiType;
    function FinalTypeIsArray:Boolean;
    procedure InternalAddType(const AColumns:TColumns;
                              const AMember:TRttiMember;
                              const AType:TRttiType);

    function IsVisible(const AMember: TRttiMember): Boolean; inline;
    class procedure LinkTo(const AColumn:TColumn; const AType:TRttiType); overload; static;
    class function NameOf(const AType:TRttiOrdinalType):String; overload; static;
    class function NameOf(const AType:TRttiFloatType):String; overload; static;
    function RttiType:TRttiType; inline;
  protected
    procedure DoSetArrayValue(const AColumn:TColumn; const ARow:Integer; const Value:TValue); virtual;
    function GetPointerOf(const AColumn:TColumn; const ARow:Integer):Pointer; virtual;
    function RowValue(const ARow:Integer):TValue; virtual;
    procedure SetField(const AColumn:TColumn; const ASource:TObject); override;
  public
    Constructor Create(const AObject:TObject;
                       const AProperty:String;
                       const ACount:Integer;
                       const AVisibility:TVisibility=[mvPublic,mvPublished];
                       const AMembers:TRttiMembers=TRttiMembers.Both;
                       const AAncestor:Boolean=False); overload;

    procedure AddColumns(const AColumns:TColumns); override;
    function AsFloat(const AColumn: TColumn; const ARow: Integer): TFloat; override;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;
    function Count:Integer; override;
    procedure Load(const AColumns:TColumns); override;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;
  end;

  TVirtualData<T>=class(TBaseVirtualData)
  private
    FData : ^T;

    procedure AddArrayColumns(const AColumns:TColumns);
    function Data:TValue; inline;
    procedure GuessArrayItems;
    procedure GuessItemsProperty(const AType:TRttiType);
  protected
    procedure DoSetArrayValue(const AColumn:TColumn; const ARow:Integer; const Value:TValue); override;
    function GetPointerOf(const AColumn:TColumn; const ARow:Integer):Pointer; override;
    function RowValue(const ARow:Integer):TValue; override;
  public
    Constructor Create(var AData:T;
                       const AVisibility:TVisibility=[mvPublic,mvPublished];
                       const AMembers:TRttiMembers=TRttiMembers.Both;
                       const AAncestor:Boolean=False); overload;

    procedure AddColumns(const AColumns:TColumns); override;
    function Count:Integer; override;
  end;

  // Helper classes, just aliases:
  TVirtualArrayData<T>=class(TVirtualData<TArray<T>>);         // Array of T
  TVirtualArray2DData<T>=class(TVirtualArrayData<TArray<T>>);  // Array of Array of T

  TVirtualListData<T>=class(TVirtualData<TList<T>>);

implementation
