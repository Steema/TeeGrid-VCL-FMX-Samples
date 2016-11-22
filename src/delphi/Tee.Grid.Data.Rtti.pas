{*********************************************}
{  TeeGrid Software Library                   }
{  VirtualData from Records and Objects       }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Data.Rtti;

interface

{
   Several classes to automatically link a TeeGrid with arrays or TList of
   records and classes.

   Example using arrays:

    type
      TPerson=record ... end;  // <-- or class

    var
      MyData : TArray<TPerson>;

      SetLength(MyData,10);
      ... fill MyData ....

      TeeGrid1.Data:= TVirtualArrayData<TPerson>.Create(MyData);

    Example using TList:

    var
      MyData : TList<TPerson>;

      TeeGrid1.Data:= TVirtualListData<TPerson>.Create(MyData);
}

uses
  Tee.Grid.Data, Tee.Grid.Columns, Tee.Painter,
  System.Generics.Collections, System.Rtti, System.TypInfo;

type
  TVirtualDataRtti=class(TVirtualData)
  private
    var
      Parents : Array[0..99] of TColumn;

    class function Add(const AColumns:TColumns;
                       const AMember:TRttiMember;
                       const AType:TRttiType):TColumn; overload; static;

    class procedure DoSetValue(const AMember:TRttiMember; const P:Pointer; const Value:TValue); overload; static;
    class procedure DoSetValue(const AColumn:TColumn; const P:Pointer; const Value:TValue); overload; static;

    //function FinalPointer(AParent: TColumn; P:Pointer):Pointer;
    function FinalValue(const AColumn: TColumn; const AValue:TValue):TValue; overload;

    class function IsBoolean(const AType:TRttiType):Boolean; static;
    class function IsDate(const AType:TRttiType):Boolean; static;
    class function IsTime(const AType:TRttiType):Boolean; static;
    class function IsDateTime(const AType:TRttiType):Boolean; static;
    class function IsNumeric(const AType:TRttiType):Boolean; overload; static;

    class function MemberOf(const AColumn:TColumn):TRttiMember; inline; static;
    class function PointerOf(const AValue:TValue):Pointer; static;
    class function TypeOf(const AColumn: TColumn): TRttiType; static;
    class function ValueOf(const AMember:TRttiMember; const P:Pointer):TValue; overload; static;
    class function ValueOf(const AColumn:TColumn; const P:Pointer):TValue; overload; static; inline;
  end;

  TVisibility=set of TMemberVisibility;

  TRttiMembers=(Both, Fields, Properties);

  TVirtualData<T>=class(TVirtualDataRtti)
  private
  class var
    Context : TRttiContext;

  var
    FMembers : TRttiMembers;
    FTypeInfo : PTypeInfo;
    FVisibility : TVisibility;

    procedure AddFields(const AColumns:TColumns; const AType:TRttiType);
    procedure AddProperties(const AColumns:TColumns; const AType:TRttiType);

    procedure DoAddColumns(const AColumns:TColumns; const AType:TRttiType);

    procedure InternalAddType(const AColumns:TColumns;
                              const AMember:TRttiMember;
                              const AType:TRttiType);

    function IsVisible(const AMember:TRttiMember):Boolean; inline;

    class function NameOf(const AType:TRttiOrdinalType):String; overload; static;
    class function NameOf(const AType:TRttiFloatType):String; overload; static;
  public
    Constructor Create(const AType:PTypeInfo;
                       const AVisibility:TVisibility=[mvPublic,mvPublished];
                       const AMembers:TRttiMembers=TRttiMembers.Both);

    procedure AddColumns(const AColumns:TColumns); override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;
    procedure Load; override;
  end;

  TVirtualArrayData<T>=class(TVirtualData<T>)
  private
    FArray : TArray<T>;

    function FinalValue(const AColumn: TColumn; const ARow: Integer): TValue; overload;

    procedure TryInt64(const AColumn: TColumn; const ARow: Integer; const AText: String);
    procedure TryFloat(const AColumn: TColumn; const ARow: Integer; const AText: String);
  public
    Constructor Create(var Value: TArray<T>);

    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function Count:Integer; override;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;
  end;

  TVirtualListData<T>=class(TVirtualData<T>)
  private
    FList : TList<T>;
  public
    Constructor Create(const Value: TList<T>);

    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function Count:Integer; override;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;
  end;

implementation
