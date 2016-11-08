unit Tee.Grid.Data;

interface

{$IFDEF FPC}
{$ELSE}
{$IF CompilerVersion>25} // XE4 bug: Internal Error: URW1154
{$DEFINE HASRTTI}
{$ENDIF}
{$ENDIF}

uses
  System.Classes,
  {$IFDEF HASRTTI}
  System.Generics.Collections, System.Rtti, System.TypInfo,
  {$ENDIF}
  Tee.Grid.Columns, Tee.Painter;

type
  TVirtualData=class abstract
  protected
    FOnRefresh : TNotifyEvent;

    {$IFDEF HASRTTI}
    class function Add(const AColumns:TColumns;
                       const AMember:TRttiMember;
                       const AType:TRttiType):TColumn;

    class procedure DoSetValue(const AColumn:TColumn; const P:Pointer; const Value:TValue); static;
    class function GetValue(const AColumn:TColumn; const P:Pointer):TValue; static;
    class function IsBoolean(const AType:TRttiType):Boolean; static;

    class function IsDate(const AType:TRttiType):Boolean; static;
    class function IsTime(const AType:TRttiType):Boolean; static;
    class function IsDateTime(const AType:TRttiType):Boolean; static;

    class function IsNumeric(const AType:TRttiType):Boolean; static;
    class function MemberOf(const AColumn:TColumn):TRttiMember; inline; static;
    class function PointerOf(const AValue:TValue):Pointer; static;
    class function TypeOf(const AColumn: TColumn): TRttiType; static;
    class function ValueOf(const AMember:TRttiMember; const P:Pointer):TValue; static;
    {$ENDIF}

    procedure Refresh;

    function SampleDate(const AColumn:TColumn): String;
    function SampleDateTime(const AColumn:TColumn):String;
    function SampleTime(const AColumn:TColumn):String;

  public
    procedure AddColumns(const AColumns:TColumns); virtual; abstract;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; virtual; abstract;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; virtual; abstract;
    function Count:Integer; virtual; abstract;
    procedure Load; virtual; abstract;
    function ReadOnly(const AColumn:TColumn):Boolean; virtual;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); virtual; abstract;
  end;

  {$IFDEF HASRTTI}
  TVisibility=set of TMemberVisibility;

  TRttiMembers=(Both, Fields, Properties);

  TVirtualData<T>=class(TVirtualData)
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
  public
    Constructor Create(const Value: TArray<T>);

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
  {$ENDIF}

implementation
