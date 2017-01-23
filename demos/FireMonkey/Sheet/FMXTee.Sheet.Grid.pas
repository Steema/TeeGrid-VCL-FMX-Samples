unit FMXTee.Sheet.Grid;

interface

{
  Special FMX TeeGrid, maintains an internal "Sheet" object
}

uses
  System.Classes,
  FMXTee.Grid, Tee.Sheet;

type
  TGridSheet=class(TTeeGrid)
  private
    FSheet : TSheet;
  public
    Constructor Create(AOwner:TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    property Sheet:TSheet read FSheet;
  end;

  TSheetItem=class(TCollectionItem)
  public
    Sheet : TGridSheet;
  end;

  TSheets=class(TOwnedCollection)
  private
    function NewName:String;
    function Get(const Index: Integer): TSheetItem;
    procedure Put(const Index: Integer; const Value: TSheetItem);
  public
    function Add: TSheetItem;

    property Items[const Index:Integer]:TSheetItem read Get write Put; default;
  end;

implementation

uses
  {System.}SysUtils;

{ TGridSheet }

Constructor TGridSheet.Create(AOwner: TComponent);
begin
  inherited;
  FSheet:=TSheet.Create(Grid);
end;

{$IFNDEF AUTOREFCOUNT}
Destructor TGridSheet.Destroy;
begin
  FSheet.Free;
  inherited;
end;
{$ENDIF}

{ TSheets }

function TSheets.Add: TSheetItem;
begin
  result:=inherited Add as TSheetItem;

  result.Sheet:=TGridSheet.Create(Owner as TComponent);
  result.Sheet.Sheet.Name:=NewName;
end;

function TSheets.Get(const Index: Integer): TSheetItem;
begin
  result:=(inherited Items[Index] as TSheetItem);
end;

function TSheets.NewName:String;

  function Find(const AName:String):TSheetItem;
  var t : Integer;
  begin
    for t:=0 to Count-1 do
        if SameText(Items[t].Sheet.Name,AName) then
           Exit(Items[t]);

    result:=nil;
  end;

var tmp : Integer;
begin
  tmp:=1;

  repeat
    result:='Sheet'+tmp.ToString;

    if Find(result)=nil then
       break
    else
       Inc(tmp);

  until False;
end;

procedure TSheets.Put(const Index: Integer; const Value: TSheetItem);
begin
  inherited Items[Index]:=Value;
end;

end.
